# For all practical purposes, this Makefile serves as a convenient 
# wrapper around CMake and Ninja, for local development.
# The reference build is the GitHub build action.

# Project Variables
BUILD_DIR     := build
VENV_DIR      := .venv
LLVM_ROOT    ?= /usr/local/opt/llvm
CLANG_FORMAT ?= $(LLVM_ROOT)/bin/clang-format
CMAKE_EXTRA_ARGS ?=

# GNU make provides built-in defaults (CXX=c++, CC=cc). Override only those
# built-in defaults so Homebrew LLVM is used by default, while preserving any
# explicit command-line or environment overrides from the user.
ifeq ($(origin CXX), default)
CXX := $(LLVM_ROOT)/bin/clang++
endif
ifeq ($(origin CC), default)
CC := $(LLVM_ROOT)/bin/clang
endif

# Build parallelism. Keep it a bit on the slim side: most module / test
# workloads have I/O- or memory-bound phases, so pinning to `nproc` (let
# alone Ninja's default of `nproc + 2`) tends to cause thrashing rather
# than measurable speedup. Default to `max(2, nproc - 2)`:
#   - The `-2` leaves headroom for the IDE / browser locally, and for
#     I/O / GC concurrency on CI runners alike.
#   - The floor of 2 reflects that any sensible host has more than one
#     component (CPU, memory bus, disk bus, …); even on a 1- or 2-core
#     machine, two concurrent jobs let one block on I/O while the other
#     makes CPU progress.
# Override with `make JOBS=N ...` to pin a specific count (e.g. for
# micro-benchmarks or constrained environments). Passed through to
# `cmake --build` via CMake's `--parallel` flag, which is
# the portable knob that dispatches to the underlying generator.
_HOST_CORES := $(shell nproc 2>/dev/null || sysctl -n hw.ncpu 2>/dev/null || echo 4)
JOBS ?= $(shell j=$$(( $(_HOST_CORES) - 2 )); [ $$j -lt 2 ] && j=2; echo $$j)

.PHONY: help python-api jlt-lint
.PHONY: bench bench-native all clean compile test-compile test check coverage wheel wheel-verify python-coverage python-coverage-local ir-fixture-refresh ir-fixture-check format format-check install-hooks doxygen paper-jlt \
	ci-history ci-main pr-init pr-status pr-status-all pr-checks pr-watch pr-sync pr-review-comments pr-review-unresolved pr-resolve-thread pr-resolve-threads \
	check-review-comments resolve-review-comment issue-list

all: compile

# Every target with a description after ##.
help: ## List the targets
	@awk -F':.*## ' '/^[a-zA-Z0-9_-]+:.*## /{printf "  %-22s %s\n", $$1, $$2}' $(MAKEFILE_LIST)

clean: ## Remove the build directory
	rm -rf $(BUILD_DIR)

# Project configure step: select the LLVM toolchain, enable C++ module
# scanning, use a Release build, and opt into the double-as-real proxy needed
# by the current finite-dimensional test suite.
$(BUILD_DIR)/CMakeCache.txt:
	cmake -S . -B $(BUILD_DIR) -G Ninja \
		-DCMAKE_CXX_COMPILER=$(CXX) \
		-DCMAKE_C_COMPILER=$(CC) \
		-DCMAKE_CXX_SCAN_FOR_MODULES=ON \
		-DCMAKE_BUILD_TYPE=Release \
		-DDEDEKIND_ENABLE_DOUBLE_REAL_PROXY=ON \
		$(CMAKE_EXTRA_ARGS)

compile: $(BUILD_DIR)/CMakeCache.txt ## Configure if needed and build all targets
	cmake --build $(BUILD_DIR) --parallel $(JOBS)

# Build + type-check every C++ translation unit in the repo (library
# modules, test executables, IR fixture showcases) without running any
# tests. Useful as a fast local sanity gate — catches module-DAG
# violations, Unicode/species-visibility issues, and template
# instantiation errors without paying the cost of ctest or the Python
# bindings.
test-compile: compile ## compile, plus the IR-fixture showcases: type-checks every C++ file without running tests
	cmake --build $(BUILD_DIR) --target set-pruning-ir-fixture --parallel $(JOBS)

test: test-compile ## Build, then run the full test suite
	cmake --build $(BUILD_DIR) --target run-test --parallel $(JOBS)

# Build, then the lint in one pass over the whole tree and each layer's tests
# (CMake's `check`: lint, run-test-<layer>); -k 0 reports every failure, not
# the first.  CI's one entry.
check: $(BUILD_DIR)/CMakeCache.txt ## Build, then lint and test
	cmake --build $(BUILD_DIR) --target all check --parallel $(JOBS) -- -k 0

coverage: compile ## Build, run tests, and produce the LLVM coverage report
	rm -f $(BUILD_DIR)/*.profraw
	cmake --build $(BUILD_DIR) --target run-test --parallel $(JOBS)
	cmake --build $(BUILD_DIR) --target generate_coverage --parallel $(JOBS)

# Run Python tests with coverage collection and produce an XML report for Codecov.
# Uses the repo-managed housekeeping helper for auditable approvals.
# The wheel at a version (build, manylinux retag, check) and its install,
# from dist/ or an index (INDEX=test.pypi.org|pypi.org): the steps a release calls,
# by CI or by hand.
# The Listing 5 benchmark, in its own build directory, without coverage.  It fails
# if the ways of counting disagree or the library falls far behind.  Results in
# build/bench.
BENCH_DIR ?= $(BUILD_DIR)-bench
BENCH_OUT ?= $(BUILD_DIR)/bench
BENCH_FLAGS ?=
bench: ## Run the C++ benchmark, without coverage
	cmake -S . -B $(BENCH_DIR) -G Ninja \
		-DCMAKE_CXX_COMPILER=$(CXX) \
		-DCMAKE_C_COMPILER=$(CC) \
		-DCMAKE_CXX_SCAN_FOR_MODULES=ON \
		-DCMAKE_BUILD_TYPE=Release \
		-DDEDEKIND_COVERAGE=OFF \
		-DDEDEKIND_BENCH_FLAGS='$(BENCH_FLAGS)' \
		-DDEDEKIND_BENCH=ON
	cmake --build $(BENCH_DIR) --target bench_listing5 --parallel $(JOBS)
	mkdir -p $(BENCH_OUT)
	bash -o pipefail -c '$(BENCH_DIR)/bench_listing5 | tee $(BENCH_OUT)/scanner.md'
	bash -e -o pipefail -c "{ echo; echo '| Letters | Found | Peak KiB |'; echo '|---:|---:|---:|'; \
	  $(BENCH_DIR)/bench_listing5 memory 10000000; \
	  $(BENCH_DIR)/bench_listing5 memory 10000000000; } | tee -a $(BENCH_OUT)/scanner.md"
	-for f in scan_definition scan_table law_ascii law_ranks law_masks law_guarded law_blocks law_guarded_blocks law_words law_columnar; do \
	  objdump -d --no-show-raw-insn --disassemble=$$f $(BENCH_DIR)/bench_listing5; \
	done | tee $(BENCH_OUT)/law.s
	-objdump -d -C --no-show-raw-insn $(BENCH_DIR)/bench_listing5 | \
	  awk '/^[0-9a-f]+ <.*ReflectedBlock.*>:/,/^$$/' > $(BENCH_OUT)/library.s

# The same at -O3 for this machine's own instructions: what the hardware can do,
# where `bench` is what a wheel ships.  For information, so it holds nothing to the
# limits (-i), and still leaves its disassembly.
bench-native: ## Run the C++ benchmark at -O3 -march=native
	$(MAKE) -i bench BENCH_DIR=$(BUILD_DIR)-bench-native BENCH_OUT=$(BUILD_DIR)/bench/native \
		BENCH_FLAGS='-O3;-march=native'

wheel: ## Build and check the Python wheel (VERSION=)
	bash .github/scripts/wheel.sh $(VERSION)

wheel-verify: ## Install the wheel in a fresh environment and test it (VERSION=, INDEX=)
	bash .github/scripts/wheel-verify.sh $(VERSION) $(INDEX)

# The Python API reference, from the installed wheel's docstrings, for Pages
# beside the Doxygen: pdoc imports pynzli, so the wheel must be installed.
python-api: ## The Python API reference, from the installed wheel
	python -m pip install --quiet pdoc
	python -m pdoc pynzli --output-directory $(BUILD_DIR)/python-api \
		--footer-text "pynzli, the Python handles over dedekind"

python-coverage: ## Python tests and build/python-coverage.xml
	bash .github/copilot/housekeeping/python-coverage.sh

# Local convenience alias (same behavior as python-coverage).
python-coverage-local: python-coverage

ir-fixture-refresh: ## Regenerate the checked-in LLVM IR fixtures
	python .github/copilot/housekeeping/pruning-ir-fixture.py refresh

ir-fixture-check: ## Check the LLVM IR fixtures against the source
	python .github/copilot/housekeeping/pruning-ir-fixture.py check


format: ## Format all C++ sources
	find src -name "*.cpp" -o -name "*.cppm" | xargs $(CLANG_FORMAT) -i

format-check: ## Check formatting without changing files
	find src -name "*.cpp" -o -name "*.cppm" | xargs $(CLANG_FORMAT) --dry-run --Werror

# The Jlt linter (#1060), every layer (CMake's `lint`; one is lint-<layer>);
# the gate is tools/jlt-lint/scope, the report build/jlt-lint/report.txt.
jlt-lint: $(BUILD_DIR)/CMakeCache.txt ## The Jlt linter, building what it reads
	cmake --build $(BUILD_DIR) --target lint --parallel $(JOBS)

install-hooks: ## Install the pre-push git hook
	git config core.hooksPath .githooks
	chmod +x .githooks/pre-push
	@echo "Installed repo hooks from .githooks/"

# CI/PR workflow helpers (optimistic concurrency loop)

# Standardized backlog issue query helper.
# Usage examples:
#   make issue-list
#   make issue-list LIMIT=50
#   make issue-list STATE=all SEARCH="linear algebra"
#   make issue-list FIELDS="number,title,labels,comments,body"
issue-list:
	@STATE_VAL="$(STATE)"; \
	LIMIT_VAL="$(LIMIT)"; \
	SEARCH_VAL="$(SEARCH)"; \
	FIELDS_VAL="$(FIELDS)"; \
	if [ -z "$$STATE_VAL" ]; then \
		STATE_VAL="open"; \
	fi; \
	if [ -z "$$LIMIT_VAL" ]; then \
		LIMIT_VAL=50; \
	fi; \
	if [ -z "$$FIELDS_VAL" ]; then \
		FIELDS_VAL="number,title,body,labels,comments"; \
	fi; \
	echo "Listing issues: state=$$STATE_VAL limit=$$LIMIT_VAL fields=$$FIELDS_VAL"; \
	if [ -n "$$SEARCH_VAL" ]; then \
		gh issue list --state "$$STATE_VAL" --limit "$$LIMIT_VAL" --search "$$SEARCH_VAL" --json "$$FIELDS_VAL"; \
	else \
		gh issue list --state "$$STATE_VAL" --limit "$$LIMIT_VAL" --json "$$FIELDS_VAL"; \
	fi

ci-history: ## Recent CI runs of a branch (BRANCH=, LIMIT=)
	@BRANCH_NAME="$(BRANCH)"; \
	LIMIT_VAL="$(LIMIT)"; \
	if [ -z "$$BRANCH_NAME" ]; then \
		BRANCH_NAME="$$(git rev-parse --abbrev-ref HEAD)"; \
	fi; \
	if [ -z "$$LIMIT_VAL" ]; then \
		LIMIT_VAL=5; \
	fi; \
	echo "Recent CI runs for branch '$$BRANCH_NAME' (limit=$$LIMIT_VAL):"; \
	gh run list --branch "$$BRANCH_NAME" --limit "$$LIMIT_VAL"

ci-main: ## Recent CI runs of main
	@$(MAKE) ci-history BRANCH=main LIMIT=5

# Initialize an issue-scoped branch, empty checkpoint commit, remote push, and draft PR.
# PR title and body are populated from the GitHub issue content.
# Usage:
#   make pr-init ISSUES="234 236"
# Optional:
#   TYPE=feat|fix|docs|chore   (default: feat)
#   BASE=main                  (default: main)
pr-init: ## Issue branch, empty commit and draft PR (ISSUES="1 2", TYPE=, BASE=)
	@ISSUES="$(ISSUES)" TYPE="$(TYPE)" BASE="$(BASE)" \
		bash .github/copilot/housekeeping/pr-init.sh

pr-status: ## The current branch's PR
	gh pr view --json number,title,state,isDraft,url

pr-checks: ## The PR's checks now
	gh pr checks || true

pr-watch: ## Wait for the PR's checks
	gh pr checks --watch || true

pr-sync: ## Fetch, status and checks before a push; fails on merge conflicts
	git fetch --prune
	git status -sb
	gh pr view --json number,title,state,isDraft,url,mergeStateStatus
	@MERGE_STATE="$$(gh pr view --json mergeStateStatus --jq .mergeStateStatus)"; \
	if [ "$$MERGE_STATE" = "DIRTY" ] || [ "$$MERGE_STATE" = "CONFLICTING" ]; then \
		echo "ERROR: PR has merge conflicts (mergeStateStatus=$$MERGE_STATE)."; \
		echo "Resolve conflicts before continuing with PR health checks."; \
		exit 2; \
	fi
	gh pr checks || true

# Sweep ALL open PRs authored by the current user: report each PR's
# mergeability, base ref, draft status, status-check rollup, and
# unresolved-review-thread count. Read-only. Paginates over review
# threads so the count is reliable even on PRs with > 100 threads.
# Logic lives in .github/copilot/housekeeping/pr-status-all.sh.
# Usage: make pr-status-all
pr-status-all:
	@bash .github/copilot/housekeeping/pr-status-all.sh

# List inline PR review comments for the current PR (or PR=<number>).
pr-review-comments: ## Inline review comments on the PR (PR=)
	@PR_NUM="$(PR)"; \
	if [ -z "$$PR_NUM" ]; then \
		PR_NUM="$$(gh pr view --json number --jq .number)"; \
	fi; \
	REPO="$$(gh repo view --json nameWithOwner --jq .nameWithOwner)"; \
	echo "Listing review comments for PR #$$PR_NUM ($$REPO)..."; \
	gh api repos/$$REPO/pulls/$$PR_NUM/comments \
		--jq '.[] | "- " + .path + ":" + (.line|tostring) + " :: " + (.body | gsub("\\n"; " "))'

# Scan unresolved review threads on the current PR (or PR=<number>).
pr-review-unresolved: ## Unresolved review threads; non-zero if any (PR=)
	@PR_NUM="$(PR)"; \
	if [ -z "$$PR_NUM" ]; then \
		PR_NUM="$$(gh pr view --json number --jq .number)"; \
	fi; \
	REPO="$$(gh repo view --json nameWithOwner --jq .nameWithOwner)"; \
	OWNER="$${REPO%/*}"; \
	NAME="$${REPO#*/}"; \
	echo "Scanning unresolved review threads for PR #$$PR_NUM ($$REPO)..."; \
	COUNT="$$(gh api graphql \
		-F owner="$$OWNER" \
		-F name="$$NAME" \
		-F number="$$PR_NUM" \
		-f query='query($$owner:String!, $$name:String!, $$number:Int!) { repository(owner: $$owner, name: $$name) { pullRequest(number: $$number) { reviewThreads(first: 100) { nodes { id isResolved path line comments(first: 1) { nodes { author { login } } } } } } } }' \
		--jq '.data.repository.pullRequest.reviewThreads.nodes | map(select(.isResolved == false)) | length')"; \
	if [ "$$COUNT" -eq 0 ]; then \
		echo "OK: no unresolved review threads."; \
	else \
		echo "Found $$COUNT unresolved review thread(s):"; \
		gh api graphql \
			-F owner="$$OWNER" \
			-F name="$$NAME" \
			-F number="$$PR_NUM" \
			-f query='query($$owner:String!, $$name:String!, $$number:Int!) { repository(owner: $$owner, name: $$name) { pullRequest(number: $$number) { reviewThreads(first: 100) { nodes { id isResolved path line comments(first: 1) { nodes { author { login } } } } } } } }' \
			--jq '.data.repository.pullRequest.reviewThreads.nodes | map(select(.isResolved == false)) | .[] | "- " + .id + " :: " + (.path // "<unknown>") + ":" + ((.line // 0) | tostring) + " by @" + (.comments.nodes[0].author.login // "unknown")'; \
		exit 1; \
	fi

# Friendly alias: check unresolved review threads on the current PR (or PR=<number>).
# Usage: make check-review-comments [PR=<number>]
check-review-comments:
	@$(MAKE) pr-review-unresolved PR="$(PR)"

# Resolve one review thread on the current PR (or PR=<number>) and reply with a reason.
# Usage: make pr-resolve-thread THREAD_ID=<thread_id> REASON="<resolution note>" [PR=<number>]
# REASON is single-quoted from its raw value, never expanded by make or the
# shell, so backticks and $(...) in it are posted literally; it is not
# exported, since make expands an exported variable to build the environment.
# The thread is resolved only after the reply is posted.
unexport REASON
pr-resolve-thread: ## Reply to and resolve one thread (THREAD_ID=, REASON=)
	@PR_NUM="$(PR)"; \
	THREAD_ID="$(THREAD_ID)"; \
	REASON='$(subst ','\'',$(value REASON))'; \
	if [ -z "$$THREAD_ID" ]; then \
		echo "ERROR: THREAD_ID is required."; \
		echo "Usage: make pr-resolve-thread THREAD_ID=<thread_id> REASON=\"<resolution note>\" [PR=<number>]"; \
		exit 2; \
	fi; \
	if [ -z "$$REASON" ]; then \
		echo "ERROR: REASON is required."; \
		echo "Usage: make pr-resolve-thread THREAD_ID=<thread_id> REASON=\"<resolution note>\" [PR=<number>]"; \
		exit 2; \
	fi; \
	if [ -z "$$PR_NUM" ]; then \
		PR_NUM="$$(gh pr view --json number --jq .number)"; \
	fi; \
	REPO="$$(gh repo view --json nameWithOwner --jq .nameWithOwner)"; \
	THREAD_STATE="$$(gh api graphql \
		-F threadId="$$THREAD_ID" \
		-f query='query($$threadId:ID!) { node(id: $$threadId) { ... on PullRequestReviewThread { isResolved comments(first: 1) { nodes { databaseId } } } } }' \
		--jq '.data.node.isResolved')"; \
	if [ "$$THREAD_STATE" = "true" ]; then \
		echo "Thread $$THREAD_ID is already resolved."; \
		exit 0; \
	fi; \
	COMMENT_ID="$$(gh api graphql \
		-F threadId="$$THREAD_ID" \
		-f query='query($$threadId:ID!) { node(id: $$threadId) { ... on PullRequestReviewThread { comments(first: 1) { nodes { databaseId } } } } }' \
		--jq '.data.node.comments.nodes[0].databaseId')"; \
	if [ -z "$$COMMENT_ID" ] || [ "$$COMMENT_ID" = "null" ]; then \
		echo "ERROR: could not locate the lead review comment for thread $$THREAD_ID."; \
		exit 1; \
	fi; \
	echo "Replying to $$THREAD_ID with reason and resolving..."; \
	gh api repos/$$REPO/pulls/$$PR_NUM/comments/$$COMMENT_ID/replies \
		-f body="$$REASON" > /dev/null || { \
		echo "ERROR: the reply to $$THREAD_ID failed; the thread is left unresolved."; \
		exit 1; }; \
	gh api graphql \
		-F threadId="$$THREAD_ID" \
		-f query='mutation($$threadId:ID!) { resolveReviewThread(input:{threadId:$$threadId}) { thread { isResolved } } }' \
		--jq '.data.resolveReviewThread.thread.isResolved' > /dev/null; \
	echo "Resolved $$THREAD_ID"

# Friendly alias: resolve one review thread with an explicit reason.
# Usage:
#   make resolve-review-comment THREAD_ID=<thread_id> REASON="<resolution note>" [PR=<number>]
# For convenience, REVIEW_THREAD_ID is accepted as an alternative to THREAD_ID.
resolve-review-comment:
	@THREAD_VAL="$(THREAD_ID)"; \
	if [ -z "$$THREAD_VAL" ]; then \
		THREAD_VAL="$(REVIEW_THREAD_ID)"; \
	fi; \
	if [ -z "$$THREAD_VAL" ]; then \
		echo "ERROR: THREAD_ID (or REVIEW_THREAD_ID) is required."; \
		echo "Usage: make resolve-review-comment THREAD_ID=<thread_id> REASON=\"<resolution note>\" [PR=<number>]"; \
		exit 2; \
	fi; \
	$(MAKE) pr-resolve-thread THREAD_ID="$$THREAD_VAL" PR="$(PR)"

# Deprecated: bulk resolution is intentionally disabled.
# Resolve threads one by one with a reason via pr-resolve-thread.
pr-resolve-threads:
	@echo "ERROR: bulk thread resolution is disabled."; \
	echo "Use one-by-one resolution with an explicit reason:"; \
	echo "  make pr-review-unresolved"; \
	echo "  make pr-resolve-thread THREAD_ID=<thread_id> REASON=\"<resolution note>\" [PR=<number>]"; \
	exit 2

doxygen: $(BUILD_DIR)/CMakeCache.txt ## The Doxygen API docs in build/docs
	cmake -S . -B $(BUILD_DIR) -G Ninja \
		-DCMAKE_CXX_COMPILER=$(CXX) \
		-DCMAKE_C_COMPILER=$(CC) \
		-DCMAKE_CXX_SCAN_FOR_MODULES=ON \
		-DCMAKE_BUILD_TYPE=Release \
		-DDEDEKIND_ENABLE_DOUBLE_REAL_PROXY=ON \
		$(CMAKE_EXTRA_ARGS)
	cmake --build $(BUILD_DIR) --target docs --parallel $(JOBS)

papers: ## The paper with its bibliography, as CI builds it
	$(MAKE) paper-jlt

# The Jlt paper, full build with bibliography: docs/paper/jlt/jlt.pdf.
paper-jlt: ## The Jlt paper with its bibliography
	$(MAKE) -C docs/paper jlt
