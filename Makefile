# For all practical purposes, this Makefile serves as a convenient 
# wrapper around CMake and Ninja, for local development.
# The reference build is the GitHub build action.

# Project Variables
BUILD_DIR     := build
VENV_DIR      := .venv
NOTEBOOKS_DIR := docs/python/notebooks
LLVM_ROOT    ?= /usr/local/opt/llvm
CLANG_FORMAT ?= $(LLVM_ROOT)/bin/clang-format
CMAKE_EXTRA_ARGS ?=
DOCS_DIR     := docs/report
DOCS_MAIN    := report
FILTER_GVPR  := $(DOCS_DIR)/figures/filter.gvpr
DOT_FILE     := $(DOCS_DIR)/figures/dedekind_module_dependencies.dot

# GNU make provides built-in defaults (CXX=c++, CC=cc). Override only those
# built-in defaults so Homebrew LLVM is used by default, while preserving any
# explicit command-line or environment overrides from the user.
ifeq ($(origin CXX), default)
CXX := $(LLVM_ROOT)/bin/clang++
endif
ifeq ($(origin CC), default)
CC := $(LLVM_ROOT)/bin/clang
endif

.PHONY: all clean compile test integration-test coverage python-coverage python-coverage-local ir-fixture-refresh ir-fixture-check format format-check install-hooks ci-install-doxygen-deps ci-install-report-deps doxygen dot doc report \
	ci-history ci-main pr-init pr-status pr-checks pr-watch pr-sync pr-review-comments pr-review-unresolved pr-resolve-thread pr-resolve-threads \
	check-review-comments resolve-review-comment issue-list jupyter

all: compile

clean:
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

compile: $(BUILD_DIR)/CMakeCache.txt
	cmake --build $(BUILD_DIR)

test: compile
	ctest --test-dir $(BUILD_DIR) --output-on-failure

integration-test: test
	python -m pip install --upgrade pip jupyter pandas numpy
	rm -rf $(BUILD_DIR)/python-editable
	CC="$(CC)" CXX="$(CXX)" \
	CMAKE_ARGS="-DCMAKE_C_COMPILER=$(CC) -DCMAKE_CXX_COMPILER=$(CXX) -DCMAKE_CXX_SCAN_FOR_MODULES=ON -DCMAKE_BUILD_TYPE=Release -DDEDEKIND_ENABLE_DOUBLE_REAL_PROXY=ON $(CMAKE_EXTRA_ARGS)" \
	SKBUILD_BUILD_DIR="$(BUILD_DIR)/python-editable" \
	python -m pip install -e . --no-deps
	mkdir -p $(BUILD_DIR)/python-notebooks
	@NOTEBOOK_DIR="docs/python/notebooks"; \
	NOTEBOOKS="$$(find $$NOTEBOOK_DIR -maxdepth 1 -type f -name '*.ipynb' | sort)"; \
	REPORT="$(BUILD_DIR)/python-notebooks/integration-summary.txt"; \
	: > "$$REPORT"; \
	if [ -z "$$NOTEBOOKS" ]; then \
		echo "ERROR: no notebooks found in $$NOTEBOOK_DIR"; \
		echo "ERROR: no notebooks found in $$NOTEBOOK_DIR" >> "$$REPORT"; \
		exit 2; \
	fi; \
	echo "Running notebook integration tests from $$NOTEBOOK_DIR" | tee -a "$$REPORT"; \
	FAILURES=0; \
	TOTAL=0; \
	for nb in $$NOTEBOOKS; do \
		TOTAL=$$((TOTAL + 1)); \
		name="$$(basename $$nb)"; \
		if ! python -c 'import json,sys; nb=json.load(open(sys.argv[1], encoding="utf-8")); to_text=lambda s: "".join(s) if isinstance(s, list) else (s if isinstance(s, str) else ""); ok=any(c.get("cell_type") == "code" and ("import dedekind" in to_text(c.get("source", "")) or "from dedekind import" in to_text(c.get("source", ""))) for c in nb.get("cells", [])); sys.exit(0 if ok else 1)' "$$nb"; then \
			echo "FAILED: $$name does not import dedekind in any code cell" | tee -a "$$REPORT"; \
			FAILURES=$$((FAILURES + 1)); \
			continue; \
		fi; \
		echo "Executing $$name" | tee -a "$$REPORT"; \
		if ! python -m jupyter nbconvert --to notebook --execute "$$nb" --output "$$name" --output-dir $(BUILD_DIR)/python-notebooks; then \
			echo "FAILED: $$name" | tee -a "$$REPORT"; \
			FAILURES=$$((FAILURES + 1)); \
		else \
			echo "PASSED: $$name" >> "$$REPORT"; \
		fi; \
	done; \
	if [ "$$FAILURES" -ne 0 ]; then \
		echo "Notebook integration failures: $$FAILURES/$$TOTAL" | tee -a "$$REPORT"; \
		exit 1; \
	fi; \
	echo "Notebook integration tests passed: $$TOTAL notebook(s)." | tee -a "$$REPORT"

# Create a local virtual environment at $(VENV_DIR) if it does not yet exist.
$(VENV_DIR)/bin/python:
	python3 -m venv $(VENV_DIR)

# Install dedekind into .venv and launch an interactive Jupyter server.
#
# This target:
#   1. Builds the C++ library (reuses an incremental build if already current).
#   2. Creates a local .venv if it does not exist.
#   3. Installs pip, jupyter, and the dedekind editable package into .venv.
#   4. Opens a Jupyter Notebook server pointing at $(NOTEBOOKS_DIR).
#
# Usage:
#   make jupyter            # first-time or after C++ source changes
#   make jupyter            # subsequent runs reuse the compiled artifacts
#
# Note: the dedicated $(BUILD_DIR)/python-jupyter build tree is wiped on each
# invocation to prevent stale module-map files from causing compile errors.
# This makes the editable install reliable at the cost of a one-time C++ relink.
jupyter: compile $(VENV_DIR)/bin/python
	$(VENV_DIR)/bin/pip install --quiet --upgrade pip jupyter
	rm -rf $(BUILD_DIR)/python-jupyter
	CC="$(CC)" CXX="$(CXX)" \
	CMAKE_ARGS="-DCMAKE_C_COMPILER=$(CC) -DCMAKE_CXX_COMPILER=$(CXX) -DCMAKE_CXX_SCAN_FOR_MODULES=ON -DCMAKE_BUILD_TYPE=Release -DDEDEKIND_ENABLE_DOUBLE_REAL_PROXY=ON $(CMAKE_EXTRA_ARGS)" \
	SKBUILD_BUILD_DIR="$(BUILD_DIR)/python-jupyter" \
	$(VENV_DIR)/bin/pip install -e .
	$(VENV_DIR)/bin/jupyter notebook $(NOTEBOOKS_DIR)

coverage: compile
	@echo "Running tests with profile environment..."
	rm -f $(BUILD_DIR)/*.profraw
	# Set the variable for the duration of the ctest command
	ctest --test-dir $(BUILD_DIR) --output-on-failure
	
	@echo "Processing coverage..."
	cmake --build $(BUILD_DIR) --target generate_coverage

# Run Python tests with coverage collection and produce an XML report for Codecov.
# Uses the repo-managed housekeeping helper for auditable approvals.
python-coverage:
	bash .github/copilot/housekeeping/python-coverage.sh

# Local convenience alias (same behavior as python-coverage).
python-coverage-local: python-coverage

ir-fixture-refresh:
	python .github/copilot/housekeeping/pruning-ir-fixture.py refresh

ir-fixture-check:
	python .github/copilot/housekeeping/pruning-ir-fixture.py check


format:
	find src -name "*.cpp" -o -name "*.cppm" | xargs $(CLANG_FORMAT) -i

format-check:
	find src -name "*.cpp" -o -name "*.cppm" | xargs $(CLANG_FORMAT) --dry-run --Werror

install-hooks:
	git config core.hooksPath .githooks
	chmod +x .githooks/pre-push
	@echo "Installed repo hooks from .githooks/"

# CI-only helper: install packages required for Doxygen generation.
ci-install-doxygen-deps:
	@if ! command -v apt-get >/dev/null 2>&1; then \
		echo "ERROR: ci-install-doxygen-deps requires apt-get (intended for Ubuntu CI runners)."; \
		exit 2; \
	fi
	sudo apt-get update
	sudo apt-get install -y doxygen graphviz

# CI-only helper: install packages required for report/LaTeX generation.
ci-install-report-deps:
	@if ! command -v apt-get >/dev/null 2>&1; then \
		echo "ERROR: ci-install-report-deps requires apt-get (intended for Ubuntu CI runners)."; \
		exit 2; \
	fi
	sudo apt-get update
	sudo apt-get install -y biber texlive-latex-base texlive-latex-extra texlive-bibtex-extra texlive-pictures texlive-plain-generic texlive-fonts-recommended

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

ci-history:
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

ci-main:
	@$(MAKE) ci-history BRANCH=main LIMIT=5

# Initialize an issue-scoped branch, empty checkpoint commit, remote push, and draft PR.
# PR title and body are populated from the GitHub issue content.
# Usage:
#   make pr-init ISSUES="234 236"
# Optional:
#   TYPE=feat|fix|docs|chore   (default: feat)
#   BASE=main                  (default: main)
pr-init:
	@ISSUES="$(ISSUES)" TYPE="$(TYPE)" BASE="$(BASE)" \
		bash .github/copilot/housekeeping/pr-init.sh

pr-status:
	gh pr view --json number,title,state,isDraft,url

pr-checks:
	gh pr checks || true

pr-watch:
	gh pr checks --watch || true

pr-sync:
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

# List inline PR review comments for the current PR (or PR=<number>).
pr-review-comments:
	@PR_NUM="$(PR)"; \
	if [ -z "$$PR_NUM" ]; then \
		PR_NUM="$$(gh pr view --json number --jq .number)"; \
	fi; \
	REPO="$$(gh repo view --json nameWithOwner --jq .nameWithOwner)"; \
	echo "Listing review comments for PR #$$PR_NUM ($$REPO)..."; \
	gh api repos/$$REPO/pulls/$$PR_NUM/comments \
		--jq '.[] | "- " + .path + ":" + (.line|tostring) + " :: " + (.body | gsub("\\n"; " "))'

# Scan unresolved review threads on the current PR (or PR=<number>).
pr-review-unresolved:
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
pr-resolve-thread:
	@PR_NUM="$(PR)"; \
	THREAD_ID="$(THREAD_ID)"; \
	REASON="$(REASON)"; \
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
		-f body="$$REASON" > /dev/null; \
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
	$(MAKE) pr-resolve-thread THREAD_ID="$$THREAD_VAL" REASON="$(REASON)" PR="$(PR)"

# Deprecated: bulk resolution is intentionally disabled.
# Resolve threads one by one with a reason via pr-resolve-thread.
pr-resolve-threads:
	@echo "ERROR: bulk thread resolution is disabled."; \
	echo "Use one-by-one resolution with an explicit reason:"; \
	echo "  make pr-review-unresolved"; \
	echo "  make pr-resolve-thread THREAD_ID=<thread_id> REASON=\"<resolution note>\" [PR=<number>]"; \
	exit 2

doxygen: $(BUILD_DIR)/CMakeCache.txt
	cmake -S . -B $(BUILD_DIR) -G Ninja \
		-DCMAKE_CXX_COMPILER=$(CXX) \
		-DCMAKE_C_COMPILER=$(CC) \
		-DCMAKE_CXX_SCAN_FOR_MODULES=ON \
		-DCMAKE_BUILD_TYPE=Release \
		-DDEDEKIND_ENABLE_DOUBLE_REAL_PROXY=ON \
		$(CMAKE_EXTRA_ARGS)
	cmake --build $(BUILD_DIR) --target docs

report:
	$(MAKE) -C $(DOCS_DIR) ci-check

# Generate build dependency graph without breaking the Ninja build
dot: $(BUILD_DIR)/CMakeCache.txt
	@mkdir -p $(DOCS_DIR)/figures
	#ninja -C build -t graph | \
	#gvpr -c 'N[match(label, ".*\.cppm") < 0]{delete($$G, $$)}' | \
	#sed -E 's|label="(.*/)?([^/]+)\.cppm"|label="\2"|g' | \
	#> $(DOT_FILE)
	ninja -C build -t graph | gvpr -f $(FILTER_GVPR) > $(DOT_FILE)
	dot -Tpdf $(DOT_FILE) -o $(DOCS_DIR)/figures/dedekind_deps.pdf


doc: dot
	cd $(DOCS_DIR) && pdflatex $(DOCS_MAIN).tex
	cd $(DOCS_DIR) && biber $(DOCS_MAIN)
	cd $(DOCS_DIR) && pdflatex $(DOCS_MAIN).tex
