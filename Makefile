# For all practical purposes, this Makefile serves as a convenient 
# wrapper around CMake and Ninja, for local development.
# The reference build is the GitHub build action.

# Project Variables
BUILD_DIR     := build
VENV_DIR      := .venv
NOTEBOOKS_DIR := docs/python/notebooks
LLVM_ROOT    ?= /usr/local/opt/llvm
CXX          ?= $(LLVM_ROOT)/bin/clang++
CC           ?= $(LLVM_ROOT)/bin/clang
CLANG_FORMAT ?= $(LLVM_ROOT)/bin/clang-format
CMAKE_EXTRA_ARGS ?=
DOCS_DIR     := docs/report
DOCS_MAIN    := report
FILTER_GVPR  := $(DOCS_DIR)/figures/filter.gvpr
DOT_FILE     := $(DOCS_DIR)/figures/dedekind_module_dependencies.dot

.PHONY: all clean compile test integration-test coverage format format-check install-hooks ci-install-doxygen-deps ci-install-report-deps doxygen dot doc report \
	ci-history ci-main pr-init pr-status pr-checks pr-watch pr-sync pr-review-comments pr-review-unresolved pr-resolve-thread pr-resolve-threads \
	jupyter

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
	python -m pip install --upgrade pip jupyter
	rm -rf $(BUILD_DIR)/python-editable
	CC="$(CC)" CXX="$(CXX)" \
	CMAKE_ARGS="-DCMAKE_C_COMPILER=$(CC) -DCMAKE_CXX_COMPILER=$(CXX) -DCMAKE_CXX_SCAN_FOR_MODULES=ON -DCMAKE_BUILD_TYPE=Release -DDEDEKIND_ENABLE_DOUBLE_REAL_PROXY=ON $(CMAKE_EXTRA_ARGS)" \
	SKBUILD_BUILD_DIR="$(BUILD_DIR)/python-editable" \
	python -m pip install -e .
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
		if ! grep -q 'import dedekind' "$$nb"; then \
			echo "FAILED: $$name does not import dedekind" | tee -a "$$REPORT"; \
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
# Usage:
#   make pr-init ISSUES="234 236"
# Optional:
#   TYPE=feat|fix|docs|chore   (default: feat)
#   BASE=main                  (default: main)
pr-init:
	@ISSUE_LIST_RAW="$(ISSUES)"; \
	TYPE_VAL="$(TYPE)"; \
	BASE_BRANCH="$(BASE)"; \
	if [ -z "$$ISSUE_LIST_RAW" ]; then \
		echo "ERROR: ISSUES is required."; \
		echo "Usage: make pr-init ISSUES=\"234 236\" [TYPE=feat] [BASE=main]"; \
		exit 2; \
	fi; \
	if [ -z "$$TYPE_VAL" ]; then \
		TYPE_VAL="feat"; \
	fi; \
	if [ -z "$$BASE_BRANCH" ]; then \
		BASE_BRANCH="main"; \
	fi; \
	if [ -n "$$(git status --porcelain)" ]; then \
		echo "ERROR: working tree must be clean before pr-init."; \
		echo "Commit, stash, or discard current changes before running this target."; \
		exit 2; \
	fi; \
	ISSUE_NUMBERS="$$(printf '%s\n' "$$ISSUE_LIST_RAW" | tr ' ,' '\n\n' | sed '/^$$/d')"; \
	ISSUE_SLUG="$$(printf '%s\n' "$$ISSUE_NUMBERS" | paste -sd- -)"; \
	ISSUE_REFS="$$(printf '%s\n' "$$ISSUE_NUMBERS" | sed 's/^/#/' | paste -sd'/' -)"; \
	BRANCH_NAME="$$TYPE_VAL/issues-$$ISSUE_SLUG"; \
	COMMIT_MSG="$$TYPE_VAL: initialize $$ISSUE_REFS scope"; \
	PR_TITLE="stub: initialize CI for issues $$ISSUE_REFS"; \
	PR_BODY="$$(printf '%s\n' \
		'## Summary' \
		"- initialize a draft PR and CI lane for issues $$ISSUE_REFS" \
		'- create the issue-scoped branch and an empty checkpoint commit for follow-up work' \
		'' \
		'## Scope' \
		'- branch setup for the selected issue batch' \
		'- initial empty checkpoint commit for auditable PR initialization' \
		"- draft PR creation against $$BASE_BRANCH" \
		'' \
		'## Notes' \
		'This is an initialization PR intended to start CI and collect subsequent commits for the selected issue scope.')"; \
	CURRENT_BRANCH="$$(git branch --show-current)"; \
	if [ "$$CURRENT_BRANCH" != "$$BRANCH_NAME" ]; then \
		if git show-ref --verify --quiet refs/heads/$$BRANCH_NAME; then \
			git switch "$$BRANCH_NAME"; \
		else \
			git switch -c "$$BRANCH_NAME"; \
		fi; \
	fi; \
	git commit --allow-empty -m "$$COMMIT_MSG"; \
	git push -u origin "$$BRANCH_NAME"; \
	if gh pr list --head "$$BRANCH_NAME" --state open --json number --jq 'length' | grep -qx '0'; then \
		gh pr create --draft --base "$$BASE_BRANCH" --head "$$BRANCH_NAME" --title "$$PR_TITLE" --body "$$PR_BODY"; \
	else \
		echo "Open PR already exists for $$BRANCH_NAME; skipping PR creation."; \
	fi

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
