# For all practical purposes, this Makefile serves as a convenient 
# wrapper around CMake and Ninja, for local development.
# The reference build is the GitHub build action.

# Project Variables
BUILD_DIR    := build
LLVM_ROOT    ?= /usr/local/opt/llvm
CXX          ?= $(LLVM_ROOT)/bin/clang++
CC           ?= $(LLVM_ROOT)/bin/clang
CLANG_FORMAT ?= $(LLVM_ROOT)/bin/clang-format
CMAKE_EXTRA_ARGS ?=
DOCS_DIR     := docs/report
DOCS_MAIN    := report
FILTER_GVPR  := $(DOCS_DIR)/figures/filter.gvpr
DOT_FILE     := $(DOCS_DIR)/figures/dedekind_module_dependencies.dot

.PHONY: all clean compile test coverage format format-check install-hooks doxygen dot doc report \
	ci-main pr-status pr-checks pr-watch pr-sync pr-review-comments pr-review-unresolved pr-resolve-thread pr-resolve-threads

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

# CI/PR workflow helpers (optimistic concurrency loop)
ci-main:
	gh run list --branch main --limit 5

pr-status:
	gh pr view --json number,title,state,isDraft,url

pr-checks:
	gh pr checks || true

pr-watch:
	gh pr checks --watch || true

pr-sync:
	git fetch --prune
	git status -sb
	gh pr view --json number,title,state,isDraft,url
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
