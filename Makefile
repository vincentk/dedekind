# For all practical purposes, this Makefile serves as a convenient 
# wrapper around CMake and Ninja, for local development.
# The reference build is the GitHub build action.

# Project Variables
BUILD_DIR    := build
LLVM_ROOT    := /usr/local/opt/llvm
CXX          := $(LLVM_ROOT)/bin/clang++
CC           := $(LLVM_ROOT)/bin/clang
DOCS_DIR     := docs/report
DOCS_MAIN    := report
FILTER_GVPR  := $(DOCS_DIR)/figures/filter.gvpr
DOT_FILE     := $(DOCS_DIR)/figures/dedekind_module_dependencies.dot

.PHONY: all clean compile test coverage format install-hooks

all: compile

clean:
	rm -rf $(BUILD_DIR)

# Minimal config: Only tell CMake which compiler to use.
$(BUILD_DIR)/CMakeCache.txt:
	cmake -S . -B $(BUILD_DIR) -G Ninja \
		-DCMAKE_CXX_COMPILER=$(CXX) \
		-DCMAKE_C_COMPILER=$(CC) \
		-DCMAKE_CXX_SCAN_FOR_MODULES=ON \
		-DCMAKE_BUILD_TYPE=Release

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
	find src -name "*.cpp" -o -name "*.cppm" | xargs $(LLVM_ROOT)/bin/clang-format -i

install-hooks:
	git config core.hooksPath .githooks
	chmod +x .githooks/pre-push
	@echo "Installed repo hooks from .githooks/"

doxygen: compile
	cmake --build $(BUILD_DIR) --target docs

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
