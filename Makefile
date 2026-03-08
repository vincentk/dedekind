# Project Variables
PROJECT_NAME := dedekind
BUILD_DIR    := build
INSTALL_DIR  := dist

# Toolchain Paths (Homebrew Intel Mac)
LLVM_ROOT    := /usr/local/opt/llvm
LLVM_CXX_LIB := $(LLVM_ROOT)/lib/c++
SDK_PATH     := $(shell xcrun --sdk macosx --show-sdk-path)

# Compilers
CXX := $(LLVM_ROOT)/bin/clang++
CC  := $(LLVM_ROOT)/bin/clang

# Profiling and coverage
PROFDATA := $(LLVM_ROOT)/bin/llvm-profdata
COV      := $(LLVM_ROOT)/bin/llvm-cov

# Flags
CXXFLAGS    := "-stdlib=libc++ -isysroot $(SDK_PATH)"
LDFLAGS     := "-L$(LLVM_CXX_LIB) -L$(LLVM_ROOT)/lib -lc++ -Wl,-rpath,$(LLVM_CXX_LIB),-rpath,$(LLVM_ROOT)/lib"

.PHONY: all clean compile test install format doc coverage

# Default: mvn compile
all: compile

# mvn clean
clean:
	rm -rf $(BUILD_DIR) $(INSTALL_DIR)

# mvn initialize (Configure CMake)
$(BUILD_DIR)/CMakeCache.txt:
	cmake -S . -B $(BUILD_DIR) -G Ninja \
		-DCMAKE_CXX_COMPILER=$(CXX) \
		-DCMAKE_C_COMPILER=$(CC) \
		-DCMAKE_CXX_FLAGS=$(CXXFLAGS) \
		-DCMAKE_EXE_LINKER_FLAGS=$(LDFLAGS) \
		-DCMAKE_CXX_SCAN_FOR_MODULES=ON

# mvn compile
compile: $(BUILD_DIR)/CMakeCache.txt
	cmake --build $(BUILD_DIR)

# mvn test
test: compile
	ctest --test-dir $(BUILD_DIR) --output-on-failure

# mvn install
install: compile
	cmake --install $(BUILD_DIR) --prefix $(INSTALL_DIR)

# mvn checkstyle (Format)
format:
	find src -name "*.cpp" -o -name "*.cppm" | xargs $(LLVM_ROOT)/bin/clang-format -i

# mvn site (Generate Documentation)
doc: compile
	cmake --build $(BUILD_DIR) --target docs
	open $(BUILD_DIR)/html/index.html


# The coverage report depends on the tests having been run
coverage: test
	@echo "Generating LLVM coverage report..."
	# 1. Run with the profile environment variable to get the data
	LLVM_PROFILE_FILE="$(BUILD_DIR)/dedekind.profraw" ./$(BUILD_DIR)/dedekind_test
	
	# 2. Merge and index
	$(PROFDATA) merge -sparse $(BUILD_DIR)/dedekind.profraw -o $(BUILD_DIR)/dedekind.profdata
	
	# 3. Generate HTML (Focusing only on your source, ignoring Catch2/GTest)
	$(COV) show ./$(BUILD_DIR)/dedekind_test \
		-instr-profile=$(BUILD_DIR)/dedekind.profdata \
		-ignore-filename-regex=".*_deps/.*" \
		-format=html \
		-output-dir=$(BUILD_DIR)/coverage
	
	# 4. Summary
	$(COV) report ./$(BUILD_DIR)/dedekind_test -instr-profile=$(BUILD_DIR)/dedekind.profdata -ignore-filename-regex=".*_deps/.*"
	
	@echo "Report generated at $(BUILD_DIR)/coverage/index.html"
	open $(BUILD_DIR)/coverage/index.html
