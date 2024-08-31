.PHONY: compile

compile:
	rebar3 compile

WAMR_VERSION = 2.1.2
WAMR_DIR = _build/wamr

UNAME_S := $(shell uname -s)
UNAME_M := $(shell uname -m)

ifeq ($(UNAME_S),Darwin)
    WAMR_BUILD_PLATFORM = darwin
    ifeq ($(UNAME_M),arm64)
        WAMR_BUILD_TARGET = AARCH64
    else
        WAMR_BUILD_TARGET = X86_64
    endif
else
    WAMR_BUILD_PLATFORM = linux
    WAMR_BUILD_TARGET = AARCH64
endif

# Clone the WAMR repository at our target release
$(WAMR_DIR):
	git clone \
		https://github.com/bytecodealliance/wasm-micro-runtime.git \
		$(WAMR_DIR) \
		-b WAMR-$(WAMR_VERSION) \
		--single-branch

# Build the WAMR library
wamr: $(WAMR_DIR)
	cmake \
		-DCMAKE_BUILD_TYPE=Debug \
		-DWASM_ENABLE_LOG=1 \
		-S $(WAMR_DIR) \
		-B $(WAMR_DIR)/lib \
		-DWAMR_BUILD_TARGET=$(WAMR_BUILD_TARGET) \
		-DWAMR_BUILD_PLATFORM=$(WAMR_BUILD_PLATFORM) \
		-DWAMR_BUILD_LIBC_WASI=0 \
		-DWAMR_BUILD_MEMORY64=1 \
		-DWAMR_DISABLE_HW_BOUND_CHECK=1 \
		-DWAMR_BUILD_EXCE_HANDLING=1 \
		-DWAMR_BUILD_SHARED_MEMORY=0 \
		-DWAMR_BUILD_AOT=0 \
		-DWAMR_BUILD_JIT=0 \
		-DWAMR_ENABLE_MEMORY_PROFILING=1
	make -C $(WAMR_DIR)/lib -j

clean:
	rm -rf $(WAMR_DIR) lib

# Add a new target to print the library path
print-lib-path:
	@echo $(CURDIR)/lib/libvmlib.a