.PHONY: compile setup-cu start-cu stop-cu

compile:
	rebar3 compile

WAMR_VERSION = 2.2.0
WAMR_DIR = _build/wamr

ifdef HB_DEBUG
	WAMR_FLAGS = -DWAMR_ENABLE_LOG=1 -DWAMR_BUILD_DUMP_CALL_STACK=1 -DCMAKE_BUILD_TYPE=Debug
else
	WAMR_FLAGS = -DCMAKE_BUILD_TYPE=Release
endif

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
    WAMR_BUILD_TARGET = X86_64
endif

wamr: $(WAMR_DIR)/lib/libvmlib.a

debug: debug-clean $(WAMR_DIR)
	HB_DEBUG=1 make $(WAMR_DIR)/lib/libvmlib.a
	CFLAGS="-DHB_DEBUG=1" rebar3 compile

debug-clean:
	rm -rf priv
	rm -rf $(WAMR_DIR)

# Clone the WAMR repository at our target release
$(WAMR_DIR):
	git clone \
		https://github.com/bytecodealliance/wasm-micro-runtime.git \
		$(WAMR_DIR) \
		-b WAMR-$(WAMR_VERSION) \
		--single-branch

$(WAMR_DIR)/lib/libvmlib.a: $(WAMR_DIR)
	sed -i '742a tbl_inst->is_table64 = 1;' ./_build/wamr/core/iwasm/aot/aot_runtime.c; \
	cmake \
		$(WAMR_FLAGS) \
		-S $(WAMR_DIR) \
		-B $(WAMR_DIR)/lib \
		-DWAMR_BUILD_TARGET=$(WAMR_BUILD_TARGET) \
		-DWAMR_BUILD_PLATFORM=$(WAMR_BUILD_PLATFORM) \
		-DWAMR_BUILD_MEMORY64=1 \
		-DWAMR_DISABLE_HW_BOUND_CHECK=1 \
		-DWAMR_BUILD_EXCE_HANDLING=1 \
		-DWAMR_BUILD_SHARED_MEMORY=0 \
		-DWAMR_BUILD_AOT=1 \
		-DWAMR_BUILD_LIBC_WASI=0 \
		-DWAMR_BUILD_FAST_INTERP=0 \
		-DWAMR_BUILD_INTERP=1 \
		-DWAMR_BUILD_JIT=0 \
		-DWAMR_BUILD_FAST_JIT=0 \
        -DWAMR_BUILD_DEBUG_AOT=1 \
        -DWAMR_BUILD_TAIL_CALL=1 \
        -DWAMR_BUILD_AOT_STACK_FRAME=1 \
        -DWAMR_BUILD_MEMORY_PROFILING=1 \
        -DWAMR_BUILD_DUMP_CALL_STACK=1
	make -C $(WAMR_DIR)/lib -j8

clean:
	rebar3 clean

# Add a new target to print the library path
print-lib-path:
	@echo $(CURDIR)/lib/libvmlib.a

# Set up CU environment
setup-cu:
	@# Check if Node.js is installed
	@if ! command -v node > /dev/null; then \
		echo "Error: Node.js is not installed. Please install Node.js before continuing."; \
		echo "For Ubuntu/Debian, you can install it with:"; \
		echo "  curl -fsSL https://deb.nodesource.com/setup_22.x | sudo -E bash - && \\"; \
		echo "  apt-get install -y nodejs && \\"; \
		echo "  node -v && npm -v"; \
		exit 1; \
	fi
	@if [ ! -d "$(CURDIR)/cu" ]; then \
		echo "Cloning CU repository..."; \
		tmp_dir=$$(mktemp -d); \
		git clone --depth=1 -b tillathehun0/cu-experimental https://github.com/permaweb/ao.git $$tmp_dir && \
		mkdir -p $(CURDIR)/cu && \
		cp -r $$tmp_dir/servers/cu/* $(CURDIR)/cu/ && \
		rm -rf $$tmp_dir && \
		echo "Extracted servers/cu to $(CURDIR)/cu"; \
	fi
	@if [ ! -f "$(CURDIR)/cu/.env" ]; then \
		cd $(CURDIR)/cu && \
			npx --yes @permaweb/wallet >> wallet.json && \
			echo 'NODE_CONFIG_ENV="development"' > .env && \
			echo "WALLET_FILE=./wallet.json" >> .env && \
			echo "HB_URL=http://localhost:10000" >> .env && \
			echo "UNIT_MODE=hbu" >> .env && \
			echo "PORT=6363" >> .env; \
	fi
	@cd $(CURDIR)/cu && npm install

# Start the CU server
start-cu:
	@echo "Starting CU server..."
	@cd $(CURDIR)/cu && npm run dev > /dev/null 2>&1 &
	@sleep 3 # Give it a moment to start
	@lsof -i :6363 -t | head -1 > $(CURDIR)/cu/.cu_server.pid
	@if [ -s "$(CURDIR)/cu/.cu_server.pid" ]; then \
		echo "CU server running with PID $$(cat $(CURDIR)/cu/.cu_server.pid)"; \
	else \
		echo "Warning: Could not find CU server process on port 6363"; \
	fi

# Stop the CU server
stop-cu:
	@if [ -f "$(CURDIR)/cu/.cu_server.pid" ]; then \
		CU_PID=$$(cat $(CURDIR)/cu/.cu_server.pid); \
		echo "Stopping Node.js process with PID $${CU_PID}"; \
		kill -9 $${CU_PID} 2>/dev/null || echo "Node process already stopped"; \
		rm -f $(CURDIR)/cu/.cu_server.pid; \
		echo "CU server stopped"; \
	else \
		echo "No CU server PID file found"; \
	fi
