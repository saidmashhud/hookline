.PHONY: all build test c-build c-test eunit dev clean

# Build C daemon and Erlang apps
all: build

build: c-build
	rebar3 compile

# Run all tests (C + Erlang unit)
test: c-test eunit

c-build:
	$(MAKE) -C c

c-test:
	$(MAKE) -C c test

eunit:
	rebar3 eunit

# Start gp_store daemon + Erlang shell for local development
dev: c-build
	@echo "Starting gp_store..."
	@mkdir -p /tmp/gp_data
	@./c/build/gp_store /tmp/gp_store.sock /tmp/gp_data &
	@sleep 0.3
	GP_API_KEY=dev-secret rebar3 shell

clean:
	$(MAKE) -C c clean
	rebar3 clean
