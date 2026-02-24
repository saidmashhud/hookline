.PHONY: all build test c-build c-test eunit lint e2e-integration e2e-embedded e2e-traces \
	dev docker-up docker-down ops-up ops-down prod-gate release-baseline clean

# ── Build ─────────────────────────────────────────────────────────────────

all: build

build: c-build
	rebar3 compile

c-build:
	$(MAKE) -C c

# ── Tests ─────────────────────────────────────────────────────────────────

test: c-test eunit

c-test:
	$(MAKE) -C c test

eunit:
	rebar3 eunit

e2e-integration:
	./test/integration.sh

e2e-embedded:
	./test/embedded-mode.sh

e2e-traces:
	./test/trace-propagation.sh

# ── Lint ──────────────────────────────────────────────────────────────────

lint:
	rebar3 dialyzer

# ── Docker ────────────────────────────────────────────────────────────────

docker-up:
	docker compose up -d --build

docker-down:
	docker compose down

ops-up:
	docker compose up -d --build

ops-down:
	docker compose down

prod-gate:
	./test/production-readiness.sh

release-baseline:
	bash scripts/release/tag-baseline.sh

# ── Dev ───────────────────────────────────────────────────────────────────

dev: c-build
	@echo "Starting hl_store..."
	@mkdir -p /tmp/hl_data
	@./c/build/hl_store /tmp/hl_store.sock /tmp/hl_data &
	@sleep 0.3
	HL_API_KEY=dev-secret rebar3 shell

# ── Clean ─────────────────────────────────────────────────────────────────

clean:
	$(MAKE) -C c clean
	rebar3 clean
