# Contributing to GatePulse

Thank you for your interest in contributing! 🎉

## Development Setup

### Prerequisites
- Erlang/OTP 27+
- rebar3
- GCC / Clang (for C daemon)
- Docker (optional, for integration tests)

### Getting Started

```bash
git clone https://github.com/saidmashhud/gatepulse.git
cd gatepulse

# Build everything and run all tests
make test

# Run locally
make dev
```

## Workflow

1. **Fork** the repository
2. **Create a branch**: `git checkout -b feat/my-feature`
3. **Write tests** for new functionality
4. **Ensure all tests pass**: `make test`
5. **Open a pull request** against `main`

## Code Style

### Erlang
- Follow existing module structure
- Use `snake_case` for functions/variables
- Keep functions small and focused
- Add `-spec` for exported functions
- No warnings in `rebar3 compile`

### C
- C11 standard
- `snake_case` for all identifiers
- `gp_` prefix for all public symbols
- No memory leaks (verify with valgrind)
- No compiler warnings (`-Wall -Wextra`)

## Commit Messages

Follow [Conventional Commits](https://www.conventionalcommits.org/):

```
feat: add rate limiting per endpoint
fix: correct CRC32 calculation for empty payloads
docs: update quick start guide
test: add queue lease reap tests
```

## Pull Request Guidelines

- PRs should be focused on a single change
- Include tests for new functionality
- Update documentation if needed
- Ensure CI passes before requesting review

## Questions?

Open a [GitHub Issue](https://github.com/saidmashhud/gatepulse/issues) or start a
[GitHub Discussion](https://github.com/saidmashhud/gatepulse/discussions).
