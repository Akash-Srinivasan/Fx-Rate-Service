.PHONY: build run test clean docker-build docker-up docker-down install format

# Build the project
build:
	cabal build

# Run the API server
run:
	cabal run fx-rate-service

# Run the fetcher
fetch:
	cabal run fx-rate-fetcher

# Run tests
test:
	cabal test

# Clean build artifacts
clean:
	cabal clean

# Install dependencies
install:
	cabal update
	cabal build --only-dependencies

# Format code
format:
	ormolu --mode inplace $$(find src app test -name '*.hs')

# Docker commands
docker-build:
	docker-compose build

docker-up:
	docker-compose up -d

docker-down:
	docker-compose down

docker-logs:
	docker-compose logs -f

# Database commands
db-create:
	createdb fxrates

db-drop:
	dropdb fxrates

db-reset: db-drop db-create

# Development
watch:
	ghcid --command="cabal repl"

# Help
help:
	@echo "Available commands:"
	@echo "  make build        - Build the project"
	@echo "  make run          - Run the API server"
	@echo "  make fetch        - Run the rate fetcher"
	@echo "  make test         - Run tests"
	@echo "  make clean        - Clean build artifacts"
	@echo "  make install      - Install dependencies"
	@echo "  make format       - Format code with ormolu"
	@echo "  make docker-build - Build Docker images"
	@echo "  make docker-up    - Start services with Docker"
	@echo "  make docker-down  - Stop Docker services"
	@echo "  make docker-logs  - View Docker logs"
	@echo "  make db-create    - Create database"
	@echo "  make db-drop      - Drop database"
	@echo "  make db-reset     - Reset database"
	@echo "  make watch        - Auto-reload with ghcid"
