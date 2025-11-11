# Single-stage build - using full haskell image (has PostgreSQL libs)
FROM haskell:9.6.3

WORKDIR /app

# Copy cabal files first for better caching
COPY fx-rate-service.cabal ./
RUN cabal update && cabal build --only-dependencies

# Copy source code
COPY . .

# Build the application
RUN cabal build all

# Find executables and make them easy to run
RUN cp $(find dist-newstyle -name 'fx-rate-service' -type f -executable) /usr/local/bin/fx-rate-service && \
    cp $(find dist-newstyle -name 'fx-rate-fetcher' -type f -executable) /usr/local/bin/fx-rate-fetcher

# Expose API port
EXPOSE 8080

# Default command (can be overridden in docker-compose)
CMD ["fx-rate-service"]
