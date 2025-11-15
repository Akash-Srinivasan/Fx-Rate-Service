#!/bin/bash
set -e

echo "=== FX Rate Service - Railway Startup ==="
echo "Starting at: $(date)"

# Function to wait for database
wait_for_db() {
    echo "Waiting for PostgreSQL to be ready..."
    local max_attempts=30
    local attempt=1

    while [ $attempt -le $max_attempts ]; do
        if pg_isready -h "$DB_HOST" -p "$DB_PORT" -U "$DB_USER" > /dev/null 2>&1; then
            echo "PostgreSQL is ready!"
            return 0
        fi

        echo "Attempt $attempt/$max_attempts: PostgreSQL not ready yet, waiting..."
        sleep 2
        attempt=$((attempt + 1))
    done

    echo "ERROR: PostgreSQL failed to become ready after $max_attempts attempts"
    return 1
}

# Print environment info (without sensitive data)
echo "Configuration:"
echo "  PORT: ${PORT:-8080}"
echo "  DB_HOST: ${DB_HOST:-not set}"
echo "  DB_PORT: ${DB_PORT:-5432}"
echo "  DB_NAME: ${DB_NAME:-not set}"
echo "  LOG_LEVEL: ${LOG_LEVEL:-INFO}"
echo "  FETCH_INTERVAL: ${FETCH_INTERVAL:-15} minutes"

# Wait for database if DB_HOST is set
if [ -n "$DB_HOST" ]; then
    wait_for_db || exit 1
else
    echo "WARNING: DB_HOST not set, skipping database readiness check"
fi

# Determine which service to start
SERVICE="${1:-fx-rate-service}"

echo "Starting service: $SERVICE"
echo "================================"

# Start the appropriate service
exec "$SERVICE"
