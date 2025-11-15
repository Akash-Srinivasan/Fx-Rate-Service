#!/bin/bash
# Railway Deployment Helper Script
# This script helps you deploy the FX Rate Service to Railway

set -e

echo "==================================="
echo "FX Rate Service - Railway Deployment"
echo "==================================="
echo ""

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Check if Railway CLI is installed
if ! command -v railway &> /dev/null; then
    echo -e "${RED}Error: Railway CLI is not installed${NC}"
    echo ""
    echo "Please install it first:"
    echo "  npm install -g @railway/cli"
    echo "  or"
    echo "  brew install railway"
    echo ""
    exit 1
fi

echo -e "${GREEN}✓ Railway CLI found${NC}"

# Check if logged in
if ! railway whoami &> /dev/null; then
    echo -e "${YELLOW}Please login to Railway first${NC}"
    railway login
fi

echo -e "${GREEN}✓ Logged in to Railway${NC}"
echo ""

# Menu
echo "What would you like to do?"
echo "1) Create new Railway project and deploy"
echo "2) Deploy to existing Railway project"
echo "3) Add PostgreSQL database to project"
echo "4) Set environment variables"
echo "5) View deployment status"
echo "6) View logs"
echo "7) Exit"
echo ""
read -p "Enter your choice (1-7): " choice

case $choice in
    1)
        echo ""
        echo -e "${YELLOW}Creating new Railway project...${NC}"
        railway init

        echo ""
        echo -e "${YELLOW}Adding PostgreSQL database...${NC}"
        railway add --database postgresql

        echo ""
        echo -e "${YELLOW}Setting environment variables...${NC}"
        railway variables set PORT=8080
        railway variables set LOG_LEVEL=INFO
        railway variables set FETCH_INTERVAL=15

        echo ""
        echo -e "${GREEN}Environment variables set!${NC}"
        echo "Note: Database variables (DB_HOST, DB_PORT, etc.) are automatically"
        echo "configured by Railway when you add PostgreSQL."

        echo ""
        read -p "Deploy now? (y/n): " deploy_now
        if [[ $deploy_now == "y" || $deploy_now == "Y" ]]; then
            railway up
            echo ""
            echo -e "${GREEN}✓ Deployment initiated!${NC}"
            railway status
        fi
        ;;

    2)
        echo ""
        echo -e "${YELLOW}Linking to existing project...${NC}"
        railway link

        echo ""
        read -p "Deploy now? (y/n): " deploy_now
        if [[ $deploy_now == "y" || $deploy_now == "Y" ]]; then
            railway up
            echo ""
            echo -e "${GREEN}✓ Deployment initiated!${NC}"
            railway status
        fi
        ;;

    3)
        echo ""
        echo -e "${YELLOW}Adding PostgreSQL database...${NC}"
        railway add --database postgresql
        echo ""
        echo -e "${GREEN}✓ PostgreSQL added!${NC}"
        echo "Database connection variables are automatically available in your services."
        ;;

    4)
        echo ""
        echo -e "${YELLOW}Setting environment variables...${NC}"
        echo ""

        read -p "Enter PORT (default: 8080): " port
        port=${port:-8080}
        railway variables set PORT=$port

        read -p "Enter LOG_LEVEL (default: INFO): " log_level
        log_level=${log_level:-INFO}
        railway variables set LOG_LEVEL=$log_level

        read -p "Enter FETCH_INTERVAL in minutes (default: 15): " fetch_interval
        fetch_interval=${fetch_interval:-15}
        railway variables set FETCH_INTERVAL=$fetch_interval

        echo ""
        echo -e "${GREEN}✓ Environment variables updated!${NC}"

        echo ""
        read -p "Do you need to set database variables manually? (y/n): " set_db
        if [[ $set_db == "y" || $set_db == "Y" ]]; then
            echo ""
            echo "Tip: If you added PostgreSQL via Railway, use these values:"
            echo '  DB_HOST=${{Postgres.PGHOST}}'
            echo '  DB_PORT=${{Postgres.PGPORT}}'
            echo '  DB_NAME=${{Postgres.PGDATABASE}}'
            echo '  DB_USER=${{Postgres.PGUSER}}'
            echo '  DB_PASSWORD=${{Postgres.PGPASSWORD}}'
            echo ""

            read -p "Enter DB_HOST: " db_host
            railway variables set DB_HOST="$db_host"

            read -p "Enter DB_PORT (default: 5432): " db_port
            db_port=${db_port:-5432}
            railway variables set DB_PORT=$db_port

            read -p "Enter DB_NAME: " db_name
            railway variables set DB_NAME="$db_name"

            read -p "Enter DB_USER: " db_user
            railway variables set DB_USER="$db_user"

            read -p "Enter DB_PASSWORD: " -s db_password
            echo ""
            railway variables set DB_PASSWORD="$db_password"

            echo ""
            echo -e "${GREEN}✓ Database variables set!${NC}"
        fi
        ;;

    5)
        echo ""
        railway status
        ;;

    6)
        echo ""
        echo -e "${YELLOW}Streaming logs (Ctrl+C to exit)...${NC}"
        echo ""
        railway logs
        ;;

    7)
        echo ""
        echo "Goodbye!"
        exit 0
        ;;

    *)
        echo -e "${RED}Invalid choice${NC}"
        exit 1
        ;;
esac

echo ""
echo -e "${GREEN}Done!${NC}"
echo ""
echo "Useful commands:"
echo "  railway status  - Check deployment status"
echo "  railway logs    - View application logs"
echo "  railway open    - Open project in browser"
echo "  railway link    - Link to existing project"
echo ""
