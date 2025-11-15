# Railway Quick Start Guide

## Deploy in 5 Minutes

### Option 1: One-Click Deploy (Web UI)

1. **Fork/Clone this repo** to your GitHub account

2. **Go to Railway** → [railway.app/new](https://railway.app/new)

3. **Deploy from GitHub** → Select this repository

4. **Add Database** → Click "+ New" → Select "PostgreSQL"

5. **Set Variables** → Go to your service → Variables tab:
   ```
   PORT=8080
   LOG_LEVEL=INFO
   FETCH_INTERVAL=15
   DB_HOST=${{Postgres.PGHOST}}
   DB_PORT=${{Postgres.PGPORT}}
   DB_NAME=${{Postgres.PGDATABASE}}
   DB_USER=${{Postgres.PGUSER}}
   DB_PASSWORD=${{Postgres.PGPASSWORD}}
   ```

6. **Deploy!** Railway will automatically build and deploy

7. **Test** → Open your app URL and visit `/health`

### Option 2: CLI Deploy

```bash
# Install Railway CLI
npm install -g @railway/cli

# Run deployment script
./deploy-railway.sh

# Or manually:
railway login
railway init
railway add --database postgresql
railway variables set PORT=8080 LOG_LEVEL=INFO FETCH_INTERVAL=15
railway up
```

## Adding the Worker Service

The worker service (`fx-rate-fetcher`) fetches exchange rates in the background.

### Via Web UI:
1. In your Railway project, click "+ New"
2. Select "GitHub Repo" → Same repository
3. Go to Settings → Change start command to: `/usr/local/bin/start-railway.sh fx-rate-fetcher`
4. Add same database environment variables

### Via CLI:
```bash
# In a separate service deployment
railway up
# Then in Railway dashboard, override start command
```

## Environment Variables Quick Reference

**Required:**
- `PORT` - API server port (default: 8080)
- `DB_HOST` - Use `${{Postgres.PGHOST}}`
- `DB_PORT` - Use `${{Postgres.PGPORT}}`
- `DB_NAME` - Use `${{Postgres.PGDATABASE}}`
- `DB_USER` - Use `${{Postgres.PGUSER}}`
- `DB_PASSWORD` - Use `${{Postgres.PGPASSWORD}}`

**Optional:**
- `LOG_LEVEL` - DEBUG, INFO, WARN, ERROR (default: INFO)
- `FETCH_INTERVAL` - Minutes between fetches (default: 15)

## Testing Your Deployment

```bash
# Health check
curl https://your-app.railway.app/health

# Get exchange rate
curl https://your-app.railway.app/rates/USD-EUR

# Get live rates
curl https://your-app.railway.app/rates/live/USD
```

## View Logs

**Web UI:** Project → Service → Deployments → Click deployment → View Logs

**CLI:**
```bash
railway logs
```

## Common Issues

**"Database connection failed"**
- Ensure PostgreSQL service is running in your project
- Verify DB_ environment variables are set correctly
- Check services are in the same Railway project

**"Health check timeout"**
- First deployment takes 2-3 minutes for DB initialization
- Check logs to see application status
- Ensure `/health` endpoint is accessible

**"Build failed"**
- Check Railway build logs
- Verify Dockerfile syntax
- Ensure all dependencies in `fx-rate-service.cabal`

## Need Help?

- Full docs: See [RAILWAY.md](./RAILWAY.md)
- Railway docs: https://docs.railway.app
- Railway Discord: https://discord.gg/railway

## Architecture

```
┌─────────────────┐
│  Railway Cloud  │
├─────────────────┤
│                 │
│  Web Service    │  ← fx-rate-service (API)
│  (Port 8080)    │
│                 │
│  Worker Service │  ← fx-rate-fetcher (background)
│                 │
│  PostgreSQL     │  ← Database
│  (Port 5432)    │
│                 │
└─────────────────┘
```

## What Gets Deployed

- **Web Service**: REST API server responding to HTTP requests
- **Worker Service**: Background job fetching rates every 15 minutes
- **PostgreSQL**: Database storing exchange rates and historical data
- **Health Checks**: Automatic monitoring and restart on failures
- **Auto-scaling**: Handles traffic spikes automatically

## Cost Estimate

- **Free Tier**: $5/month credit - good for development
- **Hobby Plan**: $5/month - good for personal projects
- **Pro Plan**: $20/month - production-ready with backups

## Next Steps After Deploy

1. ✅ Verify `/health` returns `{"status":"ok"}`
2. ✅ Check worker logs to confirm rates are being fetched
3. ✅ Test API endpoints with sample requests
4. ✅ Set up custom domain (optional)
5. ✅ Configure monitoring/alerts
6. ✅ Review and optimize FETCH_INTERVAL for your needs
