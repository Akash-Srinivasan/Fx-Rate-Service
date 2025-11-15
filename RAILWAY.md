# Railway Deployment Guide for FX Rate Service

This guide will help you deploy the FX Rate Service to Railway with PostgreSQL database support.

## Prerequisites

1. **Railway Account**: Sign up at [railway.app](https://railway.app)
2. **Railway CLI** (optional but recommended):
   ```bash
   npm install -g @railway/cli
   # Or with Homebrew
   brew install railway
   ```
3. **GitHub Repository**: Push your code to GitHub

## Quick Deploy (Recommended)

### Option 1: Deploy via Railway Dashboard

1. **Create New Project**
   - Go to [railway.app/new](https://railway.app/new)
   - Click "Deploy from GitHub repo"
   - Select your repository
   - Railway will auto-detect the Dockerfile

2. **Add PostgreSQL Database**
   - In your Railway project, click "+ New"
   - Select "Database" → "PostgreSQL"
   - Railway will automatically create a PostgreSQL instance

3. **Configure Environment Variables**
   - Click on your service
   - Go to "Variables" tab
   - Add the following variables:
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

4. **Deploy**
   - Railway will automatically deploy your service
   - The health check endpoint `/health` will be monitored
   - Your service will be available at the generated Railway URL

### Option 2: Deploy via Railway CLI

```bash
# Login to Railway
railway login

# Initialize new project
railway init

# Link to your Railway project (if already created)
# railway link

# Add PostgreSQL database
railway add --database postgresql

# Set environment variables
railway variables set PORT=8080
railway variables set LOG_LEVEL=INFO
railway variables set FETCH_INTERVAL=15

# Deploy
railway up

# Check deployment status
railway status

# View logs
railway logs
```

## Multi-Service Deployment (Web + Worker)

The FX Rate Service consists of two components:
- **Web Service** (`fx-rate-service`): REST API server
- **Worker Service** (`fx-rate-fetcher`): Background rate fetcher

### Deploy Both Services

1. **Create First Service (Web)**
   - Deploy the main service as described above
   - This will run `fx-rate-service` (defined in `railway.toml`)

2. **Add Worker Service**
   - In your Railway project, click "+ New"
   - Select "GitHub Repo" → same repository
   - In service settings, override the start command:
     ```
     fx-rate-fetcher
     ```
   - Set the same environment variables (they'll connect to the same database)

3. **Configure Service Dependencies**
   - Both services depend on PostgreSQL
   - Railway will handle the startup order automatically

## Environment Variables Reference

### Required Variables

| Variable | Description | Example |
|----------|-------------|---------|
| `PORT` | API server port | `8080` |
| `DB_HOST` | PostgreSQL host | `${{Postgres.PGHOST}}` |
| `DB_PORT` | PostgreSQL port | `${{Postgres.PGPORT}}` |
| `DB_NAME` | Database name | `${{Postgres.PGDATABASE}}` |
| `DB_USER` | Database user | `${{Postgres.PGUSER}}` |
| `DB_PASSWORD` | Database password | `${{Postgres.PGPASSWORD}}` |

### Optional Variables

| Variable | Description | Default |
|----------|-------------|---------|
| `LOG_LEVEL` | Logging level (DEBUG, INFO, WARN, ERROR) | `INFO` |
| `FETCH_INTERVAL` | Minutes between rate fetches | `15` |
| `EXCHANGERATE_API_KEY` | ExchangeRate API key (for future use) | - |
| `FIXER_API_KEY` | Fixer.io API key (for future use) | - |

## Database Initialization

The application automatically initializes the database schema on first run. The schema includes:
- `exchange_rates` table for storing historical rates
- `latest_rates` materialized view for quick lookups
- Indexes for optimal query performance

## Monitoring and Debugging

### View Logs

**Via Dashboard:**
- Go to your service → "Deployments" tab
- Click on a deployment → "View Logs"

**Via CLI:**
```bash
# Stream logs
railway logs

# View specific service logs
railway logs --service web
railway logs --service worker
```

### Health Check

Test your deployed service:
```bash
curl https://your-app.railway.app/health
```

Expected response:
```json
{
  "status": "ok",
  "version": "0.1.0"
}
```

### Common Issues

**1. Database Connection Failed**
- Verify database environment variables are set correctly
- Check if PostgreSQL service is running
- Ensure services are in the same Railway project

**2. Build Failed**
- Check build logs in Railway dashboard
- Verify Dockerfile syntax
- Ensure all dependencies are specified in `fx-rate-service.cabal`

**3. Health Check Timeout**
- Database initialization may take 2-3 minutes on first deployment
- Health check timeout is set to 300 seconds in `railway.toml`
- Check logs to see if application started successfully

**4. Worker Not Fetching Rates**
- Verify worker service is running separately
- Check worker logs for errors
- Ensure `FETCH_INTERVAL` is set appropriately

## Scaling

### Horizontal Scaling
- Railway Pro plan supports multiple instances
- Use Railway's "Replicas" setting to scale the web service
- Worker service typically runs as a single instance

### Vertical Scaling
- Adjust resources in Railway dashboard
- Settings → Resources → Memory/CPU

## Cost Optimization

1. **Use Railway's Free Tier**
   - $5 free credit per month
   - Suitable for development/testing

2. **Optimize Worker Interval**
   - Increase `FETCH_INTERVAL` to reduce database writes
   - Recommended: 15-30 minutes for production

3. **Database Optimization**
   - Railway's Postgres has built-in connection pooling
   - Consider using materialized views (already implemented)

## Custom Domain

1. Go to service settings
2. Click "Settings" → "Domains"
3. Add your custom domain
4. Update DNS records as instructed by Railway

## Continuous Deployment

Railway automatically deploys when you push to your GitHub repository:
```bash
git add .
git commit -m "Update FX rate service"
git push origin main
```

Railway will:
1. Build new Docker image
2. Run health checks
3. Deploy with zero downtime (on Pro plan)

## Backup and Recovery

### Database Backups

**Via CLI:**
```bash
# Create backup
railway run pg_dump > backup.sql

# Restore backup
railway run psql < backup.sql
```

**Via Dashboard:**
- Railway automatically creates daily backups (Pro plan)
- Go to PostgreSQL service → "Backups"

## Production Checklist

- [ ] PostgreSQL database added and connected
- [ ] All environment variables configured
- [ ] Web service deployed and health check passing
- [ ] Worker service deployed and fetching rates
- [ ] Logs reviewed for errors
- [ ] API endpoints tested
- [ ] Custom domain configured (optional)
- [ ] Monitoring/alerting setup (optional)

## Support

- **Railway Docs**: https://docs.railway.app
- **Railway Discord**: https://discord.gg/railway
- **Project Issues**: Check your GitHub repository issues

## Next Steps

After successful deployment:
1. Test API endpoints: `/health`, `/rates/USD-EUR`, `/rates/live/USD`
2. Monitor worker logs to ensure rates are being fetched
3. Set up alerts for service health
4. Consider adding authentication for production use
5. Implement rate limiting and caching for better performance
