# Beamline Router Docker Compose

Full stack deployment of Beamline Router with all components using Docker Compose.

## Services

### Core Services

1. **NATS** - Message bus with JetStream
   - Port: 4222 (client), 8222 (monitoring)
   - Image: nats:2.10-alpine

2. **Router** - Beamline Router (Erlang/OTP)
   - Port: 50051 (gRPC)
   - Language: Erlang/OTP

3. **c-gateway** - HTTP-to-NATS gateway
   - Port: 8080 (HTTP API)
   - Language: C++

4. **CAF Worker** - Execution engine
   - Port: 9090 (Prometheus metrics)
   - Language: C++ (CAF)

### Supporting Services

5. **Redis** - Distributed rate limiting (optional)
   - Port: 6379

6. **Prometheus** - Metrics collection
   - Port: 9091 (UI)

7. **OpenTelemetry Collector** - Distributed tracing (optional)
   - Port: 4318 (OTLP HTTP)

## Quick Start

### Prerequisites

- Docker 20.10+
- Docker Compose 2.0+
- 4GB+ RAM available

### 1. Clone and Navigate

```bash
cd ~/aigroup/apps/otp/router/examples
```

### 2. Configure Environment

```bash
# Copy example env file
cp .env.example .env

# Edit if needed
nano .env
```

### 3. Start All Services

```bash
docker-compose up -d
```

### 4. Check Status

```bash
docker-compose ps
```

**Expected output**:
```
NAME                  STATUS
beamline-nats         Up (healthy)
beamline-router       Up (healthy)
beamline-c-gateway    Up (healthy)
beamline-caf-worker   Up (healthy)
beamline-redis        Up (healthy)
beamline-prometheus   Up
beamline-otel         Up
```

### 5. Test the Stack

**Check health**:
```bash
curl http://localhost:8080/_health
```

**Make a request**:
```bash
curl -X POST http://localhost:8080/api/v1/routes/decide \
  -H "Content-Type: application/json" \
  -H "X-Tenant-ID: demo-tenant" \
  -H "X-API-Key: demo-api-key" \
  -d '{
    "version": "1",
    "message": {
      "type": "text.generate",
      "payload": "Hello, Docker!"
    },
    "policy_id": "demo-policy"
  }'
```

## Service Details

### NATS

**Purpose**: Message bus for inter-service communication

**Endpoints**:
- Client: nats://localhost:4222
- Monitoring: http://localhost:8222

**Health Check**:
```bash
curl http://localhost:8222/healthz
```

### Router

**Purpose**: Core routing logic and policy evaluation

**Endpoints**:
- gRPC: localhost:50051

**Health Check**:
```bash
# Via c-gateway
curl http://localhost:8080/_health
```

### c-gateway

**Purpose**: HTTP API entry point

**Endpoints**:
- HTTP API: http://localhost:8080
- Health: http://localhost:8080/_health
- Metrics: http://localhost:8080/metrics

**API Documentation**: See `../clients/curl/README.md`

### CAF Worker

**Purpose**: Block execution engine

**Endpoints**:
- Metrics: http://localhost:9090/metrics

**Health Check**:
```bash
curl http://localhost:9090/health
```

### Prometheus

**Purpose**: Metrics collection and visualization

**Endpoints**:
- UI: http://localhost:9091

**Targets**:
- c-gateway metrics
- CAF Worker metrics
- NATS metrics

**Access UI**:
```bash
open http://localhost:9091  # macOS
xdg-open http://localhost:9091  # Linux
```

## Configuration

### Environment Variables

Edit `.env` file or set environment variables:

**NATS**:
- `NATS_URL` - NATS server URL (default: nats://nats:4222)

**c-gateway**:
- `GATEWAY_RATE_LIMIT_ROUTES_DECIDE_LIMIT` - Rate limit (default: 100)
- `GATEWAY_DISTRIBUTED_RATE_LIMIT_ENABLED` - Use Redis (default: false)
- `GATEWAY_REDIS_URL` - Redis URL (default: redis://redis:6379)
- `GATEWAY_ADMIN_API_KEY` - Admin API key (default: demo-api-key)

**CAF Worker**:
- `WORKER_CPU_POOL_SIZE` - CPU pool size (default: 4)
- `WORKER_IO_POOL_SIZE` - I/O pool size (default: 8)
- `WORKER_MAX_MEMORY_MB` - Max memory (default: 1024)

**Observability**:
- `OTEL_EXPORTER_OTLP_ENDPOINT` - OTLP endpoint (default: http://otel-collector:4318)

### prometheus.yml

Configure scrape targets in `prometheus.yml`:

```yaml
scrape_configs:
  - job_name: 'c-gateway'
    static_configs:
      - targets: ['c-gateway:8080']
```

## Usage

### Start Services

**All services**:
```bash
docker-compose up -d
```

**Specific service**:
```bash
docker-compose up -d nats router c-gateway
```

**With logs**:
```bash
docker-compose up
```

### Stop Services

**All services**:
```bash
docker-compose down
```

**Without removing volumes**:
```bash
docker-compose stop
```

### View Logs

**All services**:
```bash
docker-compose logs -f
```

**Specific service**:
```bash
docker-compose logs -f c-gateway
docker-compose logs -f router
docker-compose logs -f caf-worker
```

**Last N lines**:
```bash
docker-compose logs --tail=100 c-gateway
```

### Restart Services

**All services**:
```bash
docker-compose restart
```

**Specific service**:
```bash
docker-compose restart c-gateway
```

### Scale Services

**CAF Workers**:
```bash
docker-compose up -d --scale caf-worker=3
```

## Testing

### Run Client Examples

**cURL**:
```bash
cd clients/curl
./decide.sh
./health.sh
```

**Python**:
```bash
cd clients/python
python http_client.py
```

**Node.js**:
```bash
cd clients/nodejs
node http_client.js
```

**Go**:
```bash
cd clients/go
go run http_client.go
```

### Load Testing

```bash
# Simple load test
for i in {1..100}; do
  curl -s -X POST http://localhost:8080/api/v1/routes/decide \
    -H "Content-Type: application/json" \
    -H "X-Tenant-ID: demo-tenant" \
    -H "X-API-Key: demo-api-key" \
    -d '{"version":"1","message":{"type":"text.generate","payload":"Test"},"policy_id":"demo-policy"}' &
done
wait
```

## Monitoring

### Prometheus Metrics

**Access UI**:
```
http://localhost:9091
```

**Key Metrics**:
- `gateway_requests_total` - Total requests
- `gateway_request_duration_seconds` - Request latency
- `gateway_rate_limit_exceeded_total` - Rate limit hits
- `worker_tasks_total` - CAF Worker tasks
- `worker_task_latency_ms` - Task latency

**Example Queries**:
```promql
# Request rate
rate(gateway_requests_total[5m])

# P95 latency
histogram_quantile(0.95, gateway_request_duration_seconds_bucket)

# Error rate
rate(gateway_requests_total{status=~"5.."}[5m])
```

### Health Checks

**All services**:
```bash
# c-gateway
curl http://localhost:8080/_health

# NATS
curl http://localhost:8222/healthz

# CAF Worker
curl http://localhost:9090/health

# Prometheus
curl http://localhost:9091/-/healthy
```

### Logs

**Stream logs**:
```bash
docker-compose logs -f --tail=100
```

**Filter by service**:
```bash
docker-compose logs -f c-gateway | grep ERROR
```

## Troubleshooting

### "Cannot connect to the Docker daemon"

**Cause**: Docker not running

**Solution**:
```bash
sudo systemctl start docker  # Linux
# or
open -a Docker  # macOS
```

### "port is already allocated"

**Cause**: Port conflict

**Solution**:
```bash
# Find process using port
lsof -i :8080
# or
netstat -tuln | grep 8080

# Kill process or change port in docker-compose.yml
```

### "unhealthy" status

**Cause**: Service health check failing

**Solution**:
```bash
# Check logs
docker-compose logs <service-name>

# Check dependencies
docker-compose ps

# Restart service
docker-compose restart <service-name>
```

### High memory usage

**Cause**: Too many services or large workload

**Solution**:
```bash
# Reduce worker pool sizes in .env
WORKER_CPU_POOL_SIZE=2
WORKER_IO_POOL_SIZE=4

# Or disable optional services
docker-compose up -d nats router c-gateway
```

### "NATS disconnected"

**Cause**: NATS not ready when services start

**Solution**:
```bash
# Restart dependent services
docker-compose restart router c-gateway caf-worker
```

## Production Considerations

### Resource Limits

Add resource limits to `docker-compose.yml`:

```yaml
services:
  router:
    deploy:
      resources:
        limits:
          cpus: '2'
          memory: 2G
        reservations:
          cpus: '1'
          memory: 1G
```

### Persistence

**NATS JetStream**:
```yaml
services:
  nats:
    volumes:
      - nats-data:/data
    command: ["-js", "-sd", "/data"]
```

**Prometheus**:
Already configured with volume `prometheus-data`

### Security

**1. Change default API keys**:
```bash
# In .env
GATEWAY_ADMIN_API_KEY=<strong-random-key>
```

**2. Use TLS**:
```yaml
services:
  c-gateway:
    environment:
      - GATEWAY_TLS_ENABLED=true
      - GATEWAY_TLS_CERT=/certs/cert.pem
      - GATEWAY_TLS_KEY=/certs/key.pem
    volumes:
      - ./certs:/certs:ro
```

**3. Network isolation**:
```yaml
services:
  redis:
    networks:
      - internal  # Not exposed externally
```

### Scaling

**Horizontal scaling**:
```bash
# Scale CAF Workers
docker-compose up -d --scale caf-worker=5

# Use load balancer for c-gateway
docker-compose up -d --scale c-gateway=3
```

## Development

### Build from Source

```bash
# Build all services
docker-compose build

# Build specific service
docker-compose build c-gateway

# Build without cache
docker-compose build --no-cache
```

### Local Changes

**Mount local code**:
```yaml
services:
  router:
    volumes:
      - ../../..:/app
```

**Note**: Requires restart to pick up changes.

## Clean Up

### Remove Containers

```bash
docker-compose down
```

### Remove Volumes

```bash
docker-compose down -v
```

### Remove Images

```bash
docker-compose down --rmi all
```

### Full Cleanup

```bash
docker-compose down -v --rmi all --remove-orphans
```

## Next Steps

- **Client Examples**: See `clients/` directory
- **Integration Guide**: See `../docs/INTEGRATION_GUIDE.md`
- **Monitoring**: Set up Grafana dashboards
- **Production**: Configure TLS, authentication, scaling

## Support

- **Documentation**: See `../docs/`
- **Examples**: See `clients/`
- **Issues**: Check logs with `docker-compose logs`

## License

Same as Beamline Router project.
