# Production Logging Guide

**Date**: 2025-11-30  
**Component**: Router (`apps/otp/router/`)  
**Purpose**: Production logging configuration and log rotation

---

## Overview

Router writes structured JSON logs to files in the format `router_YYYY-MM-DD.jsonl`. In production environments, these log files should be rotated to prevent disk space issues and enable log aggregation.

**Log File Format**: `router_YYYY-MM-DD.jsonl`  
**Default Location**: `.windsurf/reports/router_YYYY-MM-DD.jsonl`  
**Configurable Location**: Set via `beamline_router.log_dir` application environment variable

---

## Log Rotation Strategies

### Using logrotate

**Recommended for**: Traditional Linux deployments

Create `/etc/logrotate.d/beamline-router`:

```bash
.windsurf/reports/router_*.jsonl {
    daily
    rotate 7
    compress
    delaycompress
    notifempty
    create 0640 router router
    missingok
    postrotate
        # Reload Router if needed (optional)
        systemctl reload beamline-router || true
    endscript
}
```

**Configuration Options**:
- `daily`: Rotate logs daily
- `rotate 7`: Keep 7 days of logs
- `compress`: Compress rotated logs (gzip)
- `delaycompress`: Compress on next rotation cycle (keeps current log uncompressed)
- `notifempty`: Don't rotate empty files
- `create 0640 router router`: Create new log files with permissions 0640, owned by router user
- `missingok`: Don't error if log files are missing
- `postrotate`: Commands to run after rotation (optional Router reload)

**Alternative Configuration** (weekly rotation, 4 weeks retention):

```bash
.windsurf/reports/router_*.jsonl {
    weekly
    rotate 4
    compress
    delaycompress
    notifempty
    create 0640 router router
    missingok
}
```

**Test Configuration**:
```bash
# Test logrotate configuration (dry-run)
sudo logrotate -d /etc/logrotate.d/beamline-router

# Force rotation (for testing)
sudo logrotate -f /etc/logrotate.d/beamline-router
```

### Using systemd

**Recommended for**: systemd-based deployments

Configure systemd service with log rotation:

**Service File** (`/etc/systemd/system/beamline-router.service`):

```ini
[Unit]
Description=Beamline Router
After=network.target

[Service]
Type=simple
User=router
Group=router
WorkingDirectory=/opt/beamline/router
ExecStart=/usr/bin/beamline_router
StandardOutput=append:/var/log/router/router.log
StandardError=append:/var/log/router/router_error.log
Restart=always
RestartSec=10

# Log rotation via journald
StandardOutput=journal
StandardError=journal

[Install]
WantedBy=multi-user.target
```

**Journald Configuration** (`/etc/systemd/journald.conf`):

```ini
[Journal]
SystemMaxUse=1G
SystemKeepFree=2G
MaxRetentionSec=7d
MaxFileSec=1day
Compress=yes
```

**Journald Rotation**:
- `SystemMaxUse=1G`: Maximum disk space for logs
- `SystemKeepFree=2G`: Keep 2GB free on disk
- `MaxRetentionSec=7d`: Keep logs for 7 days
- `MaxFileSec=1day`: Rotate log files daily
- `Compress=yes`: Compress rotated logs

**View Logs**:
```bash
# View Router logs via journald
journalctl -u beamline-router -f

# View logs for specific date
journalctl -u beamline-router --since "2025-11-30" --until "2025-01-28"

# Export logs to JSONL format
journalctl -u beamline-router -o json --since "2025-11-30" > router_2025-11-30.jsonl
```

### Using Docker

**Recommended for**: Containerized deployments

**Option 1: Volume Mounts with External Rotation**

```yaml
services:
  router:
    image: beamline/router:latest
    volumes:
      - ./logs:/app/.windsurf/reports
    logging:
      driver: "json-file"
      options:
        max-size: "10m"
        max-file: "7"
        compress: "true"
```

**Docker Logging Options**:
- `max-size`: Maximum size of log file before rotation (10MB)
- `max-file`: Number of log files to keep (7 files)
- `compress`: Compress rotated logs (gzip)

**Option 2: Docker Logging Driver with Rotation**

```bash
docker run \
  --log-driver json-file \
  --log-opt max-size=10m \
  --log-opt max-file=7 \
  --log-opt compress=true \
  beamline/router:latest
```

**Option 3: External Log Rotation (logrotate in container)**

Create `Dockerfile` with logrotate:

```dockerfile
FROM erlang:26.0

# Install logrotate
RUN apt-get update && apt-get install -y logrotate

# Copy logrotate configuration
COPY logrotate-router /etc/logrotate.d/router

# Copy Router application
COPY . /app
WORKDIR /app

# Start Router
CMD ["/app/bin/beamline_router", "foreground"]
```

**logrotate Configuration** (`logrotate-router`):

```bash
/app/.windsurf/reports/router_*.jsonl {
    daily
    rotate 7
    compress
    delaycompress
    notifempty
    create 0640 router router
    missingok
}
```

---

## Log Aggregation

### Loki Integration

**Collection Method**: File-based collection using Promtail

**Promtail Configuration** (`promtail-config.yml`):

```yaml
server:
  http_listen_port: 9080
  grpc_listen_port: 0

positions:
  filename: /tmp/positions.yaml

clients:
  - url: http://loki:3100/loki/api/v1/push

scrape_configs:
  - job_name: router
    static_configs:
      - targets:
          - localhost
        labels:
          job: router
          component: router
          __path__: /var/log/router/router_*.jsonl
```

**Docker Compose Example**:

```yaml
services:
  router:
    image: beamline/router:latest
    volumes:
      - ./logs:/var/log/router
    networks:
      - observability

  promtail:
    image: grafana/promtail:latest
    volumes:
      - ./logs:/var/log/router:ro
      - ./promtail-config.yml:/etc/promtail/config.yml
    command: -config.file=/etc/promtail/config.yml
    networks:
      - observability

  loki:
    image: grafana/loki:latest
    ports:
      - "3100:3100"
    networks:
      - observability

networks:
  observability:
    driver: bridge
```

**Loki Query Examples**:
- Error logs: `{component="router"} |= "ERROR"`
- High latency: `{component="router"} | json | latency_ms > 1000`
- Specific tenant: `{component="router"} | json | tenant_id="tenant_123"`
- Trace correlation: `{component="router"} | json | trace_id="trace_abc123"`

### ELK Stack Integration

**Filebeat Configuration** (`filebeat.yml`):

```yaml
filebeat.inputs:
  - type: log
    enabled: true
    paths:
      - /var/log/router/router_*.jsonl
    json.keys_under_root: true
    json.add_error_key: true
    json.message_key: message

output.elasticsearch:
  hosts: ["elasticsearch:9200"]
  index: "router-logs-%{+yyyy.MM.dd}"

processors:
  - add_fields:
      fields:
        component: router
        environment: production
```

**Logstash Configuration** (alternative):

```ruby
input {
  file {
    path => "/var/log/router/router_*.jsonl"
    codec => json_lines
    start_position => "beginning"
  }
}

filter {
  json {
    source => "message"
  }
  
  date {
    match => ["timestamp", "ISO8601"]
  }
  
  mutate {
    add_field => {
      "component" => "router"
      "environment" => "production"
    }
  }
}

output {
  elasticsearch {
    hosts => ["elasticsearch:9200"]
    index => "router-logs-%{+YYYY.MM.dd}"
  }
}
```

---

## Production Best Practices

### Log Retention

**Recommended Retention Periods**:
- **Development**: 3-7 days
- **Staging**: 7-14 days
- **Production**: 14-30 days (compliance dependent)

**Disk Space Calculation**:
- Average log entry: ~500 bytes
- Logs per day: ~100,000 entries
- Daily log size: ~50 MB
- 30-day retention: ~1.5 GB (compressed: ~150 MB)

### Log Compression

**Enable Compression**:
- Reduces disk usage by ~90%
- Minimal CPU overhead
- Standard gzip compression

**Compression Settings**:
- `compress`: Enable compression in logrotate
- `delaycompress`: Compress on next rotation (keeps current log uncompressed for easier access)

### Log Monitoring

**Disk Space Monitoring**:
```bash
# Check log directory size
du -sh /var/log/router/

# Check individual log files
ls -lh /var/log/router/router_*.jsonl

# Monitor disk usage
df -h /var/log/router/
```

**Alerting** (when Prometheus is available in CP2):
- Alert when log directory exceeds 80% of disk space
- Alert when log rotation fails
- Alert when log files are not being created

### Log Access

**File Permissions**:
- Log files: `0640` (readable by owner and group)
- Log directory: `0750` (accessible by owner and group)
- Owner: `router` user
- Group: `router` group

**Access Control**:
- Only `router` user and `router` group can read logs
- Log aggregation tools (Promtail, Filebeat) should run as `router` user or have group access

### Performance Considerations

**Log I/O**:
- Logging is asynchronous to minimize performance impact
- JSON serialization is optimized for performance
- PII filtering adds minimal overhead (~10-50 microseconds per log entry)

**File System**:
- Use fast storage (SSD) for log directory
- Avoid network-mounted filesystems for log directory (use local storage)
- Consider separate disk partition for logs

---

## Troubleshooting

### Logs Not Rotating

**Problem**: Log files are not being rotated.

**Solutions**:
1. Check logrotate configuration:
   ```bash
   sudo logrotate -d /etc/logrotate.d/beamline-router
   ```
2. Verify logrotate is running:
   ```bash
   systemctl status logrotate.timer
   ```
3. Check logrotate logs:
   ```bash
   grep router /var/log/logrotate.log
   ```

### Disk Space Issues

**Problem**: Log directory is consuming too much disk space.

**Solutions**:
1. Reduce retention period in logrotate configuration
2. Enable compression (`compress`)
3. Manually rotate logs:
   ```bash
   sudo logrotate -f /etc/logrotate.d/beamline-router
   ```
4. Archive old logs:
   ```bash
   tar -czf router-logs-archive-$(date +%Y%m%d).tar.gz /var/log/router/router_*.jsonl
   ```

### Log Aggregation Issues

**Problem**: Logs are not being collected by Loki/ELK.

**Solutions**:
1. Verify log files are accessible:
   ```bash
   ls -la /var/log/router/router_*.jsonl
   ```
2. Check Promtail/Filebeat configuration
3. Verify network connectivity to Loki/Elasticsearch
4. Check Promtail/Filebeat logs:
   ```bash
   journalctl -u promtail -f
   ```

---

## References

- `apps/otp/router/docs/OBSERVABILITY.md` - Router observability documentation
- `docs/OBSERVABILITY.md` - Unified observability requirements
- `docs/OBSERVABILITY_CP1_INVARIANTS.md` - CP1 observability invariants
- Logrotate documentation: https://linux.die.net/man/8/logrotate
- systemd journald documentation: https://www.freedesktop.org/software/systemd/man/journald.conf.html
- Docker logging drivers: https://docs.docker.com/config/containers/logging/

---

## Summary

Router logs should be rotated in production to prevent disk space issues. Recommended approach:

1. **Use logrotate** for traditional Linux deployments
2. **Use systemd journald** for systemd-based deployments
3. **Use Docker logging driver** for containerized deployments
4. **Enable compression** to reduce disk usage
5. **Set appropriate retention** (7-30 days depending on requirements)
6. **Monitor disk space** and set up alerts
7. **Use log aggregation** (Loki, ELK) for centralized log collection

**Status**: ✅ **Production-Ready**

