# Task: T-PRODUCT-02 — Commercial Framing

**Created**: 2025-12-22  
**Status**: In Progress  
**Priority**: High  
**Type**: Business Strategy / Product

## Objective

Define commercial strategy and product packaging:
1. **Core Engine** (Open Source / Included)
2. **Enterprise features** (Commercial add-ons)
3. **Licensing model** (Open-source + commercial)
4. **Usage/pricing model**

## Deliverables

### 1. Commercial Product Document

**File**: `docs/COMMERCIAL_PRODUCT.md`

**Sections**:
1. Product Packaging (Core vs Enterprise)
2. Feature Comparison Matrix
3. Licensing Model
4. Usage-Based Pricing
5. Support Tiers
6. Deployment Options

### 2. Licensing Files

**Files**:
- `LICENSE` - Core engine license (Apache 2.0 / MIT)
- `LICENSE-ENTERPRISE` - Enterprise features license
- `PRICING.md` - Public pricing page

### 3. Feature Gates

**Implementation**:
- Feature flags for enterprise features
- License validation
- Usage tracking

## Product Packaging

### Core Engine (Open Source)

**License**: Apache 2.0 (permissive, commercial-friendly)

**Features**:
- ✅ Router (policy-based routing)
- ✅ c-gateway (HTTP-to-NATS)
- ✅ Extensions Pipeline (4 types)
- ✅ Multi-tenancy (basic)
- ✅ Prometheus metrics
- ✅ Docker Compose deployment
- ✅ Community support

**Target**: Startups, developers, evaluation

### Enterprise Edition

**License**: Commercial (proprietary)

**Features**:
- ✅ **SLA Support** (99.99% uptime guarantee)
- ✅ **Advanced Audit Logging** (compliance, GDPR)
- ✅ **High Availability** (multi-region, failover)
- ✅ **Priority Support** (24/7, SLA response times)
- ✅ **Advanced RBAC** (fine-grained permissions)
- ✅ **Enterprise Connectors** (Kafka, Kinesis, etc.)
- ✅ **Custom Extensions** (professional services)
- ✅ **On-premises deployment** (air-gapped)

**Target**: Enterprises, regulated industries, mission-critical

## Licensing Strategy

### Open Core Model

**Core** (Apache 2.0):
- Full routing functionality
- Production-ready
- Self-serve deployment
- Community support

**Enterprise** (Commercial):
- SLA guarantees
- Advanced features
- Professional support
- Training & consulting

**Inspiration**: Elastic, GitLab, HashiCorp model

### Why Open Core?

**Benefits**:
- ✅ Community adoption (free tier)
- ✅ Developer evangelism
- ✅ Rapid feedback loop
- ✅ Ecosystem growth

**Revenue**:
- ✅ Enterprise licenses
- ✅ Professional services
- ✅ Managed hosting (SaaS)

## Enterprise Features Detail

### 1. SLA Support (99.99% Uptime)

**Included**:
- Multi-region deployment
- Automatic failover
- Health monitoring
- Incident response
- Post-mortem analysis

**Value**: $250K-1M cost of downtime avoided

### 2. Advanced Audit Logging

**Included**:
- Request-level audit trail
- Compliance reporting (GDPR, HIPAA, SOC2)
- Tamper-proof logs
- Long-term retention
- Export to SIEM

**Value**: Compliance requirements

### 3. High Availability (HA)

**Included**:
- Active-active clustering
- Zero-downtime deploys
- Cross-region replication
- Automatic backup/restore
- Disaster recovery

**Value**: Business continuity

### 4. Advanced RBAC

**Included**:
- Fine-grained permissions
- SSO integration (SAML, OAuth)
- API key management
- Team collaboration
- Audit trails

**Value**: Security & governance

### 5. Enterprise Connectors

**Included**:
- Kafka integration
- AWS Kinesis
- Azure Event Hubs
- Google Pub/Sub
- Custom connectors

**Value**: Existing infrastructure integration

### 6. Professional Services

**Included**:
- Dedicated TAM (Technical Account Manager)
- Architecture review
- Custom development
- Training programs
- Migration assistance

**Value**: Accelerated time-to-value

## Pricing Model

### Usage-Based (Recommended)

**Metric**: Requests per month

**Tiers**:
1. **Community** (Free)
   - 1M requests/month
   - Community support
   - Core features only

2. **Startup** ($99/month)
   - 10M requests/month
   - Email support
   - Basic SLA (99.9%)

3. **Business** ($499/month)
   - 100M requests/month
   - Priority support
   - SLA 99.95%
   - Audit logging

4. **Enterprise** (Custom pricing)
   - Unlimited requests
   - 24/7 support
   - SLA 99.99%
   - All features
   - Professional services

**Additional**: $5 per 1M extra requests

### Seat-Based (Alternative)

**Metric**: Number of developers/operators

**Tiers**:
1. **Team** ($50/user/month)
   - Up to 10 users
   - Core features
   - Email support

2. **Enterprise** ($100/user/month)
   - Unlimited users
   - All features
   - Priority support

### Hybrid Model

**Base**: Subscription + Usage overage

**Example**: $499/month + $5/1M requests over 100M

## Support Tiers

### Community (Free)

- GitHub issues
- Community forum
- Documentation
- Best-effort response

### Business ($499/month+)

- Email support
- 24-hour SLA
- Slack channel
- Quarterly reviews

### Enterprise (Custom)

- 24/7 phone/email
- 1-hour critical SLA
- Dedicated TAM
- On-site support

## Deployment Options

### Self-Hosted (All Tiers)

**Core**:
- Docker Compose
- Kubernetes Helm charts
- Manual deployment

**Enterprise**:
- Multi-region setup
- HA configuration
- Professional services

### Managed Cloud (Future)

**SaaS offering**:
- Beamline-hosted
- Auto-scaling
- Managed updates
- Built-in monitoring

**Pricing**: Premium over self-hosted

## Timeline

- **Product packaging**: 2-3 hours
- **Licensing strategy**: 1-2 hours
- **Pricing model**: 2-3 hours
- **Documentation**: 2-3 hours
- **Total**: 1-1.5 days

## Success Criteria

- [x] Clear Core vs Enterprise split
- [x] Defensible enterprise features
- [x] Fair pricing model
- [x] Open-source license chosen
- [x] Commercial value articulated

## References

- Elastic licensing model
- GitLab open core strategy
- HashiCorp commercial products
- Open source business models (COSS)
