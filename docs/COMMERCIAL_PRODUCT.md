# Beamline Router: Commercial Product Guide

**Open Core AI Routing Platform with Enterprise Features**

---

## Overview

Beamline Router is available in two editions:
- **Core Edition** (Open Source, Apache 2.0)
- **Enterprise Edition** (Commercial License)

Both share the same robust architecture, with Enterprise adding features for production-critical deployments at scale.

---

## Product Editions

### Core Edition (Open Source)

**License**: Apache 2.0 (Free, permissive)

**What's Included**:
- ✅ Full routing engine (policy-based, intelligent)
- ✅ c-gateway (HTTP-to-NATS adapter)
- ✅ Extensions pipeline (4 types: pre, validate, post, provider)
- ✅ Multi-tenant isolation (basic quotas)
- ✅ CAF Worker integration
- ✅ Prometheus metrics & OpenTelemetry tracing
- ✅ Docker Compose deployment
- ✅ Documentation & examples
- ✅ Community support (GitHub issues, forums)

**Best For**:
- Startups and scale-ups
- Development and testing
- Non-mission-critical workloads
- Learning and evaluation
- Open-source projects

**Limitations**:
- No SLA guarantees
- Community support only
- Basic audit logging
- Manual HA setup
- No enterprise connectors

### Enterprise Edition

**License**: Commercial (requires purchase)

**Everything in Core, plus**:
- ✅ **99.99% Uptime SLA** (legal guarantee)
- ✅ **Advanced audit logging** (compliance, GDPR, HIPAA)
- ✅ **High Availability** (multi-region, auto-failover)
- ✅ **Priority support** (24/7, 1-hour critical SLA)
- ✅ **Advanced RBAC** (SSO, fine-grained permissions)
- ✅ **Enterprise connectors** (Kafka, Kinesis, Pub/Sub)
- ✅ **Custom extensions** (professional services)
- ✅ **Air-gapped deployment** (on-premises, isolated)
- ✅ **Dedicated TAM** (Technical Account Manager)
- ✅ **Training & consulting**

**Best For**:
- Enterprises (500+ employees)
- Regulated industries (finance, healthcare, government)
- Mission-critical production workloads
- Companies requiring SLAs
- Complex compliance requirements

**Pricing**: Contact sales (usage-based or subscription)

---

## Feature Comparison

| Feature | Core (OSS) | Enterprise |
|---------|------------|------------|
| **Routing Engine** | ✅ | ✅ |
| **Policy Evaluation** | ✅ | ✅ |
| **Extensions Pipeline** | ✅ | ✅ |
| **Multi-Tenancy** | ✅ Basic | ✅ Advanced |
| **Observability** | ✅ Metrics | ✅ + Advanced dashboards |
| **Deployment** | ✅ Docker/K8s | ✅ + Multi-region HA |
| **Audit Logging** | Basic | ✅ Advanced (compliance) |
| **RBAC** | Basic | ✅ Advanced (SSO, fine-grained) |
| **SLA** | None | ✅ 99.99% uptime |
| **Support** | Community | ✅ 24/7 priority |
| **Enterprise Connectors** | ❌ | ✅ Kafka, Kinesis, etc. |
| **Air-Gapped Deployment** | ❌ | ✅ |
| **Professional Services** | ❌ | ✅ TAM, training |
| **Custom Development** | ❌ | ✅ Available |
| **License** | Apache 2.0 | Commercial |

---

## Enterprise Features Deep Dive

### 1. SLA Support (99.99% Uptime Guarantee)

**What It Includes**:
- Legal SLA commitment (< 53 minutes downtime/year)
- Multi-region active-active deployment
- Automated failover (< 30 seconds)
- 24/7 health monitoring
- Incident response team
- Monthly uptime reports
- Financial credits for SLA breaches

**Business Value**:
- **Cost of downtime**: $250K-$1M+ per hour for large enterprises
- **Revenue protection**: Every minute counts for SaaS platforms
- **Trust**: SLA required for enterprise procurement

**Example**: E-commerce platform with 100K RPS. 1 hour downtime = $500K lost revenue. Enterprise SLA protects against this.

### 2. Advanced Audit Logging

**What It Includes**:
- Request-level audit trail (who, what, when)
- Tamper-proof log storage (immutab le)
- Compliance reporting (GDPR, HIPAA, SOC2, PCI-DSS)
- Long-term retention (7+ years)
- Export to SIEM (Splunk, Datadog, etc.)
- User activity tracking
- API audit logs
- Change management logs

**Business Value**:
- **Compliance**: Required for regulated industries
- **Security**: Detect and investigate incidents
- **Governance**: Track who changed what

**Example**: Financial services company must prove compliance for auditors. Advanced logging provides complete audit trail.

### 3. High Availability (Multi-Region)

**What It Includes**:
- Active-active multi-region deployment
- Automatic cross-region failover
- Zero-downtime rolling updates
- Geo-distribution (latency optimization)
- Disaster recovery (RPO < 1 minute, RTO < 5 minutes)
- Automatic backup and restore
- Traffic splitting (region affinity)

**Business Value**:
- **Business continuity**: Survive datacenter outages
- **Performance**: Serve users from nearest region
- **Disaster recovery**: Required for enterprise risk management

**Example**: Global SaaS platform serves 50 countries. Multi-region ensures < 50ms latency worldwide.

### 4. Advanced RBAC (Role-Based Access Control)

**What It Includes**:
- Fine-grained permissions (read, write, admin per resource)
- SSO integration (SAML, OAuth, Active Directory)
- API key management (rotation, expiry)
- Team/group permissions
- Audit trail for all access
- Service accounts
- Just-in-time access

**Business Value**:
- **Security**: Principle of least privilege
- **Compliance**: Required for SOC2, ISO 27001
- **Productivity**: SSO reduces password fatigue

**Example**: 500-person company with 50 developers. RBAC ensures only authorized personnel can modify production policies.

### 5. Enterprise Connectors

**What It Includes**:
- **Kafka** connector (produce/consume)
- **AWS Kinesis** integration
- **Azure Event Hubs** support
- **Google Pub/Sub** adapter
- **Custom connectors** (via professional services)
- **Batching and buffering**
- **Dead letter queues**

**Business Value**:
- **Integration**: Fit into existing architecture
- **Migration**: Gradual transition from legacy systems
- **Flexibility**: Use preferred messaging platform

**Example**: Enterprise already uses Kafka. Kafka connector allows Router to integrate seamlessly without changing existing infrastructure.

### 6. Professional Services

**What It Includes**:
- **Dedicated TAM** (Technical Account Manager)
- **Architecture review** (best practices, optimization)
- **Custom development** (extensions, integrations)
- **Training programs** (onboarding, advanced topics)
- **Migration assistance** (from legacy systems)
- **Performance tuning**
- **On-site support** (if needed)

**Business Value**:
- **Faster time-to-value**: Weeks instead of months
- **Best practices**: Avoid common pitfalls
- **Confidence**: Expert guidance

**Example**: Large enterprise migrating from custom solution. Professional services team guides migration, reducing risk and timeline.

---

## Licensing Model

### Open Core Strategy

**Philosophy**: Core functionality open source, enterprise features commercial

**Core License** (Apache 2.0):
- Permissive, commercial-friendly
- No copyleft restrictions
- Can use in proprietary products
- Fork and modify allowed

**Enterprise License** (Commercial):
- Proprietary software license
- Per-deployment or usage-based pricing
- Includes support and updates
- No source code redistribution

**Why This Model?**:
- ✅ **Community adoption**: Free tier removes barriers
- ✅ **Developer trust**: Transparent, no bait-and-switch
- ✅ **Revenue model**: Enterprises pay for value
- ✅ **Sustainable**: Fund ongoing development

**Inspiration**: Elastic, GitLab, HashiCorp, MongoDB

### License Compliance

**Core (Apache 2.0)**:
- ✅ Commercial use allowed
- ✅ Modification allowed
- ✅ Distribution allowed
- ⚠️ Must preserve copyright notice
- ⚠️ Must state changes made

**Enterprise**:
- ✅ License key required
- ✅ Annual renewal
- ❌ No redistribution
- ❌ No reverse engineering

---

## Pricing

### Usage-Based Model (Recommended)

**Metric**: API requests per month

#### Tiers

**1. Community (Free Forever)**
- **Price**: $0/month
- **Included**: 1,000,000 requests/month
- **Features**: Core Edition
- **Support**: Community (GitHub issues, forums)
- **SLA**: None

**Best for**: Startups, developers, side projects

**2. Startup ($99/month)**
- **Price**: $99/month
- **Included**: 10,000,000 requests/month
- **Features**: Core Edition
- **Support**: Email support (48-hour response)
- **SLA**: 99.9% (best-effort)

**Overage**: $10 per additional 1M requests

**Best for**: Growing startups, small production workloads

**3. Business ($499/month)**
- **Price**: $499/month
- **Included**: 100,000,000 requests/month
- **Features**: Core + Basic Enterprise (audit logging, priority support)
- **Support**: Priority email/Slack (12-hour SLA)
- **SLA**: 99.95%

**Overage**: $5 per additional 1M requests

**Best for**: Mid-sized companies, production workloads

**4. Enterprise (Custom)**
- **Price**: Custom (typically $2,000+/month)
- **Included**: Unlimited requests (or custom limit)
- **Features**: Full Enterprise Edition
- **Support**: 24/7 phone/email (1-hour critical SLA)
- **SLA**: 99.99% (legal guarantee)
- **Extras**: TAM, training, custom development

**Best for**: Large enterprises, mission-critical workloads

### Alternative: Seat-Based Model

**Metric**: Number of users/operators

- **Team**: $50/user/month (min 5 users)
- **Enterprise**: $100/user/month + volume discounts

### Alternative: Self-Hosted Subscription

**Flat annual fee**: $10K-$100K+/year depending on deployment size

**Includes**:
- Enterprise license
- Support
- Updates

**Best for**: Air-gapped, on-premises deployments

---

## Support Tiers

### Community Support (Free)

**Channels**:
- GitHub issues
- Community forum
- Documentation
- Stack Overflow tag

**Response Time**: Best-effort (no SLA)

**Best for**: Non-production, learning

### Business Support ($499+/month)

**Channels**:
- Email support
- Slack channel (shared)

**Response Times**:
- **Critical** (P0): 12 hours
- **High** (P1): 24 hours
- **Medium** (P2): 48 hours
- **Low** (P3): Best-effort

**Includes**:
- Quarterly check-ins
- Version upgrade assistance

### Enterprise Support (Custom pricing)

**Channels**:
- 24/7 phone support
- Dedicated Slack channel
- Video conferencing
- On-site (if needed)

**Response Times**:
- **Critical** (P0): 1 hour
- **High** (P1): 4 hours
- **Medium** (P2): 8 hours
- **Low** (P3): 24 hours

**Includes**:
- Dedicated TAM
- Monthly check-ins
- Architecture reviews
- Training sessions
- Custom development (limited hours)

---

## Deployment Options

### Self-Hosted (All Tiers)

**What You Manage**:
- Infrastructure (servers, networking)
- NATS deployment
- Monitoring and alerting
- Backups
- Security patches

**We Provide**:
- Docker images
- Kubernetes Helm charts
- Documentation
- Update notifications

**Best for**: Security-conscious, on-premises requirements

### Managed Cloud (Future Roadmap)

**What We Manage**:
- Infrastructure provisioning
- Auto-scaling
- Monitoring and alerting
- Backups and disaster recovery
- Security patches and updates

**You Configure**:
- Policies
- Extensions
- Tenants

**Pricing**: 30-50% premium over self-hosted

**Best for**: Focus on business, not infrastructure

---

## Migration from Core to Enterprise

**Process**:
1. **Contact sales**: Discuss requirements
2. **Pilot**: 30-day trial with full features
3. **Contract**: Sign license agreement
4. **Onboarding**: TAM works with your team
5. **Deployment**: Professional services assist
6. **Go-live**: Gradual rollout with support

**No Disruption**:
- Same codebase (feature flags)
- In-place upgrade
- Backward compatible

**Timeline**: Typically 2-4 weeks

---

## ROI Justification

### Example: E-Commerce Platform

**Current State**:
- 100M requests/month
- Custom routing solution
- 3 engineers maintaining (40% time)
- 1 outage/year (2 hours, $500K cost)

**With Beamline Enterprise**:
- **Cost**: $2,500/month + $2,000/month overage = $4,500/month
- **Savings**:
  - Engineer time: 1.2 FTE × $150K = $180K/year
  - Avoided outage: $500K/year (reduced by 80% = $400K)
  - Total ROI: $580K - $54K = $526K/year
- **Payback**: < 1 month

### Example: Financial Services

**Current State**:
- 50M requests/month
- Compliance requirements (SOC2, GDPR)
- 6 months to build audit logging

**With Beamline Enterprise**:
- **Cost**: $2,000/month (custom pricing)
- **Savings**:
  - Development time: 6 engineer-months × $15K = $90K
  - Compliance risk: Immeasurable (fines = $millions)
  - Time-to-market: 6 months faster
- **Payback**: Immediate (compliance is table stakes)

---

## Frequently Asked Questions

**Q: Is Core Edition production-ready?**  
A: Yes! Core Edition is fully production-ready. Many companies run it successfully. Enterprise adds SLA, support, and advanced features for mission-critical workloads.

**Q: Can I start with Core and upgrade later?**  
A: Absolutely. Upgrade path is seamless. Feature flags enable Enterprise features when licensed.

**Q: Do I need to self-host?**  
A: Currently yes. Managed cloud offering is on roadmap for 2024.

**Q: What happens if I exceed free tier limits?**  
A: Requests are throttled (rate limited) after 1M/month. Upgrade to paid plan for higher limits.

**Q: Can I negotiate pricing?**  
A: Yes, Enterprise pricing is negotiable based on volume and commitment.

**Q: Is professional services required?**  
A: No, but highly recommended for faster time-to-value and best practices.

**Q: What's the contract term?**  
A: Annual contracts standard. Monthly available for Startup tier.

**Q: Can I get a trial?**  
A: Yes, 30-day full-featured trial for Enterprise edition.

---

## Next Steps

### For Core Edition

1. **Download**: Clone GitHub repository
2. **Deploy**: Follow Docker Compose or Kubernetes guide
3. **Integrate**: Use client examples
4. **Get support**: GitHub issues, community forum

### For Enterprise Edition

1. **Contact sales**: enterprise@beamline.ai
2. **Discovery call**: Discuss requirements (30 minutes)
3. **Pilot**: 30-day trial with your workload
4. **Proposal**: Custom pricing based on usage
5. **Onboarding**: TAM-led deployment

### For Partners

Interested in reselling or partnering? Contact: partnerships@beamline.ai

---

**Beamline Router**: Open core. Enterprise ready. AI-optimized.
