# CP2 Vision: The Future of Beamline Router

**Control Protocol 2.0 and Long-Term Product Vision**

---

## Executive Summary

CP2 (Control Protocol 2.0) represents the next major evolution of Beamline Router, planned for 2025-2026. This document defines:
- **What's in CP2**: Streaming, enhanced policy language, advanced features
- **What we deliberately won't build**: Conscious non-goals
- **Router boundaries**: Where Router ends, other systems begin

---

## CP1 Current State (1.x.x)

### What We Have Today

**Protocol**: CP1 (Control Protocol v1)
- JSON-based request-reply
- NATS subjects: `beamline.router.v1.*`
- Synchronous decision-making

**Core Features**:
- ✅ Policy-based routing (cost, latency, quality)
- ✅ Extensions Pipeline (4 types: pre, validate, post, provider)
- ✅ Multi-tenancy (basic isolation and quotas)
- ✅ CAF Worker integration (block execution)
- ✅ NATS messaging (request-reply)
- ✅ Prometheus metrics
- ✅ Docker deployment

**Limitations** (addressed in CP2):
- ❌ No streaming responses
- ❌ Limited bi-directional communication
- ❌ Basic cost tracking (no forecasting)
- ❌ Simple policy language (DSL planned)
- ❌ No built-in A/B testing
- ❌ Manual tenant management

### What Works Well (Keep in CP2)

- ✅ **Extensions framework**: Powerful and flexible
- ✅ **Multi-tenancy model**: Proven at scale
- ✅ **NATS integration**: Performant and reliable
- ✅ **Erlang/OTP**: Fault-tolerant and concurrent
- ✅ **Open Core model**: Community + enterprise

---

## CP2 Vision (2.0.0)

### Timeline

**Planning**: Q1-Q2 2025  
**Development**: Q3 2025 - Q2 2026  
**Release**: Q3 2026 (target)

### Major Features

#### 1. Streaming Support

**Problem**: Many AI models support streaming (ChatGPT, Claude)  
**Solution**: Router supports streaming end-to-end

**Features**:
- **Server-Sent Events (SSE)**: For HTTP streaming
- **WebSocket support**: Bi-directional communication
- **gRPC streams**: Binary protocol streaming
- **Chunked responses**: Incremental data

**Use Cases**:
- Real-time chat applications
- Progressive content generation
- Live model output
- Interactive AI assistants

**Impact**: 10x better user experience for streaming apps

#### 2. Enhanced Policy Language (DSL)

**Problem**: Current policies are static JSON  
**Solution**: Turing-complete policy DSL

**Features**:
- **Conditional logic**: if/else/switch
- **Variables**: Store and reuse values
- **Functions**: Reusable policy fragments
- **Composition**: Combine multiple policies
- **Templates**: Parameterized policies

**Example**:
```python
# Policy DSL (syntax TBD)
policy demo_policy:
  if request.cost_budget > 0.01:
    route to expensive_provider
  else if request.latency_sla < 500ms:
    route to fast_provider
  else:
    route to cheap_provider
```

**Impact**: 100x more expressive policies

#### 3. Advanced Cost Tracking

**Problem**: Limited visibility into AI spend  
**Solution**: Real-time cost management

**Features**:
- **Real-time cost calculation**: Per request, per tenant
- **Budget alerts**: Notify when approaching limit
- **Cost attribution**: Track spend by tenant/team/project
- **Forecasting**: Predict future costs based on usage
- **Cost optimization suggestions**: Recommend cheaper alternatives

**Use Cases**:
- Per-tenant billing
- Department chargebacks
- Cost anomaly detection
- Budget enforcement

**Impact**: $100K+ savings for large deployments

#### 4. Built-in A/B Testing

**Problem**: Manual A/B testing is complex  
**Solution**: Router handles traffic splitting and metrics

**Features**:
- **Traffic splitting**: 50/50, 70/30, canary (1%)
- **Metric collection**: Latency, cost, quality
- **Statistical significance**: Auto-calculate p-values
- **Auto-winner selection**: Promote winning variant
- **Multi-armed bandit**: Adaptive traffic allocation

**Use Cases**:
- Model comparison (GPT-4 vs Claude)
- Prompt engineering (test variations)
- Provider evaluation (OpenAI vs Azure OpenAI)
- Feature rollouts (canary deployments)

**Impact**: Data-driven provider selection

#### 5. Improved Observability

**Problem**: Basic metrics insufficient for production  
**Solution**: Advanced observability built-in

**Features**:
- **Distributed tracing**: OpenTelemetry standard
- **Real-time dashboards**: Pre-built Grafana dashboards
- **Anomaly detection**: ML-based alerting
- **Predictive alerting**: Warn before incidents
- **Request replay**: Reproduce issues
- **Performance profiling**: Identify bottlenecks

**Use Cases**:
- Debug production issues
- Capacity planning
- SLA monitoring
- Performance optimization

**Impact**: 10x faster incident resolution

#### 6. Enhanced Multi-Tenancy

**Problem**: Basic multi-tenancy insufficient for enterprises  
**Solution**: Enterprise-grade isolation

**Features**:
- **Tenant hierarchies**: Organizations → teams → users
- **Resource quotas**: CPU, memory, cost, requests
- **Tenant-specific SLAs**: Per-tenant uptime guarantees
- **Billing integration**: Export to Stripe, Chargebee
- **Tenant admin panel**: Self-service configuration
- **Tenant isolation**: Network-level separation

**Use Cases**:
- SaaS platforms (100s-1000s of tenants)
- Enterprise departments
- Reseller models
- Managed service providers

**Impact**: Support 10x more tenants per cluster

#### 7. Protocol Improvements (CP2)

**Problem**: JSON/HTTP overhead  
**Solution**: Binary protocol

**Features**:
- **gRPC streaming**: Binary, efficient
- **Compression**: Reduce bandwidth
- **Batching**: Multiple requests in one call
- **Connection pooling**: Reuse connections
- **Protocol negotiation**: CP1/CP2 coexistence

**Impact**:
- 5x throughput improvement
- 3x latency reduction
- 10x bandwidth savings

#### 8. Advanced Extensions

**Problem**: Extensions are stateless, limited  
**Solution**: Powerful extension capabilities

**Features**:
- **Stateful extensions**: Maintain state across requests
- **Extension dependencies**: Extensions call other extensions
- **Hot-reload extensions**: Deploy without restart
- **Extension marketplace**: Community extensions
- **Extension monitoring**: Per-extension metrics
- **Extension versioning**: A/B test extensions

**Use Cases**:
- Complex business logic
- Multi-step workflows
- Extension ecosystem
- Vendor integrations

**Impact**: Infinite extensibility

---

## Breaking Changes in CP2

### Protocol Changes

**CP1 → CP2**:
```json
// CP1 (current)
{
  "version": "1",
  "message": {...},
  "policy_id": "..."
}

// CP2 (future)
message DecideRequest {
  string version = 1;  // "2"
  Message message = 2;
  string policy_id = 3;
  StreamOptions stream = 4;  // NEW
  CostBudget budget = 5;     // NEW
}
```

**NATS Subjects**:
- CP1: `beamline.router.v1.*`
- CP2: `beamline.router.v2.*`

### API Changes

**Policy Format**:
- CP1: Static JSON
- CP2: DSL (compiled to bytecode)

**Extension Interface**:
- CP1: Request-reply
- CP2: Streaming + stateful

**Configuration**:
- CP1: TOML/ENV vars
- CP2: Dynamic config API

### Migration Path

**Backward Compatibility**:
- CP1 and CP2 coexist for 6 months
- Router supports both protocols simultaneously
- Gradual migration (tenant-by-tenant)

**Migration Tools**:
- Automated policy converter (CP1 JSON → CP2 DSL)
- Configuration migration script
- Compatibility testing framework

**Timeline**:
1. **Month 0**: CP2 released with CP1 compat mode
2. **Month 3**: Deprecation warnings for CP1
3. **Month 6**: CP1 support removed (v2.1.0)

---

## Conscious Non-Goals

### What We Deliberately WON'T Build

#### 1. Model Hosting / Inference

**Not Building**:
- ❌ LLM model hosting
- ❌ GPU orchestration
- ❌ Model training
- ❌ Fine-tuning infrastructure

**Why**:
- LLM providers (OpenAI, Anthropic) specialize in this
- Requires massive infrastructure investment
- Not our core competency

**Alternative**:
- Integrate via Extensions
- Route to existing providers
- Use CAF Worker for lightweight inference (if needed)

**Boundary**: Router routes TO models, doesn't HOST them

#### 2. Vector Database

**Not Building**:
- ❌ Vector storage
- ❌ Embedding generation
- ❌ Similarity search
- ❌ Indexing

**Why**:
- Pinecone, Weaviate, Milvus specialize in this
- Complex database engineering
- Not core to routing

**Alternative**:
- Integrate via Extensions
- Use CAF Worker blocks for queries
- Recommend existing solutions

**Boundary**: Router routes queries, doesn't STORE vectors

#### 3. Full Observability Platform

**Not Building**:
- ❌ Long-term metrics storage
- ❌ Alerting platform
- ❌ Dashboarding (full-featured)
- ❌ Log aggregation

**Why**:
- Grafana, Datadog, New Relic specialize
- Mature ecosystem exists
- Focus on routing, not monitoring

**Alternative**:
- Export to Prometheus
- Integrate with existing tools
- Provide pre-built dashboards

**Boundary**: Router exports metrics, doesn't REPLACE observability platforms

#### 4. Message Broker

**Not Building**:
- ❌ Kafka/RabbitMQ replacement
- ❌ Queue management
- ❌ Topic administration
- ❌ Broker clustering

**Why**:
- NATS is our dependency (excellent broker)
- Don't reinvent the wheel
- Focus on routing logic

**Alternative**:
- Use NATS (required dependency)
- Integrate with Kafka via extensions
- CAF Worker for transformations

**Boundary**: Router uses NATS, doesn't REPLACE it

#### 5. Full API Gateway

**Not Building**:
- ❌ OAuth server
- ❌ Developer portal
- ❌ API key management (full-featured)
- ❌ Rate limiting (general HTTP)
- ❌ WAF (Web Application Firewall)

**Why**:
- Kong, Tyk, AWS API Gateway specialize
- General API management != AI routing
- c-gateway handles basics

**Alternative**:
- Use Kong + Router together
- c-gateway for simple cases
- Delegate auth to gateway

**Boundary**: Router is AI-specific, NOT a general API gateway

#### 6. Data Warehouse / Analytics

**Not Building**:
- ❌ OLAP database
- ❌ Data lake
- ❌ BI tools
- ❌ Report generation

**Why**:
- ClickHouse, BigQuery, Snowflake specialize
- Complex data engineering
- Export >> build

**Alternative**:
- Export logs/metrics to warehouse
- Pre-built export scripts
- Integration guides

**Boundary**: Router generates data, doesn't ANALYZE it long-term

#### 7. Billing System

**Not Building**:
- ❌ Invoice generation
- ❌ Payment processing
- ❌ Subscription management
- ❌ Dunning logic

**Why**:
- Stripe, Chargebee specialize
- Compliance/PCI requirements
- Not our expertise

**Alternative**:
- Export cost data
- Integration with Stripe
- Webhooks for events

**Boundary**: Router tracks costs, doesn't BILL customers

---

## Router Scope Boundaries

### What Router IS

**Core Identity**: Intelligent AI Request Router

**Primary Responsibilities**:
1. ✅ **Policy Evaluation**: Which provider for this request?
2. ✅ **Provider Selection**: Route to OpenAI, Anthropic, etc.
3. ✅ **Cost Optimization**: Minimize spend per request
4. ✅ **Extensions Pipeline**: Pre/validate/post processing
5. ✅ **Multi-Tenant Isolation**: Separate tenants safely
6. ✅ **Basic Observability**: Metrics, logs, traces
7. ✅ **Request Transformation**: Normalize formats

**In Scope**:
- Routing logic
- Policy language
- Cost tracking
- Latency optimization
- Provider failover
- A/B testing (routing-level)

### What Router IS NOT

**Not Responsible For**:
1. ❌ **Model Inference**: That's OpenAI/Anthropic's job
2. ❌ **Long-Term Storage**: Use databases, data lakes
3. ❌ **User Authentication**: Delegate to c-gateway/API gateway
4. ❌ **Billing**: Export data to Stripe
5. ❌ **Vector Search**: Use Pinecone, Weaviate
6. ❌ **Full API Management**: Use Kong, Tyk

**Out of Scope**:
- Hosting models
- Storing embeddings
- Managing users (gateway does this)
- Generating invoices
- Complex analytics (export data)

### Integration Philosophy

**Router is a HUB, not a PLATFORM**:
- Integrates with best-of-breed tools
- Exports data to specialized systems
- Focuses on routing excellence
- Doesn't reinvent wheels

**Example Stack**:
```
 User Auth: Keycloak/Auth0
     ↓
 API Gateway: Kong/Tyk
     ↓
 AI Router: Beamline ← (Focus here)
     ↓
 Models: OpenAI, Anthropic
 
 Observability: Grafana/Datadog
 Analytics: ClickHouse/BigQuery
 Billing: Stripe/Chargebee
 Vectors: Pinecone/Weaviate
```

**Router's Role**: Intelligent connection layer

---

## Phased Roadmap

### Phase 1: CP1 Refinement (Current - Q2 2025)

**Version**: 1.0.0 → 1.5.0

**Focus**: Production hardening

**Features**:
- 1.1.0: Performance improvements (10x throughput)
- 1.2.0: Enhanced extensions (stateful preview)
- 1.3.0: Better observability (OpenTelemetry)
- 1.4.0: Multi-tenancy improvements (hierarchies)
- 1.5.0: Stability milestone (99.99% uptime)

**Goal**: Production-ready, battle-tested CP1

### Phase 2: CP2 Design & Prototyping (Q2-Q3 2025)

**Duration**: 6 months

**Activities**:
- Design CP2 protocol specification
- Prototype streaming (gRPC)
- Policy language design (DSL syntax)
- Breaking changes analysis
- Migration tooling design

**Deliverables**:
- CP2 Specification (RFC)
- Streaming prototype
- Policy language grammar
- Migration guide draft

### Phase 3: CP2 Development (Q4 2025 - Q2 2026)

**Duration**: 9 months

**Milestones**:
1. **M1**: Protocol implementation (Q4 2025)
   - gRPC streaming
   - Binary protocol
   - Backward compat mode

2. **M2**: Core Features (Q1 2026)
   - Policy DSL compiler
   - Advanced cost tracking
   - A/B testing framework

3. **M3**: Advanced Features (Q2 2026)
   - Enhanced multi-tenancy
   - Stateful extensions
   - Real-time dashboards

**Release**: 2.0.0 (Q3 2026)

### Phase 4: CP2 Adoption (Q3-Q4 2026)

**Duration**: 6 months

**Focus**:
- Community migration (provide tools)
- Enterprise customer upgrades
- Bug fixes and polish
- Documentation and examples

**Goal**: 50% adoption of CP2 within 6 months

### Long-Term Vision (2027+)

**Version**: 3.0.0+

**Possibilities** (not committed):
- Multi-cloud routing (AWS, GCP, Azure)
- Edge deployment (latency optimization)
- Hardware acceleration (FPGA for routing)
- ML-driven policy optimization
- Self-tuning policies

---

## Success Metrics

### CP2 Adoption

- **6 months**: 50% of users on CP2
- **12 months**: 90% of users on CP2
- **18 months**: CP1 EOL

### Performance Targets

- **Throughput**: 5x improvement (2.5M req/sec)
- **Latency**: 3x reduction (< 300µs p99)
- **Cost**: 50% reduction (better routing)

### Community Growth

- **GitHub Stars**: 5K+ (from 1K)
- **Contributors**: 100+ (from 20)
- **Extensions**: 50+ community extensions

---

## FAQ

**Q: Will CP1 be supported after CP2?**  
A: Yes, for 6 months. Then deprecated, EOL after 18 months.

**Q: Can I migrate gradually?**  
A: Yes, tenant-by-tenant migration supported.

**Q: Will CP2 require Erlang upgrade?**  
A: Possibly. Will specify minimum OTP version.

**Q: Is the scope too ambitious?**  
A: Phased approach. Core features first, advanced later.

**Q: Why not build X feature?**  
A: Focus. Best-of-breed integrations >> monolith.

---

## Conclusion

**CP2 represents**:
- Next-generation routing protocol
- Streaming and advanced features
- Clear scope boundaries
- Sustainable product vision

**We deliberately**:
- Focus on routing excellence
- Integrate with specialists
- Avoid feature bloat
- Stay lean and fast

**The future is**:
- Powerful yet focused
- Extensible via integrations
- Production-ready
- Community-driven

---

**CP2: Intelligent AI routing at scale, nothing more, nothing less.**

**Last Updated**: 2025-12-22  
**Document Version**: 1.0.0
