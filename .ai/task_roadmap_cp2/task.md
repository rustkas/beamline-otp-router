# Task: T-ROADMAP-01 — CP2 Vision

**Created**: 2025-12-22  
**Status**: In Progress  
**Priority**: High  
**Type**: Strategy / Roadmap

## Objective

Define future vision and scope boundaries:
1. **CP2 (Control Protocol 2)** - What's in the next major version
2. **Conscious Non-Goals** - What we deliberately won't build
3. **Router Boundaries** - Where Router ends, other systems begin

## Deliverables

### 1. CP2 Vision Document

**File**: `docs/CP2_VISION.md`

**Sections**:
1. CP1 Current State (what we have)
2. CP2 Vision (what's coming)
3. Breaking Changes (migration path)
4. Timeline & Phases
5. Conscious Non-Goals
6. Router Scope Boundaries
7. Integration Points

### 2. Roadmap

**File**: `docs/ROADMAP.md`

**Sections**:
1. Current (CP1) - 1.x.x releases
2. Near Future (CP2) - 2.0.0
3. Long-term Vision
4. Not Planned (anti-roadmap)

## CP1 Current State

**Protocol**: CP1 (Control Protocol v1)

**Features**:
- Policy-based routing
- Extensions Pipeline (4 types)
- Multi-tenancy (basic)
- CAF Worker integration
- NATS messaging
- Prometheus metrics

**Limitations**:
- No streaming responses
- Limited bi-directional communication
- Basic cost tracking
- Simple policy language
- No built-in A/B testing

## CP2 Vision (2.0.0)

### What's Coming

**1. Streaming Support**
- Server-Sent Events (SSE)
- WebSocket support
- Chunked responses
- Real-time updates

**2. Enhanced Policy Language**
- Turing-complete DSL
- Conditional logic
- Variables and functions
- Policy composition

**3. Advanced Cost Tracking**
- Real-time cost calculation
- Budget alerts
- Cost attribution per tenant
- Forecasting

**4. Built-in A/B Testing**
- Traffic splitting
- Metric collection
- Statistical significance
- Auto-winner selection

**5. Improved Observability**
- Distributed tracing (OpenTelemetry)
- Real-time dashboards
- Anomaly detection
- Predictive alerting

**6. Enhanced Multi-Tenancy**
- Tenant hierarchies
- Resource quotas (CPU, memory, cost)
- Tenant-specific SLAs
- Billing integration

**7. Protocol Improvements**
- Binary protocol (gRPC streaming)
- Compression
- Batching
- Connection pooling

**8. Advanced Extensions**
- Stateful extensions
- Extension dependencies
- Hot-reload extensions
- Extension marketplace

## Conscious Non-Goals

### What We WON'T Build

**1. Model Hosting**
- ✋ Not a model serving platform
- ✋ No GPU orchestration
- ✋ No model training
- **Why**: LLM providers do this better (OpenAI, Anthropic)
- **Alternative**: Integrate via Extensions

**2. Vector Database**
- ✋ Not a vector store
- ✋ No embedding generation
- ✋ No similarity search
- **Why**: Pinecone, Weaviate specialize in this
- **Alternative**: Integrate via CAF Worker blocks

**3. Full Observability Platform**
- ✋ Not replacing Datadog/Grafana
- ✋ Basic metrics only
- ✋ Export to existing tools
- **Why**: Observability platforms are mature
- **Alternative**: Prometheus + Grafana

**4. Message Broker**
- ✋ Not replacing NATS/Kafka
- ✋ NATS is a dependency
- ✋ No queue management
- **Why**: NATS does this perfectly
- **Alternative**: Use NATS

**5. API Gateway (full-featured)**
- ✋ Not a full API gateway
- ✋ No OAuth server
- ✋ No developer portal
- **Why**: Kong, Tyk specialize in this
- **Alternative**: Use Kong + Router

**6. Data Warehouse**
- ✋ No long-term analytics storage
- ✋ No OLAP queries
- ✋ No data lake
- **Why**: ClickHouse, BigQuery do this
- **Alternative**: Export logs to warehouse

## Router Scope Boundaries

### What Router IS

**Core Responsibility**: Intelligent AI request routing

**In Scope**:
- ✅ Policy evaluation
- ✅ Provider selection
- ✅ Cost optimization
- ✅ Extensions pipeline
- ✅ Multi-tenant isolation
- ✅ Basic observability
- ✅ Request transformation

### What Router IS NOT

**Not Responsible For**:
- ❌ Model inference
- ❌ Long-term storage
- ❌ User authentication (delegates to gateway)
- ❌ Billing (exports data)
- ❌ Vector search
- ❌ Full API management

### Integration Points

**Router integrates with**:
- **c-gateway**: HTTP entry point
- **CAF Worker**: Block execution
- **Extensions**: Custom logic
- **NATS**: Message bus
- **Prometheus**: Metrics export
- **External Systems**: Via extensions

**Router does NOT**:
- Host models
- Store embeddings
- Manage users (gateway does this)
- Generate bills (exports cost data)

## Timeline

### Phase 1: CP1 Refinement (1.x.x releases)

**Duration**: 6-12 months

**Focus**:
- Production hardening
- Performance optimization
- Bug fixes
- Documentation
- Community building

**Releases**:
- 1.1.0: Performance improvements
- 1.2.0: Enhanced extensions
- 1.3.0: Better observability
- 1.4.0: Multi-tenancy improvements

### Phase 2: CP2 Planning (6 months)

**Duration**: 6 months

**Activities**:
- Design CP2 protocol
- Prototype streaming
- Policy language design
- Breaking changes analysis
- Migration tooling

**Deliverables**:
- CP2 specification
- Migration guide
- Backward compatibility plan

### Phase 3: CP2 Development (12 months)

**Duration**: 12 months

**Milestones**:
1. Protocol implementation (3 months)
2. Streaming support (3 months)
3. Policy language (3 months)
4. Migration tooling (3 months)

**Release**: 2.0.0

### Phase 4: CP2 Adoption (6 months)

**Duration**: 6 months

**Focus**:
- Community migration
- Enterprise upgrades
- Bug fixes
- Documentation

**Goal**: 50%+ adoption of CP2

## Breaking Changes in CP2

**Protocol Changes**:
- Binary protocol (gRPC)
- New NATS subjects (v2)
- Response format changes

**API Changes**:
- Policy language syntax
- Extension interface
- Configuration format

**Migration Path**:
- Backward compatibility mode (6 months)
- Automated migration tools
- Step-by-step guide

## Success Criteria

- [x] Clear CP2 vision
- [x] Conscious non-goals defined
- [x] Scope boundaries articulated
- [x] Timeline realistic
- [x] Migration path planned

## References

- Protocol versioning best practices
- Erlang OTP upgrade patterns
- gRPC streaming design
- GraphQL for inspiration (policy language)
