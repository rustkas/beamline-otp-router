# Product Framing Assessment 

## Capability Summary 

The Router implements a policy-driven decision pipeline and dispatches execution 
requests to external systems. 

- Decision logic: `router_decider.erl` 
- Execution dispatch: `router_caf_adapter.erl` 
- State: ETS soft-state (`router_policy_store.erl` ) 
-  Interfaces: NATS (primary), gRPC (internal) 

## Framing Options Evaluation 

### Standalone Service — REJECTED 
-  No persistent state 
-  No execution responsibility 
-  Misleading expectations 

### Licensable Infrastructure — REJECTED 
-  Domain-specific contracts 
-  Not a generic proxy/router 

### Core Platform Engine — SELECTED 
-  Centralized decision-making 
-  Decouples policy from execution 
-  Designed as an internal brain of the platform 

## Final Positioning 

**The Router is a Core Platform Decision Engine.** 

It governs execution behavior via policy and dispatches work to external 
execution backends. It is not intended to operate as a standalone product. 
