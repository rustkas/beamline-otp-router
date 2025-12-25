## State Management
- **Stateless / Soft-State Design**: 
  - **Policies**: Stored in in-memory ETS tables (`router_policy_store`).
  - **Persistence**: Currently loads from fixtures (CP1); no database dependency.
  - **Fault Tolerance**: Uses ETS heir pattern to preserve state across process restarts.
  - **Assignments**: Published to CAF/NATS; not stored in the router.
