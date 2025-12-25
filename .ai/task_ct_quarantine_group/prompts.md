# Key Prompts and Decisions

## Initial Task Setup
**Prompt**: Create a new task for implementing native Common Test group/tag support for quarantine functionality, building upon the existing runner-level quarantine implementation.

**Key Decisions**:
1. Create a new task instead of modifying the existing quarantine task
2. Maintain backward compatibility with the existing mechanism
3. Use `{quarantine, [...]}` as the group syntax
4. Focus on documentation and planning in the initial phase

## Implementation Approach
**Prompt**: How should the quarantine group be implemented in Common Test suites?

**Decision**:
- Use Common Test's built-in group functionality
- Support both single tests and test case groups
- Make the implementation non-disruptive to existing tests
- Document the migration path from the old mechanism

## CI/CD Integration
**Prompt**: How should the quarantine group be handled in CI/CD pipelines?

**Decision**:
- Exclude quarantine group from PR/CI test runs by default
- Include quarantine group in nightly/heavy test runs
- Add configuration options to override default behavior when needed
- Document the CI/CD integration points

## Future Considerations
- Plan for deprecation of the old quarantine mechanism
- Monitor usage of the new quarantine group
- Consider automation for migrating existing quarantined tests
