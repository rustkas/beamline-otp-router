# R10 Incident Playbooks

**Purpose**: Structured incident response playbooks for R10 circuit breaker incidents  
**Format**: YAML specifications that can be converted to Markdown playbooks

---

## Overview

This directory contains YAML-based incident playbook specifications for R10 circuit breaker incidents. These playbooks can be automatically converted to Markdown using the `r10_playbook_gen.py` script.

---

## Available Playbooks

### 1. Latency Outage

**File**: `r10_latency_outage.yaml`

**Scenario**: Provider latency exceeds threshold, triggering circuit breaker

**Alert**: `R10LatencyTriggerDominating`

**Generated**: `R10_LATENCY_OUTAGE_PLAYBOOK.md`

---

## Usage

### Generate Playbook from YAML

```bash
python3 scripts/r10_playbook_gen.py \
  --input apps/otp/router/incident_playbooks/r10_latency_outage.yaml \
  --output apps/otp/router/incident_playbooks/R10_LATENCY_OUTAGE_PLAYBOOK.md
```

### Create New Playbook

1. Copy `r10_latency_outage.yaml` as a template
2. Update fields:
   - `id`: Unique identifier
   - `title`: Descriptive title
   - `severity`: high/warning/info
   - `tenant_id` / `provider_id`: Test identifiers
   - `trigger`: Alert type and description
   - `impact`: Technical and business impact
   - `detection`: How to detect the incident
   - `diagnosis`: Diagnostic steps
   - `mitigation`: Mitigation actions
   - `verification`: Verification checks
   - `postmortem`: Post-incident tasks

3. Generate Markdown:
   ```bash
   python3 scripts/r10_playbook_gen.py \
     --input apps/otp/router/incident_playbooks/your_playbook.yaml \
     --output apps/otp/router/incident_playbooks/YOUR_PLAYBOOK.md
   ```

---

## YAML Schema

```yaml
id: unique_identifier
title: "Descriptive Title"
severity: high | warning | info
tenant_id: "tenantX"
provider_id: "providerX"

trigger:
  type: latency | error | flapping
  alert_name: "AlertName"
  description: "Detailed description"

impact:
  description: "Technical impact"
  blast_radius: "Affected components"
  business_impact: "Business impact"

detection:
  alerts:
    - "Alert name or description"
  dashboards:
    - "Dashboard panel or view"
  cli:
    - "./router_ctl r10 status ..."

diagnosis:
  steps:
    - "Diagnostic step 1"
    - "Diagnostic step 2"

mitigation:
  steps:
    - "Mitigation action 1"
    - "Mitigation action 2"

verification:
  checks:
    - "Verification check 1"
    - "Verification check 2"

postmortem:
  tasks:
    - "Post-incident task 1"
    - "Post-incident task 2"
```

---

## Integration with Incident Management

**Recommended workflow**:

1. **Create YAML playbook** for new incident type
2. **Generate Markdown** using `r10_playbook_gen.py`
3. **Review and refine** generated playbook
4. **Link from alert annotations** (runbook_url)
5. **Add to incident ticket templates** (Jira/YouTrack/etc.)
6. **Update after real incidents** with lessons learned

---

## References

- **Operational Checklist**: `docs/R10_SRE_OPERATIONAL_CHECKLIST.md`
- **How to Operate**: `docs/R10_HOW_TO_OPERATE.md`
- **Incident Simulation**: `test/R10_INCIDENT_SIMULATION.md`
- **Runbook**: `test/R10_RUNBOOK.md`
