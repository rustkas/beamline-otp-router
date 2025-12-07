#!/usr/bin/env python3
"""
Generate R10 incident playbook (Markdown) from YAML spec.

Usage:
    python3 scripts/r10_playbook_gen.py \
        --input apps/otp/router/incident_playbooks/r10_latency_outage.yaml \
        --output apps/otp/router/incident_playbooks/R10_LATENCY_OUTAGE_PLAYBOOK.md
"""

import argparse
from pathlib import Path
from typing import Any, Dict
import yaml

PLAYBOOK_TEMPLATE = """# {title}

**ID:** {id}  
**Severity:** {severity}  
**Tenant:** {tenant_id}  
**Provider:** {provider_id}  

---

## 1. Context

**Trigger type:** {trigger_type}  
**Alert name:** {alert_name}  

**Description:**  

{trigger_description}

---

## 2. Impact

- **Technical impact:** {impact_description}

- **Blast radius:** {blast_radius}

- **Business impact:** {business_impact}

---

## 3. Detection

### 3.1 Alerts

{alerts_block}

### 3.2 Dashboards

{dashboards_block}

### 3.3 CLI

{cli_block}

---

## 4. Diagnosis

{diagnosis_block}

---

## 5. Mitigation

{mitigation_block}

---

## 6. Verification

{verification_block}

---

## 7. Postmortem

{postmortem_block}

"""


def _format_list_block(items, bullet: str = "-") -> str:
    if not items:
        return f"{bullet} (none specified)"
    return "\n".join(f"{bullet} {item}" for item in items)


def _format_steps_block(steps, title_prefix: str = "") -> str:
    if not steps:
        return "- (no steps specified)"
    lines = []
    for idx, step in enumerate(steps, start=1):
        prefix = f"{title_prefix}{idx}."
        lines.append(f"{prefix} {step}")
    return "\n".join(lines)


def load_yaml(path: Path) -> Dict[str, Any]:
    with path.open("r", encoding="utf-8") as f:
        data = yaml.safe_load(f)
    if not isinstance(data, dict):
        raise ValueError(f"Top-level YAML must be a mapping, got {type(data)}")
    return data


def render_playbook(spec: Dict[str, Any]) -> str:
    trigger = spec.get("trigger", {})
    impact = spec.get("impact", {})
    detection = spec.get("detection", {})
    diagnosis = spec.get("diagnosis", {})
    mitigation = spec.get("mitigation", {})
    verification = spec.get("verification", {})
    postmortem = spec.get("postmortem", {})

    alerts_block = _format_list_block(detection.get("alerts", []))
    dashboards_block = _format_list_block(detection.get("dashboards", []))
    cli_block = _format_list_block(detection.get("cli", []))

    diagnosis_block = _format_steps_block(diagnosis.get("steps", []), title_prefix="")
    mitigation_block = _format_steps_block(mitigation.get("steps", []), title_prefix="")
    verification_block = _format_steps_block(verification.get("checks", []), title_prefix="")
    postmortem_block = _format_steps_block(postmortem.get("tasks", []), title_prefix="")

    content = PLAYBOOK_TEMPLATE.format(
        id=spec.get("id", "unknown_id"),
        title=spec.get("title", "Untitled R10 Incident Playbook"),
        severity=spec.get("severity", "unknown"),
        tenant_id=spec.get("tenant_id", "-"),
        provider_id=spec.get("provider_id", "-"),
        trigger_type=trigger.get("type", "-"),
        alert_name=trigger.get("alert_name", "-"),
        trigger_description=trigger.get("description", "(not specified)"),
        impact_description=impact.get("description", "(not specified)"),
        blast_radius=impact.get("blast_radius", "(not specified)"),
        business_impact=impact.get("business_impact", "(not specified)"),
        alerts_block=alerts_block,
        dashboards_block=dashboards_block,
        cli_block=cli_block,
        diagnosis_block=diagnosis_block,
        mitigation_block=mitigation_block,
        verification_block=verification_block,
        postmortem_block=postmortem_block,
    )

    return content


def main() -> None:
    parser = argparse.ArgumentParser(
        description="Generate R10 incident playbook (Markdown) from YAML spec."
    )
    parser.add_argument(
        "--input",
        "-i",
        required=True,
        help="Path to YAML incident spec.",
    )
    parser.add_argument(
        "--output",
        "-o",
        required=True,
        help="Path to output Markdown file.",
    )
    args = parser.parse_args()

    input_path = Path(args.input)
    output_path = Path(args.output)

    if not input_path.exists():
        parser.error(f"Input file does not exist: {input_path}")

    spec = load_yaml(input_path)
    markdown = render_playbook(spec)

    output_path.parent.mkdir(parents=True, exist_ok=True)
    output_path.write_text(markdown, encoding="utf-8")

    print(f"Generated playbook: {output_path}")


if __name__ == "__main__":
    main()

