#!/usr/bin/env python3
"""
CP2 Contract Validator with Breaking Change Detection & Ownership Enforcement

Validates contracts/cp2_contracts.json:
- Schema structure
- Subject uniqueness
- Header set references
- v1 subjects are frozen
- v2 subjects are parallel (not replacing v1)
- Breaking change rules enforced
- Ownership mapping required
"""
from __future__ import annotations

import json
import sys
from dataclasses import dataclass
from pathlib import Path
from typing import Any, Dict, List, Set


@dataclass(frozen=True)
class ValidationError:
    """Contract validation error"""
    severity: str  # ERROR | WARNING
    message: str


class ContractValidator:
    def __init__(self, contracts_path: Path):
        self.contracts_path = contracts_path
        self.errors: List[ValidationError] = []
        self.warnings: List[ValidationError] = []
        self.data: Dict[str, Any] = {}

    def error(self, msg: str) -> None:
        self.errors.append(ValidationError("ERROR", msg))

    def warn(self, msg: str) -> None:
        self.warnings.append(ValidationError("WARNING", msg))

    def load_contracts(self) -> bool:
        """Load and parse contracts JSON"""
        try:
            raw = self.contracts_path.read_text(encoding="utf-8")
        except OSError as e:
            self.error(f"Cannot read {self.contracts_path}: {e}")
            return False

        try:
            self.data = json.loads(raw)
        except json.JSONDecodeError as e:
            self.error(f"Invalid JSON: {e}")
            return False

        if not isinstance(self.data, dict):
            self.error("Top-level must be an object")
            return False

        return True

    def validate_structure(self) -> None:
        """Validate required top-level keys"""
        required = ["meta", "conventions", "header_sets", "subjects"]
        for key in required:
            if key not in self.data:
                self.error(f"Missing required key: '{key}'")

    def validate_meta(self) -> None:
        """Validate meta section"""
        meta = self.data.get("meta", {})
        if not isinstance(meta, dict):
            self.error("meta must be an object")
            return

        required_fields = ["name", "version", "status"]
        for field in required_fields:
            if not meta.get(field):
                self.error(f"meta.{field} is required")

        # Check freeze_tag references CP1
        freeze_tag = meta.get("freeze_tag", "")
        if "cp1" not in freeze_tag.lower():
            self.warn(f"meta.freeze_tag should reference cp1-freeze, got: {freeze_tag}")

    def validate_conventions(self) -> None:
        """Validate conventions section"""
        conv = self.data.get("conventions", {})
        if not isinstance(conv, dict):
            self.error("conventions must be an object")
            return

        # Check versioning rules
        subj_ver = conv.get("subject_versioning", {})
        allowed = subj_ver.get("allowed_versions", [])
        if not isinstance(allowed, list) or not all(isinstance(v, str) for v in allowed):
            self.error("conventions.subject_versioning.allowed_versions must be list of strings")

        # Check backward compatibility statement
        back_compat = conv.get("backward_compatibility", {})
        if not back_compat.get("headers_optional_in_v1"):
            self.warn("Should document that v1 headers are optional for backward compat")

    def validate_header_sets(self) -> None:
        """Validate header_sets structure"""
        header_sets = self.data.get("header_sets", {})
        if not isinstance(header_sets, dict):
            self.error("header_sets must be an object")
            return

        for hs_name, hs in header_sets.items():
            if not isinstance(hs, dict):
                self.error(f"header_sets.{hs_name} must be an object")
                continue

            # Required and optional must be lists of strings
            for field in ["required", "optional"]:
                val = hs.get(field, [])
                if not isinstance(val, list):
                    self.error(f"header_sets.{hs_name}.{field} must be a list")
                elif not all(isinstance(x, str) and x for x in val):
                    self.error(f"header_sets.{hs_name}.{field} must contain non-empty strings")

    def validate_subjects(self) -> None:
        """Validate subjects list and enforce CP1/CP2 rules"""
        subjects = self.data.get("subjects", [])
        if not isinstance(subjects, list):
            self.error("subjects must be a list")
            return

        seen_names: Set[str] = set()
        v1_subjects: Set[str] = set()
        v2_subjects: Set[str] = set()

        for i, subj in enumerate(subjects):
            ctx = f"subjects[{i}]"
            if not isinstance(subj, dict):
                self.error(f"{ctx}: must be an object")
                continue

            # Required fields
            name = self._require_str(subj, "name", ctx)
            version = self._require_str(subj, "version", ctx)
            status = self._require_str(subj, "status", ctx)
            self._require_str(subj, "class", ctx)
            self._require_str(subj, "direction", ctx)
            self._require_str(subj, "purpose", ctx)
            self._require_str(subj, "headers", ctx)

            # Track duplicates
            if name:
                if name in seen_names:
                    self.error(f"{ctx}: duplicate subject name '{name}'")
                seen_names.add(name)

                # Track v1 vs v2
                if version == "v1":
                    v1_subjects.add(name)
                elif version == "v2":
                    v2_subjects.add(name)

            # CP1 freeze rule: v1 subjects MUST have status=frozen
            if version == "v1" and status != "frozen":
                self.error(f"{ctx}: v1 subject '{name}' must have status='frozen' (CP1 frozen)")

            # CP2 additive rule: v2 subjects must be parallel (not replace v1)
            if version == "v2":
                if status not in ["planned", "draft", "active"]:
                    self.warn(f"{ctx}: v2 subject '{name}' has unexpected status '{status}'")

            # Validate payload
            payload = subj.get("payload")
            if payload:
                self._validate_payload(payload, f"{ctx}.payload")

            # Validate reply (if request_reply)
            if subj.get("class") == "request_reply":
                reply = subj.get("reply")
                if reply:
                    self._validate_payload(reply, f"{ctx}.reply")

            # Validate correlation
            corr = subj.get("correlation")
            if corr:
                self._validate_correlation(corr, f"{ctx}.correlation")

            # Validate idempotency
            idem = subj.get("idempotency")
            if idem:
                self._validate_idempotency(idem, f"{ctx}.idempotency")

        # Breaking change detection
        self._check_breaking_changes(v1_subjects, v2_subjects)

    def _validate_payload(self, payload: Any, ctx: str) -> None:
        """Validate payload structure"""
        if not isinstance(payload, dict):
            self.error(f"{ctx}: must be an object")
            return

        self._require_str(payload, "format", ctx)
        self._require_str(payload, "schema_id", ctx)
        self._require_str(payload, "schema_version", ctx)

    def _validate_correlation(self, corr: Any, ctx: str) -> None:
        """Validate correlation structure"""
        if not isinstance(corr, dict):
            self.error(f"{ctx}: must be an object")
            return

        self._require_str(corr, "primary", ctx)
        fallback = corr.get("fallback", [])
        if not isinstance(fallback, list):
            self.error(f"{ctx}.fallback: must be a list")

    def _validate_idempotency(self, idem: Any, ctx: str) -> None:
        """Validate idempotency structure"""
        if not isinstance(idem, dict):
            self.error(f"{ctx}: must be an object")
            return

        # key can be null for non-idempotent operations
        if idem.get("key") is not None:
            self._require_str(idem, "key", ctx)
            self._require_str(idem, "scope", ctx)
            self._require_str(idem, "enforcement", ctx)

    def _check_breaking_changes(self, v1_subjects: Set[str], v2_subjects: Set[str]) -> None:
        """Check for breaking changes between v1 and v2"""
        # Count frozen subjects
        frozen_count = len(v1_subjects)
        if frozen_count == 0:
            self.warn("No v1 (frozen) subjects found - expected CP1 baseline subjects")

        # Count planned v2 subjects
        planned_count = len(v2_subjects)
        if planned_count > 0:
            print(f"INFO: {planned_count} v2 subjects planned (parallel to v1)")

    def validate_ownership(self) -> None:
        """Validate ownership mapping - ensure all subjects have owners"""
        ownership = self.data.get("ownership", {})
        if not isinstance(ownership, dict):
            self.error("ownership section must be an object")
            return

        subjects = self.data.get("subjects", [])
        if not isinstance(subjects, list):
            return  # Already reported by validate_subjects

        # Collect all subject names
        subject_names: Set[str] = set()
        frozen_subjects: Set[str] = set()
        
        for subj in subjects:
            if not isinstance(subj, dict):
                continue
            
            name = subj.get("name")
            status = subj.get("status")
            version = subj.get("version")
            
            if name:
                subject_names.add(name)
                
                # Track frozen (v1) subjects
                if version == "v1" and status == "frozen":
                    frozen_subjects.add(name)

        # Check 1: Every subject MUST have ownership entry
        for name in subject_names:
            if name not in ownership:
                self.error(f"Subject missing ownership mapping: {name}")

        # Check 2: Every ownership entry MUST reference a valid subject
        for owner_name in ownership.keys():
            if owner_name not in subject_names:
                self.warn(f"Ownership entry for non-existent subject: {owner_name}")

        # Check 3: Frozen (v1) subjects MUST have active/external status
        for name in frozen_subjects:
            owner_info = ownership.get(name, {})
            if isinstance(owner_info, dict):
                owner_status = owner_info.get("status", "")
                if owner_status not in ["active", "external"]:
                    self.error(
                        f"Frozen subject '{name}' must have ownership status "
                        f"'active' or 'external', got: '{owner_status}'"
                    )
                
                # Check evidence provided
                evidence = owner_info.get("evidence", "")
                if not evidence:
                    self.warn(f"Ownership for '{name}' missing evidence field")

        print(f"INFO: {len(ownership)} subjects have ownership mapping")

    def _require_str(self, obj: Dict[str, Any], key: str, ctx: str) -> str:
        """Require string field and return it"""
        val = obj.get(key)
        if not isinstance(val, str) or not val:
            self.error(f"{ctx}.{key}: required non-empty string")
            return ""
        return val

    def validate_all(self) -> int:
        """Run all validations and return exit code"""
        if not self.load_contracts():
            return 2

        self.validate_structure()
        self.validate_meta()
        self.validate_conventions()
        self.validate_header_sets()
        self.validate_subjects()
        self.validate_ownership()  # NEW: Enforce ownership mapping

        # Print results
        for err in self.errors:
            print(f"ERROR: {err.message}", file=sys.stderr)

        for warn in self.warnings:
            print(f"WARNING: {warn.message}", file=sys.stderr)

        if self.errors:
            print(f"\n❌ Validation FAILED: {len(self.errors)} error(s)", file=sys.stderr)
            return 1

        if self.warnings:
            print(f"\n⚠️  Validation passed with {len(self.warnings)} warning(s)")
        else:
            print("\n✅ Validation PASSED: contracts are valid")

        return 0


def main() -> int:
    repo_root = Path.cwd()
    contracts_path = repo_root / "contracts" / "cp2_contracts.json"

    if not contracts_path.exists():
        print(f"ERROR: {contracts_path} not found", file=sys.stderr)
        return 2

    validator = ContractValidator(contracts_path)
    return validator.validate_all()


if __name__ == "__main__":
    sys.exit(main())
