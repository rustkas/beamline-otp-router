# T-OPS-01 — Rollback and Recovery Runbooks

## Problem
Для эксплуатации в продакшене (Internal SRE) недостаточно иметь скрипты. Необходимо иметь пошаговые инструкции (runbooks) по действиям в случае сбоев: как откатить версию (Rollback), как действовать при переполнении очередей JetStream, как сбросить состояние Circuit Breaker.

## Goal
Создать операционную документацию (Runbooks):
- **Rollback Runbook**: Пошаговый процесс отката с использованием `scripts/rollback.sh`.
- **NATS Recovery Runbook**: Действия при потере связи с NATS или переполнении JetStream.
- **Circuit Breaker Runbook**: Как интерпретировать алерты от CB и при необходимости принудительно открывать/закрывать их (через административный API).

## Outcome
1. Файл `docs/operations/ROLLBACK_RUNBOOK.md`.
2. Файл `docs/operations/RECOVERY_RUNBOOK.md`.
3. Описанные сценарии "Clean State Restart" (полная очистка и перезапуск).
