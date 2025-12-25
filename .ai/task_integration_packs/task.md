# T-INTEG-01 — Integration Packs

## Problem
Router интегрируется через NATS/JetStream + gRPC, но внешним интеграторам (Gateway/CAF/third-party) нужны “готовые пакеты”: примеры, SDK/clients, mock server, contract tests, и checklist интеграции.

## Goal
Сделать integration packs: минимальный набор артефактов, которые позволяют сторонней системе быстро и правильно подключиться к Router.

## Outcome
Появляется папка/набор материалов: примеры клиентов, contract-test набор, mock NATS harness, и интеграционный чеклист.
