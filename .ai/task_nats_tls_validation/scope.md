# Scope — T-SEC-01

## In scope
- Генерация самоподписанных сертификатов для тестирования.
- Изменение конфигурации NATS Server (tls block).
- Настройка `router` (через `config/test_real_nats.config` или `sys.config`).
- Тестирование Handshake и передачи данных.

## Out of scope
- Настройка внешнего CA (типа Let's Encrypt).
- Ротация сертификатов (Certificate Rotation).
- TLS для gRPC API самого роутера (фокус только на NATS).

## Constraints
- Использовать `openssl` для генерации сертификатов.
- Поддержка TLS v1.2+.
