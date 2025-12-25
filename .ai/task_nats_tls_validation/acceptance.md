# Acceptance — T-SEC-01

## Done criteria
1. Сертификаты успешно генерируются скриптом `scripts/generate_certs.sh`.
2. NATS сервер запускается с включенным TLS (проверяется через `varz` или логи).
3. Router успешно подключается к NATS по адресу `nats://localhost:4222` с включенными TLS опциями.
4. Heavy CT тесты проходят успешно при включенном TLS.
5. `progress.md` содержит подтверждение (логи) успешного TLS handshake.
