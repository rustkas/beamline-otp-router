# Plan — T-SEC-01

1. Создать скрипт `scripts/generate_certs.sh` для создания тестового PKI.
2. Подготовить конфиг файл `config/nats_tls.conf`.
3. Обновить `scripts/nats_start.sh` для поддержки флага `--tls`.
4. Обновить конфигурацию Router в `config/test_real_nats.config` для передачи ssl опций в `enats`.
5. Проверить соединение через `rebar3 ct`.
6. Собрать логи и зафиксировать результат в `progress.md`.
