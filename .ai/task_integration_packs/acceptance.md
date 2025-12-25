# Acceptance — T-INTEG-01

## Done criteria
1. Есть `docs/INTEGRATION_GUIDE.md` (или секция в каноне) с пошаговой интеграцией.
2. Есть runnable example (script или маленький клиент) в `examples/`:
   * отправить request
   * получить response
3. Есть mock/staging harness скрипты:
   * start/stop NATS/JS
   * health checks
   * logs в `_artifacts/`
4. Есть contract-test suite (пусть минимальный) с командой запуска.
