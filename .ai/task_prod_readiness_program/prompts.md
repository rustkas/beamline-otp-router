# Operating Instructions — T-PROD-01

- Всегда работать из корня repo.
- Любой прогон: `2>&1 | tee _artifacts/<name>_<ts>.log` и сохранить команду в progress.md.
- Не менять семантику контрактов без обновления `docs/API_CONTRACTS.md`.
- Не “чинить” flaky тесты костылями (sleep), только детерминированные ожидания.
