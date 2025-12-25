# Title
Repo-wide sweep: eliminate legacy .ide subjects outside controlled alias/deprecation

# Context
- Invariant: `.ide.` subjects must not appear outside controlled alias tests and explicit deprecation notes.
- Repo-wide drift creates accidental compatibility obligations.

# Goal (RU)
Вычистить `.ide.` references по всему репо, кроме контролируемых мест: alias-mode tests и deprecation note.

# Goal (EN)
Remove `.ide.` references across the repo except controlled locations: alias-mode tests and explicit deprecation notes.

# Scope
- Скрипты, тесты, примеры, конфиги, docs.
- Составить контролируемый allowlist оставшихся совпадений.

# Non-goals
- Изменение поведения системы (кроме нейминга и ссылок).

# Acceptance Criteria
1) `rg -n "beamline\.router\.ide\.v1" -S .` возвращает контролируемый список:
   - alias-mode tests
   - deprecation note
2) Любые прочие совпадения устранены.
3) Allowlist (если нужен) зафиксирован в progress.md.

# Verification
- `rg -n "beamline\.router\.ide\.v1" -S .`
- Прогон ключевых suites (core + control) при необходимости
- Логи/артефакты зафиксированы в progress.md

# Implementation Plan
1) Repo scan по legacy subjects.
2) Замены на canonical.
3) Alias tests оставить, но явно пометить.
4) Повторный scan + фиксация.

# Risks / Notes
- Не переписать случайно строки в binary fixtures без корректировки тестов.
