# Title
Release/profile split: core-only vs dev-control (control app enabled)

# Context
- Control app must be enabled via profile; core must remain independent.
- Profiles document expected startup differences for local/dev use.

# Goal (RU)
Развести релиз/профили так, чтобы core profile запускал Router без control app, а dev-control profile запускал Router + control app, и это было документировано.

# Goal (EN)
Split release/profiles so core runs without control app, and dev-control runs Router + control app, with documented startup differences.

# Scope
- Конфиги профилей (rebar3/relx).
- Документация запуска.

# Non-goals
- Изменять runtime конфиг Router кроме профилей.

# Acceptance Criteria
1) Core profile стартует без control children.
2) Dev-control profile стартует с control supervision tree.
3) Отличия профилей документированы.

# Verification
- Запуск core-only и dev-control профилей (команды + наблюдения)
- Фиксация в progress.md (логи/артефакты)

# Implementation Plan
1) Определить текущие профили и механизм выбора.
2) Добавить/обновить profile для control.
3) Проверить supervision tree.
4) Документировать различия.

# Risks / Notes
- Следить, чтобы control deps не подтягивались в core profile.
