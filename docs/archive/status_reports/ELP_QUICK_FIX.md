# Quick Fix: ELP "Unknown application" Error

## TL;DR

Если VS Code показывает ошибку **"Unknown application" (L0003)** для тестовых файлов:

```bash
# 1. Сгенерировать правильный build_info.json
./scripts/generate_build_info.sh

# 2. Перезагрузить VS Code
# Ctrl+Shift+P → "Developer: Reload Window"
```

## Объяснение

**Проблема:** ELP использует путь `_build/test/lib/beamline_router` вместо корня проекта `.`

**Решение:** `build_info.json` должен содержать `"dir": "."` для указания на корень проекта.

## Автоматизация

После каждого `rebar3 compile` запускайте:
```bash
./scripts/generate_build_info.sh
```

Или добавьте в Makefile:
```makefile
compile:
	rebar3 compile
	./scripts/generate_build_info.sh
```

## Подробности

См. [docs/ELP_SETUP.md](docs/ELP_SETUP.md)
