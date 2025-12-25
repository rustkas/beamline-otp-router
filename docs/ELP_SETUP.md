# Настройка ELP (Erlang Language Platform) для VS Code

## Проблема "Unknown application"

Если VS Code/ELP показывает ошибку **"Unknown application"** (код L0003) для файлов проекта, это означает, что ELP не может правильно идентифицировать Erlang приложение.

### Причина проблемы

ELP использует файл `build_info.json` для понимания структуры проекта. Проблемы возникают когда:

1. **`build_info.json` не существует** или устарел
2. **Неправильный путь к приложению** - ELP видит `_build/test/lib/beamline_router` вместо корня `.`
3. **Тестовые файлы не включены** в `src_dirs` основного приложения
4. **ELP не перезагружен** после изменений конфигурации

### Решение

#### 1. Проверьте `.elp.toml`

Файл `.elp.toml` в корне проекта должен содержать:

```toml
[build_info]
deps = ["deps/*", "_build/default/lib/*", "_build/test/lib/*"]

[[app]]
name = "beamline_router"
dir = "."  # ← Важно! Корень проекта, не _build/test/lib/...
src_dirs = ["src", "test", "test_support"]
include_dirs = ["include", "_build/default/plugins/gpb/include"]
extra_src_dirs = ["test"]

[rebar]
profile = "test"
```

#### 2. Сгенерируйте правильный `build_info.json`

После каждой компиляции проекта запустите:

```bash
./scripts/generate_build_info.sh
```

Или вручную убедитесь, что `build_info.json` содержит:

```json
{
  "apps": [
    {
      "name": "beamline_router",
      "dir": ".",  // ← Корень проекта!
      "src_dirs": ["src", "test", "test_support"],
      "extra_src_dirs": ["test", "test_support"],
      "include_dirs": [
        "include",
        "_build/default/plugins/gpb/include",
        "_build/default/lib/grpcbox/include",
        "src"
      ]
    }
  ],
  "deps": [...]
}
```

**Критически важно**: поле `"dir": "."` должно указывать на корень проекта, а НЕ на `_build/test/lib/beamline_router`.

#### 3. Перезагрузите ELP

После изменения конфигурации:

**Вариант А: Перезагрузка окна VS Code**
- `Ctrl+Shift+P` → "Developer: Reload Window"

**Вариант Б: Перезапуск ELP сервера**
```bash
# Найти процесс ELP
ps aux | grep elp

# Убить процесс
kill <PID>

# VS Code автоматически перезапустит ELP
```

**Вариант В: Полный перезапуск VS Code**
```bash
code --reuse-window /path/to/project
```

#### 4. Проверьте работу

После перезагрузки:
1. Откройте тестовый файл (например, `test/router_gateway_integration_SUITE.erl`)
2. Ошибка "Unknown application" должна исчезнуть
3. Проверьте панель "Problems" в VS Code

### Автоматизация

Добавьте генерацию `build_info.json` в процесс сборки:

**В Makefile:**
```makefile
compile:
	rebar3 compile
	./scripts/generate_build_info.sh
```

**В CI/CD:**
```yaml
- name: Build project
  run: |
    rebar3 compile
    ./scripts/generate_build_info.sh
```

### Отладка

Если проблема сохраняется:

1. **Проверьте версию ELP:**
   ```bash
   elp version
   ```

2. **Проверьте логи ELP:**
   - VS Code: `Ctrl+Shift+P` → "Developer: Show Logs" → "Extension Host"
   - Или панель "Output" → выберите "Erlang" или "ELP"

3. **Проверьте процессы:**
   ```bash
   ps aux | grep -E "(elp|erlang_ls)"
   ```

4. **Вручную проверьте build_info:**
   ```bash
   cat build_info.json | jq '.apps[] | {name, dir}'
   ```

### Полезные ссылки

- [ELP Documentation](https://whatsapp.github.io/erlang-language-platform/)
- [Error L0003: Unknown application](https://whatsapp.github.io/erlang-language-platform/docs/erlang-error-index/l/L0003)
- [ELP Configuration](https://whatsapp.github.io/erlang-language-platform/docs/get-started/configuration/)

## Workflow для корректной работы

1. `rebar3 compile` - компиляция проекта
2. `./scripts/generate_build_info.sh` - генерация build_info.json
3. Reload Window в VS Code
4. Проверка отсутствия ошибок "Unknown application"
