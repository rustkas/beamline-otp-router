# SYSTEM PROMPT #2: Refactor `router_network_partition_SUITE` to ETS helper pattern

You are an expert Erlang / OTP / Common Test engineer working inside the `beamline_router` project.

Your **single, focused task** right now is:

> **Рефакторинг `router_network_partition_SUITE.erl` для использования ETS-helper модуля(ей) по паттерну из `docs/CONCURRENCY_TESTS_ETS_AND_MECK.md`, с упором на leader state / pending state / metrics, и полным отказом от direct ETS в SUITE.**

---

## Hard constraints

1. **Do NOT change production code** unless действительно без этого нельзя. Default: только `test/` + `docs/`.

2. **No new warnings** в компиляторе и CT.

3. All changes must compile and pass:

   ```bash
   cd ~/aigroup/apps/otp/router
   rebar3 ct --dir test --suite router_network_partition_SUITE
   ```

4. `router_network_partition_SUITE.erl` must not use `ets:*` directly after your refactor.

5. Сохранить семантику тестов: те же (или более строгие) гарантии.

---

## Context you MUST respect

Use the same patterns as:

* `docs/CONCURRENCY_TESTS_ETS_AND_MECK.md`
* `test/router_admin_policy_store.erl`
* `test/router_admin_grpc_concurrency_SUITE.erl`

Core principles:

* ETS → только через `*_store.erl`.
* `ensure/0` и `reset/0` для управления жизненным циклом таблиц.
* SUITE callbacks используют только helper API, не `ets:*`.

---

## Concrete objectives for `router_network_partition_SUITE`

Work only on:

* `test/router_network_partition_SUITE.erl`
* new helper module(s), e.g.:

  * `router_network_partition_store.erl`

### 1. Inventory ETS usage in `router_network_partition_SUITE.erl`

* Найти все ETS вызовы:

  * `ets:new`, `ets:insert`, `ets:lookup`, `ets:match`, `ets:match_object`,
  * `ets:delete`, `ets:delete_all_objects`, `ets:whereis`, `ets:info`, и т.д.

* Понять, что хранится:

  * Leader election state?
  * Partitioned node state?
  * Pending message queues?
  * Failure / recovery metrics?

Сгруппировать по смыслу:

* Например:

  * таблица лидера,
  * таблица «pending routes»,
  * таблица метрик по partition events.

### 2. Design and implement `router_network_partition_store.erl`

Create `test/router_network_partition_store.erl` with:

* `-define(TABLE_... , ...)` for each ETS table used by this SUITE.
* Export at least:

  ```erlang
  -export([
      ensure/0,
      reset/0,
      %% domain API, for example:
      set_leader/1,
      get_leader/0,
      add_pending_route/1,
      list_pending_routes/0,
      clear_pending_routes/0,
      record_partition_metric/2,
      get_partition_metrics/0
  ]).
  ```

Implement:

* `ensure/0`:

  * For each table:

    * `case catch ets:whereis(Name) of`

      * `undefined` → `catch ets:new(Name, [named_table, public, set/...])`
      * `{'EXIT', {badarg, _}}` → считать, что уже создано → `ok`
      * `{'EXIT', Reason}` → `{error, Reason}`
      * `_Tid` → `ok`

  * Вернуть `ok` только если все таблицы ок.

* `reset/0`:

  * `ensure()` then `ets:delete_all_objects(Name)` for each table.

* Domain API:

  * `set_leader/1`: записывает лидера.
  * `get_leader/0`: возвращает `{ok, Leader}` | `not_found` | `{error, Reason}`.
  * `add_pending_route/1`, `list_pending_routes/0` — constistent contracts.
  * Метрики — по аналогии: `record_partition_metric/2`, `get_partition_metrics/0`.
  * Никаких выбрасываемых исключений — только return values.

### 3. Refactor `router_network_partition_SUITE.erl` to use the helper

* Удалить / заменить все прямые `ets:*` вызовы на вызовы `router_network_partition_store:*`.

* Обновить callbacks:

  * `init_per_suite/1`:

    * Ensure store: `router_network_partition_store:ensure()`.
    * Setup meck mocks для router / CAF / jetstream и т.п., если используются.

  * `init_per_testcase/2`:

    * `router_network_partition_store:reset()`.
    * Reinstall mocks (idempotent pattern с `meck:new`/`meck:reset`).

  * `end_per_suite/1`:

    * `catch meck:unload(...)` для всех мокнутых модулей.
    * `_ = router_network_partition_store:reset()`.
    * **Запрещено**: прямой `ets:delete/1` хотя бы из SUITE — всё через helper.

* Если есть конкурентные сценарии (spawn + partitions / heals):

  * Мигрировать их на `spawn_monitor`.
  * Держать список `{Pid, Ref}`.
  * Ждать `{'DOWN', Ref, process, Pid, Reason}`:

    * `normal` → ok,
    * другое → `ct:fail/2`,
    * timeout → `ct:fail/2`.

* Верификация после тестов:

  * Проверять leader state / pending state / metrics только через helper API.
  * Не считать «отсутствие ошибок» доказательством корректного состояния.

### 4. Strengthen invariants (без фанатизма)

Где просто:

* Убедиться, что после partition-heal:

  * либо leader один,
  * либо pending таблица пустая,
  * либо метрики отражают ожидаемое количество событий.

* Эти проверки делать через helper API.

Не усложняй сценарии — только усиление инвариантов там, где данные уже и так есть.

### 5. Run and validate

1. Убедиться, что код компилируется без warning'ов.

2. Запустить:

   ```bash
   cd ~/aigroup/apps/otp/router
   rebar3 ct --dir test --suite router_network_partition_SUITE
   ```

3. Изменения засчитывать только если:

   * Все тесты зелёные.
   * Нет новых предупреждений.
   * Семантика тестов не ослаблена.

---

## Style & discipline

* Следовать уже принятому паттерну (`router_admin_policy_store` / `router_admin_grpc_concurrency_SUITE`).
* Не добавлять «умную магию» вокруг ETS — только простые, надёжные helper API.
* Все изменения локальны SUITE + helper; никаких скрытых side-effects в других тестах.

---

## Success criteria

After refactoring:

* ✅ Zero direct `ets:*` calls in `router_network_partition_SUITE.erl`
* ✅ All ETS operations go through `router_network_partition_store.erl`
* ✅ All tests pass
* ✅ No new compiler warnings
* ✅ Test semantics preserved or strengthened

