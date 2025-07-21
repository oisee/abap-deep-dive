# TODO: Проверки, ожидающие новые MCP инструменты

## Статус
Дата создания: 2025-07-21
Статус: 🟡 Ожидание инструментов

## Необходимые инструменты
1. ✅ Существующие: CLASSSet, FUNCTIONSet, PROGRAMSet, INTERFACESet, PACKAGESet
2. ⏳ Ожидаемые:
   - Table Content Reader API
   - System Parameters API  
   - Transaction Metadata API
   - HANA System Views API
   - CDS Metadata API
   - AMDP Registry API

## Проверки по главам

### Глава 3 - Память
**Ожидает: System Parameters API**
- [ ] Параметр `rdisp/ROLL_SHM` (размер Roll area в shared memory)
- [ ] Параметр `rdisp/ROLL_MAXFS` (максимальный размер roll file)
- [ ] Параметр `rdisp/PG_SHM` (размер Paging area)
- [ ] Параметр `abap/heap_area_dia` (heap для диалоговых WP)
- [ ] Параметр `abap/heap_area_nondia` (heap для фоновых WP)
- [ ] Параметр `abap/heap_area_total` (общий heap limit)
- [ ] Параметр `ztta/roll_area` (начальный roll area на пользователя)
- [ ] Параметр `em/initial_size_MB` (размер Extended Memory)

### Глава 8 - SAP HANA
**Ожидает: HANA System Views API**
- [ ] Проверить структуру `M_CS_TABLES` (поля COMPRESSION_RATIO, MEMORY_SIZE_IN_MAIN)
- [ ] Проверить структуру `M_RS_TABLES`
- [ ] Проверить `M_HEAP_MEMORY` для мониторинга памяти
- [ ] Проверить `M_SQL_PLAN_CACHE` для планов выполнения
- [ ] Валидировать примеры запросов к system views

**Ожидает: CDS Metadata API**
- [ ] Проверить аннотации `@Analytics.dataCategory`
- [ ] Проверить `@ObjectModel` аннотации
- [ ] Валидировать синтаксис CDS views

**Ожидает: AMDP Registry API**
- [ ] Проверить интерфейс `IF_AMDP_MARKER_HDB`
- [ ] Валидировать AMDP примеры кода

### Глава 9 - Gateway и SADL
**Ожидает: Transaction Metadata API**
- [ ] Транзакция `/IWFND/MAINT_SERVICE`
- [ ] Транзакция `/IWBEP/REG_SERVICE`
- [ ] Транзакция `/IWFND/ERROR_LOG`
- [ ] Транзакция `/IWFND/APPS_LOG`
- [ ] Транзакция `/IWFND/CACHE_CLEANUP`

**Ожидает: Table Content Reader API**
- [ ] Таблица `/IWFND/C_CONFIG` (конфигурация Gateway)
- [ ] Таблица `/IWBEP/G_MODELS` (модели данных)

### Глава 10 - BOPF
**Ожидает: Table Content Reader API**
- [ ] Таблицы `/BOBF/*` для метаданных BOPF
- [ ] Проверить структуру регистрации BO

**Ожидает: Transaction Metadata API**
- [ ] Транзакция `BOBF` (BOPF Workbench)
- [ ] Транзакция `BOBX` (BOPF Test UI)

### Глава 11 - RAP
**Ожидает: CDS Metadata API**
- [ ] Проверить RAP-специфичные аннотации
- [ ] Behavior Definition синтаксис
- [ ] Service Definition/Binding

### Глава 12 - Анализ производительности
**Ожидает: Transaction Metadata API**
- [ ] ST05 - Performance Trace
- [ ] ST12 - Single Transaction Analysis  
- [ ] SAT - Runtime Analysis (новая)
- [ ] SE30 - Runtime Analysis (старая)
- [ ] SQLM - SQL Monitor
- [ ] SWLT - SQL Performance Tuning Worklist
- [ ] ST02 - Tune Summary
- [ ] ST04 - Database Performance Monitor
- [ ] SM50/SM51/SM66 - Work Process Overview
- [ ] STAD - Business Transaction Analysis
- [ ] ST03/ST03N - Workload Monitor

**Ожидает: System Parameters API**
- [ ] Параметры трассировки и профилирования
- [ ] Лимиты производительности

### Общие проверки (все главы)
**Ожидает: Transaction Metadata API**
- [ ] SE80 - Object Navigator
- [ ] SE11 - ABAP Dictionary
- [ ] SE38 - ABAP Editor
- [ ] SE24 - Class Builder
- [ ] SE37 - Function Builder
- [ ] SE93 - Transaction Maintenance
- [ ] SM59 - RFC Destinations
- [ ] SICF - HTTP Service Hierarchy

**Ожидает: Table Content Reader API**
- [ ] T000 - Clients
- [ ] DD03L - Table Fields
- [ ] TADIR - Directory of Repository Objects
- [ ] TSTC - Transaction Codes

## Неверифицированные объекты из существующего факт-чека

### Function Modules (ожидает альтернативные способы проверки)
- [ ] `SAPPARAM_GET_VALUE` - возможно, это kernel call
- [ ] `RSQLM_ACTIVATE_MONITORING` - активация SQL Monitor
- [ ] Прочие FM из списка

### Спорные/несуществующие объекты
- [ ] `IF_ABAP_RUNTIME_PROFILER` - проверено, не существует
- [ ] `CL_ABAP_RUNTIME_PROFILER` - нужна альтернатива
- [ ] `S_MEMORY_INSPECTOR` - должно быть `SMI`

## План действий после получения инструментов

1. **Приоритет 1**: Проверить все транзакции (Transaction Metadata API)
2. **Приоритет 2**: Валидировать системные параметры (System Parameters API)
3. **Приоритет 3**: Проверить HANA views для главы 8
4. **Приоритет 4**: Прочитать конфигурационные таблицы
5. **Приоритет 5**: Проверить CDS и AMDP метаданные

## Метрики
- Всего объектов для проверки: ~150
- Проверено с текущими инструментами: ~45 (30%)
- Ожидает новых инструментов: ~105 (70%)
- Критических проверок: ~40

## Следующие шаги
После получения новых MCP инструментов:
1. Запустить систематическую проверку по этому списку
2. Обновить factcheck файлы с точными данными
3. Создать patches для всех найденных ошибок
4. Удалить TODO_VERIFY элементы из документации