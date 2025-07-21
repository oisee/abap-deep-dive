# TODO: Глава 12 - Инструменты анализа

## Высокий приоритет

- [ ] [TODO.1] Исправить название транзакции Memory Inspector
  - Контекст: Строка 425
  - Что нужно: Заменить S_MEMORY_INSPECTOR на SMI
  - Как исправить: Простая замена текста

- [ ] [TODO.2] Добавить современные инструменты анализа
  - Контекст: Отсутствуют важные инструменты
  - Что нужно: ST12, SQLM, STAD описание
  - Как получить: SAP documentation

- [ ] [TODO.3] Уточнить SE30 vs SAT
  - Контекст: Строка 229
  - Что нужно: Объяснить что обе транзакции существуют
  - Как исправить: Добавить пояснение

## Средний приоритет

- [ ] [TODO.4] Добавить версионность инструментов
  - Контекст: Введение каждого инструмента
  - Что нужно: SAT (7.02+), SMI (7.0+), etc.
  - Как получить: Release Notes

- [ ] [TODO.5] Kernel Snapshot уточнения
  - Контекст: Раздел 12.4
  - Что нужно: Уточнить что это не транзакция
  - Как исправить: Описать dpmon/sapcontrol использование

- [ ] [TODO.6] Примеры анализа
  - Контекст: Каждый инструмент
  - Что нужно: Конкретный пример с выводом
  - Как получить: Реальные кейсы

## Низкий приоритет

- [ ] [TODO.7] Параметры производительности
  - Контекст: Trace buffer settings
  - Что нужно: Размеры буферов, параметры
  - Ожидает: System Parameters API

- [ ] [TODO.8] Best practices секция
  - Контекст: Когда какой инструмент
  - Что нужно: Decision tree для выбора
  - Как создать: На основе use cases

- [ ] [TODO.9] HANA-specific расширения
  - Контекст: ST05 для HANA
  - Что нужно: Plan Visualizer интеграция
  - Как получить: HANA documentation

## Проверки для новых MCP инструментов

### Transaction Metadata API
- ST05 - SQL Trace
- SAT - Runtime Analysis (new)
- SE30 - Runtime Analysis (old)
- SMI - Memory Inspector
- ST12 - Single Transaction Analysis
- STAD - Business Transaction Analysis
- SQLM - SQL Monitor
- ST02 - Tune Summary
- ST04 - DB Performance Monitor
- SMLG - Logon Group maintenance

### System Parameters API
- rstr/filename - trace file location
- rstr/max_diskspace - max trace size
- abap/atrasizequota - trace quota
- DIR_TEMP - temporary directory

### Table Content Reader API
- Trace configuration tables
- Performance history tables