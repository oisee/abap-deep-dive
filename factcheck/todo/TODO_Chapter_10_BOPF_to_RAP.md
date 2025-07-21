# TODO: Глава 10 - От BOPF к RAP

## Высокий приоритет

- [ ] [TODO.1] Добавить информацию о версиях
  - Контекст: Введение RAP
  - Что нужно: С какой версии S/4HANA доступен RAP (1909)
  - Как получить: SAP Release Notes

- [ ] [TODO.2] Реальный пример BOPF объекта
  - Контекст: Строка 154, константы BOPF
  - Что нужно: Стандартный BOPF BO (например, /SCMG/*)
  - Как получить: Транзакция BOBF

## Средний приоритет

- [ ] [TODO.3] Пример миграции BOPF → RAP
  - Контекст: Переход между технологиями
  - Что нужно: Step-by-step миграция реального объекта
  - Как получить: SAP Community, блоги

- [ ] [TODO.4] Service Definition и Binding примеры
  - Контекст: RAP artifacts
  - Что нужно: Полные примеры с метаданными
  - Как получить: ADT, существующие RAP сервисы

- [ ] [TODO.5] Сравнение производительности
  - Контекст: BOPF vs RAP
  - Что нужно: Метрики, benchmarks
  - Как получить: SAP Performance Guide

## Низкий приоритет

- [ ] [TODO.6] Draft admin include варианты
  - Контекст: Строка 932
  - Что нужно: Различия include в разных версиях
  - Как получить: Проверка в разных системах

- [ ] [TODO.7] Troubleshooting секция
  - Контекст: Частые ошибки
  - Что нужно: Типичные проблемы при миграции
  - Как получить: SAP Notes, форумы

- [ ] [TODO.8] RAP в BTP ABAP Environment
  - Контекст: Облачные ограничения
  - Что нужно: Что недоступно в Steampunk
  - Как получить: BTP documentation

## Проверки для новых MCP инструментов

### Transaction Metadata API
- BOBF - BOPF Workbench
- BOBX - BOPF Test UI
- /BOBF/TEST_UI - Generic Test UI

### Table Content Reader API
- /BOBF/* таблицы конфигурации
- Метаданные BOPF объектов

### CDS Metadata API
- Проверка RAP-enabled CDS views
- Аннотации для managed scenarios