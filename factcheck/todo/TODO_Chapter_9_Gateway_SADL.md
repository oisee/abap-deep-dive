# TODO: Глава 9 - SADL и Gateway

## Высокий приоритет

- [ ] [TODO.1] Найти правильное имя класса для Gateway runtime
  - Контекст: Строка 142-144, `/iwfnd/cl_mgw_runtime`
  - Что нужно: Точное имя runtime класса
  - Как получить: Проверить в системе или документации SAP

- [ ] [TODO.2] Добавить реальный пример CDS с OData
  - Контекст: Строки 411-475
  - Что нужно: Работающий пример CDS view с @OData.publish
  - Как получить: Из Fiori Apps Reference Library или стандартных CDS

- [ ] [TODO.3] Проверить все транзакции Gateway
  - Контекст: Мониторинг и администрирование
  - Что нужно: Проверка существования и назначения транзакций
  - Ожидает: Transaction Metadata API

## Средний приоритет

- [ ] [TODO.4] Добавить информацию о версиях
  - Контекст: Компоненты Gateway
  - Что нужно: С какой версии NetWeaver/S/4HANA доступны компоненты
  - Как получить: SAP Help Portal, Release Notes

- [ ] [TODO.5] Реальные примеры SADL классов
  - Контекст: SADL runtime обработка
  - Что нужно: Имена конкретных SADL классов
  - Как получить: Debugger в работающем OData сервисе

- [ ] [TODO.6] Пример обработки $batch
  - Контекст: Batch processing упоминается но не показан
  - Что нужно: Код обработки batch запросов
  - Как получить: Gateway documentation

## Низкий приоритет

- [ ] [TODO.7] Параметры производительности
  - Контекст: Оптимизация Gateway
  - Что нужно: Конкретные параметры профиля
  - Ожидает: System Parameters API

- [ ] [TODO.8] Лимиты и ограничения
  - Контекст: Runtime обработка
  - Что нужно: Максимальный размер ответа, timeout значения
  - Как получить: Документация и параметры системы

- [ ] [TODO.9] Примеры error responses
  - Контекст: Обработка ошибок
  - Что нужно: Реальные примеры OData error форматов
  - Как получить: Gateway error log

## Проверки для новых MCP инструментов

### Transaction Metadata API
- /IWFND/TRACES
- /IWFND/ERROR_LOG
- /IWFND/STATS
- /IWFND/MED_ACTIVATE
- /IWFND/CACHE_CLEANUP
- /IWFND/GW_CLIENT
- /IWFND/MAINT_SERVICE
- /IWBEP/REG_SERVICE

### Table Content Reader API
- /IWFND/C_CONFIG - конфигурация Gateway
- /IWBEP/G_MODELS - модели данных

### System Parameters API
- Параметры ICM для Gateway
- Параметры кэширования
- Лимиты производительности