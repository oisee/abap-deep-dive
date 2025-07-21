# TODO: Глава 11.1 - ABAP Daemons и Channels

## Высокий приоритет

- [ ] [TODO.1] Добавить версионность компонентов
  - Контекст: Введение реактивной архитектуры
  - Что нужно: ABAP Daemons (7.52+), AMC (7.40+), APC (7.40+)
  - Как получить: SAP Release Notes

- [ ] [TODO.2] Пример создания AMC application
  - Контекст: Строка 368-370, комментарий о SE80
  - Что нужно: Пошаговая инструкция создания AMC app
  - Как получить: SAP Help, скриншоты из SE80

- [ ] [TODO.3] JavaScript клиент для WebSocket
  - Контекст: APC WebSocket implementation
  - Что нужно: Пример HTML/JS клиента
  - Как получить: SAP Web IDE примеры

## Средний приоритет

- [ ] [TODO.4] Ограничения ABAP Daemon
  - Контекст: Daemon lifecycle
  - Что нужно: Память, CPU, время жизни лимиты
  - Как получить: SAP Notes, документация

- [ ] [TODO.5] Конфигурация SICF для APC
  - Контекст: WebSocket endpoint setup
  - Что нужно: Настройка ICF сервиса для APC
  - Как получить: Транзакция SICF

- [ ] [TODO.6] Объекты авторизации
  - Контекст: Security не описана
  - Что нужно: S_APC_APP, S_AMC_APP описание
  - Как получить: SU21, документация

## Низкий приоритет

- [ ] [TODO.7] Параметры производительности
  - Контекст: Runtime configuration
  - Что нужно: icm/HTTP/websocket_timeout и др.
  - Ожидает: System Parameters API

- [ ] [TODO.8] Мониторинг daemon процессов
  - Контекст: Health monitoring
  - Что нужно: Как мониторить через SM50/SM66
  - Как получить: Системная документация

- [ ] [TODO.9] Troubleshooting WebSocket
  - Контекст: Частые проблемы
  - Что нужно: Proxy issues, SSL, timeouts
  - Как получить: SAP Notes, форумы

- [ ] [TODO.10] Cloud readiness
  - Контекст: BTP ABAP Environment
  - Что нужно: Доступность в Steampunk
  - Как получить: BTP documentation

## Проверки для новых MCP инструментов

### Transaction Metadata API
- SMDAEMON - Daemon management
- SAMC - AMC configuration
- SAPC - APC configuration
- SMICM - ICM monitor (WebSocket)

### System Parameters API
- icm/HTTP/websocket_timeout
- rdisp/daemon_max_instances
- amc/message_size_limit

### Table Content Reader API
- Daemon registry tables
- AMC channel configuration
- APC endpoint mapping