# Глава 11.1: ABAP Daemons и Channels - реактивная архитектура

### Требования к версии
- SAP NetWeaver 7.40 SP08 - AMC (ABAP Messaging Channels)
- SAP NetWeaver 7.40 SP05 - APC (ABAP Push Channels)
- SAP NetWeaver 7.52 - ABAP Daemon Framework
- SAP NetWeaver 7.54 - расширенные возможности daemons

## 11.1.1. ABAP Daemon Framework

ABAP Daemon Framework (ADF) представляет собой революционное расширение классической модели выполнения ABAP, позволяющее создавать долгоживущие фоновые процессы, которые могут реагировать на события в реальном времени.

### Архитектура ABAP Daemons

```mermaid
graph TB
    subgraph "ABAP Daemon Architecture"
        subgraph "Daemon Lifecycle"
            CREATE[Daemon Creation]
            START[Start Event]
            RUNNING[Running State<br/>Event Loop]
            STOP[Stop Event]
            DESTROY[Destruction]
        end
        
        subgraph "Daemon Manager"
            DM[Daemon Manager<br/>SMDAEMON]
            REGISTRY[Daemon Registry]
            MONITOR[Health Monitor]
            RESTART[Auto-restart Logic]
        end
        
        subgraph "Work Process"
            WP_DAEMON[Dedicated WP<br/>or Shared WP]
            CONTEXT[Daemon Context]
            EVENT_QUEUE[Event Queue]
            TIMER[Timer Service]
        end
        
        subgraph "Communication"
            AMC[AMC Channels]
            APC[APC WebSocket]
            EVENTS[System Events]
            API[Daemon API]
        end
        
        CREATE --> START
        START --> RUNNING
        RUNNING --> STOP
        STOP --> DESTROY
        
        DM --> REGISTRY
        DM --> MONITOR
        MONITOR --> RESTART
        
        RUNNING --> EVENT_QUEUE
        EVENT_QUEUE --> AMC
        EVENT_QUEUE --> EVENTS
        
        WP_DAEMON --> CONTEXT
        CONTEXT --> RUNNING
    end
    
    style DM fill:#ff9999,stroke:#333,stroke-width:4px
    style RUNNING fill:#99ff99,stroke:#333,stroke-width:2px
    style AMC fill:#99ccff,stroke:#333,stroke-width:2px
```

### Daemon Implementation Example

```abap
CLASS zcl_demo_daemon DEFINITION
  PUBLIC
  INHERITING FROM cl_abap_daemon_ext_base
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    " Daemon name
    CLASS-DATA: gv_daemon_name TYPE string VALUE 'DEMO_DAEMON'.
    
    " Events
    EVENTS: data_received TYPE abap_daemon_event_type.
    
  PROTECTED SECTION.
    " Daemon lifecycle methods
    METHODS: on_event REDEFINITION,
             on_start REDEFINITION,
             on_stop REDEFINITION,
             on_restart REDEFINITION,
             on_error REDEFINITION.
             
  PRIVATE SECTION.
    DATA: mv_counter TYPE i,
          mo_timer TYPE REF TO if_abap_timer,
          mt_buffer TYPE TABLE OF string.
    
    METHODS: process_timer_event,
             process_message
               IMPORTING iv_message TYPE string,
             setup_timer.
ENDCLASS.

CLASS zcl_demo_daemon IMPLEMENTATION.
  METHOD on_start.
    " Вызывается при старте демона
    TRY.
        " Инициализация
        CLEAR: mv_counter, mt_buffer.
        
        " Подписка на AMC канал
        cl_amc_channel_manager=>create_message_consumer(
          i_application_id = 'ZAMC_DEMO'
          i_channel_id     = '/demo/events'
        )->start_message_delivery( i_receiver = me ).
        
        " Настройка таймера (каждые 30 секунд)
        setup_timer( ).
        
        " Логирование
        MESSAGE |Daemon { gv_daemon_name } started| TYPE 'I'.
        
      CATCH cx_amc_error INTO DATA(lx_amc).
        RAISE EXCEPTION TYPE cx_abap_daemon_error
          EXPORTING previous = lx_amc.
    ENDTRY.
  ENDMETHOD.
  
  METHOD on_event.
    " Обработка входящих событий
    TRY.
        CASE i_event_type.
          WHEN if_abap_daemon_extension=>co_event_type_timer.
            process_timer_event( ).
            
          WHEN if_abap_daemon_extension=>co_event_type_message.
            " AMC message received
            DATA(lo_message) = CAST if_amc_message_text(
              i_message_manager->get_message( ) ).
            process_message( lo_message->get_text( ) ).
            
          WHEN if_abap_daemon_extension=>co_event_type_system.
            " System event (shutdown, etc.)
            MESSAGE |System event received| TYPE 'I'.
            
        ENDCASE.
        
      CATCH cx_root INTO DATA(lx_error).
        " Обработка ошибок
        on_error( lx_error ).
    ENDTRY.
  ENDMETHOD.
  
  METHOD process_timer_event.
    " Периодическая обработка
    ADD 1 TO mv_counter.
    
    " Проверка буфера
    IF lines( mt_buffer ) > 0.
      " Batch processing
      DATA(lv_count) = lines( mt_buffer ).
      
      " Отправка accumulated данных
      TRY.
          cl_amc_channel_manager=>create_message_producer(
            i_application_id = 'ZAMC_DEMO'
            i_channel_id     = '/demo/results'
          )->send_message( |Processed { lv_count } messages| ).
          
          CLEAR mt_buffer.
          
        CATCH cx_amc_error.
          " Handle error
      ENDTRY.
    ENDIF.
    
    " Проверка здоровья
    IF mv_counter MOD 10 = 0.
      MESSAGE |Daemon health check: OK, counter = { mv_counter }| TYPE 'I'.
    ENDIF.
  ENDMETHOD.
  
  METHOD process_message.
    " Обработка входящего сообщения
    APPEND iv_message TO mt_buffer.
    
    " Немедленная обработка при достижении порога
    IF lines( mt_buffer ) >= 100.
      process_timer_event( ).
    ENDIF.
  ENDMETHOD.
  
  METHOD setup_timer.
    " Настройка периодического таймера
    mo_timer = cl_abap_timer_manager=>get_timer_manager( 
      )->start_timer(
        i_timeout = 30  " секунды
        i_timer_handler = me
    ).
  ENDMETHOD.
  
  METHOD on_stop.
    " Graceful shutdown
    IF mo_timer IS BOUND.
      mo_timer->stop( ).
    ENDIF.
    
    " Сохранение состояния если необходимо
    IF lines( mt_buffer ) > 0.
      " Persist buffered data
      process_timer_event( ).
    ENDIF.
    
    MESSAGE |Daemon { gv_daemon_name } stopped| TYPE 'I'.
  ENDMETHOD.
  
  METHOD on_error.
    " Централизованная обработка ошибок
    DATA(lv_error_text) = ix_error->get_text( ).
    MESSAGE lv_error_text TYPE 'E'.
    
    " Решение о продолжении работы
    IF ix_error IS INSTANCE OF cx_fatal_error.
      " Критическая ошибка - остановка демона
      RAISE EXCEPTION TYPE cx_abap_daemon_error
        EXPORTING previous = ix_error.
    ENDIF.
    " Иначе продолжаем работу
  ENDMETHOD.
ENDCLASS.
```

### Управление демонами

```abap
CLASS zcl_daemon_manager DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CLASS-METHODS:
      start_daemon
        IMPORTING
          iv_daemon_class TYPE seoclsname
          iv_name         TYPE string OPTIONAL
        RETURNING
          VALUE(ro_handle) TYPE REF TO if_abap_daemon_handle
        RAISING
          cx_abap_daemon_error,
          
      stop_daemon
        IMPORTING
          iv_name TYPE string
        RAISING
          cx_abap_daemon_error,
          
      get_daemon_info
        IMPORTING
          iv_name TYPE string
        RETURNING
          VALUE(rs_info) TYPE abap_daemon_info
        RAISING
          cx_abap_daemon_error,
          
      list_active_daemons
        RETURNING
          VALUE(rt_daemons) TYPE abap_daemon_info_table.
          
ENDCLASS.

CLASS zcl_daemon_manager IMPLEMENTATION.
  METHOD start_daemon.
    " Запуск демона с проверками
    DATA: lo_factory TYPE REF TO if_abap_daemon_factory.
    
    " Получение фабрики
    lo_factory = cl_abap_daemon_factory=>get_factory( ).
    
    " Проверка, не запущен ли уже
    TRY.
        DATA(ls_info) = get_daemon_info( iv_name ).
        IF ls_info-state = if_abap_daemon_factory=>co_state_running.
          RAISE EXCEPTION TYPE cx_abap_daemon_error
            EXPORTING textid = cx_abap_daemon_error=>daemon_already_running.
        ENDIF.
      CATCH cx_abap_daemon_error.
        " Daemon not found - OK to start
    ENDTRY.
    
    " Настройка параметров запуска
    DATA(lo_config) = cl_abap_daemon_config=>create( ).
    lo_config->set_restart_mode( if_abap_daemon_config=>co_restart_on_error ).
    lo_config->set_max_restart_count( 3 ).
    lo_config->set_restart_delay( 60 ). " секунды
    
    " Запуск демона
    ro_handle = lo_factory->start_daemon(
      i_class_name = iv_daemon_class
      i_name       = iv_name
      i_config     = lo_config
      i_parameter  = VALUE #( )  " Параметры запуска
    ).
    
    " Ожидание старта
    WAIT UP TO 2 SECONDS.
    
    " Проверка статуса
    ls_info = get_daemon_info( iv_name ).
    IF ls_info-state <> if_abap_daemon_factory=>co_state_running.
      RAISE EXCEPTION TYPE cx_abap_daemon_error
        EXPORTING textid = cx_abap_daemon_error=>daemon_start_failed.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
```

## 11.1.2. AMC: Publish/Subscribe в ABAP

ABAP Messaging Channels (AMC) обеспечивают асинхронную коммуникацию между различными сессиями и процессами в SAP системе.

### Архитектура AMC

```mermaid
graph TB
    subgraph "AMC Architecture"
        subgraph "AMC Components"
            BROKER[Message Broker<br/>Shared Memory]
            CHANNELS[Channel Registry]
            SECURITY[Authorization]
            PERSIST[Persistence<br/>Optional]
        end
        
        subgraph "Publishers"
            PUB1[ABAP Program]
            PUB2[ABAP Daemon]
            PUB3[Web Service]
            PUB4[Batch Job]
        end
        
        subgraph "Subscribers"
            SUB1[ABAP Session]
            SUB2[ABAP Daemon]
            SUB3[APC WebSocket]
            SUB4[Event Handler]
        end
        
        subgraph "Message Flow"
            QUEUE[Message Queue<br/>FIFO/Priority]
            FILTER[Message Filters]
            ROUTING[Routing Rules]
        end
        
        PUB1 --> BROKER
        PUB2 --> BROKER
        PUB3 --> BROKER
        
        BROKER --> QUEUE
        QUEUE --> FILTER
        FILTER --> ROUTING
        
        ROUTING --> SUB1
        ROUTING --> SUB2
        ROUTING --> SUB3
    end
    
    style BROKER fill:#ff9999,stroke:#333,stroke-width:4px
    style QUEUE fill:#99ccff,stroke:#333,stroke-width:2px
```

### AMC Implementation

```abap
" AMC Application definition (SE80 -> Create -> ABAP Messaging Channel Application)
" Application ID: ZAMC_DEMO
" Channels: /demo/events, /demo/commands, /demo/results

CLASS zcl_amc_demo DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES: if_amc_message_receiver,
                if_amc_message_type_text.
    
    TYPES: BEGIN OF ty_event,
             event_type TYPE string,
             timestamp  TYPE timestampl,
             user       TYPE sy-uname,
             data       TYPE string,
           END OF ty_event.
    
    CLASS-METHODS:
      publish_event
        IMPORTING
          is_event TYPE ty_event
        RAISING
          cx_amc_error,
          
      subscribe_to_channel
        IMPORTING
          iv_channel_id TYPE string
          io_receiver   TYPE REF TO if_amc_message_receiver
        RAISING
          cx_amc_error.
          
  PRIVATE SECTION.
    DATA: mv_channel_id TYPE string.
    
ENDCLASS.

CLASS zcl_amc_demo IMPLEMENTATION.
  METHOD publish_event.
    " Публикация события в AMC канал
    TRY.
        " Создание producer
        DATA(lo_producer) = cl_amc_channel_manager=>create_message_producer(
          i_application_id = 'ZAMC_DEMO'
          i_channel_id     = '/demo/events'
        ).
        
        " Сериализация события
        DATA(lv_json) = /ui2/cl_json=>serialize(
          data = is_event
          pretty_name = /ui2/cl_json=>pretty_mode-low_case
        ).
        
        " Отправка сообщения
        lo_producer->send_message( lv_json ).
        
      CATCH cx_amc_error INTO DATA(lx_error).
        " Логирование ошибки
        MESSAGE lx_error->get_text( ) TYPE 'E'.
        RAISE EXCEPTION lx_error.
    ENDTRY.
  ENDMETHOD.
  
  METHOD subscribe_to_channel.
    " Подписка на AMC канал
    TRY.
        " Создание consumer
        DATA(lo_consumer) = cl_amc_channel_manager=>create_message_consumer(
          i_application_id = 'ZAMC_DEMO'
          i_channel_id     = iv_channel_id
        ).
        
        " Начало доставки сообщений
        lo_consumer->start_message_delivery( i_receiver = io_receiver ).
        
      CATCH cx_amc_error INTO DATA(lx_error).
        MESSAGE lx_error->get_text( ) TYPE 'E'.
        RAISE EXCEPTION lx_error.
    ENDTRY.
  ENDMETHOD.
  
  METHOD if_amc_message_receiver~receive.
    " Обработка полученного сообщения
    TRY.
        " Получение текстового сообщения
        DATA(lo_text_message) = CAST if_amc_message_text( i_message ).
        DATA(lv_json) = lo_text_message->get_text( ).
        
        " Десериализация
        DATA: ls_event TYPE ty_event.
        /ui2/cl_json=>deserialize(
          EXPORTING json = lv_json
          CHANGING  data = ls_event
        ).
        
        " Обработка события
        CASE ls_event-event_type.
          WHEN 'ORDER_CREATED'.
            " Process new order
            MESSAGE |New order created by { ls_event-user }| TYPE 'I'.
            
          WHEN 'STOCK_LOW'.
            " Alert handling
            MESSAGE |Low stock alert: { ls_event-data }| TYPE 'W'.
            
          WHEN OTHERS.
            " Unknown event
        ENDCASE.
        
      CATCH cx_root INTO DATA(lx_error).
        MESSAGE lx_error->get_text( ) TYPE 'E'.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
```

## 11.1.3. APC: WebSocket сервер в ABAP

ABAP Push Channels (APC) позволяют создавать WebSocket серверы непосредственно в ABAP, обеспечивая двунаправленную коммуникацию в реальном времени.

### Архитектура APC

```mermaid
graph TB
    subgraph "APC WebSocket Architecture"
        subgraph "Client Side"
            BROWSER[Web Browser]
            JS[JavaScript<br/>WebSocket API]
            MOBILE[Mobile App]
        end
        
        subgraph "SAP Server"
            subgraph "ICM"
                WS_HANDLER[WebSocket Handler]
                UPGRADE[HTTP Upgrade]
                WS_PROTOCOL[WS Protocol]
            end
            
            subgraph "APC Framework"
                APC_HANDLER[APC Handler Class]
                SESSION_MGR[Session Manager]
                MSG_QUEUE[Message Queue]
            end
            
            subgraph "Integration"
                AMC_INT[AMC Integration]
                ABAP_LOGIC[Business Logic]
                DB_ACCESS[Database]
            end
        end
        
        subgraph "Communication"
            BIDIRECT[Bidirectional<br/>Messages]
            HEARTBEAT[Heartbeat]
            BINARY[Binary/Text]
        end
        
        BROWSER --> JS
        JS --> WS_HANDLER
        
        WS_HANDLER --> UPGRADE
        UPGRADE --> WS_PROTOCOL
        WS_PROTOCOL --> APC_HANDLER
        
        APC_HANDLER --> SESSION_MGR
        APC_HANDLER --> AMC_INT
        APC_HANDLER --> ABAP_LOGIC
        
        APC_HANDLER <--> BIDIRECT
        BIDIRECT <--> JS
    end
    
    style APC_HANDLER fill:#ff9999,stroke:#333,stroke-width:4px
    style BIDIRECT fill:#99ff99,stroke:#333,stroke-width:2px
```

### APC Implementation

```abap
CLASS zcl_apc_websocket_handler DEFINITION
  PUBLIC
  INHERITING FROM cl_apc_wsp_ext_stateless_base
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES: if_apc_wsp_event_handler,
                if_apc_wsp_message_handler,
                if_amc_message_receiver.
    
  PRIVATE SECTION.
    TYPES: BEGIN OF ty_client,
             client_id    TYPE sysuuid_c32,
             session      TYPE REF TO if_apc_wsp_server_context,
             user         TYPE sy-uname,
             connected_at TYPE timestampl,
           END OF ty_client.
    
    CLASS-DATA: gt_clients TYPE HASHED TABLE OF ty_client
                           WITH UNIQUE KEY client_id.
    
    METHODS: handle_client_message
               IMPORTING
                 iv_message TYPE string
                 io_session TYPE REF TO if_apc_wsp_server_context,
                 
             broadcast_message
               IMPORTING
                 iv_message TYPE string
               RAISING
                 cx_apc_error,
                 
             register_client
               IMPORTING
                 io_session TYPE REF TO if_apc_wsp_server_context
               RETURNING
                 VALUE(rv_client_id) TYPE sysuuid_c32.
ENDCLASS.

CLASS zcl_apc_websocket_handler IMPLEMENTATION.
  METHOD if_apc_wsp_event_handler~on_open.
    " Новое WebSocket соединение
    TRY.
        " Регистрация клиента
        DATA(lv_client_id) = register_client( i_context ).
        
        " Подписка на AMC для этого клиента
        cl_amc_channel_manager=>create_message_consumer(
          i_application_id = 'ZAMC_DEMO'
          i_channel_id     = '/demo/broadcast'
        )->start_message_delivery( i_receiver = me ).
        
        " Отправка приветствия
        DATA(lo_message) = i_message_factory->create_message( ).
        lo_message->set_text(
          |Welcome! Your client ID is { lv_client_id }|
        ).
        i_context->send_message( lo_message ).
        
      CATCH cx_apc_error cx_amc_error INTO DATA(lx_error).
        MESSAGE lx_error->get_text( ) TYPE 'E'.
    ENDTRY.
  ENDMETHOD.
  
  METHOD if_apc_wsp_event_handler~on_close.
    " Закрытие соединения
    " Удаление клиента из таблицы
    LOOP AT gt_clients INTO DATA(ls_client).
      IF ls_client-session = i_context.
        DELETE gt_clients WHERE client_id = ls_client-client_id.
        EXIT.
      ENDIF.
    ENDLOOP.
    
    " Уведомление других клиентов
    TRY.
        broadcast_message( |Client disconnected| ).
      CATCH cx_apc_error.
        " Ignore errors during cleanup
    ENDTRY.
  ENDMETHOD.
  
  METHOD if_apc_wsp_message_handler~on_message.
    " Обработка входящего сообщения от клиента
    TRY.
        DATA(lv_text) = i_message->get_text( ).
        
        " Обработка команд
        handle_client_message(
          iv_message = lv_text
          io_session = i_context
        ).
        
      CATCH cx_apc_error INTO DATA(lx_error).
        " Отправка ошибки клиенту
        DATA(lo_error_msg) = i_message_factory->create_message( ).
        lo_error_msg->set_text( |Error: { lx_error->get_text( ) }| ).
        i_context->send_message( lo_error_msg ).
    ENDTRY.
  ENDMETHOD.
  
  METHOD handle_client_message.
    " Парсинг и обработка команд от клиента
    DATA: BEGIN OF ls_command,
            action TYPE string,
            data   TYPE string,
          END OF ls_command.
    
    TRY.
        " Десериализация JSON команды
        /ui2/cl_json=>deserialize(
          EXPORTING json = iv_message
          CHANGING  data = ls_command
        ).
        
        CASE ls_command-action.
          WHEN 'SUBSCRIBE'.
            " Подписка на определенный топик
            MESSAGE |Client subscribed to { ls_command-data }| TYPE 'I'.
            
          WHEN 'PUBLISH'.
            " Публикация сообщения всем клиентам
            broadcast_message( ls_command-data ).
            
          WHEN 'PING'.
            " Heartbeat response
            DATA(lo_pong) = cl_apc_wsp_message_factory=>create_message( ).
            lo_pong->set_text( 'PONG' ).
            io_session->send_message( lo_pong ).
            
        ENDCASE.
        
      CATCH cx_root INTO DATA(lx_error).
        MESSAGE lx_error->get_text( ) TYPE 'E'.
    ENDTRY.
  ENDMETHOD.
  
  METHOD broadcast_message.
    " Отправка сообщения всем подключенным клиентам
    DATA(lo_message) = cl_apc_wsp_message_factory=>create_message( ).
    lo_message->set_text( iv_message ).
    
    LOOP AT gt_clients INTO DATA(ls_client).
      TRY.
          ls_client-session->send_message( lo_message ).
        CATCH cx_apc_error.
          " Client disconnected - remove
          DELETE gt_clients WHERE client_id = ls_client-client_id.
      ENDTRY.
    ENDLOOP.
  ENDMETHOD.
  
  METHOD if_amc_message_receiver~receive.
    " Получение сообщения из AMC и broadcast через WebSocket
    TRY.
        DATA(lo_text_message) = CAST if_amc_message_text( i_message ).
        broadcast_message( lo_text_message->get_text( ) ).
        
      CATCH cx_root INTO DATA(lx_error).
        MESSAGE lx_error->get_text( ) TYPE 'E'.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
```

## 11.1.4. Collaboration scenario

### Интеграция Daemons, AMC и APC

```mermaid
sequenceDiagram
    participant USER as User Browser
    participant APC as APC WebSocket
    participant DAEMON as ABAP Daemon
    participant AMC as AMC Channel
    participant DB as Database
    participant EXT as External System
    
    Note over USER,EXT: Real-time Collaboration Flow
    
    USER->>APC: Connect WebSocket
    APC->>APC: Register Client
    APC->>AMC: Subscribe to Updates
    
    USER->>APC: Send Command
    APC->>AMC: Publish to Channel
    
    AMC->>DAEMON: Message Event
    DAEMON->>DAEMON: Process Command
    DAEMON->>DB: Update Data
    
    DAEMON->>AMC: Publish Result
    
    par Broadcast to All Clients
        AMC->>APC: Notify Result
        APC->>USER: Push Update
    and External Integration
        AMC->>EXT: Send Event
        EXT->>AMC: Acknowledge
    end
    
    loop Daemon Background Processing
        DAEMON->>DAEMON: Timer Event
        DAEMON->>DB: Check Changes
        DAEMON->>AMC: Publish Changes
        AMC->>APC: Broadcast
        APC->>USER: Real-time Update
    end
```

### Complete Example: Real-time Dashboard

```abap
" Daemon для мониторинга системы
CLASS zcl_dashboard_daemon DEFINITION
  PUBLIC
  INHERITING FROM cl_abap_daemon_ext_base
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES: BEGIN OF ty_metrics,
             timestamp    TYPE timestampl,
             cpu_usage    TYPE decfloat16,
             memory_usage TYPE decfloat16,
             active_users TYPE i,
             response_time TYPE decfloat16,
           END OF ty_metrics.
           
  PROTECTED SECTION.
    METHODS: on_event REDEFINITION,
             on_start REDEFINITION.
             
  PRIVATE SECTION.
    METHODS: collect_metrics
               RETURNING VALUE(rs_metrics) TYPE ty_metrics,
             publish_metrics
               IMPORTING is_metrics TYPE ty_metrics.
ENDCLASS.

CLASS zcl_dashboard_daemon IMPLEMENTATION.
  METHOD on_start.
    " Запуск сбора метрик каждые 5 секунд
    cl_abap_timer_manager=>get_timer_manager( )->start_timer(
      i_timeout = 5
      i_timer_handler = me
    ).
  ENDMETHOD.
  
  METHOD on_event.
    IF i_event_type = if_abap_daemon_extension=>co_event_type_timer.
      " Сбор и публикация метрик
      DATA(ls_metrics) = collect_metrics( ).
      publish_metrics( ls_metrics ).
    ENDIF.
  ENDMETHOD.
  
  METHOD collect_metrics.
    GET TIME STAMP FIELD rs_metrics-timestamp.
    
    " Симуляция сбора метрик (в реальности - из ST06, SM50, etc.)
    rs_metrics-cpu_usage = cl_abap_random_decfloat16=>create(
      min = '20.0'
      max = '80.0'
    )->get_next( ).
    
    rs_metrics-memory_usage = cl_abap_random_decfloat16=>create(
      min = '40.0'
      max = '90.0'
    )->get_next( ).
    
    " Активные пользователи из SM04
    SELECT COUNT(*) FROM usr41
      WHERE termid NE space
      INTO @rs_metrics-active_users.
    
    rs_metrics-response_time = cl_abap_random_decfloat16=>create(
      min = '50.0'
      max = '500.0'
    )->get_next( ).
  ENDMETHOD.
  
  METHOD publish_metrics.
    TRY.
        " Публикация в AMC
        DATA(lv_json) = /ui2/cl_json=>serialize( data = is_metrics ).
        
        cl_amc_channel_manager=>create_message_producer(
          i_application_id = 'ZAMC_DASHBOARD'
          i_channel_id     = '/metrics/realtime'
        )->send_message( lv_json ).
        
      CATCH cx_amc_error INTO DATA(lx_error).
        MESSAGE lx_error->get_text( ) TYPE 'E'.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
```

### Архитектурные паттерны

```mermaid
graph TB
    subgraph "Reactive Architecture Patterns"
        subgraph "Event-Driven"
            EVENT_SRC[Event Sources<br/>DB Triggers<br/>User Actions<br/>System Events]
            EVENT_BUS[AMC Event Bus]
            EVENT_PROC[Event Processors<br/>Daemons]
        end
        
        subgraph "Push Notifications"
            CHANGE[State Change]
            NOTIFY[AMC Notification]
            PUSH[APC Push]
            CLIENT[Client Update]
        end
        
        subgraph "Background Processing"
            QUEUE[Task Queue]
            DAEMON_POOL[Daemon Pool]
            ASYNC[Async Execution]
            CALLBACK[Result Callback]
        end
        
        subgraph "Real-time Analytics"
            STREAM[Data Stream]
            WINDOW[Time Window]
            AGGREGATE[Aggregation]
            DASHBOARD[Live Dashboard]
        end
        
        EVENT_SRC --> EVENT_BUS
        EVENT_BUS --> EVENT_PROC
        
        CHANGE --> NOTIFY
        NOTIFY --> PUSH
        PUSH --> CLIENT
        
        QUEUE --> DAEMON_POOL
        DAEMON_POOL --> ASYNC
        ASYNC --> CALLBACK
        
        STREAM --> WINDOW
        WINDOW --> AGGREGATE
        AGGREGATE --> DASHBOARD
    end
    
    style EVENT_BUS fill:#ff9999,stroke:#333,stroke-width:2px
    style DAEMON_POOL fill:#99ff99,stroke:#333,stroke-width:2px
    style PUSH fill:#99ccff,stroke:#333,stroke-width:2px
```

## 11.1.5. Интеграция с фреймворками фоновой обработки

### Выбор между ABAP Daemons и bgPF/bgRFC

При проектировании системы важно понимать, когда использовать ABAP Daemons, а когда традиционные фреймворки фоновой обработки:

```mermaid
graph TB
    subgraph "Decision Matrix: Processing Framework Selection"
        subgraph "ABAP Daemons - Use Cases"
            REALTIME[Real-time Processing<br/>< 1 second response]
            STATEFUL[Stateful Operations<br/>Session context]
            CONTINUOUS[Continuous Monitoring<br/>24/7 operations]
            INTERACTIVE[Interactive Response<br/>WebSocket/AMC]
        end
        
        subgraph "bgPF/bgRFC - Use Cases"
            BATCH[Batch Processing<br/>Large data volumes]
            SCHEDULED[Scheduled Jobs<br/>Time-based execution]
            RESILIENT[High Resilience<br/>Restart capability]
            SCALABLE[Horizontal Scaling<br/>Multiple servers]
        end
        
        subgraph "Hybrid Scenarios"
            TRIGGER[Event Triggering]
            COORDINATION[Process Coordination]
            HANDOFF[Task Handoff]
        end
        
        REALTIME --> TRIGGER
        BATCH --> TRIGGER
        TRIGGER --> COORDINATION
        COORDINATION --> HANDOFF
    end
    
    style REALTIME fill:#99ff99,stroke:#333,stroke-width:2px
    style BATCH fill:#99ccff,stroke:#333,stroke-width:2px
    style TRIGGER fill:#ffcc99,stroke:#333,stroke-width:2px
```

### Event-driven Background Processing

AMC может эффективно запускать фоновую обработку по событиям:

```abap
CLASS zcl_event_driven_processor DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES: if_amc_message_receiver.
    
    TYPES: BEGIN OF ty_process_request,
             process_type TYPE string,
             priority     TYPE i,
             data_id      TYPE string,
             parameters   TYPE string_table,
           END OF ty_process_request.
           
    CLASS-METHODS:
      start_listener
        RAISING cx_amc_error,
        
      trigger_background_processing
        IMPORTING
          is_request TYPE ty_process_request
        RAISING
          cx_bgpf_error
          cx_rfc_error.
          
  PRIVATE SECTION.
    METHODS:
      process_heavy_task
        IMPORTING
          is_request TYPE ty_process_request
        RAISING
          cx_bgpf_error,
          
      process_distributed_task
        IMPORTING
          is_request TYPE ty_process_request
        RAISING
          cx_rfc_error,
          
      schedule_batch_job
        IMPORTING
          is_request TYPE ty_process_request
        RAISING
          cx_bgpf_error.
ENDCLASS.

CLASS zcl_event_driven_processor IMPLEMENTATION.
  METHOD start_listener.
    " Подписка на команды для фоновой обработки
    cl_amc_channel_manager=>create_message_consumer(
      i_application_id = 'ZAMC_BACKGROUND'
      i_channel_id     = '/background/commands'
    )->start_message_delivery( i_receiver = NEW zcl_event_driven_processor( ) ).
  ENDMETHOD.
  
  METHOD if_amc_message_receiver~receive.
    " Обработка команд на фоновую обработку
    TRY.
        DATA(lo_text_message) = CAST if_amc_message_text( i_message ).
        DATA(lv_json) = lo_text_message->get_text( ).
        
        " Десериализация запроса
        DATA: ls_request TYPE ty_process_request.
        /ui2/cl_json=>deserialize(
          EXPORTING json = lv_json
          CHANGING  data = ls_request
        ).
        
        " Маршрутизация на соответствующий фреймворк
        CASE ls_request-process_type.
          WHEN 'HEAVY_COMPUTATION'.
            " Тяжелые вычисления - bgPF
            process_heavy_task( ls_request ).
            
          WHEN 'DISTRIBUTED'.
            " Распределенная обработка - bgRFC
            process_distributed_task( ls_request ).
            
          WHEN 'SCHEDULED'.
            " Отложенное выполнение - Batch Job
            schedule_batch_job( ls_request ).
            
        ENDCASE.
        
      CATCH cx_root INTO DATA(lx_error).
        " Уведомление об ошибке через AMC
        cl_amc_channel_manager=>create_message_producer(
          i_application_id = 'ZAMC_BACKGROUND'
          i_channel_id     = '/background/errors'
        )->send_message( |Error processing request: { lx_error->get_text( ) }| ).
    ENDTRY.
  ENDMETHOD.
  
  METHOD process_heavy_task.
    " Запуск через bgPF для тяжелых вычислений
    DATA: lo_unit TYPE REF TO if_bgpf_unit.
    
    " Создание bgPF unit
    lo_unit = cl_bgpf_unit_factory=>get_instance( )->create_unit(
      i_name = |HEAVY_TASK_{ is_request-data_id }|
    ).
    
    " Добавление задач
    lo_unit->add_task(
      i_task_name     = 'PROCESS_DATA'
      i_task_class    = 'ZCL_HEAVY_PROCESSOR'
      i_task_method   = 'EXECUTE'
      i_task_data     = is_request-data_id
      i_priority      = is_request-priority
    ).
    
    " Запуск с callback для уведомления через AMC
    lo_unit->set_callback(
      i_callback_class  = 'ZCL_BGPF_AMC_CALLBACK'
      i_callback_method = 'ON_COMPLETE'
    ).
    
    lo_unit->start( ).
    
    " Немедленное уведомление о запуске
    cl_amc_channel_manager=>create_message_producer(
      i_application_id = 'ZAMC_BACKGROUND'
      i_channel_id     = '/background/status'
    )->send_message( |Heavy task started: { is_request-data_id }| ).
  ENDMETHOD.
  
  METHOD process_distributed_task.
    " Запуск через bgRFC для распределенной обработки
    DATA: lo_destination TYPE REF TO if_bgrfc_destination_outbound.
    
    " Выбор пула серверов для обработки
    lo_destination = cl_bgrfc_destination_outbound=>create(
      i_destination = 'BGPF_SERVER_POOL'
    ).
    
    " Создание bgRFC unit of work
    DATA(lo_unit) = lo_destination->create_unit( ).
    
    " Добавление вызовов
    lo_unit->add_call(
      i_function_name = 'Z_DISTRIBUTED_PROCESSOR'
      i_parameter     = VALUE #(
        ( name = 'DATA_ID' value = is_request-data_id )
        ( name = 'PARAMETERS' value = is_request-parameters )
      )
    ).
    
    " Отправка с tracking ID
    DATA(lv_tracking_id) = lo_unit->send( ).
    
    " Уведомление о запуске
    cl_amc_channel_manager=>create_message_producer(
      i_application_id = 'ZAMC_BACKGROUND'
      i_channel_id     = '/background/status'
    )->send_message( |Distributed task sent: { lv_tracking_id }| ).
  ENDMETHOD.
ENDCLASS.
```

### Daemon как координатор фоновых процессов

Демон может выступать в роли координатора, управляющего жизненным циклом фоновых задач:

```abap
CLASS zcl_background_coordinator DEFINITION
  PUBLIC
  INHERITING FROM cl_abap_daemon_ext_base
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES: BEGIN OF ty_task_status,
             task_id    TYPE string,
             type       TYPE string,
             status     TYPE string,
             started_at TYPE timestampl,
             progress   TYPE i,
           END OF ty_task_status.
    
  PROTECTED SECTION.
    METHODS: on_event REDEFINITION,
             on_start REDEFINITION.
             
  PRIVATE SECTION.
    DATA: mt_tasks TYPE TABLE OF ty_task_status,
          mo_timer TYPE REF TO if_abap_timer.
    
    METHODS:
      monitor_tasks,
      cleanup_completed_tasks,
      handle_task_event
        IMPORTING iv_message TYPE string,
      send_status_update
        IMPORTING is_task TYPE ty_task_status.
ENDCLASS.

CLASS zcl_background_coordinator IMPLEMENTATION.
  METHOD on_start.
    " Подписка на события от фоновых процессов
    cl_amc_channel_manager=>create_message_consumer(
      i_application_id = 'ZAMC_BACKGROUND'
      i_channel_id     = '/background/events'
    )->start_message_delivery( i_receiver = me ).
    
    " Таймер для мониторинга (каждые 30 секунд)
    mo_timer = cl_abap_timer_manager=>get_timer_manager( )->start_timer(
      i_timeout = 30
      i_timer_handler = me
    ).
  ENDMETHOD.
  
  METHOD on_event.
    CASE i_event_type.
      WHEN if_abap_daemon_extension=>co_event_type_timer.
        monitor_tasks( ).
        cleanup_completed_tasks( ).
        
      WHEN if_abap_daemon_extension=>co_event_type_message.
        " Обработка событий от фоновых процессов
        DATA(lo_message) = CAST if_amc_message_text(
          i_message_manager->get_message( ) ).
        handle_task_event( lo_message->get_text( ) ).
        
    ENDCASE.
  ENDMETHOD.
  
  METHOD monitor_tasks.
    " Проверка статуса всех активных задач
    LOOP AT mt_tasks INTO DATA(ls_task) WHERE status = 'RUNNING'.
      " Проверка через соответствующий API
      CASE ls_task-type.
        WHEN 'BGPF'.
          " Проверка статуса bgPF unit
          TRY.
              DATA(lo_unit) = cl_bgpf_unit_factory=>get_instance( 
                )->get_unit( ls_task-task_id ).
              DATA(lv_status) = lo_unit->get_status( ).
              
              IF lv_status = if_bgpf_unit=>co_status_completed.
                ls_task-status = 'COMPLETED'.
                MODIFY mt_tasks FROM ls_task TRANSPORTING status.
                send_status_update( ls_task ).
              ENDIF.
              
            CATCH cx_bgpf_error.
              ls_task-status = 'ERROR'.
              MODIFY mt_tasks FROM ls_task TRANSPORTING status.
          ENDTRY.
          
        WHEN 'BGRFC'.
          " Проверка статуса bgRFC через мониторинг API
          " Implementation depends on specific bgRFC monitoring
          
      ENDCASE.
    ENDLOOP.
  ENDMETHOD.
  
  METHOD handle_task_event.
    " Обработка событий от задач
    DATA: BEGIN OF ls_event,
            event_type TYPE string,
            task_id    TYPE string,
            status     TYPE string,
            progress   TYPE i,
          END OF ls_event.
    
    /ui2/cl_json=>deserialize(
      EXPORTING json = iv_message
      CHANGING  data = ls_event
    ).
    
    " Обновление статуса задачи
    READ TABLE mt_tasks INTO DATA(ls_task) 
      WITH KEY task_id = ls_event-task_id.
    IF sy-subrc = 0.
      ls_task-status = ls_event-status.
      ls_task-progress = ls_event-progress.
      MODIFY mt_tasks FROM ls_task TRANSPORTING status progress.
      
      " Уведомление клиентов о изменении статуса
      send_status_update( ls_task ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
```

### Практический пример: Файловый монитор

Демон контролирует файловую систему и запускает bgPF процессы для обработки новых файлов:

```abap
CLASS zcl_file_monitor_daemon DEFINITION
  PUBLIC
  INHERITING FROM cl_abap_daemon_ext_base
  FINAL
  CREATE PUBLIC .

  PRIVATE SECTION.
    DATA: mv_watch_directory TYPE string,
          mt_processed_files TYPE TABLE OF string.
    
    METHODS:
      scan_directory
        RETURNING VALUE(rt_files) TYPE string_table,
      trigger_file_processing
        IMPORTING iv_filename TYPE string
        RAISING   cx_bgpf_error.
ENDCLASS.

CLASS zcl_file_monitor_daemon IMPLEMENTATION.
  METHOD on_start.
    mv_watch_directory = '/tmp/inbound/'.
    
    " Сканирование каждые 10 секунд
    cl_abap_timer_manager=>get_timer_manager( )->start_timer(
      i_timeout = 10
      i_timer_handler = me
    ).
  ENDMETHOD.
  
  METHOD on_event.
    IF i_event_type = if_abap_daemon_extension=>co_event_type_timer.
      " Сканирование новых файлов
      DATA(lt_files) = scan_directory( ).
      
      LOOP AT lt_files INTO DATA(lv_file).
        " Проверка, не обрабатывался ли файл
        READ TABLE mt_processed_files TRANSPORTING NO FIELDS
          WITH KEY table_line = lv_file.
        IF sy-subrc <> 0.
          " Новый файл - запуск обработки
          TRY.
              trigger_file_processing( lv_file ).
              APPEND lv_file TO mt_processed_files.
              
            CATCH cx_bgpf_error INTO DATA(lx_error).
              MESSAGE |File processing failed: { lx_error->get_text( ) }| TYPE 'E'.
          ENDTRY.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.
  
  METHOD trigger_file_processing.
    " Запуск bgPF процесса для обработки файла
    DATA(lo_unit) = cl_bgpf_unit_factory=>get_instance( )->create_unit(
      i_name = |FILE_PROCESS_{ iv_filename }|
    ).
    
    lo_unit->add_task(
      i_task_name   = 'PARSE_FILE'
      i_task_class  = 'ZCL_FILE_PARSER'
      i_task_method = 'PROCESS'
      i_task_data   = iv_filename
    ).
    
    lo_unit->add_task(
      i_task_name   = 'VALIDATE_DATA'
      i_task_class  = 'ZCL_DATA_VALIDATOR'
      i_task_method = 'VALIDATE'
      i_depends_on  = VALUE #( ( 'PARSE_FILE' ) )
    ).
    
    lo_unit->add_task(
      i_task_name   = 'SAVE_TO_DB'
      i_task_class  = 'ZCL_DB_PERSISTER'
      i_task_method = 'SAVE'
      i_depends_on  = VALUE #( ( 'VALIDATE_DATA' ) )
    ).
    
    " Callback для уведомления через AMC
    lo_unit->set_callback(
      i_callback_class  = 'ZCL_FILE_PROCESS_CALLBACK'
      i_callback_method = 'ON_COMPLETE'
      i_callback_data   = iv_filename
    ).
    
    lo_unit->start( ).
    
    " Уведомление о запуске
    cl_amc_channel_manager=>create_message_producer(
      i_application_id = 'ZAMC_FILES'
      i_channel_id     = '/files/processing'
    )->send_message( |File processing started: { iv_filename }| ).
  ENDMETHOD.
ENDCLASS.
```

### Архитектурные паттерны интеграции

```mermaid
graph TB
    subgraph "Integration Patterns"
        subgraph "Event-Driven Pattern"
            DAEMON1[ABAP Daemon<br/>Event Monitor]
            AMC1[AMC Channel]
            BGPF1[bgPF Process]
            
            DAEMON1 -->|Event Detection| AMC1
            AMC1 -->|Trigger| BGPF1
        end
        
        subgraph "Coordinator Pattern"
            DAEMON2[Coordinator Daemon]
            BGPF2[bgPF Unit 1]
            BGPF3[bgPF Unit 2]
            BGRFC1[bgRFC Call]
            
            DAEMON2 -->|Orchestrate| BGPF2
            DAEMON2 -->|Orchestrate| BGPF3
            DAEMON2 -->|Delegate| BGRFC1
        end
        
        subgraph "Hybrid Processing"
            REALTIME[Real-time Logic<br/>Daemon]
            BATCH[Batch Processing<br/>bgPF]
            
            REALTIME -->|Heavy Tasks| BATCH
            BATCH -->|Results| REALTIME
        end
    end
    
    style DAEMON1 fill:#99ff99,stroke:#333,stroke-width:2px
    style DAEMON2 fill:#ffcc99,stroke:#333,stroke-width:2px
    style BGPF1 fill:#99ccff,stroke:#333,stroke-width:2px
```

Интеграция ABAP Daemons с традиционными фреймворками фоновой обработки открывает новые возможности для создания эффективных, отказоустойчивых и масштабируемых решений. Правильный выбор фреймворка зависит от специфических требований к производительности, надежности и архитектуре системы.

Подробнее о фреймворках фоновой обработки см. **Главу 11.2**.

## Заключение

Реактивная архитектура в ABAP представляет собой революционное расширение классической модели выполнения:

**Ключевые компоненты**:
- **ABAP Daemons** обеспечивают долгоживущие фоновые процессы
- **AMC (ABAP Messaging Channels)** предоставляет pub/sub messaging
- **APC (ABAP Push Channels)** включает WebSocket коммуникации
- Интеграция всех компонентов создает полноценную event-driven архитектуру

**Новые возможности**:
- Real-time приложения в ABAP
- Event-driven архитектуры
- Асинхронная обработка событий
- Современные паттерны интеграции
- Push-уведомления и двунаправленная связь

Эта архитектура особенно важна для современных сценариев IoT, real-time analytics и collaborative приложений.

В следующей главе мы рассмотрим инструменты анализа и отладки, которые помогают понять и оптимизировать работу всех рассмотренных компонентов архитектуры SAP.