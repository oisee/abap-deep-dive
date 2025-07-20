# Глава 11: BTP и Steampunk - ABAP в облаке

## 11.1. Архитектура BTP ABAP Environment

SAP Business Technology Platform (BTP) ABAP Environment, также известная под кодовым названием "Steampunk", представляет собой революционный подход к запуску ABAP в облаке. Это не просто перенос традиционного ABAP в cloud, а фундаментальное переосмысление платформы для cloud-native разработки.

### Архитектура BTP ABAP Environment

```mermaid
graph TB
    subgraph "BTP ABAP Environment Architecture"
        subgraph "Cloud Foundry Layer"
            CF_ROUTER[CF Router]
            CF_CONTROLLER[Cloud Controller]
            DIEGO[Diego Cells]
            UAA[UAA Service]
        end
        
        subgraph "ABAP Environment"
            subgraph "System Components"
                ABAP_SYS[ABAP System<br/>Steampunk Runtime]
                GW[Embedded Gateway]
                FIORI[Fiori Launchpad]
            end
            
            subgraph "Cloud Services"
                HANA_SERVICE[HANA Cloud Service]
                IDENTITY[Identity Service]
                CONNECTIVITY[Connectivity Service]
                DESTINATION[Destination Service]
            end
            
            subgraph "Development"
                ADT[Eclipse ADT]
                BAS[Business Application Studio]
                GIT[abapGit Integration]
            end
        end
        
        subgraph "Integration Layer"
            API_GW[API Gateway]
            EVENT_MESH[Event Mesh]
            INTEGRATION[Integration Suite]
        end
        
        subgraph "Tenant Isolation"
            TENANT1[Tenant 1<br/>Schema]
            TENANT2[Tenant 2<br/>Schema]
            TENANT_N[Tenant N<br/>Schema]
        end
        
        CF_ROUTER --> ABAP_SYS
        ABAP_SYS --> HANA_SERVICE
        
        HANA_SERVICE --> TENANT1
        HANA_SERVICE --> TENANT2
        HANA_SERVICE --> TENANT_N
        
        ABAP_SYS --> API_GW
        ABAP_SYS --> EVENT_MESH
        
        ADT --> CF_CONTROLLER
        CF_CONTROLLER --> ABAP_SYS
    end
    
    style ABAP_SYS fill:#4CAF50,stroke:#333,stroke-width:4px
    style TENANT1 fill:#99ccff,stroke:#333,stroke-width:2px
```

### Multi-tenancy в Steampunk

```mermaid
graph TB
    subgraph "Multi-tenant Architecture"
        subgraph "Shared Components"
            KERNEL[ABAP Kernel<br/>Shared Binary]
            SYSTEM_CLIENT[System Client 000]
            CROSS_CLIENT[Cross-client Repository]
            RUNTIME[Shared Runtime]
        end
        
        subgraph "Tenant-specific"
            subgraph "Tenant A"
                CLIENT_A[Client 100]
                SCHEMA_A[DB Schema A]
                DATA_A[Business Data A]
                CONFIG_A[Configuration A]
            end
            
            subgraph "Tenant B"
                CLIENT_B[Client 200]
                SCHEMA_B[DB Schema B]
                DATA_B[Business Data B]
                CONFIG_B[Configuration B]
            end
        end
        
        subgraph "Isolation Mechanisms"
            DB_ISOLATION[Database Schema Isolation]
            RUNTIME_ISOLATION[Runtime Context Isolation]
            NETWORK_ISOLATION[Network Isolation]
            STORAGE_ISOLATION[Storage Isolation]
        end
        
        KERNEL --> RUNTIME
        RUNTIME --> CLIENT_A
        RUNTIME --> CLIENT_B
        
        CLIENT_A --> SCHEMA_A
        CLIENT_B --> SCHEMA_B
        
        SCHEMA_A --> DB_ISOLATION
        RUNTIME --> RUNTIME_ISOLATION
    end
    
    style KERNEL fill:#ff9999,stroke:#333,stroke-width:2px
    style DB_ISOLATION fill:#99ff99,stroke:#333,stroke-width:2px
```

### Lifecycle Management

```mermaid
sequenceDiagram
    participant DEV as Developer
    participant GIT as Git Repository
    participant GCTS as gCTS
    participant DEV_SYS as Development System
    participant TEST_SYS as Test System
    participant PROD_SYS as Production System
    
    DEV->>GIT: Push ABAP Code
    GIT->>GCTS: Trigger Pipeline
    
    GCTS->>DEV_SYS: Pull & Deploy
    DEV_SYS->>DEV_SYS: Run ATC Checks
    DEV_SYS->>DEV_SYS: Run Unit Tests
    
    alt Tests Pass
        GCTS->>TEST_SYS: Deploy to Test
        TEST_SYS->>TEST_SYS: Integration Tests
        
        alt Integration Pass
            GCTS->>PROD_SYS: Deploy to Production
            PROD_SYS-->>GCTS: Deployment Success
        else Integration Fail
            TEST_SYS-->>GCTS: Test Failures
            GCTS-->>DEV: Notification
        end
    else Tests Fail
        DEV_SYS-->>GCTS: Build Failed
        GCTS-->>DEV: Fix Required
    end
```

## 11.2. Ограничения ABAP языка в облаке

Steampunk вводит строгие ограничения на использование ABAP для обеспечения cloud-совместимости, безопасности и изоляции tenant'ов.

### Released APIs и Cloud Readiness

```mermaid
graph TB
    subgraph "ABAP Cloud Restrictions"
        subgraph "Forbidden Operations"
            DYNPRO[❌ Classic Dynpro<br/>No SAP GUI]
            FILES[❌ File System Access<br/>No server files]
            SYSTEM[❌ System Commands<br/>No OS access]
            KERNEL[❌ Kernel Calls<br/>No C routines]
            DB_NATIVE[❌ Native SQL<br/>No direct DB access]
        end
        
        subgraph "Released APIs Only"
            RELEASED[✓ Released APIs<br/>C1 contract]
            CDS_CLOUD[✓ CDS Views<br/>Cloud-enabled]
            RAP_CLOUD[✓ RAP Business Objects]
            CLASSES[✓ Released Classes<br/>and Interfaces]
        end
        
        subgraph "Syntax Restrictions"
            NO_FORM[❌ FORM/PERFORM]
            NO_INCLUDES[❌ Include Programs]
            NO_REPORTS[❌ Classic Reports]
            ONLY_CLASSES[✓ Only Classes]
            STRICT_MODE[✓ Strict Syntax Check]
        end
        
        subgraph "New Cloud Objects"
            HTTP_SERVICE[✓ HTTP Service]
            SERVICE_BINDING[✓ Service Binding]
            IAM_APP[✓ IAM App]
            COMMUNICATION[✓ Communication Scenario]
        end
    end
    
    style DYNPRO fill:#ff9999,stroke:#333,stroke-width:2px
    style RELEASED fill:#99ff99,stroke:#333,stroke-width:2px
    style HTTP_SERVICE fill:#99ccff,stroke:#333,stroke-width:2px
```

### API Classification System

```mermaid
graph LR
    subgraph "API Release Contracts"
        subgraph "C0 - Deprecated"
            C0[C0 Contract<br/>Will be removed<br/>Do not use]
        end
        
        subgraph "C1 - Released"
            C1[C1 Contract<br/>Stable API<br/>Cloud ready<br/>Compatible changes only]
        end
        
        subgraph "C2 - System Internal"
            C2[C2 Contract<br/>SAP internal only<br/>Can change anytime]
        end
        
        subgraph "C3 - Restricted"
            C3[C3 Contract<br/>Partner use only<br/>Special agreement]
        end
        
        subgraph "C4 - Test"
            C4[C4 Contract<br/>Test/Demo only<br/>Not for production]
        end
    end
    
    C0 -->|Migrate to| C1
    C2 -->|Cannot use| BLOCKED[❌ Blocked in Cloud]
    C1 -->|Safe to use| ALLOWED[✓ Cloud Development]
    
    style C1 fill:#99ff99,stroke:#333,stroke-width:2px
    style C0 fill:#ff9999,stroke:#333,stroke-width:2px
    style C2 fill:#ffcccc,stroke:#333,stroke-width:2px
```

### Code Migration к Cloud

```mermaid
flowchart TD
    START[Classic ABAP Code]
    
    ANALYZE[Analyze with ATC<br/>Cloud Readiness Check]
    
    CHECK1{Uses Released<br/>APIs only?}
    CHECK2{Has GUI<br/>Dependencies?}
    CHECK3{File System<br/>Access?}
    CHECK4{Native SQL?}
    
    FIX_API[Replace with<br/>Released APIs]
    FIX_GUI[Convert to<br/>Fiori/REST]
    FIX_FILE[Use BTP<br/>Document Service]
    FIX_SQL[Convert to<br/>CDS/Open SQL]
    
    MIGRATE[Deploy to<br/>Steampunk]
    
    START --> ANALYZE
    ANALYZE --> CHECK1
    
    CHECK1 -->|No| FIX_API
    CHECK1 -->|Yes| CHECK2
    
    CHECK2 -->|Yes| FIX_GUI
    CHECK2 -->|No| CHECK3
    
    CHECK3 -->|Yes| FIX_FILE
    CHECK3 -->|No| CHECK4
    
    CHECK4 -->|Yes| FIX_SQL
    CHECK4 -->|No| MIGRATE
    
    FIX_API --> CHECK1
    FIX_GUI --> CHECK3
    FIX_FILE --> CHECK4
    FIX_SQL --> MIGRATE
```

## 11.3. Multi-tenancy и изоляция

### Архитектура изоляции tenant'ов

```mermaid
graph TB
    subgraph "Tenant Isolation Layers"
        subgraph "Application Layer"
            WP_POOL[Shared Work Process Pool]
            TENANT_CONTEXT[Tenant Context Switch]
            SESSION_ISOLATION[Session Isolation]
        end
        
        subgraph "Database Layer"
            subgraph "Logical Isolation"
                SCHEMA_SEP[Schema Separation]
                VIEW_RESTRICTION[View Restrictions]
                ROW_LEVEL[Row-level Security]
            end
            
            subgraph "Physical Isolation"
                TABLESPACE[Separate Tablespaces]
                ENCRYPTION[Tenant Encryption Keys]
                BACKUP_SEP[Separate Backups]
            end
        end
        
        subgraph "Network Layer"
            VNET[Virtual Networks]
            FIREWALL[Tenant Firewalls]
            ROUTING[Isolated Routing]
        end
        
        subgraph "Storage Layer"
            OBJECT_STORE[Object Storage<br/>Tenant Containers]
            VOLUME_SEP[Volume Separation]
        end
    end
    
    WP_POOL --> TENANT_CONTEXT
    TENANT_CONTEXT --> SCHEMA_SEP
    SCHEMA_SEP --> TABLESPACE
    
    style TENANT_CONTEXT fill:#ff9999,stroke:#333,stroke-width:2px
    style SCHEMA_SEP fill:#99ccff,stroke:#333,stroke-width:2px
```

### Tenant Onboarding Process

```mermaid
sequenceDiagram
    participant CUSTOMER as Customer
    participant BTP as BTP Cockpit
    participant PROVISIONING as Provisioning Service
    participant ABAP_ENV as ABAP Environment
    participant HANA as HANA Cloud
    participant IAM as Identity Service
    
    CUSTOMER->>BTP: Subscribe to ABAP Environment
    BTP->>PROVISIONING: Create Tenant Request
    
    PROVISIONING->>HANA: Create Schema
    HANA-->>PROVISIONING: Schema Created
    
    PROVISIONING->>ABAP_ENV: Initialize Tenant
    ABAP_ENV->>ABAP_ENV: Create Client
    ABAP_ENV->>ABAP_ENV: Setup Repository
    ABAP_ENV->>ABAP_ENV: Initialize Services
    
    PROVISIONING->>IAM: Setup Identity Provider
    IAM-->>PROVISIONING: IdP Configured
    
    PROVISIONING->>BTP: Tenant Ready
    BTP-->>CUSTOMER: Access Details
    
    Note over CUSTOMER,ABAP_ENV: Tenant fully isolated and ready
```

## 11.4. Embedded Steampunk vs Steampunk

### Deployment Models Comparison

```mermaid
graph TB
    subgraph "Steampunk Variants"
        subgraph "Standalone Steampunk"
            BTP_ABAP[BTP ABAP Environment<br/>Pure Cloud]
            MULTI_TENANT[Multi-tenant]
            FULL_RESTRICT[Full Restrictions]
            CLOUD_ONLY[Cloud Services Only]
        end
        
        subgraph "Embedded Steampunk"
            S4_EMBEDDED[S/4HANA Embedded<br/>On-premise + Cloud]
            SINGLE_TENANT[Single-tenant]
            RELAXED[Some Restrictions Relaxed]
            HYBRID_SERVICES[Hybrid Services]
        end
        
        subgraph "Key Differences"
            CLASSIC_ACCESS[Classic Objects:<br/>Embedded ✓<br/>Standalone ❌]
            
            DEPLOYMENT[Deployment:<br/>Embedded: On-prem/Private<br/>Standalone: Public Cloud]
            
            EXTENSIBILITY[Extensibility:<br/>Embedded: Side-by-side<br/>Standalone: Cloud-native]
        end
        
        BTP_ABAP --> FULL_RESTRICT
        S4_EMBEDDED --> RELAXED
        
        RELAXED --> CLASSIC_ACCESS
        FULL_RESTRICT --> CLOUD_ONLY
    end
    
    style BTP_ABAP fill:#4CAF50,stroke:#333,stroke-width:2px
    style S4_EMBEDDED fill:#99ccff,stroke:#333,stroke-width:2px
```

### Developer Experience

```mermaid
graph LR
    subgraph "Development Tools"
        subgraph "Supported IDEs"
            ADT_ECLIPSE[Eclipse ADT<br/>Primary IDE]
            BAS_CLOUD[Business Application<br/>Studio]
            VSCODE[VS Code<br/>Limited Support]
        end
        
        subgraph "Version Control"
            ABAPGIT[abapGit<br/>Git Integration]
            GCTS[gCTS<br/>Git-enabled CTS]
        end
        
        subgraph "Testing"
            ABAP_UNIT[ABAP Unit<br/>Unit Testing]
            INTEGRATION[Integration Tests]
            E2E[End-to-End Tests]
        end
        
        subgraph "DevOps"
            PIPELINE[CI/CD Pipeline]
            ATC_CLOUD[ATC Cloud Checks]
            SECURITY[Security Scans]
        end
    end
    
    ADT_ECLIPSE --> ABAPGIT
    ABAPGIT --> PIPELINE
    PIPELINE --> ATC_CLOUD
    ATC_CLOUD --> ABAP_UNIT
```

---

# Глава 11.1: ABAP Daemons и Channels - реактивная архитектура

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

## Заключение

ABAP в облаке представляет собой фундаментальную трансформацию платформы:

**BTP ABAP Environment (Steampunk)**:
- Полностью cloud-native архитектура
- Строгие ограничения для обеспечения изоляции
- Multi-tenant by design
- Современные инструменты разработки

**Реактивная архитектура**:
- **ABAP Daemons** обеспечивают долгоживущие процессы
- **AMC** предоставляет pub/sub messaging
- **APC** включает WebSocket коммуникации
- Интеграция всех компонентов создает полноценную event-driven архитектуру

Эти технологии открывают новые возможности:
- Real-time приложения в ABAP
- Event-driven архитектуры
- Современные паттерны интеграции
- Cloud-ready решения

В следующей главе мы рассмотрим инструменты анализа и отладки, которые помогают понять и оптимизировать работу всех рассмотренных компонентов архитектуры SAP.