# Глава 10: От BOPF к RAP - эволюция бизнес-объектов

## 10.1. BOPF: конфигурационный подход

Business Object Processing Framework (BOPF) был представлен SAP как унифицированный фреймворк для создания и управления бизнес-объектами. Основная идея заключалась в декларативном подходе: разработчик конфигурирует бизнес-объект через специальные инструменты, а фреймворк генерирует необходимую инфраструктуру.

### Архитектура BOPF

```mermaid
graph TB
    subgraph "BOPF Architecture"
        subgraph "Design Time"
            BO_MODEL[Business Object Model]
            NODES[Node Structure]
            ASSOC[Associations]
            ACTIONS[Actions]
            DETERM[Determinations]
            VALID[Validations]
            QUERIES[Queries]
        end
        
        subgraph "BOPF Runtime"
            subgraph "Core Services"
                MANAGER[BO Manager]
                BUFFER[Transactional Buffer]
                PERSIST[Persistence Layer]
                AUTH_SRV[Authorization Service]
            end
            
            subgraph "Service Providers"
                CONF_PROV[Configuration Provider]
                DATA_PROV[Data Provider]
                META_PROV[Metadata Provider]
            end
        end
        
        subgraph "Generated Artifacts"
            CONST[Constants Interface]
            STRUCTS[DDIC Structures]
            DB_TABLES[Database Tables]
            CDS_VIEWS[CDS Views]
        end
        
        subgraph "Consumption"
            FBI[Floorplan Manager BOPF Integration]
            GATEWAY[SAP Gateway]
            CUSTOM[Custom ABAP]
        end
        
        BO_MODEL --> MANAGER
        NODES --> CONF_PROV
        ASSOC --> CONF_PROV
        ACTIONS --> CONF_PROV
        
        MANAGER --> BUFFER
        BUFFER --> PERSIST
        MANAGER --> AUTH_SRV
        
        CONF_PROV --> CONST
        DATA_PROV --> DB_TABLES
        
        FBI --> MANAGER
        GATEWAY --> MANAGER
        CUSTOM --> MANAGER
    end
    
    style MANAGER fill:#ff9999,stroke:#333,stroke-width:4px
    style BUFFER fill:#99ccff,stroke:#333,stroke-width:2px
```

### Основные концепции BOPF

**Узлы (Nodes)** - основные строительные блоки бизнес-объекта:

```abap
* Пример структуры узла BOPF
TYPES: BEGIN OF ts_sales_order_node,
         key         TYPE /bobf/conf_key,  " Технический ключ BOPF
         db_key      TYPE vbeln,           " Бизнес-ключ
         order_date  TYPE erdat,
         customer    TYPE kunnr,
         total_value TYPE netwr,
         currency    TYPE waerk,
         status      TYPE char2,
       END OF ts_sales_order_node.
```

**Ассоциации** связывают узлы между собой:

```mermaid
graph LR
    subgraph "BOPF Node Associations"
        SO[Sales Order<br/>Root Node]
        ITEM[Order Items<br/>Sub Node]
        PARTNER[Partners<br/>Sub Node]
        TEXT[Texts<br/>Sub Node]
        
        SO -->|Composition| ITEM
        SO -->|Composition| PARTNER
        SO -->|Composition| TEXT
        
        ITEM -->|To Parent| SO
        PARTNER -->|To Parent| SO
        
        SO -.->|Cross-BO| CUSTOMER[Customer BO]
    end
    
    style SO fill:#4CAF50,stroke:#333,stroke-width:2px
```

### Транзакционная модель BOPF

```mermaid
sequenceDiagram
    participant UI as User Interface
    participant Manager as BOPF Manager
    participant Buffer as Transaction Buffer
    participant Determ as Determinations
    participant Valid as Validations
    participant DB as Database
    
    UI->>Manager: Modify Request
    Manager->>Buffer: Store Changes
    Buffer->>Buffer: Track Modifications
    
    Manager->>Determ: Execute Before Save
    Determ->>Buffer: Update Data
    
    Manager->>Valid: Run Validations
    Valid-->>Manager: Validation Result
    
    alt Validation Success
        Manager->>Buffer: Prepare Save
        Buffer->>DB: Persist Changes
        DB-->>Buffer: Confirmation
        Buffer->>Buffer: Clear Buffer
        Manager-->>UI: Success
    else Validation Failed
        Manager-->>UI: Error Messages
    end
```

### Конфигурация бизнес-объекта

```abap
* Использование BOPF Manager
DATA: lo_manager      TYPE REF TO /bobf/if_tra_service_manager,
      lt_key          TYPE /bobf/t_frw_key,
      lt_sales_order  TYPE TABLE OF ts_sales_order_node,
      lo_message      TYPE REF TO /bobf/if_frw_message.

" Получение экземпляра менеджера
lo_manager = /bobf/cl_tra_serv_mgr_factory=>get_service_manager(
  /scmg/if_constant=>sc_bo_key ).

" Чтение данных
lo_manager->retrieve(
  EXPORTING
    iv_node_key     = /scmg/if_constant=>sc_node-root
    it_key          = lt_key
  IMPORTING
    et_data         = lt_sales_order
    eo_message      = lo_message ).

" Модификация данных
lo_manager->modify(
  EXPORTING
    iv_node_key     = /scmg/if_constant=>sc_node-root
    iv_key          = ls_key
    is_data         = ls_sales_order
    iv_root_key     = ls_root_key
  IMPORTING
    eo_message      = lo_message ).

" Сохранение транзакции
lo_manager->save(
  IMPORTING
    eo_message = lo_message
    ev_rejected = lv_rejected ).
```

### Действия и детерминации

```abap
* Пример детерминации BOPF
CLASS lcl_d_calculate_total IMPLEMENTATION.
  METHOD /bobf/if_frw_determination~execute.
    DATA: lt_sales_order TYPE TABLE OF ts_sales_order_node,
          lt_items       TYPE TABLE OF ts_item_node.
    
    " Получение данных корневого узла
    io_read->retrieve(
      EXPORTING
        iv_node   = is_ctx-node_key
        it_key    = it_key
      IMPORTING
        et_data   = lt_sales_order ).
    
    " Получение элементов через ассоциацию
    io_read->retrieve_by_association(
      EXPORTING
        iv_node                 = is_ctx-node_key
        it_key                  = it_key
        iv_association          = /scmg/if_constant=>sc_association-root-items
      IMPORTING
        et_data                 = lt_items ).
    
    " Расчет суммы
    LOOP AT lt_sales_order REFERENCE INTO DATA(lr_order).
      lr_order->total_value = 0.
      LOOP AT lt_items INTO DATA(ls_item) 
        WHERE parent_key = lr_order->key.
        lr_order->total_value += ls_item-net_value.
      ENDLOOP.
      
      " Обновление через modify
      io_modify->update(
        iv_node = is_ctx-node_key
        iv_key  = lr_order->key
        is_data = lr_order->* ).
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
```

## 10.2. Ограничения и проблемы BOPF

Несмотря на мощные возможности, BOPF имел ряд серьезных ограничений, которые привели к его устареванию:

### Архитектурные проблемы

```mermaid
graph TB
    subgraph "BOPF Limitations"
        subgraph "Development Complexity"
            TOOLS[Special Tools Required<br/>BO Builder]
            CONFIG[Complex Configuration]
            DEBUG[Difficult Debugging]
            LEARN[Steep Learning Curve]
        end
        
        subgraph "Runtime Issues"
            PERF[Performance Overhead]
            MEM[Memory Consumption]
            BUFFER_ISSUE[Buffer Complexity]
            TRACE[Poor Traceability]
        end
        
        subgraph "Integration Problems"
            CDS[Limited CDS Support]
            MODERN[No Modern ABAP]
            TEST[Testing Difficulties]
            CLOUD[Not Cloud Ready]
        end
        
        subgraph "Maintenance"
            VERSION[Versioning Issues]
            TRANSPORT[Transport Complexity]
            ENHANCE[Enhancement Limitations]
            DOCU[Poor Documentation]
        end
    end
    
    style PERF fill:#ff9999,stroke:#333,stroke-width:2px
    style CLOUD fill:#ff9999,stroke:#333,stroke-width:2px
    style CONFIG fill:#ffcccc,stroke:#333,stroke-width:2px
```

### Проблемы производительности

```abap
* Типичный анти-паттерн BOPF - множественные вызовы retrieve
METHOD get_order_details_bopf.
  " Проблема 1: Отдельный вызов для каждого узла
  lo_manager->retrieve(
    EXPORTING iv_node_key = sc_node-root
              it_key      = lt_order_keys
    IMPORTING et_data     = lt_orders ).
  
  " Проблема 2: N+1 запросов для ассоциаций
  LOOP AT lt_orders INTO ls_order.
    lo_manager->retrieve_by_association(
      EXPORTING iv_node        = sc_node-root
                it_key         = VALUE #( ( key = ls_order-key ) )
                iv_association = sc_association-root-items
      IMPORTING et_data        = lt_items ).
    
    " Проблема 3: Еще больше вызовов для вложенных данных
    LOOP AT lt_items INTO ls_item.
      lo_manager->retrieve_by_association(
        EXPORTING iv_node        = sc_node-items  
                  it_key         = VALUE #( ( key = ls_item-key ) )
                  iv_association = sc_association-items-schedule_lines
        IMPORTING et_data        = lt_schedule ).
    ENDLOOP.
  ENDLOOP.
  " Результат: сотни запросов для простой операции
ENDMETHOD.
```

### Сложность отладки

```mermaid
sequenceDiagram
    participant Dev as Developer
    participant Code as ABAP Code
    participant BOPF as BOPF Runtime
    participant Config as Configuration
    participant Gen as Generated Code
    
    Dev->>Code: Set Breakpoint
    Code->>BOPF: Call Manager
    BOPF->>Config: Load Configuration
    Config->>Gen: Execute Generated Code
    Gen->>Gen: Internal Processing
    Note over Gen: No access to debug
    Gen-->>BOPF: Result
    BOPF-->>Code: Return
    
    Note over Dev,Gen: Developer cannot see<br/>what happens inside
```

### Несовместимость с современным ABAP

```abap
* BOPF не поддерживает современный синтаксис
" Это НЕ работает с BOPF:
SELECT FROM demo_sales_order
  FIELDS order_id,
         customer,
         total_value
  WHERE status = 'OPEN'
  INTO TABLE @DATA(lt_orders).

" Вместо этого приходится использовать:
DATA: lt_key TYPE /bobf/t_frw_key,
      lt_data TYPE TABLE OF ts_order.

lo_manager->query(
  EXPORTING
    iv_query_key    = sc_query-select_by_status
    it_filter_key   = VALUE #( ( attribute_name = 'STATUS' 
                                sign = 'I' 
                                option = 'EQ' 
                                low = 'OPEN' ) )
  IMPORTING
    et_key          = lt_key ).

lo_manager->retrieve(
  EXPORTING
    iv_node_key     = sc_node-root
    it_key          = lt_key
  IMPORTING
    et_data         = lt_data ).
```

## 10.3. RAP: код как конфигурация

RESTful ABAP Programming Model (RAP) представляет собой революционный подход, где бизнес-объекты определяются непосредственно в коде ABAP, а не через внешнюю конфигурацию.

### Архитектура RAP

```mermaid
graph TB
    subgraph "RAP Architecture"
        subgraph "Development Artifacts"
            CDS_INT[CDS Interface View]
            CDS_CONS[CDS Consumption View]
            BDEF[Behavior Definition]
            BIMP[Behavior Implementation]
            SRVD[Service Definition]
            SRVB[Service Binding]
        end
        
        subgraph "RAP Runtime"
            RUNTIME[RAP Runtime Engine]
            SADL_INT[SADL Integration]
            TM[Transaction Manager]
            BM[Buffer Manager]
            VM[Validation Manager]
        end
        
        subgraph "Generated at Runtime"
            ODATA[OData Service]
            UI5[Fiori Elements UI]
            API[Business API]
        end
        
        subgraph "Consumption"
            FIORI[Fiori Apps]
            EML[ABAP with EML]
            EXTERNAL[External Systems]
        end
        
        CDS_INT --> BDEF
        CDS_CONS --> SRVD
        BDEF --> BIMP
        SRVD --> SRVB
        
        SRVB --> RUNTIME
        RUNTIME --> SADL_INT
        RUNTIME --> TM
        RUNTIME --> BM
        
        SADL_INT --> ODATA
        ODATA --> FIORI
        RUNTIME --> API
        API --> EML
        API --> EXTERNAL
    end
    
    style RUNTIME fill:#4CAF50,stroke:#333,stroke-width:4px
    style BDEF fill:#ff9999,stroke:#333,stroke-width:2px
```

### Базовая структура RAP

```abap
* 1. CDS Interface View
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Sales Order Interface'
define root view entity ZI_SalesOrder
  as select from vbak
  composition [0..*] of ZI_SalesOrderItem as _Items
{
  key vbeln as SalesOrder,
      erdat as OrderDate,
      ernam as CreatedBy,
      kunnr as Customer,
      @Semantics.amount.currencyCode: 'Currency'
      netwr as TotalValue,
      waerk as Currency,
      
      /* Associations */
      _Items
}

* 2. CDS Projection View
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Sales Order Projection'
@UI.headerInfo: { 
  typeName: 'Sales Order',
  typeNamePlural: 'Sales Orders',
  title: { value: 'SalesOrder' }
}
define root view entity ZC_SalesOrder
  provider contract transactional_query
  as projection on ZI_SalesOrder
{
  @UI.facet: [{ 
    id: 'OrderHeader',
    type: #IDENTIFICATION_REFERENCE,
    label: 'Order Information',
    position: 10 
  }]
  
  @UI.lineItem: [{ position: 10 }]
  @UI.identification: [{ position: 10 }]
  key SalesOrder,
  
  @UI.lineItem: [{ position: 20 }]
  @UI.identification: [{ position: 20 }]
  OrderDate,
  
  @UI.lineItem: [{ position: 30 }]
  @UI.identification: [{ position: 30 }]
  Customer,
  
  @UI.lineItem: [{ position: 40 }]
  @UI.identification: [{ position: 40 }]
  TotalValue,
  
  @UI.hidden: true
  Currency,
  
  /* Associations */
  _Items : redirected to composition child ZC_SalesOrderItem
}
```

### Behavior Definition

```abap
managed implementation in class zbp_i_salesorder unique;
strict ( 2 );
with draft;

define behavior for ZI_SalesOrder alias SalesOrder
persistent table vbak
draft table zd_salesorder
etag master LastChangedAt
lock master total etag CreatedAt
authorization master ( global )
{
  field ( readonly ) SalesOrder, CreatedAt, CreatedBy;
  field ( mandatory : create ) Customer;
  
  create;
  update;
  delete;
  
  draft action Edit;
  draft action Activate;
  draft action Discard;
  draft action Resume;
  draft determine action Prepare;
  
  determination calculateTotalValue on save { create; update; }
  validation validateCustomer on save { create; update; field Customer; }
  
  action ( features : instance ) confirmOrder result [1] $self;
  
  association _Items { create; with draft; }
  
  mapping for vbak
  {
    SalesOrder = vbeln;
    OrderDate = erdat;
    CreatedBy = ernam;
    Customer = kunnr;
    TotalValue = netwr;
    Currency = waerk;
  }
}

define behavior for ZI_SalesOrderItem alias Item
persistent table vbap
draft table zd_salesitem
etag master LastChangedAt
lock dependent by _Order
authorization dependent by _Order
{
  field ( readonly ) SalesOrder, ItemNumber;
  field ( readonly : update ) Material;
  
  update;
  delete;
  
  determination calculateItemValue on save { create; update; field Quantity, Price; }
  
  association _Order { with draft; }
  
  mapping for vbap
  {
    SalesOrder = vbeln;
    ItemNumber = posnr;
    Material = matnr;
    Quantity = kwmeng;
    Price = netpr;
    ItemValue = netwr;
  }
}
```

### Behavior Implementation

```abap
CLASS zbp_i_salesorder IMPLEMENTATION.

  METHOD calculateTotalValue.
    " Читаем все элементы для заказов
    READ ENTITIES OF zi_salesorder IN LOCAL MODE
      ENTITY SalesOrder
        FIELDS ( SalesOrder )
        WITH CORRESPONDING #( keys )
      RESULT DATA(orders)
      
      ENTITY SalesOrder BY \_Items
        FIELDS ( ItemValue )
        WITH CORRESPONDING #( keys )
      LINK DATA(order_items).
    
    " Вычисляем сумму
    LOOP AT orders INTO DATA(order).
      DATA(total) = REDUCE netwr( INIT sum = 0
                                  FOR item IN order_items 
                                  WHERE ( source-SalesOrder = order-SalesOrder )
                                  NEXT sum = sum + item-target-ItemValue ).
      
      APPEND VALUE #( %tky = order-%tky
                      TotalValue = total ) TO entities_update.
    ENDLOOP.
    
    " Обновляем заказы
    MODIFY ENTITIES OF zi_salesorder IN LOCAL MODE
      ENTITY SalesOrder
        UPDATE FIELDS ( TotalValue )
        WITH entities_update.
  ENDMETHOD.
  
  METHOD validateCustomer.
    " Читаем клиентов
    READ ENTITIES OF zi_salesorder IN LOCAL MODE
      ENTITY SalesOrder
        FIELDS ( Customer )
        WITH CORRESPONDING #( keys )
      RESULT DATA(orders).
    
    " Проверяем существование клиентов
    SELECT kunnr FROM kna1
      FOR ALL ENTRIES IN @orders
      WHERE kunnr = @orders-Customer
      INTO TABLE @DATA(valid_customers).
    
    " Формируем сообщения об ошибках
    LOOP AT orders INTO DATA(order).
      IF NOT line_exists( valid_customers[ kunnr = order-Customer ] ).
        APPEND VALUE #( %tky = order-%tky ) TO failed-salesorder.
        
        APPEND VALUE #( %tky = order-%tky
                        %msg = new_message_with_text(
                          severity = if_abap_behv_message=>severity-error
                          text = |Customer { order-Customer } does not exist| )
                        %element-Customer = if_abap_behv=>mk-on
                      ) TO reported-salesorder.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
  
  METHOD confirmOrder.
    " Изменяем статус заказа
    MODIFY ENTITIES OF zi_salesorder IN LOCAL MODE
      ENTITY SalesOrder
        UPDATE FIELDS ( Status )
        WITH VALUE #( FOR key IN keys
                      ( %tky = key-%tky
                        Status = 'CONFIRMED' ) ).
    
    " Возвращаем измененные экземпляры
    READ ENTITIES OF zi_salesorder IN LOCAL MODE
      ENTITY SalesOrder
        ALL FIELDS
        WITH CORRESPONDING #( keys )
      RESULT DATA(orders).
    
    result = VALUE #( FOR order IN orders
                      ( %tky = order-%tky
                        %param = order ) ).
  ENDMETHOD.

ENDCLASS.
```

## 10.4. Managed vs Unmanaged сценарии

### Managed сценарий

В managed сценарии RAP runtime автоматически генерирует всю инфраструктуру:

```mermaid
graph TB
    subgraph "Managed Implementation"
        subgraph "Developer Provides"
            BDEF_M[Behavior Definition<br/>with 'managed']
            BIMP_M[Behavior Implementation<br/>Only Business Logic]
            TABLE[Database Table]
        end
        
        subgraph "RAP Generates"
            CRUD[CRUD Operations]
            LOCK[Lock Handling]
            DRAFT[Draft Handling]
            ETG[ETag Management]
            NUM[Numbering]
            BUFF[Buffer Management]
        end
        
        subgraph "Runtime Flow"
            REQ[Request]
            RAP_RT[RAP Runtime]
            AUTO[Auto-generated Logic]
            CUSTOM[Custom Logic]
            DB[(Database)]
        end
        
        BDEF_M --> CRUD
        BDEF_M --> LOCK
        BDEF_M --> DRAFT
        
        REQ --> RAP_RT
        RAP_RT --> AUTO
        AUTO --> CUSTOM
        CUSTOM --> DB
    end
    
    style AUTO fill:#4CAF50,stroke:#333,stroke-width:2px
    style CUSTOM fill:#99ccff,stroke:#333,stroke-width:2px
```

### Unmanaged сценарий

В unmanaged сценарии разработчик контролирует все аспекты:

```abap
CLASS zbp_i_legacy_order IMPLEMENTATION.

  METHOD create.
    LOOP AT entities INTO DATA(entity).
      " Собственная логика создания
      CALL FUNCTION 'SD_SALES_ORDER_CREATE'
        EXPORTING
          order_header = VALUE bapisdhd1( 
            doc_type = 'TA'
            sales_org = '1000'
            distr_chan = '01'
            division = '01'
            sold_to_party = entity-Customer )
        IMPORTING
          salesdocument = DATA(new_order)
        TABLES
          return = DATA(return_tab).
      
      IF line_exists( return_tab[ type = 'E' ] ).
        APPEND VALUE #( %cid = entity-%cid ) TO failed-salesorder.
        APPEND VALUE #( %cid = entity-%cid
                        %msg = new_message_with_text( 
                          text = return_tab[ type = 'E' ]-message ) 
                      ) TO reported-salesorder.
      ELSE.
        APPEND VALUE #( %cid = entity-%cid
                        SalesOrder = new_order ) TO mapped-salesorder.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
  
  METHOD read.
    " Собственная логика чтения
    SELECT * FROM vbak
      FOR ALL ENTRIES IN @keys
      WHERE vbeln = @keys-SalesOrder
      INTO CORRESPONDING FIELDS OF TABLE @result.
  ENDMETHOD.
  
  METHOD update.
    " Собственная логика обновления
    LOOP AT entities INTO DATA(entity).
      CALL FUNCTION 'SD_SALES_ORDER_CHANGE'
        EXPORTING
          salesdocument = entity-SalesOrder
          order_header_inx = VALUE bapisdhd1x( updateflag = 'U' )
        TABLES
          return = DATA(return_tab).
    ENDLOOP.
  ENDMETHOD.
  
  METHOD delete.
    " Собственная логика удаления - часто не реализуется
    APPEND VALUE #( %key = keys[ 1 ]
                    %msg = new_message_with_text( 
                      text = 'Deletion not allowed' )
                  ) TO reported-salesorder.
  ENDMETHOD.
  
  METHOD lock.
    " Собственная логика блокировки
    LOOP AT keys INTO DATA(key).
      CALL FUNCTION 'ENQUEUE_EVVBAKE'
        EXPORTING
          vbeln = key-SalesOrder
        EXCEPTIONS
          foreign_lock = 1
          OTHERS = 2.
          
      IF sy-subrc <> 0.
        APPEND VALUE #( %key = key ) TO failed-salesorder.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
```

### Сравнение подходов

```mermaid
graph LR
    subgraph "Comparison: Managed vs Unmanaged"
        subgraph "Managed"
            M_PROS[Pros:<br/>- Fast development<br/>- Standard behavior<br/>- Automatic features<br/>- Less code]
            M_CONS[Cons:<br/>- Less flexibility<br/>- Standard tables only<br/>- Limited legacy integration]
        end
        
        subgraph "Unmanaged"
            U_PROS[Pros:<br/>- Full control<br/>- Legacy integration<br/>- Custom logic<br/>- Any data source]
            U_CONS[Cons:<br/>- More code<br/>- Manual everything<br/>- Error prone<br/>- Slower development]
        end
        
        subgraph "Use Cases"
            M_USE[Managed for:<br/>- New development<br/>- Standard scenarios<br/>- Rapid prototyping]
            U_USE[Unmanaged for:<br/>- Legacy systems<br/>- Complex logic<br/>- External data<br/>- Special requirements]
        end
        
        M_PROS --> M_USE
        M_CONS --> U_USE
        U_PROS --> U_USE
        U_CONS --> M_USE
    end
    
    style M_PROS fill:#99ff99,stroke:#333,stroke-width:2px
    style U_PROS fill:#99ccff,stroke:#333,stroke-width:2px
```

### Entity Manipulation Language (EML)

EML - это расширение ABAP для работы с RAP бизнес-объектами:

```abap
* Использование EML для работы с бизнес-объектами
METHOD process_orders_with_eml.
  " CREATE - создание новых заказов
  MODIFY ENTITIES OF zi_salesorder
    ENTITY SalesOrder
      CREATE FIELDS ( Customer OrderDate )
      WITH VALUE #( ( %cid = 'ORDER1'
                      Customer = '1000'
                      OrderDate = sy-datum ) )
      
    ENTITY SalesOrder
      CREATE BY \_Items
      FIELDS ( Material Quantity Price )
      WITH VALUE #( ( %cid_ref = 'ORDER1'
                      %target = VALUE #( 
                        ( %cid = 'ITEM1'
                          Material = 'MAT001'
                          Quantity = 10
                          Price = 100 ) ) ) )
                          
    MAPPED DATA(mapped)
    FAILED DATA(failed)
    REPORTED DATA(reported).
  
  " Проверка на ошибки
  IF failed IS NOT INITIAL.
    " Обработка ошибок
    LOOP AT reported-salesorder INTO DATA(msg).
      MESSAGE msg-%msg TYPE 'E'.
    ENDLOOP.
    RETURN.
  ENDIF.
  
  " COMMIT ENTITIES - сохранение с вызовом всех детерминаций
  COMMIT ENTITIES
    RESPONSE OF zi_salesorder
    FAILED DATA(failed_commit)
    REPORTED DATA(reported_commit).
  
  " READ - чтение созданных данных
  READ ENTITIES OF zi_salesorder
    ENTITY SalesOrder
      ALL FIELDS
      WITH VALUE #( ( SalesOrder = mapped-salesorder[ %cid = 'ORDER1' ]-SalesOrder ) )
    RESULT DATA(orders).
  
  " UPDATE - обновление
  MODIFY ENTITIES OF zi_salesorder
    ENTITY SalesOrder
      UPDATE FIELDS ( Status )
      WITH VALUE #( ( %key-SalesOrder = orders[ 1 ]-SalesOrder
                      Status = 'RELEASED' ) ).
  
  " ACTION - вызов действия
  MODIFY ENTITIES OF zi_salesorder
    ENTITY SalesOrder
      EXECUTE confirmOrder
      FROM VALUE #( ( %key-SalesOrder = orders[ 1 ]-SalesOrder ) )
    RESULT DATA(confirmation_result).
  
  " DELETE - удаление
  MODIFY ENTITIES OF zi_salesorder
    ENTITY SalesOrder
      DELETE FROM VALUE #( ( %key-SalesOrder = orders[ 1 ]-SalesOrder ) ).
ENDMETHOD.
```

### Транзакционная обработка в RAP

```mermaid
sequenceDiagram
    participant Client
    participant RAP as RAP Runtime
    participant TM as Transaction Manager
    participant BM as Buffer Manager
    participant BI as Behavior Implementation
    participant DB as Database
    
    Client->>RAP: MODIFY ENTITIES
    RAP->>TM: Start Transaction
    TM->>BM: Initialize Buffer
    
    RAP->>BI: Call CREATE
    BI->>BM: Store in Buffer
    
    RAP->>BI: Call Determination
    BI->>BM: Update Buffer
    
    Client->>RAP: COMMIT ENTITIES
    RAP->>BI: Call Validation
    BI-->>RAP: Validation Result
    
    alt Validation Success
        RAP->>TM: Prepare Commit
        TM->>BM: Get Changes
        BM-->>TM: Changed Data
        TM->>DB: Save Changes
        DB-->>TM: Success
        TM->>TM: Commit
        RAP-->>Client: Success
    else Validation Failed
        RAP->>TM: Rollback
        TM->>BM: Clear Buffer
        RAP-->>Client: Failed with Messages
    end
```

### Draft обработка

```abap
* Draft таблица для сохранения черновиков
@EndUserText.label : 'Draft table for ZI_SALESORDER'
@AbapCatalog.enhancement.category : #EXTENSIBLE_ANY
@AbapCatalog.tableCategory : #TRANSPARENT
@AbapCatalog.deliveryClass : #A
@AbapCatalog.dataMaintenance : #RESTRICTED
define table zd_salesorder {
  key mandt      : mandt not null;
  key salesorder : vbeln not null;
  orderdate      : erdat;
  customer       : kunnr;
  totalvalue     : netwr;
  currency       : waerk;
  
  "%admin"       : include sych_bdl_draft_admin_inc;
}
```

```mermaid
graph TB
    subgraph "Draft Lifecycle"
        EDIT[Edit Action<br/>Create Draft]
        DRAFT[Draft Instance<br/>In Draft Table]
        MODIFY[User Modifies<br/>Auto-save]
        PREPARE[Prepare Action<br/>Final Validations]
        ACTIVATE[Activate Action<br/>Make Persistent]
        DISCARD[Discard Action<br/>Delete Draft]
        
        ACTIVE[Active Instance<br/>In Main Table]
        
        ACTIVE -->|User clicks Edit| EDIT
        EDIT --> DRAFT
        DRAFT -->|User changes| MODIFY
        MODIFY --> DRAFT
        DRAFT -->|User saves| PREPARE
        PREPARE -->|Valid| ACTIVATE
        PREPARE -->|Invalid| DRAFT
        ACTIVATE --> ACTIVE
        DRAFT -->|User cancels| DISCARD
        DISCARD --> ACTIVE
    end
    
    style DRAFT fill:#ffff99,stroke:#333,stroke-width:2px
    style ACTIVE fill:#99ff99,stroke:#333,stroke-width:2px
```

### Side Effects в RAP

```abap
* Определение side effects в behavior definition
define behavior for ZI_SalesOrder alias SalesOrder
{
  field ( features : instance ) Discount triggers DeterminePrice;
  
  determination DeterminePrice on modify { field Discount; }
  
  side effects
  {
    field Discount affects field TotalValue, field TaxAmount;
    field Customer affects entity _Items;
    action confirmOrder affects $self;
  }
}

* Реализация с учетом side effects
METHOD DeterminePrice.
  READ ENTITIES OF zi_salesorder IN LOCAL MODE
    ENTITY SalesOrder
      FIELDS ( Discount TotalValue )
      WITH CORRESPONDING #( keys )
    RESULT DATA(orders).
    
  MODIFY ENTITIES OF zi_salesorder IN LOCAL MODE
    ENTITY SalesOrder
      UPDATE FIELDS ( TotalValue TaxAmount )
      WITH VALUE #( FOR order IN orders
                    ( %tky = order-%tky
                      TotalValue = order-TotalValue * ( 1 - order-Discount / 100 )
                      TaxAmount = order-TotalValue * '0.19' ) ).
ENDMETHOD.
```

## Заключение

Переход от BOPF к RAP представляет собой фундаментальный сдвиг в подходе к созданию бизнес-объектов в SAP:

1. **От конфигурации к коду**: RAP использует ABAP код вместо внешних инструментов конфигурации
2. **Интеграция с CDS**: Нативная поддержка современных возможностей CDS views
3. **Гибкость выбора**: Managed для быстрой разработки, unmanaged для сложных сценариев
4. **Entity Manipulation Language**: Единый язык для работы с бизнес-объектами

Ключевые преимущества RAP:

- Прозрачность и отладка
- Использование современного ABAP
- Готовность к облаку (Steampunk)
- Автоматическая генерация OData сервисов
- Встроенная поддержка draft

RAP является основой для разработки современных приложений в S/4HANA и SAP BTP, обеспечивая единую модель от базы данных до пользовательского интерфейса. В следующей главе мы рассмотрим, как эта модель адаптирована для облачной среды в BTP ABAP Environment.