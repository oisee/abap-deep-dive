# Глава 8: SAP HANA - больше чем база данных

## 8.1. Архитектура in-memory database

SAP HANA представляет собой революционную платформу, которая переосмысливает традиционные подходы к управлению данными. В отличие от классических СУБД, оптимизированных для дискового хранения, HANA изначально спроектирована для работы с данными в оперативной памяти, что обеспечивает радикальное увеличение производительности.

### Многоуровневая архитектура HANA

```mermaid
graph TB
    subgraph "SAP HANA Architecture"
        subgraph "Client Layer"
            APPS[Applications<br/>ABAP, Java, etc.]
            BI[BI Tools]
            STUDIO[HANA Studio/Cockpit]
            WEB[Web Clients]
        end
        
        subgraph "Interface Layer"
            SQL_INT[SQL Interface<br/>JDBC/ODBC/ODBO]
            MDX_INT[MDX Interface]
            REST[REST API]
            ODATA_INT[OData Services]
        end
        
        subgraph "Processing Engines"
            subgraph "Calculation Engine"
                OLAP[OLAP Engine]
                PLAN[Planning Engine]
                CALC[Calc View Processor]
            end
            
            subgraph "SQL Engine"
                PARSER[SQL Parser]
                OPTIMIZER[Query Optimizer]
                EXECUTOR[Execution Engine]
            end
            
            subgraph "Specialized Engines"
                TEXT[Text Analysis]
                GRAPH[Graph Engine]
                SPATIAL[Spatial Engine]
                SERIES[Series Data]
            end
        end
        
        subgraph "Storage Engine"
            ROW_STORE[Row Store]
            COLUMN_STORE[Column Store]
            PERSIST[Persistence Layer]
        end
        
        subgraph "Foundation"
            MEM_MGR[Memory Manager]
            PROC_MGR[Process Manager]
            TRANS_MGR[Transaction Manager]
            AUTH_MGR[Authorization]
        end
        
        APPS --> SQL_INT
        BI --> MDX_INT
        STUDIO --> REST
        
        SQL_INT --> PARSER
        MDX_INT --> OLAP
        
        PARSER --> OPTIMIZER
        OPTIMIZER --> EXECUTOR
        
        EXECUTOR --> ROW_STORE
        EXECUTOR --> COLUMN_STORE
        CALC --> COLUMN_STORE
        
        COLUMN_STORE --> MEM_MGR
        ROW_STORE --> MEM_MGR
        MEM_MGR --> PERSIST
    end
    
    style COLUMN_STORE fill:#4CAF50,stroke:#333,stroke-width:4px
    style MEM_MGR fill:#ff9999,stroke:#333,stroke-width:2px
    style CALC fill:#99ccff,stroke:#333,stroke-width:2px
```

### Процессная модель HANA

HANA использует многопроцессную архитектуру для обеспечения стабильности и производительности:

```mermaid
graph TB
    subgraph "HANA Process Architecture"
        subgraph "Master Process"
            DAEMON[hdbdaemon<br/>System Controller]
            MONITOR[Monitoring]
            RESTART[Auto-restart]
        end
        
        subgraph "Core Services"
            NAME[nameserver<br/>Topology & Metadata]
            INDEX[indexserver<br/>Main Database]
            COMPILE[compileserver<br/>Compile Procedures]
            PREPROC[preprocessor<br/>Text Processing]
        end
        
        subgraph "Optional Services"
            XS[xsengine<br/>Application Server]
            SCRIPT[scriptserver<br/>Python/R Runtime]
            WEB[webdispatcher<br/>Load Balancer]
            DPSERVER[dpserver<br/>Data Provisioning]
        end
        
        subgraph "Shared Memory"
            GLOBAL[Global Allocation]
            HEAP[Process Heap]
            STACK[Process Stack]
            CODE[Code Segment]
        end
        
        DAEMON --> NAME
        DAEMON --> INDEX
        DAEMON --> COMPILE
        
        NAME --> GLOBAL
        INDEX --> GLOBAL
        INDEX --> HEAP
        
        COMPILE --> CODE
        SCRIPT --> HEAP
    end
    
    style DAEMON fill:#ff9999,stroke:#333,stroke-width:4px
    style INDEX fill:#4CAF50,stroke:#333,stroke-width:2px
```

### Управление памятью в HANA

```mermaid
graph LR
    subgraph "HANA Memory Architecture"
        subgraph "Physical Memory"
            RAM[Total RAM<br/>e.g., 1TB]
        end
        
        subgraph "HANA Allocation"
            subgraph "Used Memory"
                CODE_M[Code & Stack<br/>~10GB]
                TABLES[Table Data<br/>Variable]
                TEMP[Temporary<br/>Computations]
                CACHE[Caches &<br/>System Tables]
            end
            
            subgraph "Memory Pools"
                MAIN[Main Store]
                DELTA[Delta Store]
                UNDO[Undo Log]
                TEMP_COMP[Temp Computations]
            end
        end
        
        subgraph "Memory Management"
            ALLOC[Allocators]
            GC[Garbage Collection]
            COMPRESS[Compression]
            UNLOAD[Table Unload]
        end
        
        RAM --> CODE_M
        RAM --> TABLES
        RAM --> TEMP
        
        TABLES --> MAIN
        TABLES --> DELTA
        
        MAIN --> COMPRESS
        DELTA --> GC
        TEMP --> UNLOAD
    end
    
    style RAM fill:#4CAF50,stroke:#333,stroke-width:2px
    style COMPRESS fill:#99ccff,stroke:#333,stroke-width:2px
```

### Persistence Layer

Несмотря на in-memory природу, HANA обеспечивает полную durability через sophisticated persistence layer:

```mermaid
sequenceDiagram
    participant APP as Application
    participant HANA as HANA Engine
    participant MEM as Main Memory
    participant LOG as Log Volumes
    participant DATA as Data Volumes
    participant BACKUP as Backup Storage
    
    Note over APP,BACKUP: Transaction Flow with Persistence
    
    APP->>HANA: INSERT/UPDATE/DELETE
    HANA->>MEM: Write to Delta Store
    HANA->>LOG: Write to Redo Log
    LOG-->>HANA: Acknowledge
    HANA-->>APP: Commit Success
    
    Note over HANA,DATA: Asynchronous Savepoint
    
    loop Every 5 minutes (default)
        HANA->>HANA: Trigger Savepoint
        HANA->>DATA: Write Changed Pages
        DATA-->>HANA: Persisted
        HANA->>LOG: Write Savepoint Log
    end
    
    Note over MEM,DATA: Delta Merge Process
    
    HANA->>MEM: Merge Delta to Main
    MEM->>MEM: Optimize & Compress
    MEM->>DATA: Persist Merged Data
    
    Note over HANA,BACKUP: Backup Process
    
    HANA->>BACKUP: Full Data Backup
    HANA->>BACKUP: Incremental Log Backup
```

## 8.2. Колоночное хранение и сжатие

Колоночное хранение является фундаментальной инновацией HANA, особенно эффективной для аналитических нагрузок.

### Row Store vs Column Store

```mermaid
graph TB
    subgraph "Storage Comparison"
        subgraph "Row Store"
            ROW1[|ID|Name|Dept|Salary|<br/>|1|Alice|IT|5000|]
            ROW2[|2|Bob|HR|4500|]
            ROW3[|3|Carol|IT|5500|]
            ROW_USE[✓ OLTP workloads<br/>✓ Full row access<br/>✗ Analytics<br/>✗ Compression]
        end
        
        subgraph "Column Store"
            COL_ID[ID<br/>1<br/>2<br/>3]
            COL_NAME[Name<br/>Alice<br/>Bob<br/>Carol]
            COL_DEPT[Dept<br/>IT<br/>HR<br/>IT]
            COL_SAL[Salary<br/>5000<br/>4500<br/>5500]
            COL_USE[✗ Single row access<br/>✓ Analytics<br/>✓ Compression<br/>✓ Parallel processing]
        end
        
        subgraph "Query: SELECT AVG(Salary) WHERE Dept='IT'"
            ROW_SCAN[Row: Scan all rows<br/>Read unnecessary data]
            COL_SCAN[Column: Scan only<br/>Dept and Salary columns]
        end
        
        ROW1 --> ROW_SCAN
        ROW2 --> ROW_SCAN
        ROW3 --> ROW_SCAN
        
        COL_DEPT --> COL_SCAN
        COL_SAL --> COL_SCAN
    end
    
    style COL_SCAN fill:#4CAF50,stroke:#333,stroke-width:2px
    style ROW_SCAN fill:#ff9999,stroke:#333,stroke-width:2px
```

### Механизмы сжатия в Column Store

```mermaid
graph TB
    subgraph "HANA Compression Techniques"
        subgraph "Dictionary Encoding"
            RAW_DEPT[Raw Values:<br/>IT<br/>HR<br/>IT<br/>IT<br/>HR<br/>Sales<br/>IT]
            DICT[Dictionary:<br/>0: HR<br/>1: IT<br/>2: Sales]
            ENCODED[Encoded:<br/>1<br/>0<br/>1<br/>1<br/>0<br/>2<br/>1]
            
            RAW_DEPT --> DICT
            DICT --> ENCODED
        end
        
        subgraph "Run Length Encoding"
            RAW_STATUS[Status:<br/>Active<br/>Active<br/>Active<br/>Active<br/>Inactive]
            RLE[RLE:<br/>(Active, 4)<br/>(Inactive, 1)]
            
            RAW_STATUS --> RLE
        end
        
        subgraph "Bit-Vector Encoding"
            VALUES[Values:<br/>1<br/>5<br/>1<br/>3<br/>1]
            BITVEC[BitVectors:<br/>1: 10101<br/>3: 00010<br/>5: 01000]
            
            VALUES --> BITVEC
        end
        
        subgraph "Advanced"
            PREFIX[Prefix Encoding]
            CLUSTER[Cluster Encoding]
            INDIRECT[Indirect Encoding]
        end
    end
    
    style DICT fill:#4CAF50,stroke:#333,stroke-width:2px
    style RLE fill:#99ccff,stroke:#333,stroke-width:2px
```

### Delta Store и Main Store

```mermaid
graph TB
    subgraph "Column Store Architecture"
        subgraph "Write Operations"
            INSERT[INSERT]
            UPDATE[UPDATE]
            DELETE[DELETE]
        end
        
        subgraph "Delta Store"
            DELTA_TAB[Uncompressed<br/>Row Format<br/>Write-Optimized]
            DELTA_LOG[Delta Log]
        end
        
        subgraph "Main Store"
            MAIN_TAB[Compressed<br/>Column Format<br/>Read-Optimized]
            DICT_M[Dictionary]
            COMP_DATA[Compressed Data]
        end
        
        subgraph "Read Operations"
            QUERY[SELECT Query]
            MERGE_VIEW[Unified View<br/>Main + Delta]
        end
        
        subgraph "Delta Merge Process"
            TRIGGER[Merge Trigger<br/>- Size threshold<br/>- Time interval<br/>- Manual]
            MERGE_PROC[Merge Process<br/>- Sort data<br/>- Compress<br/>- Update dictionary]
        end
        
        INSERT --> DELTA_TAB
        UPDATE --> DELTA_TAB
        DELETE --> DELTA_LOG
        
        QUERY --> MERGE_VIEW
        MERGE_VIEW --> MAIN_TAB
        MERGE_VIEW --> DELTA_TAB
        
        DELTA_TAB --> TRIGGER
        TRIGGER --> MERGE_PROC
        MERGE_PROC --> MAIN_TAB
    end
    
    style DELTA_TAB fill:#ffff99,stroke:#333,stroke-width:2px
    style MAIN_TAB fill:#99ff99,stroke:#333,stroke-width:2px
    style MERGE_PROC fill:#99ccff,stroke:#333,stroke-width:2px
```

### Эффективность сжатия

```mermaid
graph LR
    subgraph "Compression Efficiency Example"
        subgraph "Original Data"
            ORIG[Table: 1M rows<br/>Column: Country<br/>Size: 20 bytes/value<br/>Total: 20 MB]
        end
        
        subgraph "After Dictionary"
            DICT_C[Dictionary:<br/>200 countries<br/>200 × 20 = 4KB<br/>+<br/>Value IDs:<br/>1M × 1 byte = 1MB<br/>Total: ~1 MB]
        end
        
        subgraph "Compression Ratio"
            RATIO[20:1<br/>95% reduction]
        end
        
        subgraph "Benefits"
            MEM_SAVE[Memory Savings]
            CPU_CACHE[Better CPU Cache Usage]
            SCAN_SPEED[Faster Scans]
            BANDWIDTH[Less Memory Bandwidth]
        end
        
        ORIG --> DICT_C
        DICT_C --> RATIO
        
        RATIO --> MEM_SAVE
        RATIO --> CPU_CACHE
        RATIO --> SCAN_SPEED
        RATIO --> BANDWIDTH
    end
    
    style RATIO fill:#4CAF50,stroke:#333,stroke-width:4px
```

## 8.3. Вычислительный движок и SQLScript

### Calculation Engine Architecture

```mermaid
graph TB
    subgraph "HANA Calculation Engine"
        subgraph "View Types"
            ATTR[Attribute Views<br/>Master Data]
            ANAL[Analytic Views<br/>Facts + Dimensions]
            CALC[Calculation Views<br/>Complex Logic]
        end
        
        subgraph "Calc Engine Components"
            PARSER_CE[CE Parser]
            OPTIMIZER_CE[CE Optimizer]
            subgraph "Operators"
                JOIN_OP[Join Operator]
                AGGR_OP[Aggregation Op]
                PROJ_OP[Projection Op]
                CALC_OP[Calculation Op]
                UNION_OP[Union Operator]
            end
            EXEC_CE[CE Executor]
        end
        
        subgraph "Optimization"
            PUSH_DOWN[Filter Pushdown]
            JOIN_OPT[Join Optimization]
            PARALLEL[Parallelization]
            CACHE_CE[Result Cache]
        end
        
        subgraph "Integration"
            SQL_ENG[SQL Engine]
            OLAP_ENG[OLAP Engine]
        end
        
        CALC --> PARSER_CE
        PARSER_CE --> OPTIMIZER_CE
        
        OPTIMIZER_CE --> JOIN_OP
        OPTIMIZER_CE --> AGGR_OP
        OPTIMIZER_CE --> CALC_OP
        
        OPTIMIZER_CE --> PUSH_DOWN
        PUSH_DOWN --> SQL_ENG
        
        EXEC_CE --> PARALLEL
        PARALLEL --> CACHE_CE
    end
    
    style OPTIMIZER_CE fill:#ff9999,stroke:#333,stroke-width:2px
    style PARALLEL fill:#4CAF50,stroke:#333,stroke-width:2px
```

### SQLScript - процедурный язык HANA

SQLScript расширяет стандартный SQL процедурными возможностями, оптимизированными для in-memory обработки:

```sql
-- Пример SQLScript процедуры
CREATE PROCEDURE calculate_customer_value (
    IN ip_date DATE,
    OUT ot_results TABLE (
        customer_id NVARCHAR(10),
        total_orders DECIMAL(15,2),
        avg_order_value DECIMAL(15,2),
        customer_score INTEGER
    )
)
LANGUAGE SQLSCRIPT
SQL SECURITY INVOKER
READS SQL DATA AS
BEGIN
    -- Декларативная часть: использование CE функций
    lt_orders = CE_CALC_VIEW("_SYS_BIC"."sap.demo/OrdersView", 
                            ["CUSTOMER_ID", "ORDER_VALUE", "ORDER_DATE"]);
    
    -- Фильтрация с pushdown
    lt_filtered = APPLY_FILTER(:lt_orders, 
                              "ORDER_DATE" >= ADD_MONTHS(:ip_date, -12));
    
    -- Императивная логика
    DECLARE lv_threshold DECIMAL(15,2) := 10000;
    DECLARE CURSOR c_customers FOR
        SELECT DISTINCT customer_id FROM :lt_filtered;
    
    -- Table variable для результатов
    DECLARE lt_temp TABLE LIKE :ot_results;
    
    -- Параллельная обработка через PARALLEL EXECUTION
    FOR cur_row AS c_customers DO
        PARALLEL EXECUTION
        lt_customer_orders = SELECT * FROM :lt_filtered 
                            WHERE customer_id = cur_row.customer_id;
        
        INSERT INTO :lt_temp
        SELECT customer_id,
               SUM(order_value) as total_orders,
               AVG(order_value) as avg_order_value,
               CASE 
                   WHEN SUM(order_value) > :lv_threshold * 2 THEN 100
                   WHEN SUM(order_value) > :lv_threshold THEN 75
                   ELSE 50
               END as customer_score
        FROM :lt_customer_orders
        GROUP BY customer_id;
    END FOR;
    
    -- Возврат результата
    ot_results = SELECT * FROM :lt_temp;
END;
```

### Оптимизация выполнения SQLScript

```mermaid
sequenceDiagram
    participant Client
    participant Parser as SQLScript Parser
    participant Optimizer as Plan Optimizer
    participant CE as Calc Engine
    participant SQL as SQL Engine
    participant COL as Column Store
    
    Client->>Parser: Call Procedure
    Parser->>Parser: Parse SQLScript
    Parser->>Optimizer: Generate Plan Graph
    
    Optimizer->>Optimizer: Identify Declarative Parts
    Optimizer->>Optimizer: Detect Parallelization
    Optimizer->>Optimizer: Plan Optimization
    
    Note over Optimizer,SQL: Execution Plan
    
    Optimizer->>CE: Push Calc Operations
    Optimizer->>SQL: Push SQL Operations
    
    par Parallel Execution
        CE->>COL: Execute Calculations
        SQL->>COL: Execute Queries
    end
    
    COL-->>CE: Results
    COL-->>SQL: Results
    
    CE-->>Optimizer: Intermediate Results
    SQL-->>Optimizer: Intermediate Results
    
    Optimizer->>Optimizer: Combine Results
    Optimizer-->>Client: Final Result
```

### Векторизация и SIMD

```mermaid
graph TB
    subgraph "HANA Vectorization"
        subgraph "Traditional Processing"
            SCALAR[Scalar Operations<br/>One value at a time]
            LOOP1[for each row:<br/>  result = a + b]
        end
        
        subgraph "Vectorized Processing"
            VECTOR[Vector Operations<br/>Multiple values]
            SIMD[SIMD Instructions<br/>AVX2/AVX512]
            BATCH[Process 8-16 values<br/>simultaneously]
        end
        
        subgraph "Example: SUM(column)"
            subgraph "Scalar"
                S1[Load value 1]
                S2[Add to sum]
                S3[Load value 2]
                S4[Add to sum]
                S_MORE[...]
            end
            
            subgraph "Vector"
                V1[Load 8 values]
                V2[Vector ADD]
                V3[Accumulate]
                V_REPEAT[Repeat for next 8]
            end
        end
        
        S1 --> S2
        S2 --> S3
        S3 --> S4
        
        V1 --> V2
        V2 --> V3
        V3 --> V_REPEAT
    end
    
    style VECTOR fill:#4CAF50,stroke:#333,stroke-width:2px
    style SCALAR fill:#ff9999,stroke:#333,stroke-width:2px
```

## 8.4. Интеграция с ABAP: CDS и AMDP

### CDS Views в контексте HANA

Core Data Services (CDS) в контексте HANA обеспечивают семантически богатое моделирование данных с автоматической оптимизацией для in-memory обработки:

```mermaid
graph TB
    subgraph "CDS on HANA Architecture"
        subgraph "ABAP Layer"
            CDS_DDL[CDS Definition<br/>@Analytics.dataCategory]
            ANNO[CDS Annotations<br/>@ObjectModel<br/>@Analytics<br/>@Performance]
        end
        
        subgraph "Generation"
            CDS_PROC[CDS Processor]
            SQL_GEN[SQL View Generator]
            HANA_OPT[HANA Optimizer Hints]
        end
        
        subgraph "HANA Layer"
            DB_VIEW[Database View]
            CALC_SCENARIO[Calculation Scenario]
            COL_STORE_OPT[Column Store<br/>Optimizations]
        end
        
        subgraph "Runtime"
            QUERY_PROC[Query Processor]
            CACHE_CTRL[Cache Controller]
            STATS[Statistics]
        end
        
        CDS_DDL --> CDS_PROC
        ANNO --> CDS_PROC
        
        CDS_PROC --> SQL_GEN
        CDS_PROC --> HANA_OPT
        
        SQL_GEN --> DB_VIEW
        HANA_OPT --> CALC_SCENARIO
        
        DB_VIEW --> COL_STORE_OPT
        CALC_SCENARIO --> COL_STORE_OPT
        
        QUERY_PROC --> COL_STORE_OPT
        COL_STORE_OPT --> CACHE_CTRL
        COL_STORE_OPT --> STATS
    end
    
    style CDS_PROC fill:#ff9999,stroke:#333,stroke-width:2px
    style COL_STORE_OPT fill:#4CAF50,stroke:#333,stroke-width:2px
```

### AMDP - прямой доступ к мощи HANA

ABAP Managed Database Procedures позволяют писать SQLScript прямо в ABAP классах:

```abap
CLASS zcl_amdp_example DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES: if_amdp_marker_hdb.
    
    TYPES: BEGIN OF ty_customer_analytics,
             customer_id TYPE kunnr,
             total_revenue TYPE wrbtr,
             order_count TYPE i,
             avg_order_value TYPE wrbtr,
             days_since_last_order TYPE i,
             churn_probability TYPE decfloat34,
           END OF ty_customer_analytics.
    
    TYPES: tt_customer_analytics TYPE TABLE OF ty_customer_analytics.
    
    CLASS-METHODS: analyze_customers
      IMPORTING
        VALUE(iv_date_from) TYPE dats
        VALUE(iv_date_to) TYPE dats
      EXPORTING
        VALUE(et_analytics) TYPE tt_customer_analytics.
        
ENDCLASS.

CLASS zcl_amdp_example IMPLEMENTATION.
  METHOD analyze_customers BY DATABASE PROCEDURE
                          FOR HDB
                          LANGUAGE SQLSCRIPT
                          OPTIONS READ-ONLY
                          USING vbak vbap kna1.
    
    -- Использование HANA-специфичных функций
    DECLARE lv_current_date DATE;
    lv_current_date = CURRENT_DATE;
    
    -- Window functions для аналитики
    lt_customer_orders = 
      SELECT 
        h.kunnr as customer_id,
        h.vbeln,
        h.erdat as order_date,
        SUM(i.netwr) as order_value,
        -- Window function для последнего заказа
        MAX(h.erdat) OVER (PARTITION BY h.kunnr) as last_order_date,
        -- Running total
        SUM(SUM(i.netwr)) OVER (
          PARTITION BY h.kunnr 
          ORDER BY h.erdat 
          ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW
        ) as running_total
      FROM vbak as h
      INNER JOIN vbap as i ON h.vbeln = i.vbeln
      WHERE h.erdat BETWEEN :iv_date_from AND :iv_date_to
      GROUP BY h.kunnr, h.vbeln, h.erdat;
    
    -- Использование HANA ML функций
    lt_churn_prediction = 
      SELECT 
        customer_id,
        -- Predictive функция (simplified)
        CASE 
          WHEN days_since_last > 180 THEN 
            0.8 + (days_since_last - 180) * 0.001
          WHEN days_since_last > 90 THEN
            0.5 + (days_since_last - 90) * 0.003
          ELSE
            days_since_last * 0.005
        END as churn_probability
      FROM (
        SELECT 
          customer_id,
          DAYS_BETWEEN(MAX(order_date), :lv_current_date) as days_since_last
        FROM :lt_customer_orders
        GROUP BY customer_id
      );
    
    -- Финальная агрегация с использованием параллелизма
    et_analytics = 
      SELECT 
        co.customer_id,
        SUM(co.order_value) as total_revenue,
        COUNT(DISTINCT co.vbeln) as order_count,
        AVG(co.order_value) as avg_order_value,
        DAYS_BETWEEN(MAX(co.order_date), :lv_current_date) as days_since_last_order,
        COALESCE(cp.churn_probability, 0) as churn_probability
      FROM :lt_customer_orders as co
      LEFT JOIN :lt_churn_prediction as cp
        ON co.customer_id = cp.customer_id
      GROUP BY co.customer_id, cp.churn_probability
      ORDER BY total_revenue DESC;
      
  ENDMETHOD.
ENDCLASS.
```

### Оптимизация ABAP для HANA

```mermaid
graph TB
    subgraph "HANA Optimization Patterns"
        subgraph "Code Pushdown"
            OLD_LOOP[❌ LOOP AT itab<br/>SELECT SINGLE]
            NEW_JOIN[✓ JOIN in SELECT<br/>or CDS View]
            
            OLD_CALC[❌ Calculate in ABAP]
            NEW_CALC[✓ Calculate in DB<br/>CDS or AMDP]
        end
        
        subgraph "Data Volume"
            OLD_ALL[❌ SELECT *]
            NEW_PROJ[✓ SELECT only<br/>needed fields]
            
            OLD_CLIENT[❌ Check client<br/>in ABAP]
            NEW_WHERE[✓ WHERE client<br/>in SELECT]
        end
        
        subgraph "Aggregation"
            OLD_AGGR[❌ SUM in LOOP]
            NEW_AGGR[✓ SUM in SELECT<br/>GROUP BY]
            
            OLD_SORT[❌ SORT itab]
            NEW_ORDER[✓ ORDER BY<br/>in SELECT]
        end
        
        subgraph "Modern ABAP"
            INLINE[Inline Declarations]
            STRING_EXPR[String Expressions]
            NEW_SQL[New Open SQL]
            CDS_USE[CDS Views]
        end
    end
    
    style NEW_JOIN fill:#4CAF50,stroke:#333,stroke-width:2px
    style OLD_LOOP fill:#ff9999,stroke:#333,stroke-width:2px
    style CDS_USE fill:#99ccff,stroke:#333,stroke-width:2px
```

### Мониторинг производительности HANA

```mermaid
graph LR
    subgraph "HANA Performance Monitoring"
        subgraph "System Views"
            M_CS[M_CS_TABLES<br/>Column Store Stats]
            M_RS[M_RS_TABLES<br/>Row Store Stats]
            M_HEAP[M_HEAP_MEMORY<br/>Memory Usage]
            M_SQL[M_SQL_PLAN_CACHE<br/>SQL Plans]
        end
        
        subgraph "Tools"
            HANA_COCKPIT[HANA Cockpit]
            STUDIO_PERF[Studio Performance<br/>Analysis]
            PLAN_VIZ[Plan Visualizer]
            SQL_ANALYZER[SQL Analyzer]
        end
        
        subgraph "ABAP Integration"
            ST05_HANA[ST05 with<br/>HANA Trace]
            SQLM[SQLM - SQL<br/>Monitor]
            ATC[ATC HANA<br/>Checks]
            SWLT[SQL Performance<br/>Tuning Worklist]
        end
        
        M_CS --> HANA_COCKPIT
        M_SQL --> PLAN_VIZ
        
        PLAN_VIZ --> ST05_HANA
        SQL_ANALYZER --> SQLM
    end
    
    style HANA_COCKPIT fill:#4CAF50,stroke:#333,stroke-width:2px
    style PLAN_VIZ fill:#99ccff,stroke:#333,stroke-width:2px
```

### Best Practices для ABAP на HANA

```mermaid
mindmap
  root((ABAP on HANA<br/>Best Practices))
    Code Pushdown
      Use CDS Views
      Implement AMDP
      FOR ALL ENTRIES → JOIN
      Calculations in DB
    Data Model
      Avoid Redundancy
      Use HANA Views
      Leverage Associations
      Optimize Keys
    Performance
      Minimize Data Transfer
      Use Field List
      Avoid SELECT *
      Batch Operations
    Modern ABAP
      Inline Declarations
      String Templates
      NEW Constructor
      CORRESPONDING
    Monitoring
      Regular Plan Analysis
      Check Hot Tables
      Monitor Memory
      SQL Performance Trace
```

## Заключение

SAP HANA представляет собой не просто базу данных, а комплексную платформу для обработки данных, которая фундаментально меняет подход к разработке корпоративных приложений:

1. **In-Memory Architecture** обеспечивает производительность, недостижимую для традиционных СУБД
2. **Column Store** с продвинутым сжатием позволяет обрабатывать огромные объемы данных
3. **Calculation Engine** переносит вычисления к данным, минимизируя движение данных
4. **SQLScript и AMDP** дают разработчикам прямой доступ к мощи платформы
5. **Тесная интеграция с ABAP** через CDS и современные конструкции языка

Ключевые преимущества для разработчиков:
- Радикальное упрощение кода за счет переноса логики в БД
- Реальная аналитика в реальном времени
- Новые возможности, недоступные в традиционных БД

Понимание архитектуры HANA критически важно для создания современных, высокопроизводительных приложений в экосистеме SAP. В следующей главе мы рассмотрим, как эти возможности используются в современных фреймворках для создания REST API и веб-сервисов.