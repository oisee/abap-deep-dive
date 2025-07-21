# Глава 7: От R/2 до S/4HANA - архитектурные революции

## 7.1. R/2 и эпоха мейнфреймов

SAP R/2, выпущенная в 1979 году, представляла собой революционную для своего времени систему управления предприятием, работающую на мейнфреймах IBM, Siemens и других производителей. Понимание архитектуры R/2 важно не только с исторической точки зрения — многие концептуальные решения той эпохи до сих пор влияют на современные системы SAP.

### Архитектура R/2

```mermaid
graph TB
    subgraph "R/2 Mainframe Architecture"
        subgraph "Terminal Layer"
            T1[3270 Terminal]
            T2[3270 Terminal]
            T3[3270 Terminal]
            TN[...Terminal N]
        end
        
        subgraph "Mainframe System"
            subgraph "Operating System"
                MVS[MVS/ESA<br/>or VM/CMS]
                CICS[CICS/IMS<br/>Transaction Monitor]
            end
            
            subgraph "R/2 Components"
                DIALOG[Dialog Control]
                ABAP4[ABAP/4<br/>Interpreter]
                DBMS[Database<br/>IMS/DB2/VSAM]
                SPOOL[Spool System]
            end
            
            subgraph "Memory Regions"
                CSA[Common System Area]
                PRIVATE[Private Areas]
                BUFFER[Buffer Pools]
            end
        end
        
        subgraph "Storage"
            DASD[DASD<br/>Direct Access Storage]
            TAPE[Tape Storage]
        end
        
        T1 --> CICS
        T2 --> CICS
        T3 --> CICS
        TN --> CICS
        
        CICS --> DIALOG
        DIALOG --> ABAP4
        ABAP4 --> DBMS
        DBMS --> BUFFER
        BUFFER --> DASD
        
        MVS --> CSA
        MVS --> PRIVATE
    end
    
    style ABAP4 fill:#ff9999,stroke:#333,stroke-width:4px
    style CICS fill:#99ccff,stroke:#333,stroke-width:2px
```

### Ключевые характеристики R/2

**Монолитная архитектура**: Все компоненты выполнялись в едином адресном пространстве мейнфрейма. Это обеспечивало высокую производительность за счет отсутствия сетевых задержек, но ограничивало масштабируемость.

**Терминальная модель**: Пользователи работали через "глупые" терминалы 3270, которые могли отображать только текст в формате 24×80 символов. Вся логика выполнялась на мейнфрейме.

**ABAP/4**: Ранняя версия языка ABAP была процедурной и оптимизированной для batch-обработки больших объемов данных.

### Управление транзакциями в R/2

```mermaid
sequenceDiagram
    participant Terminal as 3270 Terminal
    participant CICS as CICS/TP Monitor
    participant DC as Dialog Control
    participant ABAP as ABAP/4
    participant DB as IMS/DB2
    
    Terminal->>CICS: Transaction Code
    CICS->>CICS: Allocate Resources
    CICS->>DC: Start Dialog
    
    DC->>ABAP: Load Program
    ABAP->>DB: Read Data
    DB-->>ABAP: Data
    
    ABAP->>ABAP: Process
    ABAP->>Terminal: Send Screen
    
    Terminal->>CICS: User Input
    CICS->>DC: Continue Dialog
    DC->>ABAP: Process Input
    
    ABAP->>DB: Update Data
    DB-->>ABAP: Confirmation
    
    ABAP->>CICS: Commit
    CICS->>DB: Two-Phase Commit
    DB-->>CICS: Committed
    
    CICS->>Terminal: Next Screen
```

### Ограничения R/2

```mermaid
graph TB
    subgraph "R/2 Limitations"
        subgraph "Hardware Dependency"
            VENDOR[Vendor Lock-in<br/>IBM, Siemens, etc.]
            COST[High Cost<br/>Mainframe pricing]
            SCALE[Vertical Scaling<br/>Only]
        end
        
        subgraph "User Interface"
            TEXT[Text-only<br/>24x80 screens]
            RIGID[Rigid Layout]
            NOGUII[No Graphics]
        end
        
        subgraph "Architecture"
            MONO[Monolithic<br/>Design]
            CENTRAL[Centralized<br/>Processing]
            BATCH[Batch-oriented]
        end
        
        subgraph "Development"
            PROC[Procedural<br/>Programming]
            LIMITED[Limited<br/>Modularity]
            TOOLS[Basic<br/>Dev Tools]
        end
    end
    
    style VENDOR fill:#ff9999,stroke:#333,stroke-width:2px
    style MONO fill:#ffcccc,stroke:#333,stroke-width:2px
```

## 7.2. R/3 и трёхуровневая революция

В 1992 году SAP выпустила R/3 — систему, которая кардинально изменила ландшафт корпоративного ПО. Переход от мейнфреймов к клиент-серверной архитектуре был не просто технологическим обновлением, а фундаментальной сменой парадигмы.

### Драйверы перехода к R/3

```mermaid
graph LR
    subgraph "Market Forces 1990s"
        subgraph "Technology Trends"
            UNIX[UNIX Servers<br/>Cost-effective]
            PC[PC Revolution<br/>Powerful clients]
            LAN[LAN/WAN<br/>Networks]
            RDBMS[Relational<br/>Databases]
        end
        
        subgraph "Business Needs"
            COST_R[Cost Reduction]
            FLEX[Flexibility]
            SCALE_H[Horizontal<br/>Scalability]
            OPEN[Open Systems]
        end
        
        subgraph "Limitations of R/2"
            MAINFRAME[Expensive<br/>Mainframes]
            CLOSED[Proprietary<br/>Systems]
            UI[Poor UI]
        end
        
        UNIX --> DECISION[Decision:<br/>Build R/3]
        PC --> DECISION
        LAN --> DECISION
        
        COST_R --> DECISION
        FLEX --> DECISION
        
        MAINFRAME --> DECISION
        CLOSED --> DECISION
        
        DECISION --> R3[R/3<br/>Client-Server]
    end
    
    style DECISION fill:#4CAF50,stroke:#333,stroke-width:4px
    style R3 fill:#99ff99,stroke:#333,stroke-width:2px
```

### Революционная трёхуровневая архитектура

```mermaid
graph TB
    subgraph "R/3 Three-Tier Architecture"
        subgraph "Presentation Tier"
            SAPGUI[SAP GUI<br/>Windows/Java/HTML]
            LOCAL[Local Processing<br/>Screen Rendering]
            DIAG_PROT[DIAG Protocol<br/>Optimized]
        end
        
        subgraph "Application Tier"
            subgraph "Application Server 1"
                DISP1[Dispatcher]
                WP_D1[Dialog WPs]
                WP_B1[Background WPs]
                WP_U1[Update WPs]
            end
            
            subgraph "Application Server N"
                DISP2[Dispatcher]
                WP_D2[Dialog WPs]
                WP_B2[Background WPs]
            end
            
            subgraph "Central Services"
                MS[Message Server]
                ENQ[Enqueue Server]
            end
        end
        
        subgraph "Database Tier"
            RDBMS[Relational Database<br/>Oracle/DB2/Informix/SQL Server]
            DATA[Application Data]
            REPO[ABAP Repository]
        end
        
        SAPGUI --> DISP1
        SAPGUI --> DISP2
        
        DISP1 --> WP_D1
        DISP1 --> WP_B1
        DISP1 --> WP_U1
        
        WP_D1 --> RDBMS
        WP_B1 --> RDBMS
        
        MS <--> DISP1
        MS <--> DISP2
        
        ENQ <--> WP_D1
        ENQ <--> WP_D2
    end
    
    style MS fill:#ff9999,stroke:#333,stroke-width:4px
    style DISP1 fill:#99ccff,stroke:#333,stroke-width:2px
```

### Инновации R/3

**SAP GUI и DIAG протокол**: Графический интерфейс революционизировал пользовательский опыт, при этом DIAG протокол минимизировал сетевой трафик, передавая только изменения экрана.

**Work Process концепция**: Введение специализированных процессов (Dialog, Background, Update, Spool) позволило оптимизировать обработку различных типов задач.

**ABAP Objects**: С версии 4.6 ABAP получил объектно-ориентированные возможности, что кардинально расширило возможности разработки.

### Масштабируемость R/3

```mermaid
graph LR
    subgraph "R/3 Scalability Model"
        subgraph "Vertical Scaling"
            V_CPU[Add CPUs]
            V_MEM[Add Memory]
            V_LIMIT[Hardware<br/>Limits]
        end
        
        subgraph "Horizontal Scaling"
            H_APP[Add App<br/>Servers]
            H_LOAD[Load<br/>Balancing]
            H_UNLIMITED[Theoretically<br/>Unlimited]
        end
        
        subgraph "Database Challenge"
            DB_SINGLE[Single DB<br/>Instance]
            DB_BOTTLE[Potential<br/>Bottleneck]
            DB_SCALE[DB Clustering<br/>Complex]
        end
        
        V_CPU --> V_LIMIT
        V_MEM --> V_LIMIT
        
        H_APP --> H_UNLIMITED
        H_LOAD --> H_UNLIMITED
        
        H_UNLIMITED --> DB_SINGLE
        DB_SINGLE --> DB_BOTTLE
        DB_BOTTLE --> DB_SCALE
    end
    
    style H_UNLIMITED fill:#99ff99,stroke:#333,stroke-width:2px
    style DB_BOTTLE fill:#ff9999,stroke:#333,stroke-width:2px
```

## 7.3. NetWeaver и интеграция Java

В 2004 году SAP представила NetWeaver — технологическую платформу, которая должна была объединить ABAP и Java миры. Это была амбициозная попытка создать универсальную платформу для всех типов приложений.

### Архитектура NetWeaver Dual-Stack

```mermaid
graph TB
    subgraph "NetWeaver Dual-Stack Architecture"
        subgraph "Common Layer"
            ICM[Internet Communication Manager]
            MS[Message Server]
            ENQ[Enqueue Server]
            CS[Central Services]
        end
        
        subgraph "ABAP Stack"
            DISP_A[ABAP Dispatcher]
            WP[Work Processes]
            ABAP_VM[ABAP VM]
            ABAP_APPS[ABAP Applications]
        end
        
        subgraph "Java Stack"
            DISP_J[Java Dispatcher]
            SERVER[Server Nodes]
            JVM[Java VMs]
            J2EE_APPS[J2EE Applications]
        end
        
        subgraph "Shared Components"
            DB[(Shared Database)]
            FS[Shared File System]
            UME[User Management Engine]
        end
        
        subgraph "Integration Layer"
            JCO[Java Connector]
            RFC[RFC Protocol]
            XI[Exchange Infrastructure]
        end
        
        ICM --> DISP_A
        ICM --> DISP_J
        
        DISP_A --> WP
        WP --> ABAP_VM
        
        DISP_J --> SERVER
        SERVER --> JVM
        
        ABAP_APPS <--> JCO
        JCO <--> J2EE_APPS
        
        WP --> DB
        JVM --> DB
        
        ABAP_VM --> UME
        JVM --> UME
    end
    
    style ICM fill:#4CAF50,stroke:#333,stroke-width:4px
    style JCO fill:#ff9999,stroke:#333,stroke-width:2px
```

### Проблемы Dual-Stack подхода

```mermaid
graph TB
    subgraph "Dual-Stack Challenges"
        subgraph "Operational Complexity"
            ADMIN[Double Administration<br/>ABAP + Java]
            PATCH[Complex Patching<br/>Dependencies]
            MONITOR[Separate Monitoring<br/>Tools]
        end
        
        subgraph "Resource Overhead"
            MEMORY[High Memory<br/>Consumption]
            CPU[CPU Overhead<br/>Two Runtimes]
            STARTUP[Long Startup<br/>Times]
        end
        
        subgraph "Integration Issues"
            PERF[JCo Performance<br/>Overhead]
            TRANS[Transaction<br/>Handling]
            DEBUG[Complex<br/>Debugging]
        end
        
        subgraph "Development"
            SKILLS[Two Skill Sets<br/>Required]
            TOOLS[Different<br/>Tool Chains]
            DEPLOY[Complex<br/>Deployment]
        end
    end
    
    style ADMIN fill:#ff9999,stroke:#333,stroke-width:2px
    style MEMORY fill:#ffcccc,stroke:#333,stroke-width:2px
    style PERF fill:#ff9999,stroke:#333,stroke-width:2px
```

### Эволюция к раздельным стекам

```mermaid
timeline
    title NetWeaver Evolution
    
    2004 : NetWeaver 04
         : First dual-stack
         : J2EE 1.3
         : Web AS 6.40
    
    2007 : NetWeaver 7.0
         : Mature dual-stack
         : Portal, PI, BI
         : Java performance issues
    
    2011 : NetWeaver 7.3
         : Last dual-stack
         : Separate recommended
         : ABAP and Java split
    
    2013 : NetWeaver 7.4
         : ABAP-only focus
         : Java deprecated
         : HANA optimization
    
    2015 : NetWeaver 7.5
         : S/4HANA foundation
         : Pure ABAP stack
         : Fiori/Gateway
```

### Уроки NetWeaver эры

**Технологический долг**: Попытка объединить две принципиально разные платформы привела к значительному усложнению архитектуры.

**Производительность**: JCo (Java Connector) создавал дополнительный overhead при коммуникации между стеками.

**Эволюция рынка**: Появление легковесных микросервисов и REST API сделало тяжеловесную J2EE архитектуру устаревшей.

## 7.4. S/4HANA и in-memory парадигма

S/4HANA, анонсированная в 2015 году, представляет собой не просто обновление ERP-системы, а фундаментальную переархитектуру, использующую возможности in-memory вычислений SAP HANA.

### Архитектурная трансформация

```mermaid
graph TB
    subgraph "S/4HANA Architecture Transformation"
        subgraph "Traditional ERP (ECC)"
            APP_OLD[Application Server<br/>Complex Logic]
            AGGR[Aggregation Tables]
            INDEX[Secondary Indexes]
            BATCH[Batch Jobs]
            CACHE[Application Cache]
            DB_OLD[(Traditional RDBMS<br/>Disk-based)]
        end
        
        subgraph "S/4HANA"
            APP_NEW[Application Server<br/>Orchestration]
            CDS[CDS Views]
            AMDP[AMDP Procedures]
            REALTIME[Real-time Processing]
            PUSH[Code Pushdown]
            HANA[(SAP HANA<br/>In-Memory)]
        end
        
        subgraph "Eliminated Redundancy"
            NO_AGGR[❌ Aggregation Tables]
            NO_INDEX[❌ Most Indexes]
            NO_BATCH[❌ Many Batch Jobs]
            NO_CACHE[❌ Complex Caching]
        end
        
        APP_OLD --> DB_OLD
        AGGR --> DB_OLD
        INDEX --> DB_OLD
        
        APP_NEW --> HANA
        CDS --> PUSH
        AMDP --> PUSH
        PUSH --> HANA
        
        AGGR --> NO_AGGR
        INDEX --> NO_INDEX
        BATCH --> NO_BATCH
        CACHE --> NO_CACHE
    end
    
    style HANA fill:#4CAF50,stroke:#333,stroke-width:4px
    style PUSH fill:#99ff99,stroke:#333,stroke-width:2px
    style NO_AGGR fill:#ffcccc,stroke:#333,stroke-width:2px
```

### Упрощение данных модели

```mermaid
graph LR
    subgraph "Data Model Simplification"
        subgraph "ECC Model"
            BKPF[BKPF<br/>Header]
            BSEG[BSEG<br/>Line Items]
            BSIS[BSIS<br/>Open Items GL]
            BSAS[BSAS<br/>Cleared Items GL]
            BSIK[BSIK<br/>Open Items Vendor]
            BSAK[BSAK<br/>Cleared Items Vendor]
            MORE[...12 more tables]
        end
        
        subgraph "S/4HANA Model"
            ACDOCA[ACDOCA<br/>Universal Journal]
            BKPF_S4[BKPF<br/>Compatibility View]
            BSEG_S4[BSEG<br/>Compatibility View]
        end
        
        BKPF --> ACDOCA
        BSEG --> ACDOCA
        BSIS --> ACDOCA
        BSAS --> ACDOCA
        BSIK --> ACDOCA
        BSAK --> ACDOCA
        MORE --> ACDOCA
        
        ACDOCA --> BKPF_S4
        ACDOCA --> BSEG_S4
    end
    
    style ACDOCA fill:#4CAF50,stroke:#333,stroke-width:4px
```

### Code-to-Data парадигма в действии

```mermaid
sequenceDiagram
    participant User
    participant Fiori as Fiori App
    participant AS as App Server
    participant CDS as CDS View
    participant HANA as HANA DB
    participant CE as Calculation Engine
    
    User->>Fiori: Request Analytics
    Fiori->>AS: OData Call
    AS->>CDS: Execute View
    
    Note over CDS,HANA: Code Pushdown
    
    CDS->>HANA: Optimized Query
    HANA->>CE: Complex Calculations
    
    par Parallel Execution
        CE->>CE: Aggregate
        CE->>CE: Join
        CE->>CE: Calculate
        CE->>CE: Filter
    end
    
    CE-->>HANA: Result Set
    HANA-->>CDS: Small Result
    CDS-->>AS: Processed Data
    AS-->>Fiori: JSON Response
    Fiori-->>User: Real-time Display
    
    Note over User,CE: Seconds vs Hours
```

### Инновации S/4HANA

**Fiori UX**: Современный пользовательский интерфейс, построенный на HTML5/SAPUI5, работающий на любых устройствах.

**Embedded Analytics**: Аналитика в реальном времени без отдельных БД или кубов.

**Simplified Data Model**: Устранение избыточности через использование возможностей HANA.

**Digital Core**: Готовность к интеграции с IoT, ML, блокчейн и другими современными технологиями.

### Сравнение архитектурных эпох

```mermaid
graph TB
    subgraph "SAP Architecture Evolution"
        subgraph "R/2 (1979-1992)"
            R2_CHAR[✓ Mainframe<br/>✓ Monolithic<br/>✓ Text UI<br/>✓ Batch Processing]
        end
        
        subgraph "R/3 (1992-2015)"
            R3_CHAR[✓ Client-Server<br/>✓ 3-Tier<br/>✓ GUI<br/>✓ Real-time OLTP]
        end
        
        subgraph "NetWeaver (2004-2015)"
            NW_CHAR[✓ SOA<br/>✓ Dual-Stack<br/>✓ Web Services<br/>✓ Portal]
        end
        
        subgraph "S/4HANA (2015-Now)"
            S4_CHAR[✓ In-Memory<br/>✓ Simplified<br/>✓ Mobile/Cloud<br/>✓ Real-time Everything]
        end
        
        R2_CHAR --> R3_CHAR
        R3_CHAR --> NW_CHAR
        NW_CHAR --> S4_CHAR
        
        subgraph "Key Metrics"
            SCALE[Scalability:<br/>R/2: ⭐<br/>R/3: ⭐⭐⭐<br/>NW: ⭐⭐⭐<br/>S/4: ⭐⭐⭐⭐⭐]
            
            COMPLEX[Complexity:<br/>R/2: ⭐⭐<br/>R/3: ⭐⭐⭐<br/>NW: ⭐⭐⭐⭐⭐<br/>S/4: ⭐⭐⭐]
            
            PERF[Performance:<br/>R/2: ⭐⭐⭐<br/>R/3: ⭐⭐⭐<br/>NW: ⭐⭐<br/>S/4: ⭐⭐⭐⭐⭐]
        end
    end
    
    style S4_CHAR fill:#4CAF50,stroke:#333,stroke-width:4px
    style R3_CHAR fill:#99ccff,stroke:#333,stroke-width:2px
```

### Будущее: Intelligent Enterprise

```mermaid
graph TB
    subgraph "Future Architecture Trends"
        subgraph "Cloud Native"
            BTP[Business Technology Platform]
            MICRO[Microservices]
            CONTAINER[Containerization]
            SERVER[Serverless]
        end
        
        subgraph "Intelligence"
            AI[Embedded AI/ML]
            RPA[Robotic Process Automation]
            ANALYTICS[Predictive Analytics]
            NLP[Natural Language Processing]
        end
        
        subgraph "Integration"
            API[API Economy]
            EVENT[Event-Driven]
            MESH[Data Mesh]
            GRAPH[Knowledge Graphs]
        end
        
        S4HANA[S/4HANA Core] --> BTP
        S4HANA --> AI
        S4HANA --> API
        
        BTP --> MICRO
        BTP --> CONTAINER
        
        AI --> ANALYTICS
        AI --> NLP
        
        API --> EVENT
        API --> MESH
    end
    
    style S4HANA fill:#4CAF50,stroke:#333,stroke-width:4px
    style BTP fill:#99ff99,stroke:#333,stroke-width:2px
```

## Заключение

История архитектурной эволюции SAP — это история адаптации к меняющимся технологическим возможностям и бизнес-требованиям:

1. **R/2** отражала эпоху централизованных вычислений, где мейнфреймы были единственным способом обработки корпоративных данных

2. **R/3** революционизировала индустрию, демократизировав доступ к ERP через клиент-серверную архитектуру

3. **NetWeaver** представлял амбициозную, но в конечном счете неудачную попытку объединить ABAP и Java миры

4. **S/4HANA** использует прорыв в технологиях памяти для фундаментального переосмысления обработки данных

Каждая архитектурная эпоха решала проблемы предыдущей, создавая при этом новые вызовы. Понимание этой эволюции критически важно для:

- Архитекторов, проектирующих современные решения
- Разработчиков, работающих с legacy системами
- Руководителей, принимающих решения о модернизации
- Всех, кто хочет понять, куда движется платформа SAP

В следующей главе мы детально рассмотрим SAP HANA — технологию, которая сделала возможной трансформацию S/4HANA и определяет будущее корпоративных вычислений.