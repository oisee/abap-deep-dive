# Глава 2: Ядро SAP - операционная система бизнес-приложений

## 2.1. Состав и функции: операционная система ABAP

Ядро SAP (SAP Kernel) представляет собой набор платформенно-зависимых исполняемых файлов, которые формируют среду выполнения для всех приложений ABAP. По своей сути, это специализированная операционная система для бизнес-приложений, предоставляющая уровень абстракции между прикладным кодом ABAP и базовой операционной системой хоста.

### Архитектурная роль ядра

```mermaid
graph TB
    subgraph "SAP System Stack"
        subgraph "Application Layer"
            ABAP[ABAP Programs]
            DDIC[Data Dictionary]
            REPO[Repository Objects]
        end
        
        subgraph "SAP Kernel Layer"
            subgraph "Core Components"
                VM[ABAP Virtual Machine]
                MM[Memory Manager]
                PM[Process Manager]
                DBM[Database Manager]
            end
            
            subgraph "Services"
                NET[Network Handler]
                SEC[Security Manager]
                LOG[Logging System]
                MON[Monitoring]
            end
        end
        
        subgraph "Operating System Layer"
            OS[Operating System<br/>Linux/Windows/AIX/HPUX]
            FS[File System]
            SOCK[Network Stack]
            MEM[Physical Memory]
        end
        
        subgraph "Hardware Layer"
            CPU[CPU]
            RAM[RAM]
            DISK[Storage]
            NIC[Network]
        end
    end
    
    ABAP --> VM
    DDIC --> DBM
    REPO --> MM
    
    VM --> OS
    MM --> MEM
    PM --> OS
    DBM --> SOCK
    NET --> SOCK
    
    OS --> CPU
    OS --> RAM
    FS --> DISK
    SOCK --> NIC
    
    style VM fill:#ff9999,stroke:#333,stroke-width:4px
    style MM fill:#ff9999,stroke:#333,stroke-width:4px
    style PM fill:#ff9999,stroke:#333,stroke-width:4px
    style DBM fill:#ff9999,stroke:#333,stroke-width:4px
```

Ядро SAP выполняет следующие критические функции:

1. **Абстракция платформы**: Обеспечивает единый API для ABAP-приложений независимо от ОС
2. **Управление процессами**: Запуск, остановка и координация work processes
3. **Управление памятью**: Выделение и управление shared memory сегментами
4. **Сетевое взаимодействие**: Обработка различных протоколов (DIAG, RFC, HTTP)
5. **Безопасность**: Аутентификация, авторизация, шифрование
6. **Интеграция с БД**: Унифицированный интерфейс к различным СУБД

### Структура каталога ядра

Типичная структура каталога ядра в файловой системе:

```bash
/usr/sap/<SID>/SYS/exe/uc/<platform>/
├── disp+work           # Основной исполняемый файл (dispatcher + work process)
├── gwrd                # Gateway daemon
├── icman               # Internet Communication Manager
├── ms.sap              # Message Server
├── enserver            # Enqueue Server
├── sapstartsrv         # Start Service
├── sapcontrol          # Control Interface
├── saphostexec         # Host Agent Executable
├── libsapu16.so        # Unicode library
├── libicuuc.so.*       # ICU Unicode libraries
├── libsapdb.so         # Database interface library
├── dboraslib.so        # Oracle-specific DB library
├── dbhdbslib.so        # HANA-specific DB library
└── ...                 # Множество других библиотек и утилит
```

### Версионирование ядра

Версия ядра SAP следует строгой схеме нумерации:

```
<release>.<patch_level>.<platform>.<bit>
Пример: 753.02.100.x86_64
```

Где:

- **release**: Основная версия (753 = NetWeaver 7.53)
- **patch_level**: Уровень патча
- **platform**: Целевая платформа (linuxx86_64, ntamd64, etc.)
- **bit**: Разрядность (устарело, теперь только 64-bit)

```mermaid
graph LR
    subgraph "Kernel Version Components"
        VER[753.02.100.x86_64]
        
        VER --> REL[Release: 753<br/>NetWeaver 7.53]
        VER --> PATCH[Patch: 02<br/>Second patch level]
        VER --> PLAT[Platform: 100<br/>Linux specific]
        VER --> ARCH[Architecture: x86_64<br/>64-bit Intel/AMD]
    end
    
    subgraph "Compatibility Matrix"
        K753[Kernel 753] --> NW753[NetWeaver 7.53]
        K753 --> NW752[NetWeaver 7.52]
        K753 --> NW751[NetWeaver 7.51]
        
        K745[Kernel 745] --> NW745[NetWeaver 7.45]
        K745 -.X.-> NW753
    end
    
    style VER fill:#2196F3,stroke:#333,stroke-width:2px
    style K753 fill:#4CAF50,stroke:#333,stroke-width:2px
```

### Обратная совместимость

Ключевой особенностью архитектуры ядра SAP является обратная совместимость:

```abap
* Этот ABAP код, написанный для NetWeaver 7.40
* будет работать на ядре 7.53 без изменений
DATA: lv_string TYPE string.
lv_string = |Hello from { sy-sysid }|.
WRITE: / lv_string.
```

Обратная совместимость достигается за счет:

- Стабильного ABI (Application Binary Interface)
- Версионирования внутренних структур
- Эмуляции устаревших функций
- Тщательного тестирования регрессий

## 2.2. Анализ ключевых исполняемых файлов ядра

### disp+work: Швейцарский нож SAP

Файл `disp+work` — это центральный исполняемый файл ядра SAP. Уникальность его архитектуры заключается в том, что один и тот же бинарный файл может выполнять различные роли в зависимости от параметров запуска.

```mermaid
graph TB
    subgraph "disp+work Execution Modes"
        BIN[disp+work binary]
        
        subgraph "Start Parameters"
            P1[pf=profile SAPSYSTEMNAME=SID<br/>SAPSYSTEM=00 INSTANCE_NAME=DVEBMGS00]
            P2[pf=profile wp_id=1]
            P3[-V or -version]
        end
        
        subgraph "Execution Modes"
            DISP[Dispatcher Mode<br/>Process Manager]
            WP[Work Process Mode<br/>ABAP Executor]
            INFO[Information Mode<br/>Version Display]
        end
        
        BIN --> P1
        BIN --> P2
        BIN --> P3
        
        P1 --> DISP
        P2 --> WP
        P3 --> INFO
    end
    
    style BIN fill:#ff9999,stroke:#333,stroke-width:4px
    style DISP fill:#99ccff,stroke:#333,stroke-width:2px
    style WP fill:#99ff99,stroke:#333,stroke-width:2px
```

#### Режим диспетчера

Когда `disp+work` запускается без параметра `wp_id`, он работает как диспетчер:

```c
// Псевдокод внутренней логики disp+work
int main(int argc, char* argv[]) {
    if (get_parameter("wp_id") == NULL) {
        return dispatcher_main();  // Режим диспетчера
    } else {
        return work_process_main(); // Режим work process
    }
}
```

Функции диспетчера:

- Управление очередью запросов
- Распределение задач между work processes
- Мониторинг состояния процессов
- Управление shared memory

#### Режим work process

С параметром `wp_id`, тот же исполняемый файл становится work process:

```bash
# Пример запуска work process
disp+work pf=/sapmnt/SID/profile/SID_DVEBMGS00_hostname wp_id=1 wp_type=DIA
```

### ICM (Internet Communication Manager)

ICM — это веб-сервер и клиент в составе ядра SAP, обрабатывающий HTTP(S), SMTP и другие интернет-протоколы.

```mermaid
sequenceDiagram
    participant Browser
    participant ICM
    participant WP as Work Process
    participant Handler as HTTP Handler
    
    Browser->>ICM: HTTPS Request
    ICM->>ICM: SSL Termination
    ICM->>ICM: Check URL Rules
    
    alt Static Content
        ICM->>ICM: Serve from cache
        ICM-->>Browser: Response
    else Dynamic Content
        ICM->>WP: Forward to free WP
        WP->>Handler: Call ABAP Handler
        Handler->>Handler: Process Business Logic
        Handler-->>WP: HTTP Response
        WP-->>ICM: Response Data
        ICM-->>Browser: HTTPS Response
    end
```

Конфигурация ICM через профильные параметры:

```ini
# Пример конфигурации ICM в профиле
icm/server_port_0 = PROT=HTTPS,PORT=443,PROCTIMEOUT=600,TIMEOUT=600
icm/server_port_1 = PROT=HTTP,PORT=8000,PROCTIMEOUT=600,TIMEOUT=600
icm/max_threads = 250
icm/max_conn = 5000
icm/keep_alive_timeout = 60
icm/host_name_full = $(SAPLOCALHOST).company.com
```

### Gateway (gwrd)

Gateway — это коммуникационный процесс, обеспечивающий RFC (Remote Function Call) взаимодействие.

```mermaid
graph TB
    subgraph "Gateway Architecture"
        subgraph "Gateway Process"
            GWRD[gwrd process]
            REG[Program Registry]
            CONN[Connection Table]
            SEC[Security Table]
        end
        
        subgraph "Communication Types"
            RFC1[Inbound RFC]
            RFC2[Outbound RFC]
            REG_PROG[Registered Programs]
            START_PROG[Started Programs]
        end
        
        subgraph "Protocols"
            CPIC[CPI-C Protocol]
            RFC[RFC Protocol]
            SNC[Secure Network Comm]
        end
    end
    
    RFC1 --> GWRD
    RFC2 --> GWRD
    REG_PROG --> REG
    START_PROG --> GWRD
    
    GWRD --> CPIC
    GWRD --> RFC
    GWRD --> SNC
    
    style GWRD fill:#ff9999,stroke:#333,stroke-width:4px
```

Ключевые функции Gateway:

- Регистрация внешних программ
- Управление RFC соединениями
- Безопасность и контроль доступа
- Конвертация протоколов

### Message Server (ms.sap)

Message Server выполняет критические функции координации в SAP системе:

```mermaid
stateDiagram-v2
    [*] --> Starting
    
    Starting --> Running: Initialize
    
    state Running {
        [*] --> Listening
        
        Listening --> RegisteringInstance: New Instance
        RegisteringInstance --> UpdatedRegistry: Add to List
        UpdatedRegistry --> Listening
        
        Listening --> LoadBalancing: User Logon
        LoadBalancing --> RouteToInstance: Select Server
        RouteToInstance --> Listening
        
        Listening --> Monitoring: Health Check
        Monitoring --> UpdateStatus: Check Instances
        UpdateStatus --> Listening
    }
    
    Running --> Stopping: Shutdown Signal
    Stopping --> [*]
```

## 2.3. Обновления ядра и необходимость SGEN

### Процесс обновления ядра

Обновление ядра SAP — это критическая операция, требующая тщательного планирования:

```mermaid
graph TB
    subgraph "Kernel Update Process"
        PREP[Preparation]
        DOWNLOAD[Download New Kernel]
        BACKUP[Backup Current Kernel]
        STOP[Stop SAP System]
        EXTRACT[Extract New Files]
        PERMISSION[Set Permissions]
        START[Start SAP System]
        SGEN[Run SGEN]
        TEST[Test System]
    end
    
    PREP --> DOWNLOAD
    DOWNLOAD --> BACKUP
    BACKUP --> STOP
    STOP --> EXTRACT
    EXTRACT --> PERMISSION
    PERMISSION --> START
    START --> SGEN
    SGEN --> TEST
    
    subgraph "File Operations"
        OLD[/exe/uc/linuxx86_64/<br/>Old kernel files]
        NEW[/exe/uc/linuxx86_64/<br/>New kernel files]
        
        BACKUP -.backup.-> OLD
        EXTRACT -.replace.-> NEW
    end
    
    style STOP fill:#ff9999,stroke:#333,stroke-width:2px
    style SGEN fill:#ffff99,stroke:#333,stroke-width:2px
```

Типичная последовательность команд для обновления:

```bash
# 1. Создание резервной копии
cd /usr/sap/<SID>/SYS/exe/uc/linuxx86_64
mkdir kernel_backup_$(date +%Y%m%d)
cp -a * kernel_backup_$(date +%Y%m%d)/

# 2. Остановка системы
stopsap all

# 3. Распаковка нового ядра
cd /usr/sap/<SID>/SYS/exe/uc/linuxx86_64
SAPCAR -xf /tmp/SAPEXE_100-80005374.SAR
SAPCAR -xf /tmp/SAPEXEDB_100-80005373.SAR

# 4. Установка прав
./saproot.sh <sid>

# 5. Запуск системы
startsap all
```

### ABAP Load и инвалидация

Ключевой аспект обновления ядра — инвалидация ABAP Load. Чтобы понять этот процесс, необходимо рассмотреть структуру ABAP Load:

```mermaid
graph LR
    subgraph "ABAP Program Lifecycle"
        SOURCE[ABAP Source Code<br/>Table: REPOSRC]
        COMPILER[ABAP Compiler<br/>Kernel Component]
        LOAD[ABAP Load<br/>Table: REPOLOAD]
        VM[ABAP VM<br/>Kernel Component]
    end
    
    SOURCE -->|Compile| COMPILER
    COMPILER -->|Generate| LOAD
    LOAD -->|Execute| VM
    
    subgraph "Load Structure"
        HEADER[Load Header<br/>- Version Info<br/>- Timestamp<br/>- Dependencies]
        BYTECODE[Bytecode<br/>- VM Instructions<br/>- Data Structures]
        METADATA[Metadata<br/>- Symbol Table<br/>- Debug Info]
    end
    
    LOAD --> HEADER
    LOAD --> BYTECODE
    LOAD --> METADATA
    
    style COMPILER fill:#ff9999,stroke:#333,stroke-width:2px
    style VM fill:#ff9999,stroke:#333,stroke-width:2px
```

#### Структура ABAP Load

ABAP Load содержит версионную информацию, привязанную к версии компилятора:

```c
// Псевдо-структура ABAP Load Header
typedef struct {
    uint32_t magic;           // Magic number: 0xABAP
    uint16_t load_version;    // Version of load format
    uint16_t compiler_version;// Compiler version
    uint32_t timestamp;       // Generation timestamp
    uint32_t checksum;        // Load checksum
    // ... другие поля
} abap_load_header_t;
```

### SGEN (SAP Load Generator)

SGEN — это массовый генератор ABAP Load, критически важный после обновления ядра:

```mermaid
graph TB
    subgraph "SGEN Process Flow"
        START[Start SGEN]
        ANALYZE[Analyze System]
        
        subgraph "Parallel Processing"
            QUEUE[Work Queue]
            WP1[Worker 1]
            WP2[Worker 2]
            WP3[Worker 3]
            WPN[Worker N]
        end
        
        COMPILE[Compile Programs]
        STORE[Store in DB]
        REPORT[Progress Report]
        
        START --> ANALYZE
        ANALYZE --> QUEUE
        
        QUEUE --> WP1
        QUEUE --> WP2
        QUEUE --> WP3
        QUEUE --> WPN
        
        WP1 --> COMPILE
        WP2 --> COMPILE
        WP3 --> COMPILE
        WPN --> COMPILE
        
        COMPILE --> STORE
        STORE --> REPORT
    end
    
    subgraph "What Gets Generated"
        PROGRAMS[All Programs]
        INCLUDES[All Includes]
        CLASSES[All Classes]
        TYPES[Type Groups]
        
        PROGRAMS --> QUEUE
        INCLUDES --> QUEUE
        CLASSES --> QUEUE
        TYPES --> QUEUE
    end
    
    style QUEUE fill:#ffff99,stroke:#333,stroke-width:2px
    style COMPILE fill:#99ff99,stroke:#333,stroke-width:2px
```

Параметры SGEN для оптимизации:

```abap
* Пример настройки параллельности SGEN
DATA: lv_parallel_processes TYPE i VALUE 10.

CALL FUNCTION 'SGEN_SET_PARALLEL_PROCESSES'
  EXPORTING
    parallel_processes = lv_parallel_processes.
```

### Почему SGEN необходим

Инвалидация происходит по нескольким причинам:

1. **Изменения в VM инструкциях**: Новая версия ядра может содержать оптимизированные или новые VM инструкции
    
2. **Изменения внутренних структур**: Структуры данных, используемые runtime, могут измениться
    
3. **Исправления безопасности**: Патчи могут изменять способ выполнения определенных операций
    

```mermaid
sequenceDiagram
    participant User
    participant System
    participant Kernel
    participant DB
    
    User->>System: Start transaction
    System->>DB: Check ABAP Load
    DB-->>System: Load version mismatch!
    
    alt Automatic Regeneration
        System->>Kernel: Compile program
        Kernel->>Kernel: Generate new load
        Kernel->>DB: Store new load
        Kernel-->>System: New load ready
        System->>User: Execute (with delay)
    else Load Dump
        System->>User: LOAD_VERSION_LOST dump
    end
```

## 2.4. Kernel как спецификация виртуальной машины

### Архитектура виртуальной машины ABAP

Ядро SAP по сути определяет спецификацию виртуальной машины ABAP — абстрактной вычислительной машины, которая выполняет ABAP байт-код:

```mermaid
graph TB
    subgraph "ABAP VM Architecture"
        subgraph "Frontend"
            PARSER[Parser]
            LEXER[Lexer]
            AST[AST Builder]
            SEMANTIC[Semantic Analyzer]
        end
        
        subgraph "Middle-end"
            OPT[Optimizer]
            CODEGEN[Code Generator]
        end
        
        subgraph "Backend"
            subgraph "Execution Engine"
                DECODER[Instruction Decoder]
                EXEC[Execution Unit]
                STACK[Stack Manager]
                HEAP[Heap Manager]
            end
            
            subgraph "Runtime Services"
                GC[Garbage Collector]
                EXC[Exception Handler]
                DBG[Debugger Interface]
                PROF[Profiler]
            end
        end
    end
    
    PARSER --> AST
    AST --> SEMANTIC
    SEMANTIC --> OPT
    OPT --> CODEGEN
    CODEGEN --> DECODER
    DECODER --> EXEC
    
    EXEC <--> STACK
    EXEC <--> HEAP
    EXEC <--> GC
    EXEC <--> EXC
    
    style EXEC fill:#ff9999,stroke:#333,stroke-width:4px
    style CODEGEN fill:#99ff99,stroke:#333,stroke-width:2px
```

### Набор инструкций VM

ABAP VM использует stack-based архитектуру с набором инструкций, оптимизированных для бизнес-логики:

```mermaid
graph LR
    subgraph "VM Instruction Categories"
        subgraph "Data Operations"
            LOAD[LOAD - Load to stack]
            STORE[STORE - Store from stack]
            MOVE[MOVE - Move data]
        end
        
        subgraph "Arithmetic"
            ADD[ADD - Addition]
            SUB[SUB - Subtraction]
            MUL[MUL - Multiplication]
            DIV[DIV - Division]
        end
        
        subgraph "Control Flow"
            JMP[JMP - Jump]
            JZ[JZ - Jump if Zero]
            CALL[CALL - Call method]
            RET[RET - Return]
        end
        
        subgraph "Special"
            SQL[SQL - Database op]
            AUTH[AUTH - Authority check]
            LOCK[LOCK - Set lock]
        end
    end
    
    style SQL fill:#99ccff,stroke:#333,stroke-width:2px
    style AUTH fill:#ffcc99,stroke:#333,stroke-width:2px
```

Пример декомпиляции простой ABAP программы:

```abap
* Исходный ABAP код
DATA: lv_result TYPE i.
lv_result = 10 + 20.
WRITE: / lv_result.
```

Превращается в последовательность VM инструкций (упрощенно):

```assembly
; Псевдо-ассемблер ABAP VM
ALLOC   4           ; Allocate 4 bytes for lv_result
PUSH    10          ; Push 10 to stack
PUSH    20          ; Push 20 to stack
ADD                 ; Add top two stack values
STORE   lv_result   ; Store result
LOAD    lv_result   ; Load for output
WRITE   /           ; Write with new line
```

### Оптимизации в современном ядре

Современные версии ядра включают множество оптимизаций:

```mermaid
graph TB
    subgraph "Runtime Optimizations"
        subgraph "JIT Compilation"
            HOT[Hot Spot Detection]
            JIT[JIT Compiler]
            NATIVE[Native Code Cache]
        end
        
        subgraph "Memory Optimizations"
            POOL[Object Pooling]
            INTERN[String Interning]
            COMPRESS[Data Compression]
        end
        
        subgraph "Execution Optimizations"
            INLINE[Method Inlining]
            CONST[Constant Folding]
            DEAD[Dead Code Elimination]
        end
    end
    
    HOT --> JIT
    JIT --> NATIVE
    
    style JIT fill:#4CAF50,stroke:#333,stroke-width:2px
```

### Интеграция с современными процессорами

Ядро SAP оптимизировано для использования современных возможностей процессоров:

```c
// Пример использования SIMD инструкций в ядре
void abap_vm_array_add_optimized(double* a, double* b, double* result, size_t n) {
    #ifdef __AVX2__
    size_t simd_end = n - (n % 4);
    for (size_t i = 0; i < simd_end; i += 4) {
        __m256d va = _mm256_load_pd(&a[i]);
        __m256d vb = _mm256_load_pd(&b[i]);
        __m256d vr = _mm256_add_pd(va, vb);
        _mm256_store_pd(&result[i], vr);
    }
    // Handle remaining elements
    for (size_t i = simd_end; i < n; i++) {
        result[i] = a[i] + b[i];
    }
    #else
    // Fallback to scalar implementation
    for (size_t i = 0; i < n; i++) {
        result[i] = a[i] + b[i];
    }
    #endif
}
```

### Эволюция спецификации VM

```mermaid
timeline
    title ABAP VM Evolution
    
    1992 : R/3 Release 1.0
         : Basic stack machine
         : 16-bit addressing
         : Single-byte chars
    
    1997 : R/3 Release 4.0
         : Unicode support
         : Object orientation
         : 32-bit addressing
    
    2004 : NetWeaver 6.40
         : 64-bit support
         : JIT experiments
         : Improved GC
    
    2011 : NetWeaver 7.40
         : HANA optimizations
         : Expressions
         : Inline declarations
    
    2020 : ABAP Platform 2020
         : Cloud runtime
         : Restricted mode
         : Native integration
```

### Debugging и профилирование на уровне VM

Ядро предоставляет богатые возможности для отладки и профилирования:

```mermaid
graph TB
    subgraph "Debugging Infrastructure"
        subgraph "Breakpoint Types"
            BP1[Session Breakpoints]
            BP2[External Breakpoints]
            BP3[System Breakpoints]
        end
        
        subgraph "Debug Info"
            SRC[Source Mapping]
            VAR[Variable Table]
            STACK_T[Stack Trace]
        end
        
        subgraph "Profiling"
            TIME[Time Profiling]
            MEM_P[Memory Profiling]
            SQL_P[SQL Profiling]
        end
    end
    
    BP1 --> SRC
    BP2 --> SRC
    BP3 --> SRC
    
    SRC --> VAR
    VAR --> STACK_T
    
    style TIME fill:#99ff99,stroke:#333,stroke-width:2px
    style MEM_P fill:#99ff99,stroke:#333,stroke-width:2px
```

Пример активации профилирования на уровне VM:

```abap
* Активация профилирования VM
DATA: lo_profiler TYPE REF TO cl_abap_runtime_profiler.

CREATE OBJECT lo_profiler.
lo_profiler->start_measurement( ).

" Код для профилирования
DO 1000000 TIMES.
  " Some operation
ENDDO.

lo_profiler->stop_measurement( ).
lo_profiler->show_results( ).
```

## Заключение

Ядро SAP представляет собой сложную, многослойную систему, которая эволюционировала более 30 лет, сохраняя при этом обратную совместимость. Понимание архитектуры ядра критически важно для:

1. **Администраторов**: Правильное планирование обновлений, понимание необходимости SGEN, диагностика проблем производительности
    
2. **Разработчиков**: Написание оптимизированного кода, понимание ограничений VM, использование новых возможностей
    
3. **Архитекторов**: Проектирование масштабируемых решений, понимание влияния архитектурных решений на производительность
    

Ключевые выводы:

- Ядро SAP — это полноценная виртуальная машина со своей спецификацией
- Файл disp+work — универсальный исполняемый файл с multiple personalities
- Обновление ядра требует регенерации всего ABAP кода через SGEN
- Современное ядро включает продвинутые оптимизации для современного hardware

В следующей главе мы углубимся в детали работы work processes — основных исполнительных единиц, где происходит вся магия выполнения ABAP кода.