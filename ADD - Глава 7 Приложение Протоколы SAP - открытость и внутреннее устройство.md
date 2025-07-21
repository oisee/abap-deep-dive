## Приложение к главе 7: Протоколы SAP - открытость и внутреннее устройство

### DIAG Protocol (Dynamic Information and Action Gateway)

**Статус**: Проприетарный, закрытый протокол SAP

DIAG является основным протоколом коммуникации между SAP GUI и Application Server. Несмотря на закрытость, сообщество исследователей смогло частично реверс-инжинирить протокол.

#### Структура DIAG протокола

```mermaid
graph TB
    subgraph "DIAG Protocol Structure"
        subgraph "DIAG Header"
            MODE[Mode: 0xFF]
            COM[Communication Flag]
            MODE2[Mode2]
            UNCOMP_LEN[Uncompressed Length]
            COMP_LEN[Compressed Length]
            UNUSED[Unused: 0x00]
        end
        
        subgraph "DIAG Message"
            HEADER[Message Header]
            APPL[APPL/APPL4 Items]
            subgraph "Compression"
                LZH[LZH Compression]
                LZC[LZC Compression]
                NONE[No Compression]
            end
        end
        
        subgraph "APPL Items"
            SES[SES - Session Data]
            DYNT[DYNT - Screen Data]
            CONT[CONT - Control Data]
            VARINFO[VARINFO - Variable Info]
        end
        
        MODE --> HEADER
        HEADER --> APPL
        APPL --> LZH
        APPL --> LZC
        APPL --> NONE
        
        APPL --> SES
        APPL --> DYNT
        APPL --> CONT
        APPL --> VARINFO
    end
    
    style MODE fill:#ff9999,stroke:#333,stroke-width:2px
    style LZH fill:#99ccff,stroke:#333,stroke-width:2px
```

#### Известные детали DIAG

```python
# Структура DIAG заголовка (псевдокод на основе реверс-инжиниринга)
class DIAGHeader:
    def __init__(self):
        self.mode = 0xFF  # Всегда 0xFF для DIAG
        self.com_flag = 0x00  # Communication flags
        self.mode2 = 0x00  # Additional mode
        self.uncomp_len = 0  # Размер несжатых данных
        self.comp_len = 0  # Размер сжатых данных
        self.unused = [0x00] * 4  # Зарезервировано

# Типы DIAG items
DIAG_ITEM_TYPES = {
    0x01: "SES",      # Session/connection
    0x02: "ICO",      # Icon
    0x03: "TIT",      # Title
    0x04: "DIA",      # Dialog info
    0x10: "APPL",     # Application data
    0x11: "APPL4",    # Application data (4-byte length)
    0x12: "DYNT",     # Screen/Dynpro data
    0x13: "CONT",     # Container/Control
}
```

#### Безопасность DIAG

- **Шифрование**: По умолчанию отсутствует (!), требует активации SNC
- **Аутентификация**: Базовая, уязвима для атак
- **Компрессия**: LZH/LZC может скрывать payload

#### Инструменты для работы с DIAG

1. **Wireshark** - имеет диссектор для DIAG (частичная поддержка)
2. **pysap** - Python библиотека от CoreLabs для работы с SAP протоколами
3. **SAPRouter** - может логировать DIAG трафик

### RFC Protocol (Remote Function Call)

**Статус**: Частично документирован, проприетарный

RFC протокол используется для вызова функций между SAP системами и внешними приложениями.

#### Структура RFC

```mermaid
graph TB
    subgraph "RFC Protocol Layers"
        subgraph "RFC Header"
            VERSION[Version]
            FUNC_TYPE[Function Type]
            PROTOCOL[Protocol Type]
            ENCODING[Character Encoding]
            FLOAT_TYPE[Float Type]
        end
        
        subgraph "CPIC Layer"
            CPIC_HEADER[CPI-C Header]
            CONV_ID[Conversation ID]
            LU_NAME[LU Name]
        end
        
        subgraph "RFC Data"
            FUNC_NAME[Function Name]
            PARAMS[Parameters]
            subgraph "Parameter Types"
                IMPORT[Import Params]
                EXPORT[Export Params]
                TABLES[Table Params]
                CHANGING[Changing Params]
            end
        end
        
        VERSION --> CPIC_HEADER
        CPIC_HEADER --> FUNC_NAME
        FUNC_NAME --> PARAMS
        
        PARAMS --> IMPORT
        PARAMS --> EXPORT
        PARAMS --> TABLES
        PARAMS --> CHANGING
    end
    
    style VERSION fill:#4CAF50,stroke:#333,stroke-width:2px
    style FUNC_NAME fill:#99ccff,stroke:#333,stroke-width:2px
```

#### RFC библиотеки и SDK

- **SAP NetWeaver RFC SDK**: Официальная библиотека (требует лицензию)
- **PyRFC**: Python wrapper для RFC SDK
- **node-rfc**: Node.js wrapper
- **JCo**: Java Connector для RFC

### ADT Protocol (ABAP Development Tools)

**Статус**: REST-based, частично документирован

ADT использует REST API поверх HTTP/HTTPS для коммуникации между Eclipse и SAP системой.

#### Архитектура ADT

```mermaid
graph TB
    subgraph "ADT Protocol Architecture"
        subgraph "Client (Eclipse)"
            IDE[Eclipse IDE]
            ADT_PLUGIN[ADT Plugin]
            REST_CLIENT[REST Client]
        end
        
        subgraph "Transport"
            HTTPS[HTTPS/TLS]
            subgraph "Authentication"
                BASIC[Basic Auth]
                SAML[SAML 2.0]
                OAUTH[OAuth 2.0]
            end
        end
        
        subgraph "Server (SAP)"
            ICF_HANDLER["ICF Handler<br/>/sap/bc/adt/*"]
            ADT_CORE[ADT Core Services]
            ABAP_REPO[ABAP Repository]
        end
        
        subgraph "REST Endpoints"
            DISCOVERY["/sap/bc/adt/discovery"]
            REPO_EP["/sap/bc/adt/repository/*"]
            DEBUG_EP["/sap/bc/adt/debugger/*"]
            TRANSPORT_EP["/sap/bc/adt/cts/*"]
        end
        
        IDE --> ADT_PLUGIN
        ADT_PLUGIN --> REST_CLIENT
        REST_CLIENT --> HTTPS
        HTTPS --> ICF_HANDLER
        
        ICF_HANDLER --> DISCOVERY
        ICF_HANDLER --> REPO_EP
        ICF_HANDLER --> DEBUG_EP
        ICF_HANDLER --> TRANSPORT_EP
        
        ADT_CORE --> ABAP_REPO
    end
    
    style HTTPS fill:#4CAF50,stroke:#333,stroke-width:2px
    style REST_CLIENT fill:#99ccff,stroke:#333,stroke-width:2px
```

#### Примеры ADT REST вызовов

```http
# Discovery service
GET /sap/bc/adt/discovery HTTP/1.1
Host: sap-system.company.com
Authorization: Basic dXNlcjpwYXNzd29yZA==
Accept: application/xml

# Чтение ABAP класса
GET /sap/bc/adt/oo/classes/zcl_example/source/main HTTP/1.1
Host: sap-system.company.com
Authorization: Basic dXNlcjpwYXNzd29yZA==
Accept: text/plain

# Активация объекта
POST /sap/bc/adt/activation HTTP/1.1
Host: sap-system.company.com
Content-Type: application/xml
Authorization: Basic dXNlcjpwYXNzd29yZA==

<?xml version="1.0" encoding="UTF-8"?>
<adtcore:objectReferences xmlns:adtcore="http://www.sap.com/adt/core">
  <adtcore:objectReference adtcore:uri="/sap/bc/adt/oo/classes/zcl_example"/>
</adtcore:objectReferences>
```

### Message Server Protocol

**Статус**: Проприетарный, частично известен

Message Server использует собственный протокол для координации инстанций.

#### Структура MS протокола

```mermaid
graph LR
    subgraph "Message Server Protocol"
        subgraph "MS Header"
            FLAG["Flag: -10/-11/-12"]
            OPCODE[Opcode]
            LENGTH[Message Length]
        end
        
        subgraph "MS Operations"
            OP_LOGIN[MS_LOGIN]
            OP_LOGOUT[MS_LOGOUT]
            OP_SERVER_LIST[MS_SERVER_LST]
            OP_PROPERTY[MS_PROPERTY]
        end
        
        subgraph "Port Types"
            PORT_3900[3900 + Instance]
            PORT_HTTP[HTTP Port 81XX]
            PORT_INTERNAL[36XX Internal]
        end
        
        FLAG --> OPCODE
        OPCODE --> OP_LOGIN
        OPCODE --> OP_LOGOUT
        OPCODE --> OP_SERVER_LIST
        
        OP_LOGIN --> PORT_3900
        OP_SERVER_LIST --> PORT_HTTP
    end
    
    style FLAG fill:#ff9999,stroke:#333,stroke-width:2px
```

### Gateway Protocol

**Статус**: Проприетарный

Gateway протокол используется для RFC коммуникаций и регистрации внешних программ.

```mermaid
graph TB
    subgraph "Gateway Protocol Components"
        subgraph "Gateway Monitor"
            GWMON[gwmon Commands]
            MENU[Menu System]
            TRACE[Trace Options]
        end
        
        subgraph "Security Table"
            REG_INFO[reg_info]
            SEC_INFO[sec_info]
            PRM_REG[prm_reg]
        end
        
        subgraph "Registered Programs"
            RFC_SERVER[RFC Server Programs]
            STARTED[Started Programs]
        end
        
        GWMON --> REG_INFO
        GWMON --> SEC_INFO
        
        SEC_INFO --> RFC_SERVER
        REG_INFO --> STARTED
    end
    
    style GWMON fill:#4CAF50,stroke:#333,stroke-width:2px
```

### Сравнение открытости протоколов

```mermaid
graph TB
    subgraph "SAP Protocol Openness Comparison"
        subgraph "Closed/Proprietary"
            DIAG_C[DIAG Protocol ❌]
            RFC_C[RFC Protocol ⚠️]
            MS_C[Message Server ❌]
            GW_C[Gateway Protocol ❌]
        end
        
        subgraph "Partially Open"
            ADT_O[ADT REST API ✓]
            ODATA[OData Services ✓]
            SOAP_O[SOAP Web Services ✓]
        end
        
        subgraph "Based on Standards"
            HTTP_S["HTTP/HTTPS ✓✓"]
            SAML_S[SAML 2.0 ✓✓]
            OAUTH_S[OAuth 2.0 ✓✓]
        end
        
        subgraph "Security Implications"
            CLOSED_SEC[Closed = Security by Obscurity]
            OPEN_SEC[Open = Community Review]
            HYBRID[Hybrid = Worst of Both?]
        end
        
        DIAG_C --> CLOSED_SEC
        ADT_O --> HYBRID
        HTTP_S --> OPEN_SEC
    end
    
    style DIAG_C fill:#ff9999,stroke:#333,stroke-width:2px
    style ADT_O fill:#ffff99,stroke:#333,stroke-width:2px
    style HTTP_S fill:#99ff99,stroke:#333,stroke-width:2px
```

### Инструменты для исследования протоколов

1. **Wireshark** - Поддержка DIAG, частичная поддержка RFC
2. **pysap** - Comprehensive SAP protocol library
   ```bash
   pip install pysap
   ```
3. **Burp Suite** - Для ADT и других HTTP-based протоколов
4. **SAP GUI Scripting** - Для автоматизации и анализа GUI
5. **NMAP scripts** - NSE скрипты для SAP:
   ```bash
   nmap --script sap-* target
   ```

### Выводы о протоколах SAP

1. **Безопасность через неясность**: Большинство протоколов закрыты, что не обеспечивает реальной безопасности
2. **Необходимость шифрования**: Многие протоколы передают данные в открытом виде
3. **Сложность интеграции**: Закрытость протоколов усложняет разработку сторонних инструментов
4. **Движение к открытости**: Новые протоколы (ADT, OData) базируются на открытых стандартах

Понимание этих протоколов критически важно для:
- Администраторов безопасности
- Разработчиков интеграционных решений
- Исследователей безопасности
- Архитекторов систем