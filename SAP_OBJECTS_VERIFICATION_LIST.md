# SAP Objects Verification List

This document contains all SAP objects found in factcheck, issues, todo, and patches directories that need verification.

## Classes

### Standard Classes
- `CL_ABAP_TRACE` - ABAP trace/profiling class (Chapter 5)
- `CL_SPFL_PROFILE_PARAMETER` - Profile parameter access class (Chapter 4)
- `CL_SHMM_UTILS` - Shared memory utilities (Chapter 4)
- `CL_SYSTEM_MONITOR` - System monitoring class (Chapter 4)
- `CL_ABAP_MEMORY_INSPECTOR` - Memory inspector API (Chapter 12)
- `CL_ABAP_DAEMON_EXTENSION` - ABAP daemon extension (Chapter 11.1)
- `CL_SQL_CONNECTION` - SQL connection management (Chapter 6)
- `CL_SQL_STATEMENT` - SQL statement execution (Chapter 6)
- `CL_SQL_PREPARED_STATEMENT` - Prepared statements (Chapter 6)
- `CL_SQL_RESULT_SET` - SQL result set handling (Chapter 6)

### Gateway/OData Classes
- `/IWFND/CL_SODATA_HTTP_HANDLER` - OData HTTP handler (Chapter 9)
- `/IWFND/CL_MGW_RT_SERVICE` - Gateway runtime service (Chapter 9)
- `/IWBEP/CL_MGDEPIA2` - SADL runtime class (Chapter 9)

## Function Modules

### Standard Function Modules
- `SAPPARAM_GET_VALUE` - Get SAP parameter value (Chapter 4)
- `SGEN_SET_PARALLEL_PROCESSES` - SGEN parallelization (disputed - Chapter 2)
- `UPDATE_MATERIAL` - Update material (example - Chapter 6)
- `UPDATE_STATISTICS` - Update statistics (example - Chapter 6)
- `Z_UPDATE_MATERIAL` - Custom update function (example - Chapter 7 Appendix)
- `Z_CALCULATE_PRICE` - Custom calculation function (example - Chapter 7)
- `RSQLM_ACTIVATE_MONITORING` - SQL monitor activation (Chapter 12)
- `RSQLM_DEACTIVATE_MONITORING` - SQL monitor deactivation (Chapter 12)

## Transactions

### System Monitoring
- `SM50` - Process Overview (current instance)
- `SM51` - SAP Servers
- `SM66` - Global Work Process Overview
- `SM12` - Lock Entries
- `SM13` - Update Records
- `SM20` - Security Audit Log
- `SM21` - System Log
- `SM35` - Batch Input Monitoring
- `SM04` - User List
- `SMEM` - Extended Memory Monitor
- `SMICM` - ICM Monitor
- `SMI` - Memory Inspector (not S_MEMORY_INSPECTOR)
- `SHMM` - Shared Memory Management

### Performance Analysis
- `ST02` - Tune Summary (buffers and memory)
- `ST03` - Workload Analysis
- `ST03N` - Workload Monitor
- `ST04` - Database Performance Monitor
- `ST05` - SQL Trace
- `ST06` - Operating System Monitor
- `ST12` - Single Transaction Analysis
- `ST22` - ABAP Runtime Errors
- `STAD` - Statistics Records

### Development Tools
- `SE11` - ABAP Dictionary
- `SE30` - Runtime Analysis (old, replaced by SAT)
- `SE38` - ABAP Editor
- `SE80` - Object Navigator
- `SAT` - ABAP Trace (formerly SE30)
- `SGEN` - SAP Load Generator

### Gateway/OData
- `/IWFND/MAINT_SERVICE` - Service Maintenance
- `/IWFND/ERROR_LOG` - Error Log Analysis
- `/IWFND/GW_CLIENT` - Gateway Client
- `/IWFND/CACHE_CLEANUP` - Cache Management
- `/IWFND/MED_ACTIVATE` - Metadata Activation
- `/IWBEP/REG_SERVICE` - Service Registration

### Other Transactions
- `SAPC_ADMIN` - APC Administration (Chapter 11.1)
- `SICF` - HTTP Service Hierarchy Maintenance

## Programs/Reports
- `RSGENINVLAS` - SGEN automation report (Chapter 2)
- `RSTSMEMORY` - Memory analysis program (Chapter 12)

## System Tables
- `M_CS_TABLES` - HANA Column Store tables (Chapter 8)
- `M_RS_TABLES` - HANA Row Store tables (Chapter 8)
- `M_EXPENSIVE_STATEMENTS` - HANA expensive statements (Chapter 8)
- `M_SQL_PLAN_CACHE_OVERVIEW` - HANA SQL plan cache (Chapter 8)
- `M_TABLE_STATISTICS` - HANA table statistics (Chapter 8)
- `SRTM_MEASUREMENT` - Runtime measurement table (Chapter 12)
- `TADIR` - Directory of Repository Objects
- `DD02L`, `DD03L`, etc. - Dictionary tables

## Interfaces
- `IF_ABAP_RUNTIME_PROFILER` - Runtime profiler interface (disputed - doesn't exist, Chapter 5)
- `ZIF_I_SALESORDER_C` - Example business object interface (Chapter 10)

## Service Definitions
- `ZSD_SERVICE` - Example service definition (Chapter 11)
- `ZSD_SALESORDER` - Sales order service definition (Chapter 10)
- `ZSB_SALESORDER` - Service binding (Chapter 10)
- `ZSB_SALESORDER_V4` - OData V4 service binding (Chapter 10)
- `ZSD_SALES_ORDER_SRV` - Sales order service (Chapter 11)
- `ZUI_SALES_ORDER_O4` - UI service binding (Chapter 11)

## Parameters
- `rdisp/ROLL_SHM` - Roll area in shared memory
- `rdisp/ROLL_MAXFS` - Maximum roll file size
- `gw/sec_info` - Gateway security info file
- `gw/reg_info` - Gateway registration info file

## Protocols
- `DIAG` - SAP GUI protocol
- `RFC` - Remote Function Call
- `HTTP/HTTPS` - Web protocols
- `SNC` - Secure Network Communications

## Kernel Calls
- `GET_BUFFER_INFO` - Buffer information (disputed - not documented, Chapter 4)

## CDS Views
- `ZCDS_ADVANCED` - Example CDS view (Chapter 6)
- `ZPRICE_CALC` - Price calculation view (Chapter 7)

## Authorization Objects
- `S_TABU_CLI` - Table client authorization (Chapter 6)
- `S_ADMI_FCD` - Administration functions authorization (Chapter 12)

## Application IDs
- `ZAMC_DEMO` - APC demo application (Chapter 11.1)
- `SAP_COM_0001` - Communication scenario (Chapter 7 Appendix)

## Memory Areas
- `DP_QUEUE_ENTRY` - Dispatcher queue entry structure (Chapter 3)
- `DIR_TEMP` - Temporary directory
- `DIR_DATA` - Data directory
- `MAP_SHARED` vs `MAP_PRIVATE` - Memory mapping modes (Chapter 4)

## Opcodes (ABAP VM)
- `OP_PUSH_INT` - Push integer (example)
- `OP_ADD` - Addition operation (example)
- `CHECK_AUTH` - Authorization check opcode (Chapter 5)

## Dumps/Errors
- `TIME_OUT` - Timeout runtime error
- `MEMORY_NO_MORE_PAGING` - Memory exhaustion
- `RFC_ERROR_SYSTEM_FAILURE` - RFC system failure

## Notes for Verification

1. **Classes with `/` prefix** are typically namespace-protected SAP standard classes
2. **Function modules starting with `Z`** are custom examples
3. **Transactions starting with `S`** are typically SAP standard
4. **Some objects are marked as "disputed" or "doesn't exist"** - these need special verification
5. **Version dependencies** are critical - many features are version-specific (e.g., bgRFC from NetWeaver 7.11)

## Verification Priority

### High Priority (Core functionality)
- All Gateway/OData classes and transactions
- Memory management classes and parameters
- Performance monitoring transactions (ST*)
- ABAP VM related classes

### Medium Priority (Examples and utilities)
- Custom function modules (Z*)
- Service definitions and bindings
- CDS views

### Low Priority (Already verified or examples)
- Standard transactions (SM*, SE*)
- Well-known parameters
- Example opcodes