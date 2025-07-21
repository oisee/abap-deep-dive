# SAP MCP Tools Specification

## Overview
This document specifies additional MCP tools needed for comprehensive SAP system analysis and fact-checking. These tools will enable deep inspection of SAP configuration, metadata, and runtime data.

## 1. Table Content Reader API

### Purpose
Generic table content reader similar to SE16/SE16N functionality, allowing read access to any SAP table with proper authorization checks.

### Endpoints

#### GET /TableContentReader/{table_name}
Read table contents with filtering and pagination support.

**Parameters:**
- `table_name` (string, required): SAP table name (e.g., T000, USR02, DD03L)
- `select` (string, optional): Comma-separated list of fields to return
- `filter` (string, optional): OData-style filter expression
- `orderby` (string, optional): Sort order
- `top` (integer, optional): Maximum rows to return (default: 100, max: 1000)
- `skip` (integer, optional): Number of rows to skip
- `count` (boolean, optional): Include total row count
- `client` (string, optional): Client number (default: current client)

**Response:**
```json
{
  "value": [
    {
      "field1": "value1",
      "field2": "value2"
    }
  ],
  "count": 1234  // if requested
}
```

#### GET /TableContentReader/{table_name}/$metadata
Get table structure and metadata.

**Response:**
```json
{
  "table_name": "T000",
  "description": "Clients",
  "delivery_class": "C",
  "fields": [
    {
      "fieldname": "MANDT",
      "datatype": "CLNT",
      "length": 3,
      "decimals": 0,
      "key": true,
      "description": "Client"
    }
  ]
}
```

### Security Considerations
- Implement authorization check using S_TABU_DIS
- Log access to sensitive tables
- Exclude tables with special protection (e.g., password tables)

## 2. System Parameters API

### Purpose
Read SAP instance and system profile parameters, similar to RZ10/RZ11 functionality.

### Endpoints

#### GET /SystemParameters
List all profile parameters with current values.

**Parameters:**
- `filter` (string, optional): Filter by parameter name pattern
- `profile` (string, optional): Filter by profile (DEFAULT, INSTANCE)
- `dynamic_only` (boolean, optional): Show only dynamically changeable parameters

**Response:**
```json
{
  "value": [
    {
      "parameter": "rdisp/max_wprun_time",
      "current_value": "600",
      "default_value": "600",
      "profile_value": "1200",
      "source": "INSTANCE_PROFILE",
      "dynamic": true,
      "description": "Maximum work process runtime"
    }
  ]
}
```

#### GET /SystemParameters/{parameter_name}
Get detailed information about a specific parameter.

**Response:**
```json
{
  "parameter": "rdisp/ROLL_SHM",
  "current_value": "16384",
  "default_value": "16384",
  "profile_value": null,
  "source": "DEFAULT",
  "dynamic": false,
  "unit": "8KB blocks",
  "description": "Roll area in shared memory",
  "documentation": "...",
  "valid_values": "1-2147483647",
  "kernel_release": "753"
}
```

## 3. Transaction Metadata API

### Purpose
Check existence and properties of SAP transactions, similar to SE93 functionality.

### Endpoints

#### GET /Transactions/{tcode}
Get transaction details.

**Response:**
```json
{
  "tcode": "ST05",
  "program": "RSMON000_ALV",
  "screen": "0100",
  "transaction_type": "P",
  "description": "Performance Trace",
  "package": "STUN",
  "authorization_object": "S_ADMI_FCD",
  "locked": false,
  "classification": "Professional User Transaction"
}
```

#### GET /Transactions
Search transactions by pattern.

**Parameters:**
- `filter` (string, optional): Filter expression (e.g., startswith(tcode, 'ST'))
- `select` (string, optional): Fields to return
- `top` (integer, optional): Maximum results

## 4. HANA System Views API

### Purpose
Access HANA monitoring views (M_* tables) for system analysis.

### Endpoints

#### GET /HANASystemViews/{view_name}
Read HANA system monitoring views.

**Supported Views:**
- M_CS_TABLES (Column Store Tables)
- M_RS_TABLES (Row Store Tables)
- M_TABLE_PARTITIONS
- M_HEAP_MEMORY
- M_SQL_PLAN_CACHE
- M_CONNECTION_STATISTICS
- M_SERVICE_MEMORY
- M_SYSTEM_OVERVIEW

**Parameters:**
- Same as Table Content Reader API

**Example Response for M_CS_TABLES:**
```json
{
  "value": [
    {
      "SCHEMA_NAME": "SAPABAP1",
      "TABLE_NAME": "VBAK",
      "RECORD_COUNT": 1500000,
      "MEMORY_SIZE_IN_MAIN": 256000000,
      "MEMORY_SIZE_IN_DELTA": 12000000,
      "COMPRESSION_RATIO": 5.2,
      "LAST_MERGE_TIME": "2024-01-15T10:30:00Z"
    }
  ]
}
```

## 5. CDS Metadata API

### Purpose
Access CDS view definitions and metadata, including annotations and associations.

### Endpoints

#### GET /CDSViews/{view_name}
Get CDS view definition and metadata.

**Response:**
```json
{
  "view_name": "I_SalesOrder",
  "ddl_source": "...",
  "base_tables": ["VBAK", "VBAP"],
  "annotations": {
    "@ObjectModel.dataCategory": "#FACT",
    "@Analytics.dataExtraction.enabled": "true",
    "@VDM.viewType": "#BASIC"
  },
  "fields": [
    {
      "field": "SalesOrder",
      "type": "vbeln",
      "key": true,
      "annotations": {
        "@EndUserText.label": "Sales Order"
      }
    }
  ],
  "associations": [
    {
      "name": "_Customer",
      "target": "I_Customer",
      "cardinality": "[0..1]"
    }
  ]
}
```

#### GET /CDSViews
List CDS views with filtering.

**Parameters:**
- `filter` (string, optional): Filter by name, annotation, or base table
- `include_private` (boolean, optional): Include private views
- `vdm_type` (string, optional): Filter by VDM type (BASIC, COMPOSITE, etc.)

## 6. AMDP Registry API

### Purpose
Access ABAP Managed Database Procedure definitions and metadata.

### Endpoints

#### GET /AMDPProcedures/{class_name}/{method_name}
Get AMDP method implementation details.

**Response:**
```json
{
  "class": "ZCL_DEMO_AMDP",
  "method": "GET_SALES_ANALYTICS",
  "interface_marker": "IF_AMDP_MARKER_HDB",
  "database_type": "HDB",
  "language": "SQLSCRIPT",
  "options": ["READ-ONLY"],
  "using_objects": ["VBAK", "VBAP", "KNA1"],
  "sqlscript_source": "...",
  "parameters": [
    {
      "name": "IV_DATE_FROM",
      "type": "DATS",
      "direction": "IMPORTING"
    }
  ]
}
```

#### GET /AMDPProcedures
List all AMDP procedures in the system.

**Parameters:**
- `filter` (string, optional): Filter by class or package
- `database_type` (string, optional): Filter by DB type (HDB)

## 7. Authorization Check API

### Purpose
Check if current user has authorization for specific operations.

### Endpoints

#### POST /AuthorizationCheck
Check authorization for an operation.

**Request Body:**
```json
{
  "object": "S_TABU_DIS",
  "field_values": {
    "ACTVT": "03",
    "DICBERCLS": "SS"
  }
}
```

**Response:**
```json
{
  "authorized": true,
  "checked_values": {
    "ACTVT": "03",
    "DICBERCLS": "SS"
  }
}
```

## Implementation Notes

### General Principles
1. All APIs should follow OData v4 conventions where applicable
2. Support for $count, $top, $skip, $orderby, $select, $filter
3. Consistent error handling with meaningful messages
4. Performance optimization for large datasets
5. Respect SAP authorization model

### Error Responses
Standard HTTP status codes with detailed error information:
```json
{
  "error": {
    "code": "TABLE_NOT_FOUND",
    "message": "Table ZTABLE does not exist",
    "details": {
      "table_name": "ZTABLE",
      "checked_in_client": "100"
    }
  }
}
```

### Performance Considerations
1. Implement server-side pagination
2. Use database hints for optimal access paths
3. Cache metadata where appropriate
4. Implement query timeout mechanisms
5. Log and monitor expensive operations

### Security Model
1. All operations require valid SAP user session
2. Authorization checks before data access
3. Audit logging for sensitive operations
4. Data masking for sensitive fields
5. Rate limiting to prevent abuse

## Usage Examples

### Example 1: Check if transaction exists
```
GET /Transactions/ST05
```

### Example 2: Read system parameter
```
GET /SystemParameters/rdisp%2FROLL_SHM
```

### Example 3: Analyze HANA table memory usage
```
GET /HANASystemViews/M_CS_TABLES?$filter=MEMORY_SIZE_IN_MAIN gt 1000000000&$orderby=MEMORY_SIZE_IN_MAIN desc&$top=10
```

### Example 4: Find CDS views using specific table
```
GET /CDSViews?$filter=contains(base_tables, 'VBAK')
```

### Example 5: Read table with authorization check
```
GET /TableContentReader/T000?$select=MANDT,MTEXT&$orderby=MANDT
```

## Testing Requirements

1. Unit tests for each endpoint
2. Integration tests with real SAP system
3. Performance tests with large datasets
4. Security tests for authorization checks
5. Error handling tests

## Documentation Requirements

1. OpenAPI/Swagger specification
2. Usage examples for each endpoint
3. Authorization requirements documentation
4. Performance tuning guide
5. Troubleshooting guide