# Fact-Check Report: Chapter 7 Appendix - SAP Protocols

**Date**: 2025-07-21  
**Chapter**: Приложение к главе 7: Протоколы SAP - открытость и внутреннее устройство  
**Status**: 🔴 Critical issues found

## Executive Summary

The appendix contains several technical inaccuracies, missing critical information, and unverified claims about SAP protocols. While the general structure is good, many technical details need correction and expansion.

## Critical Issues

### 1. DIAG Protocol

#### 1.1 Port Information Missing
- **Issue**: No mention of standard DIAG port (3200 + instance number)
- **Correction**: DIAG typically uses port 32NN where NN is the instance number

#### 1.2 Compression Details Incorrect
- **Issue**: Claims LZH/LZC compression, but modern DIAG uses different algorithms
- **Correction**: Modern DIAG uses LZF (Lempel-Ziv-Fast) compression, not LZH/LZC

#### 1.3 Security Claims
- **Issue**: States "encryption is absent by default" without version context
- **Correction**: Since NetWeaver 7.40, SNC (Secure Network Communications) can be enforced. Modern systems support TLS encryption for DIAG

#### 1.4 DIAG Header Structure
- **Issue**: Pseudo-code structure is oversimplified and potentially incorrect
- **Correction**: DIAG header is more complex, includes version info, compression flags, and message types

### 2. RFC Protocol

#### 2.1 Missing Port Information
- **Issue**: No mention of RFC gateway port (3300 + instance number)
- **Correction**: RFC uses gateway port 33NN, program port 48NN

#### 2.2 Incomplete Protocol Layers
- **Issue**: Shows CPI-C layer but missing tRFC, qRFC, bgRFC variants
- **Correction**: Should include:
  - Synchronous RFC (sRFC)
  - Transactional RFC (tRFC) 
  - Queued RFC (qRFC)
  - Background RFC (bgRFC) - since NetWeaver 7.11

#### 2.3 Missing Security Information
- **Issue**: No mention of RFC security mechanisms
- **Correction**: RFC supports SNC, SSL/TLS (since 7.20), and authentication methods

### 3. ADT Protocol

#### 3.1 Incorrect REST Endpoints
- **Issue**: Shows generic endpoints without version information
- **Correction**: ADT endpoints vary by NetWeaver version:
  - `/sap/bc/adt/discovery` (7.31+)
  - `/sap/bc/adt/repository/informationsystem` (7.40+)
  - `/sap/bc/adt/vit` (7.50+ for ABAP Unit)

#### 3.2 Missing Authentication Methods
- **Issue**: Lists OAuth 2.0 which is not standard for on-premise
- **Correction**: Standard methods are:
  - Basic Authentication (most common)
  - SAML 2.0 (enterprise scenarios)
  - X.509 certificates
  - OAuth 2.0 only for BTP/Cloud scenarios

### 4. Message Server Protocol

#### 4.1 Incorrect Port Numbers
- **Issue**: Shows "3900 + Instance" which is incorrect
- **Correction**: Message Server uses:
  - Internal port: 39NN (NN = instance)
  - HTTP port: 81NN
  - HTTPS port: 44300 + instance

#### 4.2 Missing Protocol Details
- **Issue**: No mention of MS protocol versions or handshake
- **Correction**: MS protocol has versions (currently v6), specific handshake sequence

### 5. Gateway Protocol

#### 5.1 Missing Security Files
- **Issue**: Shows only reg_info and sec_info
- **Correction**: Modern Gateway uses:
  - `reginfo` (not reg_info)
  - `secinfo` (not sec_info)
  - `prxyinfo` (proxy information)
  - `reginfo.dat` and `secinfo.dat` are deprecated names

### 6. General Protocol Issues

#### 6.1 Missing APC (ABAP Push Channel) Protocol
- **Issue**: Title mentions APC but no content about it
- **Correction**: Should include APC WebSocket protocol details

#### 6.2 No Version Information
- **Issue**: Protocol details don't specify SAP versions
- **Correction**: Each protocol feature should specify minimum SAP version

#### 6.3 Missing Protocol Performance Metrics
- **Issue**: No compression rates or performance characteristics
- **Correction**: Should include typical compression ratios, latency, throughput

## Major Omissions

### 1. Missing Protocols
- **SAPRouter protocol** - Critical for network security
- **CPIC protocol details** - Foundation for RFC
- **P4 protocol** - Used for monitoring
- **NI (Network Interface) protocol** - Low-level SAP networking

### 2. Missing Binary Format Details
- No actual binary structure examples
- No packet capture examples
- No hex dump analysis

### 3. Missing Security Vulnerabilities
- No mention of known CVEs
- No security best practices
- No mention of protocol-specific attacks

### 4. Missing Code Examples
- No ABAP code for protocol usage
- No client library examples
- No debugging/tracing examples

## Inaccurate Technical Details

### 1. Wireshark Support
- **Issue**: Claims Wireshark has DIAG dissector
- **Reality**: Wireshark DIAG dissector is limited and outdated

### 2. pysap Library
- **Issue**: Presented as comprehensive
- **Reality**: pysap is research-focused, not production-ready

### 3. Tool Recommendations
- Missing official SAP tools
- No mention of SAP Solution Manager capabilities
- No mention of SAP NetWeaver Administrator

## Recommendations for Correction

### 1. Add Concrete Examples
```abap
" Example: RFC call with explicit protocol settings
DATA: lo_dest TYPE REF TO if_rfc_dest.

cl_rfc_destination_provider=>create_by_comm_arrangement(
  EXPORTING
    comm_scenario  = 'SAP_COM_0001'
    service_id     = 'OUTBOUND_SERVICE'
  IMPORTING
    destination    = lo_dest ).
```

### 2. Add Binary Format Examples
```
DIAG Packet Structure:
00000000: FF 00 00 00 34 00 00 00  |....4...|  # Header
00000008: 10 04 02 00 53 45 53 00  |....SES.|  # SES item
```

### 3. Add Performance Metrics
- DIAG compression: typically 60-80% for screen data
- RFC latency: 1-5ms LAN, 20-100ms WAN
- ADT response time: 50-500ms depending on operation

### 4. Add Security Configuration
```
# Gateway security configuration example
gw/sec_info = $(DIR_DATA)/secinfo
gw/reg_info = $(DIR_DATA)/reginfo
gw/reg_no_conn_info = 1
gw/acl_mode = 1
```

## Sources for Verification

1. SAP Help Portal - Network and Communication Security
2. SAP Note 1408081 - Basic Settings for reg_info and sec_info
3. SAP Note 2183207 - SAP Gateway Security Guide
4. SAP Note 910919 - Setting up SSL on Application Server ABAP
5. SAP Community blogs on protocol internals

## Severity Assessment

- **Critical**: 6 issues (incorrect port numbers, missing security info)
- **Major**: 8 issues (incomplete protocol details, missing examples)
- **Minor**: 4 issues (terminology, formatting)

## Conclusion

While the appendix provides a good overview of SAP protocols, it requires significant technical corrections and additions to be accurate and useful. The focus on reverse-engineering tools overshadows official documentation and security best practices.