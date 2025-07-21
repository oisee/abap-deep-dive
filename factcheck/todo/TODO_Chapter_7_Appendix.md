# TODO List: Chapter 7 Appendix - SAP Protocols

**Date**: 2025-07-21  
**Chapter**: Приложение к главе 7: Протоколы SAP - открытость и внутреннее устройство  
**Priority**: High - Security and protocol accuracy is critical

## High Priority Tasks

### 1. DIAG Protocol Section

#### 1.1 Add Complete Port Information
- [ ] Add standard DIAG port: 3200 + instance number
- [ ] Add dispatcher port: 3200-3299 range
- [ ] Add secure DIAG port information (SNC)

#### 1.2 Fix Compression Information
- [ ] Replace LZH/LZC with correct LZF compression
- [ ] Add compression ratio statistics (60-80% for screens)
- [ ] Add compression negotiation details

#### 1.3 Add Security Details
- [ ] Add SNC (Secure Network Communications) configuration
- [ ] Add TLS/SSL support details (NetWeaver 7.40+)
- [ ] Add encryption cipher suites supported
- [ ] Add certificate-based authentication

#### 1.4 Add Binary Format Examples
```
Example needed:
DIAG Header (16 bytes):
Offset  Size  Description
0x00    1     Mode (0xFF for DIAG)
0x01    1     Communication flags
0x02    1     Mode2
0x03    1     Reserved
0x04    4     Uncompressed length (big-endian)
0x08    4     Compressed length (big-endian)
0x0C    4     Reserved
```

### 2. RFC Protocol Section

#### 2.1 Add Complete Port Information
- [ ] Gateway port: 3300 + instance (sapgwNN)
- [ ] Program registration port: 4800 + instance
- [ ] Internal RFC port: 3900 + instance

#### 2.2 Add RFC Variants
- [ ] sRFC - Synchronous RFC (basic)
- [ ] tRFC - Transactional RFC (exactly once)
- [ ] qRFC - Queued RFC (ordered delivery)
- [ ] bgRFC - Background RFC (NetWeaver 7.11+)
- [ ] WebSocket RFC (NetWeaver 7.50+)

#### 2.3 Add Code Examples
```abap
" Example: tRFC call with error handling
CALL FUNCTION 'Z_UPDATE_MATERIAL' IN BACKGROUND TASK
  DESTINATION 'RFC_DEST'
  EXPORTING
    iv_matnr = '000000001'
    iv_maktx = 'Updated description'
  EXCEPTIONS
    communication_failure = 1
    system_failure       = 2.

IF sy-subrc = 0.
  COMMIT WORK.
ENDIF.
```

#### 2.4 Add Performance Metrics
- [ ] Latency measurements (LAN vs WAN)
- [ ] Throughput benchmarks
- [ ] Connection pooling benefits

### 3. ADT Protocol Section

#### 3.1 Fix REST Endpoints
- [ ] Add version-specific endpoints
- [ ] Add complete URI patterns
- [ ] Add request/response examples with headers

#### 3.2 Add Authentication Examples
```http
# Basic Authentication
GET /sap/bc/adt/discovery HTTP/1.1
Host: sap-dev.company.com:8000
Authorization: Basic <base64(user:password)>
X-CSRF-Token: Fetch

# SAML Assertion
POST /sap/bc/adt/saml2/login HTTP/1.1
Host: sap-dev.company.com:8000
Content-Type: application/x-www-form-urlencoded
SAMLResponse=<base64-encoded-saml-response>
```

#### 3.3 Add ADT-specific Features
- [ ] CSRF token handling
- [ ] Stateful vs stateless modes
- [ ] Eclipse negotiation headers
- [ ] Content negotiation examples

### 4. Message Server Protocol

#### 4.1 Fix Port Numbers
- [ ] Internal MS port: 3900 + instance
- [ ] HTTP port: 8100 + instance  
- [ ] HTTPS port: 44300 + instance
- [ ] J2EE MS port: 3900 + instance + 100

#### 4.2 Add Protocol Operations
- [ ] Logon sequence with handshake
- [ ] Server list request format
- [ ] Load balancing algorithm
- [ ] Failover mechanism

#### 4.3 Add Binary Examples
```
MS_LOGIN_2 packet:
00: 00 00 00 4D  // Length
04: FF FF FF FF  // Flag (-1)
08: 04 00        // Opcode (MS_LOGIN_2)
0A: ...          // Client info
```

### 5. Gateway Protocol Section

#### 5.1 Fix Security File Names
- [ ] Change reg_info to reginfo
- [ ] Change sec_info to secinfo  
- [ ] Add prxyinfo for proxy settings
- [ ] Add ACL examples

#### 5.2 Add gwmon Commands
```
# Example gwmon commands
gwmon pf=<profile>
1. Registered programs     -> Show external programs
2. Logged on clients      -> Show RFC clients
3. Gateway statistics     -> Performance metrics
4. Security info          -> Show security settings
```

#### 5.3 Add Security Configuration
```
# reginfo example
P TP=* HOST=* ACCESS=*

# secinfo example  
P TP=SAPXPG HOST=internal ACCESS=*
D TP=* HOST=external ACCESS=*
```

### 6. Add Missing Protocols

#### 6.1 APC (ABAP Push Channel)
- [ ] WebSocket protocol details
- [ ] Message format (JSON/XML)
- [ ] Connection lifecycle
- [ ] Code example:
```abap
CLASS lcl_apc_handler DEFINITION INHERITING FROM cl_apc_wsp_ext_stateless_base.
  PUBLIC SECTION.
    METHODS: if_apc_wsp_ext~on_message REDEFINITION.
ENDCLASS.
```

#### 6.2 SAPRouter Protocol
- [ ] Route string format
- [ ] Encryption with SNC
- [ ] Port 3299 details
- [ ] Configuration examples

#### 6.3 CPIC Protocol
- [ ] Protocol layers
- [ ] Conversation states
- [ ] Error handling
- [ ] Integration with RFC

### 7. Add Security Best Practices

#### 7.1 Protocol-Specific Security
- [ ] DIAG: Enforce SNC, disable weak ciphers
- [ ] RFC: Use SSL, implement authorization checks
- [ ] Gateway: Restrict reginfo/secinfo, enable logging
- [ ] MS: Use HTTPS, implement ACLs

#### 7.2 Common Vulnerabilities
- [ ] CVE references for each protocol
- [ ] Mitigation strategies
- [ ] Security notes references

### 8. Add Performance Optimization

#### 8.1 Protocol Tuning
- [ ] DIAG: Compression settings, keep-alive
- [ ] RFC: Connection pooling, load balancing
- [ ] ADT: Caching strategies, batch operations

#### 8.2 Monitoring and Troubleshooting
- [ ] Transaction codes for each protocol
- [ ] Log file locations
- [ ] Common error messages

### 9. Add Practical Examples

#### 9.1 Packet Capture Analysis
- [ ] Wireshark filter examples
- [ ] tcpdump commands
- [ ] Binary analysis tools

#### 9.2 Client Implementation
- [ ] Python examples with pysap
- [ ] Java examples with JCo
- [ ] Node.js examples with node-rfc

### 10. Add Version Matrix

Create a table showing protocol features by SAP version:

| Protocol | Feature | 7.0 | 7.40 | 7.50 | S/4HANA |
|----------|---------|-----|------|------|---------|
| DIAG | SNC | ✓ | ✓ | ✓ | ✓ |
| DIAG | TLS | ✗ | ✓ | ✓ | ✓ |
| RFC | bgRFC | ✗ | ✓ | ✓ | ✓ |
| RFC | WebSocket | ✗ | ✗ | ✓ | ✓ |
| ADT | REST API | ✗ | ✓ | ✓ | ✓ |
| APC | WebSocket | ✗ | ✗ | ✓ | ✓ |

## Medium Priority Tasks

### 11. Improve Diagrams
- [ ] Add sequence diagrams for protocol flows
- [ ] Add timing diagrams for performance
- [ ] Add security architecture diagrams

### 12. Add References
- [ ] SAP Help Portal links for each protocol
- [ ] Relevant SAP Notes
- [ ] Books and publications
- [ ] Community resources

### 13. Add Glossary
- [ ] Protocol-specific terminology
- [ ] Abbreviations and acronyms
- [ ] Common error codes

## Low Priority Tasks

### 14. Formatting Improvements
- [ ] Consistent code highlighting
- [ ] Better table formatting
- [ ] Clear section separators

### 15. Additional Tools
- [ ] SAP Solution Manager capabilities
- [ ] Third-party monitoring tools
- [ ] Development tools

## Completion Checklist

- [ ] All port numbers verified and correct
- [ ] All protocol versions documented
- [ ] Security configurations complete
- [ ] Code examples compile and run
- [ ] Binary formats documented
- [ ] Performance metrics included
- [ ] Version compatibility matrix complete
- [ ] All diagrams technically accurate
- [ ] References to official documentation
- [ ] Security best practices included

## Resources Needed

1. Access to SAP system for testing
2. SAP documentation portal access
3. Protocol analysis tools
4. Network packet captures
5. SAP Notes access

## Estimated Effort

- High Priority: 40 hours
- Medium Priority: 20 hours  
- Low Priority: 10 hours
- Total: ~70 hours

## Next Steps

1. Start with fixing critical port and security information
2. Add concrete code examples that can be tested
3. Create accurate binary format documentation
4. Validate all technical claims with SAP documentation