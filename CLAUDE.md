# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Repository Overview

This repository contains a comprehensive technical documentation about SAP ABAP architecture, internals, and evolution. It's a Russian-language deep dive into SAP systems, structured as a multi-chapter technical book.

## Repository Structure

The documentation is organized as follows:
- `ADD - ABAP Deep Dive.md` - Main table of contents and overview
- Individual chapter files covering specific topics (Глава 1-12)

## Content Architecture

The documentation covers:

### Part I: Fundamental Architecture
- SAP system anatomy and three-tier architecture
- SAP kernel as the operating system for business applications
- Work Process internals and execution model

### Part II: Memory Management and Code Execution
- Memory hierarchy and scalability considerations
- ABAP Virtual Machine implementation
- Database Interface bridging ABAP and DBMS

### Part III: Platform Evolution
- Architectural evolution from R/2 to S/4HANA
- SAP HANA as more than a database

### Part IV: Modern Frameworks and Cloud
- SADL and Gateway for REST API automation
- Evolution from BOPF to RAP (RESTful Application Programming)
- ABAP Daemons and Channels for reactive architecture

### Part V: Debugging and Performance Analysis
- Analysis tools and performance optimization

## Key Technical Concepts

The documentation uses extensive technical diagrams (mermaid format) and covers:
- SAP Instance = `disp+work` process + shared memory + configuration
- Work Process types: DIA (dialog), BTC (background), UPD (update), ENQ (locks), SPO (spool)
- ASCS (ABAP System Central Services) components
- Memory management tiers and optimization strategies
- ABAP VM internals and code execution flow

## Working with This Repository

This is a documentation-only repository focused on deep technical understanding of SAP ABAP systems. When making changes:
- Maintain consistency with existing technical depth and style
- Use mermaid diagrams for architectural illustrations
- Keep the Russian language consistent throughout
- Focus on technical accuracy and architectural details

## Technical Fact-Checking Protocol

When fact-checking chapters, use the following systematic approach:

### Role and Expertise
You are a technical fact-checker with SAP Press editorial board level expertise. Specialization: SAP architecture, ABAP, HANA, S/4HANA, and SAP cloud technologies.

### Fact-Checking Process

1. **Extract and Categorize Claims**
   - Technical facts requiring verification: [F.1], [F.2]...
   - Missing examples or data: [TODO.1], [TODO.2]...
   - Code snippets to validate: [CODE.1], [CODE.2]...

2. **TODO Categories**
   - `TODO_EXAMPLE`: Need concrete example (code, configuration)
   - `TODO_VERIFY`: Need verification in live SAP system
   - `TODO_SOURCE`: Need authoritative source/SAP Note
   - `TODO_DETAIL`: Need technical specifications
   - `TODO_VISUAL`: Need diagram or screenshot
   - `TODO_UPDATE`: Information outdated, needs current data

3. **Verification Criteria**
   - **Process Names**: Verify exact executable names for Unix/Linux vs Windows
   - **Memory Limits**: Check against current SAP parameter documentation
   - **Performance Metrics**: Require specific numbers with context
   - **Code Examples**: Must be syntactically correct and follow best practices
   - **Historical Facts**: Need dates and version numbers
   - **Architecture Claims**: Verify against official SAP documentation

4. **Known Facts Reference**
   ```
   Message Server: ms.sap<SID> (Unix), msg_server.exe (Windows)
   Dispatcher: disp+work (both platforms)
   SAP GUI: Thin client (presentation layer only)
   Work Process Types: DIA, BTC, UPD, UPD2, ENQ, SPO
   ICM: Integrated into disp+work since 6.40
   ```

5. **Output Format**
   Create two files in `factcheck/` directory:
   
   a) `factcheck/issues/FACTCHECK_Chapter_X.md`:
   ```markdown
   # Факт-чек: [Chapter Name]
   Дата проверки: [date]
   Статус: 🔴 Требует доработки | 🟡 Минорные правки | 🟢 Готово
   
   ## Сводка
   - Проверено фактов: X
   - Найдено проблем: Y
   - Критических ошибок: Z
   - TODO items: N
   
   ## Критические проблемы (HIGH)
   ### ❌ [F.X] [Issue Title]
   **Строка**: [line number]
   **Написано**: "[original text]"
   **Должно быть**: "[correct text]"
   **Источник**: [SAP Note/Documentation]
   ```
   
   b) `factcheck/todo/TODO_Chapter_X.md`:
   ```markdown
   # TODO: [Chapter Name]
   
   ## Высокий приоритет
   - [ ] [TODO.X] Description
     - Контекст: [where in chapter]
     - Что нужно: [specific requirement]
     - Как получить: [steps or sources]
   ```

6. **Common Issues to Check**
   - Vague phrases: "примерно так", "что-то вроде", "упрощенно"
   - Missing concrete examples after "Например:" or "Пример:"
   - Outdated references: R/3, ECC 6.0 specific features
   - Performance claims without numbers
   - Pseudocode without real implementation
   - Process/executable names accuracy
   - Memory parameters and limits
   - Transaction codes and function modules

7. **Code Validation Rules**
   - ABAP: No SELECT *, no obsolete statements (MOVE, COMPUTE)
   - SQL: Check for logical consistency (e.g., SELECT SINGLE with ORDER BY)
   - All code must be complete and runnable (no "..." or incomplete snippets)

### Fact-Checking Workflow
1. Read chapter completely first
2. Extract all verifiable claims
3. Identify missing content and vague statements
4. Verify technical facts against SAP documentation
5. Validate all code examples
6. Generate structured reports in factcheck/ subdirectories
7. Maintain running statistics in factcheck/README.md