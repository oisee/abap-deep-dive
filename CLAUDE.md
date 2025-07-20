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