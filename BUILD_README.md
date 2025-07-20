# ABAP Deep Dive - Build System

## Overview

This build system creates PDF and EPUB versions of the ABAP Deep Dive book with automatic version management.

## Features

- **Automatic versioning**: Uses date-version-build format (e.g., `20241220-v1.0-b1`)
- **Multiple formats**: Generates both PDF and EPUB
- **Mermaid diagram support**: Renders Mermaid diagrams in PDF
- **Incremental builds**: Build numbers auto-increment
- **Clean output**: Organized build directory

## Prerequisites

The build system requires:
- Python 3.6+
- pandoc
- mermaid-filter (for diagram rendering)
- LaTeX (lualatex) for PDF generation

## Installation

### Quick Install (macOS/Linux)

```bash
# Using Make
make install-deps

# Or using shell script
./build.sh install-deps
```

### Manual Installation

1. **pandoc**: https://pandoc.org/installing.html
2. **mermaid-filter**: `npm install -g mermaid-filter`
3. **LaTeX**: 
   - macOS: `brew install --cask mactex`
   - Linux: `sudo apt-get install texlive-full`

## Usage

### Using Make

```bash
# Build all formats
make

# Build only PDF
make pdf

# Build only EPUB
make epub

# Clean build artifacts
make clean

# Check dependencies
make check-deps

# Increment versions
make version-major  # 1.0 -> 2.0
make version-minor  # 1.0 -> 1.1
```

### Using Shell Script

```bash
# Build all formats
./build.sh

# Build only PDF
./build.sh pdf

# Build only EPUB
./build.sh epub

# Other commands same as Make
```

### Using Python Directly

```bash
# Build all formats
python3 build.py --all

# Build specific format
python3 build.py --pdf
python3 build.py --epub

# Version management
python3 build.py --increment-major --pdf
python3 build.py --increment-minor --epub
```

## Version Management

Version format: `YYYYMMDD-vMAJOR.MINOR-bBUILD`

Example: `20241220-v1.0-b1`
- Date: 2024-12-20
- Version: 1.0
- Build: 1

Version info is stored in `version.json`:
```json
{
  "major": 1,
  "minor": 0,
  "build_number": 1,
  "last_build": "2024-12-20T10:30:00"
}
```

## Output

Built files are placed in the `build/` directory:
- `ABAP_Deep_Dive_YYYYMMDD-vX.Y-bZ.pdf`
- `ABAP_Deep_Dive_YYYYMMDD-vX.Y-bZ.epub`

## Troubleshooting

### Missing Dependencies

Run `make check-deps` to verify all tools are installed.

### Mermaid Diagrams Not Rendering

If mermaid-filter fails, the build will continue without diagrams. Install it with:
```bash
npm install -g mermaid-filter
```

### PDF Build Fails

Ensure LaTeX is installed and lualatex is in PATH:
```bash
which lualatex
```

### Font Issues

The build uses DejaVu fonts for Unicode support. On some systems you may need:
```bash
# Ubuntu/Debian
sudo apt-get install fonts-dejavu

# macOS (usually pre-installed)
```

## Development

To modify the build process, edit:
- `build.py` - Main build logic
- `Makefile` - Make targets
- `build.sh` - Shell script wrapper

## Chapter Management

Chapters are defined in `build.py` in the `CHAPTERS` list. Add new chapters there to include them in the build.