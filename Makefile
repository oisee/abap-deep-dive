# ABAP Deep Dive Book Builder Makefile

.PHONY: all pdf epub clean install-deps check-deps help version-major version-minor release vector-release

# Default target - builds recommended vector versions
all: pdf-vector pdf-vector-a5 epub

# Build PDF version
pdf:
	@echo "📄 Building PDF version..."
	@python3 build.py --pdf

# Build PDF with SVG diagrams
pdf-svg:
	@echo "📄 Building PDF version with SVG diagrams..."
	@python3 build_v2.py --pdf

# Build PDF with PNG diagrams
pdf-png:
	@echo "📄 Building PDF version with PNG diagrams..."
	@python3 build_v2.py --pdf --diagram-format=png

# Build PDF with PDF diagrams (RECOMMENDED)
pdf-vector:
	@echo "📄 Building PDF version with vector diagrams (RECOMMENDED)..."
	@python3 build_v2.py --pdf --diagram-format=pdf

# Build PDF in A5 format with PDF diagrams (RECOMMENDED)
pdf-vector-a5:
	@echo "📱 Building PDF version in A5 format with vector diagrams (RECOMMENDED)..."
	@python3 build_v2.py --pdf-a5 --diagram-format=pdf

# Legacy targets for compatibility
pdf-pdf: pdf-vector
pdf-pdf-a5: pdf-vector-a5

# Build EPUB version  
epub:
	@echo "📱 Building EPUB version..."
	@python3 build.py --epub

# Build all formats (legacy)
build: 
	@echo "📚 Building all formats..."
	@python3 build.py --all

# Build release-ready files (vector versions only)
release: clean pdf-vector pdf-vector-a5 epub
	@echo "🚀 Building release-ready files..."
	@mkdir -p release
	@cp build/ABAP_Deep_Dive_*-v*.pdf release/
	@cp build/ABAP_Deep_Dive_*.epub release/
	@echo "✅ Release files created in release/ directory"
	@ls -la release/

# Build development versions (includes PNG for testing)
dev-build: pdf-vector pdf-png pdf-vector-a5 epub
	@echo "🛠️  Building development versions..."
	@echo "✅ Development build complete"

# Clean build artifacts
clean:
	@echo "🧹 Cleaning build artifacts..."
	@rm -rf build/
	@rm -f metadata.yaml
	@echo "✅ Clean complete"

# Install dependencies
install-deps:
	@echo "📦 Installing dependencies..."
	@echo "Installing pandoc..."
	@if command -v brew >/dev/null 2>&1; then \
		brew install pandoc; \
	elif command -v apt-get >/dev/null 2>&1; then \
		sudo apt-get update && sudo apt-get install -y pandoc; \
	else \
		echo "❌ Please install pandoc manually from https://pandoc.org/installing.html"; \
	fi
	@echo "Installing mermaid-filter..."
	@npm install -g mermaid-filter
	@echo "Installing LaTeX (this may take a while)..."
	@if command -v brew >/dev/null 2>&1; then \
		brew install --cask mactex; \
	elif command -v apt-get >/dev/null 2>&1; then \
		sudo apt-get install -y texlive-full; \
	else \
		echo "❌ Please install LaTeX manually"; \
	fi
	@echo "✅ Dependencies installed"

# Check dependencies
check-deps:
	@python3 build.py --check-deps

# Increment major version
version-major:
	@echo "⬆️  Incrementing major version..."
	@python3 build.py --increment-major --pdf

# Increment minor version
version-minor:
	@echo "⬆️  Incrementing minor version..."
	@python3 build.py --increment-minor --pdf

# Help
help:
	@echo "📚 ABAP Deep Dive Book Builder"
	@echo ""
	@echo "🎯 RECOMMENDED TARGETS:"
	@echo "  make                  - Build recommended formats (vector PDFs + EPUB)"
	@echo "  make release          - Build production-ready release files"
	@echo "  make pdf-vector       - Build A4 PDF with vector diagrams (6.6MB)"
	@echo "  make pdf-vector-a5    - Build A5 PDF with vector diagrams (6.6MB)"
	@echo ""
	@echo "📄 PDF FORMATS:"
	@echo "  make pdf              - Build basic PDF (text only, 2.3MB)"
	@echo "  make pdf-svg          - Build PDF with SVG diagrams"
	@echo "  make pdf-png          - Build PDF with PNG diagrams (large file)"
	@echo ""
	@echo "📱 OTHER FORMATS:"
	@echo "  make epub             - Build EPUB for e-readers (2.8MB)"
	@echo ""
	@echo "🛠️  MAINTENANCE:"
	@echo "  make clean            - Remove build artifacts"
	@echo "  make install-deps     - Install required dependencies"
	@echo "  make check-deps       - Check if dependencies are installed"
	@echo "  make dev-build        - Build development versions (all formats)"
	@echo ""
	@echo "📈 VERSIONING:"
	@echo "  make version-major    - Increment major version and build"
	@echo "  make version-minor    - Increment minor version and build"
	@echo ""
	@echo "💡 EXAMPLES:"
	@echo "  make                  - Quick build (recommended formats)"
	@echo "  make release          - Production build for GitHub release"
	@echo "  make clean release    - Clean rebuild for release"
	@echo "  make pdf-vector       - Single vector PDF for development"
	@echo ""
	@echo "📊 FILE SIZES (approximate):"
	@echo "  Vector PDF A4:    6.6MB  (192 diagrams, scalable)"
	@echo "  Vector PDF A5:    6.6MB  (192 diagrams, mobile-friendly)"
	@echo "  Basic PDF:        2.3MB  (text only, no diagrams)"
	@echo "  EPUB:             2.8MB  (mobile/e-reader optimized)"