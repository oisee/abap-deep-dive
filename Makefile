# ABAP Deep Dive Book Builder Makefile

.PHONY: all pdf epub clean install-deps check-deps help version-major version-minor

# Default target
all: pdf epub

# Build PDF version
pdf:
	@echo "📄 Building PDF version..."
	@python3 build.py --pdf

# Build EPUB version  
epub:
	@echo "📱 Building EPUB version..."
	@python3 build.py --epub

# Build all formats
build: 
	@echo "📚 Building all formats..."
	@python3 build.py --all

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
	@echo "ABAP Deep Dive Book Builder"
	@echo ""
	@echo "Available targets:"
	@echo "  make all          - Build both PDF and EPUB versions"
	@echo "  make pdf          - Build only PDF version"
	@echo "  make epub         - Build only EPUB version"
	@echo "  make clean        - Remove build artifacts"
	@echo "  make install-deps - Install required dependencies"
	@echo "  make check-deps   - Check if dependencies are installed"
	@echo "  make version-major - Increment major version and build"
	@echo "  make version-minor - Increment minor version and build"
	@echo "  make help         - Show this help message"
	@echo ""
	@echo "Examples:"
	@echo "  make              - Build all formats (PDF and EPUB)"
	@echo "  make pdf          - Build only PDF"
	@echo "  make clean all    - Clean and rebuild everything"