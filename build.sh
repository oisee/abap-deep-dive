#!/bin/bash

# ABAP Deep Dive Book Builder Script
# Alternative to Makefile for systems without make

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Functions
print_help() {
    echo "ABAP Deep Dive Book Builder"
    echo ""
    echo "Usage: ./build.sh [command]"
    echo ""
    echo "Commands:"
    echo "  all          - Build both PDF and EPUB versions"
    echo "  pdf          - Build only PDF version"
    echo "  epub         - Build only EPUB version"
    echo "  clean        - Remove build artifacts"
    echo "  install-deps - Install required dependencies"
    echo "  check-deps   - Check if dependencies are installed"
    echo "  version-major - Increment major version and build"
    echo "  version-minor - Increment minor version and build"
    echo "  help         - Show this help message"
    echo ""
    echo "Examples:"
    echo "  ./build.sh       - Build all formats (default)"
    echo "  ./build.sh pdf   - Build only PDF"
    echo "  ./build.sh clean - Clean build artifacts"
}

check_python() {
    if ! command -v python3 &> /dev/null; then
        echo -e "${RED}❌ Python 3 is required but not installed${NC}"
        exit 1
    fi
}

build_pdf() {
    echo -e "${BLUE}📄 Building PDF version...${NC}"
    python3 build.py --pdf
}

build_epub() {
    echo -e "${BLUE}📱 Building EPUB version...${NC}"
    python3 build.py --epub
}

build_all() {
    echo -e "${BLUE}📚 Building all formats...${NC}"
    python3 build.py --all
}

clean() {
    echo -e "${YELLOW}🧹 Cleaning build artifacts...${NC}"
    rm -rf build/
    rm -f metadata.yaml
    echo -e "${GREEN}✅ Clean complete${NC}"
}

install_deps() {
    echo -e "${BLUE}📦 Installing dependencies...${NC}"
    
    # Detect OS
    if [[ "$OSTYPE" == "darwin"* ]]; then
        # macOS
        echo "Detected macOS"
        if ! command -v brew &> /dev/null; then
            echo -e "${RED}❌ Homebrew is required. Install from https://brew.sh${NC}"
            exit 1
        fi
        
        echo "Installing pandoc..."
        brew install pandoc
        
        echo "Installing mermaid-filter..."
        npm install -g mermaid-filter
        
        echo "Installing LaTeX (this may take a while)..."
        brew install --cask mactex
        
    elif [[ "$OSTYPE" == "linux-gnu"* ]]; then
        # Linux
        echo "Detected Linux"
        
        echo "Installing pandoc..."
        sudo apt-get update && sudo apt-get install -y pandoc
        
        echo "Installing Node.js and npm if needed..."
        if ! command -v npm &> /dev/null; then
            sudo apt-get install -y nodejs npm
        fi
        
        echo "Installing mermaid-filter..."
        sudo npm install -g mermaid-filter
        
        echo "Installing LaTeX (this may take a while)..."
        sudo apt-get install -y texlive-full
        
    else
        echo -e "${RED}❌ Unsupported OS. Please install dependencies manually:${NC}"
        echo "  - pandoc: https://pandoc.org/installing.html"
        echo "  - mermaid-filter: npm install -g mermaid-filter"
        echo "  - LaTeX: https://www.latex-project.org/get/"
        exit 1
    fi
    
    echo -e "${GREEN}✅ Dependencies installed${NC}"
}

check_deps() {
    echo -e "${BLUE}🔍 Checking dependencies...${NC}"
    
    missing=()
    
    # Check pandoc
    if ! command -v pandoc &> /dev/null; then
        missing+=("pandoc")
    fi
    
    # Check mermaid-filter
    if ! command -v mermaid-filter &> /dev/null; then
        missing+=("mermaid-filter")
    fi
    
    # Check lualatex
    if ! command -v lualatex &> /dev/null; then
        missing+=("lualatex (LaTeX)")
    fi
    
    if [ ${#missing[@]} -eq 0 ]; then
        echo -e "${GREEN}✅ All dependencies are installed${NC}"
    else
        echo -e "${RED}❌ Missing dependencies:${NC}"
        for dep in "${missing[@]}"; do
            echo "  - $dep"
        done
        echo ""
        echo "Run './build.sh install-deps' to install them"
        exit 1
    fi
}

version_major() {
    echo -e "${BLUE}⬆️  Incrementing major version...${NC}"
    python3 build.py --increment-major --pdf
}

version_minor() {
    echo -e "${BLUE}⬆️  Incrementing minor version...${NC}"
    python3 build.py --increment-minor --pdf
}

# Main script
check_python

# Parse command
case "${1:-all}" in
    all)
        build_all
        ;;
    pdf)
        build_pdf
        ;;
    epub)
        build_epub
        ;;
    clean)
        clean
        ;;
    install-deps)
        install_deps
        ;;
    check-deps)
        check_deps
        ;;
    version-major)
        version_major
        ;;
    version-minor)
        version_minor
        ;;
    help|--help|-h)
        print_help
        ;;
    *)
        echo -e "${RED}Unknown command: $1${NC}"
        echo ""
        print_help
        exit 1
        ;;
esac