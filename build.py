#!/usr/bin/env python3
"""
ABAP Deep Dive Book Builder
Builds PDF and EPUB versions with auto-incrementing version numbers
"""

import os
import sys
import json
import subprocess
import shutil
from datetime import datetime
from pathlib import Path
import tempfile
import re

# Configuration
BOOK_TITLE = "ABAP Deep Dive: Архитектура, ядро и эволюция SAP"
AUTHOR = "ABAP Deep Dive Project"
VERSION_FILE = "version.json"
OUTPUT_DIR = "build"
METADATA_FILE = "metadata.yaml"

# Chapter order
CHAPTERS = [
    "ADD - ABAP Deep Dive.md",  # Main TOC
    "ADD - Глава 1  Анатомия SAP системы.md",
    "ADD - Глава 2 Ядро SAP - операционная система бизнес-приложений.md",
    "ADD - Глава 3 Work Process - микрокосм выполнения.md",
    "ADD - Глава 4 Иерархия памяти - балансировка между масштабируемостью и выживанием.md",
    "ADD - Глава 5 ABAP Virtual Machine - от исходника к исполнению.md",
    "ADD - Глава 6 Database Interface - мост между ABAP и СУБД.md",
    "ADD - Глава 7 От R2 до S4HANA - архитектурные революции.md",
    "ADD - Глава 7 Приложение Протоколы SAP - открытость и внутреннее устройство.md",
    "ADD - Глава 8 SAP HANA - больше чем база данных.md",
    "ADD - Глава 9 SADL и Gateway - автоматизация REST API.md",
    "ADD - Глава 10 От BOPF к RAP - эволюция бизнес-объектов.md",
    "ADD - Глава 11 BTP и Steampunk - ABAP в облаке.md",
    "ADD - Глава 11 ABAP Daemons и Channels - реактивная архитектура.md",
    "ADD - Глава 12 Инструменты анализа - заглядываем внутрь.md"
]

class BookBuilder:
    def __init__(self):
        self.version_info = self.load_version()
        self.build_number = self.version_info['build_number']
        self.version_string = self.get_version_string()
        
    def load_version(self):
        """Load or create version info"""
        if os.path.exists(VERSION_FILE):
            with open(VERSION_FILE, 'r') as f:
                return json.load(f)
        else:
            return {
                'major': 1,
                'minor': 0,
                'build_number': 0,
                'last_build': None
            }
    
    def save_version(self):
        """Save version info"""
        with open(VERSION_FILE, 'w') as f:
            json.dump(self.version_info, f, indent=2, ensure_ascii=False)
    
    def increment_build(self):
        """Increment build number"""
        self.version_info['build_number'] += 1
        self.version_info['last_build'] = datetime.now().isoformat()
        self.build_number = self.version_info['build_number']
        self.version_string = self.get_version_string()
        self.save_version()
    
    def get_version_string(self):
        """Generate version string: date-version-build"""
        date_str = datetime.now().strftime("%Y%m%d")
        return f"{date_str}-v{self.version_info['major']}.{self.version_info['minor']}-b{self.build_number}"
    
    def check_dependencies(self):
        """Check if required tools are installed"""
        tools = {
            'pandoc': 'pandoc --version',
            'mermaid-filter': 'mermaid-filter --version',
            'lualatex': 'lualatex --version'
        }
        
        missing = []
        optional_missing = []
        for tool, cmd in tools.items():
            try:
                subprocess.run(cmd.split(), capture_output=True, check=True, timeout=5)
            except (subprocess.CalledProcessError, FileNotFoundError, subprocess.TimeoutExpired):
                if tool == 'mermaid-filter':
                    optional_missing.append(tool)
                else:
                    missing.append(tool)
        
        if missing:
            print(f"❌ Missing required tools: {', '.join(missing)}")
            print("\nInstall them with:")
            if 'pandoc' in missing:
                print("  • brew install pandoc (macOS) or apt-get install pandoc (Linux)")
            if 'lualatex' in missing:
                print("  • brew install --cask mactex (macOS) or apt-get install texlive-full (Linux)")
            sys.exit(1)
        
        if optional_missing:
            print(f"⚠️  Optional tools missing: {', '.join(optional_missing)}")
            print("  Mermaid diagrams will not be rendered in PDF")
            print("  Install with: npm install -g mermaid-filter")
        else:
            print("✅ All dependencies are installed")
    
    def create_metadata(self):
        """Create metadata file for pandoc"""
        metadata = f"""---
title: "{BOOK_TITLE}"
author: "{AUTHOR}"
date: "{datetime.now().strftime('%B %Y')}"
version: "{self.version_string}"
lang: ru-RU
documentclass: book
geometry:
  - papersize=a4
  - margin=2.5cm
mainfont: "DejaVu Sans"
monofont: "DejaVu Sans Mono"
colorlinks: true
linkcolor: blue
urlcolor: blue
toccolor: black
toc-depth: 3
---
"""
        with open(METADATA_FILE, 'w') as f:
            f.write(metadata)
    
    def preprocess_markdown(self, content):
        """Preprocess markdown content for better PDF rendering"""
        # Fix image references
        content = re.sub(r'!\[([^\]]*)\]\(([^)]+)\)', r'![\\1](./\\2)', content)
        
        # Ensure mermaid blocks are properly formatted
        content = re.sub(r'```mermaid\s*\n', '```{.mermaid}\n', content)
        
        # Temporarily comment out mermaid blocks to avoid parsing errors
        # We'll render them as code blocks instead
        def comment_mermaid(match):
            diagram = match.group(1)
            return f'```\n[Mermaid diagram - not rendered in PDF]\n{diagram}\n```'
        
        content = re.sub(r'```{\.mermaid}\n(.*?)\n```', comment_mermaid, content, flags=re.DOTALL)
        
        return content
    
    def combine_chapters(self):
        """Combine all chapters into a single markdown file"""
        combined_path = os.path.join(OUTPUT_DIR, f"combined_{self.version_string}.md")
        
        with open(combined_path, 'w') as outfile:
            # Write metadata first
            with open(METADATA_FILE, 'r') as f:
                outfile.write(f.read())
            outfile.write("\n\\newpage\n\n")
            
            # Write each chapter
            for chapter in CHAPTERS:
                if os.path.exists(chapter):
                    print(f"  • Adding {chapter}")
                    with open(chapter, 'r') as f:
                        content = f.read()
                        content = self.preprocess_markdown(content)
                        outfile.write(content)
                        outfile.write("\n\\newpage\n\n")
                else:
                    print(f"  ⚠️  Warning: {chapter} not found")
        
        return combined_path
    
    def build_pdf(self, input_file):
        """Build PDF using pandoc"""
        output_file = os.path.join(OUTPUT_DIR, f"ABAP_Deep_Dive_{self.version_string}.pdf")
        
        cmd = [
            'pandoc',
            input_file,
            '--filter', 'mermaid-filter',
            '--pdf-engine=lualatex',
            '-V', 'geometry:margin=2.5cm',
            '-V', 'mainfont=DejaVu Sans',
            '-V', 'monofont=DejaVu Sans Mono',
            '--toc',
            '--toc-depth=3',
            '--highlight-style=tango',
            '-o', output_file
        ]
        
        print(f"\n📄 Building PDF...")
        try:
            result = subprocess.run(cmd, capture_output=True, text=True)
            if result.returncode == 0:
                print(f"✅ PDF created: {output_file}")
                return output_file
            else:
                print(f"❌ PDF build failed:")
                print(result.stderr)
                
                # Try without mermaid-filter as fallback
                print("\n🔄 Trying without mermaid-filter...")
                cmd.remove('--filter')
                cmd.remove('mermaid-filter')
                result = subprocess.run(cmd, capture_output=True, text=True)
                if result.returncode == 0:
                    print(f"✅ PDF created (without mermaid diagrams): {output_file}")
                    return output_file
                else:
                    print(f"❌ PDF build failed completely")
                    return None
        except Exception as e:
            print(f"❌ Error building PDF: {e}")
            return None
    
    def build_epub(self, input_file):
        """Build EPUB using pandoc"""
        output_file = os.path.join(OUTPUT_DIR, f"ABAP_Deep_Dive_{self.version_string}.epub")
        
        # Copy cover image if exists
        cover_option = []
        if os.path.exists('add-cover.png'):
            cover_path = os.path.join(OUTPUT_DIR, 'add-cover.png')
            shutil.copy('add-cover.png', cover_path)
            cover_option = ['--epub-cover-image=add-cover.png']
        
        cmd = [
            'pandoc',
            input_file,
            '--toc',
            '--toc-depth=3',
            '--epub-chapter-level=2',
            '--highlight-style=tango',
            '-o', output_file
        ] + cover_option
        
        print(f"\n📱 Building EPUB...")
        try:
            result = subprocess.run(cmd, capture_output=True, text=True)
            if result.returncode == 0:
                print(f"✅ EPUB created: {output_file}")
                return output_file
            else:
                print(f"❌ EPUB build failed:")
                print(result.stderr)
                return None
        except Exception as e:
            print(f"❌ Error building EPUB: {e}")
            return None
    
    def build(self, formats=['pdf', 'epub']):
        """Main build process"""
        print(f"\n🔨 ABAP Deep Dive Book Builder")
        print(f"📋 Current version: {self.version_string}")
        
        # Check dependencies
        self.check_dependencies()
        
        # Increment build number
        self.increment_build()
        print(f"📋 New version: {self.version_string}")
        
        # Create output directory
        os.makedirs(OUTPUT_DIR, exist_ok=True)
        
        # Create metadata
        self.create_metadata()
        
        # Combine chapters
        print(f"\n📚 Combining chapters...")
        combined_file = self.combine_chapters()
        
        # Build requested formats
        results = {}
        if 'pdf' in formats:
            results['pdf'] = self.build_pdf(combined_file)
        
        if 'epub' in formats:
            results['epub'] = self.build_epub(combined_file)
        
        # Clean up temporary files
        os.remove(METADATA_FILE)
        
        # Summary
        print(f"\n📊 Build Summary:")
        print(f"   Version: {self.version_string}")
        for fmt, path in results.items():
            if path:
                size = os.path.getsize(path) / (1024 * 1024)  # MB
                print(f"   {fmt.upper()}: {os.path.basename(path)} ({size:.1f} MB)")
        
        return results

def main():
    """Main entry point"""
    import argparse
    
    parser = argparse.ArgumentParser(description='Build ABAP Deep Dive book')
    parser.add_argument('--pdf', action='store_true', help='Build PDF version')
    parser.add_argument('--epub', action='store_true', help='Build EPUB version')
    parser.add_argument('--all', action='store_true', help='Build all formats')
    parser.add_argument('--increment-major', action='store_true', help='Increment major version')
    parser.add_argument('--increment-minor', action='store_true', help='Increment minor version')
    
    args = parser.parse_args()
    
    # Determine formats to build
    formats = []
    if args.all or (not args.pdf and not args.epub):
        formats = ['pdf', 'epub']
    else:
        if args.pdf:
            formats.append('pdf')
        if args.epub:
            formats.append('epub')
    
    # Build
    builder = BookBuilder()
    
    # Handle version increments
    if args.increment_major:
        builder.version_info['major'] += 1
        builder.version_info['minor'] = 0
        builder.save_version()
    elif args.increment_minor:
        builder.version_info['minor'] += 1
        builder.save_version()
    
    builder.build(formats)

if __name__ == '__main__':
    main()