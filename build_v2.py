#!/usr/bin/env python3
"""
ABAP Deep Dive Book Builder v2
Builds PDF and EPUB versions with auto-incrementing version numbers
Now with Mermaid to SVG conversion support
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
import hashlib

# Configuration
BOOK_TITLE = "ABAP Deep Dive: Архитектура, ядро и эволюция SAP"
AUTHOR = "ABAP Deep Dive Project"
VERSION_FILE = "version.json"
OUTPUT_DIR = "build"
TEMP_DIR = "build/temp"
DIAGRAMS_DIR = "build/diagrams"
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

class MermaidProcessor:
    """Handles extraction and rendering of Mermaid diagrams"""
    
    def __init__(self, diagrams_dir, output_format='png'):
        self.diagrams_dir = diagrams_dir
        self.output_format = output_format
        os.makedirs(diagrams_dir, exist_ok=True)
        self.diagram_counter = 0
        
    def extract_mermaid_blocks(self, content):
        """Extract all mermaid blocks from markdown content"""
        mermaid_pattern = r'```(?:mermaid|{\.mermaid})\n(.*?)\n```'
        matches = list(re.finditer(mermaid_pattern, content, re.DOTALL))
        return matches
    
    def generate_diagram_id(self, mermaid_content):
        """Generate unique ID for diagram based on content"""
        # Use hash of content for consistent IDs
        content_hash = hashlib.md5(mermaid_content.encode()).hexdigest()[:8]
        return f"diagram_{content_hash}"
    
    def render_mermaid_to_svg(self, mermaid_content, output_path, output_format='png'):
        """Render mermaid diagram to specified format using mermaid CLI (mmdc)"""
        try:
            # Import re at the top of the method
            import re
            
            # Preprocess mermaid content to handle special characters
            lines = mermaid_content.split('\n')
            processed_lines = []
            for line in lines:
                # Handle pipes in node labels
                if '[' in line and ']' in line:
                    # Find content within brackets
                    def escape_special_chars_in_brackets(match):
                        content = match.group(1)
                        # Escape special characters
                        content = content.replace('|', '&#124;')
                        content = content.replace('(', '&#40;')
                        content = content.replace(')', '&#41;')
                        content = content.replace('"', '&quot;')
                        # Handle <br/> tags - ensure they are lowercase
                        content = content.replace('<BR/>', '<br/>')
                        content = content.replace('<BR>', '<br/>')
                        content = content.replace('<Br/>', '<br/>')
                        return '[' + content + ']'
                    
                    line = re.sub(r'\[([^\]]+)\]', escape_special_chars_in_brackets, line)
                
                # Also handle quotes in node IDs (like STR1[String "ABC"])
                if re.match(r'^\s*\w+\[.*".*"\]', line):
                    line = re.sub(r'"([^"]+)"', r'&quot;\1&quot;', line)
                    
                processed_lines.append(line)
            
            processed_content = '\n'.join(processed_lines)
            
            # Create temporary mermaid file
            with tempfile.NamedTemporaryFile(mode='w', suffix='.mmd', delete=False, encoding='utf-8') as f:
                f.write(processed_content)
                temp_mmd = f.name
            
            # Check if mmdc (mermaid CLI) is available
            try:
                subprocess.run(['mmdc', '--version'], capture_output=True, check=True)
            except (subprocess.CalledProcessError, FileNotFoundError):
                print("⚠️  mermaid CLI (mmdc) not found. Installing...")
                subprocess.run(['npm', 'install', '-g', '@mermaid-js/mermaid-cli'], check=True)
            
            # Determine output path based on format
            if output_format == 'pdf':
                final_path = output_path.replace('.svg', '.pdf')
                cmd = [
                    'mmdc',
                    '-i', temp_mmd,
                    '-o', final_path,
                    '-t', 'default',
                    '-b', 'white',
                    '--pdfFit'  # Fit to PDF page
                ]
            else:  # PNG by default
                final_path = output_path.replace('.svg', '.png')
                cmd = [
                    'mmdc',
                    '-i', temp_mmd,
                    '-o', final_path,
                    '-t', 'default',
                    '-b', 'white',
                    '--width', '1600',
                    '--height', '1200',
                    '--scale', '2'
                ]
            
            result = subprocess.run(cmd, capture_output=True, text=True)
            if result.returncode != 0:
                print(f"⚠️  Failed to render diagram: {result.stderr}")
                return False
            
            # Clean up
            os.unlink(temp_mmd)
            return True
            
        except Exception as e:
            print(f"❌ Error rendering mermaid diagram: {e}")
            return False
    
    def process_content(self, content, chapter_name=""):
        """Process markdown content and replace mermaid blocks with SVG images"""
        matches = self.extract_mermaid_blocks(content)
        
        if not matches:
            return content
        
        print(f"  • Found {len(matches)} mermaid diagrams in {chapter_name}")
        
        # Process from end to beginning to maintain positions
        for match in reversed(matches):
            self.diagram_counter += 1
            diagram_content = match.group(1)
            diagram_id = self.generate_diagram_id(diagram_content)
            svg_filename = f"{diagram_id}.svg"
            svg_path = os.path.join(self.diagrams_dir, svg_filename)
            
            # Determine output filename based on format
            if self.output_format == 'pdf':
                output_filename = f"{diagram_id}.pdf"
                output_path = os.path.join(self.diagrams_dir, output_filename)
            else:
                output_filename = f"{diagram_id}.png"
                output_path = os.path.join(self.diagrams_dir, output_filename)
            
            # Render diagram if not already exists
            if not os.path.exists(output_path):
                print(f"    - Rendering diagram {self.diagram_counter}: {diagram_id}")
                if self.render_mermaid_to_svg(diagram_content, svg_path, self.output_format):
                    print(f"      ✓ Saved as {output_filename}")
                else:
                    print(f"      ✗ Failed to render, keeping as code block")
                    continue
            else:
                print(f"    - Using cached diagram: {output_filename}")
            
            # Replace mermaid block with image reference
            relative_path = f"diagrams/{output_filename}"
            img_tag = f'![Diagram {self.diagram_counter}]({relative_path})'
            content = content[:match.start()] + img_tag + content[match.end():]
        
        return content

class BookBuilder:
    def __init__(self, diagram_format='png'):
        self.version_info = self.load_version()
        self.build_number = self.version_info['build_number']
        self.version_string = self.get_version_string()
        self.diagram_format = diagram_format
        self.mermaid_processor = MermaidProcessor(DIAGRAMS_DIR, diagram_format)
        
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
            'lualatex': 'lualatex --version'
        }
        
        missing = []
        for tool, cmd in tools.items():
            try:
                subprocess.run(cmd.split(), capture_output=True, check=True)
            except (subprocess.CalledProcessError, FileNotFoundError):
                missing.append(tool)
        
        if missing:
            print(f"❌ Missing required tools: {', '.join(missing)}")
            print("\nInstall them with:")
            if 'pandoc' in missing:
                print("  • brew install pandoc (macOS) or apt-get install pandoc (Linux)")
            if 'lualatex' in missing:
                print("  • brew install --cask mactex (macOS) or apt-get install texlive-full (Linux)")
            sys.exit(1)
        
        # Check for mermaid CLI (optional)
        try:
            subprocess.run(['mmdc', '--version'], capture_output=True, check=True)
            print("✅ All dependencies are installed (including mermaid CLI)")
        except (subprocess.CalledProcessError, FileNotFoundError):
            print("⚠️  Optional: mermaid CLI not found")
            print("  Install with: npm install -g @mermaid-js/mermaid-cli")
            print("  Without it, diagrams will be rendered on first run")
    
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
        # Fix image references to use relative paths
        content = re.sub(r'!\[([^\]]*)\]\(([^)]+)\)', r'![\1](\2)', content)
        
        # Replace problematic Unicode characters in headers with text equivalents
        # This helps with PDF rendering issues
        unicode_replacements = {
            '✅': '[OK]',
            '❌': '[X]',
            '⚠️': '[!]',
            '📚': '[BOOK]',
            '🔍': '[SEARCH]',
            '💡': '[IDEA]',
            '🎯': '[TARGET]',
            '🚀': '[ROCKET]',
            '⚡': '[LIGHTNING]',
            '🔧': '[WRENCH]',
            '📊': '[CHART]',
            '🏗️': '[BUILDING]',
            '🔄': '[CYCLE]',
            '📝': '[MEMO]',
            '🎨': '[ART]',
            '🌟': '[STAR]',
            '💾': '[DISK]',
            '🔗': '[LINK]',
            '🔨': '[HAMMER]',
            '📄': '[PAGE]',
            '📱': '[PHONE]',
            '🧹': '[BROOM]',
            '📦': '[PACKAGE]',
            '⬆️': '[UP]',
            '📋': '[CLIPBOARD]'
        }
        
        # Apply replacements only in headers (lines starting with #)
        lines = content.split('\n')
        for i, line in enumerate(lines):
            if line.strip().startswith('#'):
                for emoji, replacement in unicode_replacements.items():
                    line = line.replace(emoji, replacement)
                lines[i] = line
        
        return '\n'.join(lines)
    
    def process_chapter(self, chapter_path):
        """Process a single chapter: extract and render mermaid diagrams"""
        with open(chapter_path, 'r') as f:
            content = f.read()
        
        # Process mermaid diagrams
        content = self.mermaid_processor.process_content(content, os.path.basename(chapter_path))
        
        # Additional preprocessing
        content = self.preprocess_markdown(content)
        
        return content
    
    def combine_chapters(self):
        """Combine all chapters into a single markdown file with processed diagrams"""
        os.makedirs(OUTPUT_DIR, exist_ok=True)
        combined_path = os.path.join(OUTPUT_DIR, f"combined_{self.version_string}.md")
        
        with open(combined_path, 'w') as outfile:
            # Write metadata first
            with open(METADATA_FILE, 'r') as f:
                outfile.write(f.read())
            outfile.write("\n\\newpage\n\n")
            
            # Add title page with cover image if exists
            if os.path.exists('add-cover.png'):
                outfile.write("\\begin{titlepage}\n")
                outfile.write("\\centering\n")
                outfile.write("\\includegraphics[width=\\textwidth]{add-cover.png}\n")
                outfile.write("\\end{titlepage}\n\n")
            
            # Process and write each chapter
            for chapter in CHAPTERS:
                if os.path.exists(chapter):
                    print(f"  • Processing {chapter}")
                    processed_content = self.process_chapter(chapter)
                    outfile.write(processed_content)
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
            '--pdf-engine=lualatex',
            '-V', 'geometry:margin=2.5cm',
            '-V', 'mainfont=DejaVu Sans',
            '-V', 'monofont=DejaVu Sans Mono',
            '-V', 'documentclass=book',
            '-V', 'fontenc=T1',
            '-V', 'inputenc=utf8',
            '--toc',
            '--toc-depth=3',
            '--highlight-style=tango',
            '--resource-path=.:build:build/diagrams',  # Add diagrams directory to resource path
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
            '--resource-path=.:build:build/diagrams',  # Add diagrams directory to resource path
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
    
    def clean_temp_files(self):
        """Clean up temporary files"""
        if os.path.exists(METADATA_FILE):
            os.remove(METADATA_FILE)
        # Keep temp directory for debugging, but can be removed if needed
        # if os.path.exists(TEMP_DIR):
        #     shutil.rmtree(TEMP_DIR)
    
    def build(self, formats=['pdf', 'epub']):
        """Main build process"""
        print(f"\n🔨 ABAP Deep Dive Book Builder v2")
        print(f"📋 Current version: {self.version_string}")
        
        # Check dependencies
        self.check_dependencies()
        
        # Increment build number
        self.increment_build()
        print(f"📋 New version: {self.version_string}")
        
        # Create output directories
        os.makedirs(OUTPUT_DIR, exist_ok=True)
        os.makedirs(TEMP_DIR, exist_ok=True)
        os.makedirs(DIAGRAMS_DIR, exist_ok=True)
        
        # Create metadata
        self.create_metadata()
        
        # Process chapters and combine
        print(f"\n📚 Processing chapters and extracting diagrams...")
        combined_file = self.combine_chapters()
        print(f"\n🎨 Processed {self.mermaid_processor.diagram_counter} diagrams total")
        print(f"📄 Combined markdown with SVG references: {combined_file}")
        
        # Build requested formats
        results = {}
        if 'pdf' in formats:
            results['pdf'] = self.build_pdf(combined_file)
        
        if 'epub' in formats:
            results['epub'] = self.build_epub(combined_file)
        
        # Clean up temporary files
        self.clean_temp_files()
        
        # Summary
        print(f"\n📊 Build Summary:")
        print(f"   Version: {self.version_string}")
        print(f"   Diagrams rendered: {self.mermaid_processor.diagram_counter}")
        for fmt, path in results.items():
            if path:
                size = os.path.getsize(path) / (1024 * 1024)  # MB
                print(f"   {fmt.upper()}: {os.path.basename(path)} ({size:.1f} MB)")
        
        return results

def main():
    """Main entry point"""
    import argparse
    
    parser = argparse.ArgumentParser(description='Build ABAP Deep Dive book v2')
    parser.add_argument('--pdf', action='store_true', help='Build PDF version')
    parser.add_argument('--epub', action='store_true', help='Build EPUB version')
    parser.add_argument('--all', action='store_true', help='Build all formats')
    parser.add_argument('--increment-major', action='store_true', help='Increment major version')
    parser.add_argument('--increment-minor', action='store_true', help='Increment minor version')
    parser.add_argument('--clean', action='store_true', help='Clean build artifacts')
    parser.add_argument('--diagram-format', choices=['png', 'pdf'], default='png', 
                        help='Format for diagrams (png or pdf)')
    
    args = parser.parse_args()
    
    # Handle clean
    if args.clean:
        print("🧹 Cleaning build artifacts...")
        if os.path.exists(OUTPUT_DIR):
            shutil.rmtree(OUTPUT_DIR)
        if os.path.exists(METADATA_FILE):
            os.remove(METADATA_FILE)
        print("✅ Clean complete")
        return
    
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
    builder = BookBuilder(diagram_format=args.diagram_format)
    
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