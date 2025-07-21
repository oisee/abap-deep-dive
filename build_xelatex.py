#!/usr/bin/env python3
"""
ABAP Deep Dive Book Builder with XeLaTeX
Handles Unicode/Cyrillic text properly
"""

import os
import sys
import json
import subprocess
import shutil
from datetime import datetime
from pathlib import Path

# Configuration
BOOK_TITLE = "ABAP Deep Dive: Архитектура, ядро и эволюция SAP"
AUTHOR = "ABAP Deep Dive Project"
VERSION_FILE = "version.json"
OUTPUT_DIR = "build"
METADATA_FILE = "metadata.yaml"

# Chapter order
CHAPTERS = [
    "ADD - ABAP Deep Dive.md",
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

def load_version():
    """Load version info"""
    if os.path.exists(VERSION_FILE):
        with open(VERSION_FILE, 'r') as f:
            return json.load(f)
    return {'major': 1, 'minor': 0, 'build_number': 0}

def save_version(version_info):
    """Save version info"""
    with open(VERSION_FILE, 'w') as f:
        json.dump(version_info, f, indent=2)

def get_version_string(version_info):
    """Generate version string"""
    date_str = datetime.now().strftime("%Y%m%d")
    return f"{date_str}-v{version_info['major']}.{version_info['minor']}-b{version_info['build_number']}"

def create_metadata(version_string):
    """Create metadata for XeLaTeX"""
    metadata = f"""---
title: "{BOOK_TITLE}"
author: "{AUTHOR}"
date: "{datetime.now().strftime('%B %Y')}"
version: "{version_string}"
lang: ru-RU
documentclass: book
geometry:
  - papersize=a4
  - margin=2.5cm
mainfont: "Arial"
monofont: "Courier New"
fontsize: 11pt
toc: true
toc-depth: 3
colorlinks: true
linkcolor: blue
urlcolor: blue
header-includes:
  - \\usepackage{{polyglossia}}
  - \\setmainlanguage{{russian}}
  - \\setotherlanguage{{english}}
  - \\usepackage{{fontspec}}
  - \\defaultfontfeatures{{Ligatures=TeX}}
---
"""
    with open(METADATA_FILE, 'w') as f:
        f.write(metadata)

def process_content(content):
    """Process markdown content"""
    # Replace Unicode in headers
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
    
    lines = content.split('\n')
    for i, line in enumerate(lines):
        if line.strip().startswith('#'):
            for emoji, replacement in unicode_replacements.items():
                line = line.replace(emoji, replacement)
            lines[i] = line
    
    return '\n'.join(lines)

def main():
    print("📚 ABAP Deep Dive Book Builder (XeLaTeX)")
    
    # Load and update version
    version_info = load_version()
    version_info['build_number'] += 1
    version_info['last_build'] = datetime.now().isoformat()
    save_version(version_info)
    
    version_string = get_version_string(version_info)
    print(f"📋 Version: {version_string}")
    
    # Create output directory
    os.makedirs(OUTPUT_DIR, exist_ok=True)
    
    # Create metadata
    create_metadata(version_string)
    
    # Combine chapters
    combined_path = os.path.join(OUTPUT_DIR, f"combined_{version_string}.md")
    print("\n📄 Combining chapters...")
    
    with open(combined_path, 'w') as outfile:
        # Write metadata
        with open(METADATA_FILE, 'r') as f:
            outfile.write(f.read())
        outfile.write("\n\\newpage\n\n")
        
        # Add cover if exists
        if os.path.exists('add-cover.png'):
            print("  • Adding cover image")
            outfile.write("\\begin{titlepage}\n")
            outfile.write("\\centering\n") 
            outfile.write("\\includegraphics[width=\\textwidth]{add-cover.png}\n")
            outfile.write("\\end{titlepage}\n\n")
        
        # Process each chapter
        for chapter in CHAPTERS:
            if os.path.exists(chapter):
                print(f"  • Processing {chapter}")
                with open(chapter, 'r') as f:
                    content = f.read()
                    content = process_content(content)
                    outfile.write(content)
                    outfile.write("\n\n\\newpage\n\n")
    
    # Build PDF with XeLaTeX
    output_pdf = os.path.join(OUTPUT_DIR, f"ABAP_Deep_Dive_{version_string}.pdf")
    print("\n📄 Building PDF with XeLaTeX...")
    
    cmd = [
        'pandoc',
        combined_path,
        '--pdf-engine=xelatex',
        '--resource-path=.:build:build/diagrams',
        '-o', output_pdf
    ]
    
    try:
        result = subprocess.run(cmd, capture_output=True, text=True, timeout=300)
        if result.returncode == 0:
            size_mb = os.path.getsize(output_pdf) / (1024 * 1024)
            print(f"✅ PDF created: {output_pdf} ({size_mb:.1f} MB)")
        else:
            print(f"❌ PDF build failed")
            print(f"Error: {result.stderr[:1000]}")
            
            # Try with basic settings
            print("\n🔄 Retrying with basic settings...")
            cmd = [
                'pandoc',
                combined_path,
                '--pdf-engine=xelatex',
                '-V', 'mainfont=Arial',
                '-V', 'geometry:margin=2.5cm',
                '--resource-path=.:build:build/diagrams',
                '-o', output_pdf
            ]
            
            result = subprocess.run(cmd, capture_output=True, text=True, timeout=300)
            if result.returncode == 0:
                size_mb = os.path.getsize(output_pdf) / (1024 * 1024)
                print(f"✅ PDF created: {output_pdf} ({size_mb:.1f} MB)")
            else:
                print(f"❌ Still failed: {result.stderr[:500]}")
                
    except subprocess.TimeoutExpired:
        print("❌ PDF build timed out")
    except Exception as e:
        print(f"❌ Error: {e}")
    finally:
        # Clean up
        if os.path.exists(METADATA_FILE):
            os.remove(METADATA_FILE)

if __name__ == '__main__':
    main()