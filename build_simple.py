#!/usr/bin/env python3
"""
Simplified ABAP Deep Dive Book Builder
Builds PDF with pre-rendered diagrams
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
    print("📚 Simplified ABAP Deep Dive Book Builder")
    
    # Load and update version
    version_info = load_version()
    version_info['build_number'] += 1
    version_info['last_build'] = datetime.now().isoformat()
    save_version(version_info)
    
    version_string = get_version_string(version_info)
    print(f"📋 Version: {version_string}")
    
    # Create output directory
    os.makedirs(OUTPUT_DIR, exist_ok=True)
    
    # Combine chapters
    combined_path = os.path.join(OUTPUT_DIR, f"combined_{version_string}.md")
    print("\n📄 Combining chapters...")
    
    with open(combined_path, 'w') as outfile:
        # Write header
        outfile.write(f"---\n")
        outfile.write(f"title: \"{BOOK_TITLE}\"\n")
        outfile.write(f"author: \"{AUTHOR}\"\n")
        outfile.write(f"date: \"{datetime.now().strftime('%B %Y')}\"\n")
        outfile.write(f"version: \"{version_string}\"\n")
        outfile.write(f"lang: ru-RU\n")
        outfile.write(f"toc: true\n")
        outfile.write(f"toc-depth: 3\n")
        outfile.write(f"---\n\n")
        
        # Add cover if exists
        if os.path.exists('add-cover.png'):
            print("  • Adding cover image")
            outfile.write("![](add-cover.png)\n\n\\newpage\n\n")
        
        # Process each chapter
        for chapter in CHAPTERS:
            if os.path.exists(chapter):
                print(f"  • Processing {chapter}")
                with open(chapter, 'r') as f:
                    content = f.read()
                    content = process_content(content)
                    outfile.write(content)
                    outfile.write("\n\n\\newpage\n\n")
    
    # Build PDF with simplified settings
    output_pdf = os.path.join(OUTPUT_DIR, f"ABAP_Deep_Dive_{version_string}.pdf")
    print("\n📄 Building PDF...")
    
    cmd = [
        'pandoc',
        combined_path,
        '--pdf-engine=pdflatex',  # Use simpler engine
        '--toc',
        '--resource-path=.:build:build/diagrams',
        '-o', output_pdf
    ]
    
    try:
        result = subprocess.run(cmd, capture_output=True, text=True, timeout=120)
        if result.returncode == 0:
            size_mb = os.path.getsize(output_pdf) / (1024 * 1024)
            print(f"✅ PDF created: {output_pdf} ({size_mb:.1f} MB)")
        else:
            print(f"❌ PDF build failed: {result.stderr}")
            # Try without TOC
            print("🔄 Retrying without TOC...")
            cmd.remove('--toc')
            result = subprocess.run(cmd, capture_output=True, text=True, timeout=120)
            if result.returncode == 0:
                size_mb = os.path.getsize(output_pdf) / (1024 * 1024)
                print(f"✅ PDF created (no TOC): {output_pdf} ({size_mb:.1f} MB)")
            else:
                print(f"❌ PDF build still failed: {result.stderr[:500]}")
    except subprocess.TimeoutExpired:
        print("❌ PDF build timed out")
    except Exception as e:
        print(f"❌ Error: {e}")

if __name__ == '__main__':
    main()