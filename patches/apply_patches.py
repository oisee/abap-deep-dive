#!/usr/bin/env python3
"""
Скрипт для применения патчей к главам книги ABAP Deep Dive

Использование:
    python apply_patches.py [chapter_number] [--dry-run]
    
Примеры:
    python apply_patches.py          # Применить все патчи
    python apply_patches.py 1        # Применить патчи только для главы 1
    python apply_patches.py --dry-run # Показать что будет сделано без изменений
"""

import os
import sys
import re
import shutil
from datetime import datetime
from pathlib import Path

# Корневая директория проекта
ROOT_DIR = Path(__file__).parent.parent
PATCHES_DIR = ROOT_DIR / "patches"
BACKUP_DIR = ROOT_DIR / "backup"

# Маппинг номеров глав на имена файлов
CHAPTER_FILES = {
    1: "ADD - Глава 1  Анатомия SAP системы.md",
    2: "ADD - Глава 2 Ядро SAP - операционная система бизнес-приложений.md",
    3: "ADD - Глава 3 Work Process - микрокосм выполнения.md",
    4: "ADD - Глава 4 Иерархия памяти - балансировка между масштабируемостью и выживанием.md",
    5: "ADD - Глава 5 ABAP Virtual Machine - от исходника к исполнению.md",
    6: "ADD - Глава 6 Database Interface - мост между ABAP и СУБД.md",
    7: "ADD - Глава 7 От R2 до S4HANA - архитектурные революции.md",
    8: "ADD - Глава 8 SAP HANA - больше чем база данных.md",
    9: "ADD - Глава 9 SADL и Gateway - автоматизация REST API.md",
    10: "ADD - Глава 10 От BOPF к RAP - эволюция бизнес-объектов.md",
    11: "ADD - Глава 11 BTP и Steampunk - ABAP в облаке.md",
    12: "ADD - Глава 12 Инструменты анализа - заглядываем внутрь.md",
}


class PatchApplier:
    def __init__(self, dry_run=False):
        self.dry_run = dry_run
        self.applied_patches = []
        self.failed_patches = []
        
    def backup_file(self, file_path):
        """Создать резервную копию файла"""
        if self.dry_run:
            print(f"[DRY RUN] Создание backup для {file_path}")
            return
            
        timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        backup_path = BACKUP_DIR / f"{file_path.name}.{timestamp}.bak"
        backup_path.parent.mkdir(parents=True, exist_ok=True)
        
        shutil.copy2(file_path, backup_path)
        print(f"✓ Создан backup: {backup_path}")
        
    def parse_patch(self, patch_content):
        """Разобрать содержимое патч-файла"""
        patches = []
        current_patch = None
        
        lines = patch_content.split('\n')
        i = 0
        
        while i < len(lines):
            line = lines[i]
            
            # Начало нового исправления
            if line.startswith('### Исправление') or line.startswith('### Добавление'):
                if current_patch:
                    patches.append(current_patch)
                    
                current_patch = {
                    'title': line,
                    'file': None,
                    'line': None,
                    'find': None,
                    'replace': None,
                    'add_after': None,
                    'reason': None
                }
                
            # Парсинг деталей патча
            elif current_patch:
                if line.startswith('- Файл:'):
                    current_patch['file'] = line.split(':', 1)[1].strip()
                    
                elif line.startswith('- Строка:'):
                    line_info = line.split(':', 1)[1].strip()
                    # Извлечь номер строки из разных форматов
                    match = re.search(r'(\d+)', line_info)
                    if match:
                        current_patch['line'] = int(match.group(1))
                        
                elif line.startswith('- Найти:'):
                    # Собрать многострочный контент до "- Заменить:" или "- Добавить:"
                    i += 1
                    find_lines = []
                    while i < len(lines) and not lines[i].startswith('- Заменить:') and not lines[i].startswith('- Добавить:'):
                        find_lines.append(lines[i])
                        i += 1
                    current_patch['find'] = '\n'.join(find_lines).strip()
                    i -= 1  # Вернуться на строку с "- Заменить:" или "- Добавить:"
                    
                elif line.startswith('- Заменить:'):
                    # Собрать многострочный контент до "- Причина:"
                    i += 1
                    replace_lines = []
                    while i < len(lines) and not lines[i].startswith('- Причина:'):
                        replace_lines.append(lines[i])
                        i += 1
                    current_patch['replace'] = '\n'.join(replace_lines).strip()
                    i -= 1  # Вернуться на строку с "- Причина:"
                    
                elif line.startswith('- Добавить:'):
                    # Собрать многострочный контент до "- Причина:"
                    i += 1
                    add_lines = []
                    while i < len(lines) and not lines[i].startswith('- Причина:'):
                        add_lines.append(lines[i])
                        i += 1
                    current_patch['add_after'] = '\n'.join(add_lines).strip()
                    i -= 1  # Вернуться на строку с "- Причина:"
                    
                elif line.startswith('- Причина:'):
                    current_patch['reason'] = line.split(':', 1)[1].strip()
                    
            i += 1
            
        # Добавить последний патч
        if current_patch:
            patches.append(current_patch)
            
        return patches
        
    def apply_patch_to_file(self, patch, file_content):
        """Применить один патч к содержимому файла"""
        # Удалить обрамляющие ``` если есть
        if patch.get('find'):
            patch['find'] = patch['find'].strip('```').strip()
        if patch.get('replace'):
            patch['replace'] = patch['replace'].strip('```').strip()
        if patch.get('add_after'):
            patch['add_after'] = patch['add_after'].strip('```').strip()
            
        if patch.get('find') and patch.get('replace'):
            # Замена текста
            if patch['find'] in file_content:
                new_content = file_content.replace(patch['find'], patch['replace'], 1)
                return new_content, True
            else:
                print(f"  ⚠️  Не найден текст для замены: {patch['find'][:50]}...")
                return file_content, False
                
        elif patch.get('add_after') and patch.get('line'):
            # Добавление после строки
            lines = file_content.split('\n')
            if 0 <= patch['line'] <= len(lines):
                lines.insert(patch['line'], patch['add_after'])
                return '\n'.join(lines), True
            else:
                print(f"  ⚠️  Неверный номер строки: {patch['line']}")
                return file_content, False
                
        else:
            print(f"  ⚠️  Неполный патч: {patch['title']}")
            return file_content, False
            
    def apply_patches_to_chapter(self, chapter_num):
        """Применить все патчи к главе"""
        chapter_dir = PATCHES_DIR / f"chapter_{chapter_num}"
        if not chapter_dir.exists():
            print(f"Нет патчей для главы {chapter_num}")
            return
            
        chapter_file = ROOT_DIR / CHAPTER_FILES.get(chapter_num)
        if not chapter_file.exists():
            print(f"❌ Файл главы не найден: {chapter_file}")
            return
            
        print(f"\n📖 Обработка главы {chapter_num}: {CHAPTER_FILES[chapter_num]}")
        
        # Прочитать содержимое главы
        with open(chapter_file, 'r', encoding='utf-8') as f:
            content = f.read()
            
        original_content = content
        
        # Применить патчи в порядке приоритета
        patch_files = sorted(chapter_dir.glob("*.patch"))
        
        for patch_file in patch_files:
            print(f"\n📄 Применение патча: {patch_file.name}")
            
            with open(patch_file, 'r', encoding='utf-8') as f:
                patch_content = f.read()
                
            patches = self.parse_patch(patch_content)
            
            for patch in patches:
                print(f"  • {patch['title']}")
                if self.dry_run:
                    print(f"    [DRY RUN] Будет применено к {patch['file']}")
                else:
                    new_content, success = self.apply_patch_to_file(patch, content)
                    if success:
                        content = new_content
                        self.applied_patches.append(f"{chapter_num}: {patch['title']}")
                        print(f"    ✓ Применено")
                    else:
                        self.failed_patches.append(f"{chapter_num}: {patch['title']}")
                        print(f"    ❌ Ошибка применения")
        
        # Сохранить изменения
        if not self.dry_run and content != original_content:
            self.backup_file(chapter_file)
            with open(chapter_file, 'w', encoding='utf-8') as f:
                f.write(content)
            print(f"\n✓ Глава {chapter_num} обновлена")
        elif self.dry_run:
            print(f"\n[DRY RUN] Глава {chapter_num} будет обновлена")
        else:
            print(f"\nℹ️  Глава {chapter_num} не изменена")
            
    def run(self, chapter=None):
        """Запустить применение патчей"""
        print("🔧 Применение патчей к книге ABAP Deep Dive")
        print("=" * 50)
        
        if self.dry_run:
            print("⚠️  РЕЖИМ DRY RUN - изменения не будут сохранены")
            print("=" * 50)
            
        if chapter:
            # Применить к конкретной главе
            self.apply_patches_to_chapter(chapter)
        else:
            # Применить ко всем главам
            for chapter_num in sorted(CHAPTER_FILES.keys()):
                self.apply_patches_to_chapter(chapter_num)
                
        # Итоговый отчет
        print("\n" + "=" * 50)
        print("📊 ИТОГИ:")
        print(f"✓ Успешно применено: {len(self.applied_patches)} патчей")
        print(f"❌ Ошибки: {len(self.failed_patches)} патчей")
        
        if self.failed_patches:
            print("\nНеудачные патчи:")
            for failed in self.failed_patches:
                print(f"  • {failed}")


def main():
    """Главная функция"""
    dry_run = '--dry-run' in sys.argv
    chapter = None
    
    # Парсинг аргументов
    for arg in sys.argv[1:]:
        if arg.isdigit():
            chapter = int(arg)
            if chapter not in CHAPTER_FILES:
                print(f"❌ Неверный номер главы: {chapter}")
                print(f"Доступные главы: {', '.join(map(str, sorted(CHAPTER_FILES.keys())))}")
                sys.exit(1)
                
    # Запуск
    applier = PatchApplier(dry_run=dry_run)
    applier.run(chapter)


if __name__ == "__main__":
    main()