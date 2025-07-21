#!/usr/bin/env python3
"""
ABAP Deep Dive Technical Fact Checker
Verifies technical claims, code examples, and identifies missing content
"""

import os
import re
import json
from datetime import datetime
from pathlib import Path
import argparse

class ABAPFactChecker:
    def __init__(self, output_dir="factcheck"):
        self.output_dir = output_dir
        self.issues_dir = os.path.join(output_dir, "issues")
        self.todo_dir = os.path.join(output_dir, "todo")
        self.verified_dir = os.path.join(output_dir, "verified")
        
        # Create directories
        for dir_path in [self.issues_dir, self.todo_dir, self.verified_dir]:
            os.makedirs(dir_path, exist_ok=True)
            
        # Patterns to identify potential issues
        self.patterns = {
            'vague_statements': [
                r'примерно так',
                r'что-то вроде',
                r'как-то так',
                r'наверное',
                r'возможно',
                r'упрощенно',
                r'псевдокод',
                r'условно'
            ],
            'missing_examples': [
                r'[Пп]ример:?\s*$',
                r'[Нн]апример:?\s*$',
                r'[Дд]емонстрация:?\s*$'
            ],
            'outdated_references': [
                r'SAP\s+(?:R/3|46C|470|ECC\s*6\.0)',
                r'старые версии',
                r'раньше было'
            ],
            'technical_claims': [
                r'\d+\s*(?:МБ|MB|ГБ|GB|мс|ms|сек|sec)',
                r'производительность.*\d+',
                r'быстрее в\s*\d+',
                r'медленнее в\s*\d+'
            ],
            'code_blocks': [
                r'```(?:abap|sql|xml|json|yaml)',
                r'DATA:',
                r'SELECT\s+',
                r'METHOD\s+'
            ]
        }
        
        # Known facts database (could be loaded from external file)
        self.known_facts = {
            'processes': {
                'message_server': {
                    'unix': 'ms.sap<SID>',
                    'windows': 'msg_server.exe',
                    'port': '36<NN>'
                },
                'dispatcher': {
                    'unix': 'disp+work',
                    'windows': 'disp+work.exe'
                }
            },
            'memory_limits': {
                'em_initial_size_mb': {'min': 4096, 'default': 16384},
                'abap_heap_area_dia': {'max': 2000000000},
                'abap_heap_area_nondia': {'max': 0}
            },
            'architecture': {
                'sap_gui': 'thin client',
                'work_process_types': ['DIA', 'BTC', 'UPD', 'ENQ', 'SPO', 'V2', 'ICM']
            }
        }
        
    def extract_facts_from_chapter(self, content, chapter_name):
        """Extract technical claims and identify missing content"""
        facts = []
        todos = []
        fact_id = 1
        todo_id = 1
        
        lines = content.split('\n')
        
        for i, line in enumerate(lines):
            # Check for vague statements
            for pattern in self.patterns['vague_statements']:
                if re.search(pattern, line, re.IGNORECASE):
                    todos.append({
                        'id': f'TODO.{todo_id}',
                        'type': 'TODO_EXAMPLE',
                        'line': i + 1,
                        'text': line.strip(),
                        'reason': f'Vague statement: "{pattern}"',
                        'priority': 'MEDIUM'
                    })
                    todo_id += 1
            
            # Check for missing examples
            for pattern in self.patterns['missing_examples']:
                if re.search(pattern, line):
                    if i + 1 < len(lines) and not lines[i + 1].strip():
                        todos.append({
                            'id': f'TODO.{todo_id}',
                            'type': 'TODO_EXAMPLE',
                            'line': i + 1,
                            'text': line.strip(),
                            'reason': 'Example announced but missing',
                            'priority': 'HIGH'
                        })
                        todo_id += 1
            
            # Extract technical claims
            for pattern in self.patterns['technical_claims']:
                matches = re.finditer(pattern, line)
                for match in matches:
                    facts.append({
                        'id': f'F.{fact_id}',
                        'line': i + 1,
                        'claim': match.group(),
                        'context': line.strip(),
                        'needs_verification': True
                    })
                    fact_id += 1
            
            # Check for SAP GUI references
            if 'SAP GUI' in line or 'SAP-GUI' in line:
                if 'толстый клиент' in line:
                    facts.append({
                        'id': f'F.{fact_id}',
                        'line': i + 1,
                        'claim': 'SAP GUI - толстый клиент',
                        'context': line.strip(),
                        'error': 'SAP GUI is a thin client, not thick client',
                        'severity': 'HIGH'
                    })
                    fact_id += 1
        
        return facts, todos
    
    def verify_code_examples(self, content):
        """Verify ABAP code examples for syntax and best practices"""
        code_issues = []
        
        # Extract code blocks
        code_pattern = r'```(?P<lang>abap|sql)?\n(?P<code>.*?)\n```'
        matches = re.finditer(code_pattern, content, re.DOTALL)
        
        for match in matches:
            lang = match.group('lang') or 'unknown'
            code = match.group('code')
            
            if lang == 'abap':
                # Basic ABAP syntax checks
                issues = self.check_abap_code(code)
                if issues:
                    code_issues.extend(issues)
            elif lang == 'sql':
                # Basic SQL checks
                issues = self.check_sql_code(code)
                if issues:
                    code_issues.extend(issues)
        
        return code_issues
    
    def check_abap_code(self, code):
        """Basic ABAP code verification"""
        issues = []
        
        # Check for common ABAP issues
        if 'SELECT *' in code:
            issues.append({
                'type': 'PERFORMANCE',
                'message': 'SELECT * should be avoided, specify fields explicitly',
                'severity': 'MEDIUM'
            })
        
        if 'LOOP AT' in code and 'WHERE' not in code:
            issues.append({
                'type': 'PERFORMANCE',
                'message': 'LOOP AT without WHERE clause - consider filtering',
                'severity': 'LOW'
            })
        
        # Check for obsolete syntax
        obsolete_patterns = {
            'MOVE': 'Use = operator instead of MOVE',
            'COMPUTE': 'COMPUTE is obsolete, use direct assignment',
            'HEADER LINE': 'Tables with HEADER LINE are obsolete'
        }
        
        for pattern, message in obsolete_patterns.items():
            if pattern in code:
                issues.append({
                    'type': 'OBSOLETE',
                    'message': message,
                    'severity': 'MEDIUM'
                })
        
        return issues
    
    def check_sql_code(self, code):
        """Basic SQL code verification"""
        issues = []
        
        if 'SELECT SINGLE' in code and 'ORDER BY' in code:
            issues.append({
                'type': 'LOGIC',
                'message': 'SELECT SINGLE with ORDER BY is contradictory',
                'severity': 'HIGH'
            })
        
        return issues
    
    def generate_factcheck_report(self, chapter_name, facts, todos, code_issues):
        """Generate fact-check report for a chapter"""
        
        # Calculate statistics
        critical_issues = len([f for f in facts if f.get('severity') == 'HIGH'])
        medium_issues = len([f for f in facts if f.get('severity') == 'MEDIUM'])
        
        # Determine overall status
        if critical_issues > 0:
            status = "🔴 Требует доработки"
        elif medium_issues > 3:
            status = "🟡 Минорные правки"
        else:
            status = "🟢 Готово"
        
        report = f"""# Факт-чек: {chapter_name}
Дата проверки: {datetime.now().strftime('%Y-%m-%d %H:%M')}
Статус: {status}

## Сводка
- Проверено фактов: {len(facts)}
- Найдено проблем: {len([f for f in facts if 'error' in f])}
- Критических ошибок: {critical_issues}
- TODO items: {len(todos)}
- Проблем в коде: {len(code_issues)}

"""
        
        # Add critical issues
        critical_facts = [f for f in facts if f.get('severity') == 'HIGH']
        if critical_facts:
            report += "## Критические проблемы (HIGH priority)\n"
            for fact in critical_facts:
                report += f"""### ❌ [{fact['id']}] {fact.get('error', 'Требует проверки')}
**Строка**: {fact['line']}
**Написано**: "{fact['context']}"
**Проблема**: {fact.get('error', 'Требует верификации')}

"""
        
        # Add medium issues
        medium_facts = [f for f in facts if f.get('severity') == 'MEDIUM']
        if medium_facts:
            report += "## Средние проблемы (MEDIUM priority)\n"
            for fact in medium_facts:
                report += f"""### ⚠️ [{fact['id']}] {fact.get('claim', '')}
**Строка**: {fact['line']}
**Контекст**: "{fact['context']}"

"""
        
        # Add code issues
        if code_issues:
            report += "## Проблемы в примерах кода\n"
            for issue in code_issues:
                severity_icon = "❌" if issue['severity'] == 'HIGH' else "⚠️"
                report += f"""### {severity_icon} {issue['type']}
**Проблема**: {issue['message']}
**Важность**: {issue['severity']}

"""
        
        return report
    
    def generate_todo_report(self, chapter_name, todos):
        """Generate TODO report for missing content"""
        
        report = f"""# TODO лист: {chapter_name}
Сгенерирован: {datetime.now().strftime('%Y-%m-%d %H:%M')}

## Статистика
- Всего TODO: {len(todos)}
- Высокий приоритет: {len([t for t in todos if t['priority'] == 'HIGH'])}
- Средний приоритет: {len([t for t in todos if t['priority'] == 'MEDIUM'])}

"""
        
        # Group by priority
        for priority in ['HIGH', 'MEDIUM', 'LOW']:
            priority_todos = [t for t in todos if t['priority'] == priority]
            if priority_todos:
                priority_label = {
                    'HIGH': 'Высокий приоритет',
                    'MEDIUM': 'Средний приоритет',
                    'LOW': 'Низкий приоритет'
                }[priority]
                
                report += f"## {priority_label}\n"
                for todo in priority_todos:
                    report += f"""- [ ] [{todo['id']}] {todo['reason']}
  - **Строка {todo['line']}**: {todo['text'][:80]}...
  - **Тип**: {todo['type']}

"""
        
        return report
    
    def process_chapter(self, chapter_path):
        """Process a single chapter file"""
        chapter_name = Path(chapter_path).stem
        
        print(f"Проверяю: {chapter_name}")
        
        with open(chapter_path, 'r', encoding='utf-8') as f:
            content = f.read()
        
        # Extract facts and TODOs
        facts, todos = self.extract_facts_from_chapter(content, chapter_name)
        
        # Verify code examples
        code_issues = self.verify_code_examples(content)
        
        # Generate reports
        factcheck_report = self.generate_factcheck_report(chapter_name, facts, todos, code_issues)
        todo_report = self.generate_todo_report(chapter_name, todos)
        
        # Save reports
        safe_name = re.sub(r'[^\w\-_\. ]', '_', chapter_name)
        
        factcheck_path = os.path.join(self.issues_dir, f"FACTCHECK_{safe_name}.md")
        with open(factcheck_path, 'w', encoding='utf-8') as f:
            f.write(factcheck_report)
        
        todo_path = os.path.join(self.todo_dir, f"TODO_{safe_name}.md")
        with open(todo_path, 'w', encoding='utf-8') as f:
            f.write(todo_report)
        
        print(f"  ✅ Сохранено: {factcheck_path}")
        print(f"  ✅ Сохранено: {todo_path}")
        
        return {
            'chapter': chapter_name,
            'facts': len(facts),
            'issues': len([f for f in facts if 'error' in f]),
            'todos': len(todos),
            'code_issues': len(code_issues)
        }

def main():
    parser = argparse.ArgumentParser(description='ABAP Deep Dive Technical Fact Checker')
    parser.add_argument('chapters', nargs='*', help='Chapter files to check (if not specified, checks all)')
    parser.add_argument('--output-dir', default='factcheck', help='Output directory for reports')
    
    args = parser.parse_args()
    
    checker = ABAPFactChecker(args.output_dir)
    
    # Get chapter files
    if args.chapters:
        chapter_files = args.chapters
    else:
        # Check all ADD chapter files
        chapter_files = [f for f in os.listdir('.') if f.startswith('ADD') and f.endswith('.md')]
        chapter_files.sort()
    
    if not chapter_files:
        print("❌ Не найдены файлы глав для проверки")
        return
    
    print(f"🔍 ABAP Deep Dive Fact Checker")
    print(f"📁 Результаты будут сохранены в: {args.output_dir}/")
    print(f"📚 Найдено глав для проверки: {len(chapter_files)}\n")
    
    # Process all chapters
    results = []
    for chapter_file in chapter_files:
        if os.path.exists(chapter_file):
            result = checker.process_chapter(chapter_file)
            results.append(result)
        else:
            print(f"⚠️  Файл не найден: {chapter_file}")
    
    # Generate summary
    print("\n📊 Общая статистика:")
    total_facts = sum(r['facts'] for r in results)
    total_issues = sum(r['issues'] for r in results)
    total_todos = sum(r['todos'] for r in results)
    total_code_issues = sum(r['code_issues'] for r in results)
    
    print(f"  - Проверено фактов: {total_facts}")
    print(f"  - Найдено проблем: {total_issues}")
    print(f"  - TODO элементов: {total_todos}")
    print(f"  - Проблем в коде: {total_code_issues}")
    
    # Save summary
    summary_path = os.path.join(args.output_dir, "README.md")
    with open(summary_path, 'w', encoding='utf-8') as f:
        f.write(f"""# ABAP Deep Dive - Результаты факт-чекинга

Последняя проверка: {datetime.now().strftime('%Y-%m-%d %H:%M')}

## Общая статистика
- Проверено глав: {len(results)}
- Всего проверено фактов: {total_facts}
- Найдено проблем: {total_issues}
- TODO элементов: {total_todos}
- Проблем в примерах кода: {total_code_issues}

## Результаты по главам
| Глава | Факты | Проблемы | TODO | Код |
|-------|-------|----------|------|-----|
""")
        
        for result in results:
            f.write(f"| {result['chapter'][:50]}... | {result['facts']} | {result['issues']} | {result['todos']} | {result['code_issues']} |\n")
    
    print(f"\n✅ Проверка завершена! Результаты в {args.output_dir}/")

if __name__ == '__main__':
    main()