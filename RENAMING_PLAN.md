# План переименования документов проекта ABAP Deep Dive

## Соглашение об именовании
- Формат: `NNN_<descriptive_name>.md`
- NNN - трёхзначный номер (001-999)
- Исключения: README.md, CLAUDE.md, DESIGN.md
- Главы книги (ADD - *) остаются без изменений

## План переименования (в хронологическом порядке)

### Основные документы проекта
| Старое имя | Новое имя | Дата создания |
|------------|-----------|---------------|
| LICENSE.md | 001_LICENSE.md | 2025-07-21 |
| CLA.md | 002_CLA.md | 2025-07-21 |
| CONTRIBUTOR_FAQ.md | 003_CONTRIBUTOR_FAQ.md | 2025-07-21 |
| CHANGELOG_2025-07-22.md | 004_CHANGELOG_2025_07_22.md | 2025-07-22 |
| FACTCHECK.md | 005_FACTCHECK.md | 2025-07-22 |
| articles/001_recenziya_abap_deep_dive_kriticheskiy_analiz.md | 006_recenziya_abap_deep_dive_kriticheskiy_analiz.md | 2025-07-22 |
| meetings/2025-07-25.md | 007_meeting_2025_07_25.md | 2025-07-27 |
| PROJECT_STATUS_2025_01.md | 008_PROJECT_STATUS_2025_08.md | 2025-08-01 |

### Технические документы (MCP, patches, etc.)
| Старое имя | Новое имя |
|------------|-----------|
| MCP_GROUNDING/MCP_Tools_Justification.md | 020_MCP_Tools_Justification.md |
| MCP_GROUNDING/SAP_MCP_Tools_Specification.md | 021_SAP_MCP_Tools_Specification.md |
| SAP_OBJECTS_VERIFICATION_LIST.md | 022_SAP_OBJECTS_VERIFICATION_LIST.md |
| TODO_AWAITING_MCP_TOOLS.md | 023_TODO_AWAITING_MCP_TOOLS.md |

### Отчеты о патчах
| Старое имя | Новое имя |
|------------|-----------|
| patches/reports/MANUAL_PATCH_APPLICATION_REPORT_2025-07-21.md | 030_MANUAL_PATCH_APPLICATION_REPORT_2025_07_21.md |
| patches/reports/PATCH_APPLICATION_REPORT_2025-07-21.md | 031_PATCH_APPLICATION_REPORT_2025_07_21.md |
| patches/PATCH_All_Chapters_Versioning.md | 032_PATCH_All_Chapters_Versioning.md |
| patches/PATCH_Ch9_Gateway_Critical_Fixes.md | 033_PATCH_Ch9_Gateway_Critical_Fixes.md |
| patches/PATCH_Ch12_Memory_Inspector_Fix.md | 034_PATCH_Ch12_Memory_Inspector_Fix.md |

### Factcheck документы (сохраняем структуру папок)
- factcheck/issues/* - остаются в своих папках
- factcheck/todo/* - остаются в своих папках
- factcheck/FACTCHECK_SUMMARY_Ch8-12.md → factcheck/040_FACTCHECK_SUMMARY_Ch8_12.md

### Build артефакты (не переименовываем)
- build/* - остаются как есть (генерируемые файлы)

## Файлы, требующие обновления ссылок

После переименования нужно обновить ссылки в следующих файлах:
1. README.md
2. CLAUDE.md (если есть ссылки)
3. Все главы книги (ADD - *.md)
4. Переименованные документы, которые ссылаются друг на друга

## Команды для выполнения

```bash
# Создание резервной копии
cp -r . ../abap-deep-dive-backup-$(date +%Y%m%d)

# Переименование основных документов
mv LICENSE.md 001_LICENSE.md
mv CLA.md 002_CLA.md
mv CONTRIBUTOR_FAQ.md 003_CONTRIBUTOR_FAQ.md
mv CHANGELOG_2025-07-22.md 004_CHANGELOG_2025_07_22.md
mv FACTCHECK.md 005_FACTCHECK.md
mv articles/001_recenziya_abap_deep_dive_kriticheskiy_analiz.md 006_recenziya_abap_deep_dive_kriticheskiy_analiz.md
mv meetings/2025-07-25.md 007_meeting_2025_07_25.md
mv PROJECT_STATUS_2025_01.md 008_PROJECT_STATUS_2025_08.md

# Переименование технических документов
mv MCP_GROUNDING/MCP_Tools_Justification.md 020_MCP_Tools_Justification.md
mv MCP_GROUNDING/SAP_MCP_Tools_Specification.md 021_SAP_MCP_Tools_Specification.md
mv SAP_OBJECTS_VERIFICATION_LIST.md 022_SAP_OBJECTS_VERIFICATION_LIST.md
mv TODO_AWAITING_MCP_TOOLS.md 023_TODO_AWAITING_MCP_TOOLS.md

# И так далее...
```

## Новая структура проекта

```
abap-deep-dive/
├── README.md (не изменяется)
├── CLAUDE.md (не изменяется)
├── DESIGN.md (не изменяется)
├── 001_LICENSE.md
├── 002_CLA.md
├── 003_CONTRIBUTOR_FAQ.md
├── 004_CHANGELOG_2025_07_22.md
├── 005_FACTCHECK.md
├── 006_recenziya_abap_deep_dive_kriticheskiy_analiz.md
├── 007_meeting_2025_07_25.md
├── 008_PROJECT_STATUS_2025_08.md
├── 020_MCP_Tools_Justification.md
├── 021_SAP_MCP_Tools_Specification.md
├── ...
├── ADD - *.md (главы книги - не изменяются)
├── factcheck/
│   ├── issues/ (не изменяются)
│   ├── todo/ (не изменяются)
│   └── 040_FACTCHECK_SUMMARY_Ch8_12.md
└── build/ (не изменяется)
```

## Преимущества новой структуры

1. **Хронологический порядок** - легко видеть эволюцию проекта
2. **Быстрая навигация** - номера помогают быстро найти нужный документ
3. **Стандартизация** - единый подход к именованию
4. **Совместимость** - главы книги остаются без изменений для стабильности ссылок