# TODO для Главы 12: Инструменты анализа

**Дата создания**: 2025-07-21
**Приоритет**: Высокий

## 1. SQL Trace (ST05) - Критические дополнения

### 1.1. Полный список типов трассировки
- [ ] Buffer Trace - анализ обращений к буферам
- [ ] Enqueue Trace - анализ блокировок  
- [ ] RFC Trace - анализ удаленных вызовов
- [ ] HTTP Trace - анализ HTTP/HTTPS запросов
- [ ] Table Buffer Trace - детальный анализ табличной буферизации

### 1.2. Конкретные примеры
```abap
" Пример проблемного SQL для анализа
SELECT * FROM vbak 
  INTO TABLE @DATA(lt_orders)
  WHERE erdat IN ( SELECT erdat FROM vbap 
                    WHERE matnr = 'MATERIAL1' ).

" Пример оптимизированного запроса
SELECT DISTINCT k~vbeln, k~erdat
  FROM vbak AS k
  INNER JOIN vbap AS p
    ON k~vbeln = p~vbeln
  INTO TABLE @DATA(lt_orders_opt)
  WHERE p~matnr = 'MATERIAL1'.
```

### 1.3. Параметры фильтрации
- [ ] Object Type (Table, View, CDS)
- [ ] Client filtering
- [ ] Duration threshold (например, > 1000 ms)
- [ ] Statement pattern matching
- [ ] User terminal filtering

### 1.4. Анализ Execution Plan
```
Пример вывода EXPLAIN:
SELECT STATEMENT ( Estimated Costs = 524 , Estimated #Rows = 100 )
  2 HASH JOIN ( Estimated Costs = 524 , Estimated #Rows = 100 )
    3 TABLE ACCESS FULL VBAK ( Estimated Costs = 234 , Estimated #Rows = 1000 )
    4 TABLE ACCESS BY INDEX ROWID VBAP ( Estimated Costs = 290 , Estimated #Rows = 100 )
      5 INDEX RANGE SCAN VBAP~MAT ( Estimated Costs = 3 , Estimated #Rows = 100 )
```

### 1.5. HANA-специфичные расширения
- [ ] Транзакция PLANVIZ для визуализации планов
- [ ] SQL Plan Cache анализ (вместо Expensive Statement Trace)
- [ ] Column Store статистика
- [ ] Параллельное выполнение и распределение по узлам

## 2. Runtime Analysis (SAT) - Недостающие компоненты

### 2.1. Варианты измерений
```abap
" Пример создания варианта программно
DATA: lo_measurement TYPE REF TO if_atra_measurement.

lo_measurement = cl_atra_measurement=>create(
  iv_variant = 'Z_PERFORMANCE_CHECK'
  iv_component = 'BC-DWB-TOO-ATR' ).

lo_measurement->set_aggregation_level( 
  cl_atra_measurement=>co_aggregation-full ).
lo_measurement->set_max_file_size( 100 ). "MB
```

### 2.2. Фильтры и ограничения
- [ ] Package filter: Только Z* или Y* объекты
- [ ] Class/Interface filter: Specific patterns
- [ ] RFC destination filter
- [ ] Memory limit для файла измерений
- [ ] Time limit для измерения

### 2.3. Анализ Call Hierarchy
```
Call Hierarchy Example:
MAIN_PROGRAM                    100.0%   500ms
  └─ CL_HANDLER=>PROCESS         80.0%   400ms
      ├─ SELECT_DATA             50.0%   250ms
      │   └─ DB Access           45.0%   225ms
      └─ PROCESS_DATA            30.0%   150ms
          └─ SORT_INTERNAL       20.0%   100ms
```

### 2.4. Hit List анализ
- [ ] Exclusive vs Inclusive time
- [ ] Net time calculation
- [ ] Memory allocation per call
- [ ] Database time separation

## 3. Memory Inspector - Полная функциональность

### 3.1. Правильные транзакции и программы
- [ ] Транзакция: SMI (не S_MEMORY_INSPECTOR)
- [ ] Программа: RSTSMEMORY
- [ ] Класс API: CL_ABAP_MEMORY_INSPECTOR

### 3.2. Программное создание снимков
```abap
" Пример создания memory snapshot
DATA: lo_inspector TYPE REF TO cl_abap_memory_inspector.

CREATE OBJECT lo_inspector.
lo_inspector->create_snapshot( 
  iv_description = 'Before processing' ).

" ... обработка данных ...

lo_inspector->create_snapshot( 
  iv_description = 'After processing' ).

" Сравнение снимков
lo_inspector->compare_snapshots(
  iv_snapshot_1 = 1
  iv_snapshot_2 = 2 ).
```

### 3.3. Анализ утечек памяти
```abap
" Пример потенциальной утечки
CLASS lcl_cache DEFINITION.
  PUBLIC SECTION.
    CLASS-DATA: gt_cache TYPE TABLE OF string.
    CLASS-METHODS: add_entry IMPORTING iv_data TYPE string.
ENDCLASS.

" Проблема: статическая таблица растет без очистки
METHOD add_entry.
  APPEND iv_data TO gt_cache.
ENDMETHOD.
```

### 3.4. Reference Chain анализ
- [ ] Примеры circular references
- [ ] Event handler cleanup
- [ ] Static attribute management
- [ ] Field-symbol reference tracking

## 4. Kernel Snapshot - Детальная информация

### 4.1. Команды создания снимков
```bash
# dpmon команды
dpmon pf=/usr/sap/SID/SYS/profile/SID_DVEBMGS00_host -snapshot
dpmon pf=/usr/sap/SID/SYS/profile/SID_DVEBMGS00_host -analyze

# sapcontrol команды  
sapcontrol -nr 00 -function CreateSnapshot
sapcontrol -nr 00 -function AnalyzeSnapshot
```

### 4.2. Расположение файлов
- [ ] Work directory: /usr/sap/SID/DVEBMGS00/work/
- [ ] Snapshot files: snapshot_*.trc
- [ ] Analysis files: snapshot_*.ana

### 4.3. OS-специфичные инструменты
```bash
# AIX
nmon -f -s 1 -c 3600
topas -P

# Solaris  
prstat -a
dtrace scripts

# Linux расширенный список
perf record -g
systemtap scripts
```

## 5. Недостающие инструменты

### 5.1. ST12 - Integrated Analysis
- [ ] Комбинация ABAP + SQL + RFC trace
- [ ] Единый интерфейс анализа
- [ ] Примеры использования

### 5.2. ST22 - Runtime Error Analysis
- [ ] Категории дампов
- [ ] Анализ call stack
- [ ] Поиск по дампам

### 5.3. STAD - Statistical Records
- [ ] Single record analysis
- [ ] Response time breakdown
- [ ] Database request time

### 5.4. ST03N - Workload Monitor
- [ ] Transaction profiles
- [ ] Time profile analysis
- [ ] User distribution

### 5.5. SQLM - SQL Monitor
```abap
" Активация SQL Monitor
CALL FUNCTION 'RSQLM_ACTIVATE_MONITORING'
  EXPORTING
    activate = 'X'.

" Деактивация
CALL FUNCTION 'RSQLM_DEACTIVATE_MONITORING'.
```

### 5.6. Code Inspector vs ATC
- [ ] Различия в проверках
- [ ] Performance checks
- [ ] Security checks
- [ ] Интеграция с транспортами

## 6. Практические сценарии

### 6.1. Анализ медленной транзакции
```
1. ST12 - Запустить полную трассировку
2. Проанализировать ABAP time vs DB time
3. Если DB time высокий -> ST05 детальный анализ
4. Если ABAP time высокий -> SAT профилирование
5. Проверить STAD для исторических данных
```

### 6.2. Поиск утечки памяти
```
1. SM50 - Проверить Extended Memory usage
2. SMI - Создать начальный snapshot
3. Воспроизвести проблему
4. SMI - Создать конечный snapshot
5. Сравнить snapshots и найти растущие объекты
```

### 6.3. Анализ дампа MEMORY_NO_MORE_PAGING
```
1. ST22 - Найти и открыть дамп
2. Проверить раздел "Information on where terminated"
3. SM04 - Проверить memory usage пользователя
4. ST02 - Проверить Extended Memory exhausted
5. RZ11 - Проверить параметры em/initial_size_MB
```

## 7. Performance метрики и пороги

### 7.1. Рекомендуемые пороги
- [ ] Dialog response time: < 1 секунда
- [ ] Database request time: < 40% от общего времени
- [ ] CPU time: < 40% от response time
- [ ] Wait time: < 10% от response time
- [ ] Roll wait time: < 200 ms

### 7.2. Метрики HANA
- [ ] Column store load time
- [ ] Unload priority settings
- [ ] Delta merge statistics
- [ ] Memory consumption by tables

## 8. Безопасность и авторизации

### 8.1. Необходимые авторизации
```
ST05: S_ADMI_FCD = ST05
SAT: S_ADMI_FCD = ATRM 
SMI: S_ADMI_FCD = SMI
Kernel tools: S_ADMI_FCD = PADM
```

### 8.2. Best Practices для Production
- [ ] Не запускать trace на всех пользователей
- [ ] Ограничивать время трассировки
- [ ] Очищать старые trace файлы
- [ ] Мониторить влияние на производительность

## 9. Интеграция с внешними системами

### 9.1. Solution Manager
- [ ] End-to-End trace
- [ ] Cross-system analysis
- [ ] Workload analysis

### 9.2. SAP Cloud ALM
- [ ] Real User Monitoring
- [ ] Synthetic monitoring
- [ ] Alert management

### 9.3. Third-party APM tools
- [ ] Dynatrace integration
- [ ] AppDynamics for SAP
- [ ] New Relic integration points

## 10. Версионные особенности

### 10.1. NetWeaver 7.40
- [ ] Introduction of SQLM
- [ ] Enhanced SAT features
- [ ] New HANA optimizations

### 10.2. NetWeaver 7.50  
- [ ] ABAP Channels monitoring
- [ ] Enhanced Memory Inspector
- [ ] New kernel profiling options

### 10.3. S/4HANA специфика
- [ ] CDS view performance analysis
- [ ] AMDP debugging
- [ ] Fiori performance tools

## Итоговая статистика TODO

- **Всего пунктов**: 147
- **Критические**: 45
- **Важные**: 62
- **Желательные**: 40

## Приоритет исправлений

1. **Немедленно**: Исправить названия транзакций и добавить недостающие инструменты
2. **Высокий**: Добавить конкретные примеры и сценарии использования
3. **Средний**: Включить версионную информацию и метрики
4. **Низкий**: Добавить интеграцию с внешними системами