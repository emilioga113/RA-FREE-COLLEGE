# Datos requeridos y política de publicación

Los datos fuente e intermedios no se incluyen por tamaño, condiciones de uso y claridad del repositorio. Los scripts esperan una estructura equivalente a la siguiente:

```text
Construcción de panel data/
├── data/                                  # fuentes locales no versionadas
├── panel_cleaned_free_college.csv         # generado
├── ratios_fitch_panel.csv                 # generado
├── university_name_mapping_2.csv          # diccionario local
├── clasif_quintiles_ingresos_normalizado.csv
├── mapeo_indicadores.csv
└── tuition_instrument_2006.csv
```

## Variables de entorno principales

- `FREE_COLLEGE_DIR`: raíz del proyecto.
- `IPE_DATA_DIR`: carpeta con las fuentes del panel.
- `IPE_OUTPUT_DIR`: carpeta de salida del panel limpio.
- `FITCH_PANEL_DIR`: carpeta con el panel financiero.
- `DYNAMIC_DID_M_INPUT_FILE`: panel de ratios para el DiD.
- `IV_FC5_INPUT_FILE`, `IV_FC5_MAPPING_FILE`, `IV_FC5_COHORT_FILE` y `IV_FC5_TUITION_FILE`: entradas de la estrategia IV.

Las rutas por defecto apuntan a la estructura del repositorio y no contienen nombres de usuario ni ubicaciones personales.
