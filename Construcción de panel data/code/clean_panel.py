# -*- coding: utf-8 -*-
# Panel loader + authoritative university mapping v2 + covariate merges
import os
import re
import glob
import unicodedata
import numbers
from pathlib import Path
import numpy as np
import pandas as pd

# --- CONFIG -------------------------------------------------------------------
REPOSITORY_ROOT = Path(__file__).resolve().parents[2]
PANEL_DIR = REPOSITORY_ROOT / "Construcción de panel data"
DATA_DIR = os.environ.get(
    "IPE_DATA_DIR",
    str(PANEL_DIR / "data")
)

OUTPUT_DIR = os.environ.get(
    "IPE_OUTPUT_DIR",
    str(PANEL_DIR)
)

PATTERN = "IPE_DATOS_PANEL_*.xlsx"

# Mapping v2
MAPPING_CSV = os.path.join(
    DATA_DIR,
    "university_name_mapping_2.csv"
)

CLEAN_CSV = os.environ.get(
    "IPE_CLEAN_CSV",
    os.path.join(OUTPUT_DIR, "panel_cleaned_free_college.csv")
)

# Keep this filename if your corrected accreditation workbook is saved under this name.
ACCREDITATION_XLSX = os.path.join(
    DATA_DIR,
    "Acreditacion_estatus_2011_22.xlsx"
)

UNDERGRAD_ENROLLMENT_XLSX = os.path.join(
    DATA_DIR,
    "panel_matricula_pregrado_universidades_chile_2011_2022.xlsx"
)

TOTAL_ENROLLMENT_XLSX = os.path.join(
    DATA_DIR,
    "03_BaseINDICES_Indicadores (1).xlsx"
)


# Oferta académica, matrícula por programa y aranceles.
# Base CNED/ÍNDICES a nivel institución-año-programa.
PROGRAM_OFFER_XLSX = os.environ.get(
    "IPE_PROGRAM_OFFER_XLSX",
    os.path.join(
        DATA_DIR,
        "BaseDefinitivaINDICES-2005-2024.xlsx"
    )
)


CPI_XLS = os.environ.get(
    "IPE_CPI_XLS",
    os.path.join(
        OUTPUT_DIR,
        "API_FP.CPI.TOTL.ZG_DS2_es_excel_v2_326414.xls"
    )
)

CPI_CACHE_CSV = os.environ.get(
    "IPE_CPI_CACHE_CSV",
    os.path.join(
        DATA_DIR,
        "CPI_Chile_FP.CPI.TOTL.ZG_2011_2022.csv"
    )
)

ELIGIBILITY_XLSX = os.path.join(
    DATA_DIR,
    "elegibilidad_gratuidad_universidad.xlsx"
)

TOTAL_STUDENTS_COL = "N total estudiantes pregrado"
TOTAL_ENROLLMENT_COL = "Matrícula Total"

# Variables de oferta académica y aranceles agregadas a universidad-año.
N_PROGRAMAS_PREGRADO_COL = "n_programas_pregrado"
N_PROGRAMAS_POSTGRADO_COL = "n_programas_postgrado"
NEW_PROGRAMS_PREGRADO_COL = "new_programs_pregrado"
NEW_PROGRAMS_POSTGRADO_COL = "new_programas_postgrado"
ARANCEL_MEDIO_COL = "arancel_medio"
ARANCEL_MEDIANO_COL = "arancel_mediano"
ARANCEL_MATRICULA_COL = "arancel_matricula"
ARANCEL_DIF_COL = "arancel_dif"
ESTUDIANTES_PROM_PROGRAMA_COL = "estudiantes_promedio_programa"
PUNTAJE_ML_PROMEDIO_COL = "puntaje_ml_promedio"
PUNTAJE_CORTE_PROMEDIO_COL = "puntaje_corte_promedio"
INFLATION_COL = "Inflation_WB"

ELIGIBILITY_PROXY_COL = "% elegibles gratuidad (proxy escolar)"
ELIGIBILITY_FUAS_COL = "% elegibles gratuidad (FUAS)"
ELIGIBILITY_PROXY_FUAS_COL = (
    "% elegibles gratuidad (proxy escolar, muestra FUAS)"
)
ELIGIBILITY_PROXY_COVERAGE_COL = "Cobertura elegibilidad proxy (%)"
ELIGIBILITY_FUAS_COVERAGE_COL = "Cobertura FUAS (%)"

# Equivalencias explícitas para nombres de la fuente de elegibilidad que no
# coinciden literalmente con las variantes del mapping v2.
ELIGIBILITY_NAME_ALIASES = {
    "UNIVERSIDAD CENTRAL DE CHILE": "Universidad Central",
    "UNIVERSIDAD DEL ALBA": "Universidad Pedro de Valdivia",
    "UNIVERSIDAD SEK": "Universidad Internacional SEK",
    "UNIVERSIDAD DE VIÑA DEL MAR": "Universidad Vina del Mar",
    "UNIVERSIDAD DE ARTES, CIENCIAS Y COMUNICACION - UNIACC": "Uniacc",
    "UNIVERSIDAD UCINF": "Universidad de Las Ciencias de la Informática (UCINF)",
    "UNIVERSIDAD ADVENTISTA DE CHILE": "Universidad Adventista",
    "UNIVERSIDAD DE ARTE Y CIENCIAS SOCIALES ARCIS": "Universidad ARCIS",
    "UNIVERSIDAD CATOLICA CARDENAL RAUL SILVA HENRIQUEZ": (
        "Universidad Católica Silva Henríquez"
    ),
    "UNIVERSIDAD TECNOLOGICA DE CHILE INACAP": (
        "Universidad Tecnológica INACAP"
    ),
    "UNIVERSIDAD DE PLAYA ANCHA DE CIENCIAS DE LA EDUCACION": (
        "Universidad de Playa Ancha"
    ),
    "PONTIFICIA UNIVERSIDAD CATOLICA DE CHILE": (
        "Pontificia Universidad Católica"
    ),
    "PONTIFICIA UNIVERSIDAD CATOLICA DE VALPARAISO": (
        "Pontificia Universidad Católica de Valparaíso"
    ),
}

PUCV_NAME = "Pontificia Universidad Católica de Valparaíso"
# Acreditaciones institucionales PUCV: seis años en los períodos 2009-2015 y
# 2015-2021; siete años desde enero de 2022. La base institucional también
# informa 6 para 2020-2021 y 7 para 2022.
PUCV_ACCREDITATION_BY_YEAR = {
    **{year: 6 for year in range(2011, 2022)},
    2022: 7,
}


# --- HELPERS ------------------------------------------------------------------
def strip_accents(s: str) -> str:
    if s is None:
        return ""
    s = str(s)
    s = unicodedata.normalize("NFD", s)
    s = "".join(ch for ch in s if unicodedata.category(ch) != "Mn")
    return unicodedata.normalize("NFC", s)


def normalize_university_for_key(name: str) -> str:
    """
    Stable key used by the original panel:
    lowercase, no accents, only [a-z0-9 ] and single spaces.
    """
    if name is None:
        return ""
    name = str(name).strip().lstrip("+")
    name = strip_accents(name).lower()
    name = re.sub(r"[^a-z0-9\s]", " ", name)
    name = re.sub(r"\s+", " ", name).strip()
    return name


def clean_value(v):
    """
    Convert Valor to numeric when possible.
    """
    if pd.isna(v):
        return pd.NA

    # pd.read_excel ya entrega los importes numéricos como números. Convertirlos
    # primero a texto y eliminar el punto los multiplicaba por diez cuando el
    # valor se representaba como, por ejemplo, ``64484668.0``.
    if isinstance(v, numbers.Number) and not isinstance(v, bool):
        number = float(v)
        return int(number) if number.is_integer() else number

    s = str(v)
    if "|" in s:
        # Una celda compactada se separa con su año contiguo antes de convertir.
        return s.strip()
    s = s.replace("\u00A0", " ")
    s = s.replace(".", "")
    s = s.replace(",", "")
    s = s.replace("$", "").strip()
    s = re.sub(r"[^0-9\-]", "", s)

    if s in ("", "-"):
        return pd.NA

    try:
        return int(s)
    except Exception:
        try:
            return float(s)
        except Exception:
            return pd.NA


COL_CANON = {
    "universidad": "Universidad",
    "institucion": "Universidad",
    "indicador": "Indicador",
    "variable": "Indicador",
    "año": "Año",
    "ano": "Año",
    "anio": "Año",
    "valor": "Valor",
    "monto": "Valor",
}


def normalize_column_name(c: str) -> str:
    c = strip_accents(str(c)).lower().strip()
    c = re.sub(r"\s+", " ", c)
    return COL_CANON.get(c, c)


def build_expected_sheet_names(standard_names):
    """
    Reproduce exactly the Excel sheet-name rule used in the accreditation file.

    Important:
    - Excel allows max 31 characters.
    - trailing spaces are removed by Excel, so rstrip() is required.
    - suffixes are added only if truncation creates duplicates.
    """
    sheet_names = []

    for name in standard_names:
        title = str(name)[:31].rstrip()
        base = title
        i = 1

        while title in sheet_names:
            suffix = f"_{i}"
            title = base[:31 - len(suffix)].rstrip() + suffix
            i += 1

        sheet_names.append(title)

    return sheet_names


def load_mapping_v2(path):
    """
    Load the curated university_name_mapping_2.csv.

    This file is authoritative and is NOT regenerated or overwritten by this script.
    """
    if not os.path.exists(path):
        raise FileNotFoundError(
            f"Mapping v2 not found: {path}\n"
            "Place university_name_mapping_2.csv in DATA_DIR."
        )

    mapping = pd.read_csv(path, dtype=str, encoding="utf-8-sig")
    mapping.columns = [
        str(column).lstrip("\ufeff").strip()
        for column in mapping.columns
    ]

    required = {
        "Universidad_key",
        "Universidad_raw",
        "CNED_Universidad",
        "CNED_key",
        "Suggested_Standard",
        "Final_Standard",
    }

    missing_cols = sorted(required - set(mapping.columns))
    if missing_cols:
        raise ValueError(
            f"Mapping file has an incompatible schema: {os.path.abspath(path)}\n"
            "Missing required columns: " + ", ".join(missing_cols) + "\n"
            "Detected columns: " + ", ".join(map(str, mapping.columns)) + "\n"
            "Make sure MAPPING_CSV points to university_name_mapping_2.csv "
            "and rerun the complete file (not only a selection)."
        )

    mapping = mapping.copy()

    # Normalize the two keys with the same rule used by the panel.
    mapping["Universidad_key"] = (
        mapping["Universidad_key"]
        .fillna("")
        .apply(normalize_university_for_key)
    )

    mapping["CNED_key"] = (
        mapping["CNED_key"]
        .fillna("")
        .apply(normalize_university_for_key)
    )

    mapping["Final_Standard"] = mapping["Final_Standard"].astype(str).str.strip()
    mapping["CNED_Universidad"] = mapping["CNED_Universidad"].astype(str).str.strip()

    # Key uniqueness is essential for many-to-one matching.
    dup_panel_keys = mapping.loc[
        mapping["Universidad_key"].duplicated(keep=False),
        ["Universidad_key", "Final_Standard"]
    ]
    if not dup_panel_keys.empty:
        raise ValueError(
            "Duplicate Universidad_key values in mapping v2:\n"
            + dup_panel_keys.to_string(index=False)
        )

    dup_final = mapping.loc[
        mapping["Final_Standard"].duplicated(keep=False),
        ["Universidad_key", "Final_Standard"]
    ]
    if not dup_final.empty:
        raise ValueError(
            "Duplicate Final_Standard values in mapping v2:\n"
            + dup_final.to_string(index=False)
        )

    dup_cned = mapping.loc[
        mapping["CNED_key"].duplicated(keep=False),
        ["CNED_key", "CNED_Universidad", "Final_Standard"]
    ]
    if not dup_cned.empty:
        raise ValueError(
            "Duplicate CNED_key values in mapping v2:\n"
            + dup_cned.to_string(index=False)
        )

    return mapping


def load_accreditation_panel(path, final_map):
    """
    Read the reviewed accreditation workbook.

    Matching is:
      Final_Standard -> Excel sheet
      Año            -> Año
      C              -> Acreditación (años)

    CNED_Universidad/CNED_key are deliberately NOT used here because the reviewed
    accreditation workbook has sheets named according to Final_Standard.
    """
    if not os.path.exists(path):
        raise FileNotFoundError(
            f"Accreditation workbook not found: {path}"
        )

    standard_names = (
        final_map["Final_Standard"]
        .dropna()
        .astype(str)
        .str.strip()
        .drop_duplicates()
        .tolist()
    )

    expected_sheets = build_expected_sheet_names(standard_names)
    sheet_to_university = dict(zip(expected_sheets, standard_names))

    xls = pd.ExcelFile(path)
    actual_sheets = [str(s).strip() for s in xls.sheet_names]

    missing_sheets = sorted(set(expected_sheets) - set(actual_sheets))

    # The reviewed workbook may contain audit/traceability sheets in addition
    # to the 60 university sheets.  Only missing required university sheets
    # are an error; unrelated sheets are ignored below.
    if missing_sheets:
        print(
            "[Aviso] Sin hoja de acreditación para estas universidades; "
            "se conservará Acreditación (años) como faltante:\n"
            + ", ".join(missing_sheets)
        )

    frames = []

    # Use the original sheet names from pandas for reading, but strip only for lookup.
    for actual_sheet_name in xls.sheet_names:
        clean_sheet_name = str(actual_sheet_name).strip()
        if clean_sheet_name not in sheet_to_university:
            continue
        university_std = sheet_to_university[clean_sheet_name]

        df = pd.read_excel(
            path,
            sheet_name=actual_sheet_name,
            usecols=["Año", "Acreditación (años)"]
        )

        df["Año"] = pd.to_numeric(
            df["Año"], errors="coerce"
        ).astype("Int64")

        df["Acreditación (años)"] = pd.to_numeric(
            df["Acreditación (años)"], errors="coerce"
        ).astype("Int64")

        df["Universidad_std"] = university_std

        frames.append(
            df[["Universidad_std", "Año", "Acreditación (años)"]]
        )

    accreditation = pd.concat(frames, ignore_index=True)
    accreditation = accreditation.dropna(subset=["Año"])

    duplicate_mask = accreditation.duplicated(
        subset=["Universidad_std", "Año"],
        keep=False
    )

    if duplicate_mask.any():
        duplicated = accreditation.loc[
            duplicate_mask,
            ["Universidad_std", "Año", "Acreditación (años)"]
        ].sort_values(["Universidad_std", "Año"])

        raise ValueError(
            "Duplicate Universidad_std x Año rows found in accreditation workbook:\n"
            + duplicated.to_string(index=False)
        )

    return accreditation


def load_undergrad_enrollment_panel(path, final_map):
    """Read one Matrícula pregrado observation per university-year."""
    if not os.path.exists(path):
        raise FileNotFoundError(
            f"Undergraduate enrollment workbook not found: {path}"
        )

    standard_names = (
        final_map["Final_Standard"]
        .dropna().astype(str).str.strip().drop_duplicates().tolist()
    )
    expected_sheets = build_expected_sheet_names(standard_names)
    sheet_to_university = dict(zip(expected_sheets, standard_names))

    xls = pd.ExcelFile(path)
    actual_sheets = [str(sheet).strip() for sheet in xls.sheet_names]
    missing_sheets = sorted(set(expected_sheets) - set(actual_sheets))
    if missing_sheets:
        print(
            "[Aviso] Sin hoja de matrícula de pregrado para estas universidades; "
            "se aplicará el respaldo de elegibilidad cuando esté disponible:\n"
            + ", ".join(missing_sheets)
        )

    frames = []
    for actual_sheet_name in xls.sheet_names:
        clean_sheet_name = str(actual_sheet_name).strip()
        if clean_sheet_name not in sheet_to_university:
            continue

        df = pd.read_excel(path, sheet_name=actual_sheet_name)
        df.columns = [str(column).strip() for column in df.columns]
        required = {"Año", "Matrícula pregrado"}
        missing_columns = sorted(required - set(df.columns))
        if missing_columns:
            raise ValueError(
                f"Sheet '{actual_sheet_name}' is missing columns: "
                + ", ".join(missing_columns)
            )

        df = df[["Año", "Matrícula pregrado"]].copy()
        df["Año"] = pd.to_numeric(df["Año"], errors="coerce").astype("Int64")
        df["Matrícula pregrado"] = pd.to_numeric(
            df["Matrícula pregrado"], errors="coerce"
        ).astype("Int64")
        df["Universidad_std"] = sheet_to_university[clean_sheet_name]
        frames.append(df[["Universidad_std", "Año", "Matrícula pregrado"]])

    enrollment = pd.concat(frames, ignore_index=True).dropna(subset=["Año"])
    duplicate_mask = enrollment.duplicated(
        subset=["Universidad_std", "Año"], keep=False
    )
    if duplicate_mask.any():
        duplicated = enrollment.loc[
            duplicate_mask,
            ["Universidad_std", "Año", "Matrícula pregrado"]
        ].sort_values(["Universidad_std", "Año"])
        raise ValueError(
            "Duplicate Universidad_std x Año rows found in undergraduate "
            "enrollment workbook:\n" + duplicated.to_string(index=False)
        )

    return enrollment


def load_total_enrollment_panel(path, final_map):
    """Lee Matrícula Total desde la base institucional por universidad-año."""
    if not os.path.exists(path):
        raise FileNotFoundError(
            f"Institutional enrollment workbook not found: {path}"
        )

    context = pd.read_excel(path, sheet_name="1_ContextoInstitucional")
    context.columns = [str(column).strip() for column in context.columns]
    column_lookup = {
        normalize_university_for_key(column): column
        for column in context.columns
    }
    try:
        year_col = column_lookup["ano"]
        institution_col = column_lookup["institucion"]
        enrollment_col = column_lookup["matricula total"]
    except KeyError as exc:
        raise ValueError(
            "No se encontraron Año, Institución y Matrícula Total en "
            "1_ContextoInstitucional."
        ) from exc

    name_lookup = build_mapping_name_lookup(final_map)
    context = context[[year_col, institution_col, enrollment_col]].copy()
    context.columns = ["Año", "Institución_fuente", TOTAL_ENROLLMENT_COL]
    context["Año"] = pd.to_numeric(
        context["Año"], errors="coerce"
    ).astype("Int64")
    context[TOTAL_ENROLLMENT_COL] = pd.to_numeric(
        context[TOTAL_ENROLLMENT_COL], errors="coerce"
    ).astype("Int64")
    context["Universidad_std"] = context["Institución_fuente"].map(
        normalize_university_for_key
    ).map(name_lookup)
    context = context.dropna(
        subset=["Universidad_std", "Año"]
    )[["Universidad_std", "Año", TOTAL_ENROLLMENT_COL]]

    duplicate_mask = context.duplicated(
        subset=["Universidad_std", "Año"], keep=False
    )
    if duplicate_mask.any():
        duplicated = context.loc[duplicate_mask].sort_values(
            ["Universidad_std", "Año"]
        )
        raise ValueError(
            "Duplicate Universidad_std x Año rows found in total enrollment "
            "data:\n" + duplicated.to_string(index=False)
        )
    return context


def build_mapping_name_lookup(final_map):
    """Mapea variantes institucionales normalizadas a Final_Standard."""
    lookup = {}
    name_columns = [
        "Universidad_raw",
        "CNED_Universidad",
        "Suggested_Standard",
        "Final_Standard",
    ]

    for _, row in final_map.iterrows():
        final_name = str(row["Final_Standard"]).strip()
        for column in name_columns:
            key = normalize_university_for_key(row[column])
            if not key:
                continue
            previous = lookup.get(key)
            if previous is not None and previous != final_name:
                raise ValueError(
                    f"Ambiguous normalized university name '{key}': "
                    f"'{previous}' versus '{final_name}'."
                )
            lookup[key] = final_name

    return lookup


def load_eligibility_panel(path, final_map):
    """Lee matrícula de pregrado y porcentajes de elegibilidad por universidad-año."""
    if not os.path.exists(path):
        raise FileNotFoundError(
            f"Eligibility workbook not found: {path}"
        )

    eligibility = pd.read_excel(path, sheet_name="Por_universidad")
    eligibility.columns = [str(column).strip() for column in eligibility.columns]
    required = {
        "agno", "cod_inst", "nomb_inst", "alumnos", "con_proxy",
        "con_fuas", "pct_proxy", "pct_fuas", "pct_proxy_fuas",
    }
    missing_columns = sorted(required - set(eligibility.columns))
    if missing_columns:
        raise ValueError(
            "The eligibility workbook has an incompatible schema. "
            "Missing columns: " + ", ".join(missing_columns)
        )

    eligibility = eligibility.copy()
    eligibility["Año"] = pd.to_numeric(
        eligibility["agno"], errors="coerce"
    ).astype("Int64")
    eligibility = eligibility.dropna(subset=["Año", "nomb_inst"])

    name_lookup = build_mapping_name_lookup(final_map)
    alias_lookup = {
        normalize_university_for_key(source): target
        for source, target in ELIGIBILITY_NAME_ALIASES.items()
    }
    valid_final_names = set(
        final_map["Final_Standard"].dropna().astype(str).str.strip()
    )
    unknown_alias_targets = sorted(set(alias_lookup.values()) - valid_final_names)
    if unknown_alias_targets:
        raise ValueError(
            "Eligibility aliases reference unknown Final_Standard values: "
            + ", ".join(unknown_alias_targets)
        )

    eligibility["eligibility_name_key"] = eligibility["nomb_inst"].map(
        normalize_university_for_key
    )
    eligibility["Universidad_std"] = eligibility["eligibility_name_key"].map(
        name_lookup
    )
    alias_mask = eligibility["Universidad_std"].isna()
    eligibility.loc[alias_mask, "Universidad_std"] = eligibility.loc[
        alias_mask, "eligibility_name_key"
    ].map(alias_lookup)

    unmatched = (
        eligibility.loc[eligibility["Universidad_std"].isna(), [
            "cod_inst", "nomb_inst"
        ]]
        .drop_duplicates()
        .sort_values(["cod_inst", "nomb_inst"])
    )
    if not unmatched.empty:
        print(
            "[Aviso] Instituciones de elegibilidad fuera del mapping v2 "
            "(se omitirán):\n" + unmatched.to_string(index=False)
        )

    eligibility = eligibility.dropna(subset=["Universidad_std"]).copy()
    matched_final_names = set(eligibility["Universidad_std"].unique())
    missing_final_names = sorted(valid_final_names - matched_final_names)
    if missing_final_names:
        raise ValueError(
            "No eligibility source match was found for these panel universities: "
            + ", ".join(missing_final_names)
        )

    numeric_source_cols = [
        "alumnos", "con_proxy", "con_fuas",
        "pct_proxy", "pct_fuas", "pct_proxy_fuas",
    ]
    for column in numeric_source_cols:
        eligibility[column] = pd.to_numeric(
            eligibility[column], errors="coerce"
        )

    eligibility[TOTAL_STUDENTS_COL] = eligibility["alumnos"].astype("Int64")
    eligibility[ELIGIBILITY_PROXY_COL] = eligibility["pct_proxy"]
    eligibility[ELIGIBILITY_FUAS_COL] = eligibility["pct_fuas"]
    eligibility[ELIGIBILITY_PROXY_FUAS_COL] = eligibility["pct_proxy_fuas"]
    eligibility[ELIGIBILITY_PROXY_COVERAGE_COL] = (
        eligibility["con_proxy"]
        .div(eligibility["alumnos"].replace(0, pd.NA))
        .mul(100)
    )
    eligibility[ELIGIBILITY_FUAS_COVERAGE_COL] = (
        eligibility["con_fuas"]
        .div(eligibility["alumnos"].replace(0, pd.NA))
        .mul(100)
    )

    output_columns = [
        "Universidad_std", "Año", TOTAL_STUDENTS_COL,
        ELIGIBILITY_PROXY_COL, ELIGIBILITY_FUAS_COL,
        ELIGIBILITY_PROXY_FUAS_COL, ELIGIBILITY_PROXY_COVERAGE_COL,
        ELIGIBILITY_FUAS_COVERAGE_COL,
    ]
    eligibility = eligibility[output_columns]

    duplicate_mask = eligibility.duplicated(
        subset=["Universidad_std", "Año"], keep=False
    )
    if duplicate_mask.any():
        duplicated = eligibility.loc[duplicate_mask].sort_values(
            ["Universidad_std", "Año"]
        )
        raise ValueError(
            "Duplicate Universidad_std x Año rows found in eligibility data:\n"
            + duplicated.to_string(index=False)
        )

    return eligibility


def split_compound_values(panel):
    """Normaliza celdas 'a|b' como dos años consecutivos sin concatenarlos."""
    pattern = re.compile(
        r"^\s*(-?[0-9.,]+)\s*\|\s*(-?[0-9.,]+)\s*$"
    )
    extra_rows = []
    for index, row in panel.iterrows():
        raw = row.get("Valor")
        if not isinstance(raw, str):
            continue
        match = pattern.match(raw)
        if not match:
            raise ValueError(
                f"Valor compuesto inválido en fila {index}: {raw!r}"
            )
        first = clean_value(match.group(1))
        second = clean_value(match.group(2))
        year = row.get("Año")
        if pd.isna(year):
            raise ValueError(
                f"Valor compuesto sin año en fila {index}: {raw!r}"
            )
        panel.at[index, "Valor"] = first
        next_year = int(year) + 1
        same_series = panel.loc[
            (panel["Universidad"] == row["Universidad"])
            & (panel["Indicador"] == row["Indicador"])
            & (panel["Año"] == next_year)
        ]
        if same_series.empty:
            appended = row.copy()
            appended["Año"] = next_year
            appended["Valor"] = second
            extra_rows.append(appended)
        else:
            destination = same_series.index[0]
            previous = panel.at[destination, "Valor"]
            if pd.isna(previous):
                panel.at[destination, "Valor"] = second
            elif previous != second:
                raise ValueError(
                    "Conflicto al expandir valor compuesto para "
                    f"{row['Universidad']} / {row['Indicador']} / {next_year}: "
                    f"{previous!r} versus {second!r}."
                )
    if extra_rows:
        panel = pd.concat(
            [panel, pd.DataFrame(extra_rows)], ignore_index=True
        )
    return panel


def apply_pucv_accreditation(panel):
    """Completa y valida la acreditación institucional anual de la PUCV."""
    expected = panel["Año"].map(PUCV_ACCREDITATION_BY_YEAR)
    mask = panel["Universidad_std"].eq(PUCV_NAME) & expected.notna()
    existing = pd.to_numeric(
        panel.loc[mask, "Acreditación (años)"], errors="coerce"
    )
    conflicts = existing.notna() & existing.ne(expected.loc[mask])
    if conflicts.any():
        bad = panel.loc[mask].loc[
            conflicts.to_numpy(), ["Universidad_std", "Año", "Acreditación (años)"]
        ]
        raise ValueError(
            "La acreditación PUCV existente contradice la cronología validada:\n"
            + bad.to_string(index=False)
        )
    panel.loc[mask, "Acreditación (años)"] = expected.loc[mask].astype("Int64")

    pucv = panel.loc[mask, ["Año", "Acreditación (años)"]].drop_duplicates()
    missing_years = sorted(set(PUCV_ACCREDITATION_BY_YEAR) - set(pucv["Año"]))
    if missing_years or pucv["Acreditación (años)"].isna().any():
        raise ValueError(
            "No se pudo completar la acreditación PUCV 2011-2022. "
            f"Años ausentes: {missing_years}"
        )
    return panel




def load_chile_wb_inflation(xls_path, cache_path):
    """Lee FP.CPI.TOTL.ZG para Chile y devuelve la tasa anual en decimal."""
    source = xls_path
    try:
        wb = pd.read_excel(xls_path, sheet_name="Data", skiprows=3)
        required = {"Country Code", "Indicator Code"}
        if not required.issubset(wb.columns):
            raise ValueError(
                "La hoja Data del World Bank no contiene Country Code e Indicator Code."
            )

        row = wb.loc[
            wb["Country Code"].astype(str).eq("CHL")
            & wb["Indicator Code"].astype(str).eq("FP.CPI.TOTL.ZG")
        ]
        if len(row) != 1:
            raise ValueError(
                "No se encontró una fila única CHL / FP.CPI.TOTL.ZG en el archivo WB."
            )

        year_columns = [c for c in wb.columns if str(c).isdigit()]
        inflation = row[year_columns].T.reset_index()
        inflation.columns = ["Año", "Inflation_percent"]
    except Exception as exc:
        if not os.path.exists(cache_path):
            raise RuntimeError(
                "No se pudo leer el .xls del World Bank y no existe el CSV de "
                f"respaldo {cache_path}. Error original: {exc}"
            ) from exc

        print(
            "[Aviso] El motor de Python no pudo leer el .xls del World Bank; "
            "se usa el extracto CSV trazable generado desde ese archivo. "
            f"Detalle: {exc}"
        )
        inflation = pd.read_csv(cache_path, encoding="utf-8-sig")
        source = cache_path
        if "Year" in inflation.columns:
            inflation = inflation.rename(columns={"Year": "Año"})

    required = {"Año", "Inflation_percent"}
    if not required.issubset(inflation.columns):
        raise ValueError(
            f"La fuente CPI no contiene {sorted(required)}: {list(inflation.columns)}"
        )

    inflation = inflation[["Año", "Inflation_percent"]].copy()
    inflation["Año"] = pd.to_numeric(
        inflation["Año"], errors="coerce"
    ).astype("Int64")
    inflation["Inflation_percent"] = pd.to_numeric(
        inflation["Inflation_percent"], errors="coerce"
    )
    inflation = inflation.dropna(subset=["Año"]).drop_duplicates("Año", keep="last")
    inflation[INFLATION_COL] = inflation["Inflation_percent"] / 100.0
    inflation = inflation.loc[inflation["Año"].between(2011, 2022)]

    expected_years = set(range(2011, 2023))
    observed_years = set(
        inflation.loc[inflation[INFLATION_COL].notna(), "Año"].astype(int)
    )
    missing_years = sorted(expected_years - observed_years)
    if missing_years:
        raise ValueError(
            "La inflación WB de Chile no cubre todos los años 2011-2022: "
            + ", ".join(map(str, missing_years))
        )

    print("- Fuente de inflación WB:", source)
    return inflation[["Año", INFLATION_COL]].sort_values("Año")


def _normalize_level_label(value):
    """Normaliza Pregrado/Posgrado a etiquetas simples."""
    if pd.isna(value):
        return pd.NA
    key = normalize_university_for_key(value)
    if "pregrado" in key:
        return "pregrado"
    if "posgrado" in key or "postgrado" in key:
        return "postgrado"
    return pd.NA


def _first_existing_column(columns, candidates):
    """Devuelve el primer nombre de columna disponible entre varias alternativas."""
    normalized = {
        normalize_university_for_key(column): column
        for column in columns
    }
    for candidate in candidates:
        key = normalize_university_for_key(candidate)
        if key in normalized:
            return normalized[key]
    return None


def load_program_offer_panel(path, final_map):
    """
    Construye indicadores universidad-año a partir de BaseDefinitivaINDICES-2005-2024.

    Definiciones:
      - n_programas_pregrado / n_programas_postgrado:
        número de ofertas/programas distintos observados en la base ese año.
      - new_programs_*:
        programas cuyo Año Inicio Actividades coincide con el año observado.
      - arancel_*:
        calculados SOLO para pregrado y SOLO cuando Tipo Moneda = pesos.
        * arancel_medio: promedio simple.
        * arancel_mediano: mediana.
        * arancel_matricula: promedio ponderado por Matrícula Total del programa.
        * arancel_dif: máximo - mínimo.
      - estudiantes_promedio_programa:
        matrícula total de pregrado observada en la base dividida por
        n_programas_pregrado.

    La unidad de programa prioriza Códgo SIES; si falta, usa Cód. Carrera;
    como último respaldo usa programa+sede+horario. Esto evita contar dos veces
    la misma fila cuando existan duplicados exactos.
    """
    if not os.path.exists(path):
        raise FileNotFoundError(
            f"Program/tuition workbook not found: {path}"
        )

    sheet_name = "BaseINDICES_2005_2024"
    df = pd.read_excel(path, sheet_name=sheet_name)
    df.columns = [str(column).strip() for column in df.columns]

    required = {
        "Año",
        "Nombre Institución",
        "Nombre Programa",
        "Año Inicio Actividades",
        "Valor de arancel",
        "Matrícula Total",
        "Matrícula Primer Año",
        "Promedio Puntaje (promedio matemáticas y lenguaje)",
        "Puntaje de corte (promedio de la carrera)",
        "Pregrado/Posgrado",
    }
    missing = sorted(required - set(df.columns))
    if missing:
        raise ValueError(
            "BaseDefinitivaINDICES has an incompatible schema. "
            "Missing columns: " + ", ".join(missing)
        )

    code_sies_col = _first_existing_column(
        df.columns,
        ["Códgo SIES", "Código SIES", "Codgo SIES", "Codigo SIES"]
    )
    career_code_col = _first_existing_column(
        df.columns,
        ["Cód. Carrera", "Cod. Carrera", "Código Carrera", "Codigo Carrera"]
    )
    site_col = _first_existing_column(
        df.columns,
        ["Nombre de la Sede", "Sede"]
    )
    schedule_col = _first_existing_column(
        df.columns,
        ["Horario"]
    )
    currency_col = _first_existing_column(
        df.columns,
        ["Tipo Moneda", "TipoMonedaArancelAnual"]
    )

    name_lookup = build_mapping_name_lookup(final_map)

    df = df.copy()
    df["Año"] = pd.to_numeric(df["Año"], errors="coerce").astype("Int64")
    df["Año Inicio Actividades"] = pd.to_numeric(
        df["Año Inicio Actividades"], errors="coerce"
    ).astype("Int64")
    df["Valor de arancel"] = pd.to_numeric(
        df["Valor de arancel"], errors="coerce"
    )
    df["Matrícula Total"] = pd.to_numeric(
        df["Matrícula Total"], errors="coerce"
    )
    df["Matrícula Primer Año"] = pd.to_numeric(
        df["Matrícula Primer Año"], errors="coerce"
    )
    df["Promedio Puntaje (promedio matemáticas y lenguaje)"] = pd.to_numeric(
        df["Promedio Puntaje (promedio matemáticas y lenguaje)"],
        errors="coerce"
    )
    df["Puntaje de corte (promedio de la carrera)"] = pd.to_numeric(
        df["Puntaje de corte (promedio de la carrera)"],
        errors="coerce"
    )

    df["Universidad_std"] = (
        df["Nombre Institución"]
        .map(normalize_university_for_key)
        .map(name_lookup)
    )
    df["nivel"] = df["Pregrado/Posgrado"].map(_normalize_level_label)

    # Sólo conservamos universidades del panel y años relevantes.
    df = df.loc[
        df["Universidad_std"].notna()
        & df["Año"].between(2011, 2022)
        & df["nivel"].notna()
    ].copy()

    # Identificador robusto de programa/oferta.
    program_id = pd.Series(pd.NA, index=df.index, dtype="object")

    if code_sies_col is not None:
        sies = df[code_sies_col].astype("string").str.strip()
        valid = sies.notna() & sies.ne("") & sies.ne("<NA>")
        program_id.loc[valid] = "sies:" + sies.loc[valid]

    if career_code_col is not None:
        career = df[career_code_col].astype("string").str.strip()
        valid = program_id.isna() & career.notna() & career.ne("") & career.ne("<NA>")
        program_id.loc[valid] = "career:" + career.loc[valid]

    fallback_parts = [
        df["Nombre Programa"].fillna("").astype(str).str.strip(),
    ]
    if site_col is not None:
        fallback_parts.append(df[site_col].fillna("").astype(str).str.strip())
    if schedule_col is not None:
        fallback_parts.append(df[schedule_col].fillna("").astype(str).str.strip())

    fallback = fallback_parts[0]
    for part in fallback_parts[1:]:
        fallback = fallback + "|" + part

    missing_id = program_id.isna()
    program_id.loc[missing_id] = "fallback:" + fallback.loc[missing_id].map(
        normalize_university_for_key
    )
    df["program_id"] = program_id

    # Una sola observación por universidad-año-nivel-programa.
    # Si hay duplicados exactos del mismo programa, conservamos la mayor matrícula
    # y un arancel no faltante cuando exista.
    df = df.sort_values(
        ["Universidad_std", "Año", "nivel", "program_id", "Matrícula Total"],
        na_position="first"
    )
    program = (
        df.groupby(
            ["Universidad_std", "Año", "nivel", "program_id"],
            as_index=False,
            dropna=False
        )
        .agg(
            nombre_programa=("Nombre Programa", "first"),
            inicio=("Año Inicio Actividades", "min"),
            arancel=("Valor de arancel", "max"),
            matricula_programa=("Matrícula Total", "max"),
            matricula_primer_ano=("Matrícula Primer Año", "max"),
            puntaje_ml=(
                "Promedio Puntaje (promedio matemáticas y lenguaje)", "max"
            ),
            puntaje_corte_promedio=(
                "Puntaje de corte (promedio de la carrera)", "max"
            ),
            tipo_moneda=(currency_col, "first") if currency_col is not None
            else ("Nombre Programa", lambda x: "Pesos"),
        )
    )

    program["is_new"] = (
        program["inicio"].notna()
        & program["Año"].notna()
        & program["inicio"].eq(program["Año"])
    )

    keys = ["Universidad_std", "Año"]
    universe = (
        df[keys]
        .drop_duplicates()
        .sort_values(keys)
        .reset_index(drop=True)
    )

    # Conteos por nivel.
    counts = (
        program.groupby(keys + ["nivel"])["program_id"]
        .nunique()
        .unstack("nivel")
        .rename(columns={
            "pregrado": N_PROGRAMAS_PREGRADO_COL,
            "postgrado": N_PROGRAMAS_POSTGRADO_COL,
        })
        .reset_index()
    )

    new_counts = (
        program.loc[program["is_new"]]
        .groupby(keys + ["nivel"])["program_id"]
        .nunique()
        .unstack("nivel")
        .rename(columns={
            "pregrado": NEW_PROGRAMS_PREGRADO_COL,
            "postgrado": NEW_PROGRAMS_POSTGRADO_COL,
        })
        .reset_index()
    )

    out = universe.merge(counts, on=keys, how="left", validate="one_to_one")
    out = out.merge(new_counts, on=keys, how="left", validate="one_to_one")

    for column in [
        N_PROGRAMAS_PREGRADO_COL,
        N_PROGRAMAS_POSTGRADO_COL,
        NEW_PROGRAMS_PREGRADO_COL,
        NEW_PROGRAMS_POSTGRADO_COL,
    ]:
        if column not in out.columns:
            out[column] = 0
        out[column] = pd.to_numeric(out[column], errors="coerce").fillna(0).astype("Int64")

    # Aranceles: sólo pregrado y moneda pesos.
    pre = program.loc[program["nivel"].eq("pregrado")].copy()
    currency_key = pre["tipo_moneda"].map(normalize_university_for_key)
    peso_mask = currency_key.str.contains("peso", na=False)
    pre.loc[~peso_mask, "arancel"] = np.nan

    # Cero se considera no informado para arancel: no existe arancel anual
    # económicamente interpretable igual a $0 en esta aplicación.
    pre.loc[pre["arancel"].le(0), "arancel"] = np.nan

    arancel_stats = (
        pre.groupby(keys)["arancel"]
        .agg(
            **{
                ARANCEL_MEDIO_COL: "mean",
                ARANCEL_MEDIANO_COL: "median",
                "_arancel_min": "min",
                "_arancel_max": "max",
            }
        )
        .reset_index()
    )
    arancel_stats[ARANCEL_DIF_COL] = (
        arancel_stats["_arancel_max"] - arancel_stats["_arancel_min"]
    )
    arancel_stats = arancel_stats.drop(columns=["_arancel_min", "_arancel_max"])

    # Arancel ponderado por matrícula del programa.
    weighted = pre.loc[
        pre["arancel"].notna()
        & pre["matricula_programa"].gt(0)
    ].copy()
    weighted["_tuition_x_students"] = (
        weighted["arancel"] * weighted["matricula_programa"]
    )
    weighted_summary = (
        weighted.groupby(keys, as_index=False)
        .agg(
            _tuition_x_students=("_tuition_x_students", "sum"),
            _students_with_tuition=("matricula_programa", "sum"),
        )
    )
    weighted_summary[ARANCEL_MATRICULA_COL] = (
        weighted_summary["_tuition_x_students"]
        / weighted_summary["_students_with_tuition"].replace(0, np.nan)
    )
    weighted_summary = weighted_summary[
        keys + [ARANCEL_MATRICULA_COL]
    ]

    # Estudiantes promedio por programa de pregrado.
    pre_enrollment = (
        pre.groupby(keys, as_index=False)
        .agg(_matricula_pregrado_programas=("matricula_programa", "sum"))
    )
    out = out.merge(arancel_stats, on=keys, how="left", validate="one_to_one")
    out = out.merge(weighted_summary, on=keys, how="left", validate="one_to_one")
    out = out.merge(pre_enrollment, on=keys, how="left", validate="one_to_one")

    out[ESTUDIANTES_PROM_PROGRAMA_COL] = (
        out["_matricula_pregrado_programas"]
        / out[N_PROGRAMAS_PREGRADO_COL].replace(0, np.nan)
    )
    out = out.drop(columns=["_matricula_pregrado_programas"])

    # Puntajes agregados: sólo pregrado y ponderados por matrícula de primer año.
    # Esto aproxima mejor la selectividad efectiva de la universidad que un
    # promedio simple entre carreras.
    score_base = pre.loc[pre["matricula_primer_ano"].gt(0)].copy()

    def _weighted_score_summary(frame, score_col, output_col):
        valid = frame.loc[
            frame[score_col].notna()
            & frame["matricula_primer_ano"].gt(0)
        ].copy()

        if valid.empty:
            return pd.DataFrame(columns=keys + [output_col])

        valid["_score_x_firstyear"] = (
            valid[score_col] * valid["matricula_primer_ano"]
        )
        summary = (
            valid.groupby(keys, as_index=False)
            .agg(
                _score_x_firstyear=("_score_x_firstyear", "sum"),
                _firstyear_weight=("matricula_primer_ano", "sum"),
            )
        )
        summary[output_col] = (
            summary["_score_x_firstyear"]
            / summary["_firstyear_weight"].replace(0, np.nan)
        )
        return summary[keys + [output_col]]

    ml_summary = _weighted_score_summary(
        score_base,
        "puntaje_ml",
        PUNTAJE_ML_PROMEDIO_COL,
    )
    corte_summary = _weighted_score_summary(
        score_base,
        "puntaje_corte_promedio",
        PUNTAJE_CORTE_PROMEDIO_COL,
    )

    out = out.merge(ml_summary, on=keys, how="left", validate="one_to_one")
    out = out.merge(corte_summary, on=keys, how="left", validate="one_to_one")

    duplicate_mask = out.duplicated(keys, keep=False)
    if duplicate_mask.any():
        raise ValueError(
            "Duplicate Universidad_std x Año rows found in program/tuition panel:\n"
            + out.loc[duplicate_mask].sort_values(keys).to_string(index=False)
        )

    return out

# --- 1) LOAD AUTHORITATIVE MAPPING V2 -----------------------------------------
final_map = load_mapping_v2(MAPPING_CSV)

key_to_final = dict(
    zip(final_map["Universidad_key"], final_map["Final_Standard"])
)
key_to_cned_name = dict(
    zip(final_map["Universidad_key"], final_map["CNED_Universidad"])
)
key_to_cned_key = dict(
    zip(final_map["Universidad_key"], final_map["CNED_key"])
)


# --- 2) LOAD ALL PANEL FILES ---------------------------------------------------
excel_paths = sorted(glob.glob(os.path.join(DATA_DIR, PATTERN)))

if not excel_paths:
    raise FileNotFoundError(
        f"No Excel files matched {PATTERN} in {DATA_DIR}"
    )

frames = []
origins = []

for fp in excel_paths:
    try:
        df = pd.read_excel(fp, sheet_name="Sheet1")
        used_sheet = "Sheet1"
    except Exception:
        xls = pd.ExcelFile(fp)
        used_sheet = xls.sheet_names[0]
        df = pd.read_excel(fp, sheet_name=used_sheet)

    df.columns = [normalize_column_name(c) for c in df.columns]

    keep = [
        c for c in ["Universidad", "Indicador", "Año", "Valor"]
        if c in df.columns
    ]
    df = df[keep].copy()

    if "Universidad" in df:
        df["Universidad"] = df["Universidad"].astype(str).str.strip()

    if "Indicador" in df:
        df["Indicador"] = df["Indicador"].astype(str).str.strip()

    if "Año" in df:
        df["Año"] = pd.to_numeric(
            df["Año"], errors="coerce"
        ).astype("Int64")

    if "Valor" in df:
        df["Valor"] = df["Valor"].apply(clean_value)

    df["__file"] = os.path.basename(fp)
    df["__sheet"] = used_sheet

    frames.append(df)
    origins.append((fp, used_sheet, df.shape))

panel = pd.concat(frames, ignore_index=True)
panel = split_compound_values(panel)


# --- 3) BUILD PANEL UNIVERSITY KEY --------------------------------------------
panel["Universidad_raw"] = panel["Universidad"].fillna("")
panel["Universidad_key"] = (
    panel["Universidad_raw"]
    .apply(normalize_university_for_key)
)


# --- 4) VALIDATE PANEL AGAINST MAPPING V2 -------------------------------------
panel_keys = set(
    panel.loc[
        panel["Universidad_key"].ne(""),
        "Universidad_key"
    ].unique()
)

mapping_keys = set(final_map["Universidad_key"])

unmapped_keys = sorted(panel_keys - mapping_keys)

if unmapped_keys:
    examples = (
        panel.loc[
            panel["Universidad_key"].isin(unmapped_keys),
            ["Universidad_key", "Universidad_raw"]
        ]
        .drop_duplicates()
        .sort_values(["Universidad_key", "Universidad_raw"])
    )

    raise ValueError(
        "The panel contains university names not present in "
        "university_name_mapping_2.csv.\n\n"
        + examples.to_string(index=False)
    )


# --- 5) APPLY MAPPING V2 -------------------------------------------------------
panel["Universidad_std"] = panel["Universidad_key"].map(key_to_final)
panel["CNED_Universidad"] = panel["Universidad_key"].map(key_to_cned_name)
panel["CNED_key"] = panel["Universidad_key"].map(key_to_cned_key)

# If Universidad_raw is empty, Universidad_std may also be empty/NA.
panel["Universidad_std"] = (
    panel["Universidad_std"]
    .fillna(panel["Universidad_raw"])
)


# --- 6) MERGE ACCREDITATION BY UNIVERSIDAD_STD + AÑO -------------------------
accreditation = load_accreditation_panel(
    ACCREDITATION_XLSX,
    final_map
)

panel = panel.merge(
    accreditation,
    on=["Universidad_std", "Año"],
    how="left",
    validate="many_to_one"
)
panel = apply_pucv_accreditation(panel)


# --- 7) MERGE UNDERGRADUATE ENROLLMENT BY UNIVERSIDAD_STD + AÑO --------------
undergrad_enrollment = load_undergrad_enrollment_panel(
    UNDERGRAD_ENROLLMENT_XLSX,
    final_map
)

panel = panel.merge(
    undergrad_enrollment,
    on=["Universidad_std", "Año"],
    how="left",
    validate="many_to_one"
)


# --- 8) MERGE STUDENT ELIGIBILITY BY UNIVERSIDAD_STD + AÑO -------------------
eligibility = load_eligibility_panel(
    ELIGIBILITY_XLSX,
    final_map
)

panel = panel.merge(
    eligibility,
    on=["Universidad_std", "Año"],
    how="left",
    validate="many_to_one"
)

# Elegibilidad define alumnos como matrícula de pregrado.  Sólo se usa como
# respaldo cuando la planilla específica no tiene una observación.
undergrad_fallback = (
    panel["Matrícula pregrado"].isna()
    & panel[TOTAL_STUDENTS_COL].notna()
)
panel.loc[undergrad_fallback, "Matrícula pregrado"] = panel.loc[
    undergrad_fallback, TOTAL_STUDENTS_COL
]


# --- 9) MERGE TOTAL ENROLLMENT BY UNIVERSIDAD_STD + AÑO -----------------------
total_enrollment = load_total_enrollment_panel(
    TOTAL_ENROLLMENT_XLSX,
    final_map
)
panel = panel.merge(
    total_enrollment,
    on=["Universidad_std", "Año"],
    how="left",
    validate="many_to_one"
)


# --- 10) MERGE CHILE INFLATION -------------------------------------------------
inflation_wb = load_chile_wb_inflation(CPI_XLS, CPI_CACHE_CSV)

panel = panel.merge(
    inflation_wb,
    on="Año",
    how="left",
    validate="many_to_one"
)


# --- 11) MERGE PROGRAM OFFER, POSTGRAD OFFER AND TUITION INDICATORS ------------
program_offer = load_program_offer_panel(
    PROGRAM_OFFER_XLSX,
    final_map
)

panel = panel.merge(
    program_offer,
    on=["Universidad_std", "Año"],
    how="left",
    validate="many_to_one"
)


# --- 12) ORDER AND SAVE --------------------------------------------------------
ordered_cols = [
    "Universidad_std",
    "Universidad",
    "Universidad_raw",
    "Universidad_key",
    "CNED_Universidad",
    "CNED_key",
    "Indicador",
    "Año",
    "Acreditación (años)",
    "Matrícula pregrado",
    TOTAL_ENROLLMENT_COL,
    TOTAL_STUDENTS_COL,
    ELIGIBILITY_PROXY_COL,
    ELIGIBILITY_FUAS_COL,
    ELIGIBILITY_PROXY_FUAS_COL,
    ELIGIBILITY_PROXY_COVERAGE_COL,
    ELIGIBILITY_FUAS_COVERAGE_COL,
    INFLATION_COL,
    N_PROGRAMAS_PREGRADO_COL,
    N_PROGRAMAS_POSTGRADO_COL,
    NEW_PROGRAMS_PREGRADO_COL,
    NEW_PROGRAMS_POSTGRADO_COL,
    ARANCEL_MEDIO_COL,
    ARANCEL_MEDIANO_COL,
    ARANCEL_MATRICULA_COL,
    ARANCEL_DIF_COL,
    ESTUDIANTES_PROM_PROGRAMA_COL,
    PUNTAJE_ML_PROMEDIO_COL,
    PUNTAJE_CORTE_PROMEDIO_COL,
    "Valor",
    "__file",
    "__sheet",
]

panel = panel[
    [c for c in ordered_cols if c in panel.columns]
]

panel.to_csv(
    CLEAN_CSV,
    index=False,
    encoding="utf-8-sig"
)


# --- 13) REPORT ---------------------------------------------------------------
print("\n=== Files loaded ===")
for fp, sheet, shape in origins:
    print(
        f"- {os.path.basename(fp)} :: '{sheet}' "
        f"-> {shape[0]} rows, {shape[1]} cols"
    )

print("\n=== Mapping v2 ===")
print("- Mapping file:", MAPPING_CSV)
print("- Universities in mapping:", final_map["Universidad_key"].nunique())
print("- Distinct Final_Standard:", final_map["Final_Standard"].nunique())
print("- Distinct CNED_key:", final_map["CNED_key"].nunique())

print("\n=== Panel ===")
print("- Distinct standardized universities:", panel["Universidad_std"].nunique())
print("- Rows:", len(panel))
print("- Rows with missing Valor:", panel["Valor"].isna().sum())
print("- Rows with missing Acreditación (años):", panel["Acreditación (años)"].isna().sum())
print("- Rows with missing Matrícula pregrado:", panel["Matrícula pregrado"].isna().sum())
print(
    f"- Rows with missing {TOTAL_ENROLLMENT_COL}:",
    panel[TOTAL_ENROLLMENT_COL].isna().sum()
)
print(
    f"- Rows with missing {TOTAL_STUDENTS_COL}:",
    panel[TOTAL_STUDENTS_COL].isna().sum()
)
print(
    f"- Rows with missing {INFLATION_COL}:",
    panel[INFLATION_COL].isna().sum()
)

print("\n=== Programas y aranceles ===")
for column in [
    N_PROGRAMAS_PREGRADO_COL,
    N_PROGRAMAS_POSTGRADO_COL,
    NEW_PROGRAMS_PREGRADO_COL,
    NEW_PROGRAMS_POSTGRADO_COL,
    ARANCEL_MEDIO_COL,
    ARANCEL_MEDIANO_COL,
    ARANCEL_MATRICULA_COL,
    ARANCEL_DIF_COL,
    ESTUDIANTES_PROM_PROGRAMA_COL,
    PUNTAJE_ML_PROMEDIO_COL,
    PUNTAJE_CORTE_PROMEDIO_COL,
]:
    print(
        f"- University-year pairs with nonmissing {column}:",
        panel.loc[panel[column].notna(), ["Universidad_std", "Año"]]
        .drop_duplicates()
        .shape[0]
    )
for column in [
    ELIGIBILITY_PROXY_COL,
    ELIGIBILITY_FUAS_COL,
    ELIGIBILITY_PROXY_FUAS_COL,
]:
    print(f"- Rows with missing {column}:", panel[column].isna().sum())

missing_acc_pairs = (
    panel.loc[
        panel["Acreditación (años)"].isna(),
        ["Universidad_std", "Año"]
    ]
    .drop_duplicates()
    .sort_values(["Universidad_std", "Año"])
)

print(
    "- University-year pairs with missing accreditation:",
    len(missing_acc_pairs)
)

if not missing_acc_pairs.empty:
    print("Universities with at least one missing accreditation value:")
    print(
        "  "
        + ", ".join(
            missing_acc_pairs["Universidad_std"]
            .drop_duplicates()
            .astype(str)
            .tolist()
        )
    )

missing_enrollment_pairs = (
    panel.loc[
        panel["Matrícula pregrado"].isna(),
        ["Universidad_std", "Año"]
    ]
    .drop_duplicates()
    .sort_values(["Universidad_std", "Año"])
)

print(
    "- University-year pairs with missing Matrícula pregrado:",
    len(missing_enrollment_pairs)
)

missing_eligibility_pairs = (
    panel.loc[
        panel[ELIGIBILITY_PROXY_COL].isna(),
        ["Universidad_std", "Año"]
    ]
    .drop_duplicates()
    .sort_values(["Universidad_std", "Año"])
)

print(
    f"- University-year pairs with missing {ELIGIBILITY_PROXY_COL}:",
    len(missing_eligibility_pairs)
)

print("\nSaved cleaned panel to:", CLEAN_CSV)
