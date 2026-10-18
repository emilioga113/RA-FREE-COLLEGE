# -*- coding: utf-8 -*-
"""
Fitch indicators — versión funcional ("def") v4 — corregida

Cambios:
1) Usa Fixed Assets como nombre Fitch para Total activos no corrientes, de
   acuerdo con la convención del proyecto, en lugar de Edificios.
2) Calcula activos/pasivos, CapEx/matrícula total, gasto operacional/matrícula
   total e ingresos operacionales/matrícula total.
3) Elimina los tres resultados que dependen de EBITDA.
4) Incorpora el crecimiento real anual del ingreso operacional por estudiante.
5) Incorpora variaciones logarítmicas reales anuales de ingresos, gasto
   operacional y CapEx por estudiante, sin confundir cambios de unidad con
   cambios económicos.

Outputs:
- wide_after_mapping_def.csv
- reporte_faltantes.csv
- ratios_fitch_panel_def.csv
- plots por universidad

Las covariables institucionales (acreditación, matrículas y porcentajes de
elegibilidad) se conservan en los dos paneles CSV de salida y se validan como
únicas para cada combinación universidad-año.
"""
import os
import re
from pathlib import Path
import numpy as np
import pandas as pd
try:
    import matplotlib.pyplot as plt
except ImportError:  # El panel tabular no depende de gráficos.
    plt = None

# ---------------------------
# Config por defecto
# ---------------------------
# Carpeta raíz del panel financiero.
# Los archivos principales viven directamente aquí.
REPOSITORY_ROOT = Path(__file__).resolve().parents[2]
PANEL_DIR = os.environ.get(
    "FITCH_PANEL_DIR",
    str(REPOSITORY_ROOT / "Construcción de panel data")
)

PROJECT_DIR = PANEL_DIR
BASE_DIR = PANEL_DIR

INPUT_PANEL = os.environ.get(
    "FITCH_INPUT_PANEL",
    os.path.join(PANEL_DIR, "panel_cleaned_free_college.csv")
)

INPUT_MAP = os.environ.get(
    "FITCH_INPUT_MAP",
    os.path.join(PANEL_DIR, "mapeo_indicadores.csv")
)

# Output principal solicitado: misma carpeta que panel_cleaned_3.csv.
RATIOS_OUTPUT_PATH = os.environ.get(
    "FITCH_RATIOS_OUTPUT",
    os.path.join(PANEL_DIR, "ratios_fitch_panel.csv")
)

# Outputs auxiliares en la carpeta reportes ya existente.
OUT_DIR = os.environ.get(
    "FITCH_OUTPUT_DIR",
    os.path.join(PANEL_DIR, "reportes", "fitch_indicators_def")
)
PLOTS_DIR = os.path.join(OUT_DIR, "plots")

YEAR_COL = "Año"
ACCREDITATION_COL = "Acreditación (años)"
ENROLLMENT_COL = "Matrícula pregrado"
TOTAL_ENROLLMENT_COL = "Matrícula Total"
TOTAL_STUDENTS_COL = "N total estudiantes pregrado"
ELIGIBILITY_PROXY_COL = "% elegibles gratuidad (proxy escolar)"
ELIGIBILITY_FUAS_COL = "% elegibles gratuidad (FUAS)"
ELIGIBILITY_PROXY_FUAS_COL = (
    "% elegibles gratuidad (proxy escolar, muestra FUAS)"
)
ELIGIBILITY_PROXY_COVERAGE_COL = "Cobertura elegibilidad proxy (%)"
ELIGIBILITY_FUAS_COVERAGE_COL = "Cobertura FUAS (%)"
INFLATION_COL = "Inflation_WB"

COVARIATE_COLS = [
    ACCREDITATION_COL,
    ENROLLMENT_COL,
    TOTAL_ENROLLMENT_COL,
    TOTAL_STUDENTS_COL,
    ELIGIBILITY_PROXY_COL,
    ELIGIBILITY_FUAS_COL,
    ELIGIBILITY_PROXY_FUAS_COL,
    ELIGIBILITY_PROXY_COVERAGE_COL,
    ELIGIBILITY_FUAS_COVERAGE_COL,
    INFLATION_COL,
]

# ---------------------------
# Labels estándar Fitch
# ---------------------------
COL_REV = "Adjusted Total Operating Revenue"
COL_OPEX = "Total Operating Expense"
COL_AF = "Available Funds (AF)"
COL_ST_L = "Short-term Liabilities"
COL_LT_L = "Long-term Liabilities"
COL_TOTAL_LIAB = "Total Liabilities"
COL_CAPEX = "Capital Expenditures (Net)"
COL_DEPR = "Depreciation Expense"
COL_EBITDA = "EBITDA (Proxy for Cash Flow)"
COL_ASSETS = "Total Assets"
COL_CURRENT_ASSETS = "Current Assets"

# Claves corregidas
COL_EQUITY = "Net Assets / Equity"
COL_FIXED_ASSETS = "Fixed Assets"

# Ratios históricos reincorporados
R_OPER_MARGIN = "Adjusted Operating Margin (%)"
R_CF_MARGIN = "Cash Flow Margin, Adjusted (%)"
R_DEBT_SERVICE = "Current Debt Service"
R_CAPEX_DEPR = "Capital Expenditures / Depreciation Expense"
R_GROSS_MARGIN = "Gross Margin (%)"

# Ratios actuales
R_AF_TLBL = "Available Funds / Total Liabilities (%)"
R_CAPEX_FIXED = "Capital Expenditures / Fixed Assets"
R_DEPR_FIXED = "Depreciation / Fixed Assets"
R_ASSET_TURNOVER = "Asset Turnover"
R_LIAB_EQUITY = "Total Liabilities / Equity"
R_ASSETS_LIAB = "Total Assets / Total Liabilities"
R_CAPEX_ENROLLMENT = "Capital Expenditures / Total Enrollment"
R_OPEX_ENROLLMENT = "Total Operating Expense / Total Enrollment"
R_REVENUE_ENROLLMENT = "Adjusted Total Operating Revenue / Total Enrollment"
R_REVENUE_STUDENT_REAL_DLOG = (
    "Real Operating Revenue per Student - Annual Log Change"
)
R_OPEX_STUDENT_REAL_DLOG = (
    "Real Operating Expense per Student - Annual Log Change"
)
R_CAPEX_STUDENT_REAL_DLOG = (
    "Real Capital Expenditures per Student - Annual Log Change"
)

F_REVENUE_STUDENT_UNIT_BREAK = (
    "Real Operating Revenue per Student - Suspected Unit Break"
)
F_OPEX_STUDENT_UNIT_BREAK = (
    "Real Operating Expense per Student - Suspected Unit Break"
)
F_CAPEX_STUDENT_UNIT_BREAK = (
    "Real Capital Expenditures per Student - Suspected Unit Break"
)

QUALITY_FLAG_COLS = [
    F_REVENUE_STUDENT_UNIT_BREAK,
    F_OPEX_STUDENT_UNIT_BREAK,
    F_CAPEX_STUDENT_UNIT_BREAK,
]

ALL_RATIOS = [
    # Indicadores históricos
    R_OPER_MARGIN,
    R_CF_MARGIN,
    R_DEBT_SERVICE,
    R_CAPEX_DEPR,
    R_GROSS_MARGIN,

    # Indicadores actuales
    R_AF_TLBL,
    R_CAPEX_FIXED,
    R_DEPR_FIXED,
    R_ASSET_TURNOVER,
    R_LIAB_EQUITY,
    R_ASSETS_LIAB,
    R_CAPEX_ENROLLMENT,
    R_OPEX_ENROLLMENT,
    R_REVENUE_ENROLLMENT,
    R_REVENUE_STUDENT_REAL_DLOG,
    R_OPEX_STUDENT_REAL_DLOG,
    R_CAPEX_STUDENT_REAL_DLOG,
]

# ---------------------------
# Utilidades base
# ---------------------------

def ensure_dirs():
    os.makedirs(OUT_DIR, exist_ok=True)
    os.makedirs(PLOTS_DIR, exist_ok=True)


def safe_read_csv(path, **kwargs):
    if not os.path.exists(path):
        raise FileNotFoundError(f"No se encuentra el archivo: {path}")
    return pd.read_csv(path, encoding="utf-8-sig", **kwargs)


def norm_text(s: str) -> str:
    s = str(s or "").strip()
    s = re.sub(r"\s+", " ", s)
    return s


def pick_uni_col(df: pd.DataFrame) -> str:
    for c in ["Universidad_std", "Universidad", "Universidad_raw"]:
        if c in df.columns:
            return c
    raise ValueError("No encuentro columnas de Universidad.")


def as_num(s):
    return pd.to_numeric(s, errors="coerce")


def extract_university_year_covariates(
    df_long: pd.DataFrame,
    uni_col: str,
) -> pd.DataFrame:
    """Obtiene y valida una fila de covariables por universidad-año."""
    keys = [uni_col, YEAR_COL]
    covariate_cols = COVARIATE_COLS
    covariates = df_long[keys + covariate_cols].copy()

    for column in covariate_cols:
        conflicts = covariates.groupby(keys, dropna=False)[column].nunique(
            dropna=False
        )
        if conflicts.gt(1).any():
            examples = conflicts[conflicts.gt(1)].head(10).index.tolist()
            raise ValueError(
                f"Hay más de un valor de {column} para una universidad-año. "
                f"Primeros casos: {examples}"
            )

    covariates = covariates.drop_duplicates(keys)
    for column in covariate_cols:
        covariates[column] = as_num(covariates[column])
    return covariates


def add_university_year_covariates(
    wide: pd.DataFrame,
    covariates: pd.DataFrame,
    uni_col: str,
) -> pd.DataFrame:
    """Une covariables al panel ancho con cardinalidad uno-a-uno."""
    keys = [uni_col, YEAR_COL]
    result = wide.merge(covariates, on=keys, how="left", validate="one_to_one")

    labels = {
        ACCREDITATION_COL: "años de acreditación",
        ENROLLMENT_COL: "matrícula de pregrado",
        TOTAL_ENROLLMENT_COL: "matrícula total",
        TOTAL_STUDENTS_COL: "número total de estudiantes de pregrado",
        ELIGIBILITY_PROXY_COL: "elegibilidad por proxy escolar",
        ELIGIBILITY_FUAS_COL: "elegibilidad FUAS",
        ELIGIBILITY_PROXY_FUAS_COL: (
            "elegibilidad proxy en la muestra con FUAS"
        ),
        ELIGIBILITY_PROXY_COVERAGE_COL: "cobertura del proxy escolar",
        ELIGIBILITY_FUAS_COVERAGE_COL: "cobertura FUAS",
        INFLATION_COL: "inflación anual del World Bank",
    }
    for column, label in labels.items():
        missing = result[column].isna().sum()
        if missing:
            print(
                f"[Aviso] {missing} combinaciones universidad-año del panel "
                f"Fitch no tienen {label} observable."
            )

    return result


# ---------------------------
# Carga + mapeo
# ---------------------------

def load_and_map(input_panel: str, input_map: str):
    df_long = safe_read_csv(input_panel)
    map_df = safe_read_csv(input_map)

    req_panel = {"Indicador", YEAR_COL, "Valor", *COVARIATE_COLS}
    if not req_panel.issubset(df_long.columns):
        raise ValueError(
            f"Faltan columnas en panel_cleaned.csv: {req_panel - set(df_long.columns)}"
        )

    req_map = {"indicador_original", "nombre_fitch_objetivo"}
    if not req_map.issubset(map_df.columns):
        raise ValueError(
            f"Faltan columnas en mapeo_indicadores.csv: {req_map - set(map_df.columns)}"
        )

    map_df = map_df.copy()
    map_df["indicador_original"] = (
        map_df["indicador_original"].fillna("").astype(str).str.strip()
    )
    map_df["nombre_fitch_objetivo"] = (
        map_df["nombre_fitch_objetivo"].fillna("").astype(str).str.strip()
    )

    # Mantener solo mapeos explícitos no vacíos
    map_df = map_df[map_df["nombre_fitch_objetivo"] != ""].drop_duplicates(
        subset=["indicador_original"], keep="last"
    )

    map_df["indicador_original_norm"] = map_df["indicador_original"].map(norm_text)

    uni_col = pick_uni_col(df_long)
    covariates = extract_university_year_covariates(df_long, uni_col)

    df = df_long.copy()
    df["Indicador_norm"] = df["Indicador"].map(norm_text)

    df = df.merge(
        map_df[["indicador_original_norm", "nombre_fitch_objetivo"]],
        left_on="Indicador_norm",
        right_on="indicador_original_norm",
        how="left",
    )

    # Proxy CapEx: el flujo neto de inversión NO es CapEx contable puro.
    # Se fuerza a etiqueta proxy aunque el mapping lo llame CapEx directo.
    mask_capex_proxy = df["Indicador_norm"].str.contains(
        "flujo neto negativo de inversión", case=False, na=False
    )
    df.loc[mask_capex_proxy, "nombre_fitch_objetivo"] = (
        "Capital Expenditures (Net) [Proxy]"
    )

    # Fallbacks explícitos para los indicadores históricos.
    mask_depr = (
        df["nombre_fitch_objetivo"].isna()
        & df["Indicador_norm"].str.contains(
            r"gastos? de depreciaci[oó]n|depreciaci[oó]n",
            case=False,
            na=False,
            regex=True,
        )
    )
    df.loc[mask_depr, "nombre_fitch_objetivo"] = COL_DEPR

    mask_ebitda = (
        df["nombre_fitch_objetivo"].isna()
        & df["Indicador_norm"].str.contains(
            r"ebitda|ebidta",
            case=False,
            na=False,
            regex=True,
        )
    )
    df.loc[mask_ebitda, "nombre_fitch_objetivo"] = COL_EBITDA

    # Convención del proyecto: Total activos no corrientes se presenta como
    # Fixed Assets. No se usa Edificios como proxy.
    mask_noncurrent_assets = df["nombre_fitch_objetivo"].isna() & df["Indicador_norm"].str.contains(
        "total activos no corrientes", case=False, na=False
    )
    df.loc[mask_noncurrent_assets, "nombre_fitch_objetivo"] = COL_FIXED_ASSETS

    unmapped = df[df["nombre_fitch_objetivo"].isna()]["Indicador"].drop_duplicates().sort_values()
    if len(unmapped) > 0:
        print("[Aviso] Indicadores sin mapeo (se omitirán):")
        for v in unmapped.tolist():
            print(" -", v)

    df = df[~df["nombre_fitch_objetivo"].isna()].copy()
    return df, covariates


# ---------------------------
# Pivot
# ---------------------------

def to_wide(
    df: pd.DataFrame,
    covariates: pd.DataFrame,
    out_dir: str,
) -> tuple[pd.DataFrame, str]:
    uni_col = pick_uni_col(df)

    wide = (
        df.pivot_table(
            index=[uni_col, YEAR_COL],
            columns="nombre_fitch_objetivo",
            values="Valor",
            aggfunc="first",
        )
        .reset_index()
    )

    wide = add_university_year_covariates(wide, covariates, uni_col)

    path = os.path.join(out_dir, "wide_after_mapping_def.csv")
    wide.to_csv(path, index=False, encoding="utf-8-sig")
    return wide, uni_col


# ---------------------------
# Helpers
# ---------------------------

def get_series(df, primary, proxies=None):
    proxies = proxies or []
    if primary in df.columns:
        return df[primary], True, False
    for p in proxies:
        if p in df.columns:
            return df[p], True, True
    return pd.Series(np.nan, index=df.index), False, False


def add_real_annual_log_change(
    out: pd.DataFrame,
    value_col: str,
    result_col: str,
    unit_break_col: str,
) -> pd.DataFrame:
    """Calcula Δln(real por estudiante) solo para años consecutivos.

    La inflación de World Bank viene almacenada en tasa decimal (por ejemplo, 0.03 = 3%). Un salto nominal
    de 100 veces o más se marca como posible quiebre de unidad. La observación
    se conserva en el resultado: la bandera es una alerta de auditoría y no una
    regla automática de eliminación de outliers.
    """
    uni_col = pick_uni_col(out)
    ordered = out.sort_values([uni_col, YEAR_COL]).copy()
    value = as_num(ordered[value_col])
    year = as_num(ordered[YEAR_COL])
    inflation_rate = as_num(ordered[INFLATION_COL])

    grouped_value = value.groupby(ordered[uni_col], sort=False)
    grouped_year = year.groupby(ordered[uni_col], sort=False)
    lag_value = grouped_value.shift(1)
    lag_year = grouped_year.shift(1)

    consecutive = year.eq(lag_year + 1)
    positive = value.gt(0) & lag_value.gt(0)
    valid_inflation = inflation_rate.notna() & inflation_rate.gt(-1)
    valid = consecutive & positive & valid_inflation

    nominal_dlog = pd.Series(np.nan, index=ordered.index, dtype=float)
    nominal_dlog.loc[valid] = (
        np.log(value.loc[valid]) - np.log(lag_value.loc[valid])
    )

    suspected_break = valid & nominal_dlog.abs().ge(np.log(100.0))
    real_dlog = pd.Series(np.nan, index=ordered.index, dtype=float)
    real_dlog.loc[valid] = (
        nominal_dlog.loc[valid]
        - np.log1p(inflation_rate.loc[valid])
    )

    flag = pd.Series(pd.NA, index=ordered.index, dtype="Int64")
    flag.loc[valid] = suspected_break.loc[valid].astype(int)
    ordered[result_col] = real_dlog
    ordered[unit_break_col] = flag
    return ordered.sort_index()


# ---------------------------
# Cálculo de ratios
# ---------------------------

def compute_ratios(wide: pd.DataFrame) -> tuple[pd.DataFrame, dict]:
    out = wide.copy()

    notes = {
        "TLBL_PROXY": False,
        "CAPEX_PROXY": False,
        "EQUITY_PROXY": False,
        "EBITDA_PROXY": False,
    }

    # ---------------------------
    # Total Liabilities
    # ---------------------------
    if COL_TOTAL_LIAB not in out.columns:
        st_col = COL_ST_L if COL_ST_L in out.columns else (
            "Total pasivo circulante" if "Total pasivo circulante" in out.columns else None
        )
        lt_col = COL_LT_L if COL_LT_L in out.columns else (
            "Total pasivo a largo plazo" if "Total pasivo a largo plazo" in out.columns else None
        )

        if st_col and lt_col:
            st = as_num(out[st_col])
            lt = as_num(out[lt_col])
            out[COL_TOTAL_LIAB] = (st + lt).where(st.notna() & lt.notna())
            notes["TLBL_PROXY"] = True

    # ---------------------------
    # Inputs principales
    # ---------------------------

    # Pasivos totales
    tlbl, tlbl_exists, tlbl_is_proxy = get_series(
        out,
        COL_TOTAL_LIAB,
        proxies=[
            "Total pasivos",
            "Total pasivo",
            "Total pasivo circulante",
            "Total pasivo a largo plazo"
        ]
    )
    tlbl = as_num(tlbl)

    # Patrimonio
    equity, equity_exists, equity_is_proxy = get_series(
        out,
        COL_EQUITY,
        proxies=[
            "Net Assets / Equity",
            "Total Equity",
            "Total patrimonio",
            "Patrimonio",
            "Patrimonio total",
            "Total patrimonio neto",
            "Patrimonio neto"
        ]
    )
    equity = as_num(equity)

    # Fixed Assets es Total activos no corrientes según la convención del
    # proyecto. No se usa Edificios como proxy.
    fixed_assets, fixed_assets_exists, _ = get_series(
        out,
        COL_FIXED_ASSETS,
    )
    fixed_assets = as_num(fixed_assets)

    # CapEx
    capex, cap_exists, cap_is_proxy = get_series(
        out,
        COL_CAPEX,
        proxies=[
            "Capital Expenditures (Net) [Proxy]",
            "Capital Expenditures (Net)",
            "Flujos de efectivo netos utilizados en actividades de inversión",
            "Flujos de efectivo de (utilizados en) actividades de inversión",
            "Capex",
            "Gastos de capital"
        ]
    )
    capex = as_num(capex)

    # Convención económica para el proxy de CapEx:
    # el dato fuente es flujo neto de inversión con signo de caja.
    #   flujo < 0 -> salida de caja por inversión -> CapEx positivo
    #   flujo > 0 -> entrada de caja / desinversión -> CapEx negativo
    #   flujo = 0 -> CapEx = 0
    # Por tanto, para todo valor observado: CapEx = - flujo_neto_inversion.
    # Los missing se preservan como missing.
    if cap_is_proxy:
        capex_raw = capex.copy()
        capex = -capex_raw
        capex = capex.where(capex_raw.notna())

    # Revenue
    rev, rev_exists, _ = get_series(out, COL_REV)
    rev = as_num(rev)

    # Opex
    opex, opex_exists, _ = get_series(out, COL_OPEX)
    opex = as_num(opex)

    # Assets
    assets, assets_exists, _ = get_series(
        out,
        COL_ASSETS,
        proxies=[
            "Total activos",
            "Total de activos",
            "Total activos (neto)",
            "Activos",
            "Total activos corrientes + no corrientes"
        ]
    )
    assets = as_num(assets)

    # Matrícula total: denominador institucional para CapEx y gasto operacional.
    total_enrollment, enrollment_exists, _ = get_series(
        out, TOTAL_ENROLLMENT_COL
    )
    total_enrollment = as_num(total_enrollment)

    # Available funds
    af, af_exists, _ = get_series(
        out,
        COL_AF,
        proxies=[
            "Disponible",
            "Efectivo y equivalentes al final del periodo",
            "Efectivo y equivalentes al cierre",
            "Disponibilidades",
            "Caja y bancos"
        ]
    )
    af = as_num(af)

    # Depreciación
    depr, depr_exists, _ = get_series(
        out,
        COL_DEPR,
        proxies=["Gastos de Depreciación", "Depreciación", "Depreciation"]
    )
    depr = as_num(depr)

    # EBITDA: privilegiar siempre la observación reportada.
    ebitda, ebitda_exists, ebitda_is_proxy = get_series(
        out,
        COL_EBITDA,
        proxies=["EBITDA", "EBIDTA", "EBITDA (Proxy for Cash Flow)"]
    )
    ebitda = as_num(ebitda)

    # Fallback histórico sólo si no hay EBITDA observado.
    if not ebitda_exists and rev_exists and opex_exists and depr_exists:
        ebitda = (rev - opex + depr).where(
            rev.notna() & opex.notna() & depr.notna()
        )
        ebitda_exists = True
        ebitda_is_proxy = True

    # ---------------------------
    # Notas proxy
    # ---------------------------
    if tlbl_is_proxy:
        notes["TLBL_PROXY"] = True
    if cap_is_proxy:
        notes["CAPEX_PROXY"] = True
    if equity_is_proxy:
        notes["EQUITY_PROXY"] = True
    if ebitda_is_proxy:
        notes["EBITDA_PROXY"] = True

    # ---------------------------
    # Limpieza denominadores / numeradores
    # ---------------------------
    # Nunca imputar cero ni recortar signos automáticamente.

    # ---------------------------
    # Ratios finales
    # ---------------------------
    # Históricos reincorporados (nombres preservados para compatibilidad DiD).
    out[R_OPER_MARGIN] = ((ebitda - depr) / rev.replace(0, np.nan)) * 100
    out[R_CF_MARGIN] = (ebitda / rev.replace(0, np.nan)) * 100
    # Margen bruto operativo del proyecto:
    # (Ingresos operacionales - Gastos operacionales) / Ingresos operacionales.
    out[R_GROSS_MARGIN] = ((rev - opex) / rev.replace(0, np.nan)) * 100

    # Nombre histórico del proyecto; definición histórica = Total Liabilities / EBITDA.
    out[R_DEBT_SERVICE] = tlbl / ebitda.replace(0, np.nan)
    out[R_CAPEX_DEPR] = capex / depr.replace(0, np.nan)

    # Indicadores actuales
    out[R_AF_TLBL] = (af / tlbl.replace(0, np.nan)) * 100
    out[R_CAPEX_FIXED] = capex / fixed_assets.replace(0, np.nan)
    out[R_DEPR_FIXED] = depr / fixed_assets.replace(0, np.nan)
    out[R_ASSET_TURNOVER] = rev / assets.replace(0, np.nan)
    out[R_LIAB_EQUITY] = tlbl / equity.replace(0, np.nan)
    out[R_ASSETS_LIAB] = assets / tlbl.replace(0, np.nan)
    out[R_CAPEX_ENROLLMENT] = capex / total_enrollment.replace(0, np.nan)
    out[R_OPEX_ENROLLMENT] = opex / total_enrollment.replace(0, np.nan)
    out[R_REVENUE_ENROLLMENT] = rev / total_enrollment.replace(0, np.nan)

    for value_col, result_col, flag_col in [
        (
            R_REVENUE_ENROLLMENT,
            R_REVENUE_STUDENT_REAL_DLOG,
            F_REVENUE_STUDENT_UNIT_BREAK,
        ),
        (
            R_OPEX_ENROLLMENT,
            R_OPEX_STUDENT_REAL_DLOG,
            F_OPEX_STUDENT_UNIT_BREAK,
        ),
        (
            R_CAPEX_ENROLLMENT,
            R_CAPEX_STUDENT_REAL_DLOG,
            F_CAPEX_STUDENT_UNIT_BREAK,
        ),
    ]:
        out = add_real_annual_log_change(
            out,
            value_col=value_col,
            result_col=result_col,
            unit_break_col=flag_col,
        )

    return out, notes


# ---------------------------
# Reporte de faltantes
# ---------------------------

def missing_report(wide: pd.DataFrame, uni_col: str) -> pd.DataFrame:
    checks = {
        "Adjusted Operating Margin": [COL_EBITDA, COL_DEPR, COL_REV],
        "Cash Flow Margin": [COL_EBITDA, COL_REV],
        "Current Debt Service": [COL_TOTAL_LIAB, COL_EBITDA],
        "CapEx / Depreciation": [
            COL_CAPEX,
            "Capital Expenditures (Net) [Proxy]",
            COL_DEPR,
        ],
        "Gross Margin": [
            COL_REV,
            COL_OPEX,
        ],
        "AF / Total Liabilities": [
            COL_AF,
            COL_TOTAL_LIAB
        ],
        "CapEx / Fixed Assets": [
            COL_CAPEX,
            "Capital Expenditures (Net) [Proxy]",
            COL_FIXED_ASSETS,
        ],
        "Depreciation / Fixed Assets": [
            COL_DEPR,
            COL_FIXED_ASSETS,
        ],
        "Total Liabilities / Equity": [
            COL_TOTAL_LIAB,
            COL_EQUITY,
            "Net Assets / Equity",
            "Total Equity",
            "Total patrimonio",
            "Patrimonio"
        ],
        "Asset Turnover": [
            COL_REV,
            COL_ASSETS
        ],
        "Total Assets / Total Liabilities": [
            COL_ASSETS,
            COL_TOTAL_LIAB
        ],
        "CapEx / Total Enrollment": [
            COL_CAPEX,
            "Capital Expenditures (Net) [Proxy]",
            TOTAL_ENROLLMENT_COL
        ],
        "Total Operating Expense / Total Enrollment": [
            COL_OPEX,
            TOTAL_ENROLLMENT_COL
        ],
        "Adjusted Total Operating Revenue / Total Enrollment": [
            COL_REV,
            TOTAL_ENROLLMENT_COL
        ],
        "Revenue Per Student Real Growth (%)": [
                ]
    }

    rows = []

    for u in sorted(wide[uni_col].unique()):
        sub = wide[wide[uni_col] == u]
        miss = []

        for label, cols in checks.items():
            have = any(c in sub.columns for c in cols)
            if not have:
                miss.append(label)

        rows.append({
            "universidad": u,
            "faltan": ", ".join(miss) if miss else ""
        })

    rep = pd.DataFrame(rows)
    rep_path = os.path.join(OUT_DIR, "reporte_faltantes.csv")
    rep.to_csv(rep_path, index=False, encoding="utf-8-sig")
    print(f"[Reporte] Guardado faltantes: {rep_path}")

    return rep


def accounting_identity_report(wide: pd.DataFrame, uni_col: str) -> pd.DataFrame:
    """Audita identidades contables sin modificar ni imputar los datos."""
    audit = wide[[uni_col, YEAR_COL]].copy()
    for column in [
        COL_ASSETS,
        COL_CURRENT_ASSETS,
        COL_FIXED_ASSETS,
        COL_ST_L,
        COL_LT_L,
        COL_EQUITY,
    ]:
        audit[column] = (
            as_num(wide[column])
            if column in wide.columns
            else np.nan
        )

    audit[COL_TOTAL_LIAB] = (
        audit[COL_ST_L] + audit[COL_LT_L]
    ).where(audit[[COL_ST_L, COL_LT_L]].notna().all(axis=1))
    audit["AssetsMinusLiabilitiesAndEquity"] = (
        audit[COL_ASSETS] - audit[COL_TOTAL_LIAB] - audit[COL_EQUITY]
    )
    complete_balance = audit[
        [COL_ASSETS, COL_TOTAL_LIAB, COL_EQUITY]
    ].notna().all(axis=1)
    balance_tolerance = np.maximum(
        1.0,
        audit[COL_ASSETS].abs() * 1e-6,
    )
    audit["BalanceIdentityStatus"] = np.select(
        [
            ~complete_balance,
            audit["AssetsMinusLiabilitiesAndEquity"].abs().le(
                balance_tolerance
            ),
        ],
        ["Missing inputs", "Balanced within tolerance"],
        default="Mismatch",
    )

    audit["FixedAssetsMinusAssetsLessCurrentAssets"] = (
        audit[COL_FIXED_ASSETS]
        - (audit[COL_ASSETS] - audit[COL_CURRENT_ASSETS])
    )
    complete_fixed = audit[
        [COL_FIXED_ASSETS, COL_ASSETS, COL_CURRENT_ASSETS]
    ].notna().all(axis=1)
    fixed_tolerance = np.maximum(
        1.0,
        audit[COL_FIXED_ASSETS].abs() * 1e-6,
    )
    audit["FixedAssetsIdentityStatus"] = np.select(
        [
            ~complete_fixed,
            audit["FixedAssetsMinusAssetsLessCurrentAssets"].abs().le(
                fixed_tolerance
            ),
        ],
        ["Missing inputs", "Balanced within tolerance"],
        default="Mismatch",
    )

    path = os.path.join(OUT_DIR, "accounting_identity_audit.csv")
    audit.sort_values([uni_col, YEAR_COL]).to_csv(
        path, index=False, encoding="utf-8-sig"
    )
    balance_counts = audit["BalanceIdentityStatus"].value_counts().to_dict()
    fixed_counts = audit["FixedAssetsIdentityStatus"].value_counts().to_dict()
    print(f"[Audit] Accounting identity: {balance_counts}")
    print(f"[Audit] Fixed Assets identity: {fixed_counts}")
    print(f"[Audit] Saved: {path}")
    return audit


# ---------------------------
# Plots
# ---------------------------

def _sanitize(s: str) -> str:
    return re.sub(r"[^a-zA-Z0-9_-]+", "_", str(s))[:80]


def plot_metric_for_uni(df_ratios, universidad, metric, uni_col, outdir, note_proxy=""):
    sub = df_ratios[df_ratios[uni_col] == universidad].sort_values(YEAR_COL)

    y = sub[metric] if metric in sub.columns else pd.Series(np.nan, index=sub.index)
    x = sub[YEAR_COL].values if YEAR_COL in sub.columns else []

    plt.figure()

    if len(y) == 0 or np.all(pd.isna(y)):
        plt.title(f"{universidad} - {metric}{note_proxy}")
        plt.xlabel("Año")
        plt.ylabel(metric)
        plt.text(
            0.5,
            0.5,
            "Sin datos (faltan insumos)",
            ha="center",
            va="center",
            transform=plt.gca().transAxes,
        )
    else:
        plt.plot(x, y.values, marker="o")
        plt.title(f"{universidad} - {metric}{note_proxy}")
        plt.xlabel("Año")
        plt.ylabel(metric)

    plt.tight_layout()

    fname = os.path.join(
        outdir,
        f"{_sanitize(universidad)}__{_sanitize(metric)}.png"
    )

    plt.savefig(fname, dpi=150)
    plt.close()

    return fname


def plot_all(ratios: pd.DataFrame, uni_col: str, notes: dict, outdir: str, max_unis: int | None = None):
    if plt is None:
        print("[Aviso] matplotlib no está disponible; se omite la exportación de gráficos.")
        return
    plt.rcParams.update({
        "figure.figsize": (8, 4.5),
        "axes.grid": True
    })

    unis = sorted(ratios[uni_col].dropna().unique().tolist())

    if max_unis is not None:
        unis = unis[:max_unis]

    suffix = ""

    if notes.get("TLBL_PROXY"):
        suffix += " [Proxy Debt]"
    if notes.get("CAPEX_PROXY"):
        suffix += " [Proxy CapEx]"
    if notes.get("EQUITY_PROXY"):
        suffix += " [Proxy Equity]"
    if notes.get("EBITDA_PROXY"):
        suffix += " [Proxy EBITDA]"

    count = 0

    for u in unis:
        for m in ALL_RATIOS:
            plot_metric_for_uni(
                ratios,
                u,
                m,
                uni_col,
                outdir,
                note_proxy=suffix
            )
            count += 1

    print(f"[Plots] Generados {count} PNGs en {outdir}")


# ---------------------------
# Main
# ---------------------------

def main(base_dir: str | None = None, max_unis: int | None = None):
    global OUT_DIR, PLOTS_DIR

    if base_dir is not None:
        # Compatibilidad opcional: base_dir sólo cambia dónde se guardan
        # outputs auxiliares. NO cambia panel_cleaned_3.csv, mapeo_indicadores.csv
        # ni ratios_fitch_panel_def.csv.
        aux_base = os.path.abspath(base_dir)
        OUT_DIR = os.path.join(aux_base, "reportes", "fitch_indicators_def")
        PLOTS_DIR = os.path.join(OUT_DIR, "plots")

    ensure_dirs()

    df, covariates = load_and_map(INPUT_PANEL, INPUT_MAP)
    wide, uni_col = to_wide(df, covariates, OUT_DIR)

    ratios, notes = compute_ratios(wide)

    ratios = ratios[
        [
            c
            for c in ratios.columns
            if c in [uni_col, YEAR_COL, *COVARIATE_COLS]
            or c in ALL_RATIOS
            or c in QUALITY_FLAG_COLS
        ]
    ]

    missing_report(wide, uni_col)
    accounting_identity_report(wide, uni_col)

    # Output principal: ruta explícita en PANEL_DIR.
    os.makedirs(os.path.dirname(RATIOS_OUTPUT_PATH), exist_ok=True)
    ratios.to_csv(
        RATIOS_OUTPUT_PATH,
        index=False,
        encoding="utf-8-sig"
    )
    print(f"[Output] Ratios guardados: {RATIOS_OUTPUT_PATH}")

    plot_all(ratios, uni_col, notes, PLOTS_DIR, max_unis=max_unis)

    print("[OK] Pipeline DEF completado.")


if __name__ == "__main__":
    main()
