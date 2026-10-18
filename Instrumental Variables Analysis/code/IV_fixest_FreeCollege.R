# ============================================================
# FC5: fixed-effects IV for the financial effects of Chilean tuition-free policy
# Baseline estimand: static IV-FE LATE for the 2016 cohort versus never-treated
# universities. The source IV_fitch_fixest_mc.R is intentionally left unchanged.
# ============================================================

options(stringsAsFactors = FALSE, scipen = 999)

# R puede iniciarse con locale "C" desde terminales no interactivas en Windows.
# Forzar UTF-8 evita que se dañen los nombres de columnas con tildes.
if (.Platform$OS.type == "windows") {
  invisible(suppressWarnings(
    Sys.setlocale("LC_ALL", "English_United States.utf8")
  ))
}

# ------------------------------------------------------------
# 1. Paquetes
# ------------------------------------------------------------
required_packages <- c("fixest", "ggplot2", "openxlsx", "zip")
user_library <- Sys.getenv("R_LIBS_USER", unset = "")
if (!nzchar(user_library)) {
  user_library <- file.path(
    Sys.getenv("LOCALAPPDATA"), "R", "win-library",
    paste(R.version$major, strsplit(R.version$minor, "\\.")[[1]][1], sep = ".")
  )
}
dir.create(user_library, recursive = TRUE, showWarnings = FALSE)
.libPaths(unique(c(user_library, .libPaths())))
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  install.packages(
    missing_packages,
    repos = "https://cloud.r-project.org",
    lib = user_library
  )
}

library(fixest)
library(ggplot2)
library(tidyselect)

# openxlsx can retain worksheet drawing relationships without the corresponding
# drawing parts.  Excel normally repairs these silently, but other readers may
# treat the workbook as damaged.  Remove only those orphan relationships after
# saving; table contents, styles, sheet names and dimensions are unchanged.
sanitize_openxlsx_package <- function(path) {
  work <- tempfile("xlsx_sanitize_")
  dir.create(work)
  on.exit(unlink(work, recursive = TRUE, force = TRUE), add = TRUE)
  zip::unzip(path, exdir = work)

  content_types <- file.path(work, "[Content_Types].xml")
  xml <- paste(readLines(content_types, warn = FALSE, encoding = "UTF-8"), collapse = "")
  xml <- gsub(
    "<Override PartName=\"/xl/drawings/drawing[0-9]+\\.xml\"[^>]*/>",
    "", xml, perl = TRUE
  )
  writeLines(xml, content_types, useBytes = TRUE)

  rel_files <- list.files(
    file.path(work, "xl", "worksheets", "_rels"),
    pattern = "\\.rels$", full.names = TRUE
  )
  for (rel_file in rel_files) {
    rel <- paste(readLines(rel_file, warn = FALSE, encoding = "UTF-8"), collapse = "")
    rel <- gsub(
      "<Relationship[^>]+Type=\"http://schemas\\.openxmlformats\\.org/officeDocument/2006/relationships/(drawing|vmlDrawing)\"[^>]*/>",
      "", rel, perl = TRUE
    )
    writeLines(rel, rel_file, useBytes = TRUE)
  }

  repaired <- tempfile(fileext = ".xlsx")
  on.exit(unlink(repaired, force = TRUE), add = TRUE)
  entries <- list.files(work, all.files = TRUE, no.. = TRUE)
  zip::zipr(repaired, entries, root = work, include_directories = FALSE)
  if (!file.copy(repaired, path, overwrite = TRUE)) {
    stop("Could not finalize sanitized workbook: ", path)
  }
  invisible(path)
}

# ------------------------------------------------------------
# 2. Rutas
# ------------------------------------------------------------
command_args <- commandArgs(trailingOnly = FALSE)
script_arg <- grep("^--file=", command_args, value = TRUE)
script_path <- if (length(script_arg) > 0) {
  normalizePath(sub("^--file=", "", script_arg[1]), winslash = "/")
} else {
  normalizePath(".", winslash = "/")
}
project_dir <- if (file.info(script_path)$isdir) {
  normalizePath(file.path(script_path, ".."), winslash = "/")
} else {
  normalizePath(file.path(dirname(script_path), ".."), winslash = "/")
}
project_override <- Sys.getenv("IV_FC5_PROJECT_DIR", unset = "")
if (nzchar(project_override)) project_dir <- project_override
free_college_dir <- normalizePath(file.path(project_dir, ".."), winslash = "/")
panel_dir <- file.path(free_college_dir, "Construcción de panel data")
input_file <- file.path(
  panel_dir, "ratios_fitch_panel.csv"
)
mapping_file <- file.path(panel_dir, "university_name_mapping_2.csv")
cohort_file <- file.path(panel_dir, "clasif_quintiles_ingresos_normalizado.csv")
tuition_file <- file.path(panel_dir, "tuition_instrument_2006.csv")
input_override <- Sys.getenv("IV_FC5_INPUT_FILE", unset = "")
mapping_override <- Sys.getenv("IV_FC5_MAPPING_FILE", unset = "")
cohort_override <- Sys.getenv("IV_FC5_COHORT_FILE", unset = "")
tuition_override <- Sys.getenv("IV_FC5_TUITION_FILE", unset = "")
if (nzchar(input_override)) input_file <- input_override
if (nzchar(mapping_override)) mapping_file <- mapping_override
if (nzchar(cohort_override)) cohort_file <- cohort_override
if (nzchar(tuition_override)) tuition_file <- tuition_override

output_dir <- file.path(project_dir, "Results")
output_dir_override <- Sys.getenv("IV_FC5_OUTPUT_DIR", unset = "")
if (nzchar(output_dir_override)) {
  output_dir <- output_dir_override
}

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
models_dir <- file.path(project_dir, "modelos_txt")
plots_dir <- file.path(project_dir, "Plots_IV")
plots_log_dir <- plots_dir
desc_dir <- output_dir
dir.create(models_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(plots_dir, recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------
# 3. Configuración
# ------------------------------------------------------------
treatment_year <- 2016L
baseline_accreditation_year <- 2015L
confidence_level <- 0.95
z_critical <- qnorm(1 - (1 - confidence_level) / 2)

EXCLUDED_MAIN_SAMPLE <- c(
  "Universidad de Aysén",
  "Universidad de O'Higgins"
)

EXCLUDED_CLOSED_UNIVERSITIES <- c(
  "Uniacc",
  "Universidad del Pacifico",
  "Universidad de Las Ciencias de la Informática (UCINF)",
  "Universidad ARCIS",
  "Universidad los Leones"
)
pucv_name <- "Pontificia Universidad Católica de Valparaíso"
pucv_accreditation_by_year <- setNames(
  c(rep(6, 11), 7), as.character(2011:2022)
)

indicators_fitch <- c(
  "Available Funds / Total Liabilities (%)",
  "Capital Expenditures / Fixed Assets",
  "Asset Turnover",
  "Total Liabilities / Equity",
  "Total Assets / Total Liabilities",
  "Capital Expenditures / Total Enrollment",
  "Total Operating Expense / Total Enrollment",
  "Adjusted Total Operating Revenue / Total Enrollment",
  "Revenue Per Student Real Growth (%)",
  "Real Operating Revenue per Student - Annual Log Change",
  "Real Operating Expense per Student - Annual Log Change",
  "Real Capital Expenditures per Student - Annual Log Change"
)

variation_indicators <- c(
  "Real Operating Revenue per Student - Annual Log Change",
  "Real Operating Expense per Student - Annual Log Change",
  "Real Capital Expenditures per Student - Annual Log Change"
)

# ------------------------------------------------------------
# 4. Funciones auxiliares
# ------------------------------------------------------------
normalize_name <- function(x) {
  x <- enc2utf8(as.character(x))
  x <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT")
  x <- tolower(trimws(x))
  x <- gsub("[^a-z0-9]+", " ", x)
  trimws(gsub("\\s+", " ", x))
}

# Cohort-file aliases absent from university_name_mapping_2.csv. These are
# explicit institutional renamings, not fuzzy matches.
cohort_name_overrides <- c(
  "pontificia universidad catolica de chile" =
    "Pontificia Universidad Católica",
  "universidad de playa ancha de ciencias de la educacion" =
    "Universidad de Playa Ancha",
  "universidad central de chile" =
    "Universidad Central"
)

safe_file_name <- function(x) {
  x <- normalize_name(x)
  gsub(" ", "_", x, fixed = TRUE)
}

build_unit_audit <- function(data, outcomes, path) {
  rows <- list()
  for (outcome in outcomes) {
    if (!outcome %in% names(data)) next
    d <- data[c("Universidad_std", "id", "year", outcome)]
    names(d)[4] <- "value"
    d$value <- suppressWarnings(as.numeric(d$value))
    d <- d[order(d$id, d$year), ]
    d$lag_year <- ave(d$year, d$id, FUN = function(x) c(NA, head(x, -1)))
    d$lag_value <- ave(d$value, d$id, FUN = function(x) c(NA, head(x, -1)))
    d$consecutive_year <- !is.na(d$lag_year) & d$year == d$lag_year + 1
    d$year_on_year_ratio <- ifelse(
      d$consecutive_year & is.finite(d$value) & is.finite(d$lag_value) &
        d$lag_value != 0,
      d$value / d$lag_value,
      NA_real_
    )
    ratio_abs <- abs(d$year_on_year_ratio)
    d$scale_pattern <- ifelse(
      ratio_abs >= 5e5, "approximately x1,000,000 or larger",
      ifelse(
        ratio_abs >= 5e2, "approximately x1,000 or larger",
        ifelse(
          ratio_abs >= 5e1, "approximately x100 or larger",
          ifelse(
            ratio_abs > 0 & ratio_abs <= 2e-6, "approximately /1,000,000",
            ifelse(
              ratio_abs > 0 & ratio_abs <= 2e-3, "approximately /1,000",
              ifelse(
                ratio_abs > 0 & ratio_abs <= 2e-2,
                "approximately /100", ""
              )
            )
          )
        )
      )
    )
    d$audit_classification <- ifelse(
      !is.finite(d$value), "Missing value",
      ifelse(
        !d$consecutive_year, "No exact consecutive-year comparison",
        ifelse(
          !is.finite(d$lag_value), "Missing lag value",
          ifelse(
            d$value <= 0 | d$lag_value <= 0,
            "Possible sign or zero-value error / manual review",
            ifelse(
              ratio_abs >= 100 | (ratio_abs > 0 & ratio_abs <= 0.01),
              "Possible scale error / manual review",
              ifelse(
                ratio_abs >= 10 | (ratio_abs > 0 & ratio_abs <= 0.10),
                "Extreme economic change / manual review",
                "Plausible range"
              )
            )
          )
        )
      )
    )
    d$indicator <- outcome
    rows[[length(rows) + 1]] <- d[c(
      "Universidad_std", "id", "year", "indicator", "value",
      "lag_year", "lag_value", "consecutive_year", "year_on_year_ratio",
      "scale_pattern", "audit_classification"
    )]
  }
  audit <- do.call(rbind, rows)
  write.csv(audit, path, row.names = FALSE, fileEncoding = "UTF-8")
  audit
}

read_utf8_csv <- function(path) {
  if (!file.exists(path)) stop("No se encuentra el archivo: ", path)
  read.csv(
    path,
    fileEncoding = "UTF-8-BOM",
    check.names = FALSE,
    na.strings = c("", "NA", "NaN", "Inf", "-Inf")
  )
}

load_name_mapping <- function(path) {
  mapping <- read_utf8_csv(path)
  if (!"Final_Standard" %in% names(mapping)) {
    stop("El mapeo de universidades no contiene Final_Standard.")
  }

  aliases <- intersect(
    c(
      "Universidad_key", "Universidad_raw", "CNED 2011-2022",
      "Suggested_Standard", "Final_Standard"
    ),
    names(mapping)
  )
  out <- character(0)
  for (i in seq_len(nrow(mapping))) {
    standard <- mapping$Final_Standard[i]
    if (is.na(standard) || !nzchar(trimws(standard))) next
    standard <- trimws(as.character(standard))
    for (column in aliases) {
      value <- mapping[[column]][i]
      if (!is.na(value) && nzchar(trimws(as.character(value)))) {
        out[normalize_name(value)] <- standard
      }
    }
  }
  out
}

load_gratuidad_cohorts <- function(path, name_mapping) {
  if (!file.exists(path)) stop("No se encuentra la clasificación: ", path)
  cohorts <- read.csv(
    path,
    fileEncoding = "UTF-8-BOM",
    check.names = FALSE,
    na.strings = c("", "NA", "NaN")
  )
  normalized_columns <- setNames(names(cohorts), normalize_name(names(cohorts)))
  university_col <- normalized_columns[["universidad"]]
  if (is.null(university_col)) university_col <- normalized_columns[["institucion"]]
  category_col <- normalized_columns[["categoria"]]
  year_col <- normalized_columns[["ano"]]
  if (is.null(university_col) || is.null(category_col) || is.null(year_col)) {
    stop("La clasificación debe contener Universidad/Institución, Categoria y Año.")
  }

  cohort_rows <- cohorts[
    suppressWarnings(as.numeric(cohorts[[category_col]])) == 1,
    c(university_col, year_col), drop = FALSE
  ]
  cohort_rows[[year_col]] <- suppressWarnings(as.integer(cohort_rows[[year_col]]))
  cohort_rows <- cohort_rows[
    !is.na(cohort_rows[[university_col]]) & !is.na(cohort_rows[[year_col]]),
    , drop = FALSE
  ]

  out <- integer(0)
  for (i in seq_len(nrow(cohort_rows))) {
    source_name <- cohort_rows[[university_col]][i]
    standard <- unname(name_mapping[normalize_name(source_name)])
    source_key <- normalize_name(source_name)
    if (
      (length(standard) == 0 || is.na(standard)) &&
      source_key %in% names(cohort_name_overrides)
    ) {
      standard <- unname(cohort_name_overrides[source_key])
    }
    if (length(standard) == 0 || is.na(standard)) standard <- as.character(source_name)
    key <- normalize_name(standard)
    year <- as.integer(cohort_rows[[year_col]][i])
    if (key %in% names(out) && out[[key]] != year) {
      stop("Cohortes incompatibles para ", standard, ".")
    }
    out[[key]] <- year
  }
  if (length(out) == 0) stop("No se encontraron instituciones tratadas.")
  out
}

extract_term <- function(model, term, model_name, outcome, specification) {
  ct <- as.data.frame(coeftable(model))
  ct$term <- rownames(ct)
  rownames(ct) <- NULL

  hit <- ct[ct$term == term, , drop = FALSE]
  if (nrow(hit) != 1) return(NULL)

  estimate <- hit[[1]][1]
  std_error <- hit[[2]][1]
  statistic <- hit[[3]][1]
  p_value <- hit[[4]][1]

  data.frame(
    indicator = outcome,
    specification = specification,
    model = model_name,
    term = term,
    estimate = estimate,
    std_error = std_error,
    statistic = statistic,
    p_value = p_value,
    conf_low = estimate - z_critical * std_error,
    conf_high = estimate + z_critical * std_error,
    n_obs = nobs(model),
    stringsAsFactors = FALSE
  )
}

fitstat_value <- function(model, type) {
  ans <- tryCatch(
    fitstat(model, type, simplify = TRUE),
    error = function(e) NA_real_
  )
  if (length(ans) == 0 || all(is.na(ans))) return(NA_real_)
  suppressWarnings(as.numeric(ans[1]))
}

capture_model <- function(model, path, title) {
  lines <- c(
    title,
    paste(rep("=", nchar(title)), collapse = ""),
    "",
    capture.output(print(summary(model, stage = 1:2))),
    "",
    "Diagnósticos IV:",
    capture.output(print(fitstat(model, ~ ivwald1 + ivf1 + wh + sargan)))
  )
  writeLines(lines, con = path, useBytes = TRUE)
}

# ------------------------------------------------------------
# 5. Leer y preparar el panel
# ------------------------------------------------------------
name_mapping <- load_name_mapping(mapping_file)
gratuidad_cohorts <- load_gratuidad_cohorts(cohort_file, name_mapping)
panel <- read_utf8_csv(input_file)
tuition <- read_utf8_csv(tuition_file)

required_tuition_columns <- c(
  "Universidad_std", "TuitionReferenceGapPct_2006",
  "TuitionReferenceCoverage_2006", "TuitionReferenceCoverage2006_z"
)
missing_tuition_columns <- setdiff(required_tuition_columns, names(tuition))
if (length(missing_tuition_columns) > 0) {
  stop(
    "The tuition instrument file is missing columns: ",
    paste(missing_tuition_columns, collapse = ", ")
  )
}
tuition$id <- normalize_name(tuition$Universidad_std)
if (anyDuplicated(tuition$id)) {
  stop("The tuition instrument contains duplicate standardized universities.")
}
tuition$TuitionReferenceGapPct_2006 <- suppressWarnings(
  as.numeric(tuition$TuitionReferenceGapPct_2006)
)
tuition$TuitionReferenceCoverage_2006 <- suppressWarnings(
  as.numeric(tuition$TuitionReferenceCoverage_2006)
)
tuition$TuitionReferenceCoverage2006_z <- suppressWarnings(
  as.numeric(tuition$TuitionReferenceCoverage2006_z)
)

required_columns <- c(
  "Universidad_std", "Año", "Acreditación (años)", "Matrícula pregrado",
  "% elegibles gratuidad (proxy escolar)",
  "Cobertura elegibilidad proxy (%)"
)
missing_columns <- setdiff(required_columns, names(panel))
if (length(missing_columns) > 0) {
  stop("Faltan columnas requeridas: ", paste(missing_columns, collapse = ", "))
}

missing_indicators <- setdiff(indicators_fitch, names(panel))
if (length(missing_indicators) > 0) {
  warning("No se encontraron: ", paste(missing_indicators, collapse = ", "))
}
indicators_fitch <- intersect(indicators_fitch, names(panel))

panel$id <- normalize_name(panel$Universidad_std)
panel <- merge(
  panel,
  tuition[c(
    "id", "TuitionReferenceGapPct_2006",
    "TuitionReferenceCoverage_2006", "TuitionReferenceCoverage2006_z"
  )],
  by = "id", all.x = TRUE, sort = FALSE
)
panel$year <- as.integer(panel[["Año"]])
panel$accreditation_years <- suppressWarnings(
  as.numeric(panel$`Acreditación (años)`)
)
pucv_id <- normalize_name(pucv_name)
pucv_mask <- panel$id == pucv_id & panel$year %in% 2011:2022
if (!any(pucv_mask)) {
  stop("PUCV no está presente en el panel de indicadores Fitch.")
}
pucv_expected <- unname(
  pucv_accreditation_by_year[as.character(panel$year[pucv_mask])]
)
pucv_existing <- panel$accreditation_years[pucv_mask]
pucv_conflict <- !is.na(pucv_existing) & pucv_existing != pucv_expected
if (any(pucv_conflict)) {
  stop("La acreditación PUCV contradice la cronología validada 2011-2022.")
}
panel$accreditation_years[pucv_mask] <- ifelse(
  is.na(pucv_existing), pucv_expected, pucv_existing
)
panel$`Acreditación (años)`[pucv_mask] <- panel$accreditation_years[pucv_mask]
pucv_years_observed <- sort(unique(panel$year[pucv_mask]))
pucv_years_missing <- setdiff(2011:2022, pucv_years_observed)
if (length(pucv_years_missing) > 0 || any(is.na(panel$accreditation_years[pucv_mask]))) {
  stop(
    "Acreditación PUCV incompleta. Años ausentes: ",
    paste(pucv_years_missing, collapse = ", ")
  )
}
message("[CHECK] PUCV acreditación: 6 años (2011-2021), 7 años (2022).")
panel$enrollment_undergrad <- suppressWarnings(
  as.numeric(panel$`Matrícula pregrado`)
)
panel$log_enrollment_undergrad <- ifelse(
  is.finite(panel$enrollment_undergrad) & panel$enrollment_undergrad > 0,
  log(panel$enrollment_undergrad),
  NA_real_
)
panel$eligibility_proxy_current_10 <- suppressWarnings(
  as.numeric(panel$`% elegibles gratuidad (proxy escolar)`) / 10
)
panel$eligibility_coverage_current_10 <- suppressWarnings(
  as.numeric(panel$`Cobertura elegibilidad proxy (%)`) / 10
)

cohort_lookup <- setNames(
  as.integer(unname(gratuidad_cohorts)),
  normalize_name(names(gratuidad_cohorts))
)

panel$cohort <- unname(cohort_lookup[panel$id])
panel$post_2016 <- as.integer(panel$year >= treatment_year)
panel_full_for_unit_audit <- panel

# Identification sample: the 2016 adoption cohort versus never-treated
# universities. Later adopters are excluded to match the single-cohort dynamic
# DiD design rather than being coded as controls before their own adoption.
sample_scope_audit <- unique(panel[c("Universidad_std", "id", "cohort")])
eligible_cohort <- is.na(panel$cohort) | panel$cohort == treatment_year
excluded_main_norm <- normalize_name(EXCLUDED_MAIN_SAMPLE)
sample_scope_audit$sample_status <- ifelse(
  !(is.na(sample_scope_audit$cohort) | sample_scope_audit$cohort == treatment_year),
  "Excluded: later adoption cohort",
  ifelse(
    sample_scope_audit$id %in% excluded_main_norm,
    "Excluded: main financial sample",
    "Included: 2016 cohort or never treated"
  )
)
panel <- panel[eligible_cohort & !panel$id %in% excluded_main_norm, ]

# Treatment is absorbing within the retained 2016 cohort.
panel$gratuidad <- as.integer(
  !is.na(panel$cohort) & panel$cohort == treatment_year &
    panel$year >= treatment_year
)
panel <- panel[order(panel$id, panel$year), ]

# La adscripción observada se trata como absorbente. Una transición 1 -> 0
# indica una clasificación inconsistente y detiene la ejecución.
exit_check <- aggregate(gratuidad ~ id, panel, function(x) any(diff(x) < 0))
if (any(exit_check$gratuidad)) {
  stop(
    "Se detectaron universidades que pasan de gratuidad=1 a gratuidad=0: ",
    paste(exit_check$id[exit_check$gratuidad], collapse = ", ")
  )
}

# Indicadores contemporáneos. Se usan como controles o como una robustez
# explícitamente secundaria; no son los instrumentos principales porque pueden
# responder a la propia política.
panel$cumple_umbral_acreditacion <- ifelse(
  is.na(panel$accreditation_years),
  NA_integer_,
  as.integer(panel$accreditation_years >= 4)
)

panel$instrumento_acreditacion_actual_post <- with(
  panel,
  ifelse(
    is.na(cumple_umbral_acreditacion),
    NA_integer_,
    as.integer(cumple_umbral_acreditacion * post_2016)
  )
)
panel$instrumento_elegibilidad_actual_post <- with(
  panel,
  ifelse(
    is.na(eligibility_proxy_current_10),
    NA_real_,
    eligibility_proxy_current_10 * post_2016
  )
)

# Instrumentos principales: características de 2015, fijadas antes de la
# gratuidad, interactuadas con post-2016. La elegibilidad está expresada por
# cada 10 puntos porcentuales para que la primera etapa sea interpretable.
baseline_2015 <- panel[
  panel$year == baseline_accreditation_year,
  c(
    "id", "accreditation_years", "cumple_umbral_acreditacion",
    "eligibility_proxy_current_10", "eligibility_coverage_current_10"
  )
]
baseline_2015 <- baseline_2015[!duplicated(baseline_2015$id), ]
names(baseline_2015) <- c(
  "id", "acreditacion_anios_2015", "cumple_umbral_2015",
  "elegibilidad_proxy_2015_10", "cobertura_proxy_2015_10"
)

panel <- merge(panel, baseline_2015, by = "id", all.x = TRUE, sort = FALSE)
panel$instrumento_elegibilidad_2015_post <- with(
  panel,
  ifelse(
    is.na(elegibilidad_proxy_2015_10),
    NA_real_,
    elegibilidad_proxy_2015_10 * post_2016
  )
)
panel$instrumento_acreditacion4_2015_post <- with(
  panel,
  ifelse(
    is.na(cumple_umbral_2015),
    NA_real_,
    cumple_umbral_2015 * post_2016
  )
)
panel$instrumento_acreditacion_anios_2015_post <- with(
  panel,
  ifelse(
    is.na(acreditacion_anios_2015),
    NA_real_,
    acreditacion_anios_2015 * post_2016
  )
)

# New predetermined instrument: standardized institution-level coverage of the
# reference tuition fee in 2006, interacted with Post-2016. Higher coverage
# means a smaller pre-policy fee gap and therefore a lower participation cost.
panel$instrument_tuition_coverage_2006_post <- with(
  panel,
  ifelse(
    is.na(TuitionReferenceCoverage2006_z),
    NA_real_,
    TuitionReferenceCoverage2006_z * post_2016
  )
)

# Robustez predeterminada: promedios exclusivamente pretratamiento. Para la
# elegibilidad se usa 2014-2015 y para acreditación todo 2011-2015.
pre_elig <- panel[panel$year %in% 2014:2015, c(
  "id", "eligibility_proxy_current_10"
)]
pre_elig_mean <- aggregate(
  eligibility_proxy_current_10 ~ id,
  data = pre_elig,
  FUN = function(x) if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
)
names(pre_elig_mean)[2] <- "elegibilidad_media_2014_2015_10"

pre_accred <- panel[panel$year < treatment_year, c("id", "accreditation_years")]
pre_accred_mean <- aggregate(
  accreditation_years ~ id,
  data = pre_accred,
  FUN = function(x) if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
)
names(pre_accred_mean)[2] <- "acreditacion_media_pre"

panel <- merge(panel, pre_elig_mean, by = "id", all.x = TRUE, sort = FALSE)
panel <- merge(panel, pre_accred_mean, by = "id", all.x = TRUE, sort = FALSE)
panel$instrumento_elegibilidad_media_pre_post <- with(
  panel, elegibilidad_media_2014_2015_10 * post_2016
)
panel$instrumento_acreditacion_media_pre_post <- with(
  panel, acreditacion_media_pre * post_2016
)

panel <- panel[order(panel$id, panel$year), ]

# Common outcome sample for the FC5 baseline accreditation instrument.
panel$common_sample <- complete.cases(
  panel[c(
    indicators_fitch, "instrumento_acreditacion4_2015_post"
  )]
)

# Keep both analysis samples. The full comparison retains the five problematic
# controls; the main IV pipeline and diagnostics use the restricted sample.
panel_with_closure_controls <- panel
excluded_closed_norm <- normalize_name(EXCLUDED_CLOSED_UNIVERSITIES)
panel <- panel[!panel$id %in% excluded_closed_norm, , drop = FALSE]

# ------------------------------------------------------------
# 6. Auditoría de dummies y muestra
# ------------------------------------------------------------
cohort_audit <- unique(panel[c(
  "Universidad_std", "id", "cohort", "cumple_umbral_2015",
  "acreditacion_anios_2015", "elegibilidad_proxy_2015_10",
  "cobertura_proxy_2015_10", "elegibilidad_media_2014_2015_10",
  "acreditacion_media_pre", "TuitionReferenceGapPct_2006",
  "TuitionReferenceCoverage_2006", "TuitionReferenceCoverage2006_z"
)])
cohort_audit <- cohort_audit[order(cohort_audit$Universidad_std), ]

write.csv(
  cohort_audit,
  file.path(output_dir, "auditoria_universidades_cohortes.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

write.csv(
  sample_scope_audit,
  file.path(output_dir, "audit_identification_sample.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

tuition_sample_audit <- unique(panel[c(
  "Universidad_std", "id", "cohort", "TuitionReferenceGapPct_2006",
  "TuitionReferenceCoverage_2006", "TuitionReferenceCoverage2006_z"
)])
tuition_sample_audit$treated_2016 <- as.integer(
  !is.na(tuition_sample_audit$cohort) &
    tuition_sample_audit$cohort == treatment_year
)
tuition_sample_audit$instrument_available <- as.integer(
  is.finite(tuition_sample_audit$TuitionReferenceCoverage2006_z)
)
write.csv(
  tuition_sample_audit,
  file.path(output_dir, "audit_tuition_instrument_sample.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

instrument_dictionary <- data.frame(
  specification_role = c(
    "FC5 baseline", "Weak-instrument robustness", "External-data robustness",
    "Overidentified robustness"
  ),
  instrument_name = c(
    "2015 accreditation threshold x Post-2016",
    "2015 eligibility x Post-2016",
    "Standardized 2006 tuition reference coverage x Post-2016",
    "Accreditation threshold + tuition reference coverage"
  ),
  construction = c(
    "1(accreditation years in 2015 >= 4) x 1(year >= 2016)",
    "(school-proxy eligibility percentage in 2015 / 10) x 1(year >= 2016)",
    "z(reference tuition / actual tuition in 2006) x 1(year >= 2016)",
    "Both the accreditation-threshold and tuition-coverage instruments"
  ),
  expected_first_stage_sign = c("Positive", "Positive", "Positive", "Joint"),
  principal_identification_risk = c(
    "Accreditation may proxy for institutional quality, reputation or capacity",
    "Weak first stage and possible student-composition channels",
    "Pre-existing fee structure may proxy for resources, quality or selectivity",
    "Both exclusion restrictions must hold; joint weak-IV statistic may fall"
  ),
  stringsAsFactors = FALSE
)
write.csv(
  instrument_dictionary,
  file.path(output_dir, "instrument_dictionary_fc5.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

dummy_audit <- panel[c(
  "Universidad_std", "year", "accreditation_years", "cohort",
  "post_2016", "gratuidad", "cumple_umbral_acreditacion",
  "eligibility_proxy_current_10", "eligibility_coverage_current_10",
  "instrumento_elegibilidad_actual_post",
  "instrumento_acreditacion_actual_post", "cumple_umbral_2015",
  "instrumento_elegibilidad_2015_post",
  "instrumento_acreditacion4_2015_post",
  "instrumento_acreditacion_anios_2015_post",
  "TuitionReferenceGapPct_2006", "TuitionReferenceCoverage_2006",
  "TuitionReferenceCoverage2006_z",
  "instrument_tuition_coverage_2006_post",
  "instrumento_elegibilidad_media_pre_post",
  "instrumento_acreditacion_media_pre_post", "enrollment_undergrad",
  "log_enrollment_undergrad", "common_sample"
)]

write.csv(
  dummy_audit,
  file.path(output_dir, "auditoria_dummies_panel.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

unit_audit <- build_unit_audit(
  panel_full_for_unit_audit,
  c(
    "Adjusted Total Operating Revenue / Total Enrollment",
    "Total Operating Expense / Total Enrollment",
    "Capital Expenditures / Total Enrollment"
  ),
  file.path(output_dir, "audit_financial_units_fc5.csv")
)
unit_audit_summary <- as.data.frame(table(
  unit_audit$indicator,
  unit_audit$audit_classification,
  useNA = "ifany"
))
names(unit_audit_summary) <- c("indicator", "audit_classification", "n")
write.csv(
  unit_audit_summary,
  file.path(output_dir, "audit_financial_units_summary_fc5.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# ------------------------------------------------------------
# 7. Estimaciones por indicador
# ------------------------------------------------------------
results <- list()
results_variation <- list()
first_stage_results <- list()
diagnostics <- list()
reduced_form_results <- list()
combined_iv_results <- list()
contemporary_control_results <- list()
robustness_results <- list()
placebo_results <- list()
models_main <- list()
models_robust <- list()

iv_diagnostic_row <- function(model, outcome, specification, data_used) {
  if (!inherits(model, "fixest")) {
    return(data.frame(
      indicator = outcome, specification = specification,
      n_obs = NA, n_universities = NA, weak_iv_wald = NA,
      weak_iv_wald_p = NA, first_stage_f = NA, wu_hausman = NA,
      wu_hausman_p = NA, sargan_stat = NA, sargan_p = NA,
      status = conditionMessage(model), stringsAsFactors = FALSE
    ))
  }
  used <- suppressWarnings(as.integer(obs(model)))
  data.frame(
    indicator = outcome,
    specification = specification,
    n_obs = nobs(model),
    n_universities = length(unique(data_used$id[used])),
    weak_iv_wald = fitstat_value(model, "ivwald1.stat"),
    weak_iv_wald_p = fitstat_value(model, "ivwald1.p"),
    first_stage_f = fitstat_value(model, "ivf1.stat"),
    wu_hausman = fitstat_value(model, "wh.stat"),
    wu_hausman_p = fitstat_value(model, "wh.p"),
    sargan_stat = fitstat_value(model, "sargan.stat"),
    sargan_p = fitstat_value(model, "sargan.p"),
    status = "ok",
    stringsAsFactors = FALSE
  )
}

# Re-estimate only the four main specifications for the comparison column that
# retains the five closure/accreditation-risk controls. Robustness diagnostics
# continue to use the restricted main sample below.
estimate_main_results_for_sample <- function(panel_variant) {
  rows <- list()
  for (outcome in indicators_fitch) {
    d <- panel_variant[c(
      "id", "year", "gratuidad", "log_enrollment_undergrad",
      "accreditation_years", "cumple_umbral_acreditacion",
      "eligibility_proxy_current_10", "eligibility_coverage_current_10",
      "instrumento_acreditacion4_2015_post", outcome
    )]
    names(d)[names(d) == outcome] <- "y"
    d$y <- suppressWarnings(as.numeric(d$y))
    d$y[!is.finite(d$y)] <- NA_real_

    models <- list(
      "OLS-FE total effect: no post-treatment controls" = tryCatch(
        feols(y ~ gratuidad | id + year, data = d, vcov = ~ id,
              notes = FALSE, warn = FALSE), error = function(e) e
      ),
      "IV-FE baseline total effect: 2015 accreditation threshold" = tryCatch(
        feols(y ~ 1 | id + year |
                gratuidad ~ instrumento_acreditacion4_2015_post,
              data = d, vcov = ~ id, notes = FALSE, warn = FALSE),
        error = function(e) e
      ),
      "IV-FE conditional effect: contemporaneous accreditation years" = tryCatch(
        feols(y ~ log_enrollment_undergrad + eligibility_proxy_current_10 +
                eligibility_coverage_current_10 + accreditation_years |
                id + year | gratuidad ~ instrumento_acreditacion4_2015_post,
              data = d, vcov = ~ id, notes = FALSE, warn = FALSE),
        error = function(e) e
      ),
      "IV-FE conditional effect: contemporaneous accreditation threshold" = tryCatch(
        feols(y ~ log_enrollment_undergrad + eligibility_proxy_current_10 +
                eligibility_coverage_current_10 + cumple_umbral_acreditacion |
                id + year | gratuidad ~ instrumento_acreditacion4_2015_post,
              data = d, vcov = ~ id, notes = FALSE, warn = FALSE),
        error = function(e) e
      )
    )

    for (specification in names(models)) {
      model <- models[[specification]]
      if (!inherits(model, "fixest")) next
      term <- if (startsWith(specification, "OLS-FE")) "gratuidad" else "fit_gratuidad"
      row <- extract_term(
        model, term,
        if (term == "gratuidad") "OLS-FE" else "IV-FE",
        outcome, specification
      )
      if (!is.null(row)) rows[[length(rows) + 1L]] <- row
    }
  }
  if (!length(rows)) return(data.frame())
  do.call(rbind, rows)
}

for (outcome in indicators_fitch) {
  message("Estimando: ", outcome)

  d <- panel[c(
    "id", "year", "gratuidad", "post_2016",
    "log_enrollment_undergrad", "accreditation_years",
    "cumple_umbral_acreditacion", "eligibility_proxy_current_10",
    "eligibility_coverage_current_10", "elegibilidad_proxy_2015_10",
    "cumple_umbral_2015", "instrumento_elegibilidad_2015_post",
    "instrumento_acreditacion4_2015_post",
    "instrumento_acreditacion_anios_2015_post",
    "instrument_tuition_coverage_2006_post",
    "instrumento_elegibilidad_media_pre_post",
    "instrumento_acreditacion_media_pre_post",
    "instrumento_elegibilidad_actual_post",
    "instrumento_acreditacion_actual_post", outcome
  )]
  names(d)[names(d) == outcome] <- "y"
  d$y <- suppressWarnings(as.numeric(d$y))
  d$y[!is.finite(d$y)] <- NA_real_

  # Total-effect reference: no contemporaneous enrollment, eligibility or
  # accreditation controls are included because they may be policy mediators.
  m_ols <- feols(
    y ~ gratuidad | id + year,
    data = d,
    vcov = ~ id,
    notes = FALSE,
    warn = FALSE
  )
  results[[length(results) + 1]] <- extract_term(
    m_ols, "gratuidad", "OLS-FE", outcome,
    "OLS-FE total effect: no post-treatment controls"
  )

  # FC5 baseline: the stronger predetermined accreditation-threshold
  # instrument. This is an exactly identified static IV-FE LATE.
  m_iv_main <- tryCatch(
    feols(
      y ~ 1 | id + year |
        gratuidad ~ instrumento_acreditacion4_2015_post,
      data = d,
      vcov = ~ id,
      notes = FALSE,
      warn = FALSE
    ),
    error = function(e) e
  )

  # Conditional/direct-effect variants are secondary because the controls may
  # be post-treatment mediators.
  m_iv_cond_cont <- tryCatch(
    feols(
      y ~ log_enrollment_undergrad + eligibility_proxy_current_10 +
        eligibility_coverage_current_10 + accreditation_years | id + year |
        gratuidad ~ instrumento_acreditacion4_2015_post,
      data = d,
      vcov = ~ id,
      notes = FALSE,
      warn = FALSE
    ),
    error = function(e) e
  )
  m_iv_cond_dummy <- tryCatch(
    feols(
      y ~ log_enrollment_undergrad + eligibility_proxy_current_10 +
        eligibility_coverage_current_10 + cumple_umbral_acreditacion |
        id + year | gratuidad ~ instrumento_acreditacion4_2015_post,
      data = d, vcov = ~ id, notes = FALSE, warn = FALSE
    ),
    error = function(e) e
  )

  main_specs <- list(
    "IV-FE baseline total effect: 2015 accreditation threshold" = list(
      model = m_iv_main,
      instruments = "instrumento_acreditacion4_2015_post"
    ),
    "IV-FE conditional effect: contemporaneous accreditation years" = list(
      model = m_iv_cond_cont,
      instruments = "instrumento_acreditacion4_2015_post"
    ),
    "IV-FE conditional effect: contemporaneous accreditation threshold" = list(
      model = m_iv_cond_dummy,
      instruments = "instrumento_acreditacion4_2015_post"
    )
  )

  for (spec_name in names(main_specs)) {
    entry <- main_specs[[spec_name]]
    model <- entry$model
    diagnostics[[length(diagnostics) + 1]] <- iv_diagnostic_row(
      model, outcome, spec_name, d
    )
    if (!inherits(model, "fixest")) next

    estimate_row <- extract_term(
      model, "fit_gratuidad", "IV-FE", outcome, spec_name
    )
    results[[length(results) + 1]] <- estimate_row
    if (grepl("conditional effect", spec_name, fixed = TRUE)) {
      contemporary_control_results[[length(contemporary_control_results) + 1]] <-
        estimate_row
    }

    fs <- summary(model, stage = 1)
    for (instrument in entry$instruments) {
      first_stage_results[[length(first_stage_results) + 1]] <- extract_term(
        fs, instrument, "First stage", outcome, spec_name
      )
    }
  }

  if (inherits(m_iv_main, "fixest")) {
    models_main[[outcome]] <- m_iv_main
    comb <- extract_term(
      m_iv_main, "fit_gratuidad", "IV-FE", outcome,
      "IV-FE baseline total effect: 2015 accreditation threshold"
    )
    comb$sargan_stat <- fitstat_value(m_iv_main, "sargan.stat")
    comb$sargan_p <- fitstat_value(m_iv_main, "sargan.p")
    combined_iv_results[[length(combined_iv_results) + 1]] <- comb
    capture_model(
      m_iv_main,
      file.path(models_dir, paste0(
        "iv_baseline_accreditation_threshold_", safe_file_name(outcome), ".txt"
      )),
      paste("IV-FE baseline: 2015 accreditation threshold -", outcome)
    )
  }
  if (inherits(m_iv_cond_cont, "fixest")) {
    models_robust[[outcome]] <- m_iv_cond_cont
  }

  # Baseline reduced form for the accreditation-threshold instrument.
  m_rf_main <- feols(
    y ~ instrumento_acreditacion4_2015_post | id + year,
    data = d, vcov = ~ id, notes = FALSE, warn = FALSE
  )
  reduced_form_results[[length(reduced_form_results) + 1]] <- extract_term(
    m_rf_main, "instrumento_acreditacion4_2015_post", "Reduced form", outcome,
    "Reduced form: 2015 accreditation threshold x Post-2016"
  )

  m_rf_tuition <- feols(
    y ~ instrument_tuition_coverage_2006_post | id + year,
    data = d, vcov = ~ id, notes = FALSE, warn = FALSE
  )
  reduced_form_results[[length(reduced_form_results) + 1]] <- extract_term(
    m_rf_tuition,
    "instrument_tuition_coverage_2006_post",
    "Reduced form",
    outcome,
    "Reduced form: 2006 tuition reference coverage x Post-2016"
  )

  # Robusteces de identificación: cada instrumento por separado, acreditación
  # 2015 discreta versus continua, medias pretratamiento e instrumentos
  # contemporáneos (estos últimos se etiquetan como potencialmente endógenos).
  robustness_models <- list(
    "IV-FE 1Z: 2015 eligibility x Post-2016" = list(
      model = tryCatch(feols(
        y ~ 1 | id + year |
          gratuidad ~ instrumento_elegibilidad_2015_post,
        data = d, vcov = ~ id, notes = FALSE, warn = FALSE
      ), error = function(e) e),
      instruments = "instrumento_elegibilidad_2015_post"
    ),
    "IV-FE 1Z: 2015 accreditation threshold x Post-2016" = list(
      model = tryCatch(feols(
        y ~ 1 | id + year |
          gratuidad ~ instrumento_acreditacion4_2015_post,
        data = d, vcov = ~ id, notes = FALSE, warn = FALSE
      ), error = function(e) e),
      instruments = "instrumento_acreditacion4_2015_post"
    ),
    "IV-FE 1Z: 2006 tuition reference coverage x Post-2016" = list(
      model = tryCatch(feols(
        y ~ 1 | id + year |
          gratuidad ~ instrument_tuition_coverage_2006_post,
        data = d, vcov = ~ id, notes = FALSE, warn = FALSE
      ), error = function(e) e),
      instruments = "instrument_tuition_coverage_2006_post"
    ),
    "IV-FE 2Z: accreditation threshold + tuition coverage" = list(
      model = tryCatch(feols(
        y ~ 1 | id + year |
          gratuidad ~ instrumento_acreditacion4_2015_post +
            instrument_tuition_coverage_2006_post,
        data = d, vcov = ~ id, notes = FALSE, warn = FALSE
      ), error = function(e) e),
      instruments = c(
        "instrumento_acreditacion4_2015_post",
        "instrument_tuition_coverage_2006_post"
      )
    ),
    "IV-FE 2Z: 2015 eligibility + accreditation years" = list(
      model = tryCatch(feols(
        y ~ 1 | id + year |
          gratuidad ~ instrumento_elegibilidad_2015_post +
            instrumento_acreditacion_anios_2015_post,
        data = d, vcov = ~ id, notes = FALSE, warn = FALSE
      ), error = function(e) e),
      instruments = c(
        "instrumento_elegibilidad_2015_post",
        "instrumento_acreditacion_anios_2015_post"
      )
    ),
    "IV-FE 2Z: pre-treatment means" = list(
      model = tryCatch(feols(
        y ~ 1 | id + year |
          gratuidad ~ instrumento_elegibilidad_media_pre_post +
            instrumento_acreditacion_media_pre_post,
        data = d, vcov = ~ id, notes = FALSE, warn = FALSE
      ), error = function(e) e),
      instruments = c(
        "instrumento_elegibilidad_media_pre_post",
        "instrumento_acreditacion_media_pre_post"
      )
    ),
    "IV-FE 2Z: contemporaneous instruments (endogenous robustness)" = list(
      model = tryCatch(feols(
        y ~ 1 | id + year |
          gratuidad ~ instrumento_elegibilidad_actual_post +
            instrumento_acreditacion_actual_post,
        data = d, vcov = ~ id, notes = FALSE, warn = FALSE
      ), error = function(e) e),
      instruments = c(
        "instrumento_elegibilidad_actual_post",
        "instrumento_acreditacion_actual_post"
      )
    )
  )

  for (spec_name in names(robustness_models)) {
    entry <- robustness_models[[spec_name]]
    model <- entry$model
    diagnostics[[length(diagnostics) + 1]] <- iv_diagnostic_row(
      model, outcome, spec_name, d
    )
    if (!inherits(model, "fixest")) next
    robustness_results[[length(robustness_results) + 1]] <- extract_term(
      model, "fit_gratuidad", "IV-FE", outcome, spec_name
    )
    fs <- summary(model, stage = 1)
    for (instrument in entry$instruments) {
      first_stage_results[[length(first_stage_results) + 1]] <- extract_term(
        fs, instrument, "First stage", outcome, spec_name
      )
    }
  }

  # Common-sample robustness. No winsorization is performed: unit problems are
  # audited and retained for manual review rather than hidden by trimming.
  common_flag <- panel$common_sample[match(paste(d$id, d$year), paste(panel$id, panel$year))]
  d_common <- d[!is.na(common_flag) & common_flag, ]
  if (nrow(d_common) > 0) {
    m_common_main <- tryCatch(feols(
      y ~ 1 | id + year |
        gratuidad ~ instrumento_acreditacion4_2015_post,
      data = d_common, vcov = ~ id, notes = FALSE, warn = FALSE
    ), error = function(e) e)
    if (inherits(m_common_main, "fixest")) {
      robustness_results[[length(robustness_results) + 1]] <- extract_term(
        m_common_main, "fit_gratuidad", "IV-FE", outcome,
        "IV-FE baseline: common outcome sample"
      )
    }
  }

  # Placebos conjuntos de forma reducida, exclusivamente antes de 2016.
  for (fake_year in 2013:2015) {
    dp <- panel[panel$year < treatment_year, c(
      "id", "year", outcome, "elegibilidad_proxy_2015_10",
      "cumple_umbral_2015", "log_enrollment_undergrad"
    )]
    names(dp)[3] <- "y"
    dp$placebo_elegibilidad <- with(
      dp, elegibilidad_proxy_2015_10 * as.integer(year >= fake_year)
    )
    dp$placebo_acreditacion4 <- with(
      dp, cumple_umbral_2015 * as.integer(year >= fake_year)
    )
    m_placebo <- tryCatch(
      feols(
        y ~ placebo_elegibilidad + placebo_acreditacion4 +
          log_enrollment_undergrad | id + year,
        data = dp, vcov = ~ id, notes = FALSE, warn = FALSE
      ),
      error = function(e) e
    )
    if (inherits(m_placebo, "fixest")) {
      for (term in c("placebo_elegibilidad", "placebo_acreditacion4")) {
        pl <- extract_term(
          m_placebo, term, "Reduced-form placebo", outcome,
          paste0("Pseudo-treatment starts in ", fake_year)
        )
        placebo_results[[length(placebo_results) + 1]] <- pl
      }
    }
  }

  # Dedicated output for outcomes that are already annual real log changes.
  # No signed-log transformation is applied to levels or ratios.
  if (outcome %in% variation_indicators) {
    results_variation[[length(results_variation) + 1]] <- extract_term(
      m_ols, "gratuidad", "OLS-FE", outcome,
      "OLS-FE total effect: no post-treatment controls"
    )
    if (inherits(m_iv_main, "fixest")) {
      results_variation[[length(results_variation) + 1]] <- extract_term(
        m_iv_main, "fit_gratuidad", "IV-FE", outcome,
        "IV-FE baseline total effect: 2015 accreditation threshold"
      )
    }
    if (inherits(m_iv_cond_cont, "fixest")) {
      results_variation[[length(results_variation) + 1]] <- extract_term(
        m_iv_cond_cont, "fit_gratuidad", "IV-FE", outcome,
        "IV-FE conditional effect: contemporaneous accreditation years"
      )
    }
  }
}

results <- do.call(rbind, Filter(Negate(is.null), results))
first_stage_results <- do.call(
  rbind, Filter(Negate(is.null), first_stage_results)
)
diagnostics <- do.call(rbind, diagnostics)
results_variation <- do.call(
  rbind, Filter(Negate(is.null), results_variation)
)
reduced_form_results <- do.call(
  rbind, Filter(Negate(is.null), reduced_form_results)
)
combined_iv_results <- do.call(
  rbind, Filter(Negate(is.null), combined_iv_results)
)
contemporary_control_results <- do.call(
  rbind, Filter(Negate(is.null), contemporary_control_results)
)
robustness_results <- do.call(
  rbind, Filter(Negate(is.null), robustness_results)
)
placebo_results <- do.call(
  rbind, Filter(Negate(is.null), placebo_results)
)

term_labels <- c(
  "gratuidad" = "Tuition-free-policy participation",
  "fit_gratuidad" = "Instrumented tuition-free-policy participation",
  "instrumento_elegibilidad_2015_post" =
    "2015 eligibility x Post-2016",
  "instrumento_acreditacion4_2015_post" =
    "2015 accreditation threshold x Post-2016",
  "instrumento_acreditacion_anios_2015_post" =
    "2015 accreditation years x Post-2016",
  "instrument_tuition_coverage_2006_post" =
    "Standardized 2006 tuition reference coverage x Post-2016",
  "instrumento_elegibilidad_media_pre_post" =
    "Pre-treatment mean eligibility x Post-2016",
  "instrumento_acreditacion_media_pre_post" =
    "Pre-treatment mean accreditation x Post-2016",
  "instrumento_elegibilidad_actual_post" =
    "Contemporaneous eligibility x Post-2016",
  "instrumento_acreditacion_actual_post" =
    "Contemporaneous accreditation threshold x Post-2016",
  "placebo_elegibilidad" = "Eligibility placebo",
  "placebo_acreditacion4" = "Accreditation-threshold placebo"
)
add_term_label <- function(x) {
  if (is.null(x) || !"term" %in% names(x)) return(x)
  labels <- unname(term_labels[x$term])
  x$term_label <- ifelse(is.na(labels), x$term, labels)
  x
}
results <- add_term_label(results)
first_stage_results <- add_term_label(first_stage_results)
reduced_form_results <- add_term_label(reduced_form_results)
combined_iv_results <- add_term_label(combined_iv_results)
contemporary_control_results <- add_term_label(contemporary_control_results)
robustness_results <- add_term_label(robustness_results)
placebo_results <- add_term_label(placebo_results)
results_variation <- add_term_label(results_variation)

# Main table with the original and restricted samples side by side.
results_with_closure_controls <- estimate_main_results_for_sample(
  panel_with_closure_controls
)
results_with_closure_controls <- add_term_label(results_with_closure_controls)

comparison_original <- results_with_closure_controls
comparison_original$sample <- "original"
comparison_original$sample_label <- "With closure-risk controls"
comparison_restricted <- results
comparison_restricted$sample <- "restricted"
comparison_restricted$sample_label <- "Excluding closure-risk controls"
main_comparison_raw <- rbind(comparison_original, comparison_restricted)

comparison_keys <- c("indicator", "specification", "model", "term", "term_label")
comparison_measures <- c(
  "estimate", "std_error", "p_value", "conf_low", "conf_high", "n_obs"
)
original_wide <- comparison_original[c(comparison_keys, comparison_measures)]
restricted_wide <- comparison_restricted[c(comparison_keys, comparison_measures)]
names(original_wide)[match(comparison_measures, names(original_wide))] <-
  paste0(comparison_measures, "_original")
names(restricted_wide)[match(comparison_measures, names(restricted_wide))] <-
  paste0(comparison_measures, "_restricted")
main_comparison_wide <- merge(
  original_wide, restricted_wide,
  by = comparison_keys, all = TRUE, sort = FALSE
)

main_spec_order <- c(
  "OLS-FE total effect: no post-treatment controls",
  "IV-FE baseline total effect: 2015 accreditation threshold",
  "IV-FE conditional effect: contemporaneous accreditation years",
  "IV-FE conditional effect: contemporaneous accreditation threshold"
)
main_comparison_wide$spec_order <- match(
  main_comparison_wide$specification, main_spec_order
)
main_comparison_wide$outcome_order <- match(
  main_comparison_wide$indicator, indicators_fitch
)
main_comparison_wide <- main_comparison_wide[
  order(main_comparison_wide$outcome_order, main_comparison_wide$spec_order),
]
main_comparison_wide$spec_order <- NULL
main_comparison_wide$outcome_order <- NULL

iv_stars <- function(p) {
  if (!is.finite(p)) return("")
  if (p < 0.01) return("***")
  if (p < 0.05) return("**")
  if (p < 0.10) return("*")
  ""
}
iv_paper_cell <- function(row, statistic) {
  if (!nrow(row)) return("--")
  if (statistic == "estimate") {
    if (!is.finite(row$estimate[1])) return("--")
    return(paste0(sprintf("%.3f", row$estimate[1]), iv_stars(row$p_value[1])))
  }
  if (statistic == "se") {
    if (!is.finite(row$std_error[1])) return("")
    return(paste0("(", sprintf("%.3f", row$std_error[1]), ")"))
  }
  if (statistic == "ci") {
    if (!is.finite(row$conf_low[1]) || !is.finite(row$conf_high[1])) return("")
    return(paste0("[", sprintf("%.3f", row$conf_low[1]), ", ",
                  sprintf("%.3f", row$conf_high[1]), "]"))
  }
  if (statistic == "n") {
    if (!is.finite(row$n_obs[1])) return("--")
    return(as.character(as.integer(row$n_obs[1])))
  }
  ""
}

paper_rows <- list()
for (outcome in indicators_fitch) {
  for (specification in main_spec_order) {
    original_row <- comparison_original[
      comparison_original$indicator == outcome &
        comparison_original$specification == specification, , drop = FALSE
    ]
    restricted_row <- comparison_restricted[
      comparison_restricted$indicator == outcome &
        comparison_restricted$specification == specification, , drop = FALSE
    ]
    labels <- c(
      paste0(outcome, " - ", specification),
      "(SE)", "[95% CI]", "Observations"
    )
    paper_rows[[length(paper_rows) + 1L]] <- data.frame(
      `Outcome / statistic` = labels,
      `(1) With closure-risk controls` = vapply(
        c("estimate", "se", "ci", "n"),
        function(statistic) iv_paper_cell(original_row, statistic), character(1)
      ),
      `(2) Excluding closure-risk controls` = vapply(
        c("estimate", "se", "ci", "n"),
        function(statistic) iv_paper_cell(restricted_row, statistic), character(1)
      ),
      check.names = FALSE, stringsAsFactors = FALSE
    )
  }
}
main_comparison_paper <- do.call(rbind, paper_rows)

write.csv(
  main_comparison_wide,
  file.path(output_dir, "Table_IV_Main_Sample_Comparison.csv"),
  row.names = FALSE, fileEncoding = "UTF-8"
)

comparison_xlsx <- file.path(output_dir, "Table_IV_Main.xlsx")
wb_comparison <- openxlsx::createWorkbook()
openxlsx::addWorksheet(wb_comparison, "paper_table")
openxlsx::addWorksheet(wb_comparison, "raw_results")
header_style <- openxlsx::createStyle(
  textDecoration = "bold", fgFill = "#D9EAF7", border = "bottom"
)
openxlsx::writeData(
  wb_comparison, "paper_table", main_comparison_paper,
  rowNames = FALSE, keepNA = FALSE, headerStyle = header_style
)
openxlsx::writeData(
  wb_comparison, "raw_results", main_comparison_raw,
  rowNames = FALSE, keepNA = FALSE, headerStyle = header_style
)
openxlsx::freezePane(wb_comparison, "paper_table", firstRow = TRUE, firstCol = TRUE)
openxlsx::freezePane(wb_comparison, "raw_results", firstRow = TRUE)
openxlsx::setColWidths(
  wb_comparison, "paper_table",
  cols = seq_len(ncol(main_comparison_paper)), widths = "auto"
)
openxlsx::setColWidths(
  wb_comparison, "raw_results",
  cols = seq_len(ncol(main_comparison_raw)), widths = "auto"
)
openxlsx::saveWorkbook(wb_comparison, comparison_xlsx, overwrite = TRUE)
sanitize_openxlsx_package(comparison_xlsx)

tuition_specification <- grepl(
  "tuition", robustness_results$specification, ignore.case = TRUE
)
tuition_diagnostic_specification <- grepl(
  "tuition", diagnostics$specification, ignore.case = TRUE
)
tuition_iv_results <- merge(
  robustness_results[tuition_specification, , drop = FALSE],
  diagnostics[tuition_diagnostic_specification, , drop = FALSE],
  by = c("indicator", "specification"),
  all = TRUE,
  suffixes = c("_estimate", "_diagnostic")
)
tuition_first_stage_results <- first_stage_results[
  grepl("tuition", first_stage_results$specification, ignore.case = TRUE),
  , drop = FALSE
]

write.csv(
  results,
  file.path(output_dir, "results_ols_iv_fc5.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)
write.csv(
  first_stage_results,
  file.path(output_dir, "results_first_stage_fc5.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)
write.csv(
  diagnostics,
  file.path(output_dir, "diagnostics_iv_fc5.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)
write.csv(
  results_variation,
  file.path(output_dir, "results_annual_real_log_changes.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)
write.csv(
  reduced_form_results,
  file.path(output_dir, "results_reduced_form_fc5.csv"),
  row.names = FALSE, fileEncoding = "UTF-8"
)
write.csv(
  combined_iv_results,
  file.path(output_dir, "results_baseline_iv_fc5.csv"),
  row.names = FALSE, fileEncoding = "UTF-8"
)
write.csv(
  contemporary_control_results,
  file.path(output_dir, "results_conditional_iv_fc5.csv"),
  row.names = FALSE, fileEncoding = "UTF-8"
)
write.csv(
  robustness_results,
  file.path(output_dir, "results_iv_robustness_fc5.csv"),
  row.names = FALSE, fileEncoding = "UTF-8"
)
write.csv(
  placebo_results,
  file.path(output_dir, "results_pre2016_placebos_fc5.csv"),
  row.names = FALSE, fileEncoding = "UTF-8"
)
write.csv(
  tuition_iv_results,
  file.path(output_dir, "results_tuition_instrument_iv.csv"),
  row.names = FALSE, fileEncoding = "UTF-8"
)
write.csv(
  tuition_first_stage_results,
  file.path(output_dir, "results_tuition_instrument_first_stage.csv"),
  row.names = FALSE, fileEncoding = "UTF-8"
)

# Editable HTML tables.
if (length(models_main) > 0) {
  etable(
    models_main,
    headers = names(models_main),
    fitstat = ~ n + ivwald1 + wh + sargan,
    tex = FALSE,
    file = file.path(output_dir, "table_iv_baseline_accreditation_fc5.html"),
    replace = TRUE
  )
}

if (length(models_robust) > 0) {
  etable(
    models_robust,
    headers = names(models_robust),
    fitstat = ~ n + ivwald1 + wh + sargan,
    tex = FALSE,
    file = file.path(output_dir, "table_iv_conditional_fc5.html"),
    replace = TRUE
  )
}

# ------------------------------------------------------------
# 8. Gráficos generales e individuales
# ------------------------------------------------------------
plot_data <- results[
  results$model %in% c("OLS-FE", "IV-FE") &
    is.finite(results$estimate) & is.finite(results$conf_low),
]

plot_data$indicator <- factor(
  plot_data$indicator,
  levels = rev(indicators_fitch)
)

p_coefficients <- ggplot(
  plot_data,
  aes(x = estimate, y = indicator, color = specification)
) +
  geom_vline(xintercept = 0, color = "grey45", linewidth = 0.5) +
  geom_errorbar(
    aes(xmin = conf_low, xmax = conf_high),
    orientation = "y",
    width = 0.18,
    position = position_dodge(width = 0.55)
  ) +
  geom_point(size = 2.2, position = position_dodge(width = 0.55)) +
  facet_wrap(~ specification, scales = "free_x") +
  labs(
    title = "Effect of tuition-free-policy participation on Fitch indicators",
    subtitle = "OLS and IV with university and year fixed effects",
    x = "Coefficient and 95% confidence interval",
    y = NULL,
    color = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")

ggsave(
  file.path(plots_dir, "coefficients_ols_iv_fc5.png"),
  p_coefficients,
  width = 13,
  height = 9,
  dpi = 300
)

if (nrow(first_stage_results) > 0) {
  p_first_stage <- ggplot(
    first_stage_results,
    aes(x = estimate, y = indicator, color = term_label)
  ) +
    geom_vline(xintercept = 0, color = "grey45", linewidth = 0.5) +
    geom_errorbar(
      aes(xmin = conf_low, xmax = conf_high),
      orientation = "y", width = 0.15
    ) +
    geom_point(size = 2.3) +
    facet_wrap(~ specification, scales = "free_x") +
    labs(
      title = "First-stage IV estimates",
      subtitle = "Dependent variable: observed tuition-free-policy participation",
      x = "Instrument coefficient and 95% CI",
      y = NULL,
      color = "Instrument"
    ) +
    theme_minimal(base_size = 11) +
    guides(color = guide_legend(ncol = 2, byrow = TRUE)) +
    theme(
      legend.position = "bottom",
      legend.box = "vertical"
    )

  ggsave(
    file.path(plots_dir, "first_stage_iv_fc5.png"),
    p_first_stage,
    width = 12,
    height = 8,
    dpi = 300
  )
}

# Un conjunto de productos por indicador, equivalente a la organización del DiD.
for (outcome in indicators_fitch) {
  file_stub <- safe_file_name(outcome)
  one <- results[results$indicator == outcome, , drop = FALSE]
  one$specification <- factor(
    one$specification,
    levels = c(
      "OLS-FE total effect: no post-treatment controls",
      "IV-FE baseline total effect: 2015 accreditation threshold",
      "IV-FE conditional effect: contemporaneous accreditation years",
      "IV-FE conditional effect: contemporaneous accreditation threshold"
    )
  )

  p_one <- ggplot(one, aes(x = estimate, y = specification)) +
    geom_vline(xintercept = 0, color = "grey40", linewidth = 0.6) +
    geom_errorbar(
      aes(xmin = conf_low, xmax = conf_high),
      orientation = "y", width = 0.16
    ) +
    geom_point(size = 2.8) +
    labs(
      title = paste(
        strwrap(paste("OLS and IV -", outcome), width = 68),
        collapse = "\n"
      ),
      subtitle = "University and year fixed effects; 95% CI",
      x = "Estimated effect of tuition-free-policy participation",
      y = NULL
    ) +
    theme_minimal(base_size = 12) +
    theme(plot.title.position = "plot")

  ggsave(file.path(plots_dir, paste0("iv_", file_stub, ".png")),
         p_one, width = 12, height = 6.2, dpi = 300)
  ggsave(file.path(plots_dir, paste0("iv_", file_stub, ".pdf")),
         p_one, width = 12, height = 6.2)

  fs_one <- first_stage_results[
    first_stage_results$indicator == outcome, , drop = FALSE
  ]
  p_fs <- ggplot(
    fs_one,
    aes(x = estimate, y = specification, color = term_label)
  ) +
    geom_vline(xintercept = 0, color = "grey40", linewidth = 0.6) +
    geom_errorbar(
      aes(xmin = conf_low, xmax = conf_high),
      orientation = "y", width = 0.16
    ) +
    geom_point(size = 2.8) +
    labs(
      title = paste(
        strwrap(paste("First stage -", outcome), width = 68),
        collapse = "\n"
      ),
      subtitle = "Dependent variable: tuition-free-policy participation; 95% CI",
      x = "Instrument coefficient",
      y = NULL,
      color = "Instrument"
    ) +
    theme_minimal(base_size = 12) +
    guides(color = guide_legend(ncol = 2, byrow = TRUE)) +
    theme(
      legend.position = "bottom",
      legend.box = "vertical",
      plot.title.position = "plot",
      legend.text = element_text(size = 9)
    )
  ggsave(file.path(plots_dir, paste0("first_stage_", file_stub, ".png")),
         p_fs, width = 14, height = 10, dpi = 300)

  # Gráfico descriptivo de niveles, comparable al del DiD (no es el IV).
  lev <- panel[c("year", "cohort", outcome)]
  names(lev)[3] <- "y"
  lev$group <- ifelse(is.na(lev$cohort), "Control", "Treated")
  lev <- aggregate(y ~ year + group, data = lev, FUN = function(x) mean(x, na.rm = TRUE))
  lev$event_time <- lev$year - treatment_year
  lev <- lev[lev$event_time >= -5 & lev$event_time <= 5, ]

  p_levels <- ggplot(lev, aes(x = event_time, y = y, color = group)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
    geom_line(linewidth = 0.9) + geom_point(size = 2) +
    scale_x_continuous(breaks = -5:5) +
    labs(
      title = paste(
        strwrap(paste("Mean levels -", outcome), width = 68),
        collapse = "\n"
      ),
      subtitle = "Descriptive plot; not an IV estimate",
      x = "Event time relative to 2016",
      y = outcome,
      color = NULL
    ) +
    theme_minimal(base_size = 12) +
    theme(plot.title.position = "plot")
  ggsave(file.path(plots_dir, paste0("levels_", file_stub, ".png")),
         p_levels, width = 10.5, height = 6.2, dpi = 300)

  if (outcome %in% variation_indicators) {
    one_log <- results_variation[
      results_variation$indicator == outcome, , drop = FALSE
    ]
    one_log$specification <- factor(
      one_log$specification,
      levels = c(
        "OLS-FE total effect: no post-treatment controls",
        "IV-FE baseline total effect: 2015 accreditation threshold",
        "IV-FE conditional effect: contemporaneous accreditation years"
      )
    )
    p_log <- ggplot(one_log, aes(x = estimate, y = specification)) +
      geom_vline(xintercept = 0, color = "grey40", linewidth = 0.6) +
      geom_errorbar(
        aes(xmin = conf_low, xmax = conf_high),
        orientation = "y", width = 0.16
      ) +
      geom_point(size = 2.8) +
      labs(
        title = paste(
          strwrap(
            paste("Annual real log-change model -", outcome), width = 68
          ),
          collapse = "\n"
        ),
        subtitle = "University and year fixed effects; 95% CI",
        x = "Estimated effect",
        y = NULL
      ) +
      theme_minimal(base_size = 12) +
      theme(plot.title.position = "plot")
    ggsave(
      file.path(plots_log_dir, paste0("iv_annual_log_change_", file_stub, ".png")),
      p_log, width = 12, height = 6.2, dpi = 300
    )
  }

  desc_out <- merge(
    one,
    diagnostics[diagnostics$indicator == outcome, ],
    by = c("indicator", "specification"),
    all.x = TRUE
  )
  desc_out$section <- "OLS_IV_diagnostics"
  rf_out <- reduced_form_results[reduced_form_results$indicator == outcome, ]
  rf_out$section <- "reduced_form"
  robust_out <- robustness_results[robustness_results$indicator == outcome, ]
  robust_out$section <- "robustness"
  placebo_out <- placebo_results[placebo_results$indicator == outcome, ]
  placebo_out$section <- "pre2016_placebos"
  combined_out <- combined_iv_results[combined_iv_results$indicator == outcome, ]
  combined_out$section <- "iv_baseline_accreditation_threshold"

  # Cada sección conserva sus columnas específicas en un único archivo largo.
  all_names <- Reduce(union, lapply(
    list(desc_out, rf_out, robust_out, placebo_out, combined_out), names
  ))
  align_cols <- function(x) {
    for (nm in setdiff(all_names, names(x))) x[[nm]] <- NA
    x[all_names]
  }
  desc_complete <- do.call(rbind, lapply(
    list(desc_out, rf_out, robust_out, placebo_out, combined_out), align_cols
  ))
  write.csv(desc_complete, file.path(desc_dir, paste0("results_", file_stub, ".csv")),
            row.names = FALSE, fileEncoding = "UTF-8")
}

# ------------------------------------------------------------
# 9. Dynamic DiD versus static IV-FE (same treatment cohort,
#    explicitly different estimands)
# ------------------------------------------------------------
dynamic_dir <- file.path(
  free_college_dir, "Difference-in-Difference Analysis", "event"
)
dynamic_files <- if (dir.exists(dynamic_dir)) {
  list.files(
    dynamic_dir,
    pattern = "^event_dynamic_.*[.]csv$",
    full.names = TRUE
  )
} else {
  character(0)
}

find_dynamic_file <- function(outcome) {
  if (length(dynamic_files) == 0) return(NA_character_)
  stems <- sub("^event_dynamic_", "", basename(dynamic_files))
  stems <- sub("[.]csv$", "", stems)
  matches <- which(normalize_name(stems) == normalize_name(outcome))
  if (length(matches) == 1) dynamic_files[matches] else NA_character_
}

comparison_rows <- list()
for (outcome in indicators_fitch) {
  dynamic_file <- find_dynamic_file(outcome)
  iv_row <- combined_iv_results[
    combined_iv_results$indicator == outcome, , drop = FALSE
  ]
  if (is.na(dynamic_file) || nrow(iv_row) != 1) next

  dynamic <- read_utf8_csv(dynamic_file)
  required_dynamic <- c("event_time", "att", "ci_low", "ci_high")
  if (!all(required_dynamic %in% names(dynamic))) next
  for (column in required_dynamic) {
    dynamic[[column]] <- suppressWarnings(as.numeric(dynamic[[column]]))
  }
  dynamic <- dynamic[
    is.finite(dynamic$event_time) & is.finite(dynamic$att), , drop = FALSE
  ]
  if (nrow(dynamic) == 0) next

  comparison <- data.frame(
    indicator = outcome,
    event_time = dynamic$event_time,
    did_att = dynamic$att,
    did_ci_low = dynamic$ci_low,
    did_ci_high = dynamic$ci_high,
    static_iv_late = iv_row$estimate[1],
    static_iv_ci_low = iv_row$conf_low[1],
    static_iv_ci_high = iv_row$conf_high[1],
    stringsAsFactors = FALSE
  )
  comparison_rows[[length(comparison_rows) + 1]] <- comparison

  p_compare <- ggplot(comparison, aes(x = event_time, y = did_att)) +
    geom_hline(yintercept = 0, color = "grey55", linewidth = 0.5) +
    geom_hline(
      yintercept = iv_row$estimate[1], color = "#B2182B", linewidth = 1
    ) +
    geom_hline(
      yintercept = c(iv_row$conf_low[1], iv_row$conf_high[1]),
      color = "#B2182B", linetype = "dashed", linewidth = 0.6
    ) +
    geom_errorbar(
      aes(ymin = did_ci_low, ymax = did_ci_high),
      width = 0.12, na.rm = TRUE
    ) +
    geom_point(size = 2.7) +
    scale_x_continuous(breaks = -5:5) +
    labs(
      title = "Dynamic DiD and Static IV-FE Estimate",
      subtitle = paste(
        strwrap(
          paste0(
            outcome,
            " | Points: event-time ATT; red line: static IV-FE LATE. ",
            "The estimands and conditioning sets are not identical."
          ),
          width = 105
        ),
        collapse = "\n"
      ),
      x = "Event time relative to 2016",
      y = "Estimated effect"
    ) +
    theme_minimal(base_size = 11)
  ggsave(
    file.path(
      plots_dir,
      paste0("compare_dynamic_did_static_iv_", safe_file_name(outcome), ".png")
    ),
    p_compare, width = 10.5, height = 6.2, dpi = 300
  )
}

if (length(comparison_rows) > 0) {
  comparison_results <- do.call(rbind, comparison_rows)
  write.csv(
    comparison_results,
    file.path(output_dir, "comparison_dynamic_did_static_iv_fc5.csv"),
    row.names = FALSE,
    fileEncoding = "UTF-8"
  )
}

# ------------------------------------------------------------
# 10. Execution summary
# ------------------------------------------------------------
summary_lines <- c(
  "FC5 IV-FE for financial indicators",
  "==========================",
  paste("Run date:", Sys.time()),
  paste("Input file:", input_file),
  paste("Panel observations:", nrow(panel)),
  paste("Universities:", length(unique(panel$id))),
  paste("Indicators processed:", length(indicators_fitch)),
  paste("Treatment year:", treatment_year),
  paste("Baseline year for 2015 instruments:", baseline_accreditation_year),
  "Identification sample: 2016 adoption cohort versus never-treated universities.",
  "Both comparison columns exclude Universidad de O'Higgins and Universidad de Aysen.",
  "Restricted main sample additionally excludes UNIACC, Universidad del Pacifico, UCINF, ARCIS and Universidad de Los Leones.",
  "Table_IV_Main.xlsx reports the original and restricted samples side by side.",
  "",
  "Baseline instrument: 1(2015 accreditation years >= 4) x Post-2016.",
  "Baseline estimand: static IV-FE LATE, exactly identified, with no post-treatment controls.",
  "Eligibility 2015 x Post-2016 is retained only as a weak-instrument robustness check.",
  "Tuition instrument: standardized 2006 reference-tuition coverage x Post-2016.",
  "Tuition coverage = reference tuition / actual tuition = 1 + (reference - actual) / actual.",
  "The tuition instrument is estimated alone and jointly with the 2015 accreditation threshold.",
  "Total-effect baseline: no contemporaneous enrollment, eligibility or accreditation controls.",
  "Conditional model: adds contemporaneous log enrollment, eligibility, proxy coverage and accreditation.",
  "The conditional model may include post-treatment controls and is interpreted separately.",
  "Fixed effects: university and year.",
  "Standard errors: clustered by university.",
  "Weak-instrument decisions must use the clustered IV Wald statistic, not the conventional first-stage F alone.",
  "Exclusion restrictions remain substantive assumptions: accreditation and tuition coverage may proxy for persistent institutional capacity.",
  "The static IV-FE LATE is not the same estimand as a dynamic DiD event-time ATT.",
  "Treatment is absorbing; execution stops if an exit is detected.",
  "Scale robustness: annual real log-change outcomes are estimated directly; signed log1p is not used.",
  "No observations are winsorized or deleted as an automatic unit correction.",
  "Additional robustness: continuous 2015 accreditation, pre-treatment means, contemporaneous instruments, reduced forms, pre-2016 placebos and common sample.",
  "",
  paste("Results saved to:", output_dir)
)

writeLines(
  summary_lines,
  file.path(output_dir, "execution_summary_fc5.txt"),
  useBytes = TRUE
)

message("Process completed. Results saved to: ", output_dir)
