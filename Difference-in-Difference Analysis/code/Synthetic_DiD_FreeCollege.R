#!/usr/bin/env Rscript

# Synthetic Difference-in-Differences for the Chilean Free College project.
# Standalone pipeline rebuilt from scratch. It reads only the three official
# inputs under "Construccion de panel data" and writes only SDID outputs.

suppressPackageStartupMessages({
  if (!requireNamespace("synthdid", quietly = TRUE)) stop("Missing required package: synthdid")
  if (!requireNamespace("openxlsx", quietly = TRUE)) stop("Missing required package: openxlsx")
  if (!requireNamespace("zip", quietly = TRUE)) stop("Missing required package: zip")
})

try(invisible(Sys.setlocale("LC_CTYPE", "English_United States.utf8")), silent = TRUE)

FIRST_YEAR <- 2011L
BASE_YEAR <- 2015L
TREAT_YEAR <- 2016L
LAST_YEAR <- 2022L
DEFAULT_BOOT_REPS <- 500L
DEFAULT_SEED <- 123L

parse_args <- function(args = commandArgs(trailingOnly = TRUE)) {
  detected <- parallel::detectCores(logical = TRUE)
  if (!is.finite(detected)) detected <- 2L
  out <- list(root = Sys.getenv("FREE_COLLEGE_DIR", unset = "."), boot_reps = DEFAULT_BOOT_REPS,
              seed = DEFAULT_SEED, workers = max(1L, min(8L, as.integer(detected %/% 2L))))
  for (arg in args) {
    if (startsWith(arg, "--root=")) out$root <- sub("^--root=", "", arg)
    if (startsWith(arg, "--boot-reps=")) out$boot_reps <- as.integer(sub("^--boot-reps=", "", arg))
    if (startsWith(arg, "--seed=")) out$seed <- as.integer(sub("^--seed=", "", arg))
    if (startsWith(arg, "--workers=")) out$workers <- as.integer(sub("^--workers=", "", arg))
  }
  if (!is.finite(out$boot_reps) || out$boot_reps < 1L) stop("--boot-reps must be a positive integer")
  if (!is.finite(out$seed)) stop("--seed must be an integer")
  if (!is.finite(out$workers) || out$workers < 1L) stop("--workers must be a positive integer")
  out$root <- normalizePath(out$root, winslash = "/", mustWork = TRUE)
  out
}

norm_name <- function(x) {
  x <- as.character(x)
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  x <- tolower(trimws(x))
  x <- gsub("[^a-z0-9]+", " ", x)
  gsub("\\s+", " ", trimws(x))
}

safe_name <- function(x) {
  z <- iconv(as.character(x), from = "", to = "ASCII//TRANSLIT")
  z <- gsub("[^A-Za-z0-9_-]+", "_", z)
  gsub("^_+|_+$", "", z)
}

read_official_csv <- function(path) {
  x <- read.csv(path, check.names = FALSE, encoding = "UTF-8", stringsAsFactors = FALSE)
  names(x)[1] <- sub("^\\ufeff", "", names(x)[1])
  x
}

as_num <- function(x) {
  z <- suppressWarnings(as.numeric(x))
  z[!is.finite(z)] <- NA_real_
  z
}

empty_df <- function(columns) {
  out <- as.data.frame(setNames(replicate(length(columns), character(0), simplify = FALSE), columns), stringsAsFactors = FALSE)
  out
}

write_csv_utf8 <- function(x, path) {
  write.csv(x, path, row.names = FALSE, na = "", fileEncoding = "UTF-8")
}

sanitize_openxlsx_package <- function(path) {
  work <- tempfile("xlsx_sanitize_")
  dir.create(work)
  on.exit(unlink(work, recursive = TRUE, force = TRUE), add = TRUE)
  zip::unzip(path, exdir = work)
  content_types <- file.path(work, "[Content_Types].xml")
  xml <- paste(readLines(content_types, warn = FALSE, encoding = "UTF-8"), collapse = "")
  xml <- gsub("<Override PartName=\"/xl/drawings/drawing[0-9]+\\.xml\"[^>]*/>", "", xml, perl = TRUE)
  writeLines(xml, content_types, useBytes = TRUE)
  rel_files <- list.files(file.path(work, "xl", "worksheets", "_rels"), pattern = "\\.rels$", full.names = TRUE)
  for (rel_file in rel_files) {
    rel <- paste(readLines(rel_file, warn = FALSE, encoding = "UTF-8"), collapse = "")
    rel <- gsub("<Relationship[^>]+Type=\"http://schemas\\.openxmlformats\\.org/officeDocument/2006/relationships/(drawing|vmlDrawing)\"[^>]*/>", "", rel, perl = TRUE)
    writeLines(rel, rel_file, useBytes = TRUE)
  }
  repaired <- tempfile(fileext = ".xlsx")
  on.exit(unlink(repaired, force = TRUE), add = TRUE)
  entries <- list.files(work, all.files = TRUE, no.. = TRUE)
  zip::zipr(repaired, entries, root = work, include_directories = FALSE)
  if (!file.copy(repaired, path, overwrite = TRUE)) stop("Could not finalize sanitized workbook: ", path)
  invisible(path)
}

OUTCOME_SPECS <- data.frame(
  outcome = c(
    "Revenue per Student Real",
    "OPEX per Student Real",
    "CapEx per Student Real",
    "Adjusted Operating Margin (%)",
    "Gross Margin (%)",
    "Cash Flow Margin, Adjusted (%)",
    "Current Debt Service",
    "Capital Expenditures / Depreciation Expense",
    "Available Funds / Total Liabilities (%)",
    "Capital Expenditures / Fixed Assets",
    "Depreciation / Fixed Assets",
    "Asset Turnover",
    "Total Liabilities / Equity",
    "Total Assets / Total Liabilities",
    "Capital Expenditures / Total Enrollment",
    "Total Operating Expense / Total Enrollment",
    "Adjusted Total Operating Revenue / Total Enrollment",
    "Real Operating Revenue per Student - Annual Log Change",
    "Real Operating Expense per Student - Annual Log Change",
    "Real Capital Expenditures per Student - Annual Log Change"
  ),
  source = c(
    "Revenue per Student Real",
    "OPEX per Student Real",
    "CapEx per Student Real",
    "Adjusted Operating Margin (%)",
    "Gross Margin (%)",
    "Cash Flow Margin, Adjusted (%)",
    "Current Debt Service",
    "Capital Expenditures / Depreciation Expense",
    "Available Funds / Total Liabilities (%)",
    "Capital Expenditures / Fixed Assets",
    "Depreciation / Fixed Assets",
    "Asset Turnover",
    "Total Liabilities / Equity",
    "Total Assets / Total Liabilities",
    "Capital Expenditures / Total Enrollment",
    "Total Operating Expense / Total Enrollment",
    "Adjusted Total Operating Revenue / Total Enrollment",
    "Real Operating Revenue per Student - Annual Log Change",
    "Real Operating Expense per Student - Annual Log Change",
    "Real Capital Expenditures per Student - Annual Log Change"
  ),
  transform = c(
    "log", "log", "asinh", "asinh_pct", "asinh_pct", "asinh_pct",
    "asinh", "asinh", "asinh_pct", "asinh", "log", "log", "asinh",
    "log", "asinh", "log", "log", "identity", "identity", "identity"
  ),
  scale = c(
    "log", "log", "asinh", "asinh(ratio/100)", "asinh(ratio/100)",
    "asinh(ratio/100)", "asinh", "asinh", "asinh(ratio/100)", "asinh",
    "log", "log", "asinh", "log", "asinh", "log", "log",
    "real log growth", "real log growth", "real log growth"
  ),
  stringsAsFactors = FALSE
)

TREATED_2016 <- c(
  "Pontificia Universidad Catolica",
  "Pontificia Universidad Catolica de Valparaiso",
  "Universidad Academia De Humanis",
  "Universidad Alberto Hurtado",
  "Universidad Arturo Prat",
  "Universidad Austral De Chile",
  "Universidad Catolica de la Santisima Concepcion",
  "Universidad Catolica de Temuco",
  "Universidad Catolica del Maule",
  "Universidad Catolica del Norte",
  "Universidad De Antofagasta",
  "Universidad De Atacama",
  "Universidad De Chile",
  "Universidad De Concepcion",
  "Universidad De La Frontera",
  "Universidad De La Serena",
  "Universidad De Los Lagos",
  "Universidad De Magallanes",
  "Universidad de Santiago de Chile",
  "Universidad De Talca",
  "Universidad De Tarapaca",
  "Universidad de Valparaiso",
  "Universidad Del Bio-bio",
  "Universidad Diego Portales",
  "Universidad Finis Terrae",
  "Universidad Tecnica Federico Santa Maria",
  "Universidad Tecnologica Metropolitana",
  "Universidad Autonoma de Chile",
  "Universidad Catolica Silva Henriquez",
  "Universidad de Playa Ancha",
  "Universidad Metropolitana de Ciencias de la Educacion",
  "Universidad de O'Higgins"
)

EXCLUDED_MAIN_SAMPLE <- c("Universidad de O'Higgins", "Universidad de Aysen")

EXCLUDED_CLOSED_UNIVERSITIES <- c(
  "Uniacc",
  "Universidad del Pacifico",
  "Universidad de Las Ciencias de la Informatica (UCINF)",
  "Universidad ARCIS",
  "Universidad los Leones"
)

resolve_paths <- function(root) {
  candidates <- list.dirs(root, recursive = FALSE, full.names = TRUE)
  panel_hits <- candidates[startsWith(norm_name(basename(candidates)), "construcci")]
  if (length(panel_hits) != 1L) {
    stop("Expected exactly one project folder beginning with 'Construcci' under root; found ", length(panel_hits))
  }
  did_root <- file.path(root, "Difference-in-Difference Analysis")
  summary_root <- file.path(root, "Summary statistics before and after the reform")
  if (!dir.exists(did_root)) stop("Missing DiD project directory: ", did_root)
  list(
    root = root,
    panel_dir = panel_hits[[1]],
    panel_file = file.path(panel_hits[[1]], "ratios_fitch_panel.csv"),
    mapping_file = file.path(panel_hits[[1]], "university_name_mapping_2.csv"),
    cohort_file = file.path(panel_hits[[1]], "clasif_quintiles_ingresos_normalizado.csv"),
    did_root = did_root,
    diagnostics = file.path(did_root, "Diagnostics", "SDID"),
    event = file.path(did_root, "event", "SDID"),
    plots_dynamic = file.path(did_root, "Plots_Dynamic", "SDID"),
    plots_log = file.path(did_root, "Plots_Log", "SDID"),
    results_main = file.path(did_root, "results"),
    results_aux = file.path(did_root, "results", "SDID"),
    summary = file.path(summary_root, "SDID")
  )
}

validate_inputs <- function(paths) {
  inputs <- c(paths$panel_file, paths$mapping_file, paths$cohort_file)
  missing <- inputs[!file.exists(inputs)]
  if (length(missing)) stop("Missing official input(s):\n", paste(missing, collapse = "\n"))
}

clean_sdid_outputs <- function(paths) {
  targets <- c(paths$diagnostics, paths$event, paths$plots_dynamic, paths$plots_log, paths$results_aux, paths$summary)
  allowed_suffixes <- c(
    "Difference-in-Difference Analysis/Diagnostics/SDID",
    "Difference-in-Difference Analysis/event/SDID",
    "Difference-in-Difference Analysis/Plots_Dynamic/SDID",
    "Difference-in-Difference Analysis/Plots_Log/SDID",
    "Difference-in-Difference Analysis/results/SDID",
    "Summary statistics before and after the reform/SDID"
  )
  root_prefix <- paste0(tolower(normalizePath(paths$root, winslash = "/", mustWork = TRUE)), "/")
  for (i in seq_along(targets)) {
    dir.create(targets[[i]], recursive = TRUE, showWarnings = FALSE)
    resolved <- normalizePath(targets[[i]], winslash = "/", mustWork = TRUE)
    expected <- normalizePath(file.path(paths$root, allowed_suffixes[[i]]), winslash = "/", mustWork = TRUE)
    if (!startsWith(tolower(resolved), root_prefix) || !identical(tolower(resolved), tolower(expected))) {
      stop("Refusing unsafe SDID cleanup target: ", resolved)
    }
    children <- list.files(resolved, all.files = TRUE, full.names = TRUE, no.. = TRUE)
    if (length(children)) unlink(children, recursive = TRUE, force = TRUE)
  }
  dir.create(paths$results_main, recursive = TRUE, showWarnings = FALSE)
  old_main <- list.files(paths$results_main, pattern = "^Table_SDID_", full.names = TRUE)
  if (length(old_main)) unlink(old_main, recursive = FALSE, force = TRUE)
}

build_name_map <- function(mapping_data) {
  if (!"Final_Standard" %in% names(mapping_data)) stop("Mapping file lacks Final_Standard")
  source_columns <- c("Universidad_key", "Universidad_raw", "CNED 2011-2022", "Suggested_Standard", "Final_Standard")
  source_columns <- intersect(source_columns, names(mapping_data))
  out <- character(0)
  for (i in seq_len(nrow(mapping_data))) {
    standard <- trimws(as.character(mapping_data[["Final_Standard"]][i]))
    if (is.na(standard) || !nzchar(standard)) next
    for (column in source_columns) {
      source <- trimws(as.character(mapping_data[[column]][i]))
      if (!is.na(source) && nzchar(source)) out[[norm_name(source)]] <- standard
    }
  }
  out
}

standardize_names <- function(x, name_map) {
  keys <- norm_name(x)
  mapped <- unname(name_map[keys])
  mapped[is.na(mapped) | !nzchar(mapped)] <- as.character(x)[is.na(mapped) | !nzchar(mapped)]
  mapped
}

build_cohort_map <- function(cohort_data, name_map) {
  col_norm <- setNames(names(cohort_data), norm_name(names(cohort_data)))
  university_col <- unname(col_norm[c("universidad", "institucion")])
  university_col <- university_col[!is.na(university_col)][1]
  category_col <- unname(col_norm["categoria"])
  year_col <- unname(col_norm["ano"])
  if (any(is.na(c(university_col, category_col, year_col)))) stop("Cohort file lacks Universidad/Institucion, Categoria or Año")
  keep <- as_num(cohort_data[[category_col]]) == 1
  d <- cohort_data[!is.na(keep) & keep, , drop = FALSE]
  out <- numeric(0)
  for (i in seq_len(nrow(d))) {
    nm <- standardize_names(d[[university_col]][i], name_map)
    key <- norm_name(nm)
    yr <- as_num(d[[year_col]][i])
    if (!nzchar(key) || is.na(yr)) next
    if (length(out[key]) && !is.na(out[key]) && out[key] != yr) stop("Conflicting treatment cohorts for ", nm)
    out[key] <- as.integer(yr)
  }
  for (nm in TREATED_2016) {
    standard <- standardize_names(nm, name_map)
    out[norm_name(standard)] <- TREAT_YEAR
  }
  out
}

apply_pucv_rule <- function(panel) {
  key <- norm_name("Pontificia Universidad Catolica de Valparaiso")
  is_pucv <- panel$id_norm == key & panel$year %in% FIRST_YEAR:LAST_YEAR
  if (!any(is_pucv)) stop("PUCV is not present in the official panel")
  expected <- ifelse(panel$year <= 2021L, 6, 7)
  existing <- as_num(panel[["Acreditación (años)"]])
  conflict <- is_pucv & !is.na(existing) & existing != expected
  if (any(conflict)) {
    bad <- panel[conflict, c("id", "year", "Acreditación (años)"), drop = FALSE]
    stop("PUCV accreditation conflicts with the required chronology:\n", paste(capture.output(print(bad, row.names = FALSE)), collapse = "\n"))
  }
  existing[is_pucv & is.na(existing)] <- expected[is_pucv & is.na(existing)]
  panel[["Acreditación (años)"]] <- existing
  panel
}

create_real_outcomes <- function(panel) {
  inflation <- tapply(as_num(panel[["Inflation_WB"]]), panel$year, median, na.rm = TRUE)
  inflation[!is.finite(inflation)] <- NA_real_
  if (is.na(inflation[as.character(BASE_YEAR)])) stop("Inflation_WB lacks a valid 2015 value")
  pi <- setNames(rep(NA_real_, LAST_YEAR - FIRST_YEAR + 1L), FIRST_YEAR:LAST_YEAR)
  pi[as.character(BASE_YEAR)] <- 1
  for (year in (BASE_YEAR + 1L):LAST_YEAR) {
    inf <- inflation[as.character(year)]
    if (is.na(inf) || inf <= -1 || is.na(pi[as.character(year - 1L)])) next
    pi[as.character(year)] <- pi[as.character(year - 1L)] * (1 + inf)
  }
  for (year in seq(BASE_YEAR - 1L, FIRST_YEAR, by = -1L)) {
    inf_next <- inflation[as.character(year + 1L)]
    if (is.na(inf_next) || inf_next <= -1 || is.na(pi[as.character(year + 1L)])) next
    pi[as.character(year)] <- pi[as.character(year + 1L)] / (1 + inf_next)
  }
  panel$price_index_2015 <- unname(pi[as.character(panel$year)])
  panel[["Revenue per Student Real"]] <- as_num(panel[["Adjusted Total Operating Revenue / Total Enrollment"]]) / panel$price_index_2015
  panel[["OPEX per Student Real"]] <- as_num(panel[["Total Operating Expense / Total Enrollment"]]) / panel$price_index_2015
  panel[["CapEx per Student Real"]] <- as_num(panel[["Capital Expenditures / Total Enrollment"]]) / panel$price_index_2015
  panel
}

load_analysis_data <- function(paths) {
  mapping_data <- read_official_csv(paths$mapping_file)
  name_map <- build_name_map(mapping_data)
  cohort_map <- build_cohort_map(read_official_csv(paths$cohort_file), name_map)
  panel <- read_official_csv(paths$panel_file)
  required <- c("Universidad_std", "Año", "Acreditación (años)", "Inflation_WB", OUTCOME_SPECS$source[!(OUTCOME_SPECS$source %in% c("Revenue per Student Real", "OPEX per Student Real", "CapEx per Student Real"))])
  missing <- setdiff(required, names(panel))
  if (length(missing)) stop("Official panel lacks required column(s): ", paste(missing, collapse = ", "))
  panel$id <- standardize_names(panel[["Universidad_std"]], name_map)
  panel$id_norm <- norm_name(panel$id)
  panel$year <- as.integer(as_num(panel[["Año"]]))
  panel <- panel[!is.na(panel$year) & panel$year >= FIRST_YEAR & panel$year <= LAST_YEAR, , drop = FALSE]
  if (anyDuplicated(panel[c("id_norm", "year")])) stop("Duplicate standardized university-year rows in official panel")
  panel <- apply_pucv_rule(panel)
  panel <- create_real_outcomes(panel)
  panel$cohort <- unname(cohort_map[panel$id_norm])
  excluded <- norm_name(standardize_names(EXCLUDED_MAIN_SAMPLE, name_map))
  panel <- panel[!panel$id_norm %in% excluded & (is.na(panel$cohort) | panel$cohort == TREAT_YEAR), , drop = FALSE]
  panel$treated <- ifelse(is.na(panel$cohort), 0L, as.integer(panel$cohort == TREAT_YEAR))
  list(panel = panel, name_map = name_map, cohort_map = cohort_map)
}

transform_values <- function(y, mode) {
  y <- as_num(y)
  invalid <- 0L
  if (mode == "identity") return(list(value = y, invalid = invalid))
  if (mode == "log") {
    valid <- !is.na(y) & y > 0
    invalid <- sum(!is.na(y) & !valid)
    out <- rep(NA_real_, length(y)); out[valid] <- log(y[valid])
    return(list(value = out, invalid = invalid))
  }
  if (mode == "asinh") return(list(value = asinh(y), invalid = invalid))
  if (mode == "asinh_pct") return(list(value = asinh(y / 100), invalid = invalid))
  stop("Unknown transformation: ", mode)
}

build_balanced_panel <- function(panel, source, transformed, transform_mode) {
  value <- as_num(panel[[source]])
  invalid <- 0L
  if (transformed) {
    transformed_values <- transform_values(value, transform_mode)
    value <- transformed_values$value
    invalid <- transformed_values$invalid
  }
  d <- data.frame(id = panel$id, id_norm = panel$id_norm, year = panel$year,
                  treated = panel$treated, cohort = panel$cohort, value = value,
                  stringsAsFactors = FALSE)
  for (first_year in FIRST_YEAR:(TREAT_YEAR - 3L)) {
    years <- first_year:LAST_YEAR
    use <- d[d$year %in% years, , drop = FALSE]
    complete_units <- vapply(split(use, use$id_norm), function(z) {
      nrow(z) == length(years) && length(unique(z$year)) == length(years) && all(is.finite(z$value))
    }, logical(1))
    eligible <- names(complete_units)[complete_units]
    unit_info <- unique(use[use$id_norm %in% eligible, c("id", "id_norm", "treated"), drop = FALSE])
    controls <- unit_info$id_norm[unit_info$treated == 0]
    treated <- unit_info$id_norm[unit_info$treated == 1]
    if (length(controls) < 2L || length(treated) < 2L || sum(years < TREAT_YEAR) < 3L || sum(years >= TREAT_YEAR) < 1L) next
    controls <- controls[order(unit_info$id[match(controls, unit_info$id_norm)])]
    treated <- treated[order(unit_info$id[match(treated, unit_info$id_norm)])]
    units <- c(controls, treated)
    Y <- matrix(NA_real_, nrow = length(units), ncol = length(years),
                dimnames = list(unit_info$id[match(units, unit_info$id_norm)], as.character(years)))
    for (i in seq_along(units)) {
      z <- use[use$id_norm == units[[i]], c("year", "value"), drop = FALSE]
      Y[i, match(z$year, years)] <- z$value
    }
    if (any(!is.finite(Y))) stop("Internal error: balanced SDID matrix contains missing values")
    return(list(Y = Y, years = years, first_year = first_year, last_year = LAST_YEAR,
                N0 = length(controls), N1 = length(treated), T0 = sum(years < TREAT_YEAR),
                T1 = sum(years >= TREAT_YEAR), invalid_transform = invalid))
  }
  stop("No balanced panel satisfies >=3 pre periods, >=2 controls, >=2 treated and >=1 post period")
}

extract_paths <- function(estimate) {
  setup <- attr(estimate, "setup")
  weights <- attr(estimate, "weights")
  Y <- setup$Y; N0 <- setup$N0; T0 <- setup$T0
  N1 <- nrow(Y) - N0
  omega <- as.numeric(weights$omega)
  lambda <- as.numeric(weights$lambda)
  treated_path <- colMeans(Y[(N0 + 1L):nrow(Y), , drop = FALSE])
  synthetic_raw <- as.numeric(crossprod(omega, Y[seq_len(N0), , drop = FALSE]))
  pre_offset <- sum(lambda * (treated_path[seq_len(T0)] - synthetic_raw[seq_len(T0)]))
  synthetic_adjusted <- synthetic_raw + pre_offset
  gap <- treated_path - synthetic_adjusted
  post_curve <- as.numeric(synthdid::synthdid_effect_curve(estimate))
  effect <- c(gap[seq_len(T0)], post_curve)
  list(omega = omega, lambda = lambda, treated_path = treated_path,
       synthetic_raw = synthetic_raw, synthetic_adjusted = synthetic_adjusted,
       gap = gap, effect = effect, pre_offset = pre_offset,
       N1 = N1)
}

bootstrap_one_draw <- function(draw_seed, estimate) {
  setup <- attr(estimate, "setup")
  opts <- attr(estimate, "opts")
  original_weights <- attr(estimate, "weights")
  Y <- setup$Y; N0 <- setup$N0; T0 <- setup$T0; n <- nrow(Y); T <- ncol(Y)
  set.seed(draw_seed)
  for (attempts in seq_len(100L)) {
    ind <- sort(sample.int(n, n, replace = TRUE))
    n0b <- sum(ind <= N0)
    if (n0b < 2L || (n - n0b) < 2L) next
    omega_start <- original_weights$omega[ind[ind <= N0]]
    if (!all(is.finite(omega_start)) || sum(omega_start) <= 0) next
    warm <- original_weights
    warm$omega <- as.numeric(omega_start / sum(omega_start))
    eb <- try(do.call(synthdid::synthdid_estimate,
                      c(list(Y = Y[ind, , drop = FALSE], N0 = n0b, T0 = T0,
                             weights = warm), opts)), silent = TRUE)
    if (inherits(eb, "try-error") || !is.finite(as.numeric(eb))) next
    wb <- attr(eb, "weights")
    Yb <- attr(eb, "setup")$Y
    treated_path <- colMeans(Yb[(n0b + 1L):nrow(Yb), , drop = FALSE])
    synthetic_raw <- as.numeric(crossprod(as.numeric(wb$omega), Yb[seq_len(n0b), , drop = FALSE]))
    pre_offset <- sum(as.numeric(wb$lambda) * (treated_path[seq_len(T0)] - synthetic_raw[seq_len(T0)]))
    gap <- treated_path - (synthetic_raw + pre_offset)
    curve <- c(gap[seq_len(T0)], as.numeric(synthdid::synthdid_effect_curve(eb)))
    if (length(curve) != T || any(!is.finite(curve))) next
    return(list(att = as.numeric(eb), curve = curve, attempts = attempts))
  }
  list(att = NA_real_, curve = rep(NA_real_, T), attempts = 100L)
}

bootstrap_sdid <- function(estimate, replications, seed, cluster = NULL) {
  setup <- attr(estimate, "setup")
  T <- ncol(setup$Y)
  draw_seeds <- seed + seq_len(replications) * 104729L
  if (!is.null(cluster)) {
    draws <- parallel::parLapplyLB(cluster, draw_seeds, bootstrap_one_draw, estimate = estimate)
  } else {
    draws <- lapply(draw_seeds, bootstrap_one_draw, estimate = estimate)
  }
  valid <- vapply(draws, function(x) is.finite(x$att) && length(x$curve) == T && all(is.finite(x$curve)), logical(1))
  draws <- draws[valid]
  att_draws <- vapply(draws, function(x) x$att, numeric(1))
  curve_draws <- if (length(draws)) do.call(rbind, lapply(draws, function(x) x$curve)) else matrix(numeric(0), 0L, T)
  attempts <- sum(vapply(draws, function(x) x$attempts, integer(1)))
  correction <- if (length(att_draws) > 1L) sqrt((length(att_draws) - 1) / length(att_draws)) else NA_real_
  list(
    se = if (length(att_draws) > 1L) correction * sd(att_draws) else NA_real_,
    curve_se = if (nrow(curve_draws) > 1L) correction * apply(curve_draws, 2, sd) else rep(NA_real_, T),
    valid = length(att_draws), attempts = attempts
  )
}

ess <- function(w) {
  w <- as.numeric(w)
  if (!length(w) || sum(w^2) <= 0) return(NA_real_)
  sum(w)^2 / sum(w^2)
}

support_warning <- function(sample) {
  warnings <- character(0)
  if (sample$N1 >= sample$N0) warnings <- c(warnings, "N1>=N0")
  if (sample$N1 >= sqrt(sample$N0)) warnings <- c(warnings, "N1>=sqrt(N0)")
  if (sample$T0 < 5L) warnings <- c(warnings, "T0<5")
  if (sample$first_year > FIRST_YEAR) warnings <- c(warnings, paste0("panel_starts_", sample$first_year))
  if (sample$N0 <= sample$N1) warnings <- c(warnings, "placebo_SE_not_available_when_N0<=N1")
  if (!length(warnings)) "none" else paste(warnings, collapse = "; ")
}

normal_inference <- function(coef, se) {
  if (!is.finite(se) || se <= 0) return(list(p = NA_real_, low = NA_real_, high = NA_real_))
  z <- coef / se
  list(p = 2 * pnorm(-abs(z)), low = coef - qnorm(0.975) * se, high = coef + qnorm(0.975) * se)
}

fit_one_sdid <- function(panel, spec, transformed, boot_reps, seed, cluster = NULL) {
  scale <- if (transformed) spec$scale else "level"
  sample <- build_balanced_panel(panel, spec$source, transformed, spec$transform)
  estimate <- synthdid::synthdid_estimate(sample$Y, sample$N0, sample$T0)
  coef <- as.numeric(estimate)
  paths <- extract_paths(estimate)
  boot <- bootstrap_sdid(estimate, boot_reps, seed, cluster)
  bootstrap_se <- boot$se
  jackknife_se <- tryCatch(sqrt(as.numeric(stats::vcov(estimate, method = "jackknife"))), error = function(e) NA_real_)
  primary_se <- if (is.finite(bootstrap_se) && bootstrap_se > 0) bootstrap_se else jackknife_se
  inference_method <- if (is.finite(bootstrap_se) && bootstrap_se > 0) "bootstrap_by_university" else if (is.finite(jackknife_se) && jackknife_se > 0) "jackknife_fallback" else "unavailable"
  inf <- normal_inference(coef, primary_se)
  list(outcome = spec$outcome, scale = scale, transformed = transformed,
       sample = sample, estimate = estimate, coef = coef, paths = paths, boot = boot,
       Bootstrap_SE = bootstrap_se, Jackknife_SE = jackknife_se, Primary_SE = primary_se,
       Inference = inference_method, CI_Low = inf$low, CI_High = inf$high, P_value = inf$p)
}

event_frame <- function(fit) {
  years <- fit$sample$years
  event_time <- years - TREAT_YEAR
  se <- fit$boot$curve_se
  low <- fit$paths$effect - qnorm(0.975) * se
  high <- fit$paths$effect + qnorm(0.975) * se
  status <- ifelse(event_time < 0, "pre_fit_gap", ifelse(is.finite(se), "ok", "inference_unavailable"))
  n <- length(years)
  out <- data.frame(
    year = years,
    event_time = event_time,
    att = fit$paths$effect,
    att_enrollment_only = rep(NA_real_, n),
    att_accreditation_ge4 = rep(NA_real_, n),
    beta_dlog_enrollment = rep(NA_real_, n),
    beta_deligibility_proxy_10 = rep(NA_real_, n),
    beta_daccreditation_years = rep(NA_real_, n),
    beta_deligibility_coverage_10 = rep(NA_real_, n),
    n_treated = rep(fit$sample$N1, n),
    n_control = rep(fit$sample$N0, n),
    n_treated_enrollment_only = rep(NA_integer_, n),
    n_control_enrollment_only = rep(NA_integer_, n),
    n_treated_accreditation_ge4 = rep(NA_integer_, n),
    n_control_accreditation_ge4 = rep(NA_integer_, n),
    status = status,
    status_enrollment_only = rep("not_applicable", n),
    status_accreditation_ge4 = rep("not_applicable", n),
    se_boot = se,
    ci_low = low,
    ci_high = high,
    boot_reps_valid = rep(fit$boot$valid, n),
    stringsAsFactors = FALSE
  )
  if (fit$transformed) {
    out$transformation <- rep(fit$scale, n)
    if (fit$scale == "log") {
      out$att_percent_exact <- 100 * (exp(out$att) - 1)
      out$ci_low_percent_exact <- 100 * (exp(out$ci_low) - 1)
      out$ci_high_percent_exact <- 100 * (exp(out$ci_high) - 1)
    }
  }
  out
}

summary_frame <- function(fit) {
  data.frame(
    year = fit$sample$years,
    Treated = fit$paths$treated_path,
    Synthetic_Control = fit$paths$synthetic_adjusted,
    Control_Mean = colMeans(fit$sample$Y[seq_len(fit$sample$N0), , drop = FALSE]),
    period = ifelse(fit$sample$years < TREAT_YEAR, "pre", "post"),
    stringsAsFactors = FALSE
  )
}

plot_paths <- function(fit, path) {
  png(path, width = 1800, height = 1100, res = 180)
  on.exit(dev.off(), add = TRUE)
  yr <- fit$sample$years
  ylim <- range(c(fit$paths$treated_path, fit$paths$synthetic_adjusted), finite = TRUE)
  plot(yr, fit$paths$treated_path, type = "o", pch = 16, lwd = 2, col = "#1f4e79",
       xlab = "Year", ylab = fit$outcome, main = paste(fit$outcome, "- Treated vs synthetic"), ylim = ylim)
  lines(yr, fit$paths$synthetic_adjusted, type = "o", pch = 17, lwd = 2, col = "#c55a11")
  abline(v = TREAT_YEAR, lty = 2, col = "gray40")
  legend("topleft", c("Treated", "Synthetic control"), col = c("#1f4e79", "#c55a11"),
         lty = 1, pch = c(16, 17), bty = "n")
  grid(col = "gray90")
}

plot_event <- function(fit, event, path) {
  png(path, width = 1800, height = 1100, res = 180)
  on.exit(dev.off(), add = TRUE)
  ylim <- range(c(event$att, event$ci_low, event$ci_high), finite = TRUE)
  plot(event$event_time, event$att, type = "o", pch = 16, lwd = 2, col = "#1f4e79",
       xlab = "Event time (2016 = 0)", ylab = "Pre-fit gap / post-treatment effect",
       main = paste(fit$outcome, "- SDID dynamics"), ylim = ylim)
  arrows(event$event_time, event$ci_low, event$event_time, event$ci_high,
         angle = 90, code = 3, length = 0.04, col = "#1f4e79")
  abline(h = 0, col = "gray40")
  abline(v = 0, lty = 2, col = "#c55a11")
  grid(col = "gray90")
}

plot_weights <- function(fit, folder, stem) {
  unit_path <- file.path(folder, paste0("unit_weights_", stem, ".png"))
  png(unit_path, width = 1800, height = 1200, res = 180)
  old <- par(mar = c(10, 5, 4, 2) + 0.1); on.exit(par(old), add = TRUE)
  barplot(fit$paths$omega, names.arg = rownames(fit$sample$Y)[seq_len(fit$sample$N0)],
          las = 2, col = "#5b9bd5", ylab = "University weight", main = paste(fit$outcome, "- donor weights"))
  grid(nx = NA, ny = NULL, col = "gray90")
  dev.off(); on.exit(NULL, add = FALSE)
  time_path <- file.path(folder, paste0("time_weights_", stem, ".png"))
  png(time_path, width = 1500, height = 900, res = 180)
  barplot(fit$paths$lambda, names.arg = fit$sample$years[seq_len(fit$sample$T0)],
          col = "#70ad47", xlab = "Pre-treatment year", ylab = "Time weight",
          main = paste(fit$outcome, "- pre-treatment time weights"))
  grid(nx = NA, ny = NULL, col = "gray90")
  dev.off()
  c(unit_path, time_path)
}

fit_diagnostics_row <- function(fit) {
  T0 <- fit$sample$T0
  pre_raw <- fit$paths$treated_path[seq_len(T0)] - fit$paths$synthetic_raw[seq_len(T0)]
  pre_adj <- fit$paths$gap[seq_len(T0)]
  w <- fit$paths$omega; l <- fit$paths$lambda
  data.frame(
    Outcome = fit$outcome, Scale = fit$scale,
    First_Year = fit$sample$first_year, Last_Year = fit$sample$last_year,
    N0 = fit$sample$N0, N1 = fit$sample$N1, T0 = fit$sample$T0, T1 = fit$sample$T1,
    Pre_RMSPE_Adjusted = sqrt(mean(pre_adj^2)), Pre_RMSPE_Raw = sqrt(mean(pre_raw^2)),
    Pre_MAE_Adjusted = mean(abs(pre_adj)), Pre_Max_Abs_Gap = max(abs(pre_adj)),
    Pre_Level_Offset = fit$paths$pre_offset,
    Control_ESS = ess(w), Control_ESS_Share = ess(w) / fit$sample$N0,
    Max_Control_Weight = max(w), Positive_Control_Weights = sum(w > 0),
    Control_Weight_HHI = sum(w^2), Time_ESS = ess(l), Time_ESS_Share = ess(l) / fit$sample$T0,
    Max_Time_Weight = max(l), Positive_Time_Weights = sum(l > 0),
    ATT_Effect_Curve_Check = mean(fit$paths$effect[(T0 + 1L):length(fit$paths$effect)]) - fit$coef,
    Support_Warning = support_warning(fit$sample), Bootstrap_SE = fit$Bootstrap_SE,
    Jackknife_SE = fit$Jackknife_SE, Primary_SE = fit$Primary_SE,
    Inference = fit$Inference, CI_Low = fit$CI_Low, CI_High = fit$CI_High,
    P_value = fit$P_value, Bootstrap_Replications_Valid = fit$boot$valid,
    stringsAsFactors = FALSE
  )
}

unit_weights_frame <- function(fit) {
  data.frame(Outcome = fit$outcome, Scale = fit$scale,
             University = rownames(fit$sample$Y)[seq_len(fit$sample$N0)],
             Weight = fit$paths$omega, Rank = rank(-fit$paths$omega, ties.method = "first"),
             stringsAsFactors = FALSE)
}

time_weights_frame <- function(fit) {
  data.frame(Outcome = fit$outcome, Scale = fit$scale,
             Year = fit$sample$years[seq_len(fit$sample$T0)], Weight = fit$paths$lambda,
             stringsAsFactors = FALSE)
}

leave_one_out_frame <- function(fit) {
  order_idx <- order(fit$paths$omega, decreasing = TRUE)
  order_idx <- order_idx[fit$paths$omega[order_idx] > 0]
  order_idx <- head(order_idx, 5L)
  if (!length(order_idx)) return(empty_df(c("Outcome", "Scale", "Donor_Rank", "Removed_Donor", "Original_Weight", "Main_ATT", "Leave_One_Out_ATT", "Delta_ATT", "Relative_Delta", "Status")))
  rows <- vector("list", length(order_idx))
  for (j in seq_along(order_idx)) {
    idx <- order_idx[[j]]
    loo <- NA_real_; status <- "not_estimable"
    if (fit$sample$N0 - 1L >= 2L) {
      Yloo <- fit$sample$Y[-idx, , drop = FALSE]
      est <- try(synthdid::synthdid_estimate(Yloo, fit$sample$N0 - 1L, fit$sample$T0), silent = TRUE)
      if (!inherits(est, "try-error") && is.finite(as.numeric(est))) { loo <- as.numeric(est); status <- "ok" }
    }
    delta <- loo - fit$coef
    rows[[j]] <- data.frame(Outcome = fit$outcome, Scale = fit$scale, Donor_Rank = j,
                            Removed_Donor = rownames(fit$sample$Y)[idx], Original_Weight = fit$paths$omega[idx],
                            Main_ATT = fit$coef, Leave_One_Out_ATT = loo, Delta_ATT = delta,
                            Relative_Delta = if (is.finite(fit$coef) && fit$coef != 0) delta / abs(fit$coef) else NA_real_,
                            Status = status, stringsAsFactors = FALSE)
  }
  do.call(rbind, rows)
}

time_placebo_frame <- function(fit) {
  base <- data.frame(Outcome = fit$outcome, Scale = fit$scale, Pseudo_Treatment_Year = NA_integer_,
                     Placebo_ATT = NA_real_, Placebo_SE = NA_real_, Placebo_P_Value = NA_real_,
                     Placebo_T0 = NA_integer_, Placebo_T1 = NA_integer_, Status = "not_feasible",
                     stringsAsFactors = FALSE)
  placebo <- try(synthdid::synthdid_placebo(fit$estimate), silent = TRUE)
  if (inherits(placebo, "try-error") || !is.finite(as.numeric(placebo))) return(base)
  ps <- attr(placebo, "setup")
  pse <- if (ps$N0 > (nrow(ps$Y) - ps$N0)) {
    tryCatch(sqrt(as.numeric(stats::vcov(placebo, method = "jackknife"))), error = function(e) NA_real_)
  } else NA_real_
  pinf <- normal_inference(as.numeric(placebo), pse)
  base$Pseudo_Treatment_Year <- fit$sample$years[ps$T0 + 1L]
  base$Placebo_ATT <- as.numeric(placebo); base$Placebo_SE <- pse; base$Placebo_P_Value <- pinf$p
  base$Placebo_T0 <- ps$T0; base$Placebo_T1 <- ncol(ps$Y) - ps$T0
  base$Status <- if (is.finite(pse)) "ok" else "inference_unavailable"
  base
}

stars <- function(p) {
  if (!is.finite(p)) return("")
  if (p < 0.01) return("***")
  if (p < 0.05) return("**")
  if (p < 0.10) return("*")
  ""
}

make_paper_table <- function(raw_results) {
  rows <- list(); k <- 0L
  sample_columns <- c(
    original = "(1) With closure-risk controls",
    restricted = "(2) Excluding closure-risk controls"
  )
  for (outcome in OUTCOME_SPECS$outcome) {
    block <- data.frame(
      `Outcome / statistic` = c(outcome, "(SE)", "Observations", "Adjusted R2"),
      check.names = FALSE, stringsAsFactors = FALSE
    )
    for (sample_key in names(sample_columns)) {
      r <- raw_results[
        raw_results$outcome == outcome & raw_results$sample == sample_key,
        , drop = FALSE
      ]
      if (nrow(r) != 1L) stop("Expected one result for ", outcome, " / ", sample_key)
      ok_coef <- is.finite(r$coef)
      ok_se <- is.finite(r$se)
      coef_text <- if (ok_coef) paste0(sprintf("%.3f", r$coef), stars(r$pvalue)) else "--"
      se_text <- if (ok_se) paste0("(", sprintf("%.3f", r$se), ")") else ""
      n_text <- if (is.finite(r$nobs) && r$nobs > 0) as.character(as.integer(r$nobs)) else "--"
      block[[sample_columns[[sample_key]]]] <- c(coef_text, se_text, n_text, "--")
    }
    k <- k + 1L; rows[[k]] <- block
  }
  footer <- data.frame(
    `Outcome / statistic` = c("University weights", "Time weights", "Controls", "SE bootstrap by university"),
    `(1) With closure-risk controls` = c("Yes", "Yes", "No", "Yes"),
    `(2) Excluding closure-risk controls` = c("Yes", "Yes", "No", "Yes"),
    check.names = FALSE, stringsAsFactors = FALSE
  )
  rows[[length(rows) + 1L]] <- footer
  do.call(rbind, rows)
}

write_main_workbook <- function(raw_results, path) {
  raw_results <- raw_results[c("outcome", "scale", "sample", "sample_label",
                               "specification", "specification_label", "status",
                               "coef", "se", "pvalue", "ci_low", "ci_high", "nobs", "n_clusters", "adj_r2")]
  paper <- make_paper_table(raw_results)
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "paper_table")
  openxlsx::addWorksheet(wb, "raw_results")
  header_style <- openxlsx::createStyle(textDecoration = "bold", fgFill = "#D9EAF7", border = "bottom")
  openxlsx::writeData(wb, "paper_table", paper, rowNames = FALSE, keepNA = FALSE, headerStyle = header_style)
  openxlsx::writeData(wb, "raw_results", raw_results, rowNames = FALSE, keepNA = FALSE, headerStyle = header_style)
  openxlsx::freezePane(wb, "paper_table", firstRow = TRUE)
  openxlsx::freezePane(wb, "raw_results", firstRow = TRUE)
  openxlsx::setColWidths(wb, "paper_table", cols = 1:3, widths = "auto")
  openxlsx::setColWidths(wb, "raw_results", cols = 1:15, widths = "auto")
  num_style <- openxlsx::createStyle(numFmt = "0.000000")
  int_style <- openxlsx::createStyle(numFmt = "0")
  if (nrow(raw_results)) {
    openxlsx::addStyle(wb, "raw_results", num_style, rows = 2:(nrow(raw_results) + 1L), cols = c(8:12, 15), gridExpand = TRUE)
    openxlsx::addStyle(wb, "raw_results", int_style, rows = 2:(nrow(raw_results) + 1L), cols = 13:14, gridExpand = TRUE)
  }
  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
  sanitize_openxlsx_package(path)
  invisible(paper)
}

latex_escape <- function(x) {
  x <- gsub("\\\\", "\\\\textbackslash{}", as.character(x))
  x <- gsub("([&%$#_{}])", "\\\\\\1", x, perl = TRUE)
  x
}

write_latex_table <- function(paper, path, transformed) {
  caption <- if (transformed) "Synthetic Difference-in-Differences Estimates (Transformed Outcomes)" else "Synthetic Difference-in-Differences Estimates (Levels)"
  con <- file(path, open = "wt", encoding = "UTF-8")
  on.exit(close(con), add = TRUE)
  writeLines(c(
    "\\begin{table}[!htbp]", "\\centering", paste0("\\caption{", caption, "}"),
    paste0("\\label{tab:sdid_", if (transformed) "transformed" else "levels", "}"),
    "\\begin{tabular}{lcc}", "\\toprule", "Outcome / statistic & With closure-risk controls & Excluding closure-risk controls \\\\", "\\midrule"
  ), con)
  for (i in seq_len(nrow(paper))) {
    writeLines(paste0(latex_escape(paper[[1]][i]), " & ", latex_escape(paper[[2]][i]), " & ", latex_escape(paper[[3]][i]), " \\\\"), con)
  }
  writeLines(c(
    "\\bottomrule", "\\end{tabular}", "\\begin{minipage}{0.96\\textwidth}",
    "\\footnotesize \\textit{Notes:} Estimator = Synthetic Difference-in-Differences. The restricted column excludes UNIACC, Universidad del Pacífico, UCINF, ARCIS and Universidad de Los Leones. Both columns exclude Universidad de Aysén and Universidad de O'Higgins. The estimator uses university weights and pre-treatment time weights. Standard errors are obtained by bootstrap at the university level, with jackknife fallback when the bootstrap is unavailable. Adjusted $R^2$ is not defined for SDID. $^{***}p<0.01$, $^{**}p<0.05$, $^{*}p<0.10$.",
    "\\end{minipage}", "\\end{table}"
  ), con)
}

write_diagnostics_workbook <- function(tables, path) {
  sheet_map <- c(Sample_Audit = "sample_audit", Fit_Diagnostics = "fit_diagnostics",
                 Unit_Weights = "unit_weights", Time_Weights = "time_weights",
                 Time_Placebo = "time_placebo", Leave_One_Out = "leave_one_out",
                 Transformations = "transformations", Errors = "errors")
  wb <- openxlsx::createWorkbook()
  header_style <- openxlsx::createStyle(textDecoration = "bold", fgFill = "#D9EAF7", border = "bottom")
  for (sheet in names(sheet_map)) {
    dat <- tables[[sheet_map[[sheet]]]]
    openxlsx::addWorksheet(wb, sheet)
    openxlsx::writeData(wb, sheet, dat, rowNames = FALSE, keepNA = FALSE, headerStyle = header_style)
    openxlsx::freezePane(wb, sheet, firstRow = TRUE)
    if (ncol(dat)) openxlsx::setColWidths(wb, sheet, cols = seq_len(ncol(dat)), widths = "auto")
  }
  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
  sanitize_openxlsx_package(path)
}

validate_main_workbook <- function(path, expected_scale, results_aux) {
  if (!file.exists(path)) stop("Missing main workbook: ", path)
  sheets <- openxlsx::getSheetNames(path)
  if (!identical(sheets, c("paper_table", "raw_results"))) stop("Workbook sheet order/names invalid: ", path)
  paper_all <- openxlsx::read.xlsx(path, sheet = "paper_table", colNames = FALSE, skipEmptyRows = FALSE)
  paper_headers <- as.character(unlist(paper_all[1, , drop = TRUE], use.names = FALSE))
  paper <- paper_all[-1, , drop = FALSE]
  names(paper) <- paper_headers
  raw <- openxlsx::read.xlsx(path, sheet = "raw_results", check.names = FALSE, skipEmptyRows = FALSE)
  expected_headers <- c("Outcome / statistic", "(1) With closure-risk controls", "(2) Excluding closure-risk controls")
  if (!identical(paper_headers, expected_headers) || ncol(paper) != 3L) stop("paper_table columns invalid: ", path)
  raw_cols <- c("outcome", "scale", "sample", "sample_label", "specification", "specification_label", "status", "coef", "se", "pvalue", "ci_low", "ci_high", "nobs", "n_clusters", "adj_r2")
  if (!identical(names(raw), raw_cols) || ncol(raw) != 15L) stop("raw_results columns invalid: ", path)
  if (nrow(raw) != nrow(OUTCOME_SPECS) * 2L) stop("raw_results must contain both samples for all 20 outcomes: ", path)
  if (!setequal(unique(raw$sample), c("original", "restricted"))) stop("raw_results sample labels invalid: ", path)
  if (any(raw$specification != "sdid") || any(raw$specification_label != "(1) SDID")) stop("SDID specification labels invalid: ", path)
  if (expected_scale == "level" && any(raw$scale != "level")) stop("Levels workbook has invalid scale")
  if (any(!is.na(raw$adj_r2))) stop("adj_r2 must be blank/NA for SDID: ", path)
  if (nrow(paper) != nrow(OUTCOME_SPECS) * 4L + 4L) stop("paper_table row count invalid: ", path)
  for (i in seq_along(OUTCOME_SPECS$outcome)) {
    idx <- (i - 1L) * 4L + 1L
    expected <- c(OUTCOME_SPECS$outcome[[i]], "(SE)", "Observations", "Adjusted R2")
    if (!identical(paper[[1]][idx:(idx + 3L)], expected)) stop("paper_table block invalid for ", OUTCOME_SPECS$outcome[[i]])
    if (!identical(as.character(paper[[2]][idx + 3L]), "--") ||
        !identical(as.character(paper[[3]][idx + 3L]), "--")) stop("Adjusted R2 must be -- in paper_table")
  }
  footer <- tail(paper, 4L)
  expected_footer <- c("Yes", "Yes", "No", "Yes")
  if (!identical(footer[[1]], c("University weights", "Time weights", "Controls", "SE bootstrap by university")) ||
      !identical(footer[[2]], expected_footer) || !identical(footer[[3]], expected_footer)) stop("paper_table footer invalid: ", path)
  forbidden <- list.files(results_aux, pattern = "^Table_SDID_Main_.*\\.xlsx$", full.names = TRUE)
  if (length(forbidden)) stop("Main SDID workbook incorrectly written inside results/SDID: ", paste(forbidden, collapse = ", "))
  invisible(TRUE)
}

bind_rows <- function(rows, columns = NULL) {
  if (!length(rows)) return(if (is.null(columns)) data.frame() else empty_df(columns))
  all_columns <- unique(unlist(lapply(rows, names), use.names = FALSE))
  if (!is.null(columns)) all_columns <- unique(c(columns, all_columns))
  rows <- lapply(rows, function(x) {
    missing <- setdiff(all_columns, names(x))
    for (column in missing) x[[column]] <- NA
    x[all_columns]
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

write_method_readme <- function(path, boot_reps, seed) {
  lines <- c(
    "Synthetic Difference-in-Differences (SDID) pipeline",
    "",
    "Estimator: synthdid::synthdid_estimate().",
    "Sample: universities first treated in 2016 and never-treated donors only; later-treated universities are excluded.",
    "Both table columns exclude Universidad de O'Higgins and Universidad de Aysen.",
    "The restricted column additionally excludes UNIACC, Universidad del Pacifico, UCINF, ARCIS and Universidad de Los Leones.",
    "Matrices are balanced separately by outcome. Controls are ordered first and treated universities last.",
    "The longest feasible 2011-2022 window is used, allowing a later first year only when required.",
    "Pre-2016 dynamic values are synthetic pre-treatment fit gaps, not causal ATT leads.",
    "Post-2016 dynamic values are SDID effect-curve estimates.",
    paste0("Inference: university-level bootstrap (", boot_reps, " valid replications requested; seed ", seed, ") with jackknife fallback."),
    "Adjusted R-squared is not defined for SDID and is intentionally blank in raw_results and shown as -- in paper_table.",
    "Time placebo uses synthdid::synthdid_placebo(), which uses only the pre-treatment portion of the fitted panel.",
    "No missing outcome value is imputed. Non-positive values become missing before log transformation.",
    "Main tables are written directly to Difference-in-Difference Analysis/results. Diagnostics remain in SDID subdirectories."
  )
  writeLines(lines, path, useBytes = TRUE)
}

main <- function() {
  args <- parse_args()
  paths <- resolve_paths(args$root)
  validate_inputs(paths)
  clean_sdid_outputs(paths)
  loaded <- load_analysis_data(paths)
  panel <- loaded$panel
  excluded_closed <- norm_name(standardize_names(EXCLUDED_CLOSED_UNIVERSITIES, loaded$name_map))
  sample_panels <- list(
    original = panel,
    restricted = panel[!panel$id_norm %in% excluded_closed, , drop = FALSE]
  )
  sample_labels <- c(
    original = "With closure-risk controls",
    restricted = "Excluding closure-risk controls"
  )
  cluster <- NULL
  if (args$workers > 1L) {
    cluster <- parallel::makeCluster(args$workers)
    on.exit(parallel::stopCluster(cluster), add = TRUE)
  }

  raw_level_rows <- list(); raw_trans_rows <- list()
  sample_rows <- list(); fit_rows <- list(); unit_rows <- list(); time_rows <- list()
  placebo_rows <- list(); loo_rows <- list(); transform_rows <- list(); error_rows <- list(); dynamic_rows <- list()

  for (sample_key in names(sample_panels)) {
    panel_sample <- sample_panels[[sample_key]]
    sample_label <- unname(sample_labels[[sample_key]])
    primary_sample <- identical(sample_key, "restricted")
    for (transformed in c(FALSE, TRUE)) {
      for (i in seq_len(nrow(OUTCOME_SPECS))) {
      spec <- as.list(OUTCOME_SPECS[i, , drop = FALSE])
      spec <- lapply(spec, function(x) x[[1]])
      scale <- if (transformed) spec$scale else "level"
      label <- paste0(spec$outcome, " [", scale, "]")
      message("[SDID] ", sample_label, " | ", label)
      source_values <- as_num(panel_sample[[spec$source]])
      invalid <- if (transformed && spec$transform == "log") sum(!is.na(source_values) & source_values <= 0) else 0L
      transform_rows[[length(transform_rows) + 1L]] <- data.frame(
        Sample = sample_label, Outcome = spec$outcome, Scale = scale, Transformation = if (transformed) spec$scale else "none",
        Source_Column = spec$source, Nonmissing_Source = sum(is.finite(source_values)),
        Invalid_Nonpositive_Log = invalid, stringsAsFactors = FALSE
      )
      fit <- try(fit_one_sdid(panel_sample, spec, transformed, args$boot_reps,
                              args$seed + i +
                                (if (transformed) 10000L else 0L) +
                                (if (primary_sample) 20000L else 0L), cluster), silent = TRUE)
      if (inherits(fit, "try-error")) {
        err <- as.character(fit)
        raw <- data.frame(outcome = spec$outcome, scale = scale, sample = sample_key,
                          sample_label = sample_label, specification = "sdid",
                          specification_label = "(1) SDID", status = "estimation_failed",
                          coef = NA_real_, se = NA_real_, pvalue = NA_real_, ci_low = NA_real_, ci_high = NA_real_,
                          nobs = 0L, n_clusters = 0L, adj_r2 = NA_real_, stringsAsFactors = FALSE)
        sample_rows[[length(sample_rows) + 1L]] <- data.frame(
          Sample = sample_label, Outcome = spec$outcome, Scale = scale, Status = "estimation_failed",
          First_Year = NA_integer_, Last_Year = NA_integer_, N0 = NA_integer_, N1 = NA_integer_, T0 = NA_integer_, T1 = NA_integer_,
          N_Observations = 0L, stringsAsFactors = FALSE
        )
        error_rows[[length(error_rows) + 1L]] <- data.frame(Sample = sample_label, Outcome = spec$outcome, Scale = scale,
                                                            Stage = "estimation", Error = err, stringsAsFactors = FALSE)
      } else {
        status <- if (is.finite(fit$coef) && is.finite(fit$Primary_SE)) "ok" else if (is.finite(fit$coef)) "inference_unavailable" else "estimation_failed"
        raw <- data.frame(outcome = fit$outcome, scale = fit$scale, sample = sample_key,
                          sample_label = sample_label, specification = "sdid",
                          specification_label = "(1) SDID", status = status,
                          coef = fit$coef, se = fit$Primary_SE, pvalue = fit$P_value,
                          ci_low = fit$CI_Low, ci_high = fit$CI_High,
                          nobs = nrow(fit$sample$Y) * ncol(fit$sample$Y),
                          n_clusters = nrow(fit$sample$Y), adj_r2 = NA_real_, stringsAsFactors = FALSE)
        sample_rows[[length(sample_rows) + 1L]] <- data.frame(
          Sample = sample_label, Outcome = fit$outcome, Scale = fit$scale, Status = status,
          First_Year = fit$sample$first_year, Last_Year = fit$sample$last_year,
          N0 = fit$sample$N0, N1 = fit$sample$N1, T0 = fit$sample$T0, T1 = fit$sample$T1,
          N_Observations = nrow(fit$sample$Y) * ncol(fit$sample$Y), stringsAsFactors = FALSE
        )
        fit_rows[[length(fit_rows) + 1L]] <- cbind(Sample = sample_label, fit_diagnostics_row(fit))
        unit_rows[[length(unit_rows) + 1L]] <- cbind(Sample = sample_label, unit_weights_frame(fit))
        time_rows[[length(time_rows) + 1L]] <- cbind(Sample = sample_label, time_weights_frame(fit))
        placebo_rows[[length(placebo_rows) + 1L]] <- cbind(Sample = sample_label, time_placebo_frame(fit))
        loo_rows[[length(loo_rows) + 1L]] <- cbind(Sample = sample_label, leave_one_out_frame(fit))
        event <- event_frame(fit)
        dynamic_rows[[length(dynamic_rows) + 1L]] <- cbind(Sample = sample_label, Outcome = fit$outcome, Scale = fit$scale, event)
        stem <- safe_name(fit$outcome)
        if (transformed) {
          event_name <- paste0("event_log_dynamic_", stem, ".csv")
          desc_name <- paste0("desc_dynamic_", stem, "_log.csv")
          plot_folder <- paths$plots_log
          event_plot_name <- paste0("event_log_dynamic_", stem, ".png")
          paths_plot_name <- paste0("levels_mean_log_dynamic_", stem, ".png")
        } else {
          event_name <- paste0("event_dynamic_", stem, ".csv")
          desc_name <- paste0("desc_dynamic_", stem, ".csv")
          plot_folder <- paths$plots_dynamic
          event_plot_name <- paste0("event_dynamic_", stem, ".png")
          paths_plot_name <- paste0("levels_mean_dynamic_", stem, ".png")
        }
        if (primary_sample) {
          write_csv_utf8(event, file.path(paths$event, event_name))
          write_csv_utf8(summary_frame(fit), file.path(paths$summary, desc_name))
          try(plot_event(fit, event, file.path(plot_folder, event_plot_name)), silent = TRUE)
          try(plot_paths(fit, file.path(plot_folder, paths_plot_name)), silent = TRUE)
          try(plot_weights(fit, plot_folder, stem), silent = TRUE)
        }
      }
      if (transformed) raw_trans_rows[[length(raw_trans_rows) + 1L]] <- raw else raw_level_rows[[length(raw_level_rows) + 1L]] <- raw
      }
    }
  }

  raw_levels <- bind_rows(raw_level_rows)
  raw_transformed <- bind_rows(raw_trans_rows)
  sample_audit <- bind_rows(sample_rows)
  fit_diagnostics <- bind_rows(fit_rows)
  unit_weights <- bind_rows(unit_rows)
  time_weights <- bind_rows(time_rows)
  time_placebo <- bind_rows(placebo_rows)
  leave_one_out <- bind_rows(loo_rows, c("Sample", "Outcome", "Scale", "Donor_Rank", "Removed_Donor", "Original_Weight", "Main_ATT", "Leave_One_Out_ATT", "Delta_ATT", "Relative_Delta", "Status"))
  transformations <- bind_rows(transform_rows)
  errors <- bind_rows(error_rows, c("Sample", "Outcome", "Scale", "Stage", "Error"))
  dynamic_indicators <- bind_rows(dynamic_rows)

  write_csv_utf8(dynamic_indicators, file.path(paths$diagnostics, "diagnostic_dynamic_indicators.csv"))
  write_csv_utf8(transformations, file.path(paths$diagnostics, "outcome_transformation_diagnostics.csv"))
  write_csv_utf8(sample_audit, file.path(paths$diagnostics, "sdid_sample_audit.csv"))
  write_csv_utf8(fit_diagnostics, file.path(paths$diagnostics, "sdid_fit_diagnostics.csv"))
  write_csv_utf8(unit_weights, file.path(paths$diagnostics, "sdid_unit_weights.csv"))
  write_csv_utf8(time_weights, file.path(paths$diagnostics, "sdid_time_weights.csv"))
  write_csv_utf8(time_placebo, file.path(paths$diagnostics, "sdid_time_placebo_diagnostics.csv"))
  write_csv_utf8(leave_one_out, file.path(paths$diagnostics, "sdid_leave_one_out_diagnostics.csv"))
  write_csv_utf8(errors, file.path(paths$diagnostics, "sdid_estimation_errors.csv"))
  write_method_readme(file.path(paths$diagnostics, "README_SDID_method.txt"), args$boot_reps, args$seed)

  diag_tables <- list(sample_audit = sample_audit, fit_diagnostics = fit_diagnostics,
                      unit_weights = unit_weights, time_weights = time_weights,
                      time_placebo = time_placebo, leave_one_out = leave_one_out,
                      transformations = transformations, errors = errors)
  write_diagnostics_workbook(diag_tables, file.path(paths$results_aux, "SDID_Diagnostics.xlsx"))

  levels_xlsx <- file.path(paths$results_main, "Table_SDID_Main_Levels.xlsx")
  transformed_xlsx <- file.path(paths$results_main, "Table_SDID_Main_Transformed.xlsx")
  paper_levels <- write_main_workbook(raw_levels, levels_xlsx)
  paper_transformed <- write_main_workbook(raw_transformed, transformed_xlsx)
  write_latex_table(paper_levels, file.path(paths$results_main, "Table_SDID_Main_Levels.tex"), FALSE)
  write_latex_table(paper_transformed, file.path(paths$results_main, "Table_SDID_Main_Transformed.tex"), TRUE)

  validate_main_workbook(levels_xlsx, "level", paths$results_aux)
  validate_main_workbook(transformed_xlsx, "transformed", paths$results_aux)
  message("[OK] SDID pipeline completed and both main workbooks passed structural validation.")
}

if (sys.nframe() == 0L) main()





