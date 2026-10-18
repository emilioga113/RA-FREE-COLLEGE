# PSM / entropy-balancing design audit for the Chilean Gratuidad project.
# This script is preprocessing only. It never estimates the Dynamic DiD and
# never uses post-treatment outcomes to choose a design.

if (.Platform$OS.type == "windows") {
  try(Sys.setlocale("LC_CTYPE", "English_United States.utf8"), silent = TRUE)
}

# -----------------------------------------------------------------------------
# Central configuration
# -----------------------------------------------------------------------------

FREE_COLLEGE_DIR <- Sys.getenv(
  "FREE_COLLEGE_DIR",
  unset = "."
)
PANEL_DIR <- file.path(FREE_COLLEGE_DIR, "Construcci\u00f3n de panel data")
PSM_DIR <- file.path(FREE_COLLEGE_DIR, "Propensity Score Matching")
PATHS <- list(
  # Canonical inputs authorized for this module on 2026-09-17.
  ratios = file.path(PANEL_DIR, "ratios_fitch_panel.csv"),
  panel = file.path(PANEL_DIR, "panel_cleaned_free_college.csv"),
  mapping = file.path(PANEL_DIR, "university_name_mapping_2.csv"),
  cohorts = file.path(PANEL_DIR, "clasif_quintiles_ingresos_normalizado.csv"),
  code = file.path(PSM_DIR, "Code"),
  plots = file.path(PSM_DIR, "Plots_PSM"),
  results = file.path(PSM_DIR, "Results")
)

TREAT_YEAR <- 2016L
BASE_YEAR <- 2015L
AVERAGE_YEARS <- 2012:2015
MIN_AVERAGE_YEARS <- 2L
CALIPERS <- c(0.20, 0.30, 0.50, 0.75, 1.00)
RATIOS <- c(2L, 3L)
MAIN_RATIO <- 2L
ROBUST_RATIO <- 3L
EBW_MAXIT <- 1000L
EBW_EXACT_TOL <- 1e-6
EBW_RIDGE <- 0.05
EBW_APPROX_MAX_STANDARDIZED_GAP <- 0.10

# These are screening guardrails, not identification tests. Failure means that
# the script reports no defensible headline PSM rather than choosing the least
# bad design. Outcomes never enter these rules.
GUARDS <- list(
  max_abs_smd = 0.25,
  mean_abs_smd = 0.15,
  min_treated_retention = 0.50,
  min_control_ess = 5.00,
  min_treated_in_raw_control_ps_range = 0.50
)

PUCV <- "Pontificia Universidad Cat\u00f3lica de Valpara\u00edso"
EXCLUDED_UNIVERSITIES <- c("Universidad de Ays\u00e9n", "Universidad de O'Higgins")
TREATED_2016_DYNAMIC <- c(
  "Pontificia Universidad Cat\u00f3lica",
  "Pontificia Universidad Cat\u00f3lica de Valpara\u00edso",
  "Universidad Academia de Humanismo Cristiano",
  "Universidad Alberto Hurtado",
  "Universidad Arturo Prat",
  "Universidad Austral de Chile",
  "Universidad Cat\u00f3lica de la Sant\u00edsima Concepci\u00f3n",
  "Universidad Cat\u00f3lica de Temuco",
  "Universidad Cat\u00f3lica del Maule",
  "Universidad Cat\u00f3lica del Norte",
  "Universidad de Antofagasta",
  "Universidad de Atacama",
  "Universidad de Chile",
  "Universidad de Concepcion",
  "Universidad de la Frontera",
  "Universidad de la Serena",
  "Universidad de los Lagos",
  "Universidad de Magallanes",
  "Universidad de Santiago de Chile",
  "Universidad de Talca",
  "Universidad de Tarapaca",
  "Universidad de Valpara\u00edso",
  "Universidad del Bio-bio",
  "Universidad Diego Portales",
  "Universidad Finis Terrae",
  "Universidad T\u00e9cnica Federico Santa Mar\u00eda",
  "Universidad Tecnol\u00f3gica Metropolitana",
  "Universidad Aut\u00f3noma de Chile",
  "Universidad Cat\u00f3lica Silva Henr\u00edquez",
  "Universidad de Playa Ancha",
  "Universidad Metropolitana de Ciencias de la Educaci\u00f3n",
  "Universidad de O'Higgins"
)
CORE_X <- c(
  "log_enrollment",
  "log_tuition",
  "asset_turnover"
)
DIAGNOSTIC_X <- c(
  "total_assets_liabilities",
  "accreditation_years",
  "fuas_share",
  "programs_per_1000",
  "eligibility_coverage"
)
ALL_PRETREATMENT_VARS <- unique(c(CORE_X, DIAGNOSTIC_X))

ALIASES <- list(
  university = c("Universidad_std", "Final_Standard", "Universidad", "Instituci\u00f3n", "Institucion"),
  year = c("A\u00f1o", "Ano", "year"),
  enrollment = c("Matr\u00edcula pregrado", "Matricula pregrado", "N total estudiantes pregrado"),
  accreditation = c("Acreditaci\u00f3n (a\u00f1os)", "Acreditacion (anos)"),
  fuas = c("% elegibles gratuidad (FUAS)"),
  coverage = c("Cobertura FUAS (%)", "Cobertura elegibilidad proxy (%)"),
  tuition = c("arancel_medio", "Arancel medio", "arancel_mediano"),
  programs = c("n_programas_pregrado", "N programas pregrado"),
  cohort_university = c("Universidad", "Instituci\u00f3n", "Institucion"),
  cohort_category = c("Categoria", "Categor\u00eda"),
  cohort_year = c("A\u00f1o", "Ano")
)

stopf <- function(...) stop(sprintf(...), call. = FALSE)
msg <- function(...) message(sprintf(...))

read_csv_strict <- function(path) {
  out <- tryCatch(
    read.csv(path, check.names = FALSE, stringsAsFactors = FALSE,
             fileEncoding = "UTF-8", na.strings = c("", "NA")),
    error = function(e) stopf("No se pudo leer el input oficial %s\n%s", path, conditionMessage(e))
  )
  names(out) <- sub("^\ufeff", "", names(out))
  out
}

pick_col <- function(df, aliases, label, required = TRUE) {
  hit <- aliases[aliases %in% names(df)]
  if (!length(hit)) {
    if (required) stopf("Falta la variable requerida '%s'. Alias admitidos: %s",
                        label, paste(aliases, collapse = " | "))
    return(NA_character_)
  }
  hit[[1L]]
}

norm_name <- function(x) {
  x <- trimws(tolower(as.character(x)))
  x <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT")
  x <- gsub("[.]", "", x)
  gsub("[[:space:]]+", " ", x)
}

as_num <- function(x) {
  out <- suppressWarnings(as.numeric(x))
  out[!is.finite(out)] <- NA_real_
  out
}

first_unique <- function(x, group_label, var_label) {
  y <- unique(x[!is.na(x)])
  if (length(y) > 1L) {
    stopf("Valores incompatibles de %s dentro de universidad-a\u00f1o: %s",
          var_label, group_label)
  }
  if (!length(y)) NA_real_ else y[[1L]]
}

weighted_mean <- function(x, w) sum(x * w) / sum(w)

weighted_var <- function(x, w) {
  m <- weighted_mean(x, w)
  sum(w * (x - m)^2) / sum(w)
}

ess <- function(w) {
  w <- w[is.finite(w) & w > 0]
  if (!length(w)) return(NA_real_)
  sum(w)^2 / sum(w^2)
}

weighted_ecdf_max <- function(x1, w1, x0, w0) {
  grid <- sort(unique(c(x1, x0)))
  f1 <- vapply(grid, function(z) sum(w1[x1 <= z]) / sum(w1), numeric(1))
  f0 <- vapply(grid, function(z) sum(w0[x0 <= z]) / sum(w0), numeric(1))
  max(abs(f1 - f0))
}

validate_inputs <- function() {
  required <- unlist(PATHS[c("ratios", "panel", "mapping", "cohorts")])
  missing <- required[!file.exists(required)]
  if (length(missing)) {
    stopf(paste0(
      "Faltan inputs oficiales. El pipeline se detiene sin usar fallbacks:\n- ",
      paste(missing, collapse = "\n- ")
    ))
  }
  invisible(TRUE)
}

validate_output_root <- function() {
  expected <- normalizePath(PSM_DIR, winslash = "/", mustWork = FALSE)
  roots <- vapply(PATHS[c("plots", "results")],
                  normalizePath, character(1), winslash = "/", mustWork = FALSE)
  if (any(!startsWith(roots, paste0(expected, "/")))) {
    stopf("Todos los outputs deben quedar dentro de %s", PSM_DIR)
  }
  for (d in unname(PATHS[c("code", "plots", "results")])) {
    dir.create(d, recursive = TRUE, showWarnings = FALSE)
  }
}

load_name_map <- function(path) {
  m <- read_csv_strict(path)
  if (!"Final_Standard" %in% names(m)) stopf("El mapping no contiene Final_Standard")
  source_cols <- intersect(
    c("Universidad_key", "Universidad_raw", "CNED_Universidad", "CNED_key",
      "Suggested_Standard", "Final_Standard"), names(m)
  )
  out <- character()
  for (i in seq_len(nrow(m))) {
    standard <- trimws(as.character(m$Final_Standard[[i]]))
    if (is.na(standard) || !nzchar(standard)) next
    for (col in source_cols) {
      raw <- m[[col]][[i]]
      if (!is.na(raw) && nzchar(trimws(as.character(raw)))) {
        key <- norm_name(raw)
        if (key %in% names(out) && !identical(out[[key]], standard)) {
          stopf("Mapping ambiguo para '%s': '%s' vs '%s'", raw, out[[key]], standard)
        }
        out[[key]] <- standard
      }
    }
  }
  # Audited aliases that are not exact strings in the source mapping file.
  out[[norm_name("Pontificia Universidad Cat\u00f3lica de Chile")]] <-
    "Pontificia Universidad Cat\u00f3lica"
  out[[norm_name("Universidad de Playa Ancha de Ciencias de la Educaci\u00f3n")]] <-
    "Universidad de Playa Ancha"
  out
}

standardize_names <- function(x, mapping) {
  key <- norm_name(x)
  mapped <- unname(mapping[key])
  mapped[is.na(mapped)] <- trimws(as.character(x))[is.na(mapped)]
  mapped
}

load_cohorts <- function(path, mapping) {
  x <- read_csv_strict(path)
  u <- pick_col(x, ALIASES$cohort_university, "universidad de cohorte")
  ccol <- pick_col(x, ALIASES$cohort_category, "categor\u00eda de cohorte")
  y <- pick_col(x, ALIASES$cohort_year, "a\u00f1o de cohorte")
  x$university <- standardize_names(x[[u]], mapping)
  x$university_key <- norm_name(x$university)
  x$category <- as_num(x[[ccol]])
  x$treatment_year <- as_num(x[[y]])
  x <- x[x$category == 1 & !is.na(x$treatment_year),
         c("university", "university_key", "treatment_year")]
  years_by_key <- split(x$treatment_year, x$university_key)
  bad <- names(years_by_key)[vapply(years_by_key, function(z) length(unique(z)) > 1L, logical(1))]
  if (length(bad)) stopf("Cohortes incompatibles para: %s", paste(bad, collapse = ", "))
  x <- x[!duplicated(x$university_key), ]
  forced <- data.frame(
    university = standardize_names(TREATED_2016_DYNAMIC, mapping),
    treatment_year = TREAT_YEAR, stringsAsFactors = FALSE
  )
  forced$university_key <- norm_name(forced$university)
  x <- x[!x$university_key %in% forced$university_key, ]
  x <- rbind(x, forced[c("university", "university_key", "treatment_year")])
  pucv_key <- norm_name(PUCV)
  if (!pucv_key %in% x$university_key || x$treatment_year[x$university_key == pucv_key] != TREAT_YEAR) {
    stopf("PUCV debe figurar expl\u00edcitamente como cohorte 2016 en el input oficial")
  }
  x
}

collapse_institutional_panel <- function(raw, mapping) {
  cols <- list(
    university = pick_col(raw, ALIASES$university, "universidad"),
    year = pick_col(raw, ALIASES$year, "a\u00f1o"),
    enrollment = pick_col(raw, ALIASES$enrollment, "matr\u00edcula pregrado"),
    accreditation = pick_col(raw, ALIASES$accreditation, "acreditaci\u00f3n"),
    fuas = pick_col(raw, ALIASES$fuas, "FUAS"),
    coverage = pick_col(raw, ALIASES$coverage, "coverage", required = FALSE),
    tuition = pick_col(raw, ALIASES$tuition, "arancel"),
    programs = pick_col(raw, ALIASES$programs, "programas pregrado")
  )
  d <- data.frame(
    university = standardize_names(raw[[cols$university]], mapping),
    year = as_num(raw[[cols$year]]),
    enrollment = as_num(raw[[cols$enrollment]]),
    accreditation = as_num(raw[[cols$accreditation]]),
    fuas_percent = as_num(raw[[cols$fuas]]),
    coverage_percent = if (!is.na(cols$coverage)) as_num(raw[[cols$coverage]]) else NA_real_,
    tuition = as_num(raw[[cols$tuition]]),
    programs = as_num(raw[[cols$programs]]),
    stringsAsFactors = FALSE
  )
  d$university_key <- norm_name(d$university)
  d <- d[!is.na(d$year) & d$year < TREAT_YEAR, ]
  if (!nrow(d)) stopf("El panel institucional no contiene observaciones pre-2016")
  fuas <- d$fuas_percent[!is.na(d$fuas_percent)]
  if (!length(fuas)) stopf("FUAS no tiene observaciones no-missing pre-2016")
  if (any(fuas < 0 | fuas > 100)) stopf("FUAS est\u00e1 fuera de la escala esperada 0-100")
  if (max(fuas) <= 1) stopf("FUAS parece estar en 0-1; el input oficial se esperaba en 0-100")
  d$fuas_share <- d$fuas_percent / 100
  cov <- d$coverage_percent[!is.na(d$coverage_percent)]
  if (length(cov)) {
    if (any(cov < 0 | cov > 100)) stopf("Coverage est\u00e1 fuera de 0-100")
    d$coverage_share <- d$coverage_percent / 100
  } else {
    d$coverage_share <- NA_real_
  }
  if (any(d$accreditation < 0 | d$accreditation > 7, na.rm = TRUE)) {
    stopf("Acreditaci\u00f3n contiene valores fuera de 0-7 a\u00f1os")
  }
  d$log_enrollment <- ifelse(d$enrollment > 0, log(d$enrollment), NA_real_)
  d$accreditation_years <- d$accreditation
  d$log_tuition <- ifelse(d$tuition > 0, log(d$tuition), NA_real_)
  d$programs_per_1000 <- ifelse(d$enrollment > 0 & d$programs >= 0,
                                1000 * d$programs / d$enrollment, NA_real_)
  keys <- paste(d$university_key, d$year, sep = "||")
  groups <- split(seq_len(nrow(d)), keys)
  collapsed <- lapply(groups, function(idx) {
    label <- keys[[idx[[1L]]]]
    data.frame(
      university = d$university[[idx[[1L]]]],
      university_key = d$university_key[[idx[[1L]]]],
      year = d$year[[idx[[1L]]]],
      log_enrollment = first_unique(d$log_enrollment[idx], label, "log enrollment"),
      accreditation_years = first_unique(d$accreditation_years[idx], label, "accreditation"),
      fuas_share = first_unique(d$fuas_share[idx], label, "FUAS share"),
      log_tuition = first_unique(d$log_tuition[idx], label, "log tuition"),
      programs_per_1000 = first_unique(d$programs_per_1000[idx], label, "programs/1000"),
      eligibility_coverage = first_unique(d$coverage_share[idx], label, "eligibility coverage"),
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, collapsed)
  rownames(out) <- NULL
  if (anyDuplicated(out[c("university_key", "year")])) stopf("Duplicados universidad-a\u00f1o tras colapsar panel")
  out
}

load_financial_panel <- function(path, mapping) {
  x <- read_csv_strict(path)
  u <- pick_col(x, ALIASES$university, "universidad en ratios Fitch")
  y <- pick_col(x, ALIASES$year, "a\u00f1o en ratios Fitch")
  asset_col <- "Asset Turnover"
  assets_liabilities_col <- "Total Assets / Total Liabilities"
  missing <- setdiff(c(asset_col, assets_liabilities_col), names(x))
  if (length(missing)) stopf("Faltan variables financieras pretratamiento: %s", paste(missing, collapse = ", "))
  out <- data.frame(
    university = standardize_names(x[[u]], mapping),
    year = as_num(x[[y]]),
    asset_turnover = as_num(x[[asset_col]]),
    total_assets_liabilities = as_num(x[[assets_liabilities_col]]),
    stringsAsFactors = FALSE
  )
  out$university_key <- norm_name(out$university)
  out <- out[!is.na(out$university_key) & !is.na(out$year), ]
  if (anyDuplicated(out[c("university_key", "year")])) {
    stopf("ratios_fitch_panel.csv contiene duplicados universidad-a\u00f1o")
  }
  out
}

merge_financial_covariates <- function(institutional, financial_panel) {
  if (anyDuplicated(institutional[c("university_key", "year")])) stopf("Duplicados en panel institucional")
  if (anyDuplicated(financial_panel[c("university_key", "year")])) stopf("Duplicados en panel financiero")
  f <- financial_panel[c("university_key", "year", "asset_turnover", "total_assets_liabilities")]
  n_before <- nrow(institutional)
  out <- merge(institutional, f, by = c("university_key", "year"), all.x = TRUE, sort = FALSE)
  if (nrow(out) != n_before) stopf("Merge financiero cambi\u00f3 la cardinalidad del panel institucional")
  out
}

build_treatment_audit <- function(financial, cohorts) {
  fin <- financial
  names(fin)[names(fin) == "university"] <- "financial_university"
  coh <- cohorts
  names(coh)[names(coh) == "university"] <- "cohort_university"
  out <- merge(fin, coh, by = "university_key", all = TRUE, sort = FALSE)
  out$in_financial_panel <- !is.na(out$financial_university)
  out$university <- ifelse(out$in_financial_panel, out$financial_university, out$cohort_university)
  out$financial_university <- out$cohort_university <- NULL
  out$treatment_status <- ifelse(
    is.na(out$treatment_year), "never_treated",
    ifelse(out$treatment_year == TREAT_YEAR, "treated_2016", "other_treated_cohort")
  )
  out$explicit_exclusion <- out$university_key %in% norm_name(EXCLUDED_UNIVERSITIES)
  out$target_treated_population <- out$treatment_status == "treated_2016" & !out$explicit_exclusion
  out$headline_eligible_status <- out$in_financial_panel &
    out$treatment_status %in% c("treated_2016", "never_treated") & !out$explicit_exclusion
  if (any(out$headline_eligible_status & out$treatment_status == "other_treated_cohort")) {
    stopf("Later adopters entraron a la muestra headline")
  }
  if (any(out$university_key %in% norm_name(EXCLUDED_UNIVERSITIES) & out$headline_eligible_status)) {
    stopf("Ays\u00e9n/O'Higgins no fueron excluidas")
  }
  out
}

build_covariate_samples <- function(panel, treatment_audit) {
  base <- panel[panel$year == BASE_YEAR, c("university", "university_key", ALL_PRETREATMENT_VARS)]
  base$covariate_spec <- "baseline_2015"
  base$n_pre_years <- 1L

  avg_raw <- panel[panel$year %in% AVERAGE_YEARS, ]
  avg_groups <- split(seq_len(nrow(avg_raw)), avg_raw$university_key)
  avg <- lapply(avg_groups, function(idx) {
    z <- avg_raw[idx, ]
    vals <- lapply(ALL_PRETREATMENT_VARS, function(v) {
      if (sum(!is.na(z[[v]])) >= MIN_AVERAGE_YEARS) mean(z[[v]], na.rm = TRUE) else NA_real_
    })
    names(vals) <- ALL_PRETREATMENT_VARS
    data.frame(
      university = z$university[[1L]], university_key = z$university_key[[1L]],
      as.data.frame(vals, check.names = FALSE), covariate_spec = "average_2012_2015",
      n_pre_years = length(unique(z$year)), stringsAsFactors = FALSE,
      check.names = FALSE
    )
  })
  avg <- do.call(rbind, avg)
  allx <- rbind(base, avg)
  names(allx)[names(allx) == "university"] <- "covariate_university"
  skeleton <- merge(
    treatment_audit,
    data.frame(covariate_spec = c("baseline_2015", "average_2012_2015"), stringsAsFactors = FALSE),
    by = NULL
  )
  out <- merge(skeleton, allx, by = c("university_key", "covariate_spec"),
               all.x = TRUE, sort = FALSE)
  out$university <- ifelse(is.na(out$covariate_university), out$university, out$covariate_university)
  out$covariate_university <- NULL
  out$treated_2016 <- as.integer(out$treatment_status == "treated_2016")
  out$complete_x <- complete.cases(out[CORE_X])
  out$missing_covariates <- apply(out[CORE_X], 1, function(z) {
    paste(CORE_X[is.na(z)], collapse = " | ")
  })
  out$analysis_eligible <- out$headline_eligible_status & out$complete_x
  out$exclusion_reason <- ifelse(
    out$explicit_exclusion, "Aysen_or_OHiggins",
    ifelse(out$treatment_status == "other_treated_cohort", "later_or_other_adopter",
           ifelse(!out$in_financial_panel, "missing_financial_panel",
                  ifelse(!out$complete_x, "missing_required_pretreatment_covariate", "included")))
  )
  out
}

fit_propensity <- function(d) {
  if (!all(d$treated_2016 %in% c(0L, 1L))) stopf("Tratamiento no binario")
  if (length(unique(d$treated_2016)) != 2L) stopf("La muestra necesita tratadas y controles")
  form <- reformulate(CORE_X, response = "treated_2016")
  warnings <- character()
  fit <- withCallingHandlers(
    glm(form, data = d, family = binomial(link = "logit")),
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  if (!fit$converged) stopf("El propensity-score logit no convergi\u00f3")
  d$logit_ps <- as.numeric(predict(fit, type = "link"))
  d$propensity_score <- plogis(d$logit_ps)
  if (any(!is.finite(d$logit_ps)) || any(d$propensity_score <= 0 | d$propensity_score >= 1)) {
    stopf("Separaci\u00f3n perfecta: propensity scores 0/1 o logits no finitos")
  }
  attr(d, "ps_warnings") <- unique(warnings)
  attr(d, "ps_coefficients") <- coef(fit)
  d
}

balance_table <- function(d, weights, stage, specification) {
  vars <- unique(c("logit_ps", CORE_X, intersect(DIAGNOSTIC_X, names(d))))
  denom <- vapply(vars, function(v) {
    # Diagnostic-only covariates may be missing for units that remain eligible
    # for the headline match. Compute their reference dispersion on observed
    # pretreatment values rather than returning an uninformative NA SMD.
    vt <- var(d[[v]][d$treated_2016 == 1], na.rm = TRUE)
    vc <- var(d[[v]][d$treated_2016 == 0], na.rm = TRUE)
    sqrt((vt + vc) / 2)
  }, numeric(1))
  rows <- lapply(seq_along(vars), function(j) {
    v <- vars[[j]]
    keep <- weights > 0 & !is.na(d[[v]])
    t <- keep & d$treated_2016 == 1
    c0 <- keep & d$treated_2016 == 0
    mt <- weighted_mean(d[[v]][t], weights[t])
    mc <- weighted_mean(d[[v]][c0], weights[c0])
    vt <- weighted_var(d[[v]][t], weights[t])
    vc <- weighted_var(d[[v]][c0], weights[c0])
    data.frame(
      specification = specification, stage = stage, variable = v,
      in_design = v %in% c("logit_ps", CORE_X),
      mean_treated = mt, mean_control = mc,
      smd = if (is.finite(denom[[j]]) && denom[[j]] > 0) (mt - mc) / denom[[j]] else NA_real_,
      variance_ratio = if (is.finite(vc) && vc > 0) vt / vc else NA_real_,
      ecdf_max = weighted_ecdf_max(d[[v]][t], weights[t], d[[v]][c0], weights[c0]),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

weight_summary <- function(w, specification, method, group = "control") {
  w <- w[is.finite(w) & w > 0]
  qs <- if (length(w)) quantile(w, c(.50, .75, .90, .95), names = FALSE) else rep(NA_real_, 4)
  data.frame(
    specification = specification, method = method, group = group,
    nominal_n = length(w), ess = ess(w), min_weight = if (length(w)) min(w) else NA_real_,
    median_weight = qs[[1L]], p75_weight = qs[[2L]], p90_weight = qs[[3L]],
    p95_weight = qs[[4L]], max_weight = if (length(w)) max(w) else NA_real_,
    coefficient_of_variation = if (length(w) && mean(w) > 0) sd(w) / mean(w) else NA_real_,
    max_weight_share = if (length(w)) max(w) / sum(w) else NA_real_,
    stringsAsFactors = FALSE
  )
}

run_match <- function(d, ratio, caliper, covariate_spec) {
  # With replacement, each treated university can be matched independently.
  # The covariance matrix is estimated on the full eligible pretreatment sample.
  x <- as.matrix(d[CORE_X])
  s <- cov(x)
  eig <- eigen(s, symmetric = TRUE)
  tol <- max(eig$values) * ncol(s) * .Machine$double.eps
  keep_eig <- eig$values > tol
  if (!any(keep_eig)) stopf("Mahalanobis covariance matrix has zero rank")
  sinv <- eig$vectors[, keep_eig, drop = FALSE] %*%
    diag(1 / eig$values[keep_eig], nrow = sum(keep_eig)) %*%
    t(eig$vectors[, keep_eig, drop = FALSE])
  treated_idx <- which(d$treated_2016 == 1)
  control_idx <- which(d$treated_2016 == 0)
  caliper_width <- caliper * sd(d$logit_ps)
  pair_rows <- list()
  k <- 0L
  for (ti in treated_idx) {
    logit_gap <- abs(d$logit_ps[control_idx] - d$logit_ps[[ti]])
    admissible <- control_idx[logit_gap <= caliper_width]
    if (!length(admissible)) next
    delta <- sweep(x[admissible, , drop = FALSE], 2, x[ti, ], "-")
    md <- sqrt(rowSums((delta %*% sinv) * delta))
    ord <- order(md, abs(d$logit_ps[admissible] - d$logit_ps[[ti]]),
                 d$university_key[admissible])
    chosen <- admissible[ord[seq_len(min(ratio, length(ord)))]]
    for (r in seq_along(chosen)) {
      k <- k + 1L
      pair_rows[[k]] <- data.frame(
        treated_university_key = d$university_key[[ti]],
        control_university_key = d$university_key[[chosen[[r]]]], match_rank = r,
        pair_weight = 1 / length(chosen), stringsAsFactors = FALSE
      )
    }
  }
  pairs <- if (length(pair_rows)) do.call(rbind, pair_rows) else data.frame(
    treated_university_key = character(), control_university_key = character(),
    match_rank = integer(), pair_weight = numeric()
  )
  w <- setNames(rep(0, nrow(d)), d$university_key)
  matched_treated <- unique(pairs$treated_university_key)
  w[matched_treated] <- 1
  if (nrow(pairs)) {
    cw <- aggregate(pair_weight ~ control_university_key, pairs, sum)
    w[cw$control_university_key] <- cw$pair_weight
  }
  d$psm_weight <- unname(w[d$university_key])
  specification <- sprintf("%s__PSM_1to%d_caliper_%s", covariate_spec, ratio,
                           sub("\\.$", "", format(caliper, nsmall = 2)))
  pairs$specification <- specification
  if (nrow(pairs)) {
    ti <- match(pairs$treated_university_key, d$university_key)
    ci <- match(pairs$control_university_key, d$university_key)
    pairs$treated_propensity_score <- d$propensity_score[ti]
    pairs$control_propensity_score <- d$propensity_score[ci]
    pairs$abs_logit_distance <- abs(d$logit_ps[ti] - d$logit_ps[ci])
  }
  reuse <- if (nrow(pairs)) aggregate(
    cbind(reuse_count = rep(1, nrow(pairs)), total_att_weight = pairs$pair_weight),
    by = list(control_university_key = pairs$control_university_key), sum
  ) else data.frame(control_university_key = character(), reuse_count = integer(), total_att_weight = numeric())
  reuse$specification <- specification
  bt_before <- balance_table(d, rep(1, nrow(d)), "before", specification)
  bt_after <- balance_table(d, d$psm_weight, "after_psm", specification)
  x_after <- bt_after[bt_after$variable %in% CORE_X, ]
  t0 <- d$treated_2016 == 0
  t1 <- d$treated_2016 == 1
  matched1 <- t1 & d$psm_weight > 0
  matched0 <- t0 & d$psm_weight > 0
  ps0 <- d$propensity_score[t0]
  in_raw_control_range <- d$propensity_score[t1] >= min(ps0) & d$propensity_score[t1] <= max(ps0)
  grid <- data.frame(
    specification = specification, covariate_spec = covariate_spec,
    ratio = ratio, caliper = caliper,
    n_treated_original = attr(d, "n_treated_original"),
    n_control_original = attr(d, "n_control_original"),
    n_treated_covariate_complete = sum(t1), n_control_covariate_complete = sum(t0),
    n_treated_matched = sum(matched1),
    treated_lost = attr(d, "n_treated_original") - sum(matched1),
    treated_lost_missing_x = attr(d, "n_treated_original") - sum(t1),
    treated_lost_support = sum(t1) - sum(matched1),
    treated_retention = sum(matched1) / attr(d, "n_treated_original"),
    n_controls_unique = sum(matched0), control_ess = ess(d$psm_weight[matched0]),
    max_abs_smd = max(abs(x_after$smd), na.rm = TRUE),
    mean_abs_smd = mean(abs(x_after$smd), na.rm = TRUE),
    min_variance_ratio = min(x_after$variance_ratio, na.rm = TRUE),
    max_variance_ratio = max(x_after$variance_ratio, na.rm = TRUE),
    max_ecdf_difference = max(x_after$ecdf_max, na.rm = TRUE),
    ps_smd_after = bt_after$smd[bt_after$variable == "logit_ps"],
    treated_in_raw_control_ps_range = mean(in_raw_control_range),
    max_control_reuse = if (nrow(reuse)) max(reuse$reuse_count) else 0,
    max_control_weight = if (any(matched0)) max(d$psm_weight[matched0]) else NA_real_,
    stringsAsFactors = FALSE
  )
  unmatched <- d$university[d$treated_2016 == 1 & d$psm_weight == 0]
  grid$treated_excluded_missing_x <- paste(attr(d, "treated_missing_x"), collapse = " | ")
  grid$treated_unmatched_support <- paste(unmatched, collapse = " | ")
  grid$treated_unmatched <- paste(c(attr(d, "treated_missing_x"), unmatched), collapse = " | ")
  overlap <- do.call(rbind, lapply(0:1, function(g) {
    z <- d[d$treated_2016 == g, ]
    data.frame(specification = specification, group = ifelse(g == 1, "treated", "control"),
               n = nrow(z), ps_min = min(z$propensity_score), ps_p10 = quantile(z$propensity_score, .10),
               ps_median = median(z$propensity_score), ps_mean = mean(z$propensity_score),
               ps_p90 = quantile(z$propensity_score, .90), ps_max = max(z$propensity_score),
               logit_min = min(z$logit_ps), logit_max = max(z$logit_ps), stringsAsFactors = FALSE)
  }))
  list(data = d, pairs = pairs, reuse = reuse, balance = rbind(bt_before, bt_after),
       grid = grid, overlap = overlap,
       weights = weight_summary(d$psm_weight[matched0], specification, "PSM"))
}

entropy_calibrate <- function(d, base_weights, method_label) {
  treated <- d$treated_2016 == 1 & base_weights > 0
  control <- d$treated_2016 == 0 & base_weights > 0
  if (sum(treated) < 2 || sum(control) < 2) {
    return(list(feasible = FALSE, mode = "infeasible", reason = "insufficient_units", weights = rep(0, nrow(d))))
  }
  x <- as.matrix(d[CORE_X])
  center <- colMeans(x[c(treated | control), , drop = FALSE])
  scale <- apply(x[c(treated | control), , drop = FALSE], 2, sd)
  if (any(!is.finite(scale) | scale == 0)) {
    return(list(feasible = FALSE, mode = "infeasible", reason = "zero_variance_covariate", weights = rep(0, nrow(d))))
  }
  z <- sweep(sweep(x, 2, center, "-"), 2, scale, "/")
  target <- colMeans(z[treated, , drop = FALSE])
  q <- base_weights[control]
  q <- q / sum(q)
  zc <- z[control, , drop = FALSE]
  dual <- function(lambda, ridge = 0) {
    eta <- as.numeric(zc %*% lambda)
    shift <- max(eta)
    log(sum(q * exp(eta - shift))) + shift - sum(lambda * target) + .5 * ridge * sum(lambda^2)
  }
  grad <- function(lambda, ridge = 0) {
    eta <- as.numeric(zc %*% lambda)
    a <- q * exp(eta - max(eta)); a <- a / sum(a)
    colSums(zc * a) - target + ridge * lambda
  }
  solve_one <- function(ridge) {
    fit <- tryCatch(
      optim(rep(0, ncol(zc)), dual, grad, ridge = ridge, method = "BFGS",
            control = list(maxit = EBW_MAXIT, reltol = 1e-10)),
      error = identity
    )
    if (inherits(fit, "error")) return(list(ok = FALSE, reason = conditionMessage(fit)))
    eta <- as.numeric(zc %*% fit$par)
    a <- q * exp(eta - max(eta)); a <- a / sum(a)
    wc <- a * sum(base_weights[treated])
    gap <- colSums(zc * a) - target
    list(ok = fit$convergence == 0 && all(is.finite(wc)) && all(wc >= 0),
         reason = fit$message %||% "", wc = wc, gap = gap, convergence = fit$convergence)
  }
  exact <- solve_one(0)
  if (isTRUE(exact$ok) && max(abs(exact$gap)) <= EBW_EXACT_TOL) {
    w <- rep(0, nrow(d)); w[treated] <- base_weights[treated]; w[control] <- exact$wc
    return(list(feasible = TRUE, exact_feasible = TRUE, mode = "exact", reason = "", weights = w,
                max_standardized_gap = max(abs(exact$gap))))
  }
  approx <- solve_one(EBW_RIDGE)
  if (isTRUE(approx$ok)) {
    w <- rep(0, nrow(d)); w[treated] <- base_weights[treated]; w[control] <- approx$wc
    acceptable <- max(abs(approx$gap)) <= EBW_APPROX_MAX_STANDARDIZED_GAP &&
      ess(w[control]) >= 3 && max(w[control]) / sum(w[control]) <= 0.50
    return(list(feasible = acceptable, exact_feasible = FALSE,
                mode = ifelse(acceptable, "approximate_ridge", "approximate_rejected"),
                reason = ifelse(acceptable, "", "approximate guardrails failed"), weights = w,
                max_standardized_gap = max(abs(approx$gap))))
  }
  list(feasible = FALSE, exact_feasible = FALSE, mode = "infeasible",
       reason = approx$reason, weights = rep(0, nrow(d)), max_standardized_gap = NA_real_)
}

`%||%` <- function(x, y) if (is.null(x) || !length(x) || is.na(x)) y else x

select_design <- function(grid, covariate_spec, ratio) {
  g <- grid[grid$covariate_spec == covariate_spec & grid$ratio == ratio, ]
  g$passes_guardrails <- with(g,
    max_abs_smd <= GUARDS$max_abs_smd &
      mean_abs_smd <= GUARDS$mean_abs_smd &
      treated_retention >= GUARDS$min_treated_retention &
      control_ess >= GUARDS$min_control_ess &
      treated_in_raw_control_ps_range >= GUARDS$min_treated_in_raw_control_ps_range
  )
  ok <- g[g$passes_guardrails, ]
  if (!nrow(ok)) return(NULL)
  # Choose the tightest admissible caliper. Balance, ESS, retention, and overlap
  # are feasibility screens; outcomes never enter the ordering.
  ok <- ok[order(ok$caliper, ok$max_abs_smd, ok$mean_abs_smd,
                 -ok$control_ess, -ok$treated_retention), ]
  ok[1, ]
}

empty_selected <- function(reason) data.frame(
  selection_status = "no_defensible_candidate", selection_reason = reason,
  stringsAsFactors = FALSE
)

write_csv <- function(x, path) write.csv(x, path, row.names = FALSE, na = "", fileEncoding = "UTF-8")

bind_rows_fill <- function(...) {
  parts <- Filter(function(x) !is.null(x) && nrow(x), list(...))
  if (!length(parts)) return(data.frame())
  all_names <- unique(unlist(lapply(parts, names)))
  parts <- lapply(parts, function(x) {
    missing <- setdiff(all_names, names(x))
    for (nm in missing) x[[nm]] <- NA
    x[all_names]
  })
  do.call(rbind, parts)
}

plot_overlap <- function(ps, selected_spec, matched_weights, path) {
  z <- ps[ps$covariate_spec == "baseline_2015", ]
  common_bw <- bw.nrd0(z$propensity_score)
  dt_before <- density(z$propensity_score[z$treated_2016 == 1], bw = common_bw, from = 0, to = 1)
  dc_before <- density(z$propensity_score[z$treated_2016 == 0], bw = common_bw, from = 0, to = 1)

  if (is.null(selected_spec) || is.null(matched_weights) || !nrow(matched_weights)) {
    png(path, width = 2200, height = 1350, res = 220)
    on.exit(dev.off(), add = TRUE)
    plot.new(); title(main = "Propensity score overlap")
    text(.5, .5, "No selected matched sample")
    return(invisible(NULL))
  }

  after <- merge(
    matched_weights[c("university_key", "treated_2016", "weight")],
    z[c("university_key", "propensity_score")],
    by = "university_key", all.x = TRUE, sort = FALSE
  )
  if (anyNA(after$propensity_score)) stopf("Falta propensity score para una universidad emparejada")
  treated_after <- after$treated_2016 == 1
  control_after <- after$treated_2016 == 0
  wt <- after$weight[treated_after]; wc <- after$weight[control_after]
  dt_after <- density(
    after$propensity_score[treated_after], weights = wt / sum(wt),
    bw = common_bw, from = 0, to = 1
  )
  dc_after <- density(
    after$propensity_score[control_after], weights = wc / sum(wc),
    bw = common_bw, from = 0, to = 1
  )

  ylim <- c(0, max(dt_before$y, dc_before$y, dt_after$y, dc_after$y))
  png(path, width = 2600, height = 1350, res = 220)
  on.exit(dev.off(), add = TRUE)
  old <- par(mfrow = c(1, 2), mar = c(5.2, 5.0, 3.2, 1.2),
             oma = c(0, 0, 4.2, 0), family = "sans", las = 1)
  on.exit(par(old), add = TRUE)

  plot(dc_before, col = "grey35", lwd = 3, lty = 2, xlim = c(0, 1),
       ylim = ylim, xlab = "Propensity score", ylab = "Density",
       main = "Before matching", cex.main = 1.25, cex.lab = 1.1,
       cex.axis = 1.05, panel.first = grid(col = "grey90", lty = 1))
  lines(dt_before, col = "black", lwd = 3)
  legend("topleft", c("Treated 2016", "Never treated"),
         col = c("black", "grey35"), lty = c(1, 2), lwd = 3, bty = "n", cex = 1.0)

  plot(dc_after, col = "grey35", lwd = 3, lty = 2, xlim = c(0, 1),
       ylim = ylim, xlab = "Propensity score", ylab = "Density",
       main = "After PSM 1:2 (ATT weights)", cex.main = 1.25, cex.lab = 1.1,
       cex.axis = 1.05, panel.first = grid(col = "grey90", lty = 1))
  lines(dt_after, col = "black", lwd = 3)
  legend("topleft", c("Treated 2016", "Weighted controls"),
         col = c("black", "grey35"), lty = c(1, 2), lwd = 3, bty = "n", cex = 1.0)

  mtext("Propensity score overlap", side = 3, outer = TRUE,
        line = 2.2, cex = 1.45, font = 2)
  mtext("2016 cohort and never-treated controls | 2015 covariates", side = 3,
        outer = TRUE, line = 0.8, cex = 0.95, col = "grey35")
}

plot_love <- function(balance, specification, path, subtitle) {
  png(path, width = 2200, height = 1500, res = 220)
  on.exit(dev.off(), add = TRUE)
  if (is.null(specification) || !nrow(balance[balance$specification == specification, ])) {
    plot.new(); title(main = subtitle); text(.5, .5, "No defensible design selected")
    return(invisible(NULL))
  }
  shown <- c(CORE_X, "total_assets_liabilities")
  labels <- c(
    log_enrollment = "Log undergraduate enrollment",
    log_tuition = "Log tuition",
    asset_turnover = "Asset turnover",
    total_assets_liabilities = "Total assets / total liabilities"
  )
  z <- balance[balance$specification == specification & balance$variable %in% shown, ]
  before <- z[z$stage == "before", ]; after <- z[z$stage == "after_psm", ]
  vars <- shown[shown %in% z$variable]
  xb <- abs(before$smd[match(vars, before$variable)])
  xa <- abs(after$smd[match(vars, after$variable)])
  xmax <- max(c(.10, xb, xa), na.rm = TRUE) * 1.10
  old <- par(mar = c(5.2, 11.0, 5.0, 2), family = "sans", las = 1)
  on.exit(par(old), add = TRUE)
  plot(xb, seq_along(vars), pch = 1, col = "grey35", cex = 1.4, lwd = 2,
       xlim = c(0, xmax), yaxt = "n", ylab = "",
       xlab = "Absolute standardized mean difference", main = subtitle,
       cex.main = 1.5, cex.lab = 1.15, cex.axis = 1.05,
       panel.first = grid(col = "grey90", lty = 1))
  points(xa, seq_along(vars), pch = 19, col = "black", cex = 1.25)
  axis(2, at = seq_along(vars), labels = unname(labels[vars]), las = 1, cex.axis = 1.0)
  abline(v = .10, lty = 2, col = "grey35", lwd = 1.5)
  legend("topright", c("Before", "After PSM"), pch = c(1, 19),
         col = c("grey35", "black"), bty = "n", cex = 1.05)
  mtext("The financial-structure ratio is shown as a diagnostic and is not used in the selected match.",
        side = 3, line = 1.1, cex = 0.95, col = "grey35")
}

plot_weights <- function(weight_rows, path) {
  png(path, width = 2400, height = 1550, res = 220)
  on.exit(dev.off(), add = TRUE)
  if (!nrow(weight_rows)) {
    plot.new(); title(main = "Control weights"); text(.5, .5, "No usable weighted design")
    return(invisible(NULL))
  }
  z <- weight_rows[weight_rows$treated_2016 == 0 & weight_rows$weight > 0, ]
  methods <- unique(z$method)
  old <- par(mfrow = c(1, length(methods)), mar = c(5.2, 9.5, 5.0, 1.5),
             family = "sans", las = 1)
  on.exit(par(old), add = TRUE)
  for (m in methods) {
    zm <- z[z$method == m, ]
    zm <- zm[order(zm$weight, decreasing = FALSE), ]
    short <- sub("^Universidad ", "U. ", zm$university)
    plot(zm$weight, seq_len(nrow(zm)), pch = 19, col = "black", cex = 1.15,
         yaxt = "n", ylab = "", xlab = "ATT weight", xlim = c(0, max(zm$weight) * 1.12),
         main = ifelse(m == "PSM 1:2", "PSM control weights (1:2)", "PSM control weights (1:3)"),
         cex.main = 1.3, cex.lab = 1.1, cex.axis = 1.0,
         panel.first = grid(col = "grey90", lty = 1))
    segments(0, seq_len(nrow(zm)), zm$weight, seq_len(nrow(zm)), col = "grey55", lwd = 2)
    points(zm$weight, seq_len(nrow(zm)), pch = 19, col = "black", cex = 1.15)
    axis(2, at = seq_len(nrow(zm)), labels = short, las = 1, cex.axis = 0.82)
  }
}

make_readme <- function(main, robust, ebw_rows, ps_warnings) {
  selected <- !is.null(main)
  ebw_text <- if (!is.null(ebw_rows) && nrow(ebw_rows)) {
    paste(apply(ebw_rows, 1, function(z) sprintf(
      "%s: exact_ebw_feasible=%s; mode=%s; usable=%s; control ESS=%s; reason=%s",
      z[["method"]], z[["exact_ebw_feasible"]], z[["ebw_mode"]], z[["ebw_usable"]],
      ifelse(is.na(as.numeric(z[["control_ess"]])), "NA", sprintf("%.2f", as.numeric(z[["control_ess"]]))),
      z[["failure_reason"]]
    )), collapse = "\n")
  } else "No EBW result was available."
  lines <- c(
    "PSM / ENTROPY-BALANCING DESIGN AUDIT",
    sprintf("Generated: %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")), "",
    sprintf("Official institutional input: %s", PATHS$panel),
    sprintf("Official financial input: %s", PATHS$ratios), "",
    "CAUSAL OBJECTIVE",
    "Preprocess the 2016 Gratuidad cohort and never-treated universities for the existing Dynamic DiD.",
    "The identifying condition is conditional parallel trends in untreated potential outcomes given strictly pretreatment X; PSM is not the final estimator.",
    "Additional assumptions: common support, no anticipation, SUTVA, correctly defined persistent treatment from 2016, and no post-treatment covariates.", "",
    "TREATMENT AND SAMPLE",
    "Treatment: entry to Gratuidad in 2016. Controls: never-treated universities.",
    "The 2016 cohort is imposed from the existing Dynamic DiD list; the cohort CSV identifies later-treated universities and cannot silently relabel a Dynamic-DiD 2016 university as never-treated.",
    "Later/other adopters, Universidad de Aysen, and Universidad de O'Higgins are excluded.",
    "PUCV is required to be explicitly coded as treated in 2016 in the official cohort file.",
    "The original ATT concerns the complete eligible 2016 cohort. Matching changes the estimand to the ATT for treated universities on common support.", "",
    "HEADLINE COVARIATES",
    "2015 log undergraduate enrollment; log tuition; and Asset Turnover in 2015.",
    "Accreditation, FUAS eligibility share (audited and converted explicitly from 0-100 to 0-1), programs per 1,000 students, and eligibility coverage are excluded from the propensity score and retained as visible diagnostics.",
    "This parsimonious design preserves overlap but requires a stronger conditional-parallel-trends assumption because accreditation and FUAS remain potential omitted confounders.", "",
    "MATCHING",
    "ATT nearest-neighbor matching with replacement; ratio 1:2; Mahalanobis distance on X; logit propensity score used only as a caliper.",
    "Calipers evaluated: 0.20, 0.30, 0.50, 0.75, and 1.00 SD(logit PS). Ratio 1:3 and 2012-2015 averages are robustness designs.",
    "Among candidates passing prespecified balance, ESS, retention, and overlap guardrails, the tightest caliper is selected.",
    "Selection uses only overlap, balance, ESS, retention, and weight concentration; outcomes do not participate.", "",
    "HEADLINE DECISION",
    if (selected) sprintf("Selected: %s", main$specification) else "No candidate passed the prespecified diagnostic guardrails; no headline PSM weights were authorized.",
    if (selected) sprintf("Original treated: %d; retained: %d; lost: %d; retention: %.3f.", main$n_treated_original, main$n_treated_matched, main$treated_lost, main$treated_retention) else "The grid remains evidence about lack of common support, not a menu from which to choose the least bad result.",
    if (selected) sprintf("Unique controls: %d; control ESS: %.2f; max |SMD|: %.3f; mean |SMD|: %.3f.", main$n_controls_unique, main$control_ess, main$max_abs_smd, main$mean_abs_smd) else "See diagnostics/design_grid_full.csv for retention, excluded treated universities, balance, overlap, reuse, and ESS.",
    if (selected) sprintf("Treated excluded for lack of support: %s", main$treated_unmatched) else "",
    "", "ENTROPY BALANCING",
    "PSM+EBW is only a robustness refinement. Exact entropy calibration is attempted once with a finite iteration limit; failure is reported explicitly.",
    "A prespecified ridge-regularized approximation is attempted only after exact failure and is rejected if balance/ESS/concentration guardrails fail.",
    "Direct EBW is computed separately and is never relabeled as PSM+EBW. Mechanically exact moments are not evidence of identification.", "",
    ebw_text, "",
    "INFERENCE AND DOWNSTREAM USE",
    "Weights are university-level and must be repeated unchanged over all panel years before running the existing Dynamic DiD with 2015 as base year.",
    "Initial DiD inference conditional on estimated matches/weights treats preprocessing as fixed. A full university bootstrap would re-estimate PS, matching, and weights, but is not implemented here.", "",
    "LIMITATIONS",
    "Accreditation and FUAS are intentionally diagnostic rather than matching variables; residual imbalance in them must be reported and motivates pretrend checks in the downstream DiD.",
    "PS caliper + Mahalanobis + EBW may be redundant and overly restrictive; EBW is not used to rescue structural non-overlap.",
    if (length(ps_warnings)) paste("Propensity model warnings:", paste(ps_warnings, collapse = " | ")) else "Propensity model warnings: none.", "",
    "CONFIRMATION",
    "No post-2015 outcome and no Fitch outcome was used to estimate propensity scores, form matches, calibrate weights, or select a design."
  )
  paste(lines, collapse = "\n")
}

balance_metrics <- function(balance, specification, stage) {
  z <- balance[balance$specification == specification & balance$stage == stage, ]
  core <- z[z$variable %in% CORE_X, ]
  diagnostic <- z[z$variable %in% DIAGNOSTIC_X, ]
  data.frame(
    max_abs_smd_main = if (nrow(core)) max(abs(core$smd), na.rm = TRUE) else NA_real_,
    mean_abs_smd_main = if (nrow(core)) mean(abs(core$smd), na.rm = TRUE) else NA_real_,
    max_abs_smd_diagnostic = if (nrow(diagnostic)) max(abs(diagnostic$smd), na.rm = TRUE) else NA_real_,
    min_variance_ratio_main = if (nrow(core)) min(core$variance_ratio, na.rm = TRUE) else NA_real_,
    max_variance_ratio_main = if (nrow(core)) max(core$variance_ratio, na.rm = TRUE) else NA_real_,
    max_ecdf_main = if (nrow(core)) max(core$ecdf_max, na.rm = TRUE) else NA_real_,
    stringsAsFactors = FALSE
  )
}

psm_summary_row <- function(x, method, use, balance) {
  if (is.null(x) || !nrow(x)) return(NULL)
  bm <- balance_metrics(balance, x$specification[[1L]], "after_psm")
  data.frame(
    display_order = NA_integer_, method = method, recommended_use = use,
    status = x$selection_status[[1L]], specification = x$specification[[1L]],
    covariate_period = x$covariate_spec[[1L]], ratio = x$ratio[[1L]],
    caliper_sd_logit = x$caliper[[1L]],
    n_treated_original = x$n_treated_original[[1L]],
    n_treated_used = x$n_treated_matched[[1L]],
    treated_retention = x$treated_retention[[1L]],
    n_controls_available = x$n_control_covariate_complete[[1L]],
    n_controls_used = x$n_controls_unique[[1L]], control_ess = x$control_ess[[1L]],
    max_control_reuse = x$max_control_reuse[[1L]],
    max_control_weight = x$max_control_weight[[1L]],
    raw_ps_overlap_share = x$treated_in_raw_control_ps_range[[1L]],
    exact_ebw_feasible = NA, ebw_mode = NA_character_,
    max_standardized_moment_gap = NA_real_,
    bm, stringsAsFactors = FALSE
  )
}

ebw_summary_row <- function(status_row, method, use, specification, stage,
                            balance, n_treated_original, n_treated_used,
                            n_controls_available, n_controls_used) {
  if (is.null(status_row) || !nrow(status_row)) return(NULL)
  bm <- balance_metrics(balance, specification, stage)
  data.frame(
    display_order = NA_integer_, method = method, recommended_use = use,
    status = ifelse(status_row$ebw_usable[[1L]], "usable_robustness", "infeasible"),
    specification = specification, covariate_period = "baseline_2015",
    ratio = NA_integer_, caliper_sd_logit = NA_real_,
    n_treated_original = n_treated_original, n_treated_used = n_treated_used,
    treated_retention = n_treated_used / n_treated_original,
    n_controls_available = n_controls_available, n_controls_used = n_controls_used,
    control_ess = status_row$control_ess[[1L]],
    max_control_reuse = NA_real_, max_control_weight = NA_real_,
    raw_ps_overlap_share = NA_real_,
    exact_ebw_feasible = status_row$exact_ebw_feasible[[1L]],
    ebw_mode = status_row$ebw_mode[[1L]],
    max_standardized_moment_gap = status_row$max_standardized_moment_gap[[1L]],
    bm, stringsAsFactors = FALSE
  )
}

evaluate_assets_liabilities_design <- function(institutional, treatment_audit, caliper = 0.20) {
  old_core <- CORE_X
  old_diagnostic <- DIAGNOSTIC_X
  old_all <- ALL_PRETREATMENT_VARS
  on.exit({
    assign("CORE_X", old_core, envir = .GlobalEnv)
    assign("DIAGNOSTIC_X", old_diagnostic, envir = .GlobalEnv)
    assign("ALL_PRETREATMENT_VARS", old_all, envir = .GlobalEnv)
  }, add = TRUE)

  alternative_core <- c("log_enrollment", "log_tuition", "total_assets_liabilities")
  alternative_diagnostic <- unique(c("asset_turnover", old_diagnostic))
  assign("CORE_X", alternative_core, envir = .GlobalEnv)
  assign("DIAGNOSTIC_X", alternative_diagnostic, envir = .GlobalEnv)
  assign("ALL_PRETREATMENT_VARS", unique(c(alternative_core, alternative_diagnostic)), envir = .GlobalEnv)

  audit <- build_covariate_samples(institutional, treatment_audit)
  audit <- audit[audit$covariate_spec == "baseline_2015", ]
  d <- audit[audit$analysis_eligible, c(
    "university", "university_key", "treatment_year", "treated_2016",
    "covariate_spec", "n_pre_years", ALL_PRETREATMENT_VARS
  )]
  d <- fit_propensity(d)
  attr(d, "n_treated_original") <- sum(audit$target_treated_population)
  attr(d, "n_control_original") <- sum(audit$headline_eligible_status & audit$treated_2016 == 0)
  attr(d, "treated_missing_x") <- audit$university[
    audit$target_treated_population & !audit$analysis_eligible
  ]
  list(
    ratio_1to2 = run_match(d, 2L, caliper, "baseline_2015_assets_liabilities"),
    ratio_1to3 = run_match(d, 3L, caliper, "baseline_2015_assets_liabilities")
  )
}

balance_value <- function(balance, specification, stage, variable, field = "smd") {
  z <- balance[
    balance$specification == specification & balance$stage == stage & balance$variable == variable,
    field
  ]
  if (!length(z)) NA_real_ else as.numeric(z[[1L]])
}

make_paper_table <- function(columns) {
  statistics <- c(
    "Core covariates", "Treated universities: original", "Treated universities: retained",
    "Treated retention (%)", "Eligible controls", "Controls used", "Control ESS",
    "Caliper (SD logit PS)", "Max |SMD|: core covariates",
    "SMD: Total Assets / Total Liabilities", "Max control reuse", "Selected main weights"
  )
  out <- data.frame(`Outcome / statistic` = statistics, check.names = FALSE, stringsAsFactors = FALSE)
  for (column in columns) {
    out[[column$label]] <- c(
      column$core_label,
      column$n_treated_original, column$n_treated_used,
      ifelse(is.na(column$retention), NA, 100 * column$retention),
      column$n_controls_available, column$n_controls_used, column$ess,
      column$caliper, column$max_smd, column$smd_assets_liabilities,
      column$max_reuse, column$selected
    )
  }
  out
}

main <- function() {
  validate_inputs()
  validate_output_root()
  msg("[1/8] Reading official inputs only")
  mapping <- load_name_map(PATHS$mapping)
  cohorts <- load_cohorts(PATHS$cohorts, mapping)
  institutional <- collapse_institutional_panel(read_csv_strict(PATHS$panel), mapping)
  financial_panel <- load_financial_panel(PATHS$ratios, mapping)
  institutional <- merge_financial_covariates(institutional, financial_panel)
  financial <- unique(financial_panel[c("university", "university_key")])
  treatment_audit <- build_treatment_audit(financial, cohorts)
  forced_keys <- norm_name(standardize_names(TREATED_2016_DYNAMIC, mapping))
  bad_forced <- forced_keys[!forced_keys %in% treatment_audit$university_key[
    treatment_audit$treatment_status == "treated_2016"
  ]]
  if (length(bad_forced)) stopf("Cohorte Dynamic DiD no preservada para: %s", paste(bad_forced, collapse = ", "))
  sample_audit <- build_covariate_samples(institutional, treatment_audit)

  msg("[2/8] Building university-level pretreatment samples")
  specs <- c("baseline_2015", "average_2012_2015")
  ps_data <- list()
  ps_warnings <- character()
  for (s in specs) {
    audit_s <- sample_audit[sample_audit$covariate_spec == s, ]
    d <- sample_audit[sample_audit$covariate_spec == s & sample_audit$analysis_eligible,
                      c("university", "university_key", "treatment_year", "treated_2016",
                        "covariate_spec", "n_pre_years", ALL_PRETREATMENT_VARS)]
    if (!nrow(d)) stopf("No hay muestra elegible para %s", s)
    d <- fit_propensity(d)
    attr(d, "n_treated_original") <- sum(audit_s$target_treated_population)
    attr(d, "n_control_original") <- sum(audit_s$headline_eligible_status & audit_s$treated_2016 == 0)
    attr(d, "treated_missing_x") <- audit_s$university[
      audit_s$target_treated_population & !audit_s$analysis_eligible
    ]
    ps_warnings <- c(ps_warnings, attr(d, "ps_warnings"))
    ps_data[[s]] <- d
  }
  pretreat <- do.call(rbind, ps_data)
  rownames(pretreat) <- NULL

  msg("[3/8] Evaluating the prespecified caliper/ratio grid")
  runs <- list(); k <- 0L
  for (s in specs) for (ratio in RATIOS) for (caliper in CALIPERS) {
    k <- k + 1L
    runs[[k]] <- run_match(ps_data[[s]], ratio, caliper, s)
  }
  grid <- do.call(rbind, lapply(runs, `[[`, "grid"))
  overlap <- do.call(rbind, lapply(runs, `[[`, "overlap"))
  pairs <- do.call(rbind, lapply(runs, `[[`, "pairs"))
  reuse <- do.call(rbind, lapply(runs, `[[`, "reuse"))
  balance <- do.call(rbind, lapply(runs, `[[`, "balance"))
  weight_diag <- do.call(rbind, lapply(runs, `[[`, "weights"))

  msg("[4/8] Applying diagnostic selection rules without outcomes")
  selected_main <- select_design(grid, "baseline_2015", MAIN_RATIO)
  selected_avg <- select_design(grid, "average_2012_2015", MAIN_RATIO)
  selected_13 <- NULL
  if (!is.null(selected_main)) {
    selected_13 <- grid[grid$covariate_spec == "baseline_2015" & grid$ratio == ROBUST_RATIO &
                          grid$caliper == selected_main$caliper, ]
  }
  if (!is.null(selected_main)) {
    selected_main$design_role <- "headline_PSM"
    selected_main$selection_status <- "selected"
  }
  if (!is.null(selected_13) && nrow(selected_13)) {
    selected_13$design_role <- "robustness_ratio_1to3_same_caliper"
    selected_13$selection_status <- "selected_for_comparison"
  }
  if (!is.null(selected_avg)) {
    selected_avg$design_role <- "robustness_average_2012_2015"
    selected_avg$selection_status <- "selected"
  }
  main_failure <- if (is.null(selected_main)) empty_selected(
    "No baseline-2015 1:2 candidate passed balance/ESS/retention/overlap guardrails"
  ) else NULL
  if (!is.null(main_failure)) main_failure$design_role <- "headline_PSM"
  selected_rows <- Filter(Negate(is.null), list(selected_main, selected_13, selected_avg))
  selected_diag <- bind_rows_fill(
    main_failure,
    if (length(selected_rows)) do.call(bind_rows_fill, selected_rows) else NULL
  )

  find_run <- function(specification) runs[[which(vapply(runs, function(x) x$grid$specification == specification, logical(1)))[1L]]]
  main_sample <- ps_data[["baseline_2015"]][0, ]
  main_sample$psm_weight <- numeric(); main_sample$specification <- character()
  robust_sample <- main_sample; robust_sample$robustness_type <- character()
  main_weights <- data.frame(
    university = character(), university_key = character(), treated_2016 = integer(),
    weight = numeric(), specification = character(), stringsAsFactors = FALSE
  )
  robust_13_weights <- main_weights
  selected_run <- NULL
  if (!is.null(selected_main)) {
    selected_run <- find_run(selected_main$specification)
    d <- selected_run$data[selected_run$data$psm_weight > 0, ]
    d$specification <- selected_main$specification
    main_sample <- d
    main_weights <- d[c("university", "university_key", "treated_2016", "psm_weight", "specification")]
    names(main_weights)[names(main_weights) == "psm_weight"] <- "weight"
  }
  robust_parts <- list()
  if (!is.null(selected_13) && nrow(selected_13)) {
    rr <- find_run(selected_13$specification)
    z <- rr$data[rr$data$psm_weight > 0, ]; z$robustness_type <- "ratio_1to3_same_caliper"
    z$specification <- selected_13$specification; robust_parts[[length(robust_parts) + 1L]] <- z
    robust_13_weights <- z[c("university", "university_key", "treated_2016", "psm_weight", "specification")]
    names(robust_13_weights)[names(robust_13_weights) == "psm_weight"] <- "weight"
  }
  if (!is.null(selected_avg)) {
    rr <- find_run(selected_avg$specification)
    z <- rr$data[rr$data$psm_weight > 0, ]; z$robustness_type <- "average_2012_2015_ratio_1to2"
    z$specification <- selected_avg$specification; robust_parts[[length(robust_parts) + 1L]] <- z
  }
  if (length(robust_parts)) robust_sample <- do.call(rbind, robust_parts)

  msg("[4b/8] Comparing Total Assets / Total Liabilities designs")
  assets_liabilities_designs <- evaluate_assets_liabilities_design(
    institutional, treatment_audit,
    caliper = if (!is.null(selected_main)) selected_main$caliper[[1L]] else 0.20
  )

  msg("[5/8] Entropy balancing as separate robustness")
  ebw_output <- list(); ebw_weight_rows <- list()
  add_ebw <- function(d, base_w, specification, method) {
    ans <- entropy_calibrate(d, base_w, method)
    row <- data.frame(
      specification = specification, method = method,
      exact_ebw_feasible = isTRUE(ans$exact_feasible), ebw_mode = ans$mode,
      ebw_usable = isTRUE(ans$feasible), failure_reason = ans$reason,
      control_ess = ess(ans$weights[d$treated_2016 == 0]),
      max_standardized_moment_gap = ans$max_standardized_gap %||% NA_real_,
      stringsAsFactors = FALSE
    )
    if (isTRUE(ans$feasible)) {
      z <- d[c("university", "university_key", "treated_2016", "covariate_spec")]
      z$weight <- ans$weights; z$specification <- specification; z$method <- method
      ebw_weight_rows[[length(ebw_weight_rows) + 1L]] <<- z[z$weight > 0, ]
      weight_diag <<- rbind(weight_diag,
                            weight_summary(ans$weights[d$treated_2016 == 0], specification, method))
      balance <<- rbind(balance, balance_table(d, ans$weights, paste0("after_", method), specification))
    }
    row
  }
  if (!is.null(selected_run)) {
    ebw_output[[length(ebw_output) + 1L]] <- add_ebw(
      selected_run$data, selected_run$data$psm_weight,
      selected_main$specification, "PSM_plus_EBW"
    )
  }
  direct_d <- ps_data[["baseline_2015"]]
  ebw_output[[length(ebw_output) + 1L]] <- add_ebw(
    direct_d, rep(1, nrow(direct_d)), "baseline_2015__direct_EBW", "direct_EBW"
  )
  ebw_status <- do.call(rbind, ebw_output)
  ebw_weights <- if (length(ebw_weight_rows)) do.call(rbind, ebw_weight_rows) else data.frame(
    university = character(), university_key = character(), treated_2016 = integer(),
    covariate_spec = character(), weight = numeric(), specification = character(),
    method = character(), stringsAsFactors = FALSE
  )
  selected_diag <- bind_rows_fill(selected_diag, transform(ebw_status, design_role = method,
                                                           selection_status = ifelse(ebw_usable, "usable_robustness", "infeasible")))

  psm_ebw_status <- ebw_status[ebw_status$method == "PSM_plus_EBW", ]
  direct_ebw_status <- ebw_status[ebw_status$method == "direct_EBW", ]

  make_psm_column <- function(label, core_label, run, selected = "No") {
    g <- run$grid[1, ]
    b <- run$balance
    list(
      label = label, core_label = core_label,
      n_treated_original = g$n_treated_original, n_treated_used = g$n_treated_matched,
      retention = g$treated_retention,
      n_controls_available = g$n_control_covariate_complete,
      n_controls_used = g$n_controls_unique, ess = g$control_ess,
      caliper = g$caliper, max_smd = g$max_abs_smd, mean_smd = g$mean_abs_smd,
      min_vr = g$min_variance_ratio, max_ecdf = g$max_ecdf_difference,
      smd_assets_liabilities = balance_value(b, g$specification, "after_psm", "total_assets_liabilities"),
      smd_accreditation = balance_value(b, g$specification, "after_psm", "accreditation_years"),
      smd_fuas = balance_value(b, g$specification, "after_psm", "fuas_share"),
      max_reuse = g$max_control_reuse, max_weight = g$max_control_weight,
      selected = selected
    )
  }

  run_13 <- if (!is.null(selected_13) && nrow(selected_13)) find_run(selected_13$specification) else NULL
  paper_columns <- list(
    make_psm_column("(1) PSM 1:2", "Enrollment + tuition + asset turnover", selected_run, "Yes"),
    make_psm_column("(2) PSM 1:3", "Enrollment + tuition + asset turnover", run_13, "No"),
    make_psm_column("(3) Assets/liabilities 1:2", "Enrollment + tuition + total assets/liabilities",
                    assets_liabilities_designs$ratio_1to2, "No"),
    make_psm_column("(4) Assets/liabilities 1:3", "Enrollment + tuition + total assets/liabilities",
                    assets_liabilities_designs$ratio_1to3, "No")
  )
  if (nrow(psm_ebw_status) && isTRUE(psm_ebw_status$ebw_usable[[1L]])) {
    ebw_bm <- balance_metrics(balance, selected_main$specification[[1L]], "after_PSM_plus_EBW")
    paper_columns[[5L]] <- list(
      label = "(5) PSM 1:2 + EBW", core_label = "Enrollment + tuition + asset turnover",
      n_treated_original = selected_main$n_treated_original[[1L]],
      n_treated_used = selected_main$n_treated_matched[[1L]],
      retention = selected_main$treated_retention[[1L]],
      n_controls_available = selected_main$n_control_covariate_complete[[1L]],
      n_controls_used = selected_main$n_controls_unique[[1L]],
      ess = psm_ebw_status$control_ess[[1L]], caliper = selected_main$caliper[[1L]],
      max_smd = ebw_bm$max_abs_smd_main[[1L]], mean_smd = ebw_bm$mean_abs_smd_main[[1L]],
      min_vr = ebw_bm$min_variance_ratio_main[[1L]], max_ecdf = ebw_bm$max_ecdf_main[[1L]],
      smd_assets_liabilities = balance_value(balance, selected_main$specification[[1L]],
                                             "after_PSM_plus_EBW", "total_assets_liabilities"),
      smd_accreditation = balance_value(balance, selected_main$specification[[1L]],
                                        "after_PSM_plus_EBW", "accreditation_years"),
      smd_fuas = balance_value(balance, selected_main$specification[[1L]],
                              "after_PSM_plus_EBW", "fuas_share"),
      max_reuse = NA_real_, max_weight = NA_real_, selected = "Robustness"
    )
  }
  paper_table <- make_paper_table(paper_columns)
  write_csv(paper_table, file.path(PATHS$results, "Table_PSM_Design.csv"))

  weight_parts <- list()
  if (nrow(main_weights)) {
    z <- main_weights; z$method <- "PSM_1to2_main"; z$selected_main <- TRUE
    weight_parts[[length(weight_parts) + 1L]] <- z
  }
  if (nrow(robust_13_weights)) {
    z <- robust_13_weights; z$method <- "PSM_1to3_robustness"; z$selected_main <- FALSE
    weight_parts[[length(weight_parts) + 1L]] <- z
  }
  if (nrow(ebw_weights)) {
    z <- ebw_weights[c("university", "university_key", "treated_2016", "weight", "specification", "method")]
    z$selected_main <- FALSE
    weight_parts[[length(weight_parts) + 1L]] <- z
  }
  weights_output <- do.call(rbind, weight_parts)
  weights_output <- weights_output[c(
    "university", "university_key", "treated_2016", "method", "weight",
    "specification", "selected_main"
  )]
  write_csv(weights_output, file.path(PATHS$results, "PSM_weights.csv"))

  msg("[6/8] Static integrity checks")
  if (nrow(main_weights)) {
    if (any(!is.finite(main_weights$weight) | main_weights$weight < 0)) stopf("Pesos PSM inv\u00e1lidos")
    if (any(main_weights$university_key %in% treatment_audit$university_key[
      treatment_audit$treatment_status == "other_treated_cohort"])) stopf("Later adopter en weights main")
    if (any(main_weights$university_key %in% norm_name(EXCLUDED_UNIVERSITIES))) stopf("Universidad excluida en weights main")
  }
  if (!norm_name(PUCV) %in% sample_audit$university_key[sample_audit$treatment_status == "treated_2016"]) {
    stopf("PUCV no est\u00e1 correctamente tratada en 2016")
  }

  msg("[7/8] Plots")
  plot_overlap(
    pretreat,
    if (!is.null(selected_main)) selected_main$specification else NULL,
    main_weights,
    file.path(PATHS$plots, "propensity_score_overlap.png")
  )
  plot_love(balance, if (!is.null(selected_main)) selected_main$specification else NULL,
            file.path(PATHS$plots, "covariate_balance.png"), "PSM covariate balance")
  plot_rows <- list()
  if (nrow(main_weights)) {
    z <- main_weights[c("university", "treated_2016", "weight")]
    z$method <- "PSM 1:2"; plot_rows[[1L]] <- z
  }
  if (nrow(robust_13_weights)) {
    z <- robust_13_weights[c("university", "treated_2016", "weight")]
    z$method <- "PSM 1:3"
    plot_rows[[length(plot_rows) + 1L]] <- z
  }
  plot_weights(if (length(plot_rows)) do.call(rbind, plot_rows) else data.frame(),
               file.path(PATHS$plots, "control_weights.png"))

  msg("[8/8] Execution summary and completion")
  alt12 <- assets_liabilities_designs$ratio_1to2$grid[1, ]
  alt13 <- assets_liabilities_designs$ratio_1to3$grid[1, ]
  summary_lines <- c(
    "Propensity Score Matching Analysis",
    "==================================",
    sprintf("Run date: %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
    sprintf("Input panel: %s", basename(PATHS$panel)),
    sprintf("Financial ratios: %s", basename(PATHS$ratios)),
    "Treatment cohort: 2016",
    "Controls: never-treated universities",
    "Baseline year: 2015",
    "",
    "Selected design",
    "---------------",
    "ATT nearest-neighbor matching with replacement; ratio 1:2.",
    "Mahalanobis distance on log enrollment, log tuition, and Asset Turnover.",
    sprintf("Logit propensity-score caliper: %.2f SD.", selected_main$caliper[[1L]]),
    sprintf("Treated retained: %d/%d.", selected_main$n_treated_matched[[1L]], selected_main$n_treated_original[[1L]]),
    sprintf("Controls used: %d; control ESS: %.2f.", selected_main$n_controls_unique[[1L]], selected_main$control_ess[[1L]]),
    sprintf("Max |SMD| in core X: %.3f.", selected_main$max_abs_smd[[1L]]),
    "",
    "Design comparisons",
    "------------------",
    sprintf("PSM 1:3 retains %d/%d treated, uses %d controls, ESS %.2f, max |SMD| %.3f.",
            selected_13$n_treated_matched[[1L]], selected_13$n_treated_original[[1L]],
            selected_13$n_controls_unique[[1L]], selected_13$control_ess[[1L]], selected_13$max_abs_smd[[1L]]),
    sprintf("Assets/liabilities 1:2 retains %d/%d treated, ESS %.2f, max |SMD| %.3f.",
            alt12$n_treated_matched[[1L]], alt12$n_treated_original[[1L]], alt12$control_ess[[1L]], alt12$max_abs_smd[[1L]]),
    sprintf("Assets/liabilities 1:3 retains %d/%d treated, ESS %.2f, max |SMD| %.3f.",
            alt13$n_treated_matched[[1L]], alt13$n_treated_original[[1L]], alt13$control_ess[[1L]], alt13$max_abs_smd[[1L]]),
    "The assets/liabilities designs are comparison designs only and are not used for main weights.",
    "No post-2015 outcome was used to form or select matches.",
    if (length(unique(ps_warnings))) paste("Propensity-model warnings:", paste(unique(ps_warnings), collapse = " | ")) else "Propensity-model warnings: none."
  )
  writeLines(summary_lines, file.path(PATHS$results, "PSM_execution_summary.txt"), useBytes = TRUE)
  msg("Done. Design status: %s", ifelse(is.null(selected_main), "NO DEFENSIBLE HEADLINE PSM", selected_main$specification))
}

if (sys.nframe() == 0L) main()
