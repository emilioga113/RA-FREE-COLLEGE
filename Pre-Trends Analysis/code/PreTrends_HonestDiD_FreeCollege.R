#!/usr/bin/env Rscript

# Pre-trends, anticipation, pretrends power, and HonestDiD sensitivity module.
# Main workbooks deliberately reproduce the normal DiD table architecture.

options(stringsAsFactors = FALSE, scipen = 999)

user_library <- file.path(
  Sys.getenv("USERPROFILE"), "AppData", "Local", "R", "win-library",
  paste(R.version$major, strsplit(R.version$minor, "\\.")[[1]][1], sep = ".")
)
dir.create(user_library, recursive = TRUE, showWarnings = FALSE)
.libPaths(unique(c(user_library, .libPaths())))

install_if_missing <- function(package, github = NULL) {
  if (requireNamespace(package, quietly = TRUE)) return(invisible(TRUE))
  if (is.null(github)) {
    install.packages(package, repos = "https://cloud.r-project.org", lib = user_library)
  } else {
    if (!requireNamespace("remotes", quietly = TRUE)) {
      install.packages("remotes", repos = "https://cloud.r-project.org", lib = user_library)
    }
    remotes::install_github(github, lib = user_library, upgrade = "never")
  }
  if (!requireNamespace(package, quietly = TRUE)) stop("Could not install ", package)
}

install_if_missing("synthdid")
install_if_missing("openxlsx")
install_if_missing("zip")
install_if_missing("HonestDiD")
install_if_missing("pretrends", "jonathandroth/pretrends")
install_if_missing("ggplot2")

parse_args <- function(args = commandArgs(trailingOnly = TRUE)) {
  out <- list(root = Sys.getenv("FREE_COLLEGE_DIR", unset = "."), boot_reps = 500L, seed = 321L)
  for (arg in args) {
    if (startsWith(arg, "--root=")) out$root <- sub("^--root=", "", arg)
    if (startsWith(arg, "--boot-reps=")) out$boot_reps <- as.integer(sub("^--boot-reps=", "", arg))
    if (startsWith(arg, "--seed=")) out$seed <- as.integer(sub("^--seed=", "", arg))
  }
  if (!is.finite(out$boot_reps) || out$boot_reps < 1L) stop("--boot-reps must be positive")
  if (!is.finite(out$seed)) stop("--seed must be finite")
  out$root <- normalizePath(out$root, winslash = "/", mustWork = TRUE)
  out
}

args <- parse_args()
root <- args$root
analysis_dir <- file.path(root, "Pre-Trends Analysis")
code_dir <- file.path(analysis_dir, "code")
results_dir <- file.path(analysis_dir, "Results")
plots_dir <- file.path(analysis_dir, "Plots_PreTrends")
dir.create(code_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(plots_dir, recursive = TRUE, showWarnings = FALSE)

# Reuse the official paths, cleaning, transformations, and samples already
# defined by the existing SDID program. Sourcing does not execute SDID main().
sdid_script <- file.path(root, "Difference-in-Difference Analysis", "code", "Synthetic_DiD_FreeCollege.R")
if (!file.exists(sdid_script)) stop("Missing SDID reference script: ", sdid_script)
sdid <- new.env(parent = globalenv())
sys.source(sdid_script, envir = sdid)
paths <- sdid$resolve_paths(root)
sdid$validate_inputs(paths)
loaded <- sdid$load_analysis_data(paths)
panel <- loaded$panel
OUTCOME_SPECS <- sdid$OUTCOME_SPECS

as_num <- sdid$as_num
panel$log_enrollment_undergrad <- ifelse(
  as_num(panel[["Matrícula pregrado"]]) > 0,
  log(as_num(panel[["Matrícula pregrado"]])),
  NA_real_
)
panel$eligibility_proxy_10 <- as_num(panel[["% elegibles gratuidad (proxy escolar)"]]) / 10
panel$accreditation_years <- as_num(panel[["Acreditación (años)"]])
panel$eligibility_coverage_10 <- as_num(panel[["Cobertura elegibilidad proxy (%)"]]) / 10

excluded_closed <- sdid$norm_name(
  sdid$standardize_names(sdid$EXCLUDED_CLOSED_UNIVERSITIES, loaded$name_map)
)
sample_panels <- list(
  original = panel,
  restricted = panel[!panel$id_norm %in% excluded_closed, , drop = FALSE]
)
sample_labels <- c(
  original = "With closure-risk controls",
  restricted = "Excluding closure-risk controls"
)

BASE_YEAR <- 2013L
SPECIFICATIONS <- data.frame(
  year = c(2012L, 2014L, 2015L),
  specification = c("pretrend_2012", "anticipation_2014", "anticipation_2015"),
  specification_label = c(
    "(1) 2012 Pre-trend", "(2) 2014 Anticipation", "(3) 2015 Anticipation"
  ),
  stringsAsFactors = FALSE
)
CONTROL_NAMES <- c(
  "log_enrollment_undergrad", "eligibility_proxy_10",
  "accreditation_years", "eligibility_coverage_10"
)

bind_fill <- function(rows) {
  if (!length(rows)) return(data.frame())
  columns <- unique(unlist(lapply(rows, names), use.names = FALSE))
  rows <- lapply(rows, function(x) {
    for (column in setdiff(columns, names(x))) x[[column]] <- NA
    x[columns]
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

read_csv_utf8 <- function(path) {
  x <- read.csv(path, check.names = FALSE, fileEncoding = "UTF-8-BOM")
  names(x)[1] <- sub("^\\ufeff", "", names(x)[1])
  x
}

write_csv_utf8 <- function(x, path) {
  write.csv(x, path, row.names = FALSE, na = "", fileEncoding = "UTF-8")
}

stars <- function(p) {
  if (!is.finite(p)) return("")
  if (p < 0.01) return("***")
  if (p < 0.05) return("**")
  if (p < 0.10) return("*")
  ""
}

prepare_change_data <- function(panel_sample, spec, transformed, target_year) {
  y <- as_num(panel_sample[[spec$source]])
  if (transformed) y <- sdid$transform_values(y, spec$transform)$value
  d <- data.frame(
    id_norm = panel_sample$id_norm,
    treated = panel_sample$treated,
    year = panel_sample$year,
    y = y,
    log_enrollment_undergrad = panel_sample$log_enrollment_undergrad,
    eligibility_proxy_10 = panel_sample$eligibility_proxy_10,
    accreditation_years = panel_sample$accreditation_years,
    eligibility_coverage_10 = panel_sample$eligibility_coverage_10,
    stringsAsFactors = FALSE
  )
  target <- d[d$year == target_year, c("id_norm", "treated", "y", CONTROL_NAMES), drop = FALSE]
  base <- d[d$year == BASE_YEAR, c("id_norm", "treated", "y", CONTROL_NAMES), drop = FALSE]
  names(target)[-1L] <- paste0(names(target)[-1L], "_target")
  names(base)[-1L] <- paste0(names(base)[-1L], "_base")
  use <- merge(target, base, by = "id_norm", all = FALSE, sort = FALSE)
  if (!nrow(use)) return(data.frame())
  if (any(use$treated_target != use$treated_base, na.rm = TRUE)) stop("Treatment status changed within university")
  out <- data.frame(
    id_norm = use$id_norm,
    treated = use$treated_target,
    dy = use$y_target - use$y_base,
    dlog_enrollment = use$log_enrollment_undergrad_target - use$log_enrollment_undergrad_base,
    deligibility_proxy_10 = use$eligibility_proxy_10_target - use$eligibility_proxy_10_base,
    daccreditation_years = use$accreditation_years_target - use$accreditation_years_base,
    deligibility_coverage_10 = use$eligibility_coverage_10_target - use$eligibility_coverage_10_base,
    stringsAsFactors = FALSE
  )
  required <- c(
    "treated", "dy", "dlog_enrollment", "deligibility_proxy_10",
    "daccreditation_years", "deligibility_coverage_10"
  )
  out[stats::complete.cases(out[required]), , drop = FALSE]
}

fit_change_regression <- function(change_data, cluster_table, boot_reps, seed) {
  empty <- list(
    status = "estimation_failed", coef = NA_real_, se = NA_real_, pvalue = NA_real_,
    ci_low = NA_real_, ci_high = NA_real_, nobs = nrow(change_data),
    n_clusters = nrow(change_data), adj_r2 = NA_real_, boot_reps_valid = 0L
  )
  if (!nrow(change_data) || length(unique(change_data$treated)) < 2L) {
    empty$status <- "no_valid_treated_control"
    return(empty)
  }
  X <- cbind(
    `(Intercept)` = 1,
    treated = as.numeric(change_data$treated),
    dlog_enrollment = change_data$dlog_enrollment,
    deligibility_proxy_10 = change_data$deligibility_proxy_10,
    daccreditation_years = change_data$daccreditation_years,
    deligibility_coverage_10 = change_data$deligibility_coverage_10
  )
  y <- as.numeric(change_data$dy)
  if (qr(X)$rank < ncol(X) || nrow(X) <= ncol(X)) {
    empty$status <- "singular_regression"
    return(empty)
  }
  fit <- lm.fit(X, y)
  coef <- unname(fit$coefficients[["treated"]])
  rss <- sum(fit$residuals^2)
  tss <- sum((y - mean(y))^2)
  r2 <- if (tss > 0) 1 - rss / tss else NA_real_
  adj_r2 <- if (is.finite(r2) && fit$df.residual > 0) {
    1 - (1 - r2) * (nrow(X) - 1) / fit$df.residual
  } else NA_real_

  cluster_table <- unique(cluster_table[c("id_norm", "treated")])
  cluster_table <- cluster_table[order(cluster_table$treated, cluster_table$id_norm), , drop = FALSE]
  event_index <- match(change_data$id_norm, cluster_table$id_norm)
  treated_index <- which(cluster_table$treated == 1L)
  control_index <- which(cluster_table$treated == 0L)
  draws <- rep(NA_real_, boot_reps)
  set.seed(seed)
  for (b in seq_len(boot_reps)) {
    cluster_weights <- integer(nrow(cluster_table))
    cluster_weights[treated_index] <- tabulate(
      sample.int(length(treated_index), length(treated_index), replace = TRUE),
      nbins = length(treated_index)
    )
    cluster_weights[control_index] <- tabulate(
      sample.int(length(control_index), length(control_index), replace = TRUE),
      nbins = length(control_index)
    )
    w <- cluster_weights[event_index]
    active <- is.finite(w) & w > 0
    if (!any(active & change_data$treated == 1L) || !any(active & change_data$treated == 0L)) next
    if (sum(active) <= ncol(X) || qr(X[active, , drop = FALSE] * sqrt(w[active]))$rank < ncol(X)) next
    boot_fit <- try(lm.wfit(X, y, w = w), silent = TRUE)
    if (!inherits(boot_fit, "try-error") && is.finite(boot_fit$coefficients[["treated"]])) {
      draws[[b]] <- unname(boot_fit$coefficients[["treated"]])
    }
  }
  valid <- draws[is.finite(draws)]
  se <- if (length(valid) >= 2L) stats::sd(valid) else NA_real_
  ci <- if (length(valid) >= 2L) {
    unname(stats::quantile(valid, probs = c(0.025, 0.975), type = 7, na.rm = TRUE))
  } else c(NA_real_, NA_real_)
  pvalue <- if (is.finite(coef) && is.finite(se) && se > 0) {
    2 * stats::pnorm(-abs(coef / se))
  } else NA_real_
  list(
    status = if (is.finite(se)) "ok" else "inference_unavailable",
    coef = coef, se = se, pvalue = pvalue, ci_low = ci[[1]], ci_high = ci[[2]],
    nobs = nrow(change_data), n_clusters = nrow(change_data), adj_r2 = adj_r2,
    boot_reps_valid = length(valid)
  )
}

estimate_main_tables <- function() {
  level_rows <- list(); transformed_rows <- list()
  for (sample_key in names(sample_panels)) {
    panel_sample <- sample_panels[[sample_key]]
    cluster_table <- unique(panel_sample[c("id_norm", "treated")])
    for (transformed in c(FALSE, TRUE)) {
      for (i in seq_len(nrow(OUTCOME_SPECS))) {
        spec <- as.list(OUTCOME_SPECS[i, , drop = FALSE])
        spec <- lapply(spec, function(x) x[[1]])
        for (j in seq_len(nrow(SPECIFICATIONS))) {
          target <- SPECIFICATIONS[j, , drop = FALSE]
          message(
            "[Pre-Trends] ", sample_labels[[sample_key]], " | ", spec$outcome,
            " | ", if (transformed) spec$scale else "level", " | ", target$year
          )
          change_data <- prepare_change_data(panel_sample, spec, transformed, target$year)
          seed_offset <- args$seed + i * 100L + j
          # Identity transformations are the same outcome and therefore reuse
          # the same bootstrap draw sequence, as in the normal DiD pipeline.
          if (transformed && spec$transform != "identity") seed_offset <- seed_offset + 10000L
          if (sample_key == "restricted") seed_offset <- seed_offset + 20000L
          fit <- fit_change_regression(change_data, cluster_table, args$boot_reps, seed_offset)
          raw <- data.frame(
            outcome = spec$outcome,
            scale = if (transformed) spec$scale else "level",
            sample = sample_key,
            sample_label = unname(sample_labels[[sample_key]]),
            specification = target$specification,
            specification_label = target$specification_label,
            status = fit$status,
            coef = fit$coef, se = fit$se, pvalue = fit$pvalue,
            ci_low = fit$ci_low, ci_high = fit$ci_high,
            nobs = fit$nobs, n_clusters = fit$n_clusters, adj_r2 = fit$adj_r2,
            stringsAsFactors = FALSE
          )
          if (transformed) {
            transformed_rows[[length(transformed_rows) + 1L]] <- raw
          } else {
            level_rows[[length(level_rows) + 1L]] <- raw
          }
        }
      }
    }
  }
  list(levels = bind_fill(level_rows), transformed = bind_fill(transformed_rows))
}

make_paper_table <- function(raw_results) {
  rows <- list()
  for (outcome in OUTCOME_SPECS$outcome) {
    block <- data.frame(
      `Outcome / statistic` = c(outcome, "(SE)", "Observations", "Adjusted R2"),
      check.names = FALSE, stringsAsFactors = FALSE
    )
    for (sample_key in names(sample_panels)) {
      for (j in seq_len(nrow(SPECIFICATIONS))) {
        spec_key <- SPECIFICATIONS$specification[[j]]
        r <- raw_results[
          raw_results$outcome == outcome & raw_results$sample == sample_key &
            raw_results$specification == spec_key,
          , drop = FALSE
        ]
        if (nrow(r) != 1L) stop("Expected one result for ", outcome, " / ", sample_key, " / ", spec_key)
        header <- paste0(sample_labels[[sample_key]], ": ", SPECIFICATIONS$specification_label[[j]])
        coef_text <- if (is.finite(r$coef)) paste0(sprintf("%.3f", r$coef), stars(r$pvalue)) else "--"
        se_text <- if (is.finite(r$se)) paste0("(", sprintf("%.3f", r$se), ")") else ""
        n_text <- if (is.finite(r$nobs) && r$nobs > 0) as.character(as.integer(r$nobs)) else "--"
        r2_text <- if (is.finite(r$adj_r2)) sprintf("%.3f", r$adj_r2) else "--"
        block[[header]] <- c(coef_text, se_text, n_text, r2_text)
      }
    }
    rows[[length(rows) + 1L]] <- block
  }
  footer <- data.frame(
    `Outcome / statistic` = c("University FE", "Year FE", "Controls", "SE clustered by university"),
    check.names = FALSE, stringsAsFactors = FALSE
  )
  for (sample_key in names(sample_panels)) {
    for (j in seq_len(nrow(SPECIFICATIONS))) {
      header <- paste0(sample_labels[[sample_key]], ": ", SPECIFICATIONS$specification_label[[j]])
      footer[[header]] <- c("By differencing", "2013 reference", "Yes", "Yes")
    }
  }
  rows[[length(rows) + 1L]] <- footer
  do.call(rbind, rows)
}

write_main_workbook <- function(raw_results, path) {
  raw_columns <- c(
    "outcome", "scale", "sample", "sample_label", "specification",
    "specification_label", "status", "coef", "se", "pvalue", "ci_low",
    "ci_high", "nobs", "n_clusters", "adj_r2"
  )
  raw_results <- raw_results[raw_columns]
  paper <- make_paper_table(raw_results)
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "paper_table")
  openxlsx::addWorksheet(wb, "raw_results")
  # Match the normal DiD workbooks exactly: bold, centered headers, default
  # column widths, no fill, no borders, and no frozen panes.
  header_style <- openxlsx::createStyle(
    textDecoration = "bold", halign = "center", valign = "top"
  )
  openxlsx::writeData(wb, "paper_table", paper, rowNames = FALSE, keepNA = FALSE, headerStyle = header_style)
  openxlsx::writeData(wb, "raw_results", raw_results, rowNames = FALSE, keepNA = FALSE, headerStyle = header_style)
  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
  sdid$sanitize_openxlsx_package(path)
  invisible(paper)
}

latex_escape <- function(x) {
  x <- gsub("\\\\", "\\\\textbackslash{}", as.character(x))
  gsub("([&%$#_{}])", "\\\\\\1", x, perl = TRUE)
}

write_latex_table <- function(paper, path, transformed) {
  caption <- if (transformed) {
    "Pre-Trends and Anticipation Estimates (Transformed Outcomes)"
  } else {
    "Pre-Trends and Anticipation Estimates (Levels)"
  }
  con <- file(path, open = "wt", encoding = "UTF-8")
  on.exit(close(con), add = TRUE)
  writeLines(c(
    "\\begin{table}[!htbp]", "\\centering", "\\scriptsize",
    paste0("\\caption{", caption, "}"),
    paste0("\\label{tab:pretrends_", if (transformed) "transformed" else "levels", "}"),
    "\\resizebox{\\textwidth}{!}{%", "\\begin{tabular}{lcccccc}", "\\toprule",
    paste0(paste(latex_escape(names(paper)), collapse = " & "), " \\\\"), "\\midrule"
  ), con)
  for (i in seq_len(nrow(paper))) {
    writeLines(paste0(paste(latex_escape(unlist(paper[i, ], use.names = FALSE)), collapse = " & "), " \\\\"), con)
  }
  writeLines(c(
    "\\bottomrule", "\\end{tabular}}", "\\begin{minipage}{0.98\\textwidth}",
    paste0(
      "\\footnotesize \\textit{Notes:} Each coefficient comes from a university-level change regression relative to 2013. ",
      "The dependent variable is the outcome change and the coefficient shown is on the 2016-treatment indicator. ",
      "All specifications control for changes in log undergraduate enrollment, the eligibility proxy, accreditation years, ",
      "and eligibility-proxy coverage. Standard errors and 95\\% confidence intervals use a university bootstrap. ",
      "The restricted sample excludes UNIACC, Universidad del Pacífico, UCINF, ARCIS and Universidad de Los Leones. ",
      "$^{***}p<0.01$, $^{**}p<0.05$, $^{*}p<0.10$."
    ),
    "\\end{minipage}", "\\end{table}"
  ), con)
}

validate_main_workbook <- function(path, transformed) {
  sheets <- openxlsx::getSheetNames(path)
  if (!identical(sheets, c("paper_table", "raw_results"))) stop("Invalid sheets: ", path)
  paper_all <- openxlsx::read.xlsx(path, sheet = "paper_table", colNames = FALSE, skipEmptyRows = FALSE)
  headers <- as.character(unlist(paper_all[1, , drop = TRUE], use.names = FALSE))
  paper <- paper_all[-1, , drop = FALSE]
  names(paper) <- headers
  raw <- openxlsx::read.xlsx(path, sheet = "raw_results", check.names = FALSE, skipEmptyRows = FALSE)
  expected_headers <- c("Outcome / statistic")
  for (sample_key in names(sample_panels)) {
    expected_headers <- c(expected_headers, paste0(sample_labels[[sample_key]], ": ", SPECIFICATIONS$specification_label))
  }
  raw_columns <- c(
    "outcome", "scale", "sample", "sample_label", "specification",
    "specification_label", "status", "coef", "se", "pvalue", "ci_low",
    "ci_high", "nobs", "n_clusters", "adj_r2"
  )
  if (!identical(headers, expected_headers) || ncol(paper) != 7L) stop("Invalid paper columns: ", path)
  if (!identical(names(raw), raw_columns) || ncol(raw) != 15L) stop("Invalid raw columns: ", path)
  if (nrow(raw) != nrow(OUTCOME_SPECS) * 2L * 3L) stop("Invalid raw row count: ", path)
  if (nrow(paper) != nrow(OUTCOME_SPECS) * 4L + 4L) stop("Invalid paper row count: ", path)
  if (!setequal(unique(raw$sample), names(sample_panels))) stop("Invalid samples: ", path)
  if (!setequal(unique(raw$specification), SPECIFICATIONS$specification)) stop("Invalid specifications: ", path)
  if (!transformed && any(raw$scale != "level")) stop("Levels workbook contains transformed scales")
  for (i in seq_along(OUTCOME_SPECS$outcome)) {
    idx <- (i - 1L) * 4L + 1L
    expected <- c(OUTCOME_SPECS$outcome[[i]], "(SE)", "Observations", "Adjusted R2")
    if (!identical(as.character(paper[[1]][idx:(idx + 3L)]), expected)) {
      stop("Invalid outcome block for ", OUTCOME_SPECS$outcome[[i]])
    }
  }
  invisible(TRUE)
}

make_psd <- function(sigma) {
  sigma <- (sigma + t(sigma)) / 2
  ee <- eigen(sigma, symmetric = TRUE)
  floor_value <- max(1e-12, max(abs(ee$values), na.rm = TRUE) * 1e-10)
  ee$values <- pmax(ee$values, floor_value)
  out <- ee$vectors %*% diag(ee$values, nrow = length(ee$values)) %*% t(ee$vectors)
  (out + t(out)) / 2
}

patch_honestdid_norminvp <- function() {
  ns <- asNamespace("HonestDiD")
  binding <- ".norminvp_generalized"
  replacement <- function(p, l, u, mu = 0, sd = 1) {
    if (!is.finite(sd) || sd <= 0) stop("sd must be positive and finite")
    lower_prob <- stats::pnorm((l - mu) / sd)
    upper_prob <- stats::pnorm((u - mu) / sd)
    target <- lower_prob + p * (upper_prob - lower_prob)
    target <- pmin(pmax(target, .Machine$double.eps), 1 - .Machine$double.eps)
    mu + stats::qnorm(target) * sd
  }
  if (bindingIsLocked(binding, ns)) unlockBinding(binding, ns)
  assign(binding, replacement, envir = ns)
  lockBinding(binding, ns)
  invisible(TRUE)
}

run_package_diagnostics <- function() {
  patch_honestdid_norminvp()
  event_dir <- file.path(root, "Difference-in-Difference Analysis", "event")
  vcov_files <- list.files(event_dir, pattern = "^event_vcov_dynamic_.*[.]csv$", full.names = TRUE)
  power_rows <- list(); detail_rows <- list(); relative_rows <- list(); smooth_rows <- list(); error_rows <- list()
  priority_stem <- "Real_Operating_Revenue_per_Student_-_Annual_Log_Change"
  reference_event_time <- -3L

  for (vcov_file in vcov_files) {
    stem <- sub("^event_vcov_dynamic_", "", basename(vcov_file))
    stem <- sub("[.]csv$", "", stem)
    event_file <- file.path(event_dir, paste0("event_dynamic_", stem, ".csv"))
    if (!file.exists(event_file)) {
      error_rows[[length(error_rows) + 1L]] <- data.frame(
        Outcome = stem, Stage = "input", Error = "Missing event coefficient file"
      )
      next
    }
    event <- read_csv_utf8(event_file)
    vcov_raw <- read_csv_utf8(vcov_file)
    if (!all(c("event_time", "att") %in% names(event)) || !"event_time" %in% names(vcov_raw)) next
    event$event_time <- suppressWarnings(as.integer(event$event_time))
    event$att <- suppressWarnings(as.numeric(event$att))
    use <- event[is.finite(event$event_time) & is.finite(event$att), c("event_time", "att"), drop = FALSE]
    use <- use[order(use$event_time), , drop = FALSE]
    t_vec <- use$event_time
    keep_names <- as.character(t_vec)
    row_index <- match(t_vec, suppressWarnings(as.integer(vcov_raw$event_time)))
    if (!all(keep_names %in% names(vcov_raw)) || anyNA(row_index)) next
    sigma <- as.matrix(vcov_raw[row_index, keep_names, drop = FALSE])
    storage.mode(sigma) <- "double"
    if (any(!is.finite(sigma))) next
    sigma <- make_psd(sigma)
    beta <- use$att
    slope80 <- tryCatch(
      pretrends::slope_for_power(
        sigma = sigma, targetPower = 0.80, tVec = t_vec,
        referencePeriod = reference_event_time
      ),
      error = function(e) e
    )
    if (inherits(slope80, "error")) {
      error_rows[[length(error_rows) + 1L]] <- data.frame(
        Outcome = stem, Stage = "pretrends", Error = conditionMessage(slope80)
      )
      next
    }
    pre_result <- tryCatch(
      pretrends::pretrends(
        betahat = beta, sigma = sigma, tVec = t_vec,
        referencePeriod = reference_event_time,
        deltatrue = as.numeric(slope80) * (t_vec - reference_event_time)
      ),
      error = function(e) e
    )
    power_rows[[length(power_rows) + 1L]] <- data.frame(
      Outcome = stem, Reference_Year = 2013L, Reference_Event_Time = reference_event_time,
      Pre_Coefficients_Before_Reference = sum(t_vec < reference_event_time),
      Anticipation_Coefficients_2014_2015 = sum(t_vec %in% c(-2L, -1L)),
      Slope_80pct_Power = as.numeric(slope80), stringsAsFactors = FALSE
    )
    if (!inherits(pre_result, "error")) {
      detail <- as.data.frame(pre_result$df_power)
      detail$Outcome <- stem
      detail$Hypothesized_Slope <- as.numeric(slope80)
      detail_rows[[length(detail_rows) + 1L]] <- detail
      try(ggplot2::ggsave(
        file.path(plots_dir, paste0("roth_pretrends_", stem, ".png")),
        pre_result$event_plot, width = 10.5, height = 6.2, dpi = 300
      ), silent = TRUE)
    }
    if (!identical(stem, priority_stem)) next
    n_pre <- sum(t_vec < reference_event_time)
    post_times <- t_vec[t_vec > reference_event_time]
    n_post <- length(post_times)
    if (n_pre < 1L || n_post < 1L) next
    estimands <- list(
      Impact_2016 = as.numeric(post_times == 0L),
      Average_2016_2021 = as.numeric(post_times >= 0L)
    )
    estimands$Average_2016_2021 <- estimands$Average_2016_2021 / sum(estimands$Average_2016_2021)
    scale_ref <- max(
      sqrt(diag(sigma)), abs(beta), abs(diff(c(0, beta[t_vec < reference_event_time]))),
      na.rm = TRUE
    )
    if (!is.finite(scale_ref) || scale_ref <= 0) scale_ref <- 1
    for (estimand_name in names(estimands)) {
      l_vec <- estimands[[estimand_name]]
      relative <- tryCatch(
        HonestDiD::createSensitivityResults_relativeMagnitudes(
          betahat = beta, sigma = sigma, numPrePeriods = n_pre,
          numPostPeriods = n_post, Mbarvec = seq(0, 2, by = 0.25),
          l_vec = l_vec, alpha = 0.05
        ),
        error = function(e) e
      )
      if (inherits(relative, "error")) {
        error_rows[[length(error_rows) + 1L]] <- data.frame(
          Outcome = stem, Stage = paste0("HonestDiD relative magnitude ", estimand_name),
          Error = conditionMessage(relative)
        )
      } else {
        relative <- as.data.frame(relative)
        relative$Outcome <- stem
        relative$Estimand <- estimand_name
        relative_rows[[length(relative_rows) + 1L]] <- relative
      }
      for (m_value in seq(0, 2 * scale_ref, length.out = 9)) {
        smooth <- tryCatch(
          HonestDiD::createSensitivityResults(
            betahat = beta, sigma = sigma, numPrePeriods = n_pre,
            numPostPeriods = n_post, Mvec = m_value, l_vec = l_vec, alpha = 0.05
          ),
          error = function(e) e
        )
        if (inherits(smooth, "error")) {
          error_rows[[length(error_rows) + 1L]] <- data.frame(
            Outcome = stem,
            Stage = paste0("HonestDiD smoothness ", estimand_name, " (M=", format(m_value, digits = 6), ")"),
            Error = conditionMessage(smooth)
          )
        } else {
          smooth <- as.data.frame(smooth)
          smooth$Outcome <- stem
          smooth$Estimand <- estimand_name
          smooth$Scale_Reference <- scale_ref
          smooth_rows[[length(smooth_rows) + 1L]] <- smooth
        }
      }
    }
  }
  out <- list(
    power = bind_fill(power_rows), details = bind_fill(detail_rows),
    relative = bind_fill(relative_rows), smoothness = bind_fill(smooth_rows),
    errors = bind_fill(error_rows)
  )
  write_csv_utf8(out$power, file.path(results_dir, "Table_PreTrends_Power.csv"))
  write_csv_utf8(out$details, file.path(results_dir, "Table_PreTrends_Details.csv"))
  write_csv_utf8(out$relative, file.path(results_dir, "Table_HonestDiD_Relative_Magnitude.csv"))
  write_csv_utf8(out$smoothness, file.path(results_dir, "Table_HonestDiD_Smoothness.csv"))
  write_csv_utf8(out$errors, file.path(results_dir, "PreTrends_estimation_errors.csv"))
  out
}

main_results <- estimate_main_tables()
levels_xlsx <- file.path(results_dir, "Table_PreTrends_Main_Levels.xlsx")
transformed_xlsx <- file.path(results_dir, "Table_PreTrends_Main_Transformed.xlsx")
paper_levels <- write_main_workbook(main_results$levels, levels_xlsx)
paper_transformed <- write_main_workbook(main_results$transformed, transformed_xlsx)
write_csv_utf8(main_results$levels, file.path(results_dir, "Table_PreTrends_Main_Levels.csv"))
write_csv_utf8(main_results$transformed, file.path(results_dir, "Table_PreTrends_Main_Transformed.csv"))
write_latex_table(paper_levels, file.path(results_dir, "Table_PreTrends_Main_Levels.tex"), FALSE)
write_latex_table(paper_transformed, file.path(results_dir, "Table_PreTrends_Main_Transformed.tex"), TRUE)
validate_main_workbook(levels_xlsx, FALSE)
validate_main_workbook(transformed_xlsx, TRUE)
diagnostics <- run_package_diagnostics()

# Delete only the superseded non-normalized tables, after the replacements pass.
obsolete <- file.path(results_dir, c(
  "Table_PreTrends_Main.xlsx", "Table_PreTrends_Main.csv", "Table_PreTrends_Main.tex",
  "Table_HonestDiD_Main.xlsx", "Table_HonestDiD_Main.csv", "Table_HonestDiD_Main.tex"
))
unlink(obsolete[file.exists(obsolete)], recursive = FALSE, force = TRUE)

summary_lines <- c(
  "Pre-trends, anticipation, pretrends power, and HonestDiD sensitivity analysis",
  paste("Run date:", Sys.time()),
  "Main tables use exactly the DiD workbook architecture: paper_table plus raw_results.",
  "Main table columns: 2012 pre-trend, 2014 anticipation, and 2015 anticipation for both samples.",
  "Reference year: 2013.",
  "Each main estimate is a university-level change regression with the same four controls used by the DiD event study.",
  paste0("Inference: university bootstrap with ", args$boot_reps, " requested replications; seed ", args$seed, "."),
  "The restricted sample excludes UNIACC, Universidad del Pacífico, UCINF, ARCIS and Universidad de Los Leones.",
  "Both samples exclude Universidad de Aysén and Universidad de O'Higgins through the shared SDID sample loader.",
  paste("Main rows, levels:", nrow(main_results$levels)),
  paste("Main rows, transformed:", nrow(main_results$transformed)),
  paste("pretrends power outcomes:", nrow(diagnostics$power)),
  paste("HonestDiD relative-magnitude rows:", nrow(diagnostics$relative)),
  paste("HonestDiD smoothness rows:", nrow(diagnostics$smoothness)),
  paste("Recorded package-diagnostic errors:", nrow(diagnostics$errors))
)
writeLines(summary_lines, file.path(results_dir, "PreTrends_execution_summary.txt"), useBytes = TRUE)

message("[OK] Pre-Trends pipeline completed and both main workbooks passed structural validation.")
