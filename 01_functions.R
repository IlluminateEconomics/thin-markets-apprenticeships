# ==============================================================================
# 01_functions.R
# Shared helper functions
# ==============================================================================
#
# READS:    nothing (function definitions only)
# PRODUCES: functions used by every downstream script
#
# Source after 00_config.R:
#   source("R Scripts/00_config.R"); source("R Scripts/01_functions.R")
#
# The central function is read_ees_csv(), which reads an EES file while
# preserving suppression information rather than discarding it.
# See METHODOLOGY-NOTE.md, "Suppression".
# ==============================================================================

library(tidyverse)


# --- SECTION 1: SUPPRESSION-AWARE READING -------------------------------------

# Columns whose names indicate they should hold a number, as a single
# alternation regex.
measure_name_pattern <- paste(
  c("starts", "achievements", "participation", "enrolments", "vacancies",
    "adverts", "leavers", "completers", "achievers", "commitments",
    "redundancies", "population", "rate", "percent", "values"),
  collapse = "|"
)

#' Identify measure columns by name
#'
#' Matches on name alone, regardless of the type the column was parsed as.
#' A file with no suppressed cells will have its measures read as numeric,
#' but it still needs a suppression flag so that downstream code can rely on
#' the flag existing whatever the source.
#'
#' @param df a data frame
#' @return character vector of column names that look like measures
detect_measure_cols <- function(df) {
  nms <- names(df)
  # The second condition excludes the flag columns this function's output is
  # used to create, so calling it twice on the same frame is harmless.
  nms[str_detect(nms, measure_name_pattern) & !str_detect(nms, "_suppressed$|_low$")]
}

#' Convert measure columns to numeric while preserving suppression information
#'
#' EES writes the string "low" where a count is too small to publish, in
#' practice a value between 1 and 9 after rounding to the nearest 10. Coercing
#' this directly to numeric produces NA, which is indistinguishable from a
#' genuinely missing value and invisible to any later filter.
#'
#' This function instead:
#'   - records, for each measure column, a logical `<col>_suppressed` flag
#'   - sets the numeric value to NA so arithmetic behaves predictably
#'   - reports how many cells were suppressed in each column
#'
#' Downstream code should filter on the flag rather than on is.na(), so that
#' suppressed observations are counted as present but of unknown size.
#'
#' The flag is always created for every measure column, including those that
#' contained no suppressed cells and were therefore parsed as numeric. This
#' guarantees the flag exists whatever the source, so code that reads several
#' files does not have to test for its presence.
#'
#' @param df   data frame straight from read_csv()
#' @param cols measure columns; defaults to auto-detection
#' @param quiet suppress the console report
#' @return df with numeric measures plus `<col>_suppressed` logicals
clean_measures <- function(df, cols = NULL, quiet = FALSE) {

  if (is.null(cols)) cols <- detect_measure_cols(df)
  if (length(cols) == 0) return(df)

  report <- tibble(column = character(), suppressed = integer(), total = integer())

  for (cl in cols) {

    if (is.numeric(df[[cl]])) {
      # Parsed as numeric, so the source held no tokens. Flag it as
      # unsuppressed and leave the values untouched.
      df[[paste0(cl, "_suppressed")]] <- FALSE
      df[[paste0(cl, "_low")]] <- FALSE
      report <- add_row(report, column = cl, suppressed = 0L, total = nrow(df))
      next
    }

    raw <- as.character(df[[cl]])
    tok <- str_trim(str_to_lower(raw))
    is_supp <- !is.na(raw) & tok %in% suppression_tokens

    # Two flags, because the tokens do not mean the same thing:
    #   _low        the raw volume is 0-4 and has been suppressed. This does
    #               not by itself prove positive activity.
    #   _suppressed any non-numeric token, including "not applicable" and
    #               "unavailable"
    #
    # `low` is analytically distinct from "not applicable" or "unavailable",
    # but it must not be used as an activity flag because it includes zero.
    df[[paste0(cl, "_suppressed")]] <- is_supp
    df[[paste0(cl, "_low")]] <- !is.na(raw) & tok == low_token
    df[[cl]] <- suppressWarnings(as.numeric(gsub(",", "", raw)))

    report <- add_row(
      report,
      column = cl,
      suppressed = sum(is_supp),
      total = length(is_supp)
    )
  }

  if (!quiet) {
    flagged <- filter(report, suppressed > 0)
    if (nrow(flagged) > 0) {
      message("  Suppressed cells retained as flags:")
      for (i in seq_len(nrow(flagged))) {
        message(sprintf(
          "    %-16s %6d of %6d (%.1f%%)",
          flagged$column[i], flagged$suppressed[i], flagged$total[i],
          100 * flagged$suppressed[i] / flagged$total[i]
        ))
      }
    }
  }

  df
}

#' Read an EES CSV with suppression handling applied
#'
#' Measure columns are forced to character on read. Left to guess, readr
#' infers column types from the first 1,000 rows only, so a file whose early
#' rows happen to be fully populated would have its measures typed as numeric
#' and any suppression token appearing later coerced to NA without comment.
#' Reading the header first, then forcing the measure columns, removes that
#' dependency on row order.
#'
#' @param path  file path
#' @param quiet suppress the console report
read_ees_csv <- function(path, quiet = FALSE) {

  if (!file.exists(path)) {
    stop("File not found: ", path, call. = FALSE)
  }

  if (!quiet) message("Reading ", basename(path))

  header <- read_csv(path, n_max = 0, show_col_types = FALSE, progress = FALSE)
  measure_cols <- detect_measure_cols(header)

  spec <- if (length(measure_cols) > 0) {
    do.call(cols, setNames(rep(list(col_character()), length(measure_cols)), measure_cols))
  } else {
    cols()
  }

  df <- read_csv(path, col_types = spec, progress = FALSE)

  bad <- problems(df)
  if (nrow(bad) > 0 && !quiet) {
    warning(
      basename(path), ": ", nrow(bad), " parsing problem(s) reported by readr. ",
      "Inspect with problems().",
      call. = FALSE
    )
  }

  clean_measures(df, quiet = quiet)
}

#' Read a provider dashboard CSV
#'
#' Dashboard exports are unrounded and carry no suppression, so no token
#' handling is required. clean_measures() is still applied so that the
#' resulting object has the same shape as an EES-derived frame; the
#' `_suppressed` flags will all be FALSE.
read_shiny_csv <- function(path, quiet = FALSE) {

  if (!file.exists(path)) {
    stop("File not found: ", path, call. = FALSE)
  }

  if (!quiet) message("Reading ", basename(path), " (unsuppressed source)")

  read_ees_csv(path, quiet = quiet)
}


# --- SECTION 2: EES STRUCTURAL FILTERS ----------------------------------------

# EES table-tool files stack several things in one table:
#   - a `Total` row inside every dimension, alongside its own components
#   - multiple `breakdown_topic` blocks (Total, Age, Sex, ...) that each
#     independently re-partition the same starts
# Summing without filtering returns a multiple of the true figure.

#' Keep only the all-learners block of an EES file
#'
#' Filters breakdown_topic and breakdown to "Total" where those columns exist.
#' Safe to call on files that lack them.
keep_total_breakdown <- function(df) {
  if ("breakdown_topic" %in% names(df)) df <- filter(df, .data$breakdown_topic == "Total")
  if ("breakdown" %in% names(df))       df <- filter(df, .data$breakdown == "Total")
  df
}

#' Drop subtotal rows from named dimension columns
#'
#' @param df   data frame
#' @param cols dimension columns whose "Total" rows should be removed
drop_total_rows <- function(df, cols) {
  for (cl in intersect(cols, names(df))) {
    df <- filter(df, .data[[cl]] != "Total")
  }
  df
}

#' Assert that a data frame's starts sum to an expected national figure
#'
#' Cheap guard against the double-counting traps above. Call it after building
#' any national aggregate.
#'
#' @param df       data frame
#' @param expected expected total
#' @param col      measure column
#' @param tol      allowed absolute difference (rounding to nearest 10 means
#'                 component sums legitimately miss the published total)
check_total <- function(df, expected, col = "starts", tol = 100) {
  actual <- sum(df[[col]], na.rm = TRUE)
  diff <- abs(actual - expected)
  if (diff > tol) {
    warning(
      "Total check FAILED for '", col, "': got ", format(actual, big.mark = ","),
      ", expected ~", format(expected, big.mark = ","),
      " (difference ", format(diff, big.mark = ","), ").",
      call. = FALSE
    )
  } else {
    message("  Total check OK for '", col, "': ", format(actual, big.mark = ","))
  }
  invisible(actual)
}


# --- SECTION 3: JOIN INTEGRITY ------------------------------------------------

#' Assert a key is unique before it is used on the right side of a join
#'
#' left_join() silently duplicates rows when the right-hand key repeats, which
#' inflates any count or total computed afterwards without raising an error.
#' Checking the key first turns a silent data error into a visible warning.
assert_unique_key <- function(df, key, label = deparse(substitute(df))) {
  dupes <- df %>%
    filter(!is.na(.data[[key]])) %>%
    count(.data[[key]], name = "n_rows") %>%
    filter(.data$n_rows > 1)

  if (nrow(dupes) > 0) {
    warning(
      label, ": ", nrow(dupes), " duplicate value(s) of '", key,
      "'. A join on this key will fan out rows.",
      call. = FALSE
    )
    return(invisible(dupes))
  }
  message("  Key '", key, "' unique in ", label, ". OK.")
  invisible(NULL)
}

#' Report match quality after a left join
report_match <- function(df, matched_col, label = "join") {
  n <- nrow(df)
  matched <- sum(!is.na(df[[matched_col]]))
  message(sprintf(
    "  %s: %d of %d matched (%.1f%%), %d unmatched",
    label, matched, n, 100 * matched / n, n - matched
  ))
  invisible(tibble(rows = n, matched = matched, unmatched = n - matched))
}


# --- SECTION 4: LABELLING AND RECODING ----------------------------------------

#' Convert an EES time_period integer to display form: 202425 -> "2024/25"
make_academic_year <- function(time_period) {
  tp <- as.character(time_period)
  paste0(substr(tp, 1, 4), "/", substr(tp, 5, 6))
}

#' Harmonise apprenticeship level labels across sources
#'
#' EES uses "Intermediate"; the Shiny export uses "Intermediate Apprenticeship".
normalise_apps_level <- function(x) {
  case_when(
    x %in% c("Intermediate", "Intermediate Apprenticeship") ~ "Intermediate",
    x %in% c("Advanced", "Advanced Apprenticeship")         ~ "Advanced",
    x %in% c("Higher", "Higher Apprenticeship")             ~ "Higher",
    x == "Total"                                            ~ "Total",
    TRUE                                                     ~ x
  )
}

#' Extract UKPRN from a provider name of the form "NAME (10012345)"
#'
#' Dashboard exports embed the UK Provider Reference Number in the provider
#' name rather than supplying it as a column, and capitalise the name
#' inconsistently between exports. The UKPRN is therefore the only reliable
#' key for joining provider records across files.
extract_ukprn <- function(provider_name) {
  str_extract(provider_name, "(?<=\\()[0-9]+(?=\\))")
}

#' Extract an ST/FA reference from "Standard Name (ST0123)"
extract_standard_ref <- function(x) {
  str_extract(x, "ST[0-9]+|FA[0-9]+")
}

#' Strip a trailing "(ST0123)" from a standard name
strip_standard_ref <- function(x) {
  x %>%
    str_remove("\\s*\\((ST|FA)[0-9]+\\)$") %>%
    str_squish()
}

#' Band a starts count using the project's standard breaks
band_starts <- function(x) {
  cut(x, breaks = starts_band_breaks, labels = starts_band_labels, right = FALSE)
}

band_estimated_starts <- function(x) {
  cut(
    x,
    breaks = estimated_starts_band_breaks,
    labels = estimated_starts_band_labels,
    right = FALSE
  )
}


# --- SECTION 5: CONCENTRATION METRICS -----------------------------------------

#' Concentration of a measure across geographies
#'
#' Higher top_n_share and HHI mean more concentrated.
#'
#' @param data        one row per geography
#' @param geography_col unquoted column naming the geography
#' @param value_col   unquoted measure column
geo_concentration <- function(data, geography_col, value_col = starts) {

  data %>%
    filter(
      !is.na({{ geography_col }}),
      !is.na({{ value_col }}),
      {{ value_col }} > 0
    ) %>%
    mutate(
      total_value = sum({{ value_col }}, na.rm = TRUE),
      value_share = {{ value_col }} / .data$total_value
    ) %>%
    arrange(desc({{ value_col }})) %>%
    mutate(rank = row_number()) %>%
    summarise(
      geographies  = n_distinct({{ geography_col }}),
      total_value  = sum({{ value_col }}, na.rm = TRUE),
      median_value = median({{ value_col }}, na.rm = TRUE),
      top_1_share  = round(100 * sum(.data$value_share[.data$rank <= 1],  na.rm = TRUE), 1),
      top_5_share  = round(100 * sum(.data$value_share[.data$rank <= 5],  na.rm = TRUE), 1),
      top_10_share = round(100 * sum(.data$value_share[.data$rank <= 10], na.rm = TRUE), 1),
      hhi          = round(sum(.data$value_share^2, na.rm = TRUE), 4),
      .groups = "drop"
    )
}


# --- SECTION 6: THIN-MARKET TIERS ---------------------------------------------

#' Assign the six-tier thin-market classification
#'
#' Thresholds come from `tier_thresholds` in 00_config.R.
#'
#' The classification applies only to standards with recorded positive starts
#' and at least one observed provider. Approved standards with no recorded
#' starts form a separate stock population and must not be passed to this
#' function.
#'
#' @param df data frame with `starts` and `providers` columns
assign_thin_market_tier <- function(df) {

  th <- tier_thresholds

  required_columns <- c("starts", "providers")
  missing_columns <- setdiff(required_columns, names(df))

  if (length(missing_columns) > 0) {
    stop(
      "assign_thin_market_tier() requires: ",
      paste(required_columns, collapse = ", "),
      ". Missing: ", paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }

  invalid_market_rows <- df %>%
    filter(
      is.na(.data$starts) | .data$starts <= 0 |
        is.na(.data$providers) | .data$providers <= 0
    )

  if (nrow(invalid_market_rows) > 0) {
    stop(
      "assign_thin_market_tier() received ", nrow(invalid_market_rows),
      " row(s) without positive starts and provider counts. ",
      "Keep approved standards with no recorded starts in the separate stock population.",
      call. = FALSE
    )
  }

  df %>%
    mutate(
      tier = case_when(
        .data$starts <  th$severe$starts    & .data$providers <= th$severe$providers    ~ "Severe thin market",
        .data$starts <  th$thin$starts      & .data$providers <= th$thin$providers      ~ "Thin market",
        .data$starts <  th$potential$starts & .data$providers <= th$potential$providers ~ "Potential thin market",
        .data$starts >= th$provider_dependency$starts &
          .data$providers <= th$provider_dependency$providers                           ~ "Provider dependency market",
        .data$starts <  th$low_volume$starts                                            ~ "Low-volume review group",
        TRUE                                                                            ~ "Scaled market"
      ),
      tier = factor(.data$tier, levels = tier_levels)
    )
}


# --- SECTION 7: CHART THEME ---------------------------------------------------
# House style: no gridlines, a black panel border, and a very light grey panel
# on a white plot background.

theme_house <- function(base_size = 12) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      panel.grid       = ggplot2::element_blank(),
      panel.border     = ggplot2::element_rect(colour = "black", fill = NA, linewidth = 0.6),
      panel.background = ggplot2::element_rect(fill = colour_panel, colour = NA),
      plot.background  = ggplot2::element_rect(fill = "white", colour = NA),
      plot.title       = ggplot2::element_text(face = "bold", size = 14, colour = colour_text),
      plot.subtitle    = ggplot2::element_text(size = 11, colour = colour_text),
      plot.caption     = ggplot2::element_text(size = 8, colour = colour_grey, hjust = 0),
      axis.title       = ggplot2::element_text(colour = colour_text),
      axis.text        = ggplot2::element_text(colour = colour_text),
      legend.position  = "bottom"
    )
}

# Line charts carry legends on the right, so series labels sit in the same
# order as the lines themselves.
theme_house_line <- function(base_size = 12) {
  theme_house(base_size) + ggplot2::theme(legend.position = "right")
}


# --- SECTION 8: OUTPUT HELPERS ------------------------------------------------

#' Save a chart with project-standard dimensions
save_chart <- function(plot, filename, width = chart_width, height = chart_height) {
  path <- file.path(chart_folder, filename)
  ggsave(path, plot, width = width, height = height, dpi = chart_dpi, bg = "white")
  message("  Saved ", filename)
  invisible(path)
}

#' Save a table as CSV
save_table <- function(df, filename) {
  path <- file.path(table_folder, filename)
  write_csv(df, path)
  message("  Saved ", filename, " (", nrow(df), " rows)")
  invisible(path)
}


# --- SECTION 8: VERSION MARKER ------------------------------------------------
# Printed on load so it is obvious which copy of this file is in the session.
# Scripts are edited in one location and run from another, so a stale copy is
# an easy mistake to make and an awkward one to diagnose: functions silently
# behave differently rather than failing outright.

functions_version <- "2026-08-10b"

message("01_functions.R loaded (version ", functions_version, ").")
