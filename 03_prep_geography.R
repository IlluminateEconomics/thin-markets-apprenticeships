# ==============================================================================
# 03_prep_geography.R
# Subject-level geographic aggregates from the underlying starts file
# ==============================================================================
#
# READS:
#   Raw Data/apprenticeships_2025-26/supporting-files/
#     apps_u01_underlying_starts_202526_10.zip
#
# PRODUCES (aggregated only; the raw table is discarded):
#   geo_region_ssa      region x sector subject area x year
#   geo_lad_ssa         local authority district x sector subject area x year
#   geo_region_level    region x apprenticeship level x year
#   geo_lad_standard    local authority district x standard x year
#   geo_coverage_check  reconciliation against the published national total
#   historical_standard_starts_exact
#                       exact standard x year starts, 2017/18 to 2024/25
#
# DEPENDS ON: 00_config.R, 01_functions.R, 02_load_data.R
#
# ------------------------------------------------------------------------------
# WHY THIS FILE RATHER THAN THE STANDARD GEOGRAPHY TABLE
#
# The published geography table that carries sector subject area alongside
# region and local authority district (apps_19_geography_detailed) is a
# single-year file: each release covers only the year it reports on. It cannot
# therefore support subject-level geographic analysis for any earlier year.
#
# The underlying starts file solves this. It carries the full series with
# region, local authority district, parliamentary constituency and sector
# subject area in one table, on the same sector subject area mapping as the
# rest of the release. It is also far more granular, so the aggregates below
# are built to the exact cuts the analysis needs rather than to whatever
# breakdown a summary table happens to offer.
#
# See METHODOLOGY-NOTE.md, "Geographic coverage".
# ------------------------------------------------------------------------------
#
# MEMORY
#
# The compressed file is around 135 MB and considerably larger once parsed.
# It is read once, every required aggregate is produced in that single pass,
# and the raw table is then removed. Nothing downstream needs row-level
# detail, and keeping the raw table in the session would invite accidental
# use of an unfiltered total.
# ==============================================================================

library(tidyverse)

source("00_config.R")
source("01_functions.R")


# --- SECTION 0: CACHE ---------------------------------------------------------
# The aggregation below is deterministic and slow, so its results are cached.
# A cache is reusable only when its source file, geography settings and this
# aggregation script all match the provenance stored in the cache. This avoids
# silently reusing an old aggregate after the code or delivery/home basis has
# changed.

rebuild_geography <- FALSE

geo_cache <- file.path(derived_folder, "geography_aggregates.rds")

source_info <- if (file.exists(underlying_starts_zip)) {
  file.info(underlying_starts_zip)
} else {
  NULL
}

historical_existing <- underlying_starts_historical_candidates[
  file.exists(underlying_starts_historical_candidates)
]

if (length(historical_existing) == 0) {
  historical_existing <- list.files(
    raw_data_folder,
    pattern = "(?i)(underlying.*start.*(201920|1920)|(201920|1920).*underlying.*start).*\\.zip$",
    recursive = TRUE,
    full.names = TRUE
  )
}

if (length(historical_existing) == 0) {
  stop(
    "The 2019/20 underlying starts archive was not found under Raw Data.\n",
    "  Expected a filename such as underlying_starts_201920.zip.\n",
    "  This exact historical source is required for Charts 2, 6 and 8.",
    call. = FALSE
  )
}

underlying_starts_historical_zip <- historical_existing[[1]]
historical_source_info <- file.info(underlying_starts_historical_zip)

if (length(historical_existing) > 1) {
  warning(
    "More than one 2019/20 underlying starts archive was found; using:\n  ",
    underlying_starts_historical_zip,
    call. = FALSE
  )
}

cache_signature <- list(
  cache_version = 4L,
  source_path = normalizePath(underlying_starts_zip, mustWork = FALSE),
  source_size = if (is.null(source_info)) NA_real_ else unname(source_info$size),
  source_mtime_utc = if (is.null(source_info)) {
    NA_character_
  } else {
    format(source_info$mtime, "%Y-%m-%d %H:%M:%OS6", tz = "UTC")
  },
  historical_source_path = normalizePath(
    underlying_starts_historical_zip,
    mustWork = FALSE
  ),
  historical_source_size = unname(historical_source_info$size),
  historical_source_mtime_utc = format(
    historical_source_info$mtime,
    "%Y-%m-%d %H:%M:%OS6",
    tz = "UTC"
  ),
  geo_basis = as.character(geo_basis),
  ref_year = as.character(ref_year),
  aggregation_script_md5 = unname(tools::md5sum("03_prep_geography.R"))
)

cached <- NULL

if (file.exists(geo_cache) && file.exists(underlying_starts_zip) &&
    file.exists(underlying_starts_historical_zip) &&
    !rebuild_geography) {
  cached <- tryCatch(
    readRDS(geo_cache),
    error = function(e) {
      warning("Could not read geography cache; rebuilding. ", conditionMessage(e),
              call. = FALSE)
      NULL
    }
  )
}

cache_is_current <- !is.null(cached) &&
  identical(cached$cache_metadata, cache_signature)

if (cache_is_current && !rebuild_geography) {

  message("Loading cached geography aggregates (", basename(geo_cache), ").")
  message("  Built ", format(file.mtime(geo_cache), "%d %b %Y %H:%M"),
          ". Set rebuild_geography <- TRUE to force a rebuild.")

  list2env(cached, envir = environment())

  geo_region_ssa     <- cached$geo_region_ssa
  geo_lad_ssa        <- cached$geo_lad_ssa
  geo_region_level   <- cached$geo_region_level
  geo_lad_standard   <- cached$geo_lad_standard
  geo_coverage_check <- cached$geo_coverage_check
  historical_standard_starts_exact <- cached$historical_standard_starts_exact

  message("03_prep_geography.R complete (from cache).")

} else {

if (rebuild_geography) message("Rebuild requested; ignoring any cache.")


# --- SECTION 1: UNPACK --------------------------------------------------------
# Extracted to a temporary directory so no large intermediate file is left in
# the project. Removed again when the script finishes.

if (!file.exists(underlying_starts_zip)) {
  stop(
    "Underlying starts file not found:\n  ", underlying_starts_zip, "\n",
    "  Download it from the 'Supporting files' section of the EES release.",
    call. = FALSE
  )
}

unzip_dir <- file.path(tempdir(), "underlying_starts")
dir.create(unzip_dir, recursive = TRUE, showWarnings = FALSE)

message("Unpacking ", basename(underlying_starts_zip), " ...")
unzip(underlying_starts_zip, exdir = unzip_dir, overwrite = TRUE)

unzipped_csv <- list.files(unzip_dir, pattern = "\\.csv$", full.names = TRUE)

if (length(unzipped_csv) != 1) {
  stop(
    "Expected exactly one CSV in the archive, found ", length(unzipped_csv), ".",
    call. = FALSE
  )
}

message("  Extracted ", basename(unzipped_csv),
        " (", round(file.size(unzipped_csv) / 1024^2), " MB)")


# --- SECTION 2: READ ----------------------------------------------------------
# Suppression is handled exactly as for every other source: tokens are flagged
# rather than dropped, so a standard present in an area but too small to
# publish is still counted as present.

message("\nReading underlying starts (this may take a minute) ...")

underlying_starts <- read_ees_csv(unzipped_csv)

message("  ", format(nrow(underlying_starts), big.mark = ","), " rows, ",
        ncol(underlying_starts), " columns")


# --- SECTION 3: RESOLVE COLUMN NAMES ------------------------------------------
# The file supplies both a delivery-postcode and a learner-home-postcode set of
# geography columns. `geo_basis` in 00_config.R selects between them.

geo_cols <- list(
  region = paste0(geo_basis, "_region"),
  lad    = paste0(geo_basis, "_lad"),
  la     = paste0(geo_basis, "_la"),
  pcon   = paste0(geo_basis, "_pcon")
)

required_cols <- c(
  "academic_year", "starts", "ssa_tier_1", "apps_level",
  "std_fwk_name", "std_fwk_flag",
  geo_cols$region, geo_cols$lad
)

missing_cols <- setdiff(required_cols, names(underlying_starts))

if (length(missing_cols) > 0) {
  stop(
    "Expected columns not found in the underlying starts file:\n  ",
    paste(missing_cols, collapse = "\n  "), "\n",
    "  Available columns:\n  ",
    paste(names(underlying_starts), collapse = ", "),
    call. = FALSE
  )
}

message("\nGeographic basis: ", geo_basis,
        " (", geo_cols$region, ", ", geo_cols$lad, ")")

# Standardise to neutral names so downstream code does not depend on the
# chosen basis.
underlying_starts <- underlying_starts %>%
  rename(
    geo_region = all_of(geo_cols$region),
    geo_lad    = all_of(geo_cols$lad)
  )

available_years <- sort(unique(underlying_starts$academic_year))
message("  Years available: ", paste(available_years, collapse = ", "))


# --- SECTION 4: EXACT STANDARD-YEAR STARTS -----------------------------------
# Unlike the public table-tool extracts, the underlying files contain granular
# unsuppressed start counts and do not replace small counts with `low`. This produces
# the exact historical series used for the report's activity, low-volume and
# concentration charts. ST reference is used where present, with a normalised
# name fallback for the small number of historical rows without a code.

aggregate_exact_standard_starts <- function(df, year_col) {
  year_col <- rlang::ensym(year_col)

  df %>%
    mutate(
      time_period = suppressWarnings(as.integer(
        str_remove_all(as.character(!!year_col), "[^0-9]")
      )),
      standard_ref = if_else(
        !is.na(.data$st_code) & str_trim(as.character(.data$st_code)) != "",
        str_to_upper(str_trim(as.character(.data$st_code))),
        paste0("NAME:", str_squish(str_to_lower(.data$std_fwk_name)))
      )
    ) %>%
    filter(
      str_to_lower(str_trim(.data$std_fwk_flag)) == "standard",
      !is.na(.data$time_period),
      .data$time_period <= as.integer(ref_year),
      !is.na(.data$std_fwk_name),
      str_trim(.data$std_fwk_name) != "",
      !is.na(.data$starts),
      .data$starts > 0
    ) %>%
    group_by(.data$time_period, .data$standard_ref) %>%
    summarise(starts = sum(.data$starts), .groups = "drop") %>%
    mutate(
      academic_year = paste0(
        str_sub(as.character(.data$time_period), 1, 4),
        "/",
        str_sub(as.character(.data$time_period), 5, 6)
      )
    ) %>%
    select(.data$time_period, .data$academic_year, .data$standard_ref, .data$starts)
}

if (any(underlying_starts$starts_suppressed, na.rm = TRUE)) {
  stop(
    "The current underlying starts archive contains suppressed start values; ",
    "exact historical outputs cannot be produced.",
    call. = FALSE
  )
}

historical_current_exact <- aggregate_exact_standard_starts(
  underlying_starts,
  academic_year
)


# --- SECTION 5: GEOGRAPHIC AGGREGATES ----------------------------------------
# All cuts are produced in one pass while the table is in memory.
#
# `starts` is NA wherever the source was suppressed, so sums use na.rm and are
# reported alongside a count of the suppressed cells they exclude. Counts of
# standards and of areas include suppressed observations, because the standard
# is present in that area regardless of whether its volume can be published.

summarise_geo <- function(df, ...) {
  df %>%
    group_by(...) %>%
    summarise(
      starts            = sum(.data$starts, na.rm = TRUE),
      suppressed_cells  = sum(.data$starts_suppressed, na.rm = TRUE),
      standards         = n_distinct(.data$std_fwk_name),
      .groups = "drop"
    )
}


message("\nAggregating ...")

# Region x sector subject area. Replaces the regional subject analysis
# previously taken from the single-year geography table.
geo_region_ssa <- underlying_starts %>%
  filter(!is.na(.data$geo_region), .data$geo_region != "") %>%
  summarise_geo(.data$academic_year, .data$geo_region, .data$ssa_tier_1)

# Local authority district x sector subject area.
geo_lad_ssa <- underlying_starts %>%
  filter(!is.na(.data$geo_lad), .data$geo_lad != "") %>%
  summarise_geo(.data$academic_year, .data$geo_lad, .data$ssa_tier_1)

# Region x apprenticeship level.
geo_region_level <- underlying_starts %>%
  filter(!is.na(.data$geo_region), .data$geo_region != "") %>%
  summarise_geo(.data$academic_year, .data$geo_region, .data$apps_level)

# Local authority district x individual standard. This is the cut that lets
# thin-market tiers be mapped to place directly, rather than estimated by
# allocating provider totals across areas.
geo_lad_standard <- underlying_starts %>%
  filter(
    !is.na(.data$geo_lad), .data$geo_lad != "",
    .data$std_fwk_flag == "Standard"
  ) %>%
  mutate(
    standard_ref = str_to_upper(str_trim(as.character(.data$st_code)))
  ) %>%
  group_by(.data$academic_year, .data$geo_lad, .data$standard_ref, .data$std_fwk_name) %>%
  summarise(
    starts           = sum(.data$starts, na.rm = TRUE),
    suppressed_cells = sum(.data$starts_suppressed, na.rm = TRUE),
    .groups = "drop"
  )

for (obj in c("geo_region_ssa", "geo_lad_ssa", "geo_region_level", "geo_lad_standard")) {
  message("  ", obj, ": ", format(nrow(get(obj)), big.mark = ","), " rows")
}
rm(obj)


# --- SECTION 6: RECONCILE -----------------------------------------------------
# Confirms the aggregates capture the expected national volume before anything
# downstream uses them. Sums exclude suppressed cells, so a shortfall against
# the published total is expected; its size is the useful diagnostic.

geo_coverage_check <- underlying_starts %>%
  group_by(.data$academic_year) %>%
  summarise(
    starts_summed    = sum(.data$starts, na.rm = TRUE),
    suppressed_cells = sum(.data$starts_suppressed, na.rm = TRUE),
    rows             = n(),
    .groups = "drop"
  ) %>%
  mutate(
    pct_cells_suppressed = round(100 * .data$suppressed_cells / .data$rows, 1)
  )

print(geo_coverage_check)

ref_year_label_underlying <- str_replace(as.character(ref_year), "^(\\d{4})(\\d{2})$", "\\1/\\2")

ref_row <- geo_coverage_check %>%
  filter(as.character(.data$academic_year) %in%
           c(as.character(ref_year), ref_year_label, ref_year_label_underlying))

if (nrow(ref_row) == 1) {
  message(sprintf(
    "\n  Reference year %s: %s starts summed against a published total of %s (%.1f%% captured).",
    ref_year_label,
    format(ref_row$starts_summed, big.mark = ","),
    format(353500, big.mark = ","),
    100 * ref_row$starts_summed / 353500
  ))
} else {
  warning(
    "Could not identify the reference year in the underlying file. ",
    "Values present: ", paste(available_years, collapse = ", "),
    call. = FALSE
  )
}


# --- SECTION 7: ADD THE 2017/18 TO 2019/20 ARCHIVE ---------------------------
# Read only the five required columns from the older archive. Header names
# changed between releases, so they are normalised before the common aggregate
# function is applied.

historical_unzip_dir <- file.path(tempdir(), "underlying_starts_201920")
dir.create(historical_unzip_dir, recursive = TRUE, showWarnings = FALSE)

message("\nUnpacking ", basename(underlying_starts_historical_zip), " ...")
unzip(
  underlying_starts_historical_zip,
  exdir = historical_unzip_dir,
  overwrite = TRUE
)

historical_csv <- list.files(
  historical_unzip_dir,
  pattern = "\\.csv$",
  full.names = TRUE
)

if (length(historical_csv) != 1) {
  stop(
    "Expected exactly one CSV in the 2019/20 archive, found ",
    length(historical_csv),
    ".",
    call. = FALSE
  )
}

historical_header <- read_csv(
  historical_csv,
  n_max = 0,
  show_col_types = FALSE,
  progress = FALSE
)

normalise_header <- function(x) {
  x %>%
    str_to_lower() %>%
    str_replace_all("[^a-z0-9]+", "_") %>%
    str_replace_all("^_|_$", "")
}

historical_header_map <- tibble(
  raw_name = names(historical_header),
  clean_name = normalise_header(names(historical_header))
)

historical_required <- c(
  "year",
  "std_fwk_flag",
  "framework_standard_name",
  "st_code",
  "starts"
)

historical_missing <- setdiff(
  historical_required,
  historical_header_map$clean_name
)

if (length(historical_missing) > 0) {
  stop(
    "Expected columns not found in the 2019/20 underlying starts file:\n  ",
    paste(historical_missing, collapse = "\n  "),
    call. = FALSE
  )
}

historical_selected_raw <- historical_header_map %>%
  filter(.data$clean_name %in% historical_required) %>%
  pull(.data$raw_name)

historical_underlying <- read_csv(
  historical_csv,
  col_select = all_of(historical_selected_raw),
  col_types = cols(.default = col_character()),
  progress = FALSE,
  show_col_types = FALSE
)

names(historical_underlying) <- normalise_header(names(historical_underlying))

historical_underlying <- historical_underlying %>%
  rename(std_fwk_name = framework_standard_name) %>%
  mutate(starts = suppressWarnings(as.numeric(str_remove_all(.data$starts, ","))))

if (any(is.na(historical_underlying$starts))) {
  stop(
    "The 2019/20 underlying starts archive contains non-numeric start values; ",
    "exact historical outputs cannot be produced.",
    call. = FALSE
  )
}

historical_early_exact <- aggregate_exact_standard_starts(
  historical_underlying,
  year
)

historical_standard_starts_exact <- bind_rows(
  historical_early_exact,
  historical_current_exact
) %>%
  arrange(.data$time_period, desc(.data$starts), .data$standard_ref)

historical_duplicate_keys <- historical_standard_starts_exact %>%
  count(.data$time_period, .data$standard_ref, name = "rows") %>%
  filter(.data$rows > 1)

if (nrow(historical_duplicate_keys) > 0) {
  stop(
    "historical_standard_starts_exact contains duplicate year-standard keys.",
    call. = FALSE
  )
}

historical_exact_check <- historical_standard_starts_exact %>%
  group_by(.data$time_period, .data$academic_year) %>%
  summarise(
    standards_with_starts = n(),
    standards_below_50 = sum(.data$starts < low_start_cutoff),
    total_standard_starts = sum(.data$starts),
    .groups = "drop"
  )

ref_exact_check <- historical_exact_check %>%
  filter(.data$time_period == as.integer(ref_year))

if (
  nrow(ref_exact_check) != 1 ||
  ref_exact_check$standards_with_starts != 644 ||
  ref_exact_check$standards_below_50 != 241 ||
  ref_exact_check$total_standard_starts != 353463
) {
  stop(
    "Exact historical reconciliation failed for ", ref_year_label, ". ",
    "Expected 644 delivered standards, 241 below 50 starts and 353,463 ",
    "standard-based starts.",
    call. = FALSE
  )
}

print(historical_exact_check, n = Inf)


# --- SECTION 8: CLEAN UP ------------------------------------------------------
# The raw table is removed here deliberately. Only the aggregates above are
# carried forward, so no later code can accidentally take an unfiltered total
# from the full-detail file.

rm(
  underlying_starts,
  historical_underlying,
  historical_early_exact,
  historical_current_exact,
  historical_header,
  historical_header_map
)
unlink(unzip_dir, recursive = TRUE)
unlink(historical_unzip_dir, recursive = TRUE)
invisible(gc())

saveRDS(
  list(
    cache_metadata    = cache_signature,
    geo_region_ssa     = geo_region_ssa,
    geo_lad_ssa        = geo_lad_ssa,
    geo_region_level   = geo_region_level,
    geo_lad_standard   = geo_lad_standard,
    geo_coverage_check = geo_coverage_check,
    historical_standard_starts_exact = historical_standard_starts_exact
  ),
  geo_cache
)

message("\n  Aggregates cached to ", basename(geo_cache),
        " (", round(file.size(geo_cache) / 1024^2, 1), " MB).")
message("03_prep_geography.R complete. Raw table discarded; aggregates retained.")

}
