# ==============================================================================
# 99_run_all.R
# Rebuild the complete apprenticeship thin-market analysis and report outputs
# ==============================================================================
#
# Run from the folder containing the numbered scripts:
#   source("99_run_all.R")
#
# This script sources the production pipeline in numeric order, removes the
# explicitly listed legacy report outputs, then checks the final sequentially
# numbered charts, maps and report tables exist.
# ==============================================================================


pipeline_scripts <- c(
  "00_config.R",
  "01_functions.R",
  "02_load_data.R",
  "03_prep_geography.R",
  "04_source_of_truth.R",
  "05_standards_market.R",
  "06_quadrant_sensitivity.R",
  "07_geography.R",
  "08_approval_dates.R",
  "09_visualisations.R",
  "10_maps.R"
)


missing_scripts <- pipeline_scripts[!file.exists(pipeline_scripts)]

if (length(missing_scripts) > 0) {
  stop(
    "The full pipeline cannot run because these scripts are missing:\n  ",
    paste(missing_scripts, collapse = "\n  "),
    "\nSet the working directory to Claude Analysis/Scripts-Clean.",
    call. = FALSE
  )
}


pipeline_started <- Sys.time()
script_run_log <- vector("list", length(pipeline_scripts))


for (i in seq_along(pipeline_scripts)) {
  script_name <- pipeline_scripts[[i]]
  script_started <- Sys.time()

  message(
    "\n[", i, "/", length(pipeline_scripts), "] Running ", script_name, " ..."
  )

  source(script_name, local = globalenv(), chdir = FALSE)

  # 00_config.R defines the output folders. Remove only the known legacy report
  # files, never a broad directory or wildcard, before new outputs are written.
  if (identical(script_name, "00_config.R")) {
    legacy_chart_outputs <- c(
      "chart_1_thin_market_tiers.png",
      "chart_a1_starts_since_levy.png",
      "chart_a2_publication_threshold_trend.png",
      "chart_a3_low_starts_over_time.png",
      "chart_a4_starts_by_age_and_level.png",
      "chart_a5_review_group_by_route.png",
      "chart_a6_concentration_over_time.png",
      "chart_a6_starts_concentration.png",
      "chart_a7_maximum_funding_by_tier.png",
      "chart_a8_geographic_reach_by_tier.png",
      "chart_a9_is8_review_group.png",
      "live_stock_of_standards.png",
      "chart_annex2_median_quadrant.png",
      "chart_annex2_threshold_sensitivity.png",
      "chart_12_median_market_quadrant.png",
      "chart_13_threshold_sensitivity.png",
      "map_1_is8_review_group_by_lsip.png",
      "map_a3_is8_review_group_by_lad.png",
      "map_a3_is8_review_group_by_sector.png"
    )

    legacy_table_outputs <- c(
      "tier_examples.csv",
      "tier_summary.csv",
      "sensitivity_starts_threshold.csv",
      "chart_1_thin_market_tiers_data.csv",
      "chart_a1_starts_since_levy_data.csv",
      "chart_a2_publication_threshold_trend_data.csv",
      "chart_a3_low_starts_over_time_data.csv",
      "chart_a4_starts_by_age_and_level_data.csv",
      "chart_a4_starts_by_age_and_level_reconciliation.csv",
      "chart_a5_review_group_by_route_data.csv",
      "chart_a6_concentration_over_time_data.csv",
      "chart_a6_starts_concentration_data.csv",
      "chart_a7_maximum_funding_by_tier_data.csv",
      "chart_a7_funding_coverage.csv",
      "chart_a8_geographic_reach_by_tier_data.csv",
      "chart_a9_is8_multilabel_links.csv",
      "chart_a9_is8_review_group_data.csv",
      "live_stock_of_standards.csv",
      "chart_annex2_median_quadrant_data.csv",
      "chart_annex2_median_quadrant_thresholds.csv",
      "chart_annex2_threshold_sensitivity_data.csv",
      "chart_12_median_market_quadrant_data.csv",
      "chart_12_median_market_quadrant_thresholds.csv",
      "chart_13_threshold_sensitivity_data.csv",
      "map_1_is8_review_group_by_lsip_data.csv",
      "map_a3_is8_multilabel_links.csv",
      "map_a3_is8_multilabel_link_summary.csv",
      "map_a3_is8_review_group_by_lad_data.csv",
      "map_a3_is8_review_group_by_sector_data.csv"
    )

    unlink(file.path(chart_folder, legacy_chart_outputs), force = TRUE)
    unlink(file.path(table_folder, legacy_table_outputs), force = TRUE)
  }

  script_finished <- Sys.time()
  script_run_log[[i]] <- data.frame(
    script = script_name,
    started = format(script_started, "%Y-%m-%d %H:%M:%S"),
    finished = format(script_finished, "%Y-%m-%d %H:%M:%S"),
    elapsed_seconds = round(
      as.numeric(difftime(script_finished, script_started, units = "secs")),
      1
    ),
    stringsAsFactors = FALSE
  )
}


script_run_log <- dplyr::bind_rows(script_run_log)


expected_outputs <- dplyr::bind_rows(
  tibble::tibble(
    output_type = "chart",
    file = c(
      "chart_01_apprenticeship_starts_over_time.png",
      "chart_02_standards_with_recorded_starts.png",
      "chart_03_live_stock_of_standards.png",
      "chart_04_starts_by_age_and_level.png",
      "chart_05_maximum_funding_by_market_tier.png",
      "chart_06_low_start_share_over_time.png",
      "chart_07_review_group_by_route.png",
      "chart_08_top_100_concentration_over_time.png",
      "chart_09_market_tiers.png",
      "chart_10_is8_review_group_links.png",
      "chart_11_geographic_reach_by_market_tier.png",
      "map_01_review_group_delivery_by_lsip.png",
      "map_02_review_group_delivery_by_sector.png"
    )
  ),
  tibble::tibble(
    output_type = "table",
    file = c(
      "canonical_headlines.csv",
      "table_01_severe_thin_examples.csv",
      "table_03_market_tier_summary.csv",
      "table_04_starts_threshold_sensitivity.csv",
      "standards_population_summary.csv",
      "is8_summary.csv",
      "is8_review_group_standards.csv",
      "is8_review_group_totals.csv",
      "is8_mapping_audit.csv",
      "chart_01_apprenticeship_starts_over_time_data.csv",
      "chart_02_standards_with_recorded_starts_data.csv",
      "chart_03_live_stock_of_standards_data.csv",
      "chart_04_starts_by_age_and_level_data.csv",
      "chart_05_maximum_funding_by_market_tier_data.csv",
      "chart_06_low_start_share_over_time_data.csv",
      "chart_07_review_group_by_route_data.csv",
      "chart_08_top_100_concentration_over_time_data.csv",
      "chart_09_market_tiers_data.csv",
      "chart_10_is8_review_group_links_data.csv",
      "chart_11_geographic_reach_by_market_tier_data.csv",
      "map_01_review_group_delivery_by_lsip_data.csv",
      "map_02_review_group_delivery_by_sector_data.csv",
      "map_coverage_summary.csv",
      "geography_standard_link_audit.csv",
      "standard_link_audit.csv"
    )
  )
) %>%
  dplyr::mutate(
    path = dplyr::if_else(
      .data$output_type == "chart",
      file.path(chart_folder, .data$file),
      file.path(table_folder, .data$file)
    ),
    exists = file.exists(.data$path)
  )


output_info <- file.info(expected_outputs$path)

expected_outputs <- expected_outputs %>%
  dplyr::mutate(
    size_bytes = as.numeric(output_info$size),
    modified_time = output_info$mtime,
    nonempty = .data$exists & !is.na(.data$size_bytes) & .data$size_bytes > 0,
    generated_this_run = .data$exists & !is.na(.data$modified_time) &
      .data$modified_time >= pipeline_started,
    verified = .data$exists & .data$nonempty & .data$generated_this_run,
    modified = format(.data$modified_time, "%Y-%m-%d %H:%M:%S")
  ) %>%
  dplyr::select(-modified_time)


save_table(script_run_log, "production_script_run_log.csv")
save_table(expected_outputs, "production_output_manifest.csv")


missing_outputs <- expected_outputs %>%
  dplyr::filter(!.data$verified)


if (nrow(missing_outputs) > 0) {
  stop(
    "The pipeline did not produce fresh, non-empty copies of these expected outputs:\n  ",
    paste(missing_outputs$file, collapse = "\n  "),
    "\nInspect the relevant script messages and output folders.",
    call. = FALSE
  )
}


pipeline_finished <- Sys.time()

message(
  "\n99_run_all.R complete. ",
  nrow(expected_outputs),
  " expected outputs verified as fresh and non-empty in ",
  round(as.numeric(difftime(pipeline_finished, pipeline_started, units = "mins")), 1),
  " minutes."
)
