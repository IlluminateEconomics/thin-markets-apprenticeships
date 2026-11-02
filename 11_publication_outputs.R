# ==============================================================================
# 11_publication_outputs.R
# Export the six audited figures with the production R house style
# ==============================================================================
# Run by 99_run_all.R after 09_visualisations.R and 10_maps.R.
# All values are computed upstream from the original project inputs. This file
# never reads the Python audit pictures, temporary audit CSVs or checkpoint ZIP.
# Report and production chart numbers differ, so preserve production filenames
# and provide explicit aliases matching the publication checklist.

publication_required <- c(
  "chart_live_stock", "chart_a9", "chart_a8", "figure_2", "map_1",
  "map_a3_sector", "figure_2_summary", "chart_a9_links", "chart_a8_data",
  "map_a3_sector_data", "geo_review_group", "thin_market_is8"
)

publication_missing <- publication_required[
  !vapply(publication_required, exists, logical(1), inherits = TRUE)
]
if (length(publication_missing) > 0) {
  stop("Run 99_run_all.R first. Missing publication objects: ",
       paste(publication_missing, collapse = ", "), call. = FALSE)
}

publication_checks <- list()
publication_check <- function(issue, actual, expected, tolerance = 1e-8) {
  passed <- length(actual) == 1 && length(expected) == 1 &&
    !is.na(actual) && !is.na(expected) && abs(actual - expected) <= tolerance
  publication_checks[[length(publication_checks) + 1L]] <<- tibble::tibble(
    check = issue, actual = as.numeric(actual), expected = as.numeric(expected),
    passed = passed
  )
}

publication_check("All-proxy mapped plus excluded starts reconcile",
                  figure_2_summary$mapped_starts + figure_2_summary$outside_unknown_starts,
                  figure_2_summary$selected_starts)
publication_check("Review-group mapped plus excluded starts reconcile",
                  sum(map_1_data$starts) +
                    sum(geo_review_group$starts[is.na(geo_review_group$lsip_key)]),
                  sum(geo_review_group$starts))
publication_check("Chart sector links reconcile", sum(chart_a9_data$standards), nrow(chart_a9_links))
publication_check("Geography chart denominators reconcile",
                  sum(chart_a8_data$standards), geographic_observed_n)
publication_check("Sector panels have every analytical area",
                  nrow(map_a3_sector_data), nrow(lsip_boundary_base) * length(is8_sectors_ordered))
publication_check("ST0740 has no false HR sector link",
                  sum(chart_a9_links$standard_ref == "ST0740" &
                        chart_a9_links$is8_sector == "Professional and Business Services"), 0)

# The saved audit is frozen at these dates. For a later release retain the
# structural checks above, but do not force old publication totals onto it.
publication_frozen_snapshot <- as.character(ref_year) == "202425" &&
  skills_england_asat == "1 June 2026" && geo_basis == "delivery"

if (publication_frozen_snapshot) {
  publication_check("Delivered standards", nrow(thin_market_is8), 644)
  publication_check("Severe plus thin standards", review_group_standards, 202)
  publication_check("Unique core-sector review standards", core_review_total, 139)
  publication_check("Unique core plus foundational review standards", expanded_review_total, 165)
  publication_check("Inclusive sector-category links", nrow(chart_a9_links), 191)
  publication_check("Review standards with multiple mapped categories", review_group_overlap, 24)
  publication_check("Review standards with multiple core sectors", core_review_group_overlap, 23)
  publication_check("PBS linked review standards",
                    sum(chart_a9_links$is8_sector == "Professional and Business Services"), 23)
  publication_check("Geographically matched eligible standards", geographic_observed_n, 633)
  publication_check("Eligible geography standards", geographic_expected_n, 637)
  publication_check("Analytical areas", n_lsips, 42)

  expected_reach <- tibble::tribble(
    ~tier, ~standards, ~standards_in_over_quarter,
    "Severe thin market", 114, 1,
    "Thin market", 84, 22,
    "Potential thin market", 90, 43,
    "Low-volume review group", 159, 127,
    "Scaled market", 186, 183
  )
  for (i in seq_len(nrow(expected_reach))) {
    tier_name <- expected_reach$tier[[i]]
    actual <- chart_a8_data[as.character(chart_a8_data$tier) == tier_name, ]
    publication_check(paste(tier_name, "mapped denominator"),
                      actual$standards, expected_reach$standards[[i]])
    publication_check(paste(tier_name, "reach numerator"),
                      actual$standards_in_over_quarter,
                      expected_reach$standards_in_over_quarter[[i]])
  }
  publication_check("All-proxy selected standards", figure_2_summary$selected_standards, 503)
  publication_check("All-proxy mapped standards", figure_2_summary$mapped_standards, 499)
  publication_check("All-proxy mapped starts", figure_2_summary$mapped_starts, 232707)
  publication_check("All-proxy excluded starts", figure_2_summary$outside_unknown_starts, 5133)
  publication_check("Review-group mapped standards",
                    n_distinct(geo_review_group$standard_ref[!is.na(geo_review_group$lsip_key)]), 161)
  publication_check("Review-group mapped starts", sum(map_1_data$starts), 2578)
  publication_check("Review-group excluded starts",
                    sum(geo_review_group$starts[is.na(geo_review_group$lsip_key)]), 96)
  publication_check("Suppressed review-group source cells", sum(geo_review_group$suppressed_cells), 0)
  pbs_data <- map_a3_sector_data %>% sf::st_drop_geometry() %>%
    filter(as.character(.data$is8_sector) == "Professional and Business Services")
  publication_check("PBS mapped starts", sum(pbs_data$starts), 374)
  publication_check("PBS South East Midlands starts",
                    sum(pbs_data$starts[pbs_data$lsip_key == "south east midlands"]), 1)
  publication_check("Peak approved stock", max(live_stock$live), 719)
  publication_check("Final approved stock", live_stock_latest$live, 712)
  publication_check("Final paused stock", stock_paused_at_snapshot, 37)
  publication_check("Final available stock", stock_available_at_snapshot, 675)
}

publication_value_checks <- dplyr::bind_rows(publication_checks)
save_table(publication_value_checks, "publication_value_checks.csv")
if (any(!publication_value_checks$passed)) {
  stop("Publication reconciliation failed: ",
       paste(publication_value_checks$check[!publication_value_checks$passed], collapse = "; "),
       ". Inspect publication_value_checks.csv before using the figures.", call. = FALSE)
}

publication_figure_manifest <- tibble::tribble(
  ~report_reference, ~audit_stem, ~production_png, ~plot_object, ~width, ~height,
  "Chart 3", "chart_03_reconstructed_stock", "chart_03_live_stock_of_standards.png", "chart_live_stock", 9.2, 5.8,
  "Chart 9", "chart_09_corrected_inclusive_sector_links", "chart_10_is8_review_group_links.png", "chart_a9", 10.2, 7.6,
  "Chart 10", "chart_10_corrected_geographic_reach", "chart_11_geographic_reach_by_market_tier.png", "chart_a8", 9.4, 5.8,
  "Figure 2", "figure_02_direct_all_proxy_linked_starts", "figure_02_all_proxy_linked_delivery.png", "figure_2", 9.2, 8.8,
  "Map 2", "map_02_direct_review_group_starts", "map_01_review_group_delivery_by_lsip.png", "map_1", 9.2, 8.0,
  "Map 3", "map_03_corrected_sector_panels", "map_02_review_group_delivery_by_sector.png", "map_a3_sector", 13.2, 11.0
)

for (i in seq_len(nrow(publication_figure_manifest))) {
  spec <- publication_figure_manifest[i, ]
  plot <- get(spec$plot_object, inherits = TRUE)
  save_chart(plot, paste0(spec$audit_stem, ".png"), width = spec$width, height = spec$height)
  # The base SVG device avoids introducing an svglite dependency.
  ggplot2::ggsave(
    filename = file.path(chart_folder, paste0(spec$audit_stem, ".svg")),
    plot = plot, device = grDevices::svg,
    width = spec$width, height = spec$height, bg = "white"
  )
}

publication_data <- list(
  chart_03_reconstructed_stock_data = live_stock,
  chart_09_corrected_inclusive_sector_links_data = chart_a9_data %>%
    select(is8_sector, tier, scope, standards),
  chart_10_corrected_geographic_reach_data = chart_a8_data %>%
    select(-label_x, -label_hjust),
  figure_02_full_proxy_linked_direct_map = figure_2_data %>% sf::st_drop_geometry() %>%
    transmute(analytical_area = .data$boundary_lsip_name, key = .data$lsip_key,
              starts = .data$starts, standards = .data$standards),
  map_02_direct_review_group_starts_data = map_1_data %>% sf::st_drop_geometry() %>%
    select(analytical_area = boundary_lsip_name, lsip_key, starts, standards, suppressed_cells),
  map_03_corrected_sector_panels_data = map_a3_sector_data %>% sf::st_drop_geometry() %>%
    select(lsip_key, is8_sector, starts, standards, suppressed_cells,
           sector_total_starts, pct_sector_starts)
)
for (name in names(publication_data)) {
  save_table(publication_data[[name]], paste0(name, ".csv"))
}

jsonlite::write_json(
  c(as.list(figure_2_summary[1, ]), list(
    reference_year = ref_year_label, catalogue_date = skills_england_asat,
    geographic_basis = geo_basis,
    rendering = "Production R ggplot2; original house palette and themes; no gridlines.",
    publication_checks_passed = all(publication_value_checks$passed)
  )),
  path = file.path(table_folder, "figure_summary.json"),
  pretty = TRUE, auto_unbox = TRUE
)
save_table(publication_figure_manifest, "publication_figure_manifest.csv")

# 99_run_all checks these are fresh and non-empty, as it does the primary files.
publication_expected_outputs <- dplyr::bind_rows(
  tibble::tibble(output_type = "chart", file = c(
    paste0(publication_figure_manifest$audit_stem, ".png"),
    paste0(publication_figure_manifest$audit_stem, ".svg")
  )),
  tibble::tibble(output_type = "table", file = c(
    paste0(names(publication_data), ".csv"), "figure_summary.json",
    "figure_02_all_proxy_linked_delivery_summary.csv"
  ))
)

message("\n11_publication_outputs.R complete: all six audit figures and supporting data ",
        "exported in the original R house style; ", nrow(publication_value_checks), " checks passed.")
