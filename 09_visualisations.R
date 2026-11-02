# ==============================================================================
# 09_visualisations.R
# Report visualisations
# ==============================================================================
#
# READS (from earlier scripts):
#   thin_market_is8   one row per delivered standard, with starts, providers
#                    and the canonical six-tier classification
#   tier_summary      standards and starts by tier
#
# PRODUCES:
#   Outputs/charts/chart_01_apprenticeship_starts_over_time.png
#   Outputs/charts/chart_02_standards_with_recorded_starts.png
#   Outputs/charts/chart_03_live_stock_of_standards.png
#   Outputs/charts/chart_04_starts_by_age_and_level.png
#   Outputs/charts/chart_05_maximum_funding_by_market_tier.png
#   Outputs/charts/chart_06_low_start_share_over_time.png
#   Outputs/charts/chart_07_review_group_by_route.png
#   Outputs/charts/chart_08_top_100_concentration_over_time.png
#   Outputs/charts/chart_09_market_tiers.png
#   Outputs/charts/chart_10_is8_review_group_links.png
#   Outputs/charts/chart_11_geographic_reach_by_market_tier.png
#   Corresponding chart-data CSVs are saved under Outputs/tables. The live-stock
#   CSV is produced by 08_approval_dates.R; this script owns its presentation.
#
# Run with the working directory set to the folder containing this script.
# The sources below make the script reproducible in a fresh R session.
# ==============================================================================


library(tidyverse)


required_objects <- c(
  "thin_market_is8", "tier_summary", "tier_levels", "tier_thresholds",
  "standards_no_recorded_starts", "ref_year_label",
  "chart_trend_start_year", "levy_first_full_year",
  "theme_house", "save_table", "save_chart"
)


# Reuse the canonical objects when the production pipeline has already run.
# In a fresh R session, load the minimum upstream scripts needed for Chart 9.
if (!all(vapply(required_objects, exists, logical(1), inherits = TRUE))) {
  source("02_load_data.R")
  source("04_source_of_truth.R")
}




# --- CHART 9: STARTS AND PROVIDERS BY THIN-MARKET TIER ------------------------
#
# Analytical question:
#   How do standards divide into distinct market situations when learner volume
#   and the number of active providers are considered together?
#
# Population:
#   Standards with positive recorded starts and at least one active provider in
#   2024/25. Current standards with no recorded starts remain outside the market
#   tiers because neither provider concentration nor geographic reach can be
#   measured for them.


chart_1_data <- thin_market_is8 %>%
  transmute(
    standard_ref,
    standard_name,
    route,
    level,
    starts,
    providers,
    tier,
    delivery_context,
    is8_sector
  ) %>%
  mutate(
    tier = factor(.data$tier, levels = tier_levels),
    plot_order = match(
      as.character(.data$tier),
      c(
        "Scaled market",
        "Low-volume review group",
        "Potential thin market",
        "Provider dependency market",
        "Thin market",
        "Severe thin market"
      )
    )
  ) %>%
  arrange(.data$plot_order, .data$starts, .data$providers)




# Fail loudly if the chart population drifts away from the source-of-truth
# classification or admits rows that cannot be shown on logarithmic axes.
if (nrow(chart_1_data) != nrow(thin_market_is8)) {
  stop("Chart 9 population does not match the canonical market-tier population.",
       call. = FALSE)
}


if (anyDuplicated(chart_1_data$standard_ref) > 0) {
  stop("Chart 9 contains duplicate standard references.", call. = FALSE)
}


if (any(is.na(chart_1_data$starts) | chart_1_data$starts <= 0 |
        is.na(chart_1_data$providers) | chart_1_data$providers <= 0)) {
  stop("Chart 9 contains non-positive or missing starts/provider counts.",
       call. = FALSE)
}


chart_1_tier_check <- chart_1_data %>%
  count(.data$tier, name = "chart_standards") %>%
  left_join(
    tier_summary %>% select(tier, source_standards = standards),
    by = "tier"
  )


if (any(chart_1_tier_check$chart_standards != chart_1_tier_check$source_standards)) {
  stop("Chart 9 tier counts do not match tier_summary.", call. = FALSE)
}




# Match the six-tier palette already used in the report draft. The categories
# remain distinguishable by hue, while scaled markets are muted so the review
# groups stay visually prominent.
tier_palette <- c(
  "Severe thin market"         = "#CC0000",
  "Thin market"                = "#E07B00",
  "Potential thin market"      = "#E8C400",
  "Low-volume review group"    = "#6BB5E8",
  "Provider dependency market" = "#8E44AD",
  "Scaled market"              = "#CCCCCC"
)


tier_alpha <- c(
  "Severe thin market"         = 1.00,
  "Thin market"                = 1.00,
  "Potential thin market"      = 0.90,
  "Low-volume review group"    = 0.72,
  "Provider dependency market" = 1.00,
  "Scaled market"              = 0.45
)


tier_legend_labels <- tier_summary %>%
  transmute(
    tier = as.character(.data$tier),
    label = paste0(.data$tier, " (", scales::comma(.data$standards), ")")
  ) %>%
  {
    setNames(.$label, .$tier)
  }


primary_starts_threshold <- tier_thresholds$thin$starts
primary_provider_threshold <- tier_thresholds$thin$providers




chart_1 <- ggplot(
  chart_1_data,
  aes(x = .data$providers, y = .data$starts)
) +
  # Show only the report's primary review threshold. The full six-tier
  # classification remains encoded by point colour and documented in the note.
  geom_vline(
    xintercept = primary_provider_threshold,
    colour = "#444444", linetype = "dashed", linewidth = 0.55
  ) +
  geom_hline(
    yintercept = primary_starts_threshold,
    colour = "#444444", linetype = "dashed", linewidth = 0.55
  ) +
  geom_point(
    aes(colour = .data$tier, alpha = .data$tier),
    size = 2.35
  ) +
  annotate(
    "label",
    x = primary_provider_threshold,
    y = max(chart_1_data$starts, na.rm = TRUE) / 1.8,
    label = "3 providers",
    hjust = -0.08,
    size = 3.0,
    colour = "#444444",
    fill = "white",
    linewidth = 0.2,
    label.r = grid::unit(0.08, "lines")
  ) +
  annotate(
    "label",
    x = max(chart_1_data$providers, na.rm = TRUE) / 1.7,
    y = primary_starts_threshold,
    label = "50 starts",
    vjust = -0.25,
    size = 3.0,
    colour = "#444444",
    fill = "white",
    linewidth = 0.2,
    label.r = grid::unit(0.08, "lines")
  ) +
  scale_x_log10(
    breaks = c(1, 2, 3, 5, 10, 25, 50, 100, 250, 500),
    labels = scales::label_comma(),
    expand = expansion(mult = c(0.04, 0.10))
  ) +
  scale_y_log10(
    breaks = c(1, 5, 10, 20, 50, 100, 250, 1000, 5000, 20000),
    labels = scales::label_comma(),
    expand = expansion(mult = c(0.04, 0.10))
  ) +
  scale_colour_manual(
    values = tier_palette,
    breaks = tier_levels,
    labels = tier_legend_labels,
    drop = FALSE
  ) +
  scale_alpha_manual(
    values = tier_alpha,
    breaks = tier_levels,
    drop = FALSE
  ) +
  labs(
    title = "Apprenticeship standards by market tier",
    subtitle = paste0(
      "England, ", ref_year_label,
      ". Each point is one of ", scales::comma(nrow(chart_1_data)),
      " delivered standards; both axes use logarithmic scales."
    ),
    x = "Number of active providers",
    y = "Annual apprenticeship starts",
    colour = NULL,
    alpha = NULL,
    caption = paste0(
      "Source: DfE apprenticeship provider dashboard, ", ref_year_label,
      "; Skills England standards catalogue.",
      "\nNote: dashed lines mark the primary review threshold: fewer than 50 starts ",
      "and no more than 3 providers.",
      "\nColours show the full six-tier classification.",
      "\n", scales::comma(nrow(standards_no_recorded_starts)),
      " current standards with no recorded starts are excluded."
    )
  ) +
  guides(
    colour = guide_legend(
      override.aes = list(size = 3.2, alpha = 1)
    ),
    alpha = "none"
  ) +
  theme_house(base_size = 11.5) +
  theme(
    legend.position = "right",
    legend.text = element_text(size = 9),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.caption = element_text(
      lineheight = 1.12,
      margin = margin(t = 8)
    ),
    axis.title = element_text(face = "bold"),
    axis.ticks = element_line(colour = "black", linewidth = 0.35),
    axis.ticks.length = grid::unit(3, "pt")
  )




save_table(
  chart_1_data %>% select(-plot_order),
  "chart_09_market_tiers_data.csv"
)


save_chart(
  chart_1,
  "chart_09_market_tiers.png",
  width = 10,
  height = 7.1
)


message("\nChart 9 built and saved.")




# --- LOAD OBJECTS FOR THE ANNEX CHART BATCH ----------------------------------
# Reuse objects already present in the session. Source only the missing upstream
# analysis, so ordinary reruns do not repeat the slow geography preparation.


market_objects_current <-
  exists("low_start_trend", inherits = TRUE) &&
  exists("historical_concentration_trend", inherits = TRUE) &&
  all(c(
    "time_period", "academic_year", "standards_with_starts",
    "standards_below_50", "share_below_50"
  ) %in% names(low_start_trend)) &&
  all(c(
    "time_period", "academic_year", "top_n_standards",
    "top_n_share"
  ) %in% names(historical_concentration_trend))


if (!market_objects_current) {
  if (!exists("historical_standard_starts_exact", inherits = TRUE)) {
    source("03_prep_geography.R")
  }
  source("05_standards_market.R")
}


if (!exists("tier_lsip_summary", inherits = TRUE)) {
  if (!exists("geo_lad_standard", inherits = TRUE)) {
    source("03_prep_geography.R")
  }
  source("07_geography.R")
}




# --- CHART 2: STANDARDS WITH RECORDED STARTS ----------------------------------
# Exact count of standards with at least one recorded start in each final year.
# This is an activity measure, distinct from the stock of approved standards.


chart_a2_data <- low_start_trend %>%
  select(
    time_period,
    academic_year,
    standards_with_starts,
    total_standard_starts
  ) %>%
  # The July 2026 release contains provisional 2025/26 SN10 data through April.
  # Restrict the comparison to final full academic years, ending at 2024/25.
  filter(
    !is.na(.data$standards_with_starts),
    .data$time_period <= ref_year
  ) %>%
  arrange(.data$time_period) %>%
  mutate(
    academic_year = factor(
      .data$academic_year,
      levels = unique(.data$academic_year)
    )
  )


if (nrow(chart_a2_data) < 2) {
  stop("Chart 2 requires at least two years of comparable data.", call. = FALSE)
}


chart_a2 <- ggplot(
  chart_a2_data,
  aes(x = .data$academic_year, y = .data$standards_with_starts, group = 1)
) +
  geom_line(colour = colour_primary, linewidth = 1.1) +
  geom_point(colour = colour_primary, size = 2.8) +
  geom_text(
    aes(label = scales::comma(.data$standards_with_starts)),
    vjust = -0.8,
    size = 3.4,
    colour = colour_text
  ) +
  scale_y_continuous(
    labels = scales::label_comma(),
    expand = expansion(mult = c(0.04, 0.13))
  ) +
  labs(
    title = "Standards with recorded apprenticeship starts",
    subtitle = paste0(
      "England, ", as.character(first(chart_a2_data$academic_year)),
      " to ", as.character(last(chart_a2_data$academic_year)),
      "; final full-year data"
    ),
    x = NULL,
    y = "Standards",
    caption = paste(
      str_wrap(
        "Source: DfE unsuppressed underlying apprenticeship starts files.",
        width = 110
      ),
      str_wrap(
        paste0(
          "Note: a standard is counted when it has at least one recorded start in the year. ",
          "This activity measure is not the full stock of approved standards. ",
          "Provisional 2025/26 SN10 data are excluded."
        ),
        width = 110
      ),
      sep = "\n"
    )
  ) +
  theme_house(base_size = 11.5) +
  theme(
    axis.title.y = element_text(face = "bold"),
    plot.caption.position = "plot"
  )


save_table(chart_a2_data, "chart_02_standards_with_recorded_starts_data.csv")
save_chart(
  chart_a2,
  "chart_02_standards_with_recorded_starts.png",
  width = 9.2,
  height = 5.8
)




# --- CHART 6: LOW-START SHARE OVER TIME ---------------------------------------
# Exact share of delivered standards with fewer than 50 recorded starts.


chart_a3_data <- low_start_trend %>%
  filter(.data$time_period <= ref_year) %>%
  arrange(.data$time_period) %>%
  transmute(
    time_period,
    academic_year,
    year_index = row_number(),
    share_below_50,
    standards_with_starts,
    standards_below_50,
    total_standard_starts
  )


if (nrow(chart_a3_data) < 2) {
  stop("Chart 6 requires at least two final full academic years.", call. = FALSE)
}


chart_a3 <- ggplot(
  chart_a3_data,
  aes(x = .data$year_index, y = .data$share_below_50)
) +
  geom_line(colour = colour_primary, linewidth = 1.0) +
  geom_point(colour = colour_primary, size = 2.8) +
  geom_text(
    aes(label = scales::number(.data$share_below_50, accuracy = 0.1)),
    vjust = -0.85,
    colour = colour_text,
    size = 3.2
  ) +
  scale_x_continuous(
    breaks = chart_a3_data$year_index,
    labels = chart_a3_data$academic_year,
    expand = expansion(mult = c(0.02, 0.03))
  ) +
  scale_y_continuous(
    labels = scales::label_percent(scale = 1, accuracy = 1),
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.10))
  ) +
  labs(
    title = "Share of delivered standards with fewer than 50 starts",
    subtitle = paste0(
      "England, ", first(chart_a3_data$academic_year), " to ",
      last(chart_a3_data$academic_year), "; exact unsuppressed counts"
    ),
    x = NULL,
    y = "Share of active standards",
    caption = paste(
      str_wrap(
        "Source: DfE unsuppressed underlying apprenticeship starts files.",
        width = 110
      ),
      str_wrap(
        paste0(
          "Note: the denominator is standards with at least one recorded start in the ",
          "year. The numerator is those with 1 to 49 starts. Provisional 2025/26 ",
          "SN10 data are excluded."
        ),
        width = 110
      ),
      sep = "\n"
    )
  ) +
  theme_house(base_size = 11.5) +
  theme(
    axis.title.y = element_text(face = "bold"),
    plot.caption.position = "plot"
  )


save_table(chart_a3_data, "chart_06_low_start_share_over_time_data.csv")
save_chart(
  chart_a3,
  "chart_06_low_start_share_over_time.png",
  width = 9.5,
  height = 6.2
)




# --- CHART 7: REVIEW-GROUP EXPOSURE BY ROUTE ---------------------------------
# Compare each route's share of the severe + thin review group with its share of
# the full market population. This directly shows overrepresentation.


review_tiers <- c("Severe thin market", "Thin market")


route_market_share <- thin_market_is8 %>%
  mutate(route = replace_na(.data$route, "No route recorded")) %>%
  count(.data$route, name = "market_standards") %>%
  mutate(market_share = 100 * .data$market_standards / sum(.data$market_standards))


route_review_share <- thin_market_is8 %>%
  filter(.data$tier %in% review_tiers) %>%
  mutate(route = replace_na(.data$route, "No route recorded")) %>%
  count(.data$route, name = "review_standards") %>%
  mutate(review_share = 100 * .data$review_standards / sum(.data$review_standards))


chart_a5_data <- route_market_share %>%
  left_join(route_review_share, by = "route") %>%
  mutate(
    review_standards = replace_na(.data$review_standards, 0L),
    review_share = replace_na(.data$review_share, 0),
    percentage_point_gap = .data$review_share - .data$market_share,
    overrepresentation_ratio = if_else(
      .data$market_share > 0,
      .data$review_share / .data$market_share,
      NA_real_
    ),
    gap_label = sprintf("%+.1f pp", .data$percentage_point_gap),
    label_x = pmax(.data$market_share, .data$review_share) + 0.6,
    route_plot = fct_reorder(.data$route, .data$review_share)
  ) %>%
  arrange(desc(.data$review_share))


if (sum(chart_a5_data$review_standards) !=
    sum(thin_market_is8$tier %in% review_tiers)) {
  stop("Chart 7 review-group denominator does not reconcile.", call. = FALSE)
}


chart_a5 <- ggplot(chart_a5_data, aes(y = .data$route_plot)) +
  geom_segment(
    aes(
      x = .data$market_share,
      xend = .data$review_share,
      yend = .data$route_plot
    ),
    colour = "#B8B8B8",
    linewidth = 0.8
  ) +
  geom_point(
    aes(x = .data$market_share, colour = "All market standards"),
    size = 2.5
  ) +
  geom_point(
    aes(x = .data$review_share, colour = "Severe + thin review group"),
    size = 3.0
  ) +
  geom_text(
    aes(x = .data$label_x, label = .data$gap_label),
    hjust = 0,
    size = 2.8,
    colour = colour_text
  ) +
  scale_colour_manual(
    values = c(
      "All market standards" = "#777777",
      "Severe + thin review group" = colour_primary
    )
  ) +
  scale_x_continuous(
    labels = scales::label_percent(scale = 1, accuracy = 1),
    expand = expansion(mult = c(0.01, 0.18))
  ) +
  labs(
    title = "Thin markets are concentrated in particular apprenticeship routes",
    subtitle = paste0(
      "Share of ", scales::comma(sum(chart_a5_data$review_standards)),
      " severe or thin standards compared with the full market population"
    ),
    x = "Share of standards",
    y = NULL,
    colour = NULL,
    caption = paste0(
      "Source: DfE apprenticeship provider dashboard, ", ref_year_label,
      "; Skills England standards catalogue.",
      "\nNote: the market population contains ",
      scales::comma(nrow(thin_market_is8)),
      " standards with recorded starts. ",
      "Labels show the review-group share minus the market share."
    )
  ) +
  theme_house(base_size = 11.2) +
  theme(
    legend.position = "top",
    legend.justification = "left",
    axis.title.x = element_text(face = "bold"),
    plot.caption.position = "plot"
  )


save_table(
  chart_a5_data %>% select(-route_plot, -label_x, -gap_label),
  "chart_07_review_group_by_route_data.csv"
)
save_chart(
  chart_a5,
  "chart_07_review_group_by_route.png",
  width = 9.5,
  height = 7.8
)




# --- CHART 8: TOP-100 CONCENTRATION OVER TIME ---------------------------------
# Exact share of standard-based starts accounted for by the 100 largest
# delivered standards in each final year.


chart_a6_history_data <- historical_concentration_trend %>%
  filter(.data$time_period <= ref_year) %>%
  arrange(.data$time_period) %>%
  mutate(year_index = row_number())


if (nrow(chart_a6_history_data) < 2) {
  stop("Chart 8 requires at least two final full academic years.", call. = FALSE)
}


chart_a6 <- ggplot(
  chart_a6_history_data,
  aes(x = .data$year_index, y = .data$top_n_share)
) +
  geom_line(colour = colour_primary, linewidth = 1.05) +
  geom_point(colour = colour_primary, size = 2.8) +
  geom_text(
    aes(label = scales::number(.data$top_n_share, accuracy = 0.1)),
    vjust = -0.85,
    colour = colour_text,
    size = 3.2
  ) +
  scale_x_continuous(
    breaks = chart_a6_history_data$year_index,
    labels = chart_a6_history_data$academic_year,
    expand = expansion(mult = c(0.02, 0.03))
  ) +
  scale_y_continuous(
    labels = scales::label_percent(scale = 1, accuracy = 1),
    expand = expansion(mult = c(0.08, 0.16))
  ) +
  labs(
    title = "Share of starts accounted for by the 100 largest standards",
    subtitle = paste0(
      "England, ", first(chart_a6_history_data$academic_year), " to ",
      last(chart_a6_history_data$academic_year), "; final full-year data"
    ),
    x = NULL,
    y = "Share of standard-based starts",
    caption = paste(
      str_wrap(
        "Source: DfE unsuppressed underlying apprenticeship starts files.",
        width = 110
      ),
      str_wrap(
        paste0(
          "Note: standards are ranked separately within each academic year. ",
          "Provisional 2025/26 SN10 data are excluded."
        ),
        width = 110
      ),
      sep = "\n"
    )
  ) +
  theme_house(base_size = 11.5) +
  theme(
    axis.title.y = element_text(face = "bold"),
    plot.caption.position = "plot"
  )


save_table(
  chart_a6_history_data,
  "chart_08_top_100_concentration_over_time_data.csv"
)
save_chart(
  chart_a6,
  "chart_08_top_100_concentration_over_time.png",
  width = 9.2,
  height = 5.9
)




# --- CHART 11: GEOGRAPHIC REACH BY MARKET TIER -------------------------------
# Provider dependency is a distinct high-volume market situation, not a point on
# the ordered volume-thinness continuum, so it is excluded from this comparison.


geographic_tiers <- c(
  "Severe thin market",
  "Thin market",
  "Potential thin market",
  "Low-volume review group",
  "Scaled market"
)


chart_a8_data <- tier_lsip_summary %>%
  filter(as.character(.data$tier) %in% geographic_tiers) %>%
  mutate(
    tier = factor(as.character(.data$tier), levels = rev(geographic_tiers)),
    label_x = if_else(.data$pct_in_over_quarter >= 90,
                      .data$pct_in_over_quarter - 2,
                      .data$pct_in_over_quarter + 2),
    label_hjust = if_else(.data$pct_in_over_quarter >= 90, 1, 0)
  )


geographic_expected_n <- thin_market_is8 %>%
  filter(as.character(.data$tier) %in% geographic_tiers) %>%
  nrow()


geographic_observed_n <- sum(chart_a8_data$standards)


geographic_tier_labels <- chart_a8_data %>%
  transmute(
    tier = as.character(.data$tier),
    label = paste0(.data$tier, " (n=", scales::comma(.data$standards), ")")
  ) %>%
  {
    setNames(.$label, .$tier)
  }


if (nrow(chart_a8_data) != length(geographic_tiers)) {
  stop("Chart 11 is missing one or more geographic market tiers.", call. = FALSE)
}


chart_a8 <- ggplot(
  chart_a8_data,
  aes(x = .data$pct_in_over_quarter, y = .data$tier, fill = .data$tier)
) +
  geom_col(width = 0.68) +
  geom_text(
    aes(
      x = .data$label_x,
      hjust = .data$label_hjust,
      label = scales::number(.data$pct_in_over_quarter, accuracy = 0.1, suffix = "%")
    ),
    size = 3.6,
    colour = colour_text
  ) +
  scale_fill_manual(values = tier_palette, drop = FALSE) +
  scale_x_continuous(
    limits = c(0, 102),
    breaks = seq(0, 100, 20),
    labels = scales::label_percent(scale = 1),
    expand = expansion(mult = c(0, 0))
  ) +
  scale_y_discrete(labels = geographic_tier_labels) +
  labs(
    title = "Low-start standards have narrower recorded delivery footprints",
    subtitle = paste0("Share of mapped standards recorded in at least ",
                      floor(n_lsips / 4) + 1L, " of ", n_lsips, " analytical areas"),
    x = "Standards recorded in more than 25% of analytical areas",
    y = NULL,
    fill = NULL,
    caption = paste(
      str_wrap(paste0("Source: DfE underlying apprenticeship starts data, ", ref_year_label, "."), width = 115),
      str_wrap(paste0(
        "Recorded delivery-postcode basis; 38 non-London LSIP areas and four London subregions. ",
        "Provider-dependency markets are excluded."
      ), width = 115),
      str_wrap(paste0(
        "Geographic match coverage: ", scales::comma(geographic_observed_n),
        " of ", scales::comma(geographic_expected_n),
        " eligible standards. Outside-England/unknown-only standards are omitted."
      ), width = 115),
      "Recorded delivery locations do not measure all offered provision or employer access.",
      sep = "\n"
    )
  ) +
  guides(fill = "none") +
  theme_house(base_size = 11.5) +
  theme(
    axis.title.x = element_text(face = "bold"),
    plot.caption.position = "plot"
  )


save_table(
  chart_a8_data %>% select(-label_x, -label_hjust),
  "chart_11_geographic_reach_by_market_tier_data.csv"
)
save_chart(
  chart_a8,
  "chart_11_geographic_reach_by_market_tier.png",
  width = 9.4,
  height = 5.8
)




# --- CHART 3: LIVE STOCK OF APPRENTICESHIP STANDARDS -------------------------
# 08_approval_dates.R owns the catalogue-date calculation and saves the source
# data. This script owns the report chart. The series ends at the catalogue
# snapshot date, so the genuine flattening through June 2026 remains but no
# artificial post-snapshot flat months are added.


live_stock_objects_current <- all(vapply(
  c("live_stock", "catalogue_snapshot_date", "levy_start"),
  exists,
  logical(1),
  inherits = TRUE
))


if (!live_stock_objects_current) {
  source("08_approval_dates.R")
}


live_stock_peak <- live_stock %>%
  slice_max(.data$live, n = 1, with_ties = TRUE)

live_stock_peak_months <- paste(format(live_stock_peak$date, "%B %Y"), collapse = " and ")
stock_paused_at_snapshot <- sum(skills_england_dedup$is_paused, na.rm = TRUE)
stock_available_at_snapshot <- sum(skills_england_dedup$is_available_for_starts, na.rm = TRUE)


live_stock_latest <- live_stock %>%
  slice_tail(n = 1)


chart_live_stock <- ggplot(live_stock, aes(x = .data$date, y = .data$live)) +
  geom_vline(
    xintercept = levy_start,
    colour = colour_grey,
    linetype = "dashed",
    linewidth = 0.6
  ) +
  annotate(
    "text",
    x = levy_start,
    y = max(live_stock$live) * 0.06,
    label = "Apprenticeship levy,\nApril 2017",
    hjust = -0.08,
    vjust = 0,
    size = 3.2,
    colour = colour_text,
    lineheight = 0.95
  ) +
  geom_area(fill = colour_secondary, alpha = 0.35) +
  geom_line(colour = colour_primary, linewidth = 1) +
  scale_x_date(
    date_breaks = "2 years",
    date_labels = "%Y",
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  scale_y_continuous(
    labels = scales::comma,
    expand = expansion(mult = c(0, 0.08))
  ) +
  labs(
    title = "Approved stock of apprenticeship standards",
    subtitle = paste0(
      "Standards approved for delivery and not since retired or withdrawn, England.\n",
      "Peak: ", scales::comma(max(live_stock_peak$live)), " in ",
      live_stock_peak_months, "; ",
      "latest: ", scales::comma(live_stock_latest$live), " in ",
      format(live_stock_latest$date, "%B %Y"), "."
    ),
    x = NULL,
    y = "Approved standards not retired or withdrawn",
    caption = paste0(
      src_skills_england,
      "\nIncludes ", stock_paused_at_snapshot, " paused standards at the final snapshot; ",
      stock_available_at_snapshot, " standards were available for new starts.",
      "\nThis is reconstructed approved stock, not historical availability for new starts. ",
      "The series ends at the catalogue snapshot."
    )
  ) +
  theme_house(base_size = 11.5) +
  theme(plot.caption.position = "plot")


save_chart(
  chart_live_stock,
  "chart_03_live_stock_of_standards.png",
  width = 9.2,
  height = 5.8
)




# --- LOAD OBJECTS FOR THE SECOND CHART BATCH --------------------------------


historical_columns <- c(
  "time_period", "academic_year", "age_summary", "apps_level",
  "ssa_tier_1", "starts"
)


historical_objects_current <-
  exists("ees_historical", inherits = TRUE) &&
  all(historical_columns %in% names(ees_historical))


if (!historical_objects_current) {
  source("02_load_data.R")
}


sensitivity_objects_current <-
  exists("sens_grid", inherits = TRUE) &&
  exists("median_thresholds", inherits = TRUE) &&
  all(c(
    "starts_threshold", "provider_threshold", "standards", "pct_standards"
  ) %in% names(sens_grid)) &&
  all(c("median_starts", "median_providers") %in% names(median_thresholds))


if (!sensitivity_objects_current) {
  source("06_quadrant_sensitivity.R")
}


is_total_category <- function(x) {
  str_to_lower(str_squish(replace_na(x, ""))) %in%
    c("total", "all", "all ages", "all levels", "all sector subject areas")
}




# --- CHART 1: APPRENTICESHIP STARTS OVER TIME --------------------------------
# Total starts from the long historical EES series. Retain the full 2002/03
# context used in the report, with the levy marked within the series.


chart_a1_data <- ees_historical %>%
  filter(
    .data$time_period >= 200203,
    .data$time_period <= ref_year,
    is_total_category(.data$age_summary),
    is_total_category(.data$apps_level),
    is_total_category(.data$ssa_tier_1)
  ) %>%
  group_by(.data$time_period, .data$academic_year) %>%
  summarise(
    starts = sum(.data$starts, na.rm = TRUE),
    source_rows = n(),
    .groups = "drop"
  ) %>%
  arrange(.data$time_period) %>%
  mutate(
    year_index = row_number(),
    academic_year = factor(
      .data$academic_year,
      levels = unique(.data$academic_year)
    )
  )


if (nrow(chart_a1_data) < 20 ||
    any(chart_a1_data$source_rows != 1L) ||
    any(!is.finite(chart_a1_data$starts) | chart_a1_data$starts <= 0)) {
  stop(
    "Chart 1 requires one positive national-total row for each final year.",
    call. = FALSE
  )
}


levy_year_position <- match(
  levy_first_full_year,
  chart_a1_data$time_period
)


if (is.na(levy_year_position)) {
  stop("The first full post-levy year is missing from Chart 1.", call. = FALSE)
}


chart_a1_labels <- bind_rows(
  chart_a1_data %>% slice_head(n = 1),
  chart_a1_data %>% slice_max(.data$starts, n = 1, with_ties = FALSE),
  chart_a1_data %>% slice_tail(n = 1)
) %>%
  distinct(.data$time_period, .keep_all = TRUE) %>%
  mutate(
    hjust = case_when(
      .data$time_period == min(chart_a1_data$time_period) ~ 0,
      .data$time_period == max(chart_a1_data$time_period) ~ 1,
      TRUE ~ 0.5
    )
  )


chart_a1_breaks <- unique(c(
  chart_a1_data$year_index[seq(1, nrow(chart_a1_data), by = 2)],
  max(chart_a1_data$year_index)
))


chart_a1 <- ggplot(
  chart_a1_data,
  aes(x = .data$year_index, y = .data$starts, group = 1)
) +
  geom_vline(
    xintercept = levy_year_position - 0.5,
    colour = colour_grey,
    linetype = "dashed",
    linewidth = 0.6
  ) +
  annotate(
    "text",
    x = levy_year_position - 0.42,
    y = max(chart_a1_data$starts) * 0.96,
    label = "Levy introduced\nApril 2017",
    hjust = 0,
    vjust = 1,
    size = 3.2,
    colour = colour_text,
    lineheight = 0.95
  ) +
  geom_line(colour = colour_primary, linewidth = 1.1) +
  geom_point(colour = colour_primary, size = 2.5) +
  geom_text(
    data = chart_a1_labels,
    aes(
      label = scales::comma(.data$starts),
      hjust = .data$hjust
    ),
    vjust = -0.8,
    size = 3.3,
    colour = colour_text
  ) +
  scale_x_continuous(
    breaks = chart_a1_breaks,
    labels = as.character(chart_a1_data$academic_year[chart_a1_breaks]),
    expand = expansion(add = c(0.45, 0.45))
  ) +
  scale_y_continuous(
    labels = scales::label_comma(),
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.10))
  ) +
  labs(
    title = "Apprenticeship starts over time",
    subtitle = "England, final full academic years",
    x = NULL,
    y = "Starts",
    caption = paste0(
      "Source: DfE Explore Education Statistics, July 2026 release; ",
      "historical apprenticeship series.\n",
      "Note: the dashed line marks the levy, introduced during 2016/17; ",
      "2017/18 is the first full post-levy academic year."
    )
  ) +
  theme_house(base_size = 11.5) +
  theme(
    axis.title.y = element_text(face = "bold"),
    plot.caption.position = "plot"
  )


save_table(
  chart_a1_data %>% select(-source_rows, -year_index),
  "chart_01_apprenticeship_starts_over_time_data.csv"
)
save_chart(
  chart_a1,
  "chart_01_apprenticeship_starts_over_time.png",
  width = 10.2,
  height = 5.8
)




# --- CHART 4: STARTS BY AGE AND LEVEL ----------------------------------------
# Age and level are separate partitions of the same national total. They are
# plotted as two panels, with direct end labels and a common y-axis.


historical_window <- ees_historical %>%
  filter(
    .data$time_period >= chart_trend_start_year,
    .data$time_period <= ref_year
  )


chart_a4_age_raw <- historical_window %>%
  filter(
    is_total_category(.data$apps_level),
    is_total_category(.data$ssa_tier_1),
    !is_total_category(.data$age_summary)
  ) %>%
  mutate(
    source_category = .data$age_summary,
    series = case_when(
      str_detect(str_to_lower(.data$age_summary), "under 19|16[-– ]?18") ~ "Under 19",
      str_detect(str_to_lower(.data$age_summary), "19[-– ]?(23|24)") ~ "19–24",
      str_detect(str_to_lower(.data$age_summary), "25\\+|25 and over|25 or over") ~ "25+",
      TRUE ~ NA_character_
    ),
    series_order = match(.data$series, c("Under 19", "19–24", "25+"))
  )


unmapped_age <- chart_a4_age_raw %>%
  filter(is.na(.data$series), !is.na(.data$starts), .data$starts > 0) %>%
  distinct(.data$source_category)


if (nrow(unmapped_age) > 0) {
  stop(
    "Chart 4 has unmapped age categories: ",
    paste(unmapped_age$source_category, collapse = ", "),
    call. = FALSE
  )
}


chart_a4_age <- chart_a4_age_raw %>%
  filter(!is.na(.data$series)) %>%
  group_by(
    .data$time_period, .data$academic_year,
    .data$series, .data$series_order
  ) %>%
  summarise(starts = sum(.data$starts, na.rm = TRUE), .groups = "drop") %>%
  mutate(dimension = "Age at start")


chart_a4_level_raw <- historical_window %>%
  filter(
    is_total_category(.data$age_summary),
    is_total_category(.data$ssa_tier_1),
    !is_total_category(.data$apps_level)
  ) %>%
  mutate(
    source_category = .data$apps_level,
    series = case_when(
      str_detect(str_to_lower(.data$apps_level), "intermediate|level[[:space:]]*2") ~ "Level 2",
      str_detect(str_to_lower(.data$apps_level), "advanced|level[[:space:]]*3") ~ "Level 3",
      str_detect(str_to_lower(.data$apps_level), "higher|level[[:space:]]*[4-7]|level[[:space:]]*4\\+") ~ "Level 4+",
      TRUE ~ NA_character_
    ),
    series_order = match(.data$series, c("Level 2", "Level 3", "Level 4+"))
  )


unmapped_level <- chart_a4_level_raw %>%
  filter(is.na(.data$series), !is.na(.data$starts), .data$starts > 0) %>%
  distinct(.data$source_category)


if (nrow(unmapped_level) > 0) {
  stop(
    "Chart 4 has unmapped level categories: ",
    paste(unmapped_level$source_category, collapse = ", "),
    call. = FALSE
  )
}


chart_a4_level <- chart_a4_level_raw %>%
  filter(!is.na(.data$series)) %>%
  group_by(
    .data$time_period, .data$academic_year,
    .data$series, .data$series_order
  ) %>%
  summarise(starts = sum(.data$starts, na.rm = TRUE), .groups = "drop") %>%
  mutate(dimension = "Apprenticeship level")


chart_a4_data <- bind_rows(chart_a4_age, chart_a4_level) %>%
  mutate(
    academic_year = factor(
      .data$academic_year,
      levels = unique(chart_a1_data$academic_year)
    ),
    dimension = factor(
      .data$dimension,
      levels = c("Age at start", "Apprenticeship level")
    ),
    series_order = factor(.data$series_order, levels = 1:3)
  )


chart_a4_structure_check <- chart_a4_data %>%
  count(.data$dimension, .data$time_period, name = "series")


if (any(chart_a4_structure_check$series != 3L)) {
  stop("Chart 4 does not contain three series in every panel-year.", call. = FALSE)
}


chart_a4_reconciliation <- chart_a4_data %>%
  group_by(.data$dimension, .data$time_period) %>%
  summarise(component_starts = sum(.data$starts), .groups = "drop") %>%
  left_join(
    chart_a1_data %>%
      transmute(.data$time_period, total_starts = .data$starts),
    by = "time_period"
  ) %>%
  mutate(abs_difference = abs(.data$component_starts - .data$total_starts))


if (any(chart_a4_reconciliation$abs_difference > 100)) {
  stop(
    "Chart 4 age or level components do not reconcile to national starts.",
    call. = FALSE
  )
}


chart_a4_latest <- chart_a4_data %>%
  group_by(.data$dimension, .data$series) %>%
  slice_max(.data$time_period, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(
    label = paste0(.data$series, "  ", scales::comma(.data$starts)),
    # The 2024/25 Level 3 and Level 4+ endpoints are close enough for their
    # direct labels to overlap. Small, symmetric offsets retain direct labels
    # while keeping both values legible.
    label_y = case_when(
      .data$dimension == "Apprenticeship level" &
        .data$series == "Level 3" ~ .data$starts + 6000,
      .data$dimension == "Apprenticeship level" &
        .data$series == "Level 4+" ~ .data$starts - 6000,
      TRUE ~ .data$starts
    )
  )


series_palette <- c(
  "1" = colour_primary,
  "2" = colour_accent,
  "3" = "#8E44AD"
)


chart_a4 <- ggplot(
  chart_a4_data,
  aes(
    x = .data$academic_year,
    y = .data$starts,
    group = interaction(.data$dimension, .data$series),
    colour = .data$series_order
  )
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_text(
    data = chart_a4_latest,
    aes(y = .data$label_y, label = .data$label),
    hjust = -0.05,
    size = 3.1,
    show.legend = FALSE
  ) +
  facet_wrap(vars(.data$dimension), ncol = 1) +
  scale_colour_manual(values = series_palette) +
  scale_x_discrete(expand = expansion(add = c(0.25, 2.2))) +
  scale_y_continuous(
    labels = scales::label_comma(),
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.08))
  ) +
  coord_cartesian(clip = "off") +
  labs(
    title = "Apprenticeship starts by age and level",
    subtitle = "England, final full academic years; age is measured at the start of the programme",
    x = NULL,
    y = "Starts",
    caption = paste0(
      "Source: DfE Explore Education Statistics, July 2026 release; ",
      "historical apprenticeship series.\n",
      "Note: each panel partitions the same national total. Figures are rounded by DfE."
    )
  ) +
  guides(colour = "none") +
  theme_house(base_size = 11.5) +
  theme(
    strip.text = element_text(face = "bold", colour = colour_text),
    axis.title.y = element_text(face = "bold"),
    plot.caption.position = "plot",
    plot.margin = margin(10, 105, 10, 10)
  )


save_table(
  chart_a4_data %>%
    mutate(
      dimension = as.character(.data$dimension),
      series_order = as.integer(as.character(.data$series_order))
    ),
  "chart_04_starts_by_age_and_level_data.csv"
)
save_table(
  chart_a4_reconciliation,
  "chart_04_starts_by_age_and_level_reconciliation.csv"
)
save_chart(
  chart_a4,
  "chart_04_starts_by_age_and_level.png",
  width = 10.2,
  height = 7.6
)




# --- CHART 12: MEDIAN-BASED QUADRANT -----------------------------------------
# Descriptive comparison only. Relative medians move when the market changes;
# they are not used for the canonical tier classification.


median_plot_data <- thin_market_is8 %>%
  select(
    standard_ref, standard_name, route, level,
    starts, providers, tier
  ) %>%
  mutate(
    quadrant = case_when(
      .data$starts < median_thresholds$median_starts &
        .data$providers < median_thresholds$median_providers ~
        "Low starts + few providers",
      .data$starts < median_thresholds$median_starts ~
        "Low starts + many providers",
      .data$providers < median_thresholds$median_providers ~
        "High starts + few providers",
      TRUE ~ "High starts + many providers"
    )
  ) %>%
  add_count(.data$quadrant, name = "quadrant_standards") %>%
  mutate(
    quadrant_label = paste0(
      .data$quadrant, " (n=", scales::comma(.data$quadrant_standards), ")"
    )
  )


median_quadrant_order <- c(
  "Low starts + few providers",
  "Low starts + many providers",
  "High starts + few providers",
  "High starts + many providers"
)


median_quadrant_labels <- median_plot_data %>%
  distinct(.data$quadrant, .data$quadrant_label) %>%
  arrange(match(.data$quadrant, median_quadrant_order)) %>%
  pull(.data$quadrant_label)


median_palette <- c(
  "Low starts + few providers" = "#CC0000",
  "Low starts + many providers" = "#E8C400",
  "High starts + few providers" = "#8E44AD",
  "High starts + many providers" = "#CCCCCC"
)


chart_annex2_median <- ggplot(
  median_plot_data,
  aes(x = .data$providers, y = .data$starts, colour = .data$quadrant)
) +
  geom_vline(
    xintercept = median_thresholds$median_providers,
    colour = colour_text,
    linetype = "dashed",
    linewidth = 0.6
  ) +
  geom_hline(
    yintercept = median_thresholds$median_starts,
    colour = colour_text,
    linetype = "dashed",
    linewidth = 0.6
  ) +
  geom_point(size = 2.2, alpha = 0.75) +
  annotate(
    "label",
    x = median_thresholds$median_providers,
    y = max(median_plot_data$starts) / 1.7,
    label = paste0(
      "Median providers: ",
      scales::number(median_thresholds$median_providers, accuracy = 0.1)
    ),
    hjust = -0.04,
    size = 3,
    colour = colour_text,
    fill = "white",
    linewidth = 0.2
  ) +
  annotate(
    "label",
    x = max(median_plot_data$providers) / 1.7,
    y = median_thresholds$median_starts,
    label = paste0(
      "Median starts: ",
      scales::number(median_thresholds$median_starts, accuracy = 0.1)
    ),
    vjust = -0.25,
    size = 3,
    colour = colour_text,
    fill = "white",
    linewidth = 0.2
  ) +
  scale_x_log10(
    breaks = c(1, 2, 3, 5, 10, 20, 50, 100, 200),
    labels = scales::label_comma()
  ) +
  scale_y_log10(
    breaks = c(10, 20, 50, 100, 250, 500, 1000, 2500, 5000, 10000),
    labels = scales::label_comma()
  ) +
  scale_colour_manual(
    values = median_palette,
    breaks = median_quadrant_order,
    labels = median_quadrant_labels,
    drop = FALSE
  ) +
  labs(
    title = "Median-based comparison of standard markets",
    subtitle = paste0(
      scales::comma(nrow(median_plot_data)),
      " standards with recorded starts and providers, ", ref_year_label,
      "; logarithmic axes"
    ),
    x = "Providers delivering the standard",
    y = "Apprenticeship starts",
    colour = NULL,
    caption = paste0(
      src_shiny,
      "\nNote: this relative split is descriptive. It is not the six-tier policy classification."
    )
  ) +
  theme_house(base_size = 11.5) +
  theme(
    legend.position = "top",
    legend.box = "vertical",
    legend.text = element_text(size = 9),
    axis.title = element_text(face = "bold"),
    plot.caption.position = "plot"
  )


# This diagnostic is retained in-session for analytical checking, but it is not
# exported as a report chart. It duplicates Chart 9's market view.




# --- CHART 13: THRESHOLD SENSITIVITY -----------------------------------------
# The full two-dimensional grid is more honest than a single starts-threshold
# line: it shows how both analytical choices change the size of the group.


chart_annex2_sensitivity_data <- sens_grid %>%
  mutate(
    starts_label = factor(
      paste0("<", .data$starts_threshold),
      levels = paste0("<", sens_starts_values)
    ),
    providers_label = factor(
      paste0("≤", .data$provider_threshold),
      levels = paste0("≤", sens_providers_values)
    ),
    text_colour = if_else(.data$pct_standards >= 25, "white", colour_text),
    is_primary =
      .data$starts_threshold == tier_thresholds$thin$starts &
      .data$provider_threshold == tier_thresholds$thin$providers
  )


primary_sensitivity_cell <- chart_annex2_sensitivity_data %>%
  filter(.data$is_primary)


if (nrow(primary_sensitivity_cell) != 1L) {
  stop("The primary thin-market threshold is missing from the sensitivity grid.",
       call. = FALSE)
}


chart_annex2_sensitivity <- ggplot(
  chart_annex2_sensitivity_data,
  aes(
    x = .data$starts_label,
    y = .data$providers_label,
    fill = .data$pct_standards
  )
) +
  geom_tile(colour = "white", linewidth = 0.5) +
  geom_tile(
    data = primary_sensitivity_cell,
    fill = NA,
    colour = colour_text,
    linewidth = 1.2
  ) +
  geom_text(
    aes(
      label = paste0(.data$standards, "\n(", .data$pct_standards, "%)"),
      colour = .data$text_colour
    ),
    size = 3.1,
    lineheight = 0.95
  ) +
  scale_fill_gradient(
    low = "#F2F0FF",
    high = colour_primary,
    labels = scales::label_percent(scale = 1),
    name = paste0(
      "Share of ", scales::comma(nrow(thin_market_is8)),
      "\nmarket standards"
    )
  ) +
  scale_colour_identity() +
  labs(
    title = "Sensitivity of the review-group threshold",
    subtitle = paste0(
      "Cell labels show standards and share of the ",
      scales::comma(nrow(thin_market_is8)),
      "-standard market population; outlined cell is the primary <50 starts / ≤3 providers rule"
    ),
    x = "Starts threshold (fewer than)",
    y = "Provider threshold (no more than)",
    caption = paste0(
      src_shiny,
      "\nNote: larger or looser thresholds mechanically identify more standards. ",
      "This figure tests sensitivity; it does not establish a uniquely correct cut-off."
    )
  ) +
  theme_house(base_size = 11.5) +
  theme(
    panel.grid = element_blank(),
    axis.title = element_text(face = "bold"),
    legend.position = "right",
    plot.caption.position = "plot"
  )


# This diagnostic is retained in-session for analytical checking, but it is not
# exported as a report chart. Table 4 carries the sensitivity evidence.




# --- CHART 5: MAXIMUM FUNDING BAND BY MARKET TIER ----------------------------
# The catalogue supplies a maximum public funding band for each standard. This
# is a per-apprentice ceiling, not annual provider revenue. Medians and
# interquartile ranges show whether the typical band differs across tiers
# without pretending that starts convert mechanically into provider income.


chart_a7_coverage <- thin_market_is8 %>%
  mutate(has_funding = is.finite(.data$max_funding) & .data$max_funding > 0) %>%
  group_by(.data$tier) %>%
  summarise(
    market_standards = n(),
    standards_with_funding = sum(.data$has_funding),
    standards_missing_funding = sum(!.data$has_funding),
    pct_with_funding = round(100 * mean(.data$has_funding), 1),
    .groups = "drop"
  )


if (any(chart_a7_coverage$standards_missing_funding > 0)) {
  stop(
    "Chart 5 would silently exclude standards without a positive funding band. ",
    "Inspect chart_05_maximum_funding_coverage.csv before publication.",
    call. = FALSE
  )
}


chart_a7_data <- thin_market_is8 %>%
  filter(is.finite(.data$max_funding), .data$max_funding > 0) %>%
  mutate(
    tier = factor(as.character(.data$tier), levels = rev(tier_levels))
  ) %>%
  group_by(.data$tier) %>%
  summarise(
    standards = n(),
    median_funding = median(.data$max_funding),
    p25_funding = quantile(.data$max_funding, 0.25, names = FALSE),
    p75_funding = quantile(.data$max_funding, 0.75, names = FALSE),
    min_funding = min(.data$max_funding),
    max_funding = max(.data$max_funding),
    .groups = "drop"
  ) %>%
  mutate(
    median_label = scales::label_currency(
      prefix = "£", accuracy = 1, big.mark = ","
    )(.data$median_funding)
  )


if (nrow(chart_a7_data) != length(tier_levels)) {
  stop("Chart 5 is missing funding data for one or more market tiers.",
       call. = FALSE)
}


chart_a7_labels <- chart_a7_data %>%
  transmute(
    tier = as.character(.data$tier),
    label = paste0(.data$tier, " (n=", scales::comma(.data$standards), ")")
  ) %>%
  {
    setNames(.$label, .$tier)
  }


chart_a7 <- ggplot(
  chart_a7_data,
  aes(y = .data$tier, colour = .data$tier)
) +
  geom_segment(
    aes(
      x = .data$p25_funding,
      xend = .data$p75_funding,
      yend = .data$tier
    ),
    linewidth = 3.5,
    lineend = "round"
  ) +
  geom_point(aes(x = .data$median_funding), size = 3.3) +
  geom_text(
    aes(
      x = .data$p75_funding,
      label = .data$median_label
    ),
    hjust = -0.15,
    size = 3.3,
    colour = colour_text,
    show.legend = FALSE
  ) +
  scale_colour_manual(values = tier_palette, drop = FALSE) +
  scale_x_continuous(
    labels = scales::label_currency(prefix = "£", accuracy = 1, big.mark = ","),
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.18))
  ) +
  scale_y_discrete(labels = chart_a7_labels) +
  labs(
    title = "Maximum funding bands by market tier",
    subtitle = paste0(
      "Median and interquartile range, Skills England catalogue as at ",
      skills_england_asat
    ),
    x = "Maximum funding per apprentice",
    y = NULL,
    colour = NULL,
    caption = paste0(
      src_skills_england,
      "\nNote: dots show medians and thick lines the middle 50% of standards. ",
      "Funding bands are per-apprentice ceilings, not annual provider revenue."
    )
  ) +
  guides(colour = "none") +
  theme_house(base_size = 11.5) +
  theme(
    axis.title.x = element_text(face = "bold"),
    plot.caption.position = "plot"
  )


save_table(chart_a7_data, "chart_05_maximum_funding_by_market_tier_data.csv")
save_table(chart_a7_coverage, "chart_05_maximum_funding_coverage.csv")
save_chart(
  chart_a7,
  "chart_05_maximum_funding_by_market_tier.png",
  width = 9.8,
  height = 6.2
)




# --- CHART 10: INCLUSIVE REVIEW-GROUP LINKS TO IS8 PROXY SECTORS -------------
# Count a standard once in every sector to which the proxy plausibly links it.
# This avoids the arbitrary first-match allocation used in the one-to-one
# summary. Sector bars overlap and therefore must not be summed. The additional
# foundational category remains visually separate from the eight core sectors.


review_tier_order <- c("Severe thin market", "Thin market")
chart_a9_scope_order <- c(
  "Core IS8 sectors",
  "Foundational"
)


chart_a9_links <- thin_market_is8 %>%
  filter(
    .data$tier %in% review_tier_order,
    .data$is8_n_sectors_matched > 0,
    !is.na(.data$is8_sectors_matched),
    .data$is8_sectors_matched != ""
  ) %>%
  select(
    standard_ref, standard_name, tier,
    is8_n_sectors_matched, is8_sectors_matched
  ) %>%
  tidyr::separate_rows(is8_sectors_matched, sep = ";\\s*") %>%
  transmute(
    standard_ref,
    standard_name,
    tier = factor(as.character(.data$tier), levels = review_tier_order),
    is8_sector = str_trim(.data$is8_sectors_matched)
  ) %>%
  filter(.data$is8_sector %in% is8_sectors_ordered) %>%
  distinct(.data$standard_ref, .data$is8_sector, .keep_all = TRUE)


if (anyDuplicated(chart_a9_links[c("standard_ref", "is8_sector")]) > 0) {
  stop("Chart 10 counts a standard more than once within the same sector.",
       call. = FALSE)
}


review_group_standards <- thin_market_is8 %>%
  filter(.data$tier %in% review_tier_order) %>%
  summarise(standards = n_distinct(.data$standard_ref)) %>%
  pull(standards)


if (exists("thin_severe", inherits = TRUE) &&
    review_group_standards != n_distinct(thin_severe$standard_ref)) {
  stop("Chart 10 review-group denominator does not reconcile.", call. = FALSE)
}


chart_a9_scope <- tibble(is8_sector = is8_sectors_ordered) %>%
  mutate(
    scope = if_else(
      .data$is8_sector == "Foundational Industries and Inputs",
      "Foundational",
      "Core IS8 sectors"
    )
  )


chart_a9_data <- tidyr::expand_grid(
  is8_sector = is8_sectors_ordered,
  tier = review_tier_order
) %>%
  left_join(chart_a9_scope, by = "is8_sector") %>%
  left_join(
    chart_a9_links %>%
      count(.data$is8_sector, .data$tier, name = "standards") %>%
      mutate(tier = as.character(.data$tier)),
    by = c("is8_sector", "tier")
  ) %>%
  mutate(
    standards = replace_na(.data$standards, 0L),
    tier = factor(.data$tier, levels = review_tier_order),
    scope = factor(.data$scope, levels = chart_a9_scope_order)
  )


if (sum(chart_a9_data$standards) != nrow(chart_a9_links)) {
  stop("Chart 10 aggregated counts do not reconcile to the inclusive links.",
       call. = FALSE)
}


chart_a9_totals <- chart_a9_data %>%
  group_by(.data$is8_sector, .data$scope) %>%
  summarise(total_standards = sum(.data$standards), .groups = "drop")


chart_a9_sector_levels <- chart_a9_totals %>%
  arrange(.data$scope, .data$total_standards, .data$is8_sector) %>%
  pull(.data$is8_sector)


chart_a9_data <- chart_a9_data %>%
  mutate(
    sector_plot = factor(.data$is8_sector, levels = chart_a9_sector_levels)
  )


chart_a9_totals <- chart_a9_totals %>%
  mutate(
    sector_plot = factor(.data$is8_sector, levels = chart_a9_sector_levels),
    label = scales::comma(.data$total_standards)
  )


core_review_total <- chart_a9_links %>%
  filter(.data$is8_sector != "Foundational Industries and Inputs") %>%
  summarise(standards = n_distinct(.data$standard_ref)) %>%
  pull(standards)


expanded_review_total <- n_distinct(chart_a9_links$standard_ref)


review_group_overlap <- chart_a9_links %>%
  count(.data$standard_ref, name = "sectors") %>%
  filter(.data$sectors > 1) %>%
  nrow()

core_review_group_overlap <- chart_a9_links %>%
  filter(.data$is8_sector != "Foundational Industries and Inputs") %>%
  count(.data$standard_ref, name = "sectors") %>%
  filter(.data$sectors > 1) %>%
  nrow()


chart_a9 <- ggplot(
  chart_a9_data,
  aes(
    x = .data$standards,
    y = .data$sector_plot,
    fill = .data$tier
  )
) +
  geom_col(width = 0.68) +
  geom_text(
    data = chart_a9_totals,
    aes(
      x = .data$total_standards,
      y = .data$sector_plot,
      label = .data$label
    ),
    inherit.aes = FALSE,
    hjust = -0.16,
    size = 3.2,
    colour = colour_text
  ) +
  facet_grid(
    rows = vars(scope),
    scales = "free_y",
    space = "free_y"
  ) +
  scale_fill_manual(
    values = tier_palette[review_tier_order],
    breaks = review_tier_order,
    drop = FALSE
  ) +
  scale_x_continuous(
    limits = c(0, NA),
    breaks = scales::breaks_pretty(n = 6),
    expand = expansion(mult = c(0, 0.12))
  ) +
  labs(
    title = paste0(
      "Indicative links between severe and thin apprenticeship markets\n",
      "and Industrial Strategy sectors"
    ),
    subtitle = str_wrap(
      paste0(
        "Number of standards in the ", scales::comma(review_group_standards),
        "-standard review group plausibly linked to each sector, ",
        "using an inclusive route-and-title proxy"
      ),
      width = 105
    ),
    x = "Review-group standards linked to the sector",
    y = NULL,
    fill = NULL,
    caption = paste(
      str_wrap(src_shiny, width = 115),
      str_wrap(src_skills_england, width = 115),
      str_wrap(
        paste0(
          "Note: standards are counted once in every sector to which they are plausibly linked, ",
          "so sector bars overlap and must not be summed. Each standard is counted only once ",
          "within a sector. The mapping is indicative and is not an official sector crosswalk."
        ),
        width = 115
      ),
      str_wrap(
        paste0(
          scales::comma(review_group_overlap), " of ",
          scales::comma(review_group_standards),
          " review-group standards match more than one mapped category, including foundational; ",
          scales::comma(core_review_group_overlap), " match multiple core IS8 sectors. ",
          scales::comma(core_review_total),
          " unique standards link to at least one of the eight core sectors; ",
          scales::comma(expanded_review_total),
          " link to a core sector or the additional foundational category."
        ),
        width = 115
      ),
      sep = "\n"
    )
  ) +
  theme_house(base_size = 11.5) +
  theme(
    legend.position = "top",
    strip.text.y = element_text(face = "bold", colour = colour_text, angle = 0),
    axis.title.x = element_text(face = "bold"),
    plot.caption.position = "plot"
  )


save_table(chart_a9_links, "chart_10_is8_review_group_multilabel_links.csv")
save_table(chart_a9_data, "chart_10_is8_review_group_links_data.csv")
save_chart(
  chart_a9,
  "chart_10_is8_review_group_links.png",
  width = 10.2,
  height = 7.6
)


message(
  paste0(
    "\n09_visualisations.R complete: eleven report charts built and saved, ",
    "including revised funding and IS8 figures."
  )
)
