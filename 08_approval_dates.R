# ==============================================================================
# 08_approval_dates.R
# When standards were approved, and whether that relates to market thinness
# ==============================================================================
#
# READS:
#   skills_england    (02) full catalogue, one row per standard version
#   thin_market_is8   (04) classified standards
#
# PRODUCES:
#   approval_coverage       parse rate for the approval date field
#   approval_full_catalogue counts before and after the levy, whole catalogue
#   approval_by_tier        approval period against market tier
#   approval_by_is8         approval period against Industrial Strategy mapping
#   approval_summary        the headline comparison
#   live_stock              monthly live-stock series to the catalogue snapshot
#
# DEPENDS ON: 00_config.R, 01_functions.R, 02_load_data.R, 04_source_of_truth.R
#
# ------------------------------------------------------------------------------
# WHAT IS BEING TESTED
#
# Employer-led standard setting began following the Richard Review in 2013.
# The apprenticeship levy took effect in April 2017. The question is whether
# standards approved after the levy are disproportionately concentrated in the
# thinner market tiers, which would support a link between levy-era approval
# and market fragility, as against proliferation being a longer-running
# feature of employer-led design.
#
# This is an association, not a causal test. Approval date and market
# thinness are both related to how recently an occupation was formalised, and
# a newer standard has had less time to build volume. That confound is
# reported alongside the result rather than left implicit.
# ------------------------------------------------------------------------------
#
# DATE BASIS
#
# A standard can have several versions, each with its own approval date. The
# question concerns when the standard first became available, so the earliest
# approved-for-delivery date across all versions is used, not the date of the
# version currently in force. Using the latter would date standards by their
# most recent revision and push the distribution artificially late.
# ==============================================================================

library(tidyverse)
library(lubridate)

source("00_config.R")
source("01_functions.R")

levy_start <- as.Date("2017-04-06")

# The catalogue header states that the extract is accurate as of 1 June 2026.
# Do not extend catalogue-based series beyond that date: doing so would create
# artificial flat months after the last date the source can support.
catalogue_snapshot_date <- as.Date("2026-06-01")


# --- SECTION 1: PARSE DATES ---------------------------------------------------
# Catalogue dates are dd/mm/yyyy. Blank where the milestone has not occurred:
# a live standard has no retired or withdrawn date, which is meaningful
# absence rather than missing data.

parse_cat_date <- function(x) as.Date(x, format = "%d/%m/%Y")

catalogue_dates <- skills_england %>%
  filter(
    .data$programme_type == "Apprenticeship standard",
    !is.na(.data$standard_ref)
  ) %>%
  mutate(
    approved_date  = parse_cat_date(.data$approved_for_delivery_date),
    retired_date_d = parse_cat_date(.data$retired_date),
    withdrawn_date_d = parse_cat_date(.data$withdrawn_date)
  ) %>%
  group_by(.data$standard_ref) %>%
  summarise(
    # Earliest approval across versions: when the standard first existed.
    first_approved   = suppressWarnings(min(.data$approved_date, na.rm = TRUE)),
    versions         = n(),
    any_approval_date = any(!is.na(.data$approved_date)),
    retired_date     = suppressWarnings(min(.data$retired_date_d, na.rm = TRUE)),
    withdrawn_date   = suppressWarnings(min(.data$withdrawn_date_d, na.rm = TRUE)),
    status           = first(.data$status),
    .groups = "drop"
  ) %>%
  mutate(
    across(c(first_approved, retired_date, withdrawn_date), ~ if_else(is.infinite(.x), NA_Date_, .x)),
    first_approved = if_else(.data$any_approval_date, .data$first_approved, NA_Date_),
    approval_period = case_when(
      is.na(.data$first_approved)          ~ "Unknown",
      .data$first_approved <  levy_start   ~ "Pre-levy (before Apr 2017)",
      TRUE                                 ~ "Post-levy (Apr 2017 onwards)"
    ),
    is_retired   = !is.na(.data$retired_date),
    is_withdrawn = !is.na(.data$withdrawn_date),
    is_live      = !.data$is_retired & !.data$is_withdrawn
  )

assert_unique_key(catalogue_dates, "standard_ref", "catalogue_dates")


# --- SECTION 2: COVERAGE OF THE DATE FIELD ------------------------------------
# Reported before any analysis uses it. Standards with no parseable approval
# date are carried through as "Unknown" rather than dropped, so the reader can
# see how much of the catalogue the finding rests on.

approval_coverage <- catalogue_dates %>%
  summarise(
    standards          = n(),
    with_approval_date = sum(!is.na(.data$first_approved)),
    no_approval_date   = sum(is.na(.data$first_approved)),
    retired            = sum(.data$is_retired),
    withdrawn          = sum(.data$is_withdrawn),
    live               = sum(.data$is_live)
  ) %>%
  mutate(pct_dated = round(100 * .data$with_approval_date / .data$standards, 1))

print(approval_coverage)

if (approval_coverage$pct_dated < 80) {
  warning(
    "Only ", approval_coverage$pct_dated, "% of standards carry a parseable ",
    "approval date. Treat the split below as indicative.",
    call. = FALSE
  )
}


# --- SECTION 3: FULL CATALOGUE SPLIT ------------------------------------------
# Two bases, reported side by side:
#   all       every apprenticeship standard in the catalogue
#   live      excluding those with a retired or withdrawn date

approval_full_catalogue <- bind_rows(
  catalogue_dates %>%
    count(.data$approval_period, name = "standards") %>%
    mutate(basis = "All standards"),
  catalogue_dates %>%
    filter(.data$is_live) %>%
    count(.data$approval_period, name = "standards") %>%
    mutate(basis = "Live only (excludes retired and withdrawn)")
) %>%
  group_by(.data$basis) %>%
  mutate(pct = round(100 * .data$standards / sum(.data$standards), 1)) %>%
  ungroup() %>%
  select(basis, approval_period, standards, pct) %>%
  arrange(.data$basis, .data$approval_period)

print(approval_full_catalogue)


# --- SECTION 4: APPROVAL PERIOD AGAINST MARKET TIER ---------------------------
# Restricted to the market-tier population, which contains only standards with
# recorded starts and provider counts. Approved standards with no recorded
# starts are held separately in 04_source_of_truth.R.

analysis_dates <- thin_market_is8 %>%
  select(standard_ref, standard_name, tier, starts, providers, is8_flag) %>%
  left_join(
    select(catalogue_dates, standard_ref, first_approved, approval_period,
           is_retired, is_withdrawn, is_live),
    by = "standard_ref"
  )

report_match(analysis_dates, "first_approved", "classified standards to approval dates")

observable <- analysis_dates

approval_by_tier <- observable %>%
  count(.data$tier, .data$approval_period, name = "standards") %>%
  group_by(.data$tier) %>%
  mutate(
    tier_total  = sum(.data$standards),
    pct_of_tier = round(100 * .data$standards / .data$tier_total, 1)
  ) %>%
  ungroup() %>%
  arrange(.data$tier, .data$approval_period)

print(approval_by_tier, n = Inf)


# --- SECTION 5: THE HEADLINE COMPARISON ---------------------------------------
# Review group (severe thin + thin) against scaled markets. Percentages are
# calculated on standards with a known approval date, so that an unknown does
# not silently count as pre-levy.

review_tiers <- c("Severe thin market", "Thin market")

group_share <- function(df, label) {
  dated <- df %>% filter(.data$approval_period != "Unknown")
  tibble(
    group          = label,
    standards      = nrow(df),
    with_date      = nrow(dated),
    post_levy      = sum(dated$approval_period == "Post-levy (Apr 2017 onwards)"),
    pct_post_levy  = round(100 * .data$post_levy / .data$with_date, 1)
  )
}

approval_summary <- bind_rows(
  group_share(filter(observable, .data$tier %in% review_tiers), "Review group (severe thin + thin)"),
  group_share(filter(observable, .data$tier == "Scaled market"), "Scaled market"),
  group_share(filter(observable, .data$tier == "Potential thin market"), "Potential thin market"),
  group_share(filter(observable, .data$tier == "Low-volume review group"), "Low-volume review group"),
  group_share(filter(observable, .data$tier == "Provider dependency market"), "Provider dependency market"),
  group_share(observable, "All observable standards")
) %>%
  mutate(basis = "Observable standards, all statuses")

# Same comparison excluding retired and withdrawn standards.
observable_live <- observable %>% filter(.data$is_live | is.na(.data$is_live))

approval_summary_live <- bind_rows(
  group_share(filter(observable_live, .data$tier %in% review_tiers), "Review group (severe thin + thin)"),
  group_share(filter(observable_live, .data$tier == "Scaled market"), "Scaled market"),
  group_share(observable_live, "All observable standards")
) %>%
  mutate(basis = "Live only (excludes retired and withdrawn)")

approval_summary <- bind_rows(approval_summary, approval_summary_live) %>%
  select(basis, group, standards, with_date, post_levy, pct_post_levy)

print(approval_summary, n = Inf)


# --- SECTION 6: APPROVAL PERIOD AGAINST INDUSTRIAL STRATEGY MAPPING -----------

approval_by_is8 <- observable %>%
  filter(.data$approval_period != "Unknown") %>%
  count(.data$is8_flag, .data$approval_period, name = "standards") %>%
  group_by(.data$is8_flag) %>%
  mutate(pct = round(100 * .data$standards / sum(.data$standards), 1)) %>%
  ungroup() %>%
  mutate(is8_flag = if_else(.data$is8_flag, "Mapped to an IS8 sector", "Not mapped"))

print(approval_by_is8)


# --- SECTION 7: VERIFICATION --------------------------------------------------
# Each figure is recomputed by an independent route and compared. A mismatch
# means one of the two paths is wrong, and the script says so rather than
# writing an unchecked number to file.

check_fail <- 0L

verify <- function(label, a, b, tol = 0.05) {
  ok <- isTRUE(all.equal(as.numeric(a), as.numeric(b), tolerance = tol))
  message(sprintf("  %-52s %s", label, if (ok) "OK" else "MISMATCH"))
  if (!ok) {
    message("      route 1: ", paste(a, collapse = ", "))
    message("      route 2: ", paste(b, collapse = ", "))
    check_fail <<- check_fail + 1L
  }
  invisible(ok)
}

message("\n--- Verification pass 1: totals reconcile ---")

verify(
  "Catalogue rows equal sum of approval periods",
  nrow(catalogue_dates),
  sum(filter(approval_full_catalogue, .data$basis == "All standards")$standards)
)

verify(
  "Observable standards equal sum across tiers",
  nrow(observable),
  sum(approval_by_tier$standards)
)

verify(
  "Review group size matches the tier table",
  nrow(filter(observable, .data$tier %in% review_tiers)),
  sum(filter(approval_by_tier, .data$tier %in% review_tiers)$standards)
)

message("\n--- Verification pass 2: percentages recomputed independently ---")

review_direct <- observable %>%
  filter(.data$tier %in% review_tiers, .data$approval_period != "Unknown")

verify(
  "Review group post-levy share",
  filter(approval_summary,
         .data$group == "Review group (severe thin + thin)",
         .data$basis == "Observable standards, all statuses")$pct_post_levy,
  round(100 * mean(review_direct$first_approved >= levy_start), 1)
)

scaled_direct <- observable %>%
  filter(.data$tier == "Scaled market", .data$approval_period != "Unknown")

verify(
  "Scaled market post-levy share",
  filter(approval_summary,
         .data$group == "Scaled market",
         .data$basis == "Observable standards, all statuses")$pct_post_levy,
  round(100 * mean(scaled_direct$first_approved >= levy_start), 1)
)

verify(
  "No standard is both pre-levy and post-levy",
  sum(catalogue_dates$approval_period %in%
        c("Pre-levy (before Apr 2017)", "Post-levy (Apr 2017 onwards)")),
  sum(!is.na(catalogue_dates$first_approved))
)

if (check_fail > 0) {
  warning(check_fail, " verification check(s) failed. Do not use these figures.",
          call. = FALSE)
} else {
  message("\n  All verification checks passed.")
}


# --- SECTION 8: THE CONFOUND --------------------------------------------------
# Median approval date by tier. If thin-tier standards are simply younger,
# lower volume is partly a function of time in market rather than of the
# approval regime, and the association should be read accordingly.

approval_age_by_tier <- observable %>%
  filter(!is.na(.data$first_approved)) %>%
  group_by(.data$tier) %>%
  summarise(
    standards           = n(),
    median_approved     = median(.data$first_approved),
    earliest            = min(.data$first_approved),
    latest              = max(.data$first_approved),
    median_years_live   = round(
      as.numeric(catalogue_snapshot_date - median(.data$first_approved)) / 365.25,
      1
    ),
    .groups = "drop"
  )

print(approval_age_by_tier)


# --- SECTION 9: LIVE STOCK OF STANDARDS OVER TIME -----------------------------
# The stock of standards available at each point in time, rather than the
# cumulative count of approvals. A standard enters the stock when it is
# approved for delivery and leaves when it is retired or withdrawn, so the
# series shows what the system actually offered at any given date.
#
# Cumulative approvals would only ever rise, and would overstate the size of
# the live catalogue by counting standards that have since been removed.

stock_dates <- catalogue_dates %>%
  filter(!is.na(.data$first_approved)) %>%
  mutate(
    exit_date = pmin(.data$retired_date, .data$withdrawn_date, na.rm = TRUE)
  )

timeline <- seq(
  from = floor_date(min(stock_dates$first_approved), "year"),
  to   = floor_date(catalogue_snapshot_date, "month"),
  by   = "month"
)

live_stock <- tibble(date = timeline) %>%
  mutate(
    live = map_int(
      .data$date,
      ~ sum(
        stock_dates$first_approved <= .x &
          (is.na(stock_dates$exit_date) | stock_dates$exit_date > .x)
      )
    ),
    approved_to_date = map_int(
      .data$date,
      ~ sum(stock_dates$first_approved <= .x)
    )
  ) %>%
  mutate(exited = .data$approved_to_date - .data$live)

message("\n  Live stock at latest date: ", tail(live_stock$live, 1),
        " (", tail(live_stock$approved_to_date, 1), " ever approved, ",
        tail(live_stock$exited, 1), " since removed)")

peak <- live_stock %>% slice_max(.data$live, n = 1, with_ties = FALSE)
message("  Peak live stock: ", peak$live, " in ", format(peak$date, "%B %Y"))


# --- SECTION 10: SAVE ---------------------------------------------------------

save_table(approval_coverage,       "approval_coverage.csv")
save_table(approval_full_catalogue, "approval_full_catalogue.csv")
save_table(approval_by_tier,        "approval_by_tier.csv")
save_table(approval_summary,        "approval_summary.csv")
save_table(approval_by_is8,         "approval_by_is8.csv")
save_table(approval_age_by_tier,    "approval_age_by_tier.csv")
save_table(live_stock,              "chart_03_live_stock_of_standards_data.csv")

message("\n08_approval_dates.R complete.")
