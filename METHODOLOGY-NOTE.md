# Methodology note

**Apprenticeship thin markets in England — analytical approach**

*Final analytical method. Last updated 12 August 2026.*

This note explains how the analysis in this repository works: where the data comes from, how key terms are defined, the choices made in constructing the measures, and the limitations a reader should keep in mind. It assumes no prior familiarity with apprenticeship statistics.

---

## 1. What this analysis is trying to find out

England has around 675 apprenticeship standards approved for delivery. They vary enormously in size: the largest attracts more than 12,000 starts a year, while many attract only a handful, and some attract none at all.

A standard with very few starts, delivered by very few training providers, is fragile. If one provider withdraws, the standard may become effectively unavailable — even though it remains formally approved, and even though employers may still need those skills. We refer to this as a **thin market**.

This analysis identifies which standards are in thin markets, where they are concentrated by subject and by place, and how exposed the system is if provision withdraws.

---

## 2. Data sources

All figures are for **England only**. Apprenticeships are a devolved policy; Scotland, Wales and Northern Ireland run separate programmes and are not included.

### 2.1 Explore Education Statistics (EES)

The Department for Education's official apprenticeship statistics, published quarterly on [Explore Education Statistics](https://explore-education-statistics.service.gov.uk/find-statistics/apprenticeships). This analysis uses the release published **16 July 2026**.

The underlying source is the **Individualised Learner Record (ILR)**, a monthly return that every publicly funded training provider in England must submit. It is collected primarily for funding purposes rather than for statistics, which shapes some of its limitations — see section 6.

Four releases appear each year, each **cumulative from the start of the academic year**, not quarterly increments:

| Snapshot | Published | Period covered |
|---|---|---|
| SN04 | January | 1 August to 31 October |
| SN06 | March | 1 August to 31 January |
| SN10 | July | 1 August to 30 April |
| **SN14** | **November** | **1 August to 31 July (final)** |

Only the November release is final. Everything else is provisional and incomplete — at the March snapshot, only around 75% of January's starts had been reported.

The academic year runs 1 August to 31 July.

### 2.2 Provider dashboard

The DfE also publishes an [apprenticeships provider dashboard](https://department-for-education.shinyapps.io/apprenticeships-provider-dashboard/) alongside the main release. Unlike the standard downloads, it allows the user to export the exact selection they are viewing.

This matters because **dashboard exports are unrounded and carry no small-number suppression**. Where the main EES download shows `low` for a standard with three starts, the dashboard shows `3`. For an analysis specifically concerned with very small standards, that difference is decisive.

The exports used here cover the **2024/25 academic year** and reconcile to the published national total of 353,500 starts.

### 2.3 Unsuppressed underlying starts archives

The release's supporting files contain unsuppressed row-level starts. The
current archive provides 2020/21 onwards and the 2019/20 archive provides
2017/18 to 2019/20. These are aggregated by academic year and standard
reference to produce exact historical activity, low-start and concentration
series through the final 2024/25 year. The 2024/25 aggregate reconciles to 644
standards with starts, 241 below 50 starts and 353,463 standard-based starts.

### 2.4 Skills England apprenticeship catalogue

The [Skills England apprenticeship finder](https://skillsengland.education.gov.uk/apprenticeships/) lists every apprenticeship standard with its reference code, status, level, route, typical duration and maximum funding band. It is used here to establish which standards exist, which are open to new starts, and what each is worth in funding terms.

Important: the catalogue includes standards that are **retired, withdrawn, paused, or still in development** alongside those currently available. A standard with no starts because it was retired three years ago is a different finding from a live standard nobody is using, so status is always applied before drawing conclusions.

### 2.5 Apprenticeship Provider and Assessment Register (APAR)

A register of providers permitted to deliver apprenticeships. Used to identify provider type and registration status. Note this is a register, not activity data — appearing on APAR does not imply any starts.

### 2.6 Geographic boundaries

Local Skills Improvement Plan (LSIP) areas, local authority districts and regions, from the ONS Open Geography Portal, plus the ONS lookup from local authority district to combined authority.

---

## 3. Key definitions

Apprenticeship statistics use several terms that sound interchangeable but are not.

### 3.1 Starts, participation, achievements and enrolments

| Term | What it counts | Can one person appear more than once? |
|---|---|---|
| **Start** | Apprenticeship *programme* starts. Counted once per apprenticeship per provider. Changing programme, level or pathway mid-year counts as a new start | **Yes** |
| **Participation** | *Learners* in learning at any point in the year. Counted once per level, and once only in any total | No, not within a total |
| **Achievement** | Successfully completed apprenticeships. A learner completing two apprenticeships is counted twice | **Yes** |
| **Enrolment** | Apprenticeship enrolments, counted per learning aim | **Yes** |

This analysis uses **starts** as its primary measure. Starts can be summed across subgroups; participation cannot, because national participation totals de-duplicate learners across levels.

Achievements are recorded at the date of the **end-point assessment**, which may fall in a different academic year from when learning finished. Dividing achievements by starts therefore does not give a completion rate — the numerator and denominator describe different cohorts.

### 3.2 Standards and frameworks

**Frameworks** were the older, qualification-led model. They were withdrawn to new learners on 31 July 2020, and the small number of framework starts recorded since then are learners resuming after an extended break. In 2024/25 there were 40 framework starts, against 353,463 on standards.

**Standards** are employer-designed and occupation-focused, with a mandatory independent end-point assessment. This analysis concerns standards.

### 3.3 Apprenticeship levels

| Label | Qualification level | Broadly equivalent to |
|---|---|---|
| Intermediate | 2 | GCSE |
| Advanced | 3 | A level |
| Higher | 4, 5, 6 and 7 | HNC and above |
| Degree | 6 and 7 | Bachelor's or master's degree |

Degree apprenticeships are a **subset of higher**, not a separate category; adding the two together double-counts.

### 3.4 Sector subject area

Standards are grouped into 13 sector subject areas (SSA tier 1). In December 2025 the SSA codes for 139 standards were revised to align with Ofqual guidance, and DfE recalculated the previous five years on the new basis.

Data published from January 2026 onwards uses the revised mapping. **Earlier downloads are not comparable at subject level** — for 2024/25 the difference reaches 8,610 starts on a single subject area. This analysis uses the July 2026 release throughout.

One area was also renamed: *Information and Communication Technology* became *Digital Technology*.

### 3.5 Identifying standards across sources

Two sources have to be linked for this analysis: the starts data, which says how much activity each standard attracted, and the Skills England catalogue, which supplies its route, level, funding band and status.

**The starts export identifies standards by name only.** It carries no reference code, so the link has to be made on the standard's title, normalised for case and spacing. Most match exactly. A handful do not, because the two sources spell the same standard differently:

| In the starts data | In the catalogue | Why they differ |
|---|---|---|
| Dental Nurse (GDC 2023) | Dental nurse | The starts export appends the General Dental Council revision year; the catalogue does not |
| Orthodontic Therapist (GDC 2023) | Orthodontic therapist | As above |
| Ecologist (Degree) | Ecologist (integrated degree) | Different shorthand for an integrated degree |
| Teacher | Teacher - Postgraduate | The catalogue has since split teaching into postgraduate and undergraduate routes; the starts data still reports a single unsuffixed standard |
| Senior Financial Services Customer Adviser | Financial services customer adviser | The catalogue lists two references under one name, distinguished by level |

**These are resolved with an explicit alias table** rather than left unmatched. The alternative — dropping anything that fails to match — was rejected because the failures are not random. They are concentrated in standards that have been recently revised or renamed, and two of them are substantial: Dental Nurse at 2,141 starts and Teacher at 2,065. Excluding roughly 4,500 starts and five standards on a formatting difference would misstate both the size of the market and the denominator for every share expressed as a proportion of standards.

The aliases are recorded with the reasoning for each, and two carry residual uncertainty worth stating plainly:

- **Teacher.** The single reported standard is mapped to the postgraduate route, on the basis that it is the established one and the undergraduate standard is newly approved at version 1. If teaching starts in the year were in fact split across both routes, the whole 2,065 sits on one reference. This affects the attributes joined to those starts — route, level, funding — but not the volume itself.
- **Senior Financial Services Customer Adviser.** Mapped to the senior of two references listed under the same catalogue name. At 248 starts the exposure is small.

Frameworks are deliberately left unmatched. They have no standard reference by definition, and account for 40 starts in total.

**The wider point.** Name-based linking is inherently fragile, because names drift as standards are revised. The alias table is a practical fix, not a structural one. Any future release should be checked against the unmatched list before results are relied on; that list is written out with every run.

### 3.6 STEM

DfE defines STEM apprenticeships as those in four sector subject areas: Construction, Planning and the Built Environment; Digital Technology; Engineering and Manufacturing Technologies; and Science and Mathematics. Where a DfE-supplied `stem` column is available it is used directly.

---

## 4. Suppression and rounding

### 4.1 What DfE publishes

EES applies two protections to published figures:

- **Rounding.** Counts are rounded to the nearest 10. Rounded components may not
  sum exactly to a rounded total. DfE percentages calculated on unrounded data
  may not be reproducible from displayed counts.
- **Suppression.** Raw values of 0 to 4 are replaced by `low`. A `low` cell
  therefore does not establish positive activity. Other tokens include `c`
  (suppressed), `x` (unavailable) and `z` (not applicable).

Suppression is applied per cell, not per row.

### 4.2 How this analysis handles it

Suppressed cells are retained with an explicit flag and are never silently
coerced to zero, missing or a positive count. The treatment depends on the
question:

- Charts 2, 6 and 8 use the unsuppressed underlying archives, so their
  historical activity, low-start and concentration values are exact.
- Exact 2024/25 market tiers use the unsuppressed provider dashboard.
- Published table-tool files remain suppression-aware wherever they are used
  for other cuts of the data.

### 4.3 Suppression rates vary widely

The proportion of suppressed cells differs greatly between files, and is high enough in some to affect interpretation materially:

| File | Measure | Suppression rate |
|---|---|---|
| Geography detailed | starts | **78.6%** |
| Routes and standards | starts | **51.2%** |
| Provider starts | starts | 25.1% |
| Historical summary | starts | 21.0% |
| Learner detailed | starts | 7.4% |
| Geography population | starts | 0.9% |

The pattern is driven by granularity: the finer the cross-tabulation, the smaller each cell, and the more cells fall below the publication threshold.

**Where suppression exceeds half of all cells, totals derived from that file should be treated as lower bounds, not as counts.** In the geography detailed file, roughly four in five cells are suppressed, so any sum of starts by region and subject captures only a minority of activity. Counts of *standards* remain reliable in these files because suppressed observations are retained; counts of *starts* do not.

This is the main reason the analysis draws on the provider dashboard exports for standard-level work: they carry no suppression at all, so both the count of standards and the volume of starts are complete.

---

## 5. Constructing the thin-market classification

### 5.1 The two axes

Each standard is placed on two dimensions:

- **Annual starts** — how much demand there is
- **Provider count** — how many distinct training providers delivered it

Both come from the same provider dashboard export, so both are measured on the same population, in the same year, on the same basis. Taking the two axes from different sources risks comparing a current measure against a lagged one.

### 5.2 The tiers

| Tier | Definition |
|---|---|
| **No starts recorded** | Approved for delivery, but no starts in the year |
| **Severe thin market** | Fewer than 20 starts and 2 or fewer providers |
| **Thin market** | Fewer than 50 starts and 3 or fewer providers |
| **Potential thin market** | Fewer than 100 starts and 5 or fewer providers |
| **Low-volume review group** | Fewer than 250 starts, but more widely delivered |
| **Provider dependency market** | 250 or more starts but 3 or fewer providers |
| **Scaled market** | Everything else |

Tiers are evaluated in order, so each standard falls into the first tier it qualifies for.

Two design choices are worth noting. First, thresholds are **fixed rather than derived from the median**, so a tier means the same thing from one year to the next; a median-based cut would shift as the distribution changed, making the series hard to interpret. Second, the **provider dependency** tier captures a different risk: standards with healthy demand but very concentrated supply, where a single withdrawal would be disruptive despite the volume.

Sensitivity to both thresholds is tested and reported.

### 5.3 Industrial Strategy proxy sectors

Apprenticeship standards are not officially coded to the Government's eight
Industrial Strategy sectors. The analysis therefore uses an indicative proxy
based on the Skills England route and keywords in the standard title.

The final report view is inclusive and multi-label. A standard is counted once
in every sector to which it is plausibly linked. This avoids an arbitrary
first-match allocation when terms overlap. Chart 10 and Map 2
use the same standard-sector links and deduplicate within sector and location.
Sector bars and panels overlap and cannot be summed. Aggregate maps count each
standard-location once regardless of how many sectors match.

The severe-plus-thin review group contains 139 unique standards linked to at
least one core sector, 165 linked to a core sector or the additional
foundational category, 192 distinct standard-sector links and 25 standards
linked to more than one sector. These are reproducible outputs of the proxy
rules, not official sector-membership facts.

A first-match field is retained only for audit outputs that require one row per
standard. It is not the basis for the report's sector chart or panels.

### 5.4 Standards with no starts

The Skills England current offer contains 71 standards with no recorded
2024/25 starts. They are reported separately from the six market tiers because
there is no observed provider depth or geographic delivery pattern on which to
classify them. They are part of the 715-standard analytical universe but not the
644-standard market-tier population.

---

## 5.5 Measuring geographic spread---

## 5.5 Measuring geographic spread

A standard's geographic footprint is measured **directly**: the underlying starts file records starts by standard and by local authority district, so the areas in which a standard was delivered are observed rather than inferred.

An alternative approach, used in earlier versions of this analysis, distributes each provider's total starts across the areas that provider operates in. That method systematically overstates spread. A standard delivered by a single national provider is credited to every area that provider works in, whether or not the standard was taught there. The difference is large: on the direct measure, the most fragile standards are delivered in a median of one Local Skills Improvement Plan area.

**Spread is reported on two geographies**, because a share expressed as "present in more than a quarter of areas" depends entirely on how many areas exist. England has around 300 local authority districts and around 40 LSIP areas, so the same standard clears the district test far less easily. Districts are assigned to LSIP areas by locating each district's centroid within an LSIP boundary; all 296 districts match, across 42 LSIP areas.

## 5.6 Approval dates

The catalogue records an approval date per standard version. Because a standard may be revised several times, the **earliest** approval date across versions is used: the question is when a standard first became available, and dating it by its most recent revision would push the distribution artificially late.

Approval dates are parseable for 89.3% of standards. Those without a date are reported separately and excluded from percentages rather than being treated as early approvals.

Two bases are reported: all standards, and live standards only, excluding those with a retired or withdrawn date.

Any association between approval date and market thinness should be read as association, not causation. Approval date and market size are both related to how recently an occupation was formalised, and a newer standard has had less time to build volume. That confound is testable and is reported: median approval dates by tier differ by only around 1.4 years, which is too small to account for the observed differences in post-levy share.

## 6. Limitations

**Provisional data.** Figures for the current academic year cover August to April and are incomplete. Only the November release is final.

**Suppression.** See section 4.3. Totals from heavily suppressed files are lower bounds.

**Suppression conceals zeros as well as small counts in table-tool files.** In
the published subject-and-standards table, every standard appears in every year,
and `low` may represent a raw value from zero to four. Those files are not used
for Charts 2, 6 or 8. The unsuppressed underlying archives identify the exact
historical activity population and exact start counts.

**Rounding.** EES counts are rounded to the nearest 10, so small figures carry meaningful proportional uncertainty. A published value of 20 could be anywhere from 15 to 24.

**Provider counts measure delivery, not capacity.** A provider is counted if it recorded starts on a standard in the year. A provider approved and willing to deliver but with no starts does not appear, so the count reflects realised provision rather than available supply.

**Geographic coverage.** The EES file carrying subject detail alongside region and local authority is a single-year file covering only the current release year. Subject-level geographic analysis for earlier years must be derived from the underlying starts file, which carries the full series at maximum granularity.

**Geography is recorded two ways.** Both delivery postcode and learner home postcode are available, and they give different answers. Any geographic figure should state which is used.

**Standards data is as at publication.** Standard names, routes and mandatory-degree flags reflect the Skills England catalogue at the point of publication, not as at the date each apprenticeship started.

**Population rates were rebased.** Population estimates in the July 2026 release were rebased to the 2024 Census and all historic rates restated, so rates per 100,000 differ from those in earlier publications.

**Comparability over time.** Several breaks affect the series: the method for dating achievements on standards changed in 2019/20; 2020/21 counted returners from COVID-related breaks as continuing rather than as new starts; provider type categories were realigned in November 2025; and level 7 funding was restricted from January 2026.

---

## 7. Reproducing the analysis

Scripts run in numeric order. Each is self-documenting at the top with what it reads and what it produces.

| Script | Purpose |
|---|---|
| `00_config.R` | File paths, reference years, thresholds, subject groupings, plot constants |
| `01_functions.R` | Shared helpers, including suppression-aware file reading |
| `02_load_data.R` | Reads and standardises every source; validates against published totals |

The production pipeline is complete: `00_config.R` to `10_maps.R`, with `99_run_all.R` running the sequence and verifying expected outputs.

Every parameter a future refresh might need to change — reference year, thresholds, file paths — is in `00_config.R`. Nothing else hardcodes a year.

---

## 8. Sources

- [Apprenticeships, academic year 2025/26](https://explore-education-statistics.service.gov.uk/find-statistics/apprenticeships/2025-26) — Department for Education, published 16 July 2026
- [Further education and skills statistics: methodology](https://explore-education-statistics.service.gov.uk/methodology/further-education-and-skills-statistics-methodology) — Department for Education
- [Skills England: Search the Apprenticeships](https://skillsengland.education.gov.uk/apprenticeships/)
- [Apprenticeships provider dashboard](https://department-for-education.shinyapps.io/apprenticeships-provider-dashboard/) — Department for Education
- ONS Open Geography Portal — boundary and lookup files

All DfE data is published under the Open Government Licence v3.0.
