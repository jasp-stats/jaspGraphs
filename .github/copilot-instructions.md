# jaspGraphs — Copilot Instructions

## Quick summary

- jaspGraphs is an R package of plotting helpers, ggplot2 extensions, and JASP-specific themes used by JASP R modules.
- Primary code lives under `R/`; exported API is declared by roxygen tags and generated `NAMESPACE`/`man/` files.

---

## Big picture (what to know first) 🔧
- Core responsibilities: provide consistent themes (`themeJasp.R`, `themeApa.R`), colour palettes (`colorPalettes.R`, `colors.R`), custom geoms (`geom_*` files, `customGeoms.R`) and high-level plotting helpers (`JASPScatterPlot.R`, `PlotPieChart.R`, `PlotRobustnessSequential.R`).
- Conversion & integration: `convertGgplotToPlotly.R` and `plotly.R` convert ggplot objects for interactive use; keep these aligned when you change plot structure.
- Translations & UI strings: `po/` and `inst/po` where QML and R translations live — update `.pot`/`.po` when adding user-facing text.

---

## Developer workflows & commands ✅
- Iterative dev (R console):
  - Load code: `devtools::load_all()`
  - Run tests: `devtools::test()` or `testthat::test_file("tests/testthat/test-*.R")`
  - Update docs: `devtools::document()` (roxygen → `man/` + `NAMESPACE`)
  - Full pkg check: `devtools::check()` or `R CMD check --as-cran .`
- Build site: `pkgdown::build_site()` (site config in `_pkgdown.yml` and `pkgdown/`)

---

## Project-specific conventions & patterns 📐
- Naming: files and exported functions primarily use CamelCase (e.g., `JASPScatterPlot`, `PlotPieChart`). Follow existing style.
- Exports and docs: Use roxygen comments (`#' @export`, `#' @examples`) in the R source — do not manually edit `NAMESPACE` or `man/` unless necessary.
- Plot testing: tests should inspect the ggplot object structure (class, layers, mapping, scales) using `ggplot2::ggplot_build()` rather than relying on visual snapshots unless a visual tool is already in use.
- Keep themes/colors separate from data logic: prefer reusing `themeJasp()` and `colorPalettes` helpers for consistency.

---

## Integration points & risks ⚠️
- API stability matters: many functions are used by other JASP packages — avoid changing signatures of exported functions without a clear migration plan and updating dependents.
- Plotly conversion: changing layers/aesthetics can break `convertGgplotToPlotly.R` — add/update tests that exercise conversion.
- Internationalization: changes to any user-facing string require updating `po/` translation assets.

---

## Where to look for examples & guidance 📁
- Core plot examples: `R/JASPScatterPlot.R`, `R/PlotPieChart.R`, `R/PlotRobustnessSequential.R`.
- Custom geoms: `R/geom_abline2.R`, `R/geom_aligned_text.R`, `R/geom_rangeframe.R`.
- Themes & palettes: `R/themeJasp.R`, `R/themeApa.R`, `R/colorPalettes.R`, `R/colors.R`.
- Conversion & interactive: `R/convertGgplotToPlotly.R`, `R/plotly.R`.
- Tests: `tests/` (follow existing tests structure when adding new ones).

---

## Small, actionable notes for Copilot agents ✨
- When adding an exported function:
  - Add roxygen docs with `@export` and examples.
  - Run `devtools::document()` and `devtools::check()`; fix any NOTES/WARNINGS.
  - Add unit tests under `tests/testthat/` that validate return types and key plot elements.
- When changing plot structure: update conversion tests for Plotly and any example files under `inst/examples` or `pkgdown/`.
- Avoid changing public function names or argument order without adding deprecation helpers and tests.

---
