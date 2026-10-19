# Pressure-cooker

Analysis for Het Epos (Behavioral Data Science Toolbox, group 4): how time in kindergarten, learning goals and
extracurricular activities in groups 1–2 relate to results in group 3 and later.

## Setup

1. Install [R](https://cran.r-project.org/) (4.6) and [Quarto](https://quarto.org/).
2. Restore the package environment from the project folder:

   ```r
   renv::restore()
   ```

3. Put the Het Epos Excel exports in `data/` (this folder is gitignored; never commit pupil data).

## Run

```bash
quarto render analysis.qmd
```

This produces `analysis.html` with all tables and plots.

## Report

The report is a slide deck (revealjs) for the school board. All slide text is written in `report.qmd`:
edit it there. Each slide title states one finding, so the titles alone tell the story. Numbers are
inline R (`num$...`, computed in `report/numbers.R`) so they stay correct when the data changes; the
figures are in `report/plots.R` and the look of the course template is in `report/theme.scss`.

```bash
quarto render report.qmd
```

This writes `report.html`, a single file that opens in any browser. Press `s` for the speaker notes,
which hold the notebook's full conclusion behind each slide.

- **PDF.** Open `report.html` in Chrome and press `e` (or add `?view=print` to the address), then
  print to PDF with no margins and background graphics on.
- **Without data.** Set `use_analysis <- FALSE` in the first chunk of `report.qmd` to render in seconds;
  numbers show as "…" and figures as placeholders.
- **Font.** The slides use Frutiger Next Pro where it is installed and Open Sans otherwise.
- **Outputs are not committed:** `report.html` and `report.pdf` contain results (see `.gitignore`).

## Layout

| Path | Contents |
| --- | --- |
| `R/setup.R` | Packages and shared helpers |
| `R/load_data.R` | Reads the Excel files from `data/` |
| `R/cleanup.R` | Builds pupil-level tables (learning goals, CITO, interventions, CLASS, time in kindergarten) |
| `R/q1_model.R`, `R/q1_robustness.R` | First version of Q1 (kindergarten time × extracurricular activities) and its checks; not used in the notebook |
| `R/q2_analysis.R` | First version of Q2 (learning goals vs CITO); not used in the notebook, chapter 5 reuses its model in `R/cv_models.R` |
| `analysis.qmd` | Notebook that runs the scripts in order and shows the results |
| `report.qmd`, `report/` | The report slides: storyline, figures and theme |
