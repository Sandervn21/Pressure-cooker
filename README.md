# Pressure-cooker

Analysis for Het Epos (Behavioral Data Science Toolbox, group 4): how time in kindergarten, learning goals and
extracurricular activities in groups 1–2 relate to results in group 3 and later.

## Quickstart

Clone the repository and run one command from its folder:

```bash
./quickstart.sh
```

On Windows, double-click `quickstart.bat` instead. The script checks that R and Quarto are installed,
installs the exact package versions from `renv.lock`, and checks that the Het Epos exports are in
`data/`. If something is missing, it says what to install or which files to put where, and waits.
Then it renders `analysis.html` and `report.html` and opens them in the browser. The first run takes
a while because of the package installation.

## Setup by hand

1. Install [R](https://cran.r-project.org/) (4.6) and [Quarto](https://quarto.org/).
2. Restore the package environment from the project folder:

   ```r
   renv::restore()
   ```

3. Put the Het Epos Excel exports in `data/` (this folder is gitignored; never commit pupil data).

## Run

```bash
quarto render
```

This builds both documents: `analysis.html` with all tables and plots, and the report `report.html`.
To build one, name it, e.g. `quarto render analysis.qmd`. In RStudio, open `Pressure-cooker.Rproj` first.

## Report

The report is a slide deck (revealjs). All slide text is written in `report.qmd`.
Each slide title states one finding, so the titles alone tell the story. Numbers are
inline R (`num$...`, computed in `report/numbers.R`) so they stay correct when the data changes; the
figures are in `report/plots.R` and the look of the course template is in `report/theme.scss`.

```bash
quarto render report.qmd
```

This writes `report.html`, a single file that opens in any browser. Press `s` for the speaker notes,
which hold the notebook's full conclusion behind each slide.

- **PDF.** Open `report.html` in Chrome and press `e` (or add `?view=print` to the address), then
  print to PDF with no margins and background graphics on.
- **Outputs are not committed:** `report.html` and `report.pdf` contain results (see `.gitignore`).

## Layout

```text
analysis.qmd    notebook with the full analysis
report.qmd      report slides for the school
R/              analysis scripts, run in order by analysis.qmd
report/         numbers, figures and theme of the slides
data/           the Het Epos exports (not committed)
quickstart.*    one-command setup and render, checked by CI on Linux, macOS and Windows
```
