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
| `R/setup.R`, `R/load_data.R` | Packages, and reading the Excel files from `data/` |
| `R/cleanup.R` | Cleans Cito scores, support records and CLASS observations |
| `R/pupil_table.R` | The pupil table: group 3 timing, time in kindergarten, readiness, outcomes |
| `R/direct_path.R`, `R/stage1.R` | Chapters 2 and 3: time in kindergarten against group 3 and readiness |
| `R/explained_variance.R` | Chapter 4: all kindergarten factors together and their explained variance |
| `R/cv_models.R`, `R/goals.R` | Chapter 5: cross-validated models for subjects, learning domains and goals |
| `R/conclusions.R` | The conclusion boxes, written from the results |
| `analysis.qmd` | Notebook that runs the scripts in order and shows the results |
| `report.qmd`, `report/` | The report slides, their numbers, figures and theme |
| `quickstart.sh`, `quickstart.bat`, `quickstart.R` | One command that sets everything up and renders both outputs |
