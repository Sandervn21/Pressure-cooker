# Pressure-cooker

Analysis for Het Epos (Behavioral Data Science Toolbox, group 4): how time in kindergarten, learning goals and
extracurricular activities in groups 1–2 relate to results in group 3 and later.

## Setup

1. Install [R](https://cran.r-project.org/) (4.6) and [Quarto](https://quarto.org/).
2. Restore the package environment from the project folder:

   ```r
   renv::restore()
   ```

3. Put the Het Epos Excel exports in `data/` (this folder is gitignored; never commit pupil data):

   - `Extracurricular.xlsx`
   - `Observaties_groep_1_2.xlsx`
   - `Cito_UvA_EN.xlsx`
   - `Pupils.xlsx`
   - `Support interventions_care team.xlsx`
   - `CLASS.xlsx`

## Run

```bash
quarto render analysis.qmd
```

This produces `analysis.html` with all tables and plots.

## Layout

| Path | Contents |
| --- | --- |
| `R/setup.R` | Packages and shared helpers |
| `R/load_data.R` | Reads the Excel files from `data/` |
| `R/cleanup.R` | Builds pupil-level tables (learning goals, CITO, interventions, CLASS, time in kindergarten) |
| `R/q1_model.R` | Q1: kindergarten time × extracurricular activities regression |
| `R/q2_analysis.R` | Q2: learning goals vs CITO correlations and cross-validated models |
| `analysis.qmd` | Notebook that runs the scripts in order and shows the results |
