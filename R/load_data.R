# Read the raw Het Epos exports from data/ (not tracked in git).

data_dir <- "data"

read_raw <- function(file) {
  path <- file.path(data_dir, file)
  if (!file.exists(path)) {
    stop("Missing data file: ", path, ". Put the Het Epos Excel exports in ", data_dir, "/.")
  }
  read_excel(path)
}

data_extra <- read_raw("Extracurricular.xlsx")
data_observatie <- read_raw("Observaties_groep_1_2.xlsx")
data_grade <- read_raw("Cito_UvA_EN.xlsx")
data_kids <- read_raw("Pupils.xlsx")
data_intervention <- read_raw("Support interventions_care team.xlsx")
data_CLASS <- read_raw("CLASS.xlsx")
