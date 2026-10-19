# Read the raw Het Epos exports from data/ (not tracked in git).

# Folder with the exports; set PRESSURE_COOKER_DATA_DIR (e.g. in .Renviron) to read another one
data_dir <- Sys.getenv("PRESSURE_COOKER_DATA_DIR", unset = "data")

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
