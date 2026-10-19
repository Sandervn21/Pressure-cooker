# Read the raw Het Epos exports from data/ (not tracked in git).

# Folder with the exports; set PRESSURE_COOKER_DATA_DIR (e.g. in .Renviron) to read another one
data_dir <- Sys.getenv("PRESSURE_COOKER_DATA_DIR", unset = "data")

raw_path <- function(file) {
  path <- file.path(data_dir, file)
  if (!file.exists(path)) {
    stop("Missing data file: ", path, ". Put the Het Epos Excel exports in ", data_dir, "/.")
  }
  path
}

read_raw <- function(file) read_excel(raw_path(file))

# Tables the analysis can do without: NULL (with a message) when the file is missing
read_optional <- function(file) {
  if (!file.exists(file.path(data_dir, file))) {
    message("Optional data file not found, skipping: ", file.path(data_dir, file))
    return(NULL)
  }
  read_raw(file)
}

# Stack every sheet that has the required columns, adding the sheet name as `sheet_column`.
# Sheets differ in column types (e.g. Score as text in one, numbers in another), so they are
# combined as text and the types are guessed again afterwards.
read_sheets <- function(file, required, sheet_column) {
  path <- raw_path(file)
  sheets <- Filter(
    function(s) all(required %in% names(read_excel(path, sheet = s, n_max = 0))),
    excel_sheets(path)
  )
  if (length(sheets) == 0) stop("No sheet in ", path, " has the columns ", toString(required))
  sheets |>
    set_names() |>
    map(~ mutate(read_excel(path, sheet = .x), across(everything(), as.character))) |>
    list_rbind(names_to = sheet_column) |>
    type_convert(col_types = cols(.default = col_guess()))
}

data_extra <- read_raw("Extracurricular.xlsx")
data_observatie <- read_raw("Observaties_groep_1_2.xlsx")
# Cito has one sheet per leaving cohort (CLASS_2028, ...), plus explanation sheets
data_grade <- read_sheets("Cito_UvA_EN.xlsx", c("Grade", "Test", "Score", "Name"), sheet_column = "Sheet")
data_kids <- read_raw("Pupils.xlsx")
data_intervention <- read_raw("Support interventions_care team.xlsx")
data_CLASS <- read_raw("CLASS.xlsx")
data_questionnaire <- read_optional("pupil questionnaire.xlsx")
data_math <- read_optional("UvA_testresults_math.xlsx")
