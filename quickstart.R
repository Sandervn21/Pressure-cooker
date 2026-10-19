# Sets up and runs everything in one go: installs the R packages from renv.lock, checks Quarto and
# the data, renders the analysis notebook and the report, and opens both in the browser.
# Start it with ./quickstart.sh (macOS, Linux) or quickstart.bat (Windows), which first check for R.

if (!file.exists("renv.lock")) stop("Run this from the project folder (the one with renv.lock).")
if (!"renv" %in% loadedNamespaces()) source("renv/activate.R")

say <- function(...) cat("\n", ..., "\n", sep = "")

# Waits for Enter; typing q stops the script
stdin_con <- if (!interactive()) file("stdin", open = "r")
ask <- function(prompt) {
  answer <- if (interactive()) readline(prompt) else {
    cat(prompt)
    readLines(stdin_con, n = 1)
  }
  if (length(answer) == 0 || tolower(trimws(answer)) == "q") {
    say("Stopped.")
    quit(save = "no", status = 1)
  }
}

# 1. R and packages -------------------------------------------------------------------------

lock_r <- renv::lockfile_read()$R$Version
say("R ", as.character(getRversion()), " found.")
if (!identical(substr(as.character(getRversion()), 1, 3), substr(lock_r, 1, 3))) {
  say("Note: this project was made with R ", lock_r, ". It will probably work with your version, ",
      "but if packages fail to install, install R ", lock_r, " from https://cran.r-project.org/.")
}

say("Installing the R packages from renv.lock. The first time this can take a while.")
tryCatch(
  renv::restore(prompt = FALSE),
  error = function(e) {
    say("Installing the packages failed: ", conditionMessage(e))
    say("Packages that have to be built from source need build tools: on macOS run ",
        "`xcode-select --install`, on Windows install Rtools (https://cran.r-project.org/bin/windows/Rtools/), ",
        "on Linux install the system libraries named in the error above. Then run this again.")
    quit(save = "no", status = 1)
  }
)

if (!any(grepl("open sans|frutiger", systemfonts::system_fonts()$family, ignore.case = TRUE))) {
  say("Note: the font Open Sans is not installed, so the figures use a fallback font. For the same ",
      "look, install it from https://fonts.google.com/specimen/Open+Sans and run this again.")
}

# 2. Quarto ---------------------------------------------------------------------------------

find_quarto <- function() {
  candidates <- c(
    Sys.which("quarto"),
    # usual install locations, for when the installer has not updated this terminal's PATH yet
    file.path(Sys.getenv(c("ProgramFiles", "LOCALAPPDATA")), c("Quarto", "Programs/Quarto"), "bin/quarto.exe"),
    "/Applications/quarto/bin/quarto", "/opt/quarto/bin/quarto", "/usr/local/bin/quarto",
    # the copy that comes with RStudio
    "/Applications/RStudio.app/Contents/Resources/app/quarto/bin/quarto",
    file.path(Sys.getenv("ProgramFiles"), "RStudio/resources/app/bin/quarto/bin/quarto.exe")
  )
  found <- candidates[nzchar(candidates) & file.exists(candidates)]
  if (length(found)) found[[1]] else NA_character_
}

quarto <- find_quarto()
while (is.na(quarto)) {
  say("Quarto is not installed. Install it from https://quarto.org/docs/get-started/.")
  ask("Press Enter when it is installed (or type q to stop): ")
  quarto <- find_quarto()
}
say("Quarto ", system2(quarto, "--version", stdout = TRUE), " found.")

# 3. Data -----------------------------------------------------------------------------------

# Same folder as R/load_data.R
data_dir <- Sys.getenv("PRESSURE_COOKER_DATA_DIR", unset = "data")
required <- c("Pupils.xlsx", "Cito_UvA_EN.xlsx", "Observaties_groep_1_2.xlsx", "Extracurricular.xlsx",
              "Support interventions_care team.xlsx", "CLASS.xlsx")
optional <- c("UvA_testresults_math.xlsx", "pupil questionnaire.xlsx")
missing_files <- function(files) files[!file.exists(file.path(data_dir, files))]

# open the folder so the files can be dropped in
if (length(missing_files(c(required, optional))) && dir.exists(data_dir)) utils::browseURL(normalizePath(data_dir))
while (length(missing_files(required))) {
  say("These Het Epos exports are missing. Put them, with exactly these names, in\n",
      normalizePath(data_dir), "\n\n", paste0("  ", missing_files(required), collapse = "\n"))
  ask("Press Enter when they are there (or type q to stop): ")
}
if (length(missing_files(optional))) {
  say("These optional exports are missing:\n\n", paste0("  ", missing_files(optional), collapse = "\n"),
      "\n\nThe analysis runs without them, but some results will then be different.")
  ask("Add them now and press Enter, or just press Enter to go on without them (q to stop): ")
}
say("All data found in ", normalizePath(data_dir), ".")

# 4. Render and open ------------------------------------------------------------------------

Sys.setenv(QUARTO_R = R.home("bin"))  # let Quarto use this R
for (doc in c("analysis.qmd", "report.qmd")) {
  say("Rendering ", doc, " ...")
  if (system2(quarto, c("render", doc)) != 0) {
    say("Rendering ", doc, " failed; see the messages above.")
    quit(save = "no", status = 1)
  }
}

say("Done. Opening analysis.html and report.html in your browser.")
for (html in c("analysis.html", "report.html")) utils::browseURL(normalizePath(html))
