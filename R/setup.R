# Packages and helpers shared by all analysis scripts.

suppressPackageStartupMessages({
  library(readxl)
  library(tidyverse)
  library(glmnet)
})

# Normalised pupil name used to join tables that spell names differently
make_key <- function(x) tolower(trimws(gsub("\\s+", " ", as.character(x))))
