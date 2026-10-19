# Figures for the report slides: the notebook's results, drawn for a slide in the template's colours
# and font. Built from the result tables at render time, so the source holds no results.
# Needs the analysis scripts and R/conclusions.R to have been sourced, and `report_font` (set in report.qmd).

# Helpers for labels
capitalise <- function(x) paste0(toupper(substring(x, 1, 1)), substring(x, 2))
# "domain: auditory development (AO)" -> "auditory development"
predictor_name <- function(x) sub(" \\([A-Z]+\\)$", "", sub("^(domain|readiness): ", "", x))

# Plain-language names for the learning domains that may come out on top
friendly_skill <- function(predictor) {
  code <- sub(".*\\(([A-Z]+)\\)$", "\\1", predictor)
  plain <- c(
    AO = "hearing sounds", BT = "engagement", TG = "counting and numbers",
    MT = "measuring", MK = "shapes and space", VO = "visual perception", BV = "books and stories",
    IT = "talking with others", SA = "independence", SF = "flexibility with others",
    SI = "taking initiative", IB = "impulse control", IV = "empathy", WB = "well-being",
    TZ = "working on a task independently", KM = "fine motor skills"
  )
  out <- unname(plain[code])
  out[is.na(out)] <- predictor_name(predictor[is.na(out)])
  out
}

# Each kind of extracurricular lesson against the group 3 outcomes (all kindergarten factors together)
lesson_effects <- function() {
  ev_coefficient_table %>%
    filter(framing == "kindergarten factors", block == "Extracurricular", outcome != "Goals before group 3 achieved") %>%
    mutate(
      kind = capitalise(sub("^lessons: ", "", feature)),
      what = unname(c(
        "Cito maths, end of group 3" = "maths", "Cito TMT, end of group 3" = "reading speed",
        "Cito spelling, end of group 3" = "spelling", "Any support in group 3" = "support"
      )[outcome]),
      clear = p < 0.05
    )
}

report_colours <- c(
  orange = "#F6A70A", dark_orange = "#E98300", rust = "#B04E04", teal = "#3A9882",
  burgundy = "#6C1232", grey = "#9A9A9A", light = "#EDEDED", ink = "#262626"
)

# The plots' font: the slides' font, registered from its static Regular/Bold/Italic files when
# there are some (bold from a variable font file comes out at regular weight)
register_report_font <- function(family) {
  fonts <- systemfonts::system_fonts()
  fonts <- fonts[tolower(fonts$family) == tolower(family) & !grepl("variable", fonts$path, ignore.case = TRUE), ]
  pick <- function(style) {
    hit <- fonts$path[tolower(fonts$style) == style]
    if (length(hit)) hit[[1]] else NA_character_
  }
  files <- c(plain = pick("regular"), bold = pick("bold"), italic = pick("italic"), bolditalic = pick("bold italic"))
  if (is.na(files[["plain"]])) return(family)
  files[is.na(files)] <- files[["plain"]]
  systemfonts::register_font("Report Sans", plain = files[["plain"]], bold = files[["bold"]],
                             italic = files[["italic"]], bolditalic = files[["bolditalic"]])
  "Report Sans"
}

theme_report <- function(base_size = 13) {
  theme_minimal(base_size = base_size, base_family = report_font) +
    theme(
      text = element_text(colour = report_colours[["ink"]]),
      axis.text = element_text(colour = "grey35", size = rel(0.9)),
      axis.title = element_text(colour = "grey35", size = rel(0.9)),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(colour = "#E8E8E8", linewidth = 0.3),
      panel.spacing = unit(0.8, "lines"),
      strip.text = element_text(face = "bold", hjust = 0, size = rel(1)),
      legend.position = "top",
      legend.justification = "left",
      legend.title = element_blank(),
      legend.margin = margin(0, 0, 0, 0),
      legend.key.size = unit(0.8, "lines"),
      plot.caption = element_text(colour = "grey45", hjust = 0, size = rel(0.85)),
      plot.caption.position = "plot",
      plot.title = element_text(face = "bold", size = rel(1.05)),
      plot.title.position = "plot",
      plot.margin = margin(4, 6, 4, 4)
    )
}

test_label <- c(maths = "Maths", spelling = "Spelling", tmt = "TMT (reading speed)")
readiness_label <- c(
  transition_share = "Goals before group 3", basic_share = "Basic goals", ready_maths = "Maths",
  ready_language = "Language", ready_social_emotional = "Social-emotional", ready_fine_motor = "Fine motor",
  ready_gross_motor = "Gross motor"
)
outcome_label <- c(
  "Goals before group 3 achieved" = "Goals before group 3", "Cito maths, end of group 3" = "Cito maths",
  "Cito TMT, end of group 3" = "Cito TMT", "Cito spelling, end of group 3" = "Cito spelling",
  "Any support in group 3" = "Support in group 3"
)
percent_axis <- function(x) paste0(round(100 * x), "%")

# --- Time in kindergarten -------------------------------------------------------------------------

timing_colours <- c("usual" = "#B5B5B5", "earlier than most" = "#3A9882", "later than most" = "#6C1232")
timing_labels <- c("usual" = "Moved up with their age group", "earlier than most" = "A year earlier", "later than most" = "A year later")

slide_time_by_birth <- function() {
  pupils_table %>%
    filter(reached_g3, !is.na(birth_month), !is.na(months_to_g3)) %>%
    mutate(
      month = factor(month.abb[birth_month], levels = month.abb),
      g3_timing = factor(g3_timing, levels = names(timing_colours))
    ) %>%
    ggplot(aes(month, months_to_g3, colour = g3_timing)) +
    geom_point(position = position_jitter(width = 0.18, height = 0, seed = 1), size = 2.2, alpha = 0.85) +
    scale_colour_manual(values = timing_colours, labels = timing_labels) +
    scale_y_continuous(breaks = seq(12, 48, 6)) +
    labs(x = "Birth month", y = "Months in kindergarten") +
    theme_report() +
    theme(panel.grid.major.x = element_blank())
}

slide_direct_scatter <- function() {
  direct_data %>%
    select(months_to_g3, cito_g3_maths, cito_g3_tmt, cito_g3_spelling) %>%
    pivot_longer(starts_with("cito_"), names_to = "test", values_to = "ability") %>%
    filter(!is.na(ability)) %>%
    mutate(test = factor(unname(test_label[sub("cito_g3_", "", test)]), levels = test_label)) %>%
    ggplot(aes(months_to_g3, ability)) +
    geom_point(colour = "#B5B5B5", alpha = 0.7, size = 1.4) +
    geom_smooth(method = "lm", formula = y ~ x, colour = report_colours[["dark_orange"]],
                fill = report_colours[["orange"]], alpha = 0.2, linewidth = 1) +
    facet_wrap(~test, ncol = 1) +
    labs(x = "Months in kindergarten", y = "Cito score, end of group 3",
         caption = "Line: the trend, with its 95% band.") +
    theme_report()
}

slide_time_readiness <- function() {
  stage1_main %>%
    filter(factor == "6 more months in kindergarten") %>%
    mutate(
      outcome = factor(unname(readiness_label[outcome]), levels = rev(readiness_label)),
      clear = p < 0.05,
      label = if_else(clear, paste0(ifelse(per_sd > 0, "+", ""), formatC(per_sd, format = "f", digits = 2)), NA_character_)
    ) %>%
    ggplot(aes(per_sd, outcome, colour = clear)) +
    geom_vline(xintercept = 0, colour = "grey55", linewidth = 0.4) +
    geom_errorbar(aes(xmin = per_sd_low, xmax = per_sd_high), width = 0, linewidth = 1, orientation = "y") +
    geom_point(aes(size = clear)) +
    geom_text(aes(x = per_sd_high, label = label), hjust = -0.3, size = 3.4, fontface = "bold",
              family = report_font, na.rm = TRUE, show.legend = FALSE) +
    scale_colour_manual(values = c(`TRUE` = report_colours[["dark_orange"]], `FALSE` = "#B5B5B5"), guide = "none") +
    scale_size_manual(values = c(`TRUE` = 3.2, `FALSE` = 2.2), guide = "none") +
    scale_x_continuous(expand = expansion(mult = c(0.05, 0.2))) +
    labs(x = "Difference per six extra months (SD)", y = NULL,
         caption = "Lines show 95% intervals. Orange: a clear difference. Grey: no clear difference.") +
    theme_report() +
    theme(panel.grid.major.y = element_blank(), axis.text.y = element_text(size = rel(1.05), colour = report_colours[["ink"]]))
}

# --- What contributes to group 3 outcomes ----------------------------------------------------------

slide_contributions <- function() {
  block_fill <- c(Readiness = report_colours[["orange"]], Support = report_colours[["dark_orange"]],
                  Extracurricular = "#CFCFCF", Classroom = "#CFCFCF", Time = report_colours[["burgundy"]])
  block_label <- c(Readiness = "Readiness", Support = "Support needs", Extracurricular = "Extracurricular lessons",
                   Classroom = "Class", Time = "Time in kindergarten")
  ev_variance_table %>%
    filter(framing != "kindergarten factors") %>%
    mutate(
      outcome = factor(unname(outcome_label[outcome]), levels = outcome_label),
      block = factor(block, levels = rev(names(block_fill))),
      share = pmax(r2_share, 0)
    ) %>%
    ggplot(aes(share, block, fill = block)) +
    geom_col(width = 0.7) +
    geom_text(aes(label = percent_axis(share)), hjust = -0.15, size = 3.6, family = report_font, colour = "grey30") +
    facet_wrap(~outcome, ncol = 2) +
    scale_fill_manual(values = block_fill, guide = "none") +
    scale_y_discrete(labels = block_label) +
    scale_x_continuous(labels = percent_axis, n.breaks = 3, expand = expansion(mult = c(0, 0.35))) +
    labs(x = "Share of the differences between pupils", y = NULL) +
    theme_report() +
    theme(panel.grid.major.y = element_blank())
}

slide_lessons <- function() {
  lesson_effects() %>%
    mutate(
      better = if_else(what == "support", estimate < 0, estimate > 0),
      what = factor(c(maths = "Maths", `reading speed` = "Reading", spelling = "Spelling", support = "Support\nneeded")[what],
                    levels = c("Maths", "Reading", "Spelling", "Support\nneeded")),
      kind = fct_rev(factor(str_wrap(kind, 18))),
      shade = case_when(!clear ~ "unclear", better ~ "up", TRUE ~ "down"),
      label = paste0(ifelse(estimate > 0, "+", ""), formatC(estimate, format = "f", digits = 2))
    ) %>%
    ggplot(aes(what, kind, fill = shade)) +
    geom_tile(colour = "white", linewidth = 2) +
    geom_text(aes(label = label, colour = shade == "unclear"), size = 4.2, family = report_font, show.legend = FALSE) +
    scale_fill_manual(values = c(up = report_colours[["orange"]], down = report_colours[["burgundy"]], unclear = "#EFEFEF"),
                      labels = c(up = "Better result", down = "Worse result", unclear = "No clear difference"),
                      breaks = c("up", "down", "unclear")) +
    scale_colour_manual(values = c(`TRUE` = "grey45", `FALSE` = "white")) +
    scale_x_discrete(position = "top") +
    labs(x = NULL, y = NULL, caption = "Difference in group 3 (SD) for children who take more of each kind of lesson.\nReading is reading speed (Cito TMT). For support, a lower number is better.") +
    theme_report() +
    theme(panel.grid = element_blank(), axis.text.x = element_text(face = "bold", colour = report_colours[["ink"]]),
          axis.text.y = element_text(size = rel(1.05), colour = report_colours[["ink"]]), legend.position = "bottom")
}

# --- Teaching focus ----------------------------------------------------------------------------------

slide_class_check <- function() {
  class_cito_check %>%
    mutate(
      measure = if_else(row_number() == 1, "Maths readiness\n(scored by teachers)", sub(", end of group 3", "", measure)),
      measure = factor(measure, levels = rev(measure)),
      teacher = row_number() == 1,
      label = paste0(ifelse(difference_sd > 0, "+", ""), formatC(difference_sd, format = "f", digits = 2), " SD")
    ) %>%
    ggplot(aes(difference_sd, measure, colour = teacher)) +
    geom_vline(xintercept = 0, colour = "grey55", linewidth = 0.4) +
    geom_errorbar(aes(xmin = ci_low, xmax = ci_high), width = 0, linewidth = 1.1, orientation = "y") +
    geom_point(size = 3.4) +
    geom_text(aes(x = ci_high, label = label), hjust = -0.25, size = 3.4, family = report_font, show.legend = FALSE) +
    scale_colour_manual(values = c(`TRUE` = report_colours[["dark_orange"]], `FALSE` = "#9A9A9A"), guide = "none") +
    scale_x_continuous(expand = expansion(mult = c(0.05, 0.25))) +
    labs(x = "Class 1/2b compared with 1/2a (SD, 95% interval)", y = NULL) +
    theme_report() +
    theme(panel.grid.major.y = element_blank(), axis.text.y = element_text(size = rel(1.05), colour = report_colours[["ink"]]))
}

slide_skills <- function(top = 8) {
  key <- consistent_predictors()$predictor
  goals_selection %>%
    filter(model == "+ readiness by domain", block %in% c("Readiness", "Readiness by domain", "Time")) %>%
    group_by(predictor) %>%
    summarise(selected = mean(selected_pct) / 100, .groups = "drop") %>%
    arrange(desc(selected)) %>%
    filter(row_number() <= top | predictor == "months in kindergarten") %>%
    mutate(
      kind = case_when(predictor %in% key ~ "key", predictor == "months in kindergarten" ~ "time", TRUE ~ "other"),
      name = capitalise(if_else(str_starts(predictor, "readiness: "),
                                paste(sub("readiness: ", "", predictor), "(subject score)"), friendly_skill(predictor))),
      name = fct_reorder(name, selected)
    ) %>%
    ggplot(aes(selected, name, fill = kind)) +
    geom_col(width = 0.7) +
    geom_text(aes(label = percent_axis(selected)), hjust = -0.15, size = 3.8, family = report_font, colour = "grey30") +
    scale_fill_manual(values = c(key = report_colours[["orange"]], time = report_colours[["burgundy"]], other = "#CFCFCF"), guide = "none") +
    scale_x_continuous(labels = percent_axis, limits = c(0, 1.12), expand = c(0, 0)) +
    labs(x = "Share of the models that pick it", y = NULL,
         caption = "Average over the models for maths, reading speed and spelling.") +
    theme_report() +
    theme(panel.grid.major.y = element_blank(), axis.text.y = element_text(size = rel(1.05), colour = report_colours[["ink"]]))
}

slide_prediction <- function() {
  fitted <- goals_prediction %>%
    filter(str_starts(model, "\\+ readiness")) %>%
    group_by(Test) %>%
    summarise(new = max(predicted_r2, 0), .groups = "drop")
  built <- goals_prediction %>%
    filter(model == "+ readiness by subject") %>%
    select(Test, built = in_sample_r2)
  inner_join(fitted, built, by = "Test") %>%
    pivot_longer(c(built, new), names_to = "who", values_to = "share") %>%
    mutate(
      Test = factor(sub(", end of group 3", "", Test), levels = rev(sub(", end of group 3", "", goals_cito_tests))),
      who = factor(who, levels = c("built", "new"))
    ) %>%
    ggplot(aes(share, Test, fill = who, group = fct_rev(who))) +
    geom_col(position = position_dodge(width = 0.8), width = 0.75) +
    geom_text(aes(label = percent_axis(share)), position = position_dodge(width = 0.8), hjust = -0.15,
              size = 3.8, family = report_font, colour = "grey30") +
    scale_fill_manual(values = c(built = "#D9D9D9", new = report_colours[["orange"]]),
                      labels = c(built = "Pupils the model was built on", new = "New pupils")) +
    scale_x_continuous(labels = percent_axis, limits = c(0, NA), expand = expansion(mult = c(0, 0.25))) +
    labs(x = "Share of the differences in Cito that is explained", y = NULL) +
    theme_report() +
    theme(panel.grid.major.y = element_blank(), axis.text.y = element_text(size = rel(1.05), colour = report_colours[["ink"]]))
}

# --- The questions and how the analysis connects them ---------------------------------------------

slide_diagram <- function() {
  boxes <- tibble::tribble(
    ~x, ~y, ~title, ~body,
    0, 0, "Kindergarten factors", "Time in kindergarten\nClass and classroom quality\nExtracurricular lessons\nSupport",
    3.6, 0.9, "Readiness", "At the end of kindergarten:\nsubjects, learning domains\nand goals",
    7.2, 0, "Group 3 outcomes", "Cito maths, TMT and spelling\nSupport\nWell-being"
  )
  w <- 2.8; head_h <- 0.45; body_h <- 1.35
  ggplot() +
    geom_rect(data = boxes, aes(xmin = x, xmax = x + w, ymin = y - body_h, ymax = y), fill = "#F4F4F4") +
    geom_rect(data = boxes, aes(xmin = x, xmax = x + w, ymin = y, ymax = y + head_h), fill = report_colours[["orange"]]) +
    geom_text(data = boxes, aes(x = x + 0.15, y = y + head_h / 2, label = title), hjust = 0, fontface = "bold",
              size = 4.8, family = report_font, colour = report_colours[["ink"]]) +
    geom_text(data = boxes, aes(x = x + 0.15, y = y - body_h / 2, label = body), hjust = 0, lineheight = 1.05,
              size = 3.9, family = report_font, colour = "grey20") +
    annotate("segment", x = w + 0.1, xend = 3.6 - 0.1, y = 0.1, yend = 0.45,
             arrow = arrow(length = unit(0.12, "inches"), type = "closed"), colour = "grey35", linewidth = 0.6) +
    annotate("segment", x = 3.6 + w + 0.1, xend = 7.2 - 0.1, y = 0.45, yend = 0.1,
             arrow = arrow(length = unit(0.12, "inches"), type = "closed"), colour = "grey35", linewidth = 0.6) +
    annotate("curve", x = w / 2, xend = 7.2 + w / 2, y = -body_h - 0.1, yend = -body_h - 0.1, curvature = 0.22,
             arrow = arrow(length = unit(0.12, "inches"), type = "closed"), colour = "grey35", linewidth = 0.6) +
    coord_cartesian(xlim = c(-0.1, 10.1), ylim = c(-2.4, 1.5), expand = FALSE) +
    theme_void() +
    theme(plot.background = element_rect(fill = "white", colour = NA))
}
