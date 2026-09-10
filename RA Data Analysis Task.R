### Youth Impact Research Associate hiring task
### Data cleaning, KPI construction, figures, and quality checks

# Run this script from the repository root. Place the three raw .dta files either
# in data/raw/ or in the repository root. The script does not install packages so
# that it does not change the user's R environment during a reproducible run.

required_packages <- c(
  "haven", "dplyr", "tidyr", "ggplot2", "readr", "stringr",
  "scales", "purrr", "tibble"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  stop(
    "Install the following packages before running the script: ",
    paste(missing_packages, collapse = ", ")
  )
}

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(readr)
  library(stringr)
  library(scales)
  library(purrr)
  library(tibble)
})

options(dplyr.summarise.inform = FALSE)


## 1. File paths ---------------------------------------------------------------

input_files <- c(
  sensitization = "01_sensitization_data.dta",
  implementation = "02_implementation_data.dta",
  endline = "03_endline_data.dta"
)

candidate_input_dirs <- c(file.path("data", "raw"), ".")
input_dir <- candidate_input_dirs[
  vapply(
    candidate_input_dirs,
    function(path) all(file.exists(file.path(path, input_files))),
    logical(1)
  )
][1]

if (is.na(input_dir)) {
  stop(
    "Could not find the three input files. Put them in data/raw/ or in the ",
    "repository root, then run the script from the repository root."
  )
}

output_dir <- "outputs"
data_output_dir <- file.path(output_dir, "data")
table_output_dir <- file.path(output_dir, "tables")
figure_output_dir <- file.path(output_dir, "figures")

dir.create(data_output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(table_output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(figure_output_dir, recursive = TRUE, showWarnings = FALSE)


## 2. Helper functions ---------------------------------------------------------

to_numeric <- function(x) {
  suppressWarnings(as.numeric(as.character(x)))
}

clean_id <- function(x) {
  x <- str_trim(as.character(x))
  na_if(x, "")
}

first_nonmissing <- function(x) {
  keep <- !is.na(x)
  if (is.character(x)) {
    keep <- keep & nzchar(str_trim(x))
  }
  if (any(keep)) {
    x[which(keep)[1]]
  } else {
    x[NA_integer_][1]
  }
}

safe_rate <- function(numerator, denominator) {
  ifelse(denominator > 0, numerator / denominator, NA_real_)
}

# A sensitization or endline dataset contains call attempts, not independent
# students. For each student, use the earliest valid assessment. This preserves
# the temporal meaning of a baseline/final assessment and avoids choosing a
# result based on its score. If no valid assessment exists, retain the latest
# attempt so the final operational status remains available.
select_phase_record <- function(
    data,
    level_variable,
    status_variable,
    levelled_variable,
    prefix,
    valid_assessment = c("earliest", "latest")) {

  valid_assessment <- match.arg(valid_assessment)

  prepared <- data %>%
    mutate(
      hhid = clean_id(hhid),
      level_num = to_numeric(.data[[level_variable]]),
      status_num = to_numeric(.data[[status_variable]]),
      levelled_num = to_numeric(.data[[levelled_variable]]),
      assessed = between(level_num, 0, 4) & levelled_num == 1
    ) %>%
    arrange(hhid, submissiondate, key)

  group_stats <- prepared %>%
    group_by(hhid) %>%
    summarise(
      attempt_count = n(),
      assessed_submission_count = sum(assessed, na.rm = TRUE),
      distinct_assessed_levels = n_distinct(level_num[assessed], na.rm = TRUE),
      multiple_assessments = as.integer(assessed_submission_count > 1),
      assessment_conflict = as.integer(distinct_assessed_levels > 1),
      .groups = "drop"
    )

  selected <- prepared %>%
    group_by(hhid) %>%
    group_modify(function(student_rows, group_key) {
      assessed_rows <- student_rows %>% filter(assessed)

      if (nrow(assessed_rows) > 0) {
        assessed_rows <- assessed_rows %>% arrange(submissiondate, key)
        if (valid_assessment == "earliest") {
          slice_head(assessed_rows, n = 1)
        } else {
          slice_tail(assessed_rows, n = 1)
        }
      } else {
        student_rows %>%
          arrange(submissiondate, key) %>%
          slice_tail(n = 1)
      }
    }) %>%
    ungroup() %>%
    left_join(group_stats, by = "hhid") %>%
    transmute(
      hhid,
      submissiondate,
      level = level_num,
      status = status_num,
      assessed = as.integer(assessed),
      attempt_count,
      assessed_submission_count,
      multiple_assessments,
      assessment_conflict,
      school,
      school_id,
      region,
      grade,
      age,
      gender
    ) %>%
    rename_with(
      .fn = function(variable) paste0(prefix, "_", variable),
      .cols = -hhid
    )

  selected
}

# Implementation submissions are call attempts within a student-week. Select
# the earliest successful tutoring submission for each student-week. If no call
# succeeded, retain the latest attempt. Keep counts and conflict flags so the
# reduction does not erase data-quality information.
select_implementation_record <- function(data) {
  prepared <- data %>%
    mutate(
      hhid = clean_id(hhid),
      week = to_numeric(week),
      status = to_numeric(call_status_i),
      tutored = to_numeric(stud_tutored_i),
      operation = to_numeric(operation_taught_num_i),
      checkpoint_correct = to_numeric(checkpoint1_correct_num_i),
      targeting_accuracy = to_numeric(targeting_accuracy_num_i),
      duration_tutorial = to_numeric(duration_tutorial_i),
      success = tutored == 1 & between(operation, 1, 4)
    ) %>%
    filter(between(week, 1, 6)) %>%
    arrange(hhid, week, submissiondate, key)

  group_stats <- prepared %>%
    group_by(hhid, week) %>%
    summarise(
      attempt_count = n(),
      successful_submission_count = sum(success, na.rm = TRUE),
      distinct_success_operations = n_distinct(operation[success], na.rm = TRUE),
      multiple_successes = as.integer(successful_submission_count > 1),
      operation_conflict = as.integer(
        successful_submission_count > 1 & distinct_success_operations > 1
      ),
      .groups = "drop"
    )

  selected <- prepared %>%
    group_by(hhid, week) %>%
    group_modify(function(student_week_rows, group_key) {
      successful_rows <- student_week_rows %>% filter(success)
      if (nrow(successful_rows) > 0) {
        successful_rows %>%
          arrange(submissiondate, key) %>%
          slice_head(n = 1)
      } else {
        student_week_rows %>%
          arrange(submissiondate, key) %>%
          slice_tail(n = 1)
      }
    }) %>%
    ungroup() %>%
    left_join(group_stats, by = c("hhid", "week")) %>%
    mutate(
      week = as.integer(week),
      attempted = 1L,
      tutored = as.integer(success),
      checkpoint_passed = if_else(
        success,
        as.integer(checkpoint_correct > 0),
        NA_integer_
      )
    )

  selected
}


## 3. Load and audit raw data --------------------------------------------------

sensitization_raw <- read_dta(file.path(input_dir, input_files[["sensitization"]]))
implementation_raw <- read_dta(file.path(input_dir, input_files[["implementation"]]))
endline_raw <- read_dta(file.path(input_dir, input_files[["endline"]]))

raw_frames <- list(
  sensitization = sensitization_raw,
  implementation = implementation_raw,
  endline = endline_raw
)

missing_id_counts <- map_int(raw_frames, ~ sum(is.na(clean_id(.x$hhid))))
if (any(missing_id_counts > 0)) {
  stop(
    "The analysis requires hhid in every row. Missing hhid values: ",
    paste(names(missing_id_counts), missing_id_counts, collapse = "; ")
  )
}

duplicate_key_counts <- map_int(raw_frames, ~ sum(duplicated(.x$key)))
if (any(duplicate_key_counts > 0)) {
  warning(
    "Duplicate SurveyCTO submission keys found: ",
    paste(names(duplicate_key_counts), duplicate_key_counts, collapse = "; ")
  )
}

invalid_weeks <- implementation_raw %>%
  transmute(week = to_numeric(week)) %>%
  filter(!is.na(week), !between(week, 1, 6))

if (nrow(invalid_weeks) > 0) {
  warning(nrow(invalid_weeks), " implementation submissions fall outside weeks 1-6 and were excluded.")
}


## 4. Select one analytic record per phase/week --------------------------------

baseline <- select_phase_record(
  sensitization_raw,
  level_variable = "stud_level_num_b",
  status_variable = "call_status_b",
  levelled_variable = "stud_levelled_b",
  prefix = "baseline",
  valid_assessment = "earliest"
)

endline <- select_phase_record(
  endline_raw,
  level_variable = "stud_level_num",
  status_variable = "call_status_e",
  levelled_variable = "stud_levelled_e",
  prefix = "endline",
  valid_assessment = "earliest"
)

implementation_selected <- select_implementation_record(implementation_raw)

implementation_profile <- implementation_raw %>%
  mutate(hhid = clean_id(hhid)) %>%
  arrange(hhid, submissiondate, key) %>%
  group_by(hhid) %>%
  summarise(
    impl_school = first_nonmissing(school),
    impl_school_id = first_nonmissing(school_id),
    impl_region = first_nonmissing(region),
    impl_grade = first_nonmissing(grade),
    impl_age = first_nonmissing(age),
    impl_gender = first_nonmissing(gender),
    .groups = "drop"
  )

implementation_wide <- implementation_selected %>%
  select(
    hhid, week, attempted, attempt_count, status, tutored, operation,
    checkpoint_correct, checkpoint_passed, targeting_accuracy,
    duration_tutorial, successful_submission_count, multiple_successes,
    operation_conflict, submissiondate
  ) %>%
  pivot_wider(
    names_from = week,
    values_from = c(
      attempted, attempt_count, status, tutored, operation,
      checkpoint_correct, checkpoint_passed, targeting_accuracy,
      duration_tutorial, successful_submission_count, multiple_successes,
      operation_conflict, submissiondate
    ),
    names_glue = "w{week}_{.value}"
  )


## 5. Merge to one row per student --------------------------------------------

sensitization_ids <- clean_id(sensitization_raw$hhid)
implementation_ids <- clean_id(implementation_raw$hhid)
endline_ids <- clean_id(endline_raw$hhid)

student_universe <- tibble(
  hhid = sort(unique(c(sensitization_ids, implementation_ids, endline_ids)))
) %>%
  filter(!is.na(hhid))

student_level <- student_universe %>%
  left_join(baseline, by = "hhid") %>%
  left_join(implementation_profile, by = "hhid") %>%
  left_join(implementation_wide, by = "hhid") %>%
  left_join(endline, by = "hhid")

weekly_zero_fields <- grep(
  "^w[1-6]_(attempted|attempt_count|tutored|successful_submission_count|multiple_successes|operation_conflict)$",
  names(student_level),
  value = TRUE
)

student_level <- student_level %>%
  mutate(
    across(all_of(weekly_zero_fields), ~ replace_na(as.integer(.x), 0L)),
    school = coalesce(
      as.character(baseline_school), as.character(impl_school),
      as.character(endline_school)
    ),
    school_id = coalesce(
      as.character(baseline_school_id), as.character(impl_school_id),
      as.character(endline_school_id)
    ),
    region = coalesce(
      as.character(baseline_region), as.character(impl_region),
      as.character(endline_region)
    ),
    grade = coalesce(
      as.character(baseline_grade), as.character(impl_grade),
      as.character(endline_grade)
    ),
    age = coalesce(
      as.character(baseline_age), as.character(impl_age),
      as.character(endline_age)
    ),
    gender = coalesce(
      as.character(baseline_gender), as.character(impl_gender),
      as.character(endline_gender)
    ),
    tutoring_weeks_completed = rowSums(
      across(all_of(paste0("w", 1:6, "_tutored"))), na.rm = TRUE
    ),
    implementation_attempts = rowSums(
      across(all_of(paste0("w", 1:6, "_attempt_count"))), na.rm = TRUE
    ),
    any_implementation_attempt = as.integer(
      rowSums(across(all_of(paste0("w", 1:6, "_attempted"))), na.rm = TRUE) > 0
    ),
    paired_assessed = as.integer(
      replace_na(baseline_assessed, 0L) == 1 &
        replace_na(endline_assessed, 0L) == 1 &
        !is.na(baseline_level) & !is.na(endline_level)
    ),
    level_gain = if_else(
      paired_assessed == 1,
      endline_level - baseline_level,
      NA_real_
    ),
    learned_new_operation = if_else(
      paired_assessed == 1,
      as.integer(level_gain > 0),
      NA_integer_
    )
  ) %>%
  relocate(hhid, school, school_id, region, grade, age, gender)

if (anyDuplicated(student_level$hhid) > 0) {
  stop("The final dataset contains duplicate hhid values.")
}


## 6. Define analytic samples and KPIs ----------------------------------------

baseline_cohort <- student_level %>%
  filter(replace_na(baseline_assessed, 0L) == 1)

paired_sample <- student_level %>%
  filter(paired_assessed == 1)

level_labels <- c(
  `0` = "No operation",
  `1` = "Addition",
  `2` = "Subtraction",
  `3` = "Multiplication",
  `4` = "Division"
)

paired_n <- nrow(paired_sample)

kpi_summary <- tribble(
  ~metric, ~numerator, ~denominator,
  "Mastering division at endline",
  sum(paired_sample$endline_level == 4, na.rm = TRUE), paired_n,
  "Not mastering any operation at endline",
  sum(paired_sample$endline_level == 0, na.rm = TRUE), paired_n,
  "Learned at least one new operation",
  sum(paired_sample$learned_new_operation == 1, na.rm = TRUE), paired_n
) %>%
  mutate(
    rate = safe_rate(numerator, denominator),
    sample = "Students with valid baseline and endline assessments"
  )

level_distribution <- paired_sample %>%
  select(hhid, baseline_level, endline_level) %>%
  pivot_longer(
    cols = c(baseline_level, endline_level),
    names_to = "timepoint",
    values_to = "level"
  ) %>%
  mutate(
    timepoint = recode(
      timepoint,
      baseline_level = "Baseline",
      endline_level = "Endline"
    ),
    timepoint = factor(timepoint, levels = c("Baseline", "Endline")),
    level = as.integer(level)
  ) %>%
  count(timepoint, level, name = "students") %>%
  complete(
    timepoint,
    level = 0:4,
    fill = list(students = 0L)
  ) %>%
  group_by(timepoint) %>%
  mutate(
    denominator = sum(students),
    rate = safe_rate(students, denominator),
    level_label = factor(level_labels[as.character(level)], levels = level_labels)
  ) %>%
  ungroup()

weekly_fidelity <- map_dfr(1:6, function(week_number) {
  attempted <- sum(
    baseline_cohort[[paste0("w", week_number, "_attempted")]],
    na.rm = TRUE
  )
  tutored <- sum(
    baseline_cohort[[paste0("w", week_number, "_tutored")]],
    na.rm = TRUE
  )

  tibble(
    week = week_number,
    baseline_cohort = nrow(baseline_cohort),
    attempted_n = attempted,
    attempted_rate = safe_rate(attempted, nrow(baseline_cohort)),
    tutored_n = tutored,
    tutored_rate = safe_rate(tutored, nrow(baseline_cohort))
  )
})

tutoring_weeks_distribution <- baseline_cohort %>%
  count(tutoring_weeks_completed, name = "students") %>%
  complete(tutoring_weeks_completed = 0:6, fill = list(students = 0L)) %>%
  mutate(rate = safe_rate(students, nrow(baseline_cohort)))

learning_by_tutoring_weeks <- paired_sample %>%
  group_by(tutoring_weeks_completed) %>%
  summarise(
    students = n(),
    learned_n = sum(learned_new_operation == 1, na.rm = TRUE),
    learned_rate = safe_rate(learned_n, students),
    mean_level_gain = mean(level_gain, na.rm = TRUE),
    .groups = "drop"
  )

endline_followup_by_baseline_level <- baseline_cohort %>%
  mutate(
    baseline_level = as.integer(baseline_level),
    baseline_level_label = factor(
      level_labels[as.character(baseline_level)],
      levels = level_labels
    )
  ) %>%
  group_by(baseline_level, baseline_level_label) %>%
  summarise(
    baseline_students = n(),
    endline_assessed = sum(replace_na(endline_assessed, 0L) == 1),
    endline_rate = safe_rate(endline_assessed, baseline_students),
    .groups = "drop"
  )

sample_flow <- tribble(
  ~stage, ~students,
  "Any sensitization attempt", n_distinct(sensitization_ids),
  "Valid baseline assessment", nrow(baseline_cohort),
  "Any endline attempt", sum(replace_na(baseline_cohort$endline_attempt_count, 0L) > 0),
  "Valid paired assessments", nrow(paired_sample)
) %>%
  mutate(
    stage = factor(stage, levels = rev(stage)),
    share_of_baseline_assessed = students / nrow(baseline_cohort)
  )


## 7. Data-quality summary and sensitivity check -------------------------------

baseline_latest <- select_phase_record(
  sensitization_raw,
  level_variable = "stud_level_num_b",
  status_variable = "call_status_b",
  levelled_variable = "stud_levelled_b",
  prefix = "baseline",
  valid_assessment = "latest"
)

latest_baseline_sensitivity <- baseline_latest %>%
  select(hhid, baseline_level, baseline_assessed) %>%
  inner_join(
    endline %>% select(hhid, endline_level, endline_assessed),
    by = "hhid"
  ) %>%
  filter(baseline_assessed == 1, endline_assessed == 1) %>%
  summarise(
    denominator = n(),
    learned_n = sum(endline_level > baseline_level, na.rm = TRUE),
    learned_rate = safe_rate(learned_n, denominator)
  )

implementation_without_sensitization <- length(
  setdiff(unique(implementation_ids), unique(sensitization_ids))
)

data_quality_summary <- tribble(
  ~issue, ~count, ~unit, ~treatment,
  "Multiple valid sensitization assessments",
  sum(baseline$baseline_multiple_assessments, na.rm = TRUE),
  "students",
  "Use earliest valid assessment; retain count and conflict flag",
  "Conflicting sensitization levels",
  sum(baseline$baseline_assessment_conflict, na.rm = TRUE),
  "students",
  "Use earliest valid assessment; report sensitivity using latest valid assessment",
  "Multiple valid endline assessments",
  sum(endline$endline_multiple_assessments, na.rm = TRUE),
  "students",
  "Use earliest valid assessment; all repeated endline levels agree",
  "Multiple successful implementation submissions",
  sum(implementation_selected$multiple_successes, na.rm = TRUE),
  "student-weeks",
  "Use earliest successful session; retain attempt and success counts",
  "Conflicting operations across successful submissions",
  sum(implementation_selected$operation_conflict, na.rm = TRUE),
  "student-weeks",
  "Flag for review; use first successful session for the analytic record",
  "Implementation IDs absent from sensitization export",
  implementation_without_sensitization,
  "students",
  "Keep in outer-joined dataset; exclude from baseline-cohort rates",
  "Baseline-assessed students without a valid endline assessment",
  nrow(baseline_cohort) - nrow(paired_sample),
  "students",
  "Report paired-sample denominator and attrition limitation"
)


## 8. Save datasets and tables -------------------------------------------------

# The student-level outputs contain de-identified row-level records and should
# not be committed to a public repository. The repository .gitignore excludes
# this directory.
write_csv(student_level, file.path(data_output_dir, "student_level_analysis.csv"), na = "")
saveRDS(student_level, file.path(data_output_dir, "student_level_analysis.rds"))
write_csv(implementation_selected, file.path(data_output_dir, "implementation_selected_long.csv"), na = "")

write_csv(kpi_summary, file.path(table_output_dir, "kpi_summary.csv"))
write_csv(level_distribution, file.path(table_output_dir, "level_distribution_paired.csv"))
write_csv(weekly_fidelity, file.path(table_output_dir, "weekly_fidelity.csv"))
write_csv(tutoring_weeks_distribution, file.path(table_output_dir, "tutoring_weeks_distribution.csv"))
write_csv(learning_by_tutoring_weeks, file.path(table_output_dir, "learning_by_tutoring_weeks.csv"))
write_csv(endline_followup_by_baseline_level, file.path(table_output_dir, "endline_followup_by_baseline_level.csv"))
write_csv(sample_flow, file.path(table_output_dir, "sample_flow.csv"))
write_csv(data_quality_summary, file.path(table_output_dir, "data_quality_summary.csv"))
write_csv(latest_baseline_sensitivity, file.path(table_output_dir, "duplicate_rule_sensitivity.csv"))


## 9. Produce KPI and fidelity figures ----------------------------------------

blue <- "#1B6CA8"
orange <- "#F28E2B"
teal <- "#2A9D8F"
dark_text <- "#25313C"
light_grid <- "#D9E1E8"

theme_youth <- function() {
  theme_minimal(base_size = 12) +
    theme(
      text = element_text(family = "sans", colour = dark_text),
      plot.title = element_text(face = "bold", size = 16, margin = margin(b = 8)),
      plot.subtitle = element_text(size = 11, colour = "#566573", margin = margin(b = 12)),
      plot.caption = element_text(size = 9, colour = "#6B7785", hjust = 0),
      panel.grid.major.x = element_line(colour = light_grid, linewidth = 0.4),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      axis.title = element_blank(),
      legend.title = element_blank(),
      plot.margin = margin(14, 18, 14, 14)
    )
}

kpi_plot_data <- kpi_summary %>%
  mutate(
    short_metric = recode(
      metric,
      "Mastering division at endline" = "Mastering division",
      "Not mastering any operation at endline" = "No operation mastered",
      "Learned at least one new operation" = "Learned a new operation"
    ),
    short_metric = factor(short_metric, levels = rev(short_metric))
  )

kpi_plot <- ggplot(kpi_plot_data, aes(x = short_metric, y = rate)) +
  geom_col(width = 0.58, fill = blue) +
  geom_text(
    aes(label = percent(rate, accuracy = 0.1)),
    hjust = -0.14,
    size = 4.2,
    fontface = "bold",
    colour = dark_text
  ) +
  coord_flip() +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 0.9)) +
  labs(
    title = "Required learning KPIs",
    subtitle = "Students with valid baseline and endline assessments",
    caption = paste0("N = ", paired_n, ". Learning changes are descriptive; no comparison group is available.")
  ) +
  theme_youth()

ggsave(
  file.path(figure_output_dir, "01_required_kpis.png"),
  kpi_plot,
  width = 10,
  height = 5.4,
  dpi = 300,
  bg = "white"
)

level_plot <- ggplot(
  level_distribution,
  aes(x = level_label, y = rate, fill = timepoint)
) +
  geom_col(position = position_dodge(width = 0.72), width = 0.62) +
  geom_text(
    aes(label = percent(rate, accuracy = 0.1)),
    position = position_dodge(width = 0.72),
    vjust = -0.35,
    size = 3.2,
    colour = dark_text
  ) +
  scale_fill_manual(values = c("Baseline" = "#A9B8C6", "Endline" = blue)) +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 0.60)) +
  labs(
    title = "Students shifted toward multiplication and division",
    subtitle = "Highest operation mastered at baseline and endline",
    caption = paste0("Paired sample, N = ", paired_n)
  ) +
  theme_youth() +
  theme(
    axis.text.x = element_text(angle = 20, hjust = 1),
    legend.position = "top"
  )

ggsave(
  file.path(figure_output_dir, "02_level_distribution.png"),
  level_plot,
  width = 10,
  height = 5.8,
  dpi = 300,
  bg = "white"
)

weekly_plot_data <- weekly_fidelity %>%
  select(week, attempted_rate, tutored_rate) %>%
  pivot_longer(
    cols = c(attempted_rate, tutored_rate),
    names_to = "measure",
    values_to = "rate"
  ) %>%
  mutate(
    measure = recode(
      measure,
      attempted_rate = "Any call attempt logged",
      tutored_rate = "Tutoring completed"
    )
  )

weekly_plot <- ggplot(
  weekly_plot_data,
  aes(x = week, y = rate, colour = measure, group = measure)
) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.8) +
  geom_text(
    aes(label = percent(rate, accuracy = 1)),
    vjust = -0.8,
    size = 3.2,
    show.legend = FALSE
  ) +
  scale_colour_manual(values = c("Any call attempt logged" = orange, "Tutoring completed" = blue)) +
  scale_x_continuous(breaks = 1:6) +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1)) +
  labs(
    title = "Weekly tutoring completion fell from 87% to 55%",
    subtitle = "Share of students with a valid baseline assessment",
    caption = paste0("Baseline cohort, N = ", nrow(baseline_cohort))
  ) +
  theme_youth() +
  theme(legend.position = "top")

ggsave(
  file.path(figure_output_dir, "03_weekly_fidelity.png"),
  weekly_plot,
  width = 10,
  height = 5.8,
  dpi = 300,
  bg = "white"
)

weeks_plot <- ggplot(
  tutoring_weeks_distribution,
  aes(x = factor(tutoring_weeks_completed), y = rate)
) +
  geom_col(width = 0.65, fill = teal) +
  geom_text(
    aes(label = percent(rate, accuracy = 0.1)),
    vjust = -0.4,
    size = 3.4,
    colour = dark_text
  ) +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 0.62)) +
  labs(
    title = "Just over half of the baseline cohort completed all six weeks",
    subtitle = "Number of distinct weeks with a successful tutoring submission",
    caption = paste0("Baseline cohort, N = ", nrow(baseline_cohort))
  ) +
  theme_youth()

ggsave(
  file.path(figure_output_dir, "04_tutoring_weeks_completed.png"),
  weeks_plot,
  width = 10,
  height = 5.8,
  dpi = 300,
  bg = "white"
)

followup_plot <- ggplot(
  endline_followup_by_baseline_level,
  aes(x = baseline_level_label, y = endline_rate)
) +
  geom_col(width = 0.62, fill = orange) +
  geom_text(
    aes(label = percent(endline_rate, accuracy = 0.1)),
    vjust = -0.35,
    size = 3.4,
    colour = dark_text
  ) +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 0.76)) +
  labs(
    title = "Endline assessment coverage varied with baseline level",
    subtitle = "Valid endline assessments as a share of each baseline group",
    caption = paste0("Baseline cohort, N = ", nrow(baseline_cohort))
  ) +
  theme_youth() +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

ggsave(
  file.path(figure_output_dir, "05_endline_followup.png"),
  followup_plot,
  width = 10,
  height = 5.8,
  dpi = 300,
  bg = "white"
)


## 10. Console summary ---------------------------------------------------------

cat("\nYouth Impact RA analysis completed.\n")
cat("Student-level rows:", nrow(student_level), "\n")
cat("Baseline-assessed cohort:", nrow(baseline_cohort), "\n")
cat("Paired baseline-endline sample:", paired_n, "\n")
cat(
  "Mastering division at endline:",
  percent(kpi_summary$rate[kpi_summary$metric == "Mastering division at endline"], accuracy = 0.1),
  "\n"
)
cat(
  "No operation mastered at endline:",
  percent(kpi_summary$rate[kpi_summary$metric == "Not mastering any operation at endline"], accuracy = 0.1),
  "\n"
)
cat(
  "Learned at least one new operation:",
  percent(kpi_summary$rate[kpi_summary$metric == "Learned at least one new operation"], accuracy = 0.1),
  "\n"
)
cat(
  "Sensitivity using latest valid baseline assessment:",
  percent(latest_baseline_sensitivity$learned_rate, accuracy = 0.1),
  "\n"
)
