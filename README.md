# Youth Impact Research Associate task

This repository contains a reproducible R workflow and a seven-slide findings deck for Youth Impact's mEducation hiring exercise. The analysis cleans three SurveyCTO call-attempt exports, reduces repeated submissions using documented rules, and creates one analytic row per student across sensitization, six implementation weeks, and endline.

## Exercise brief

Youth Impact's mEducation program delivers phone-based tutoring to primary-school students in the Philippines. Teachers first conduct a sensitization call with a baseline assessment, then six weekly tutoring calls, and finally an endline assessment. Each call attempt is logged as a separate SurveyCTO submission.

The supplied materials include SurveyCTO form definitions and de-identified exports for all three phases. The requested work is to:

- clean repeated submissions and merge one row per student across all phases;
- produce the three required learning KPIs plus useful outcome and implementation-fidelity graphs; and
- present findings, judgment calls, data issues, and data-system recommendations in a six-to-eight-slide deck.

## Deliverables

- [`RA Data Analysis Task.R`](RA%20Data%20Analysis%20Task.R): cleaning, merging, quality checks, KPI construction, aggregate tables, and figures.
- [`deliverables/Youth_Impact_RA_Task_Rachit_Haritwal.pptx`](deliverables/Youth_Impact_RA_Task_Rachit_Haritwal.pptx): seven-slide findings and process deck.
- [`AI_USE.md`](AI_USE.md): disclosure of how AI was used. The shareable conversation link should accompany the submission separately.

## Run the analysis

Use R 4.2 or newer. From the repository root:

```r
install.packages(c(
  "haven", "dplyr", "tidyr", "ggplot2", "readr",
  "stringr", "scales", "purrr", "tibble"
))
source("RA Data Analysis Task.R")
```

The script accepts the three `.dta` inputs in either the repository root or `data/raw/`:

- `01_sensitization_data.dta`
- `02_implementation_data.dta`
- `03_endline_data.dta`

It writes the one-row-per-student dataset to `outputs/data/`, aggregate quality and KPI tables to `outputs/tables/`, and five figures to `outputs/figures/`. Generated row-level outputs are gitignored.

## Analytic decisions

The stable cross-phase key is `hhid`. A SurveyCTO `key` identifies a submission, not a student.

- **Sensitization and endline:** select the earliest valid assessment for each student. This preserves the timing of baseline/endline and avoids choosing on the observed score. If no valid assessment exists, retain the latest call attempt so its operational status is not lost.
- **Implementation:** within each student-week, select the earliest successful tutoring submission. If no attempt succeeded, retain the latest attempt. Keep submission counts and conflict flags in either case.
- **Missing weeks:** distinguish no logged attempt (`attempted = 0`) from an attempted but unsuccessful call. Outcome fields remain missing when no tutoring occurred.
- **Merge:** outer-join all observed IDs to retain implementation-only records. Cohort-specific rates use explicit eligibility flags.
- **Learning KPIs:** use students with valid assessments at both baseline and endline (`N = 240`). "Learned a new operation" means the endline numeracy level is strictly above baseline.

## Headline results

Among the paired assessment sample (`N = 240`):

| KPI | Numerator | Result |
|---|---:|---:|
| Mastering division at endline | 126 | 52.5% |
| Not mastering any operation at endline | 17 | 7.1% |
| Learned at least one new operation | 188 | 78.3% |

In the same students, division mastery was 10.8% at baseline and 37.9% mastered no operation. Weekly tutoring completion among all 431 students with a valid baseline fell from 86.5% in week 1 to 55.5% in week 6; 238 students (55.2%) completed all six weeks.

## Interpretation and limitations

These are descriptive pre/post changes, not causal impact estimates: there is no comparison group. Only 240 of 431 baseline-assessed students (55.7%) have a valid endline assessment, so attrition may affect the observed learning distribution. Follow-up also varies by baseline level. Results should therefore be read as outcomes for the paired assessment sample, alongside implementation and follow-up coverage.

The earliest-versus-latest valid baseline sensitivity check leaves the learned-new-operation estimate unchanged at 78.3%.
