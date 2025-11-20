options(repos = c(CRAN = "https://cloud.r-project.org"))

required_packages <- c("dplyr", "tidyr", "broom")
installed <- rownames(installed.packages())
for (pkg in required_packages) {
  if (!(pkg %in% installed)) {
    install.packages(pkg)
  }
  library(pkg, character.only = TRUE, quietly = TRUE)
}

stage_levels <- c("BL", "STR", "LTR")
d <- read.csv("dataset.csv", header = TRUE, stringsAsFactors = FALSE)

participant_stage_summary <- d %>%
  dplyr::filter(Stage %in% stage_levels) %>%
  dplyr::group_by(ParticipantID, Stage, Group) %>%
  dplyr::summarise(AveAbsError = mean(AbsError), .groups = "drop")

cat("Participants per group in each stage:\n")
print(
  participant_stage_summary %>%
    dplyr::count(Stage, Group, name = "n_participants")
)

# Compare Control vs Experimental at each stage
stage_tests <- participant_stage_summary %>%
  dplyr::group_by(Stage) %>%
  dplyr::group_modify(function(.x, .y) {
    control <- .x %>% dplyr::filter(Group == "C") %>% dplyr::pull(AveAbsError)
    experimental <- .x %>% dplyr::filter(Group == "E") %>% dplyr::pull(AveAbsError)
    if (length(control) == 0 || length(experimental) == 0) {
      return(
        tibble::tibble(
          control_mean = NA_real_,
          experimental_mean = NA_real_,
          mean_difference = NA_real_,
          p_value = NA_real_,
          conf_low = NA_real_,
          conf_high = NA_real_
        )
      )
    }
    t_res <- t.test(control, experimental)
    tibble::tibble(
      control_mean = mean(control),
      experimental_mean = mean(experimental),
      mean_difference = mean(experimental) - mean(control),
      p_value = t_res$p.value,
      conf_low = t_res$conf.int[1],
      conf_high = t_res$conf.int[2]
    )
  }) %>%
  dplyr::ungroup()

cat("\nStage-level Control vs Experimental comparisons:\n")
print(stage_tests)

# Prepare wide table for learning effects
avg_wide <- participant_stage_summary %>%
  dplyr::select(ParticipantID, Group, Stage, AveAbsError) %>%
  tidyr::pivot_wider(
    names_from = Stage,
    values_from = AveAbsError
  )

learning_effects <- avg_wide %>%
  dplyr::mutate(
    LearningEffect_STR = BL - STR,
    LearningEffect_LTR = BL - LTR
  )

cat("\nSummary of average absolute errors by group and stage:\n")
print(
  participant_stage_summary %>%
    dplyr::group_by(Stage, Group) %>%
    dplyr::summarise(
      mean_AveAbsError = mean(AveAbsError, na.rm = TRUE),
      sd_AveAbsError = sd(AveAbsError, na.rm = TRUE),
      .groups = "drop_last"
    )
)

# Paired tests: did average error drop relative to baseline?
paired_short_data <- avg_wide %>% dplyr::filter(!is.na(BL), !is.na(STR))
paired_long_data <- avg_wide %>% dplyr::filter(!is.na(BL), !is.na(LTR))

paired_short <- t.test(paired_short_data$BL, paired_short_data$STR, paired = TRUE)
paired_long <- t.test(paired_long_data$BL, paired_long_data$LTR, paired = TRUE)

cat("\nPaired BL vs STR test (all participants with both stages):\n")
print(paired_short)

cat("\nPaired BL vs LTR test (all participants with both stages):\n")
print(paired_long)

# Do learning effects differ between groups?
learning_tests <- learning_effects %>%
  dplyr::select(ParticipantID, Group, LearningEffect_STR, LearningEffect_LTR) %>%
  tidyr::pivot_longer(
    cols = starts_with("LearningEffect"),
    names_to = "Metric",
    values_to = "Effect"
  ) %>%
  dplyr::group_by(Metric) %>%
  dplyr::group_modify(function(.x, .y) {
    control <- .x %>% dplyr::filter(Group == "C") %>% dplyr::pull(Effect)
    experimental <- .x %>% dplyr::filter(Group == "E") %>% dplyr::pull(Effect)
    control <- control[!is.na(control)]
    experimental <- experimental[!is.na(experimental)]
    if (length(control) == 0 || length(experimental) == 0) {
      return(
        tibble::tibble(
          control_mean = NA_real_,
          experimental_mean = NA_real_,
          mean_difference = NA_real_,
          p_value = NA_real_,
          conf_low = NA_real_,
          conf_high = NA_real_
        )
      )
    }
    t_res <- t.test(control, experimental)
    tibble::tibble(
      control_mean = mean(control),
      experimental_mean = mean(experimental),
      mean_difference = mean(experimental) - mean(control),
      p_value = t_res$p.value,
      conf_low = t_res$conf.int[1],
      conf_high = t_res$conf.int[2]
    )
  }) %>%
  dplyr::ungroup()

cat("\nLearning effect comparisons between groups:\n")
print(learning_tests)

cat("\nLearning effect descriptive statistics:\n")
print(
  learning_effects %>%
    tidyr::pivot_longer(
      cols = c(LearningEffect_STR, LearningEffect_LTR),
      names_to = "Metric",
      values_to = "Effect"
    ) %>%
    dplyr::group_by(Metric, Group) %>%
    dplyr::summarise(
      mean_effect = mean(Effect, na.rm = TRUE),
      sd_effect = sd(Effect, na.rm = TRUE),
      .groups = "drop_last"
    )
)

cat("\nAnalysis complete.\n")

