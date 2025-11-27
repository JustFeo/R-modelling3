options(repos = c(CRAN = "https://cloud.r-project.org"))

d <- read.csv("dataset.csv", header = TRUE, stringsAsFactors = FALSE)

stages <- c("BL", "STR", "LTR")

stage_data <- d[d$Stage %in% stages, ]

participant_stage <- aggregate(AbsError ~ ParticipantID + Stage + Group, data = stage_data, FUN = mean)

names(participant_stage)[names(participant_stage) == "AbsError"] <- "AveAbsError"

cat("Participants per group in each stage:\n\n")

print(table(participant_stage$Stage, participant_stage$Group))

cat("\nStage-level Control vs Experimental comparisons:\n\n")

for (stage in stages) {
  stage_subset <- participant_stage[participant_stage$Stage == stage, ]
  control_values <- stage_subset$AveAbsError[stage_subset$Group == "C"]
  experimental_values <- stage_subset$AveAbsError[stage_subset$Group == "E"]
  if (length(control_values) > 1 && length(experimental_values) > 1) {
    test_res <- t.test(control_values, experimental_values)
    cat("Stage:", stage, "\n")
    cat("  Control mean:", mean(control_values), "\n")
    cat("  Experimental mean:", mean(experimental_values), "\n")
    cat("  Mean difference (Experimental - Control):", mean(experimental_values) - mean(control_values), "\n")
    cat("  p-value:", test_res$p.value, "\n\n")
  }
}

cat("Summary of average absolute errors by group and stage:\n\n")

for (stage in stages) {
  stage_subset <- participant_stage[participant_stage$Stage == stage, ]
  for (grp in c("C", "E")) {
    values <- stage_subset$AveAbsError[stage_subset$Group == grp]
    cat("Stage:", stage, "- Group:", grp, "\n")
    cat("  Mean:", mean(values), "\n")
    cat("  SD:", sd(values), "\n\n")
  }
}

avg_wide <- reshape(participant_stage,
                    idvar = c("ParticipantID", "Group"),
                    timevar = "Stage",
                    direction = "wide")

avg_wide$LearningEffect_STR <- avg_wide$AveAbsError.BL - avg_wide$AveAbsError.STR

avg_wide$LearningEffect_LTR <- avg_wide$AveAbsError.BL - avg_wide$AveAbsError.LTR

paired_short <- avg_wide[!is.na(avg_wide$AveAbsError.BL) & !is.na(avg_wide$AveAbsError.STR), ]

paired_long <- avg_wide[!is.na(avg_wide$AveAbsError.BL) & !is.na(avg_wide$AveAbsError.LTR), ]

cat("Paired BL vs STR test (participants with both stages):\n\n")

print(t.test(paired_short$AveAbsError.BL, paired_short$AveAbsError.STR, paired = TRUE))

cat("\nPaired BL vs LTR test (participants with both stages):\n\n")

print(t.test(paired_long$AveAbsError.BL, paired_long$AveAbsError.LTR, paired = TRUE))

learning_metrics <- c("LearningEffect_STR", "LearningEffect_LTR")

cat("\nLearning effect comparisons between groups:\n\n")

for (metric in learning_metrics) {
  metric_values <- avg_wide[!is.na(avg_wide[[metric]]), ]
  control_values <- metric_values[[metric]][metric_values$Group == "C"]
  experimental_values <- metric_values[[metric]][metric_values$Group == "E"]
  if (length(control_values) > 1 && length(experimental_values) > 1) {
    test_res <- t.test(control_values, experimental_values)
    cat(metric, "\n")
    cat("  Control mean:", mean(control_values), "\n")
    cat("  Experimental mean:", mean(experimental_values), "\n")
    cat("  Mean difference (Experimental - Control):", mean(experimental_values) - mean(control_values), "\n")
    cat("  p-value:", test_res$p.value, "\n\n")
  }
}

cat("Learning effect descriptive statistics:\n\n")

for (metric in learning_metrics) {
  metric_values <- avg_wide[!is.na(avg_wide[[metric]]), ]
  for (grp in c("C", "E")) {
    values <- metric_values[[metric]][metric_values$Group == grp]
    cat(metric, "- Group:", grp, "\n")
    cat("  Mean:", mean(values), "\n")
    cat("  SD:", sd(values), "\n\n")
  }
}

cat("Analysis complete.\n")

