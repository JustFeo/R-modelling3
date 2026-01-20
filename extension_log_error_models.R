options(repos = c(CRAN = "https://cloud.r-project.org"))

install.packages("car")
install.packages("MASS")

library("car")
library("MASS")

# Extension: log10(|Error|) models (multiplicative effects)
# -----------------------------------------------
# The Week 3 models treat changes in AveAbsError as additive.
# Since AbsError is strictly positive, we can consider multiplicative
# effects by working with log10(AbsError). Here we:
#   1) Build per-participant, per-stage averages of AbsError and log10(AbsError).
#   2) For each stage (BL, STR, LTR) fit:
#        (a) The original linear model:
#            AveAbsError ~ AC + FS + TC + TB + GroupE + interactions
#        (b) A log model:
#            LogAveAbsError ~ AC + FS + TC + TB + GroupE + interactions
#   3) Compare R^2 and AIC for the two models and inspect diagnostics.

d <- read.csv("dataset.csv", header = TRUE)

# Build per-participant, per-stage data with:
#   - AveAbsError: mean(AbsError)
#   - LogAveAbsError: mean(log10(AbsError))
results <- data.frame()
for (participant in unique(d$ParticipantID)) {
  AC <- unique(d$Achiever[d$ParticipantID == participant])[1]
  FS <- unique(d$FreeSpirit[d$ParticipantID == participant])[1]
  TC <- unique(d$TransformOfChallenge[d$ParticipantID == participant])[1]
  TB <- unique(d$TransformOfBorendom[d$ParticipantID == participant])[1]
  Group <- unique(d$Group[d$ParticipantID == participant])[1]

  for (stage in c("BL", "STR", "LTR")) {
    data_subset <- d[d$ParticipantID == participant & d$Stage == stage, ]
    ave_error <- mean(data_subset$AbsError)
    # log10 of each AbsError, then average
    log_ave_error <- mean(log10(data_subset$AbsError))

    results <- rbind(results, data.frame(
      ParticipantID = participant,
      Group = Group,
      Stage = stage,
      AveAbsError = ave_error,
      LogAveAbsError = log_ave_error,
      AC = AC, FS = FS, TC = TC, TB = TB
    ))
  }
}

compare_models <- function(stage_name) {
  cat("\n====================\n")
  cat("Stage:", stage_name, "\n")
  cat("====================\n\n")

  stage_data <- results[results$Stage == stage_name, ]
  stage_data$GroupE <- ifelse(stage_data$Group == "E", 1, 0)

  # Original-scale model
  model_abs <- lm(
    AveAbsError ~ AC + FS + TC + TB +
      GroupE + GroupE:AC + GroupE:FS + GroupE:TC + GroupE:TB,
    data = stage_data
  )

  # Log-scale model
  model_log <- lm(
    LogAveAbsError ~ AC + FS + TC + TB +
      GroupE + GroupE:AC + GroupE:FS + GroupE:TC + GroupE:TB,
    data = stage_data
  )

  cat("Original-scale model (AveAbsError):\n")
  cat("  R^2:", summary(model_abs)$r.squared, "\n")
  cat("  Adj R^2:", summary(model_abs)$adj.r.squared, "\n")
  cat("  AIC:", AIC(model_abs), "\n\n")

  cat("Log-scale model (LogAveAbsError):\n")
  cat("  R^2:", summary(model_log)$r.squared, "\n")
  cat("  Adj R^2:", summary(model_log)$adj.r.squared, "\n")
  cat("  AIC:", AIC(model_log), "\n\n")

  cat("Original-scale model summary:\n")
  print(summary(model_abs))
  cat("\nLog-scale model summary:\n")
  print(summary(model_log))

  # Diagnostic plots for visual comparison
  par(mfrow = c(2, 2))
  plot(model_abs, main = paste("Abs scale diagnostics -", stage_name))
  plot(model_log, main = paste("Log scale diagnostics -", stage_name))
  par(mfrow = c(1, 1))
}

for (s in c("BL", "STR", "LTR")) {
  compare_models(s)
}

cat("\nLog-error extension analysis complete.\n")

