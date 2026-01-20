options(repos = c(CRAN = "https://cloud.r-project.org"))

install.packages("car")
install.packages("MASS")

library("car")
library("MASS")

# Week 3 (PDF): regression models explaining AveAbsError using personality traits,
# and how those relationships change for the Experimental (guided) group.

d <- read.csv("dataset.csv", header = TRUE)

# Build a per-participant, per-stage dataset:
# one row per participant per stage with AveAbsError and traits.
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

    results <- rbind(results, data.frame(
      ParticipantID = participant,
      Group = Group,
      Stage = stage,
      AveAbsError = ave_error,
      AC = AC, FS = FS, TC = TC, TB = TB
    ))
  }
}

for (stage in c("BL", "STR", "LTR")) {
  cat("\n====================\n")
  cat("Stage:", stage, "\n")
  cat("====================\n\n")

  stage_data <- results[results$Stage == stage, ]

  # Model 1 (PDF): AveAbsError ~ AC + FS + TC + TB
  model1 <- lm(AveAbsError ~ AC + FS + TC + TB, data = stage_data)
  cat("Model 1: AveAbsError ~ AC + FS + TC + TB\n")
  print(summary(model1))

  # Model 2 (PDF): add Group effect + trait interactions with Group
  stage_data$GroupE <- ifelse(stage_data$Group == "E", 1, 0)
  model2 <- lm(
    AveAbsError ~ AC + FS + TC + TB +
      GroupE + GroupE:AC + GroupE:FS + GroupE:TC + GroupE:TB,
    data = stage_data
  )
  cat("\nModel 2: add GroupE and GroupE:Trait interaction terms\n")
  print(summary(model2))

  # Basic diagnostic plots for each stage (optional but useful)
  par(mfrow = c(2, 2))
  plot(model1, main = paste("Diagnostics model1 -", stage))
  plot(model2, main = paste("Diagnostics model2 -", stage))
  par(mfrow = c(1, 1))
}
