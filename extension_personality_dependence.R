options(repos = c(CRAN = "https://cloud.r-project.org"))

install.packages("Hmisc")
install.packages("corrplot")
install.packages("BMA")

library("Hmisc")
library("corrplot")
library("BMA")

# Extension (PDF suggestion): study dependence between personality traits.
# We do this with:
# 1) correlation matrices (with p-values)
# 2) BMA (bicreg) to see which traits best predict each other

d <- read.csv("dataset.csv", header = TRUE)

# Build per-participant, per-stage dataset (traits repeated by stage, but same per participant)
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

# Correlations by stage
create_cor_matrix <- function(data, stage_name) {
  cat("\n", stage_name, "\n", sep = "")
  traits <- data[, c("AC", "FS", "TC", "TB")]
  cor_test <- rcorr(as.matrix(traits))
  cat("\nCorrelations:\n")
  print(round(cor_test$r, 3))
  cat("\nP-values:\n")
  print(round(cor_test$P, 3))

  # Optional visualization
  corrplot(cor_test$r, method = "color", type = "upper", tl.col = "black",
           title = paste("Trait correlations -", stage_name), mar = c(0,0,2,0))
  return(list(matrix = cor_test$r, pvalues = cor_test$P))
}

bl_data <- results[results$Stage == "BL", c("AC", "FS", "TC", "TB")]
str_data <- results[results$Stage == "STR", c("AC", "FS", "TC", "TB")]
ltr_data <- results[results$Stage == "LTR", c("AC", "FS", "TC", "TB")]

bl_cor <- create_cor_matrix(bl_data, "Baseline")
str_cor <- create_cor_matrix(str_data, "Short-Term")
ltr_cor <- create_cor_matrix(ltr_data, "Long-Term")

# Since traits are constant per participant, keep unique rows for BMA
personality_data <- unique(results[, c("AC", "FS", "TC", "TB")])

cat("\nBMA (bicreg) summaries:\n")

bma_ac <- bicreg(
  x = personality_data[, c("FS", "TC", "TB")],
  y = personality_data$AC,
  strict = FALSE,
  OR = 20
)
bma_tc <- bicreg(
  x = personality_data[, c("FS", "AC", "TB")],
  y = personality_data$TC,
  strict = FALSE,
  OR = 20
)
bma_fs <- bicreg(
  x = personality_data[, c("AC", "TC", "TB")],
  y = personality_data$FS,
  strict = FALSE,
  OR = 20
)
bma_tb <- bicreg(
  x = personality_data[, c("FS", "AC", "TC")],
  y = personality_data$TB,
  strict = FALSE,
  OR = 20
)

summary(bma_ac)
summary(bma_tc)
summary(bma_fs)
summary(bma_tb)


