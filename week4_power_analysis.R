options(repos = c(CRAN = "https://cloud.r-project.org"))

install.packages("pwr")

library("pwr")

# Week 4 (PDF): power analysis for detecting differences between
# Control and Experimental groups with at least 80% probability.

d <- read.csv("dataset.csv", header = TRUE)

# Build wide_data with BL/STR/LTR per participant (same definition as Week 2)
results <- data.frame()
for (participant in unique(d$ParticipantID)) {
  for (stage in c("BL", "STR", "LTR")) {
    data_subset <- d[d$ParticipantID == participant & d$Stage == stage, ]
    ave_error <- mean(data_subset$AbsError)

    results <- rbind(results, data.frame(
      ParticipantID = participant,
      Group = data_subset$Group[1],
      Stage = stage,
      AveAbsError = ave_error
    ))
  }
}

wide_data <- data.frame()
for (participant in unique(results$ParticipantID)) {
  data_subset <- results[results$ParticipantID == participant, ]
  wide_data <- rbind(wide_data, data.frame(
    ParticipantID = participant,
    Group = data_subset$Group[1],
    BL = data_subset$AveAbsError[data_subset$Stage == "BL"],
    STR = data_subset$AveAbsError[data_subset$Stage == "STR"],
    LTR = data_subset$AveAbsError[data_subset$Stage == "LTR"]
  ))
}

wide_data$Learn_STR <- wide_data$BL - wide_data$STR
wide_data$Learn_LTR <- wide_data$BL - wide_data$LTR

# Cohen's d helpers
cohens_d_between <- function(x1, x2) {
  n1 <- length(x1)
  n2 <- length(x2)
  m1 <- mean(x1)
  m2 <- mean(x2)
  s1 <- sd(x1)
  s2 <- sd(x2)
  pooled_sd <- sqrt(((n1 - 1)*s1^2 + (n2 - 1)*s2^2) / (n1 + n2 - 2))
  (m1 - m2) / pooled_sd
}

cohens_d_paired <- function(diffs) {
  mean(diffs) / sd(diffs)
}

# Effect sizes (observed)
d_learn_str <- cohens_d_between(
  wide_data$Learn_STR[wide_data$Group == "C"],
  wide_data$Learn_STR[wide_data$Group == "E"]
)
d_learn_ltr <- cohens_d_between(
  wide_data$Learn_LTR[wide_data$Group == "C"],
  wide_data$Learn_LTR[wide_data$Group == "E"]
)

d_str <- cohens_d_paired(wide_data$BL - wide_data$STR)
d_ltr <- cohens_d_paired(wide_data$BL - wide_data$LTR)

cat("Observed Cohen's d (absolute values):\n")
cat("Robot-assistance (Learn_STR, C vs E):", abs(d_learn_str), "\n")
cat("Robot-assistance (Learn_LTR, C vs E):", abs(d_learn_ltr), "\n")
cat("Training (paired BL-STR):", abs(d_str), "\n")
cat("Training (paired BL-LTR):", abs(d_ltr), "\n\n")

# Required sample sizes for 80% power (choose a target effect size)
target_d <- 0.8
required_two_sample <- pwr.t.test(power = 0.80, d = target_d, sig.level = 0.05, type = "two.sample")
required_paired <- pwr.t.test(power = 0.80, d = target_d, sig.level = 0.05, type = "paired")

cat("Required sample size for 80% power (target d =", target_d, "):\n")
cat(ceiling(required_two_sample$n), "participants PER GROUP (two-sample; robot-assistance)\n")
cat("Total:", ceiling(required_two_sample$n) * 2, "participants\n")
cat(ceiling(required_paired$n), "participants (paired; training)\n\n")

# Current power with your sample sizes using observed effects
n_per_group <- sum(wide_data$Group == "C")
n_total <- nrow(wide_data)

power1 <- pwr.t.test(n = n_per_group, d = abs(d_learn_str), sig.level = 0.05, type = "two.sample")
power2 <- pwr.t.test(n = n_per_group, d = abs(d_learn_ltr), sig.level = 0.05, type = "two.sample")
power3 <- pwr.t.test(n = n_total, d = abs(d_str), sig.level = 0.05, type = "paired")
power4 <- pwr.t.test(n = n_total, d = abs(d_ltr), sig.level = 0.05, type = "paired")

cat("Current power using observed effect sizes:\n")
cat("Robot-assistance STR (two-sample):", round(power1$power * 100, 1), "%\n")
cat("Robot-assistance LTR (two-sample):", round(power2$power * 100, 1), "%\n")
cat("Training STR (paired):", round(power3$power * 100, 1), "%\n")
cat("Training LTR (paired):", round(power4$power * 100, 1), "%\n")


