# Required packages for the Week 1 analysis
options(repos = c(CRAN = "https://cloud.r-project.org"))
install.packages("carData")
install.packages("car")
install.packages("MASS")
install.packages("fitdistrplus")

library("carData")
library("MASS")
library("fitdistrplus")
library("car")

# Load the dataset that lives in the workspace root
d <- read.csv("dataset.csv", header = TRUE)

# Basic histograms of the four traits
par(mfrow = c(2, 2))
hist(d$Achiever, col = "yellow", main = "Histogram of AC")
hist(d$FreeSpirit, col = "green", main = "Histogram of FS")
hist(d$TransformOfChallenge, col = "blue", main = "Histogram of TC")
hist(d$TransformOfBorendom, col = "coral", main = "Histogram of TB")
par(mfrow = c(1, 1))

# Distribution fitting and Q-Q plots
par(mfrow = c(2, 2))

normal <- fitdist(d$FreeSpirit, "norm")
qqPlot(
  d$FreeSpirit,
  distribution = "norm",
  main = "Normal Q-Q Plot of FS"
)
print(normal)

uniform <- fitdist(d$TransformOfBorendom, "unif")
qqPlot(
  d$TransformOfBorendom,
  distribution = "unif",
  main = "Uniform Q-Q Plot of TB"
)
print(uniform)

d$Achiever_normalized <- (d$Achiever - min(d$Achiever)) /
  (max(d$Achiever) - min(d$Achiever))
beta1 <- fitdist(d$Achiever_normalized, "beta")
qqPlot(
  d$Achiever_normalized,
  distribution = "beta",
  shape1 = beta1$estimate["shape1"],
  shape2 = beta1$estimate["shape2"],
  main = "Beta Q-Q Plot of AC"
)
print(beta1)

d$TransformOfChallenge_normalized <- (d$TransformOfChallenge - min(d$TransformOfChallenge)) /
  (max(d$TransformOfChallenge) - min(d$TransformOfChallenge))
beta2 <- fitdist(d$TransformOfChallenge_normalized, "beta")
qqPlot(
  d$TransformOfChallenge_normalized,
  distribution = "beta",
  shape1 = beta2$estimate["shape1"],
  shape2 = beta2$estimate["shape2"],
  main = "Beta Q-Q Plot of TC"
)
print(beta2)

# Kolmogorov-Smirnov tests for goodness-of-fit
ks_fs <- ks.test(
  d$FreeSpirit,
  "pnorm",
  mean = normal$estimate["mean"],
  sd = normal$estimate["sd"]
)
cat("\nKolmogorov-Smirnov test for FreeSpirit:\n")
print(ks_fs)

ks_tb <- ks.test(
  d$TransformOfBorendom,
  "punif",
  min = uniform$estimate["min"],
  max = uniform$estimate["max"]
)
cat("Kolmogorov-Smirnov test for TransformOfBorendom (uniform):\n")
print(ks_tb)

ks_ac <- ks.test(
  d$Achiever_normalized,
  "pbeta",
  shape1 = beta1$estimate["shape1"],
  shape2 = beta1$estimate["shape2"]
)
cat("Kolmogorov-Smirnov test for Achiever (beta):\n")
print(ks_ac)

ks_tc <- ks.test(
  d$TransformOfChallenge_normalized,
  "pbeta",
  shape1 = beta2$estimate["shape1"],
  shape2 = beta2$estimate["shape2"]
)
cat("Kolmogorov-Smirnov test for TransformOfChallenge (beta):\n")
print(ks_tc)

par(mfrow = c(1, 1))

