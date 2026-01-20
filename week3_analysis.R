options(repos = c(CRAN = "https://cloud.r-project.org"))

install.packages("carData")
install.packages("car")
install.packages("MASS")
install.packages("fitdistrplus")
install.packages("pwr")
install.packages("BMA")
install.packages("Hmisc")
install.packages("corrplot")
install.packages("xtable")

library("carData")
library("MASS")
library("fitdistrplus")
library("car")
library("pwr")
library("BMA")
library("Hmisc")
library("corrplot")
library("xtable")

d<-read.csv("dataset.csv", header=TRUE)


par(mfrow= c(2,2))
unique_personality <- aggregate(. ~ ParticipantID, 
                                data = d[, c("ParticipantID", "Achiever", "FreeSpirit", 
                                             "TransformOfChallenge", "TransformOfBorendom")],
                                FUN = function(x) x[1])


par(mfrow = c(2,2))
for(i in 1:4) {
  trait <- c("Achiever", "FreeSpirit", "TransformOfChallenge", "TransformOfBorendom")[i]
  colors <- c("yellow", "green", "blue", "coral")[i]
  
  hist(unique_personality[[trait]], 
       col = colors, 
       main = paste(trait, "Score"),
       xlab = paste(trait, "Score (0-100)"),
       ylab = "Number of Participants",
       breaks = 8,
       xlim = c(0, 100))
}
par(mfrow = c(2,2))

FS_unique <- unique_personality$FreeSpirit
AC_unique <- unique_personality$Achiever
TC_unique <- unique_personality$TransformOfChallenge
TB_unique <- unique_personality$TransformOfBorendom

FreeSpirit_normalized <- (FS_unique - min(FS_unique)) / (max(FS_unique) - min(FS_unique))
beta1 <- fitdist(FreeSpirit_normalized, "beta")
qqPlot(FreeSpirit_normalized, distribution = "beta",
       shape1 = beta1$estimate["shape1"],
       shape2 = beta1$estimate["shape2"],
       main = "Beta Q-Q Plot of FS ")
print(beta1)

normal<- fitdist(TB_unique, "norm")
qqPlot(TB_unique, distribution = "norm",
       main = "Normal Q-Q Plot of TB ")
print(normal)

weibull1<- fitdist(AC_unique, "weibull")
normal1<-fitdist(AC_unique, "norm")
qqPlot(AC_unique, distribution = "weibull", 
       shape = weibull1$estimate["shape"],
       scale = weibull1$estimate["scale"],
       main = "Weibull Q-Q Plot of AC ")
print(weibull1)

TransformOfChallenge_normalized <- (TC_unique - min(TC_unique)) / (max(TC_unique) - min(TC_unique))
beta2 <- fitdist(TransformOfChallenge_normalized, "beta")
qqPlot(TransformOfChallenge_normalized, distribution = "beta", 
       shape1 = beta2$estimate["shape1"],
       shape2 = beta2$estimate["shape2"],
       main = "Beta Q-Q Plot of TC ")
print(beta2)

ks_fs <- ks.test(FreeSpirit_normalized, "pbeta", 
                 shape1 = beta1$estimate["shape1"], 
                 shape2 = beta1$estimate["shape2"])
cat("\nKolmogorov-Smirnov test for FreeSpirit (Unique Values):\n")
print(ks_fs)

ks_tb <- ks.test(TB_unique, "pnorm", 
                 mean = normal$estimate["mean"], 
                 sd = normal$estimate["sd"])
cat("Kolmogorov-Smirnov test for TransformOfBorendom (normal, Unique Values):\n")
print(ks_tb)

weibull1<-fitdist(AC_unique, "weibull")
ks_ac <- ks.test(AC_unique, "pweibull", 
                 shape = weibull1$estimate["shape"], 
                 scale = weibull1$estimate["scale"])
cat("Kolmogorov-Smirnov test for Achiever (Weibull, Unique Values):\n")
print(ks_ac)

ks_tc <- ks.test(TransformOfChallenge_normalized, "pbeta",
                 shape1 = beta2$estimate["shape1"],
                 shape2 = beta2$estimate["shape2"])
cat("Kolmogorov-Smirnov test for TransformOfChallenge (beta, Unique Values):\n")
print(ks_tc)

results <- data.frame()
for(participant in unique(d$ParticipantID)) {
  for(stage in c("BL", "STR", "LTR")) {
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
for(participant in unique(results$ParticipantID)) {
  data_subset <- results[results$ParticipantID == participant, ]
  wide_data <- rbind(wide_data, data.frame(
    ParticipantID = participant,
    Group = data_subset$Group[1],
    BL = data_subset$AveAbsError[data_subset$Stage == "BL"],
    STR = data_subset$AveAbsError[data_subset$Stage == "STR"],
    LTR = data_subset$AveAbsError[data_subset$Stage == "LTR"]
  ))
}

normalBL<-fitdist(wide_data$BL, "norm")
print(normalBL)

cat(mean(wide_data$BL))
cat(mean(wide_data$STR))
cat(mean(wide_data$LTR))
wide_data$Learn_STR <- wide_data$BL - wide_data$STR
wide_data$Learn_LTR <- wide_data$BL - wide_data$LTR
cohen_d_BL_STR <- mean(wide_data$Learn_STR) / sd(wide_data$Learn_STR)
cohen_d_BL_LTR <- mean(wide_data$Learn_LTR) / sd(wide_data$Learn_LTR)
cat(cohen_d_BL_STR)
cat(cohen_d_BL_LTR)

cohens_d_learn <- function(control_learn, exp_learn) {
  
  n1 <- length(control_learn)
  n2 <- length(exp_learn)
  
  mean1 <- mean(control_learn)
  mean2 <- mean(exp_learn)
  
  sd1 <- sd(control_learn)
  sd2 <- sd(exp_learn)
  
  pooled_sd <- sqrt(((n1 - 1)*sd1^2 + (n2 - 1)*sd2^2) / (n1 + n2 - 2))
  
  # Cohen's d
  d <- (mean1 - mean2) / pooled_sd
  
  return(d)
}


d_learn_str <- cohens_d_learn(
  wide_data$Learn_STR[wide_data$Group == "C"],
  wide_data$Learn_STR[wide_data$Group == "E"]
)

d_learn_ltr <- cohens_d_learn(
  wide_data$Learn_LTR[wide_data$Group == "C"],
  wide_data$Learn_LTR[wide_data$Group == "E"]
)
diffs_str <- wide_data$BL - wide_data$STR
d_str <- mean(diffs_str) / sd(diffs_str) 

diffs_ltr <- wide_data$BL - wide_data$LTR
d_ltr <- mean(diffs_ltr) / sd(diffs_ltr)  

cat("Cohen's d for (Control vs Experimental):\n")
cat("STR Robot-assistance: d =", abs(round(d_learn_str, 3)), "\n")
cat("LTR Robot-assistance: d =", abs(round(d_learn_ltr, 3)), "\n")
cat("STR Training: d=", abs(round(d_str,3)),"\n")
cat("LTR Training d=", abs(round(d_ltr,3)), "\n")


cat("Group differences:\n")
cat("BL p-value:", t.test(wide_data$BL[wide_data$Group == "C"], wide_data$BL[wide_data$Group == "E"])$p.value, "\n")
cat("STR p-value:", t.test(wide_data$STR[wide_data$Group == "C"], wide_data$STR[wide_data$Group == "E"])$p.value, "\n")
cat("LTR p-value:", t.test(wide_data$LTR[wide_data$Group == "C"], wide_data$LTR[wide_data$Group == "E"])$p.value, "\n")

cat("\nDid error reduce?\n")
cat("BL vs STR p-value:", t.test(wide_data$BL, wide_data$STR, paired = TRUE)$p.value, "\n")
cat("BL vs LTR p-value:", t.test(wide_data$BL, wide_data$LTR, paired = TRUE)$p.value, "\n")

cat("\nLearning between groups:\n")
cat("STR learning p-value:", t.test(wide_data$Learn_STR[wide_data$Group == "C"], wide_data$Learn_STR[wide_data$Group == "E"])$p.value, "\n")
cat("LTR learning p-value:", t.test(wide_data$Learn_LTR[wide_data$Group == "C"], wide_data$Learn_LTR[wide_data$Group == "E"])$p.value, "\n")

print(mean(wide_data$Learn_STR[wide_data$Group == "C"]))
print(mean(wide_data$Learn_STR[wide_data$Group == "E"]))
print(mean(wide_data$Learn_LTR[wide_data$Group == "C"]))
print(mean(wide_data$Learn_LTR[wide_data$Group == "E"]))
results <- data.frame()
for(participant in unique(d$ParticipantID)) {
  for(stage in c("BL", "STR", "LTR")) {
    data_subset <- d[d$ParticipantID == participant & d$Stage == stage, ]
    ave_error <- mean(data_subset$AbsError)
    
    AC <- unique(d$Achiever[d$ParticipantID == participant])[1]
    FS <- unique(d$FreeSpirit[d$ParticipantID == participant])[1]
    TC <- unique(d$TransformOfChallenge[d$ParticipantID == participant])[1]
    TB <- unique(d$TransformOfBorendom[d$ParticipantID == participant])[1]
    Group <- unique(d$Group[d$ParticipantID == participant])[1]
    
    results <- rbind(results, data.frame(
      ParticipantID = participant,
      Group = Group,
      Stage = stage,
      AveAbsError = ave_error,
      AC = AC, FS = FS, TC = TC, TB = TB
    ))
  }
}

for(stage in c("BL", "STR", "LTR")) {
  stage_data <- results[results$Stage == stage, ]
  model1 <- lm(AveAbsError ~ AC + FS + TC + TB, data = stage_data)
  plot(model1)
  

  stage_data$GroupE <- ifelse(stage_data$Group == "E", 1, 0)
  model2 <- lm(AveAbsError ~ AC + FS + TC + TB + 
  GroupE + GroupE:AC + GroupE:FS + GroupE:TC + GroupE:TB, 
           data = stage_data)
  plot(model2)
  print(summary(model2))
  
  model3 <- lm(FS ~ AC,
               data = stage_data)
  plot(model3)
  print(summary(model3))
}

required_sample1 <- pwr.t.test(power = 0.80,
                              d = 0.8,  
                              sig.level = 0.05,
                              type = "two.sample")
required_sample2 <- pwr.t.test(power = 0.80,
                               d = 0.8,
                               sig.level = 0.05,
                               type="paired")

cat("Required sample size for 80% power:\n")
cat(ceiling(required_sample1$n), "participants PER GROUP\n for robot-assistance conclusion")
cat("Total:", ceiling(required_sample1$n) * 2, "participants\n")
cat(ceiling(required_sample2$n), "participants PER STAGE\n for training conclusion")
cat("Total:", ceiling(required_sample2$n), "participants\n")



cat("Control participants:", sum(wide_data$Group == "C"), "\n")
cat("Experimental participants:", sum(wide_data$Group == "E"), "\n")

power1 <- pwr.t.test(n = sum(wide_data$Group=="C"),  
                          d = d_learn_str,  
                          sig.level = 0.05,
                          type = "two.sample")
power2 <- pwr.t.test(n = sum(wide_data$Group=="C"),
                     d = d_learn_ltr,
                     sig.level = 0.05,
                     type = "two.sample")
power3 <- pwr.t.test(n = 100,
                     d = d_str,
                     sig.level = 0.05,
                     type = "paired")
power4<-pwr.t.test(n = 100,
                   d = d_ltr,
                   sig.level = 0.05,
                   type="paired")

cat("Current power with 50 participants per group for STR:", round(power1$power * 100, 1), "%\n")
cat("Current power with 50 participants per group for LTR:", round(power2$power *100, 1), "%\n")
cat(round(power3$power *100, 1), "%\n")
cat(round(power4$power *100, 1), "%\n")


model4 <- lm(FS ~ AC, data = stage_data)
model5 <- lm(FS ~ TB, data = stage_data)
plot(model4)
print(summary(model4))
plot(model5)
print(summary(model5))


calculate_aic <- function(loglikelihood, num_parameters) {
  aic <- -2 * loglikelihood + 2 * num_parameters
  return(aic)
}


bl_data <- results[results$Stage == "BL", c("AC", "FS", "TC", "TB")]
str_data <- results[results$Stage == "STR", c("AC", "FS", "TC", "TB")]
ltr_data <- results[results$Stage == "LTR", c("AC", "FS", "TC", "TB")]
personality_data <- unique(results[, c("AC", "FS", "TC", "TB")])


create_cor_matrix <- function(data, stage_name) {
  cat(stage_name, "\n")
  
  traits <- data[, c("AC", "FS", "TC", "TB")]
  cor_test <- rcorr(as.matrix(traits))
  
  # Print correlations
  print(round(cor_test$r, 3))
  cat("\nP-values:\n")
  print(round(cor_test$P, 3))
  
  return(list(matrix = cor_test$r, pvalues = cor_test$P))
}

bl_cor <- create_cor_matrix(bl_data, "Baseline")
str_cor <- create_cor_matrix(str_data, "Short-Term")
ltr_cor <- create_cor_matrix(ltr_data, "Long-Term")


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


