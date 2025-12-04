options(repos = c(CRAN = "https://cloud.r-project.org"))

install.packages("carData")

install.packages("car")

install.packages("MASS")

install.packages("fitdistrplus")

install.packages("pwr")

library("carData")

library("MASS")

library("fitdistrplus")

library("car")

library("pwr")

d<-read.csv("dataset.csv", header=TRUE)

par(mfrow= c(2,2))

hist(d$Achiever, col="yellow", main= "Histogram of AC")

hist(d$FreeSpirit, col="green", main="Histogram of FS")

hist(d$TransformOfChallenge, col="blue", main= "Histogram of TC")

hist(d$TransformOfBorendom, col="coral", main="Histogram of TB")

par(mfrow= c(1,1))

par(mfrow= c(2,2))

unique_personality <- aggregate(. ~ ParticipantID, 

                                data = d[, c("ParticipantID", "Achiever", "FreeSpirit", 

                                             "TransformOfChallenge", "TransformOfBorendom")],

                                FUN = function(x) x[1])

FS_unique <- unique_personality$FreeSpirit

AC_unique <- unique_personality$Achiever

TC_unique <- unique_personality$TransformOfChallenge

TB_unique <- unique_personality$TransformOfBorendom

FreeSpirit_normalized <- (FS_unique - min(FS_unique)) / (max(FS_unique) - min(FS_unique))

beta1 <- fitdist(FreeSpirit_normalized, "beta")

qqPlot(FreeSpirit_normalized, distribution = "beta",

       shape1 = beta1$estimate["shape1"],

       shape2 = beta1$estimate["shape2"],

       main = "Beta Q-Q Plot of FS (Unique Values)")

print(beta1)

normal <- fitdist(TB_unique, "norm")

qqPlot(TB_unique, distribution = "norm",

       main = "Normal Q-Q Plot of TB (Unique Values)")

print(normal)

Achiever_normalized <- (AC_unique - min(AC_unique)) / (max(AC_unique) - min(AC_unique))

beta3 <- fitdist(Achiever_normalized, "beta")

qqPlot(Achiever_normalized, distribution = "beta", 

       shape1 = beta3$estimate["shape1"],

       shape2 = beta3$estimate["shape2"],

       main = "Beta Q-Q Plot of AC (Unique Values)")

print(beta3)

TransformOfChallenge_normalized <- (TC_unique - min(TC_unique)) / (max(TC_unique) - min(TC_unique))

beta2 <- fitdist(TransformOfChallenge_normalized, "beta")

qqPlot(TransformOfChallenge_normalized, distribution = "beta", 

       shape1 = beta2$estimate["shape1"],

       shape2 = beta2$estimate["shape2"],

       main = "Beta Q-Q Plot of TC (Unique Values)")

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

ks_ac <- ks.test(Achiever_normalized, "pbeta", 

                 shape1 = beta3$estimate["shape1"], 

                 shape2 = beta3$estimate["shape2"])

cat("Kolmogorov-Smirnov test for Achiever (beta, Unique Values):\n")

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

wide_data$Learn_STR <- wide_data$BL - wide_data$STR

wide_data$Learn_LTR <- wide_data$BL - wide_data$LTR

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

  print(summary(model1))

  stage_data$GroupE <- ifelse(stage_data$Group == "E", 1, 0)

  model2 <- lm(AveAbsError ~ AC + FS + TC + TB + 

  GroupE + GroupE:AC + GroupE:FS + GroupE:TC + GroupE:TB, 

           data = stage_data)

  print(summary(model2))

}

plot(model1)

plot(model2)

required_sample <- pwr.t.test(power = 0.80,

                              d = 0.5,  

                              sig.level = 0.05,

                              type = "two.sample")

cat("Required sample size for 80% power:\n")

cat(ceiling(required_sample$n), "participants PER GROUP\n")

cat("Total:", ceiling(required_sample$n) * 2, "participants\n")

cat("Control participants:", sum(wide_data$Group == "C"), "\n")

cat("Experimental participants:", sum(wide_data$Group == "E"), "\n")

quick_power <- pwr.t.test(n = sum(wide_data$Group=="C"),  

                          d = 0.5,  

                          sig.level = 0.05,

                          type = "two.sample")

cat("Current power with", sum(wide_data$Group=="C"), "participants per group:", round(quick_power$power * 100, 1), "%\n")


