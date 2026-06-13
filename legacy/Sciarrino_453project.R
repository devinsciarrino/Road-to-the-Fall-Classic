#raw data logistic regression
library(readxl)

Sciarrino_453project <- read_excel("Documents/Sciarrino_453project.xlsm", 
                                     +     sheet = "2000 raw")
model_1 <- glm(dWSWin ~ R + x1B + x2B + x3B + HR + BB + SO + SB + CS + HBP + SF +
                   RA + CG + SHO + SV + HA + HRA + BBA + K + 
                   E + DP,
                 data = Sciarrino_453project, family = binomial)
summary(model_1)
deviance(model_1)
predicted_probabilities_1 <- predict(model_1, type = "response")
actual_outcomes_1 <- Sciarrino_453project$dWSWin
brier_score_1 <- mean((predicted_probabilities_1 - actual_outcomes_1)^2)
print(brier_score_1)

#Calc data logistic regression
library(readxl)
> Sciarrino_453project <- read_excel("Documents/Sciarrino_453project.xlsm", 
                                   +     sheet = "2000 calc")
model_2 <- glm(dWSWin ~ BA+OBP+SLG+
                            ERA+KPN+WHIP+
                            FP,
                  data = Sciarrino_453project, family = binomial)
summary(model_2)
deviance(model_2)
predicted_probabilities_2 <- predict(model_2, type = "response")
actual_outcomes_2 <- Sciarrino_453project$dWSWin
brier_score_2 <- mean((predicted_probabilities_2 - actual_outcomes_2)^2)
print(brier_score_2)

#model 3
library(readxl)
> Sciarrino_453project <- read_excel("Documents/Sciarrino_453project.xlsm", 
                                     +     sheet = "model 3")
model_3 <- glm(dWSWin ~ BA+OBP+SLG+x3B+SB+KPN+WHIP+FP,
               data = Sciarrino_453project, family = binomial)
summary(model_3)
deviance(model_3)
predicted_probabilities_3 <- predict(model_3, type = "response")
actual_outcomes_3 <- Sciarrino_453project$dWSWin
brier_score_3 <- mean((predicted_probabilities_3 - actual_outcomes_3)^2)
print(brier_score_3)

#Model 4
library(readxl)
> Sciarrino_453project <- read_excel("Documents/Sciarrino_453project.xlsm", 
                                     +     sheet = "plus")
model_4 <- glm(dWSWin ~ OPSP+WHIPP+FPP,
               data = Sciarrino_453project, family = binomial)
summary(model_4)
deviance(model_4)
predicted_probabilities_4 <- predict(model_4, type = "response")
actual_outcomes_4 <- Sciarrino_453project$dWSWin
brier_score_4 <- mean((predicted_probabilities_4 - actual_outcomes_3)^2)
print(brier_score_4)

#2023 Rangers Predictions
#model 1
predict(model_1, data.frame(R=881, x1B = 893, x2B = 326, x3B = 18, HR = 233, BB = 599, SO = 1416, SB = 79, CS = 19, HBP = 53, SF = 10,
                                 RA =716 , CG = 3, SHO = 1, SV = 30, HA = 1330, HRA = 198, BBA = 491, K = 1351, E = 57, DP = 143), type = "response")
#model 2
predict(model_2, data.frame(data.frame(BA = .263, OBP = .337, SLG = .452, ERA = 4.28, KPN = 8.5, WHIP = 1.268, FP = .990)), type = "response")

#model 3
predict(model_3, data.frame(data.frame(BA = .263, OBP = .337, SLG = .452, SB = 79, x3B=18, KPN = 8.5, WHIP = 1.268, FP = .990)), type = "response")

#model 4
predict(model_4, data.frame(data.frame(OPSP=113, WHIPP=96.43, FPP=.406)), type = "response")





