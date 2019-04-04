setwd("C:/Gustavo/Trabalhos/UnB/TCC/LATEX/tcc_git")
setwd("~/tcc")

atualiza()

load(file="jogos.rda")
load(file="bases.rda")
load(file="final.rda")

######testando "peso"
final2001$temporadas <- 0
final2002$temporadas <- 1
final2003$temporadas <- 2
final2004$temporadas <- 3
final2005$temporadas <- 4
final2006$temporadas <- 5
final2007$temporadas <- 6
final2008$temporadas <- 7
final2009$temporadas <- 8
final2010$temporadas <- 9
final2011$temporadas <- 10
final2012$temporadas <- 11
final2013$temporadas <- 12
final2014$temporadas <- 13
final2015$temporadas <- 14
final2016$temporadas <- 15
final2017$temporadas <- 16
final2018$temporadas <- 17
final2019$temporadas <- 18
#####

#base full
final <- rbind(final2013[1:1230,],final2014[1:1230,],final2015[1:1230,],final2016[1:1230,],
               final2017[1:1230,],final2018[1:1230,])
#1 win
#2 result

#tirando variaveis de casa pro time de fora e de fora pro time de casa
final <- final[,-c(5,8,11,14,16:18,23:25,42:53,78:89,106:109,117,120,123,132:134,139:141,143:154,179:190,215:218)]

#encontrando os padroes
teste19 <- final2019[1:872,-c(5,8,11,14,16:18,23:25,42:53,78:89,106:109,117,120,123,132:134,139:141,143:154,179:190,215:218)]
padrao19 <- is.na(teste19[,-c(1,2)])

for(i in 1:length(teste19$Win)){
  for(j in 1:149){
    padrao19[i,j] <- as.numeric(padrao19[i,j])
  }
}

oi19 <- apply(padrao19,1,paste,collapse="")
nomes <- names(summary(as.factor(oi19)))

vet <- c()
for(i in 1:length(nomes)){
  full19 <- oi19==nomes[1]
  td19 <- teste19[full19,]
  colun <- !is.na(td19[1,])
  td19 <- td19[,colun]
  td <- final[,colun]
  td <- td[rowSums(is.na(td)) == 0,]
  mod <- lm(result~., data=td[,-1])
  win <- (predict(mod, newdata=td19[,-1]) > 0) == (td19$result > 0)
  vet <- c(vet,win)
}

vet2 <- c()
for(i in 1:length(nomes)){
  library(dplyr)
  full19 <- oi19==nomes[i]
  td19 <- teste19[full19,]
  colun <- !is.na(td19[1,])
  td19 <- td19[,colun]
  td <- final[,colun]
  td <- td[rowSums(is.na(td)) == 0,]
  a <- glm(Win~., data = td[,-2], family=binomial(link = "logit"))
  library(dplyr)
  probabilities <- a %>% predict(td19[,-2], type = "response")
  predicted.classes <- ifelse(probabilities > 0.5, "TRUE", "FALSE")
  win <- predicted.classes == td19$Win
  vet2 <- c(vet2,win)
}

vet3 <- c()
for(i in 1:length(nomes)){
  library(e1071)
  full19 <- oi19==nomes[i]
  td19 <- teste19[full19,]
  colun <- !is.na(td19[1,])
  td19 <- td19[,colun]
  td <- final[,colun]
  td <- td[rowSums(is.na(td)) == 0,]
  mod <- svm(result~., data=td[,-1])
  win <- (predict(mod, newdata=td19[,-1]) > 0) == (td19$result > 0)
  vet3 <- c(vet3,win)
}

mean(vet3)
mean(teste19$Win)

#######REG LINEAR
mod <- lm(result~., data=td[,-1])
summary(mod)
win <- (predict(mod, newdata=td19[,-1]) > 0) == (td19$result > 0)
mean(win)

#selecao de variaveis
require(leaps)
#com o primeiro (com menos variaveis)
step(mod, direction='backward', scope=( ~ 1))
modmin <- lm(result ~ 1, data=td[,-1])
step(modmin, direction='forward', scope=(~ Days_LG + Wins_T + Wins_A + Loss_T + Loss_A + Streak_T + 
                                           Streak_A + OT_last + Str_Sch + Mean_Pts_S_A + Max_Pts_S_A + 
                                           Min_Pts_S_A + Mean_Pts_S_T + Mean_Pts_A_A + Max_Pts_A_A + 
                                           Min_Pts_A_A + Mean_Pts_A_T + Mean_Last3_away + Max_Last3_away + 
                                           Min_Last3away + Mean_Last5_away + Max_Last5_away + Min_Last5away + 
                                           Mean_Last7_away + Max_Last7_away + Min_Last7away + Mean_Last10_away + 
                                           Max_Last10_away + Min_Last10away + Mean_Last3_total + Max_Last3_total + 
                                           Min_Last3total + Mean_Last5_total + Max_Last5_total + Min_Last5total + 
                                           Mean_Last7_total + Max_Last7_total + Min_Last7total + Mean_Last10_total + 
                                           Max_Last10_total + Min_Last10total + Mean_Last3_away_opp + 
                                           Max_Last3_away_opp + Min_Last3away_opp + Mean_Last5_away_opp + 
                                           Max_Last5_away_opp + Min_Last5away_opp + Mean_Last7_away_opp + 
                                           Max_Last7_away_opp + Min_Last7away_opp + Mean_Last10_away_opp + 
                                           Max_Last10_away_opp + Min_Last10away_opp + Mean_Last3_total_opp + 
                                           Max_Last3_total_opp + Min_Last3total_opp + Mean_Last5_total_opp + 
                                           Max_Last5_total_opp + Min_Last5total_opp + Mean_Last7_total_opp + 
                                           Max_Last7_total_opp + Min_Last7total_opp + Mean_Last10_total_opp + 
                                           Max_Last10_total_opp + Min_Last10total_opp + Win_Last3_away + 
                                           Win_Last5_away + Win_Last7_away + Win_Last10_away + Win_Last3_total + 
                                           Win_Last5_total + Win_Last7_total + Win_Last10_total + Days_LG.1 + 
                                           Wins_T.1 + Wins_H.1 + Loss_T.1 + Loss_H.1 + Streak_T.1 + 
                                           Streak_H.1 + OT_last.1 + weekday + mean_attend + Travel.1 + 
                                           Str_Sch.1 + Mean_Pts_S_H.1 + Max_Pts_S_H.1 + Min_Pts_S_H.1 + 
                                           Mean_Pts_S_T.1 + Mean_Pts_A_H.1 + Max_Pts_A_H.1 + Min_Pts_A_H.1 + 
                                           Mean_Pts_A_T.1 + Mean_Last3_home.1 + Max_Last3_home.1 + Min_Last3home.1 + 
                                           Mean_Last5_home.1 + Max_Last5_home.1 + Min_Last5home.1 + 
                                           Mean_Last7_home.1 + Max_Last7_home.1 + Min_Last7home.1 + 
                                           Mean_Last10_home.1 + Max_Last10_home.1 + Min_Last10home.1 + 
                                           Mean_Last3_total.1 + Max_Last3_total.1 + Min_Last3total.1 + 
                                           Mean_Last5_total.1 + Max_Last5_total.1 + Min_Last5total.1 + 
                                           Mean_Last7_total.1 + Max_Last7_total.1 + Min_Last7total.1 + 
                                           Mean_Last10_total.1 + Max_Last10_total.1 + Min_Last10total.1 + 
                                           Mean_Last3_home_opp.1 + Max_Last3_home_opp.1 + Min_Last3home_opp.1 + 
                                           Mean_Last5_home_opp.1 + Max_Last5_home_opp.1 + Min_Last5home_opp.1 + 
                                           Mean_Last7_home_opp.1 + Max_Last7_home_opp.1 + Min_Last7home_opp.1 + 
                                           Mean_Last10_home_opp.1 + Max_Last10_home_opp.1 + Min_Last10home_opp.1 + 
                                           Mean_Last3_total_opp.1 + Max_Last3_total_opp.1 + Min_Last3total_opp.1 + 
                                           Mean_Last5_total_opp.1 + Max_Last5_total_opp.1 + Min_Last5total_opp.1 + 
                                           Mean_Last7_total_opp.1 + Max_Last7_total_opp.1 + Min_Last7total_opp.1 + 
                                           Mean_Last10_total_opp.1 + Max_Last10_total_opp.1 + Min_Last10total_opp.1 + 
                                           Win_Last3_home.1 + Win_Last5_home.1 + Win_Last7_home.1 + 
                                           Win_Last10_home.1 + Win_Last3_total.1 + Win_Last5_total.1 + 
                                           Win_Last7_total.1 + Win_Last10_total.1))

modforw <- lm(formula = result ~ Days_LG + Wins_T + Streak_T + Mean_Pts_S_A + 
                Max_Pts_S_A + Mean_Pts_A_A + Max_Pts_A_A + Days_LG.1 + Wins_T.1 + 
                Loss_T.1 + Streak_T.1 + Str_Sch.1, data = td[, -1])
summary(modforw)
winfor <- (predict(modforw, newdata=td19[,-1]) > 0) == (td19$result > 0)
mean(winfor)

library(standardize)
pad <- standardize(formula = result ~ Win_Last10_home.1 + Win_Last10_total + 
                     Wins_T + Wins_T.1 + Mean_Last10_total.1 + Mean_Pts_A_T.1 + 
                     Mean_Pts_S_H.1 + Games_T + Min_Last3total + Min_Last3total_opp + 
                     Mean_Pts_S_T.1 + Games_H.1 + Str_Sch.1 + Streak_A + Max_Pts_A_A + 
                     Mean_Pts_S_T + Mean_Pts_A_T + Days_LG + Win_Last3_home.1 + 
                     Mean_Last5_total_opp.1 + Days_LG.1 + Min_Last10home_opp.1 + 
                     Str_Sch + Min_Last5away_opp + Min_Last10total_opp + Min_Last5home.1 + 
                     Min_Last3away, data = td[, -74])

novo <- pad$data

modnovo <- lm(td$result~Win_Last10_home.1 + Win_Last10_total + 
                Wins_T + Wins_T.1 + Mean_Last10_total.1 + Mean_Pts_A_T.1 + 
                Mean_Pts_S_H.1 + Games_T + Min_Last3total + Min_Last3total_opp + 
                Mean_Pts_S_T.1 + Games_H.1 + Str_Sch.1 + Streak_A + Max_Pts_A_A + 
                Mean_Pts_S_T + Mean_Pts_A_T + Days_LG + Win_Last3_home.1 + 
                Mean_Last5_total_opp.1 + Days_LG.1 + Min_Last10home_opp.1 + 
                Str_Sch + Min_Last5away_opp + Min_Last10total_opp + Min_Last5home.1 + 
                Min_Last3away, data = novo[, -1])

summary(modnovo)

step(mod, direction='backward', scope=( ~ 1))
modback <- lm(formula = result ~ Days_LG + Wins_T + Wins_A + Max_Pts_S_A + 
                Min_Pts_S_A + Mean_Pts_S_T + Mean_Pts_A_T + Max_Last3_away + 
                Min_Last5away + Mean_Last7_away + Max_Last7_away + Min_Last7away + 
                Max_Last10_away + Min_Last10away + Min_Last3total + Max_Last5_total + 
                Mean_Last10_total + Mean_Last3_away_opp + Max_Last3_away_opp + 
                Min_Last3away_opp + Max_Last5_away_opp + Mean_Last7_away_opp + 
                Mean_Last3_total_opp + Max_Last3_total_opp + Min_Last5total_opp + 
                Max_Last7_total_opp + Min_Last7total_opp + Mean_Last10_total_opp + 
                Win_Last7_away + Win_Last10_away + Win_Last10_total + Days_LG.1 + 
                Loss_T.1 + OT_last.1 + Mean_Pts_S_H.1 + Min_Pts_S_H.1 + Mean_Last3_home.1 + 
                Max_Last3_home.1 + Min_Last3home.1 + Max_Last5_home.1 + Mean_Last3_total.1 + 
                Max_Last3_total.1 + Max_Last7_total.1 + Min_Last7total.1 + 
                Max_Last10_total.1 + Max_Last3_home_opp.1 + Min_Last3home_opp.1 + 
                Mean_Last5_home_opp.1 + Min_Last5home_opp.1 + Mean_Last7_home_opp.1 + 
                Max_Last3_total_opp.1 + Mean_Last5_total_opp.1 + Max_Last5_total_opp.1 + 
                Mean_Last7_total_opp.1 + Max_Last7_total_opp.1 + Max_Last10_total_opp.1 + 
                Win_Last3_home.1 + Win_Last7_home.1 + Win_Last7_total.1, 
              data = td[, -1])
summary(modback)
winback <- (predict(modback, newdata=td18[,-1]) > 0) == (td18$result > 0)
mean(winback)


####REG LOGISTICA
a <- glm(Win~., data = td[,-2], family=binomial(link = "logit"))
summary(a)
library(dplyr)
probabilities <- a %>% predict(td19[,-2], type = "response")
predicted.classes <- ifelse(probabilities > 0.5, "TRUE", "FALSE")
mean(predicted.classes == td19$Win)

#selecao de variaveis
modmin2 <- glm(Win~1, data = td[,-2], family=binomial(link = "logit"))
step(modmin2, direction='forward', scope=(~Days_LG + Wins_T + Wins_A + Loss_T + Loss_A + Streak_T + 
                                            Streak_A + OT_last + Str_Sch + Mean_Pts_S_A + Max_Pts_S_A + 
                                            Min_Pts_S_A + Mean_Pts_S_T + Mean_Pts_A_A + Max_Pts_A_A + 
                                            Min_Pts_A_A + Mean_Pts_A_T + Mean_Last3_away + Max_Last3_away + 
                                            Min_Last3away + Mean_Last5_away + Max_Last5_away + Min_Last5away + 
                                            Mean_Last7_away + Max_Last7_away + Min_Last7away + Mean_Last10_away + 
                                            Max_Last10_away + Min_Last10away + Mean_Last3_total + Max_Last3_total + 
                                            Min_Last3total + Mean_Last5_total + Max_Last5_total + Min_Last5total + 
                                            Mean_Last7_total + Max_Last7_total + Min_Last7total + Mean_Last10_total + 
                                            Max_Last10_total + Min_Last10total + Mean_Last3_away_opp + 
                                            Max_Last3_away_opp + Min_Last3away_opp + Mean_Last5_away_opp + 
                                            Max_Last5_away_opp + Min_Last5away_opp + Mean_Last7_away_opp + 
                                            Max_Last7_away_opp + Min_Last7away_opp + Mean_Last10_away_opp + 
                                            Max_Last10_away_opp + Min_Last10away_opp + Mean_Last3_total_opp + 
                                            Max_Last3_total_opp + Min_Last3total_opp + Mean_Last5_total_opp + 
                                            Max_Last5_total_opp + Min_Last5total_opp + Mean_Last7_total_opp + 
                                            Max_Last7_total_opp + Min_Last7total_opp + Mean_Last10_total_opp + 
                                            Max_Last10_total_opp + Min_Last10total_opp + Win_Last3_away + 
                                            Win_Last5_away + Win_Last7_away + Win_Last10_away + Win_Last3_total + 
                                            Win_Last5_total + Win_Last7_total + Win_Last10_total + Days_LG.1 + 
                                            Wins_T.1 + Wins_H.1 + Loss_T.1 + Loss_H.1 + Streak_T.1 + 
                                            Streak_H.1 + OT_last.1 + weekday + mean_attend + Travel.1 + 
                                            Str_Sch.1 + Mean_Pts_S_H.1 + Max_Pts_S_H.1 + Min_Pts_S_H.1 + 
                                            Mean_Pts_S_T.1 + Mean_Pts_A_H.1 + Max_Pts_A_H.1 + Min_Pts_A_H.1 + 
                                            Mean_Pts_A_T.1 + Mean_Last3_home.1 + Max_Last3_home.1 + Min_Last3home.1 + 
                                            Mean_Last5_home.1 + Max_Last5_home.1 + Min_Last5home.1 + 
                                            Mean_Last7_home.1 + Max_Last7_home.1 + Min_Last7home.1 + 
                                            Mean_Last10_home.1 + Max_Last10_home.1 + Min_Last10home.1 + 
                                            Mean_Last3_total.1 + Max_Last3_total.1 + Min_Last3total.1 + 
                                            Mean_Last5_total.1 + Max_Last5_total.1 + Min_Last5total.1 + 
                                            Mean_Last7_total.1 + Max_Last7_total.1 + Min_Last7total.1 + 
                                            Mean_Last10_total.1 + Max_Last10_total.1 + Min_Last10total.1 + 
                                            Mean_Last3_home_opp.1 + Max_Last3_home_opp.1 + Min_Last3home_opp.1 + 
                                            Mean_Last5_home_opp.1 + Max_Last5_home_opp.1 + Min_Last5home_opp.1 + 
                                            Mean_Last7_home_opp.1 + Max_Last7_home_opp.1 + Min_Last7home_opp.1 + 
                                            Mean_Last10_home_opp.1 + Max_Last10_home_opp.1 + Min_Last10home_opp.1 + 
                                            Mean_Last3_total_opp.1 + Max_Last3_total_opp.1 + Min_Last3total_opp.1 + 
                                            Mean_Last5_total_opp.1 + Max_Last5_total_opp.1 + Min_Last5total_opp.1 + 
                                            Mean_Last7_total_opp.1 + Max_Last7_total_opp.1 + Min_Last7total_opp.1 + 
                                            Mean_Last10_total_opp.1 + Max_Last10_total_opp.1 + Min_Last10total_opp.1 + 
                                            Win_Last3_home.1 + Win_Last5_home.1 + Win_Last7_home.1 + 
                                            Win_Last10_home.1 + Win_Last3_total.1 + Win_Last5_total.1 + 
                                            Win_Last7_total.1 + Win_Last10_total.1))

modforw2 <- glm(formula = Win ~ Win_Last10_home.1 + Win_Last10_total + Loss_T.1 + 
                  Loss_T + Min_Pts_A_H.1 + Min_Last5home.1 + Min_Last3total_opp.1 + 
                  Mean_Last7_total.1 + Mean_Pts_A_T.1 + Mean_Pts_S_T.1 + Wins_A + 
                  Mean_Pts_A_T + Mean_Pts_S_T + Wins_T + Win_Last5_home.1 + 
                  Win_Last3_total + Min_Last3total + Max_Last7_total_opp + 
                  Max_Pts_S_H.1 + Min_Last3home_opp.1 + Min_Last7home_opp.1 + 
                  Loss_H.1 + mean_attend + Mean_Last10_home_opp.1 + Max_Pts_S_A + 
                  OT_last.1, family = binomial(link = "logit"), data = td[, 
                                                                          -2])
summary(modforw2)
probabilities3 <- modforw2 %>% predict(td19[,-2], type = "response")
predicted.classes3 <- ifelse(probabilities3 > 0.5, "TRUE", "FALSE")
mean(predicted.classes3 == td19$Win)

#########SVM
library(e1071)
teste <- svm(result~., data=final[,-1], cost=8, gamma=10^-4)
summary(teste)
winsvm <- (predict(teste, newdata=teste19[,-1]) > 0) == (teste19$result > 0)
mean(winsvm)

teste19 <- teste19[rowSums(is.na(teste19)) == 0,]

teste1 <- svm(result ~ Win_Last10_home.1 + Win_Last10_total + 
                Wins_T.1 + Wins_T + Mean_Pts_S_T + Mean_Pts_A_T + Loss_T.1 + 
                Streak_T + Win_Last5_total.1 + Min_Pts_A_H.1 + Mean_Last10_home.1 + 
                Mean_Pts_A_T.1 + Mean_Pts_S_T.1 + Days_LG + Days_LG.1 + Travel.1 + 
                Mean_Last5_away_opp + Max_Last5_away + Mean_Last10_away_opp + 
                Max_Last10_away + Min_Last3total + Min_Last5total.1 + Mean_Last3_total_opp.1 + 
                Min_Last5home_opp.1 + Mean_Last7_home_opp.1 + Max_Last10_home_opp.1 + 
                OT_last + Max_Pts_S_A + Str_Sch
              , data = td[, -1])
summary(teste1)
win1 <- (predict(teste1, newdata=td19[,-1]) > 0) == (td19$result > 0)
mean(win1)

teste2 <- svm(result ~ Win_Last10_home.1 + Win_Last10_total + 
                Wins_A + Wins_T.1 + Wins_T + Days_LG + Mean_Last7_total.1 + 
                Mean_Pts_A_T.1 + Mean_Pts_S_T.1 + Loss_T + Str_Sch.1 + Days_LG.1 + 
                Max_Pts_A_A + Mean_Pts_S_T + Mean_Pts_A_T + Min_Last5total_opp.1 + 
                Min_Last3total + Mean_Last7_total_opp + Streak_T + Str_Sch + 
                Win_Last3_home.1 + Min_Last3home_opp.1 + Min_Last5away + 
                Loss_A + Loss_H.1 + Mean_Last7_home.1 + mean_attend + Mean_Last7_total_opp.1 + 
                Min_Last3total_opp + Max_Last10_away + Max_Last7_total, data = td[, -1])
summary(teste2)
win2 <- (predict(teste2, newdata=td18[,-1]) > 0) == (td18$result > 0)
mean(win2)

###########RANDOM FOREST
require(randomForest)
mod <- randomForest(result~., data=td[,-1])
mod
mean((predict(mod, newdata=td19[,-1]) > 0) == (td19$result > 0))

mod2 <- randomForest(result~Win_Last10_home.1 + Win_Last10_total + 
                       Wins_T.1 + Wins_T + Mean_Pts_S_T + Mean_Pts_A_T + Loss_T.1 + 
                       Streak_T + Win_Last5_total.1 + Min_Pts_A_H.1 + Mean_Last10_home.1 + 
                       Mean_Pts_A_T.1 + Mean_Pts_S_T.1 + Days_LG + Days_LG.1 + Travel.1 + 
                       Mean_Last5_away_opp + Max_Last5_away + Mean_Last10_away_opp + 
                       Max_Last10_away + Min_Last3total + Min_Last5total.1 + Mean_Last3_total_opp.1 + 
                       Min_Last5home_opp.1 + Mean_Last7_home_opp.1 + Max_Last10_home_opp.1 + 
                       OT_last + Max_Pts_S_A + Str_Sch, data=td[,-1])
mean((predict(mod2, newdata=td19[,-1]) > 0) == (td19$result > 0))

mod3 <- randomForest(result~Win_Last10_home.1 + Win_Last10_total + Wins_T.1 + 
                       Wins_T + Max_Last7_total_opp + Min_Last3total + Min_Last5total_opp.1 + 
                       Mean_Last10_total.1 + Mean_Pts_A_T.1 + Mean_Pts_S_H.1 + Loss_T + 
                       Mean_Pts_A_T + Mean_Pts_S_T + Days_LG + Win_Last3_total.1 + 
                       Streak_T + Min_Last3away + Mean_Pts_S_T.1 + Min_Last10total_opp.1 + 
                       Mean_Last5_total.1 + Mean_Last3_total_opp.1 + Days_LG.1 + 
                       Str_Sch + Loss_H.1 + Wins_H.1 + Mean_Last3_total.1 + Win_Last7_total, data=td[,-1])
mean((predict(mod3, newdata=td18[,-1]) > 0) == (td18$result > 0))

####################TESTES - MUITO DEMORADO
#melhores parametros para o svm
tuneResult <- tune(svm, result ~ Win_Last10_home.1 + Win_Last10_total + Wins_T.1 + 
                     Wins_T + Wins_H.1 + Max_Last3_total_opp.1 + Min_Last5home.1 + 
                     Win_Last3_home.1 + Mean_Pts_A_T.1 + Mean_Last10_total.1 + 
                     Games_T + Mean_Pts_S_H.1 + Win_Last7_total + Games_H.1 + 
                     Win_Last3_away + Mean_Last10_away_opp + Mean_Pts_S_T + Mean_Pts_A_T + 
                     Min_Last3total + Min_Last7home.1 + Streak_T + Min_Last3total_opp + 
                     Min_Last5total_opp.1 + Win_Last3_total.1 + Mean_Last5_total.1 + 
                     Min_Last10home_opp.1 + Max_Last5_home_opp.1 + Mean_Pts_A_H.1 + 
                     Games_T.1 + Streak_H.1 + Min_Last10total_opp + Min_Last7total.1 + 
                     Mean_Last5_home.1 + Max_Pts_S_H.1 + Min_Last3total.1, data = td[, -74],
                   ranges = list(epsilon = seq(0,1,0.1), cost = 2^(2:9)))


####melhores modelos com x variaveis
library(leaps)
modelos<-regsubsets(result ~ .,data=td[,-72],nbest=1,nvmax=20,really.big=T)
summary(leaps)

###selecao stepwise
library(MASS)
stepAIC(mod, direction = "both")
