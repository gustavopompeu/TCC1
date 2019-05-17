######## melhor resultado
load(file="final.rda")
final <- rbind(final2003,final2005,final2007,final2009,final2010,final2011,final2012,final2015,final2016,final2018)
final <- final[,-c(5,8,11,14,16:18,23:25,42:53,78:89,106:109,117,120,123,132:134,139:141,143:154,179:190,215:218)]
teste19 <- final2019[1:1230,-c(5,8,11,14,16:18,23:25,42:53,78:89,106:109,117,120,123,132:134,139:141,143:154,179:190,215:218)]

mod <- glm(Win~., data=final[,-2], family=binomial(link = "logit"))
library(dplyr)
probabilities <- mod %>% predict(teste19[,-2], type = "response")
predicted.classes <- ifelse(probabilities > 0.5, "TRUE", "FALSE")
mean(predicted.classes == teste19$Win, na.rm=T)

b <- c()
for(i in 1:length(predicted.classes)){
  wr <- predicted.classes[1:i] == teste19$Win[1:i]
  if(!is.na(predicted.classes[i])){
    b[i] <- mean(wr, na.rm=T)
  }else{
    b[i] <- NaN
  }
}

plot(b, main="Porcentagem de acerto por numero do jogo",sub="temporada 00/01 a 17/18 prevendo 18/19",
     xlab="#Jogo", ylab="% acerto previsao", ylim=c(0,1))
abline(v=414, lty=2, col=2)
abline(v=1230,lty=2, col=3)
text(500,0.8,"ultimo NA")
text(600,0.1,paste("% acerto = ",round(mean(predicted.classes == teste19$Win, na.rm=T),3),sep=""))
text(600,1,"Reg Logistica",cex=1.5)

####################

load(file="final.rda")
final <- rbind(final2001,final2002,final2003,final2004,final2005,final2006,final2007,final2008,final2009,final2010,final2011,final2012,final2013,final2014,final2015,final2016,final2017,final2018)
final <- final[,-c(5,8,11,14,16:18,23:25,42:53,78:89,106:109,117,120,123,132:134,139:141,143:154,179:190,215:218)]
teste19 <- final2019[1:1230,-c(5,8,11,14,16:18,23:25,42:53,78:89,106:109,117,120,123,132:134,139:141,143:154,179:190,215:218)]

mod <- glm(Win~., data=final[,-2], family=binomial(link = "logit"))
library(dplyr)
probabilities <- mod %>% predict(teste19[,-2], type = "response")
predicted.classes <- ifelse(probabilities > 0.5, "TRUE", "FALSE")
mean(predicted.classes == teste19$Win, na.rm=T)

b <- c()
for(i in 1:length(predicted.classes)){
  wr <- predicted.classes[1:i] == teste19$Win[1:i]
  if(!is.na(predicted.classes[i])){
    b[i] <- mean(wr, na.rm=T)
  }else{
    b[i] <- NaN
  }
}

plot(b, main="Porcentagem de acerto por numero do jogo",sub="temporada 00/01 a 17/18 prevendo 18/19",
     xlab="#Jogo", ylab="% acerto previsao", ylim=c(0,1))
abline(v=414, lty=2, col=2)
abline(v=1230,lty=2, col=3)
text(500,0.8,"ultimo NA")
text(600,0.1,paste("% acerto = ",round(mean(predicted.classes == teste19$Win, na.rm=T),3),sep=""))
text(600,1,"Reg Logistica",cex=1.5)

####################

load(file="final.rda")
final <- rbind(final2002,final2003,final2004,final2005,final2006,final2007,final2008,final2009,final2010,final2011,final2012,final2013,final2014,final2015,final2016,final2017,final2018)
final <- final[,-c(5,8,11,14,16:18,23:25,42:53,78:89,106:109,117,120,123,132:134,139:141,143:154,179:190,215:218)]
teste19 <- final2019[1:1230,-c(5,8,11,14,16:18,23:25,42:53,78:89,106:109,117,120,123,132:134,139:141,143:154,179:190,215:218)]

mod <- glm(Win~., data=final[,-2], family=binomial(link = "logit"))
library(dplyr)
probabilities <- mod %>% predict(teste19[,-2], type = "response")
predicted.classes <- ifelse(probabilities > 0.5, "TRUE", "FALSE")
mean(predicted.classes == teste19$Win, na.rm=T)

b <- c()
for(i in 1:length(predicted.classes)){
  wr <- predicted.classes[1:i] == teste19$Win[1:i]
  if(!is.na(predicted.classes[i])){
    b[i] <- mean(wr, na.rm=T)
  }else{
    b[i] <- NaN
  }
}

plot(b, main="Porcentagem de acerto por numero do jogo",sub="temporada 01/02 a 17/18 prevendo 18/19",
     xlab="#Jogo", ylab="% acerto previsao", ylim=c(0,1))
abline(v=414, lty=2, col=2)
abline(v=1230,lty=2, col=3)
text(500,0.8,"ultimo NA")
text(600,0.1,paste("% acerto = ",round(mean(predicted.classes == teste19$Win, na.rm=T),3),sep=""))
text(600,1,"Reg Logistica",cex=1.5)

####################

load(file="final.rda")
final <- rbind(final2003,final2004,final2005,final2006,final2007,final2008,final2009,final2010,final2011,final2012,final2013,final2014,final2015,final2016,final2017,final2018)
final <- final[,-c(5,8,11,14,16:18,23:25,42:53,78:89,106:109,117,120,123,132:134,139:141,143:154,179:190,215:218)]
teste19 <- final2019[1:1230,-c(5,8,11,14,16:18,23:25,42:53,78:89,106:109,117,120,123,132:134,139:141,143:154,179:190,215:218)]

mod <- glm(Win~., data=final[,-2], family=binomial(link = "logit"))
library(dplyr)
probabilities <- mod %>% predict(teste19[,-2], type = "response")
predicted.classes <- ifelse(probabilities > 0.5, "TRUE", "FALSE")
mean(predicted.classes == teste19$Win, na.rm=T)

b <- c()
for(i in 1:length(predicted.classes)){
  wr <- predicted.classes[1:i] == teste19$Win[1:i]
  if(!is.na(predicted.classes[i])){
    b[i] <- mean(wr, na.rm=T)
  }else{
    b[i] <- NaN
  }
}

plot(b, main="Porcentagem de acerto por numero do jogo",sub="temporada 02/03 a 17/18 prevendo 18/19",
     xlab="#Jogo", ylab="% acerto previsao", ylim=c(0,1))
abline(v=414, lty=2, col=2)
abline(v=1230,lty=2, col=3)
text(500,0.8,"ultimo NA")
text(600,0.1,paste("% acerto = ",round(mean(predicted.classes == teste19$Win, na.rm=T),3),sep=""))
text(600,1,"Reg Logistica",cex=1.5)

####################

load(file="final.rda")
final <- rbind(final2004,final2005,final2006,final2007,final2008,final2009,final2010,final2011,final2012,final2013,final2014,final2015,final2016,final2017,final2018)
final <- final[,-c(5,8,11,14,16:18,23:25,42:53,78:89,106:109,117,120,123,132:134,139:141,143:154,179:190,215:218)]
teste19 <- final2019[1:1230,-c(5,8,11,14,16:18,23:25,42:53,78:89,106:109,117,120,123,132:134,139:141,143:154,179:190,215:218)]

mod <- glm(Win~., data=final[,-2], family=binomial(link = "logit"))
library(dplyr)
probabilities <- mod %>% predict(teste19[,-2], type = "response")
predicted.classes <- ifelse(probabilities > 0.5, "TRUE", "FALSE")
mean(predicted.classes == teste19$Win, na.rm=T)

b <- c()
for(i in 1:length(predicted.classes)){
  wr <- predicted.classes[1:i] == teste19$Win[1:i]
  if(!is.na(predicted.classes[i])){
    b[i] <- mean(wr, na.rm=T)
  }else{
    b[i] <- NaN
  }
}

plot(b, main="Porcentagem de acerto por numero do jogo",sub="temporada 03/04 a 17/18 prevendo 18/19",
     xlab="#Jogo", ylab="% acerto previsao", ylim=c(0,1))
abline(v=414, lty=2, col=2)
abline(v=1230,lty=2, col=3)
text(500,0.8,"ultimo NA")
text(600,0.1,paste("% acerto = ",round(mean(predicted.classes == teste19$Win, na.rm=T),3),sep=""))
text(600,1,"Reg Logistica",cex=1.5)

####################

load(file="final.rda")
final <- rbind(final2005,final2006,final2007,final2008,final2009,final2010,final2011,final2012,final2013,final2014,final2015,final2016,final2017,final2018)
final <- final[,-c(5,8,11,14,16:18,23:25,42:53,78:89,106:109,117,120,123,132:134,139:141,143:154,179:190,215:218)]
teste19 <- final2019[1:1230,-c(5,8,11,14,16:18,23:25,42:53,78:89,106:109,117,120,123,132:134,139:141,143:154,179:190,215:218)]

mod <- glm(Win~., data=final[,-2], family=binomial(link = "logit"))
library(dplyr)
probabilities <- mod %>% predict(teste19[,-2], type = "response")
predicted.classes <- ifelse(probabilities > 0.5, "TRUE", "FALSE")
mean(predicted.classes == teste19$Win, na.rm=T)

b <- c()
for(i in 1:length(predicted.classes)){
  wr <- predicted.classes[1:i] == teste19$Win[1:i]
  if(!is.na(predicted.classes[i])){
    b[i] <- mean(wr, na.rm=T)
  }else{
    b[i] <- NaN
  }
}

plot(b, main="Porcentagem de acerto por numero do jogo",sub="temporada 04/05 a 17/18 prevendo 18/19",
     xlab="#Jogo", ylab="% acerto previsao", ylim=c(0,1))
abline(v=414, lty=2, col=2)
abline(v=1230,lty=2, col=3)
text(500,0.8,"ultimo NA")
text(600,0.1,paste("% acerto = ",round(mean(predicted.classes == teste19$Win, na.rm=T),3),sep=""))
text(600,1,"Reg Logistica",cex=1.5)

####################

load(file="final.rda")
final <- rbind(final2006,final2007,final2008,final2009,final2010,final2011,final2012,final2013,final2014,final2015,final2016,final2017,final2018)
final <- final[,-c(5,8,11,14,16:18,23:25,42:53,78:89,106:109,117,120,123,132:134,139:141,143:154,179:190,215:218)]
teste19 <- final2019[1:1230,-c(5,8,11,14,16:18,23:25,42:53,78:89,106:109,117,120,123,132:134,139:141,143:154,179:190,215:218)]

mod <- glm(Win~., data=final[,-2], family=binomial(link = "logit"))
library(dplyr)
probabilities <- mod %>% predict(teste19[,-2], type = "response")
predicted.classes <- ifelse(probabilities > 0.5, "TRUE", "FALSE")
mean(predicted.classes == teste19$Win, na.rm=T)

b <- c()
for(i in 1:length(predicted.classes)){
  wr <- predicted.classes[1:i] == teste19$Win[1:i]
  if(!is.na(predicted.classes[i])){
    b[i] <- mean(wr, na.rm=T)
  }else{
    b[i] <- NaN
  }
}

plot(b, main="Porcentagem de acerto por numero do jogo",sub="temporada 05/06 a 17/18 prevendo 18/19",
     xlab="#Jogo", ylab="% acerto previsao", ylim=c(0,1))
abline(v=414, lty=2, col=2)
abline(v=1230,lty=2, col=3)
text(500,0.8,"ultimo NA")
text(600,0.1,paste("% acerto = ",round(mean(predicted.classes == teste19$Win, na.rm=T),3),sep=""))
text(600,1,"Reg Logistica",cex=1.5)

####################

load(file="final.rda")
final <- rbind(final2007,final2008,final2009,final2010,final2011,final2012,final2013,final2014,final2015,final2016,final2017,final2018)
final <- final[,-c(5,8,11,14,16:18,23:25,42:53,78:89,106:109,117,120,123,132:134,139:141,143:154,179:190,215:218)]
teste19 <- final2019[1:1230,-c(5,8,11,14,16:18,23:25,42:53,78:89,106:109,117,120,123,132:134,139:141,143:154,179:190,215:218)]

mod <- glm(Win~., data=final[,-2], family=binomial(link = "logit"))
library(dplyr)
probabilities <- mod %>% predict(teste19[,-2], type = "response")
predicted.classes <- ifelse(probabilities > 0.5, "TRUE", "FALSE")
mean(predicted.classes == teste19$Win, na.rm=T)

b <- c()
for(i in 1:length(predicted.classes)){
  wr <- predicted.classes[1:i] == teste19$Win[1:i]
  if(!is.na(predicted.classes[i])){
    b[i] <- mean(wr, na.rm=T)
  }else{
    b[i] <- NaN
  }
}

plot(b, main="Porcentagem de acerto por numero do jogo",sub="temporada 06/07 a 17/18 prevendo 18/19",
     xlab="#Jogo", ylab="% acerto previsao", ylim=c(0,1))
abline(v=414, lty=2, col=2)
abline(v=1230,lty=2, col=3)
text(500,0.8,"ultimo NA")
text(600,0.1,paste("% acerto = ",round(mean(predicted.classes == teste19$Win, na.rm=T),3),sep=""))
text(600,1,"Reg Logistica",cex=1.5)

####################

load(file="final.rda")
final <- rbind(final2008,final2009,final2010,final2011,final2012,final2013,final2014,final2015,final2016,final2017,final2018)
final <- final[,-c(5,8,11,14,16:18,23:25,42:53,78:89,106:109,117,120,123,132:134,139:141,143:154,179:190,215:218)]
teste19 <- final2019[1:1230,-c(5,8,11,14,16:18,23:25,42:53,78:89,106:109,117,120,123,132:134,139:141,143:154,179:190,215:218)]

mod <- glm(Win~., data=final[,-2], family=binomial(link = "logit"))
library(dplyr)
probabilities <- mod %>% predict(teste19[,-2], type = "response")
predicted.classes <- ifelse(probabilities > 0.5, "TRUE", "FALSE")
mean(predicted.classes == teste19$Win, na.rm=T)

b <- c()
for(i in 1:length(predicted.classes)){
  wr <- predicted.classes[1:i] == teste19$Win[1:i]
  if(!is.na(predicted.classes[i])){
    b[i] <- mean(wr, na.rm=T)
  }else{
    b[i] <- NaN
  }
}

plot(b, main="Porcentagem de acerto por numero do jogo",sub="temporada 07/08 a 17/18 prevendo 18/19",
     xlab="#Jogo", ylab="% acerto previsao", ylim=c(0,1))
abline(v=414, lty=2, col=2)
abline(v=1230,lty=2, col=3)
text(500,0.8,"ultimo NA")
text(600,0.1,paste("% acerto = ",round(mean(predicted.classes == teste19$Win, na.rm=T),3),sep=""))
text(600,1,"Reg Logistica",cex=1.5)

#last15 <- c()
#j <- 1
#for(i in 20:length(predicted.classes)){
#  wr <- predicted.classes[j:i] == teste19$Win[j:i]
#  if(sum(is.na(wr)) == 0){
#    last15[i] <- mean(wr)
#  }else{
#    last15[i] <- NaN
#  }
#  j <- j+1
#}

#plot(last15, main="Porcentagem de acerto por numero do jogo",sub="temporada 08/09 a 17/18 prevendo 18/19",
#     xlab="#Jogo", ylab="% acerto previsao", ylim=c(0,1))
#abline(v=443, lty=2, col=2)
#abline(v=1230,lty=2, col=3)
#text(500,0.8,"ultimo NA")
#text(1200,0.8,"playoffs >")
#text(600,0.1,"% final playoffs = 0.644")
#text(600,0.2,"% final regular = 0.646")
#text(600,1,"Reg Logistica",cex=1.5)

#locator()

####################

load(file="final.rda")
final <- rbind(final2009,final2010,final2011,final2012,final2013,final2014,final2015,final2016,final2017,final2018)
final <- final[,-c(5,8,11,14,16:18,23:25,42:53,78:89,106:109,117,120,123,132:134,139:141,143:154,179:190,215:218)]
teste19 <- final2019[1:1230,-c(5,8,11,14,16:18,23:25,42:53,78:89,106:109,117,120,123,132:134,139:141,143:154,179:190,215:218)]

mod <- glm(Win~., data=final[,-2], family=binomial(link = "logit"))
library(dplyr)
probabilities <- mod %>% predict(teste19[,-2], type = "response")
predicted.classes <- ifelse(probabilities > 0.5, "TRUE", "FALSE")
mean(predicted.classes == teste19$Win, na.rm=T)

b <- c()
for(i in 1:length(predicted.classes)){
  wr <- predicted.classes[1:i] == teste19$Win[1:i]
  if(!is.na(predicted.classes[i])){
    b[i] <- mean(wr, na.rm=T)
  }else{
    b[i] <- NaN
  }
}

plot(b, main="Porcentagem de acerto por numero do jogo",sub="temporada 08/09 a 17/18 prevendo 18/19",
     xlab="#Jogo", ylab="% acerto previsao", ylim=c(0,1))
abline(v=414, lty=2, col=2)
abline(v=1230,lty=2, col=3)
text(500,0.8,"ultimo NA")
text(600,0.1,paste("% acerto = ",round(mean(predicted.classes == teste19$Win, na.rm=T),3),sep=""))
text(600,1,"Reg Logistica",cex=1.5)

####################

load(file="final.rda")
final <- rbind(final2010,final2011,final2012,final2013,final2014,final2015,final2016,final2017,final2018)
final <- final[,-c(5,8,11,14,16:18,23:25,42:53,78:89,106:109,117,120,123,132:134,139:141,143:154,179:190,215:218)]
teste19 <- final2019[1:1230,-c(5,8,11,14,16:18,23:25,42:53,78:89,106:109,117,120,123,132:134,139:141,143:154,179:190,215:218)]

mod <- glm(Win~., data=final[,-2], family=binomial(link = "logit"))
library(dplyr)
probabilities <- mod %>% predict(teste19[,-2], type = "response")
predicted.classes <- ifelse(probabilities > 0.5, "TRUE", "FALSE")
mean(predicted.classes == teste19$Win, na.rm=T)

b <- c()
for(i in 1:length(predicted.classes)){
  wr <- predicted.classes[1:i] == teste19$Win[1:i]
  if(!is.na(predicted.classes[i])){
    b[i] <- mean(wr, na.rm=T)
  }else{
    b[i] <- NaN
  }
}

plot(b, main="Porcentagem de acerto por numero do jogo",sub="temporada 09/10 a 17/18 prevendo 18/19",
     xlab="#Jogo", ylab="% acerto previsao", ylim=c(0,1))
abline(v=414, lty=2, col=2)
abline(v=1230,lty=2, col=3)
text(500,0.8,"ultimo NA")
text(600,0.1,paste("% acerto = ",round(mean(predicted.classes == teste19$Win, na.rm=T),3),sep=""))
text(600,1,"Reg Logistica",cex=1.5)

####################

load(file="final.rda")
final <- rbind(final2011,final2012,final2013,final2014,final2015,final2016,final2017,final2018)
final <- final[,-c(5,8,11,14,16:18,23:25,42:53,78:89,106:109,117,120,123,132:134,139:141,143:154,179:190,215:218)]
teste19 <- final2019[1:1230,-c(5,8,11,14,16:18,23:25,42:53,78:89,106:109,117,120,123,132:134,139:141,143:154,179:190,215:218)]

mod <- glm(Win~., data=final[,-2], family=binomial(link = "logit"))
library(dplyr)
probabilities <- mod %>% predict(teste19[,-2], type = "response")
predicted.classes <- ifelse(probabilities > 0.5, "TRUE", "FALSE")
mean(predicted.classes == teste19$Win, na.rm=T)

b <- c()
for(i in 1:length(predicted.classes)){
  wr <- predicted.classes[1:i] == teste19$Win[1:i]
  if(!is.na(predicted.classes[i])){
    b[i] <- mean(wr, na.rm=T)
  }else{
    b[i] <- NaN
  }
}

plot(b, main="Porcentagem de acerto por numero do jogo",sub="temporada 10/11 a 17/18 prevendo 18/19",
     xlab="#Jogo", ylab="% acerto previsao", ylim=c(0,1))
abline(v=414, lty=2, col=2)
abline(v=1230,lty=2, col=3)
text(500,0.8,"ultimo NA")
text(600,0.1,paste("% acerto = ",round(mean(predicted.classes == teste19$Win, na.rm=T),3),sep=""))
text(600,1,"Reg Logistica",cex=1.5)

####################

load(file="final.rda")
final <- rbind(final2012,final2013,final2014,final2015,final2016,final2017,final2018)
final <- final[,-c(5,8,11,14,16:18,23:25,42:53,78:89,106:109,117,120,123,132:134,139:141,143:154,179:190,215:218)]
teste19 <- final2019[1:1230,-c(5,8,11,14,16:18,23:25,42:53,78:89,106:109,117,120,123,132:134,139:141,143:154,179:190,215:218)]

mod <- glm(Win~., data=final[,-2], family=binomial(link = "logit"))
library(dplyr)
probabilities <- mod %>% predict(teste19[,-2], type = "response")
predicted.classes <- ifelse(probabilities > 0.5, "TRUE", "FALSE")
mean(predicted.classes == teste19$Win, na.rm=T)

b <- c()
for(i in 1:length(predicted.classes)){
  wr <- predicted.classes[1:i] == teste19$Win[1:i]
  if(!is.na(predicted.classes[i])){
    b[i] <- mean(wr, na.rm=T)
  }else{
    b[i] <- NaN
  }
}

plot(b, main="Porcentagem de acerto por numero do jogo",sub="temporada 11/12 a 17/18 prevendo 18/19",
     xlab="#Jogo", ylab="% acerto previsao", ylim=c(0,1))
abline(v=414, lty=2, col=2)
abline(v=1230,lty=2, col=3)
text(500,0.8,"ultimo NA")
text(600,0.1,paste("% acerto = ",round(mean(predicted.classes == teste19$Win, na.rm=T),3),sep=""))
text(600,1,"Reg Logistica",cex=1.5)

####################

load(file="final.rda")
final <- rbind(final2013,final2014,final2015,final2016,final2017,final2018)
final <- final[,-c(5,8,11,14,16:18,23:25,42:53,78:89,106:109,117,120,123,132:134,139:141,143:154,179:190,215:218)]
teste19 <- final2019[1:1230,-c(5,8,11,14,16:18,23:25,42:53,78:89,106:109,117,120,123,132:134,139:141,143:154,179:190,215:218)]

mod <- glm(Win~., data=final[,-2], family=binomial(link = "logit"))
library(dplyr)
probabilities <- mod %>% predict(teste19[,-2], type = "response")
predicted.classes <- ifelse(probabilities > 0.5, "TRUE", "FALSE")
mean(predicted.classes == teste19$Win, na.rm=T)

b <- c()
for(i in 1:length(predicted.classes)){
  wr <- predicted.classes[1:i] == teste19$Win[1:i]
  if(!is.na(predicted.classes[i])){
    b[i] <- mean(wr, na.rm=T)
  }else{
    b[i] <- NaN
  }
}

plot(b, main="Porcentagem de acerto por numero do jogo",sub="temporada 12/13 a 17/18 prevendo 18/19",
     xlab="#Jogo", ylab="% acerto previsao", ylim=c(0,1))
abline(v=414, lty=2, col=2)
abline(v=1230,lty=2, col=3)
text(500,0.8,"ultimo NA")
text(600,0.1,paste("% acerto = ",round(mean(predicted.classes == teste19$Win, na.rm=T),3),sep=""))
text(600,1,"Reg Logistica",cex=1.5)

####################

load(file="final.rda")
final <- rbind(final2014,final2015,final2016,final2017,final2018)
final <- final[,-c(5,8,11,14,16:18,23:25,42:53,78:89,106:109,117,120,123,132:134,139:141,143:154,179:190,215:218)]
teste19 <- final2019[1:1230,-c(5,8,11,14,16:18,23:25,42:53,78:89,106:109,117,120,123,132:134,139:141,143:154,179:190,215:218)]

mod <- glm(Win~., data=final[,-2], family=binomial(link = "logit"))
probabilities <- mod %>% predict(teste19[,-2], type = "response")
predicted.classes <- ifelse(probabilities > 0.5, "TRUE", "FALSE")
mean(predicted.classes == teste19$Win, na.rm=T)

b <- c()
for(i in 1:length(predicted.classes)){
  wr <- predicted.classes[1:i] == teste19$Win[1:i]
  if(!is.na(predicted.classes[i])){
    b[i] <- mean(wr, na.rm=T)
  }else{
    b[i] <- NaN
  }
}

plot(b, main="Porcentagem de acerto por numero do jogo",sub="temporada 13/14 a 17/18 prevendo 18/19",
     xlab="#Jogo", ylab="% acerto previsao", ylim=c(0,1))
abline(v=414, lty=2, col=2)
abline(v=1230,lty=2, col=3)
text(500,0.8,"ultimo NA")
text(600,0.1,paste("% acerto = ",round(mean(predicted.classes == teste19$Win, na.rm=T),3),sep=""))
text(600,1,"Reg Logistica",cex=1.5)

####################

load(file="final.rda")
final <- rbind(final2015,final2016,final2017,final2018)
final <- final[,-c(5,8,11,14,16:18,23:25,42:53,78:89,106:109,117,120,123,132:134,139:141,143:154,179:190,215:218)]
teste19 <- final2019[1:1230,-c(5,8,11,14,16:18,23:25,42:53,78:89,106:109,117,120,123,132:134,139:141,143:154,179:190,215:218)]

mod <- glm(Win~., data=final[,-2], family=binomial(link = "logit"))
probabilities <- mod %>% predict(teste19[,-2], type = "response")
predicted.classes <- ifelse(probabilities > 0.5, "TRUE", "FALSE")
mean(predicted.classes == teste19$Win, na.rm=T)

b <- c()
for(i in 1:length(predicted.classes)){
  wr <- predicted.classes[1:i] == teste19$Win[1:i]
  if(!is.na(predicted.classes[i])){
    b[i] <- mean(wr, na.rm=T)
  }else{
    b[i] <- NaN
  }
}

plot(b, main="Porcentagem de acerto por numero do jogo",sub="temporada 14/15 a 17/18 prevendo 18/19",
     xlab="#Jogo", ylab="% acerto previsao", ylim=c(0,1))
abline(v=414, lty=2, col=2)
abline(v=1230,lty=2, col=3)
text(500,0.8,"ultimo NA")
text(600,0.1,paste("% acerto = ",round(mean(predicted.classes == teste19$Win, na.rm=T),3),sep=""))
text(600,1,"Reg Logistica",cex=1.5)

####################

load(file="final.rda")
final <- rbind(final2016,final2017,final2018)
final <- final[,-c(5,8,11,14,16:18,23:25,42:53,78:89,106:109,117,120,123,132:134,139:141,143:154,179:190,215:218)]
teste19 <- final2019[1:1230,-c(5,8,11,14,16:18,23:25,42:53,78:89,106:109,117,120,123,132:134,139:141,143:154,179:190,215:218)]

mod <- glm(Win~., data=final[,-2], family=binomial(link = "logit"))
probabilities <- mod %>% predict(teste19[,-2], type = "response")
predicted.classes <- ifelse(probabilities > 0.5, "TRUE", "FALSE")
mean(predicted.classes == teste19$Win, na.rm=T)

b <- c()
for(i in 1:length(predicted.classes)){
  wr <- predicted.classes[1:i] == teste19$Win[1:i]
  if(!is.na(predicted.classes[i])){
    b[i] <- mean(wr, na.rm=T)
  }else{
    b[i] <- NaN
  }
}

plot(b, main="Porcentagem de acerto por numero do jogo",sub="temporada 15/16 a 17/18 prevendo 18/19",
     xlab="#Jogo", ylab="% acerto previsao", ylim=c(0,1))
abline(v=414, lty=2, col=2)
abline(v=1230,lty=2, col=3)
text(500,0.8,"ultimo NA")
text(600,0.1,paste("% acerto = ",round(mean(predicted.classes == teste19$Win, na.rm=T),3),sep=""))
text(600,1,"Reg Logistica",cex=1.5)

####################

load(file="final.rda")
final <- rbind(final2017,final2018)
final <- final[,-c(5,8,11,14,16:18,23:25,42:53,78:89,106:109,117,120,123,132:134,139:141,143:154,179:190,215:218)]
teste19 <- final2019[1:1230,-c(5,8,11,14,16:18,23:25,42:53,78:89,106:109,117,120,123,132:134,139:141,143:154,179:190,215:218)]

mod <- glm(Win~., data=final[,-2], family=binomial(link = "logit"))
probabilities <- mod %>% predict(teste19[,-2], type = "response")
predicted.classes <- ifelse(probabilities > 0.5, "TRUE", "FALSE")
mean(predicted.classes == teste19$Win, na.rm=T)

b <- c()
for(i in 1:length(predicted.classes)){
  wr <- predicted.classes[1:i] == teste19$Win[1:i]
  if(!is.na(predicted.classes[i])){
    b[i] <- mean(wr, na.rm=T)
  }else{
    b[i] <- NaN
  }
}

plot(b, main="Porcentagem de acerto por numero do jogo",sub="temporada 16/17 a 17/18 prevendo 18/19",
     xlab="#Jogo", ylab="% acerto previsao", ylim=c(0,1))
abline(v=414, lty=2, col=2)
abline(v=1230,lty=2, col=3)
text(500,0.8,"ultimo NA")
text(600,0.1,paste("% acerto = ",round(mean(predicted.classes == teste19$Win, na.rm=T),3),sep=""))
text(600,1,"Reg Logistica",cex=1.5)

####################

load(file="final.rda")
final <- rbind(final2018)
final <- final[,-c(5,8,11,14,16:18,23:25,42:53,78:89,106:109,117,120,123,132:134,139:141,143:154,179:190,215:218)]
teste19 <- final2019[1:1230,-c(5,8,11,14,16:18,23:25,42:53,78:89,106:109,117,120,123,132:134,139:141,143:154,179:190,215:218)]

mod <- glm(Win~., data=final[,-2], family=binomial(link = "logit"))
probabilities <- mod %>% predict(teste19[,-2], type = "response")
predicted.classes <- ifelse(probabilities > 0.5, "TRUE", "FALSE")
mean(predicted.classes == teste19$Win, na.rm=T)

b <- c()
for(i in 1:length(predicted.classes)){
  wr <- predicted.classes[1:i] == teste19$Win[1:i]
  if(!is.na(predicted.classes[i])){
    b[i] <- mean(wr, na.rm=T)
  }else{
    b[i] <- NaN
  }
}

plot(b, main="Porcentagem de acerto por numero do jogo",sub="temporada 17/18 prevendo 18/19",
     xlab="#Jogo", ylab="% acerto previsao", ylim=c(0,1))
abline(v=414, lty=2, col=2)
abline(v=1230,lty=2, col=3)
text(500,0.8,"ultimo NA")
text(600,0.1,paste("% acerto = ",round(mean(predicted.classes == teste19$Win, na.rm=T),3),sep=""))
text(600,1,"Reg Logistica",cex=1.5)