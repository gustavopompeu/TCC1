teste19 <- final2019[1:1230,-c(5,8,11,14,16:18,23:25,42:53,78:89,106:109,117,120,123,132:134,139:141,143:154,179:190,215:218)]

prev_temp_log <- function(temp_inicio){
  final <- data.frame()
  for(i in temp_inicio){
    assign("base",get(paste("final",i,sep="")))
    if(i == 2001 | i == 2002 | i == 2003 | i == 2004){
      final <- rbind(final,base[1:1189,])
    }else if(i == 2012){
      final <- rbind(final,base[1:990,])
    }else{
      final <- rbind(final,base[1:1230,])
    }
  }
  
  #tirando variaveis de casa pro time de fora e de fora pro time de casa
  final <- final[,-c(5,8,11,14,16:18,23:25,42:53,78:89,106:109,117,120,123,132:134,139:141,143:154,179:190,215:218)]
  
  #encontrando os padroes
  teste19 <- final2019[1:1230,-c(5,8,11,14,16:18,23:25,42:53,78:89,106:109,117,120,123,132:134,139:141,143:154,179:190,215:218)]
  padrao19 <- is.na(teste19[,-c(1,2)])
  
  for(i in 1:length(teste19$Win)){
    for(j in 1:ncol(padrao19)){
      padrao19[i,j] <- as.numeric(padrao19[i,j])
    }
  }
  
  oi19 <- apply(padrao19,1,paste,collapse="")
  nomes <- names(summary(as.factor(oi19)))
  
  vet <- c()
  for(i in 1:length(nomes)){
    library(dplyr)
    full19 <- oi19==nomes[i]
    td19 <- teste19[full19,]
    colun <- !is.na(td19[1,])
    td19 <- td19[,colun]
    td <- final[,colun]
    td <- td[rowSums(is.na(td)) == 0,]
    a <- glm(Win~., data = td[,-2], family=binomial(link = "probit"))
    probabilities <- a %>% predict(td19[,-2], type = "response")
    predicted.classes <- ifelse(probabilities > 0.5, "TRUE", "FALSE")
    vet <- c(vet,predicted.classes)
  }
  
  ord <- match(rownames(teste19), names(vet))
  vet <- vet[ord]
  return(vet)
}

####################

prev_log <- prev_temp_log(2001)

b <- c()
for(i in 1:length(prev)){
  wr <- prev_log[1:i] == teste19$Win[1:i]
  b[i] <- mean(wr)
}

plot(b, main="Porcentagem de acerto por numero do jogo",sub="temporada 00/01 a 17/18 prevendo 18/19",
     xlab="#Jogo", ylab="% acerto previsao", ylim=c(0,1))
text(600,0.1,paste("% acerto = ",round(b[1230],3),sep=""))
text(600,.95,"Reg Probit",cex=1.5)

####################

prev_log <- prev_temp_log(2002)

b <- c()
for(i in 1:length(prev)){
  wr <- prev_log[1:i] == teste19$Win[1:i]
  b[i] <- mean(wr)
}

plot(b, main="Porcentagem de acerto por numero do jogo",sub="temporada 01/02 a 17/18 prevendo 18/19",
     xlab="#Jogo", ylab="% acerto previsao", ylim=c(0,1))
text(600,0.1,paste("% acerto = ",round(b[1230],3),sep=""))
text(600,.95,"Reg Probit",cex=1.5)

####################

prev_log <- prev_temp_log(2003)

b <- c()
for(i in 1:length(prev)){
  wr <- prev_log[1:i] == teste19$Win[1:i]
  b[i] <- mean(wr)
}

plot(b, main="Porcentagem de acerto por numero do jogo",sub="temporada 02/03 a 17/18 prevendo 18/19",
     xlab="#Jogo", ylab="% acerto previsao", ylim=c(0,1))
text(600,0.1,paste("% acerto = ",round(b[1230],3),sep=""))
text(600,.95,"Reg Probit",cex=1.5)

####################

prev_log <- prev_temp_log(2004)

b <- c()
for(i in 1:length(prev)){
  wr <- prev_log[1:i] == teste19$Win[1:i]
  b[i] <- mean(wr)
}

plot(b, main="Porcentagem de acerto por numero do jogo",sub="temporada 03/04 a 17/18 prevendo 18/19",
     xlab="#Jogo", ylab="% acerto previsao", ylim=c(0,1))
text(600,0.1,paste("% acerto = ",round(b[1230],3),sep=""))
text(600,.95,"Reg Probit",cex=1.5)

####################

prev_log <- prev_temp_log(2005)

b <- c()
for(i in 1:length(prev)){
  wr <- prev_log[1:i] == teste19$Win[1:i]
  b[i] <- mean(wr)
}

plot(b, main="Porcentagem de acerto por numero do jogo",sub="temporada 04/05 a 17/18 prevendo 18/19",
     xlab="#Jogo", ylab="% acerto previsao", ylim=c(0,1))
text(600,0.1,paste("% acerto = ",round(b[1230],3),sep=""))
text(600,.95,"Reg Probit",cex=1.5)

####################

prev_log <- prev_temp_log(2006)

b <- c()
for(i in 1:length(prev)){
  wr <- prev_log[1:i] == teste19$Win[1:i]
  b[i] <- mean(wr)
}

plot(b, main="Porcentagem de acerto por numero do jogo",sub="temporada 05/06 a 17/18 prevendo 18/19",
     xlab="#Jogo", ylab="% acerto previsao", ylim=c(0,1))
text(600,0.1,paste("% acerto = ",round(b[1230],3),sep=""))
text(600,.95,"Reg Probit",cex=1.5)

####################

prev_log <- prev_temp_log(2007)

b <- c()
for(i in 1:length(prev)){
  wr <- prev_log[1:i] == teste19$Win[1:i]
  b[i] <- mean(wr)
}

plot(b, main="Porcentagem de acerto por numero do jogo",sub="temporada 06/07 a 17/18 prevendo 18/19",
     xlab="#Jogo", ylab="% acerto previsao", ylim=c(0,1))
text(600,0.1,paste("% acerto = ",round(b[1230],3),sep=""))
text(600,.95,"Reg Probit",cex=1.5)

####################

prev_log <- prev_temp_log(2008)

b <- c()
for(i in 1:length(prev)){
  wr <- prev_log[1:i] == teste19$Win[1:i]
  b[i] <- mean(wr)
}

plot(b, main="Porcentagem de acerto por numero do jogo",sub="temporada 07/08 a 17/18 prevendo 18/19",
     xlab="#Jogo", ylab="% acerto previsao", ylim=c(0,1))
text(600,0.1,paste("% acerto = ",round(b[1230],3),sep=""))
text(600,.95,"Reg Probit",cex=1.5)

####################

prev_log <- prev_temp_log(2009)

b <- c()
for(i in 1:length(prev)){
  wr <- prev_log[1:i] == teste19$Win[1:i]
  b[i] <- mean(wr)
}

plot(b, main="Porcentagem de acerto por numero do jogo",sub="temporada 08/09 a 17/18 prevendo 18/19",
     xlab="#Jogo", ylab="% acerto previsao", ylim=c(0,1))
text(600,0.1,paste("% acerto = ",round(b[1230],3),sep=""))
text(600,.95,"Reg Probit",cex=1.5)

####################

prev_log <- prev_temp_log(2010)

b <- c()
for(i in 1:length(prev)){
  wr <- prev_log[1:i] == teste19$Win[1:i]
  b[i] <- mean(wr)
}

plot(b, main="Porcentagem de acerto por numero do jogo",sub="temporada 09/10 a 17/18 prevendo 18/19",
     xlab="#Jogo", ylab="% acerto previsao", ylim=c(0,1))
text(600,0.1,paste("% acerto = ",round(b[1230],3),sep=""))
text(600,.95,"Reg Probit",cex=1.5)

####################

prev_log <- prev_temp_log(2011)

b <- c()
for(i in 1:length(prev)){
  wr <- prev_log[1:i] == teste19$Win[1:i]
  b[i] <- mean(wr)
}

plot(b, main="Porcentagem de acerto por numero do jogo",sub="temporada 10/11 a 17/18 prevendo 18/19",
     xlab="#Jogo", ylab="% acerto previsao", ylim=c(0,1))
text(600,0.1,paste("% acerto = ",round(b[1230],3),sep=""))
text(600,.95,"Reg Probit",cex=1.5)

####################

prev_log <- prev_temp_log(2012)

b <- c()
for(i in 1:length(prev)){
  wr <- prev_log[1:i] == teste19$Win[1:i]
  b[i] <- mean(wr)
}

plot(b, main="Porcentagem de acerto por numero do jogo",sub="temporada 11/12 a 17/18 prevendo 18/19",
     xlab="#Jogo", ylab="% acerto previsao", ylim=c(0,1))
text(600,0.1,paste("% acerto = ",round(b[1230],3),sep=""))
text(600,.95,"Reg Probit",cex=1.5)

####################

prev_log <- prev_temp_log(2013)

b <- c()
for(i in 1:length(prev)){
  wr <- prev_log[1:i] == teste19$Win[1:i]
  b[i] <- mean(wr)
}

plot(b, main="Porcentagem de acerto por numero do jogo",sub="temporada 12/13 a 17/18 prevendo 18/19",
     xlab="#Jogo", ylab="% acerto previsao", ylim=c(0,1))
text(600,0.1,paste("% acerto = ",round(b[1230],3),sep=""))
text(600,.95,"Reg Probit",cex=1.5)

####################

prev_log <- prev_temp_log(2014)

b <- c()
for(i in 1:length(prev)){
  wr <- prev_log[1:i] == teste19$Win[1:i]
  b[i] <- mean(wr)
}

plot(b, main="Porcentagem de acerto por numero do jogo",sub="temporada 13/14 a 17/18 prevendo 18/19",
     xlab="#Jogo", ylab="% acerto previsao", ylim=c(0,1))
text(600,0.1,paste("% acerto = ",round(b[1230],3),sep=""))
text(600,.95,"Reg Probit",cex=1.5)

####################

prev_log <- prev_temp_log(2015)

b <- c()
for(i in 1:length(prev)){
  wr <- prev_log[1:i] == teste19$Win[1:i]
  b[i] <- mean(wr)
}

plot(b, main="Porcentagem de acerto por numero do jogo",sub="temporada 14/15 a 17/18 prevendo 18/19",
     xlab="#Jogo", ylab="% acerto previsao", ylim=c(0,1))
text(600,0.1,paste("% acerto = ",round(b[1230],3),sep=""))
text(600,.95,"Reg Probit",cex=1.5)

####################

prev_log <- prev_temp_log(2016)

b <- c()
for(i in 1:length(prev)){
  wr <- prev_log[1:i] == teste19$Win[1:i]
  b[i] <- mean(wr)
}

plot(b, main="Porcentagem de acerto por numero do jogo",sub="temporada 15/16 a 17/18 prevendo 18/19",
     xlab="#Jogo", ylab="% acerto previsao", ylim=c(0,1))
text(600,0.1,paste("% acerto = ",round(b[1230],3),sep=""))
text(600,.95,"Reg Probit",cex=1.5)

####################

prev_log <- prev_temp_log(2017)

b <- c()
for(i in 1:length(prev)){
  wr <- prev_log[1:i] == teste19$Win[1:i]
  b[i] <- mean(wr)
}

plot(b, main="Porcentagem de acerto por numero do jogo",sub="temporada 16/17 a 17/18 prevendo 18/19",
     xlab="#Jogo", ylab="% acerto previsao", ylim=c(0,1))
text(600,0.1,paste("% acerto = ",round(b[1230],3),sep=""))
text(600,.95,"Reg Probit",cex=1.5)

####################

prev_log <- prev_temp_log(2018)

b <- c()
for(i in 1:length(prev)){
  wr <- prev_log[1:i] == teste19$Win[1:i]
  b[i] <- mean(wr)
}

plot(b, main="Porcentagem de acerto por numero do jogo",sub="temporada 17/18 prevendo 18/19",
     xlab="#Jogo", ylab="% acerto previsao", ylim=c(0,1))
text(600,0.1,paste("% acerto = ",round(b[1230],3),sep=""))
text(600,.95,"Reg Probit",cex=1.5)