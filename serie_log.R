teste19 <- final2019[1:1230,]

prev_temp_log <- function(temp_inicio){
  
  final <- data.frame()
  for(i in temp_inicio:2018){
    assign("base",get(paste("final",i,sep="")))
    if(i == 2001 | i == 2002 | i == 2003 | i == 2004){
      final <- rbind(final,base[1:1189,])
    }else if(i == 2012){
      final <- rbind(final,base[1:990,])
    }else{
      final <- rbind(final,base[1:1230,])
    }
  }
  
  #encontrando os padroes
  teste19 <- final2019[1:1230,]
  padrao19 <- is.na(teste19[,-c(1,2)])
  
  for(i in 1:length(teste19$Win_Vis)){
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
    a <- glm(Win_Vis~., data = td[,-2], family=binomial(link = "logit"))
    probabilities <- a %>% predict(td19[,-2], type = "response")
    predicted.classes <- ifelse(probabilities > 0.5, "TRUE", "FALSE")
    vet <- c(vet,predicted.classes)
  }
  
  ord <- match(rownames(teste19), names(vet))
  vet <- vet[ord]
  return(vet)
}

####################
acerto_ano_log <- c()
for(j in 2001:2018){
  prev_log <- prev_temp_log(j)
  acerto_ano_log[(j-2000)] <- mean(prev_log == teste19$Win_Vis)
}
names(acerto_ano_log) <- 2001:2018
data.frame(acerto_ano_log)
####################


####################
prev_log <- prev_temp_log(2007)

b <- c()
for(i in 1:length(prev_log)){
  wr <- prev_log[1:i] == teste19$Win_Vis[1:i]
  b[i] <- mean(wr)
}

plot(b, main="Porcentagem de Acerto das Previsões Durante a Temporada",
     xlab="Número do Jogo", ylab="% Acerto", type="l", ylim=c(0.55,1))
text(600,0.85,paste("% Acerto Final = ",round(b[1230],3),sep=""))
text(600,.95,"Modelo campeão",cex=1.5)

num_last <- 61
ult <- c()
ult[1:(num_last-1)] <- NaN
for(j in num_last:1230){
  esp <- prev_log[(j-num_last+1):j] == teste19$Win_Vis[(j-num_last+1):j]
  ult[j] <- mean(esp)
}


plot(ult, type="l", xlim=c(0,1230), main="Porcentagem de Acerto das Previsões nos Últimos 61 Jogos",
     xlab="Número do Jogo", ylab="% Acerto", ylim=c(0.45,0.9))
text(600,.88,"Modelo campeão",cex=1.5)
abline(h=b[1230], lty=2, col="blue")
####################

1-mean(as.logical(prev_log))
