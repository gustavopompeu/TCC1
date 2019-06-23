load(file="final.rda")
teste19 <- final2019[1:1230,]

prev_temp_prob <- function(temp_inicio){
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
    a <- glm(Win_Vis~., data = td[,-2], family=binomial(link = "probit"))
    probabilities <- a %>% predict(td19[,-2], type = "response")
    predicted.classes <- ifelse(probabilities > 0.5, "TRUE", "FALSE")
    vet <- c(vet,predicted.classes)
  }
  
  ord <- match(rownames(teste19), names(vet))
  vet <- vet[ord]
  return(vet)
}


####################
acerto_ano_prob <- c()
for(j in 2001:2018){
  prev_prob <- prev_temp_prob(j)
  acerto_ano_prob[(j-2000)] <- mean(prev_prob == teste19$Win_Vis)
}
names(acerto_ano_prob) <- 2001:2018
data.frame(acerto_ano_prob)
####################