final_t <- data.frame()
for(i in 2007:2018){
  assign("base",get(paste("final",i,sep="")))
  if(i == 2001 | i == 2002 | i == 2003 | i == 2004){
    final_t <- rbind(final_t,base[1:1189,])
  }else if(i == 2012){
    final_t <- rbind(final_t,base[1:990,])
  }else{
    final_t <- rbind(final_t,base[1:1230,])
  }
}

#1 win
#2 result

datas <- unique(jogos_2019$Date[1:1230])
indices <- c()
for(i in 1:length(datas)){
  quais <- which(jogos_2019$Date == datas[i])
  indices[i] <- quais[length(quais)]
}

#Reg Linear
vet1 <- c()
library(dplyr)
for(i in 1:length(indices)){
  if(i > 1){
    final <- rbind(final_t[-(1:indices[i-1]),], final2019[1:indices[i-1],])
  }
  if(i==1){
    td19 <- teste19[1:indices[i],]
  }else{
    td19 <- teste19[(indices[i-1]+1):indices[i],]
  }
  padrao19 <- is.na(td19[,-c(1,2)])
  for(i in 1:length(td19$Win_Vis)){
    for(j in 1:ncol(padrao19)){
      padrao19[i,j] <- as.numeric(padrao19[i,j])
    }
  }
  oi19 <- apply(padrao19,1,paste,collapse="")
  nomes <- names(summary(as.factor(oi19)))
  for(j in 1:length(nomes)){
    full19 <- oi19==nomes[j]
    td19.2 <- td19[full19,]
    colun <- !is.na(td19.2[1,])
    td19.2 <- td19.2[,colun]
    td <- final[,colun]
    td <- td[rowSums(is.na(td)) == 0,]
    a <- glm(Win_Vis~., data = td[,-2], family=binomial(link = "logit"))
    probabilities <- a %>% predict(td19.2[,-2], type = "response")
    predicted.classes <- ifelse(probabilities > 0.5, "TRUE", "FALSE")
    win <- predicted.classes == td19.2$Win_Vis
    vet1 <- c(vet1,win)
  }
}

mean(vet1)