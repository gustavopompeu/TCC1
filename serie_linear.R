load(file="final.rda")
teste19 <- final2019[1:1230,]

prev_temp <- function(temp_inicio){
  
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
    full19 <- oi19==nomes[i]
    td19 <- teste19[full19,]
    colun <- !is.na(td19[1,])
    td19 <- td19[,colun]
    td <- final[,colun]
    td <- td[rowSums(is.na(td)) == 0,]
    mod <- lm(result_Vis~., data=td[,-1])
    win <- predict(mod, newdata=td19[,-1])
    vet <- c(vet,win)
  }
  
  ord <- match(rownames(teste19), names(vet))
  vet <- vet[ord]
  return(vet)
}

####################
acerto_ano_lin <- c()
for(j in 2001:2018){
  prev_lin <- prev_temp(j)
  acerto_ano_lin[(j-2000)] <- mean((prev_lin > 0) == (teste19$result_Vis > 0))
}
names(acerto_ano_lin) <- 2001:2018
data.frame(acerto_ano_lin)
####################

load(file="lasvegas.rda")
overunder <- which(prev>200)
prev[overunder] <- NaN
prev_apostas <- prev

#comparacao dos saldos
prev_lin2 <- prev_temp(2007)

summary(abs(prev_lin2 - teste19$result_Vis))
summary(abs(prev_lin2 - prev_apostas))
summary(abs(prev_apostas - teste19$result_Vis))

mean((prev_lin2 - teste19$result_Vis)^2)
mean((prev_apostas - teste19$result_Vis)^2, na.rm = T)
mean((prev_lin2 - prev_apostas)^2, na.rm = T)
