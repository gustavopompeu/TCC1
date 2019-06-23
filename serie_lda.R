load(file="final.rda")
teste19 <- final2019[1:1230,]

prev_temp_lda <- function(temp_inicio){
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
     library(MASS)
     full19 <- oi19==nomes[i]
     td19 <- teste19[full19,]
     colun <- !is.na(td19[1,])
     td19 <- td19[,colun]
     td <- final[,colun]
     td <- td[rowSums(is.na(td)) == 0,]
     mod <- lda(Win_Vis~., data=td[,-2])
     prd <- predict(mod, newdata=td19[,-2])
     win <- as.logical(prd$class)
     names(win) <- rownames(td19)
     vet <- c(vet,win)
  }
  
  ord <- match(rownames(teste19), names(vet))
  vet <- vet[ord]
  return(vet)
}

####################
acerto_ano_lda <- c()
for(j in 2001:2018){
  prev_lda <- prev_temp_lda(j)
  acerto_ano_lda[(j-2000)] <- mean(prev_lda == teste19$Win_Vis)
}
names(acerto_ano_lda) <- 2001:2018
data.frame(acerto_ano_lda)
####################