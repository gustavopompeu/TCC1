setwd("C:/Gustavo/Trabalhos/UnB/TCC/LATEX/tcc_git")

load(file="jogos.rda")
load(file="bases.rda")
load(file="final.rda")

#criando a base grande com as temporadas para modelagem
final <- data.frame()
for(i in 2007:2018){
  assign("base",get(paste("final",i,sep="")))
  if(i == 2001 | i == 2002 | i == 2003 | i == 2004){
    final <- rbind(final,base[1:1189,])
  }else if(i == 2012){
    final <- rbind(final,base[1:990,])
  }else{
    final <- rbind(final,base[1:1230,])
  }
}

#1 win
#2 result

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

#Reg Linear
vet <- c()
start_time <- Sys.time()
for(i in 1:length(nomes)){
  full19 <- oi19==nomes[i]
  td19 <- teste19[full19,]
  colun <- !is.na(td19[1,])
  td19 <- td19[,colun]
  td <- final[,colun]
  td <- td[rowSums(is.na(td)) == 0,]
  mod <- lm(result_Vis~., data=td[,-1])
  win <- (predict(mod, newdata=td19[,-1]) > 0) == (td19$result_Vis > 0)
  vet <- c(vet,win)
}
end_time <- Sys.time()
tempo <- end_time - start_time
tempo
as.numeric(tempo)
mean(vet)

#Reg Logistica
vet1 <- c()
start_time <- Sys.time()
library(dplyr)
for(i in 1:length(nomes)){
  full19 <- oi19==nomes[i]
  td19 <- teste19[full19,]
  colun <- !is.na(td19[1,])
  td19 <- td19[,colun]
  td <- final[,colun]
  td <- td[rowSums(is.na(td)) == 0,]
  a <- glm(Win_Vis~., data = td[,-2], family=binomial(link = "logit"))
  probabilities <- a %>% predict(td19[,-2], type = "response")
  predicted.classes <- ifelse(probabilities > 0.5, "TRUE", "FALSE")
  win <- predicted.classes == td19$Win_Vis
  vet1 <- c(vet1,win)
}
end_time <- Sys.time()
tempo <- end_time - start_time
tempo
as.numeric(tempo)
mean(vet1)

#Probit
vet2 <- c()
start_time <- Sys.time()
library(dplyr)
for(i in 1:length(nomes)){
  full19 <- oi19==nomes[i]
  td19 <- teste19[full19,]
  colun <- !is.na(td19[1,])
  td19 <- td19[,colun]
  td <- final[,colun]
  td <- td[rowSums(is.na(td)) == 0,]
  a <- glm(Win_Vis~., data = td[,-2], family=binomial(link = "probit"))
  probabilities <- a %>% predict(td19[,-2], type = "response")
  predicted.classes <- ifelse(probabilities > 0.5, "TRUE", "FALSE")
  win <- predicted.classes == td19$Win_Vis
  vet2 <- c(vet2,win)
}
end_time <- Sys.time()
tempo <- end_time - start_time
tempo
as.numeric(tempo)
mean(vet2)

#SVM parametros mudados
vet3 <- c()
library(e1071)
for(i in 1:length(nomes)){
  if(i==1){
  full19 <- oi19==nomes[i]
  td19 <- teste19[full19,]
  colun <- !is.na(td19[1,])
  td19 <- td19[,colun]
  td <- final[,colun]
  td <- td[rowSums(is.na(td)) == 0,]
  mod <- svm(result_Vis~., data=td[,-1], cost=3, gamma=10^-4)
  win <- (predict(mod, newdata=td19[,-1]) > 0) == (td19$result_Vis > 0)
  vet3 <- c(vet3,win)
  }
}
mean(vet3)

#SVM
vet3.1 <- c()
start_time <- Sys.time()
library(e1071)
for(i in 1:length(nomes)){
  full19 <- oi19==nomes[i]
  td19 <- teste19[full19,]
  colun <- !is.na(td19[1,])
  td19 <- td19[,colun]
  td <- final[,colun]
  td <- td[rowSums(is.na(td)) == 0,]
  mod <- svm(result_Vis~., data=td[,-1])
  win <- (predict(mod, newdata=td19[,-1]) > 0) == (td19$result_Vis > 0)
  vet3.1 <- c(vet3.1,win)
}
end_time <- Sys.time()
tempo <- end_time - start_time
tempo
as.numeric(tempo)*60*60

mean(vet3.1)

#LDA
vet4 <- c()
start_time <- Sys.time()
library(MASS)
for(i in 1:length(nomes)){
  full19 <- oi19==nomes[i]
  td19 <- teste19[full19,]
  colun <- !is.na(td19[1,])
  td19 <- td19[,colun]
  td <- final[,colun]
  td <- td[rowSums(is.na(td)) == 0,]
  mod <- lda(Win_Vis~., data=td[,-2])
  prd <- predict(mod, newdata=td19[,-2])
  win <- as.logical(prd$class) == td19$Win_Vis
  vet4 <- c(vet4,win)
}
end_time <- Sys.time()
tempo <- end_time - start_time
tempo
as.numeric(tempo)
mean(vet4)

#random forest
vet5 <- c()
start_time <- Sys.time()
library(randomForest)
for(i in 1:length(nomes)){
  full19 <- oi19==nomes[i]
  td19 <- teste19[full19,]
  colun <- !is.na(td19[1,])
  td19 <- td19[,colun]
  td <- final[,colun]
  td <- td[rowSums(is.na(td)) == 0,]
  mod <- randomForest(result_Vis~., data=td[,-1])
  win <- (predict(mod, newdata=td19[,-1]) > 0) == (td19$result_Vis > 0)
  vet5 <- c(vet5,win)
}
end_time <- Sys.time()
tempo <- end_time - start_time
tempo
as.numeric(tempo)*60*60
mean(vet5)

#reg arvore
vet6 <- c()
start_time <- Sys.time()
library(tree)
for(i in 1:length(nomes)){
  full19 <- oi19==nomes[i]
  td19 <- teste19[full19,]
  colun <- !is.na(td19[1,])
  td19 <- td19[,colun]
  td <- final[,colun]
  td <- td[rowSums(is.na(td)) == 0,]
  mod <- tree(result_Vis~., data=td[,-1])
  win <- (predict(mod, newdata=td19[,-1]) > 0) == (td19$result_Vis > 0)
  vet6 <- c(vet6,win)
}
end_time <- Sys.time()
tempo <- end_time - start_time
tempo
as.numeric(tempo)
mean(vet6)

#class em arvore
vet7 <- c()
start_time <- Sys.time()
library(tree)
for(i in 1:length(nomes)){
  full19 <- oi19==nomes[i]
  td19 <- teste19[full19,]
  colun <- !is.na(td19[1,])
  td19 <- td19[,colun]
  td <- final[,colun]
  td <- td[rowSums(is.na(td)) == 0,]
  mod <- tree(Win_Vis~., data=td[,-2])
  probabilities <- predict(mod, newdata=td19[,-2])
  predicted.classes <- ifelse(probabilities > 0.5, "TRUE", "FALSE")
  win <- predicted.classes == td19$Win_Vis
  vet7 <- c(vet7,win)
}
end_time <- Sys.time()
tempo <- end_time - start_time
tempo
as.numeric(tempo)
mean(vet7)

#Reg Linear c/ forward
vet8 <- c()
start_time <- Sys.time()
for(i in 1:length(nomes)){
  full19 <- oi19==nomes[i]
  td19 <- teste19[full19,]
  colun <- !is.na(td19[1,])
  td19 <- td19[,colun]
  td <- final[,colun]
  td <- td[rowSums(is.na(td)) == 0,]
  modmin <- lm(result_Vis ~ 1, data=td[,-1])
  mod <- step(modmin, direction = "forward", scope=(as.formula(td[,-1])), trace=0)
  win <- (predict(mod, newdata=td19[,-1]) > 0) == (td19$result_Vis> 0)
  vet8 <- c(vet8,win)
}
end_time <- Sys.time()
tempo <- end_time - start_time
tempo
as.numeric(tempo)*60
mean(vet8)

#Reg Logistica c/ forward
vet9 <- c()
start_time <- Sys.time()
library(dplyr)
for(i in 1:length(nomes)){
  full19 <- oi19==nomes[i]
  td19 <- teste19[full19,]
  colun <- !is.na(td19[1,])
  td19 <- td19[,colun]
  td <- final[,colun]
  td <- td[rowSums(is.na(td)) == 0,]
  modmin <- glm(Win_Vis~1, data = td[,-2], family=binomial(link = "logit"))
  a <- step(modmin, direction = "forward", scope=(as.formula(td[,-2])), trace=0)
  probabilities <- a %>% predict(td19[,-2], type = "response")
  predicted.classes <- ifelse(probabilities > 0.5, "TRUE", "FALSE")
  win <- predicted.classes == td19$Win_Vis
  vet9 <- c(vet9,win)
}
end_time <- Sys.time()
tempo <- end_time - start_time
tempo
as.numeric(tempo)*60*60
mean(vet9)

#Reg Probit c/ forward
vet10 <- c()
start_time <- Sys.time()
library(dplyr)
for(i in 1:length(nomes)){
  full19 <- oi19==nomes[i]
  td19 <- teste19[full19,]
  colun <- !is.na(td19[1,])
  td19 <- td19[,colun]
  td <- final[,colun]
  td <- td[rowSums(is.na(td)) == 0,]
  modmin <- glm(Win_Vis~1, data = td[,-2], family=binomial(link = "probit"))
  a <- step(modmin, direction = "forward", scope=(as.formula(td[,-2])), trace=0)
  probabilities <- a %>% predict(td19[,-2], type = "response")
  predicted.classes <- ifelse(probabilities > 0.5, "TRUE", "FALSE")
  win <- predicted.classes == td19$Win_Vis
  vet10 <- c(vet10,win)
}
end_time <- Sys.time()
tempo <- end_time - start_time
tempo
as.numeric(tempo)*60*60
mean(vet10)

####standardize
full19 <- oi19==nomes[1]
td19 <- teste19[full19,]
colun <- !is.na(td19[1,])
td19 <- td19[,colun]
td <- final[,colun]
td <- td[rowSums(is.na(td)) == 0,]
a <- glm(Win_Vis~., data = td[,-2], family=binomial(link = "logit"))
summary(a)

library(standardize)
pad <- standardize(formula = as.formula(td[,-2]), data = td[, -2], family=binomial(link = "logit"))
novo <- pad$data

pad19 <- standardize(formula = as.formula(td19[,-2]), data = td19[, -2], family=binomial(link = "logit"))
novo19 <- pad19$data

modnovo <- glm(Win_Vis~., data = novo, family=binomial(link = "logit"))
summary(modnovo)

library(dplyr)
probabilities <- modnovo %>% predict(novo19, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, "TRUE", "FALSE")
win <- predicted.classes == td19$Win_Vis
mean(win)


####standardize teste todos
vet11 <- c()
for(i in 1:length(nomes)){
full19 <- oi19==nomes[i]
td19 <- teste19[full19,]
colun <- !is.na(td19[1,])
td19 <- td19[,colun]
td <- final[,colun]
td <- td[rowSums(is.na(td)) == 0,]
a <- glm(Win_Vis~., data = td[,-2], family=binomial(link = "logit"))
summary(a)

library(standardize)
pad <- standardize(formula = as.formula(td[,-2]), data = td[, -2], family=binomial(link = "logit"))
novo <- pad$data

pad19 <- standardize(formula = as.formula(td19[,-2]), data = td19[, -2], family=binomial(link = "logit"))
novo19 <- pad19$data

modnovo <- glm(Win_Vis~., data = novo, family=binomial(link = "logit"))
summary(modnovo)

library(dplyr)
probabilities <- modnovo %>% predict(novo19, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, "TRUE", "FALSE")
win <- predicted.classes == td19$Win_Vis
vet11 <- c(vet11,win)
}
mean(vet11)

#melhores parametros para o svm
library(e1071)
tuneResult <- tune(svm, result_Vis ~ ., data = td[,-1],
                   ranges = list(gamma=c(10^-5,10^-4,10^-3), cost = c(3)))

#10^-4 e 3