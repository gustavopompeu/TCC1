setwd("C:/Gustavo/Trabalhos/UnB/TCC/LATEX/tcc_git")

load(file="jogos.rda")
load(file="bases.rda")
load(file="final.rda")

#criando a base grande com as temporadas para modelagem
final <- data.frame()
for(i in 2001:2018){
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

for(i in 1:length(teste19$Win)){
  for(j in 1:ncol(padrao19)){
    padrao19[i,j] <- as.numeric(padrao19[i,j])
  }
}

oi19 <- apply(padrao19,1,paste,collapse="")
nomes <- names(summary(as.factor(oi19)))

#Reg Linear
vet <- c()
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

mean(vet)

#Reg Logistica
vet1 <- c()
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

mean(vet1)

#Probit
vet2 <- c()
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

mean(vet2)

#SVM param mudados
vet3 <- c()
library(e1071)
for(i in 1:length(nomes)){
  full19 <- oi19==nomes[i]
  td19 <- teste19[full19,]
  colun <- !is.na(td19[1,])
  td19 <- td19[,colun]
  td <- final[,colun]
  td <- td[rowSums(is.na(td)) == 0,]
  mod <- svm(result_Vis~., data=td[,-1], cost=8, gamma=10^-4)
  win <- (predict(mod, newdata=td19[,-1]) > 0) == (td19$result_Vis > 0)
  vet3 <- c(vet3,win)
}

mean(vet3)

#SVM
vet3.1 <- c()
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

mean(vet3.1)

#LDA
vet4 <- c()
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

mean(vet4)

#random forest
vet5 <- c()
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

mean(vet5)

#reg arvore
vet6 <- c()
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

mean(vet6)

#class em arvore
vet7 <- c()
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

mean(vet7)

#Reg Linear c/ forward
vet8 <- c()
library(leaps)
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
mean(vet8)

#Reg Logistica c/ forward
vet9 <- c()
library(dplyr)
library(leaps)
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
mean(vet9)

#Reg Probit c/ forward
vet10 <- c()
library(dplyr)
library(leaps)
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
mean(vet10)

#######REG LINEAR
mod <- lm(result~., data=td[,-1])
summary(mod)
win <- (predict(mod, newdata=td19[,-1]) > 0) == (td19$result > 0)
mean(win)

library(standardize)
pad <- standardize(formula = result ~ ., data = td[, -74])

novo <- pad$data

####REG LOGISTICA
a <- glm(Win~., data = td[,-2], family=binomial(link = "logit"))
summary(a)
library(dplyr)
probabilities <- a %>% predict(td19[,-2], type = "response")
predicted.classes <- ifelse(probabilities > 0.5, "TRUE", "FALSE")
mean(predicted.classes == td19$Win)


#########SVM
library(e1071)
teste <- svm(result~., data=final[,-1], cost=8, gamma=10^-4)
summary(teste)
winsvm <- (predict(teste, newdata=teste19[,-1]) > 0) == (teste19$result > 0)
mean(winsvm)

teste19 <- teste19[rowSums(is.na(teste19)) == 0,]

###########RANDOM FOREST
require(randomForest)
mod <- randomForest(result~., data=td[,-1])
mod
mean((predict(mod, newdata=td19[,-1]) > 0) == (td19$result > 0))


####################TESTES - MUITO DEMORADO
#melhores parametros para o svm
tuneResult <- tune(svm, result_Vis ~ ., data = td[,-1],
                   ranges = list(gamma=c(10^-5,10^-4,10^-3), cost = c(7,8,9)))


####melhores modelos com x variaveis
library(leaps)
modelos<-regsubsets(result ~ .,data=td[,-72],nbest=1,nvmax=20,really.big=T)
summary(leaps)

###selecao stepwise
library(MASS)
stepAIC(mod, direction = "both")
