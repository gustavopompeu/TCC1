##Arrumando a base com os jogos

library(readxl)
oct_games <- read_excel("C:/Gustavo/Trabalhos/UnB/TCC/oct_games.xlsx")
oct_games <- as.data.frame(oct_games)
attach(oct_games)
oct_games$Attend <- as.numeric(gsub(",", "", Attend))
Sys.setlocale("LC_ALL","English")
oct_games$Date <- as.Date(Date, format="%a, %b %d, %Y")

str(oct_games)

##Criando base com dados

a <- data.frame()
j <- 1
for(i in 1:length(oct_games$PTS)){
#time 1 linha i
a[j,1] <- oct_games[i,2]
a[j,2] <- oct_games[i,4]
a[j,3] <- oct_games[i,3]
a[j,4] <- oct_games[i,5]
a[j,5] <- FALSE
a[j,6] <- 0
if(is.na(oct_games[i,6]) == T) a[j,7] <- FALSE; if(is.na(oct_games[i,6]) == F) a[j,7] <- TRUE
result <- a[j,3] - a[j,4]
if(result<0) a[j,8] <- FALSE; if(result>0) a[j,8] <- TRUE
if(i>1){
  temp <- oct_games[1:i,2] == a[j,1] | oct_games[1:i,4] == a[j,1]
  if(sum(temp) > 1){
    temp2 <- which(temp == TRUE)
    ind <- max(temp2[-length(temp2)])
    a[j,9] <- as.numeric(oct_games[i,1]-oct_games[ind,1])
  }else{
    a[j,9] <- 0
  }
}else{
  a[j,9] <- 0
}
a[j,10] <- sum(oct_games[1:i,2] == a[j,1]) + sum(oct_games[1:i,4] == a[j,1])
a[j,11] <- sum(oct_games[1:i,4] == a[j,1])
a[j,12] <- sum(a[(a[,1] == a[j,1]), 8])
if(a[j,11] == 0){
  a[j,13] <- 0
  a[j,14] <- 0
  a[j,15] <- 0
  a[j,16] <- 0
}else{
  temp <- a[,1] == a[j,1]
  temp2 <- which(temp == TRUE)
  ind <- max(temp2[-length(temp2)])
  a[j,13] <- a[ind,13]
  a[j,14] <- a[ind,14]
  a[j,15] <- a[ind,15]
  a[j,16] <- a[ind,16]
}
a[j,17] <- round(mean(oct_games[(oct_games[,2] == a[j,1]), 3][1:sum((oct_games[1:i,2] == a[j,1]))]),2)
a[j,18] <- max(oct_games[(oct_games[,2] == a[j,1]), 3][1:sum((oct_games[1:i,2] == a[j,1]))])
a[j,19] <- min(oct_games[(oct_games[,2] == a[j,1]), 3][1:sum((oct_games[1:i,2] == a[j,1]))])
a[j,20] <- round((a[j,14]*a[j,11]+a[j,17]*(a[j,10]-a[j,11]))/a[j,10],2)
if(a[j,11] == 0){
  a[j,21] <- 0
  a[j,22] <- 0
  a[j,23] <- 0
}else{
  temp <- a[,1] == a[j,1]
  temp2 <- which(temp == TRUE)
  ind <- max(temp2[-length(temp2)])
  a[j,21] <- a[ind,21]
  a[j,22] <- a[ind,22]
  a[j,23] <- a[ind,23]
}
a[j,24] <- round(mean(oct_games[(oct_games[,2] == a[j,1]), 5][1:sum((oct_games[1:i,2] == a[j,1]))]),2)
a[j,25] <- max(oct_games[(oct_games[,2] == a[j,1]), 5][1:sum((oct_games[1:i,2] == a[j,1]))])
a[j,26] <- min(oct_games[(oct_games[,2] == a[j,1]), 5][1:sum((oct_games[1:i,2] == a[j,1]))])
a[j,27] <- round((a[j,21]*a[j,11]+a[j,24]*(a[j,10]-a[j,11]))/a[j,10],2)
a[j,28] <- result

j <- j+1

#time 2 linha i
a[j,1] <- oct_games[i,4]
a[j,2] <- oct_games[i,2]
a[j,3] <- oct_games[i,5]
a[j,4] <- oct_games[i,3]
a[j,5] <- TRUE
a[j,6] <- oct_games[i,7]
if(is.na(oct_games[i,6]) == T) a[j,7] <- FALSE; if(is.na(oct_games[i,6]) == F) a[j,7] <- TRUE
if(result>0) a[j,8] <- FALSE; if(result<0) a[j,8] <-TRUE
if(i>1){
  temp <- oct_games[1:i,2] == a[j,1] | oct_games[1:i,4] == a[j,1]
  if(sum(temp) > 1){
    temp2 <- which(temp == TRUE)
    ind <- max(temp2[-length(temp2)])
    a[j,9] <- as.numeric(oct_games[i,1]-oct_games[ind,1])
  }else{
    a[j,9] <- 0
  }
}else{
  a[j,9] <- 0
}
a[j,10] <- sum(oct_games[1:i,2] == a[j,1]) + sum(oct_games[1:i,4] == a[j,1])
a[j,11] <- sum(oct_games[1:i,4] == a[j,1])
a[j,12] <- sum(a[(a[,1] == a[j,1]), 8])
a[j,13] <- sum(a[(a[j,1] == a[,1] & a[j,5] == a[,5]),8])
a[j,14] <- round(mean(oct_games[(oct_games[,4] == a[j,1]), 5][1:sum((oct_games[1:i,4] == a[j,1]))]),2)
a[j,15] <- max(oct_games[(oct_games[,4] == a[j,1]), 5][1:sum((oct_games[1:i,4] == a[j,1]))])
a[j,16] <- min(oct_games[(oct_games[,4] == a[j,1]), 5][1:sum((oct_games[1:i,4] == a[j,1]))])
if(a[j,10] == a[j,11]){
  a[j,17] <- 0
  a[j,18] <- 0
  a[j,19] <- 0
}else{
  temp <- a[,1] == a[j,1]
  temp2 <- which(temp == TRUE)
  ind <- max(temp2[-length(temp2)])
  a[j,17] <- a[ind,17]
  a[j,18] <- a[ind,18]
  a[j,19] <- a[ind,19]
}
a[j,20] <- round((a[j,14]*a[j,11]+a[j,17]*(a[j,10]-a[j,11]))/a[j,10], 2)
a[j,21] <- round(mean(oct_games[(oct_games[,4] == a[j,1]), 3][1:sum((oct_games[1:i,4] == a[j,1]))]), 2)
a[j,22] <- max(oct_games[(oct_games[,4] == a[j,1]), 3][1:sum((oct_games[1:i,4] == a[j,1]))])
a[j,23] <- min(oct_games[(oct_games[,4] == a[j,1]), 3][1:sum((oct_games[1:i,4] == a[j,1]))])
if(a[j,10] == a[j,11]){
  a[j,24] <- 0
  a[j,25] <- 0
  a[j,26] <- 0
}else{
  temp <- a[,1] == a[j,1]
  temp2 <- which(temp == TRUE)
  ind <- max(temp2[-length(temp2)])
  a[j,24] <- a[ind,24]
  a[j,25] <- a[ind,25]
  a[j,26] <- a[ind,26]
}
a[j,27] <- round((a[j,21]*a[j,11]+a[j,24]*(a[j,10]-a[j,11]))/a[j,10],2)
a[j,28] <- -result

j <- j+1
}

names(a) <- c("Team", "Opp", "Pts_S", "Pts_A", "Home", "Attend", "OT", "Win", "Days_LG", "Games_T", "Games_H",
              "Wins_T", "Wins_H", "Mean_Pts_S_H", "Max_Pts_S_H", "Min_Pts_S_H", "Mean_Pts_S_A", "Max_Pts_S_A", "Min_Pts_S_A",
              "Mean_Pts_S_T", "Mean_Pts_A_H", "Max_Pts_A_H", "Min_Pts_A_H", "Mean_Pts_A_A", "Max_Pts_A_A", "Min_Pts_A_A",
              "Mean_Pts_A_T", "Result")


a[1,29] <- 0
a[2,29] <- 0
for(i in 3:length(a$Team)){
p <- a[1:(i-1),]$Team == a[i,1]
if(sum(p) > 1){
  c <- which(p)
  w <- 0
  g <- 0
  for(j in 1:length(c)){
    y <- a[c[j], 2]
    if(i%%2 == 0){
      z <- a[1:(i-2),]$Team == y
    }else{
      z <- a[1:(i-1),]$Team == y
    }
    if(sum(z) > 0){
      d <- which(z)
      e <- max(d)
      w <- w+a[e,12]
      g <- g+a[e,10]
    }
  }
  if(g > 0){
    a[i,29] <- w/g
  }else{
    a[i,29] <- 0
  }
}else{
  a[i,29] <- 0
}
}

names(a)[29] <- "Str_Sch"

write.csv(a, "Tabela_Comp.csv")

casa <- a[a$Home == T, ]

teste <- lm(Result~., data=casa[200:1000,c(14,21,28,29)])
summary(teste)

win <- (predict(teste, newdata=casa[1001:1230,c(14,21,29)]) > 0) == casa$Win[1001:1230]
mean(win)

plot(teste)

library(e1071)
teste1 <- svm(Result~., data=casa[200:1000,c(14,21,28,29)])
summary(teste1)

win1 <- (predict(teste1, newdata=casa[1001:1230,c(14,21,29)]) > 0) == casa$Win[1001:1230]
mean(win1)


teste2 <- svm(as.factor(Win)~., data=casa[200:1000,c(14,21,29,8)])
summary(teste2)

win2 <- (predict(teste2, newdata=casa[1001:1230,c(14,21,29)]) == casa$Win[1001:1230])
mean(win2)
