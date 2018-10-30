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
  a[j,10] <- sum(oct_games[1:i,2] == a[j,1]) + sum(oct_games[1:i,4] == a[j,1]) - 1
  tp <- oct_games[1:i,4] == a[j,1]
  a[j,11] <- sum(tp[-length(tp)])
  tp3 <- a[(a[,1] == a[j,1]), 8]
  a[j,12] <- sum(tp3[-length(tp3)])
  if(a[j,10] == 0){
    a[j,13] <- 0
    a[j,14] <- 0
    a[j,15] <- 0
    a[j,16] <- 0
    a[j,17] <- 0
    a[j,18] <- 0
    a[j,19] <- 0
    a[j,20] <- 0
    a[j,21] <- 0
    a[j,22] <- 0
    a[j,23] <- 0
    a[j,24] <- 0
    a[j,25] <- 0
    a[j,26] <- 0
    a[j,27] <- 0
  }else if(a[j,11] == 0){
    a[j,13] <- 0
    a[j,14] <- 0
    a[j,15] <- 0
    a[j,16] <- 0
    x <- a[j,10] - a[j,11]
    tp6 <- a[(a[,1] == a[j,1] & a[,5] == FALSE), 3]
    a[j,17] <- round(mean(tp6[1:x]),2)
    a[j,18] <- max(tp6[1:x])
    a[j,19] <- min(tp6[1:x])
    a[j,20] <- round((a[j,14]*a[j,11]+a[j,17]*(a[j,10]-a[j,11]))/a[j,10],2)
    a[j,21] <- 0
    a[j,22] <- 0
    a[j,23] <- 0
    tp7 <- a[(a[,1] == a[j,1] & a[,5] == FALSE), 4]
    a[j,24] <- round(mean(tp7[1:x]),2)
    a[j,25] <- max(tp7[1:x])
    a[j,26] <- min(tp7[1:x])
    a[j,27] <- round((a[j,21]*a[j,11]+a[j,24]*(a[j,10]-a[j,11]))/a[j,10],2)
  }else if(a[j,10] == a[j,11]){
    tp5 <- a[(a[,1] == a[j,1] & a[,5] == TRUE),8]
    a[j,13] <- sum(tp5)
    x <- a[j,11]
    tp6 <- a[(a[,1] == a[j,1] & a[,5] == TRUE), 3]
    a[j,14] <- round(mean(tp6[1:x]),2)
    a[j,15] <- max(tp6[1:x])
    a[j,16] <- min(tp6[1:x])
    a[j,17] <- 0
    a[j,18] <- 0
    a[j,19] <- 0
    a[j,20] <- round((a[j,14]*a[j,11]+a[j,17]*(a[j,10]-a[j,11]))/a[j,10],2)
    tp7 <- a[(a[,1] == a[j,1] & a[,5] == TRUE), 4]
    a[j,21] <- round(mean(tp7[1:x]),2)
    a[j,22] <- max(tp7[1:x])
    a[j,23] <- min(tp7[1:x])
    a[j,24] <- 0
    a[j,25] <- 0
    a[j,26] <- 0
    a[j,27] <- round((a[j,21]*a[j,11]+a[j,24]*(a[j,10]-a[j,11]))/a[j,10],2)
  }else{
    tp5 <- a[(a[,1] == a[j,1] & a[,5] == TRUE),8]
    a[j,13] <- sum(tp5)
    x <- a[j,11]
    tp6 <- a[(a[,1] == a[j,1] & a[,5] == TRUE), 3]
    a[j,14] <- round(mean(tp6[1:x]),2)
    a[j,15] <- max(tp6[1:x])
    a[j,16] <- min(tp6[1:x])
    y <- a[j,10] - a[j,11]
    tp7 <- a[(a[,1] == a[j,1] & a[,5] == FALSE), 3]
    a[j,17] <- round(mean(tp7[1:y]),2)
    a[j,18] <- max(tp7[1:y])
    a[j,19] <- min(tp7[1:y])
    a[j,20] <- round((a[j,14]*a[j,11]+a[j,17]*(a[j,10]-a[j,11]))/a[j,10],2)
    tp8 <- a[(a[,1] == a[j,1] & a[,5] == TRUE), 4]
    a[j,21] <- round(mean(tp8[1:x]),2)
    a[j,22] <- max(tp8[1:x])
    a[j,23] <- min(tp8[1:x])
    tp9 <- a[(a[,1] == a[j,1] & a[,5] == FALSE), 4]
    a[j,24] <- round(mean(tp9[1:y]),2)
    a[j,25] <- max(tp9[1:y])
    a[j,26] <- min(tp9[1:y])
    a[j,27] <- round((a[j,21]*a[j,11]+a[j,24]*(a[j,10]-a[j,11]))/a[j,10],2)
  }
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
  a[j,10] <- sum(oct_games[1:i,2] == a[j,1]) + sum(oct_games[1:i,4] == a[j,1]) - 1
  tp2 <- oct_games[1:i,4] == a[j,1]
  a[j,11] <- sum(tp2[-length(tp2)])
  tp4 <- a[(a[,1] == a[j,1]), 8]
  a[j,12] <- sum(tp4[-length(tp4)])
  tp5 <- a[(a[j,1] == a[,1] & a[j,5] == a[,5]),8]
  a[j,13] <- sum(tp5[-length(tp5)])
  if(a[j,10] == 0){
    a[j,14] <- 0
    a[j,15] <- 0
    a[j,16] <- 0
    a[j,17] <- 0
    a[j,18] <- 0
    a[j,19] <- 0
    a[j,20] <- 0
    a[j,21] <- 0
    a[j,22] <- 0
    a[j,23] <- 0
    a[j,24] <- 0
    a[j,25] <- 0
    a[j,26] <- 0
    a[j,27] <- 0
  }else if(a[j,11] == 0){
    a[j,14] <- 0
    a[j,15] <- 0
    a[j,16] <- 0
    x <- a[j,10] - a[j,11]
    tp6 <- a[(a[,1] == a[j,1] & a[,5] == FALSE), 3]
    a[j,17] <- round(mean(tp6[1:x]),2)
    a[j,18] <- max(tp6[1:x])
    a[j,19] <- min(tp6[1:x])
    a[j,20] <- round((a[j,14]*a[j,11]+a[j,17]*(a[j,10]-a[j,11]))/a[j,10],2)
    a[j,21] <- 0
    a[j,22] <- 0
    a[j,23] <- 0
    tp7 <- a[(a[,1] == a[j,1] & a[,5] == FALSE), 4]
    a[j,24] <- round(mean(tp7[1:x]),2)
    a[j,25] <- max(tp7[1:x])
    a[j,26] <- min(tp7[1:x])
    a[j,27] <- round((a[j,21]*a[j,11]+a[j,24]*(a[j,10]-a[j,11]))/a[j,10],2)
  }else if(a[j,10] == a[j,11]){
    x <- a[j,11]
    tp6 <- a[(a[,1] == a[j,1] & a[,5] == TRUE), 3]
    a[j,14] <- round(mean(tp6[1:x]),2)
    a[j,15] <- max(tp6[1:x])
    a[j,16] <- min(tp6[1:x])
    a[j,17] <- 0
    a[j,18] <- 0
    a[j,19] <- 0
    a[j,20] <- round((a[j,14]*a[j,11]+a[j,17]*(a[j,10]-a[j,11]))/a[j,10],2)
    tp7 <- a[(a[,1] == a[j,1] & a[,5] == TRUE), 4]
    a[j,21] <- round(mean(tp7[1:x]),2)
    a[j,22] <- max(tp7[1:x])
    a[j,23] <- min(tp7[1:x])
    a[j,24] <- 0
    a[j,25] <- 0
    a[j,26] <- 0
    a[j,27] <- round((a[j,21]*a[j,11]+a[j,24]*(a[j,10]-a[j,11]))/a[j,10],2)
  }else{
    x <- a[j,11]
    tp6 <- a[(a[,1] == a[j,1] & a[,5] == TRUE), 3]
    a[j,14] <- round(mean(tp6[1:x]),2)
    a[j,15] <- max(tp6[1:x])
    a[j,16] <- min(tp6[1:x])
    y <- a[j,10] - a[j,11]
    tp7 <- a[(a[,1] == a[j,1] & a[,5] == FALSE), 3]
    a[j,17] <- round(mean(tp7[1:y]),2)
    a[j,18] <- max(tp7[1:y])
    a[j,19] <- min(tp7[1:y])
    a[j,20] <- round((a[j,14]*a[j,11]+a[j,17]*(a[j,10]-a[j,11]))/a[j,10],2)
    tp8 <- a[(a[,1] == a[j,1] & a[,5] == TRUE), 4]
    a[j,21] <- round(mean(tp8[1:x]),2)
    a[j,22] <- max(tp8[1:x])
    a[j,23] <- min(tp8[1:x])
    tp9 <- a[(a[,1] == a[j,1] & a[,5] == FALSE), 4]
    a[j,24] <- round(mean(tp9[1:y]),2)
    a[j,25] <- max(tp9[1:y])
    a[j,26] <- min(tp9[1:y])
    a[j,27] <- round((a[j,21]*a[j,11]+a[j,24]*(a[j,10]-a[j,11]))/a[j,10],2)
  }
  a[j,28] <- -result
  
  j <- j+1
}

tste <- a[a$Games_T==5,]

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
        w <- w+a[e,12]+(a[e,28]>0)
        g <- g+a[e,10]+1
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

b <- data.frame()
for(i in seq(1,2460,by=2)){
  d <- cbind(a[i,],a[i+1,])
  b <- rbind(b,d)
}

names(b) <- c("Team", "Opp", "Pts_S", "Pts_A", "Home", "Attend", "OT", "Win", "Days_LG", "Games_T", "Games_H",
              "Wins_T", "Wins_H", "Mean_Pts_S_H", "Max_Pts_S_H", "Min_Pts_S_H", "Mean_Pts_S_A", "Max_Pts_S_A", "Min_Pts_S_A",
              "Mean_Pts_S_T", "Mean_Pts_A_H", "Max_Pts_A_H", "Min_Pts_A_H", "Mean_Pts_A_A", "Max_Pts_A_A", "Min_Pts_A_A",
              "Mean_Pts_A_T", "Result", "Str_Sch", "Team_O", "Opp_O", "Pts_S_O", "Pts_A_O", "Home_O", "Attend_O", "OT_O", "Win_O", "Days_LG_O", "Games_T_O", "Games_H_O",
              "Wins_T_O", "Wins_H_O", "Mean_Pts_S_H_O", "Max_Pts_S_H_O", "Min_Pts_S_H_O", "Mean_Pts_S_A_O", "Max_Pts_S_A_O", "Min_Pts_S_A_O",
              "Mean_Pts_S_T_O", "Mean_Pts_A_H_O", "Max_Pts_A_H_O", "Min_Pts_A_H_O", "Mean_Pts_A_A_O", "Max_Pts_A_A_O", "Min_Pts_A_A_O",
              "Mean_Pts_A_T_O", "Result_O", "Str_Sch_O")


teste <- lm(Result_O~., data=b[56:1000,c(9,12,13,17,20,24,27,29,35,38,41,42,43,49,50,56,57,58)])
summary(teste)

win <- (predict(teste, newdata=b[1001:1230,c(9,12,13,17,20,24,27,29,35,38,41,42,43,49,50,56,58)]) > 0) == b$Win_O[1001:1230]
mean(win)

plot(teste)

library(e1071)
teste1 <- svm(Result_O~., data=b[56:1000,c(9,12,13,17,20,24,27,29,35,38,41,42,43,49,50,56,57,58)])
summary(teste1)

win1 <- (predict(teste1, newdata=b[1001:1230,c(9,12,13,17,20,24,27,29,35,38,41,42,43,49,50,56,58)]) > 0) == b$Win_O[1001:1230]
mean(win1)


teste2 <- svm(as.factor(Win)~., data=casa[200:1000,c(14,21,29,8)])
summary(teste2)

win2 <- (predict(teste2, newdata=casa[1001:1230,c(14,21,29)]) == casa$Win[1001:1230])
mean(win2)
