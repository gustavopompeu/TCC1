bases <- rbind(base2001,base2002,base2003,base2004,base2005,base2006,base2007,base2008,base2009,base2010,base2011,base2013,base2014,base2015,base2016,base2017,base2018)
g82 <- bases[bases$Games_T==82,]
a <- base2012[base2012$Games_T==66,]
g82 <- rbind(g82,a)
rownames(g82) <- NULL
g82$Champion <- FALSE
g82$Champion[c(10,32,42,58,80,86,112,122,142,156,164,186,208,212,234,252,258,278)] <- TRUE
for(i in 1:length(g82$mean_attend)){
  if(g82$mean_attend[i] > 0){
    g82$mean_attend[i] <- TRUE
  }else{
    g82$mean_attend[i] <- FALSE
  }
}
g82$mean_attend <- as.logical(g82$mean_attend)
names(g82)[114] <- "Seed_4"
g82$Wins_T[1:272] <- g82$Wins_T[1:272]/82
g82$Wins_T[273:288] <- g82$Wins_T[273:288]/66
g82$Wins_H[1:272] <- g82$Wins_H[1:272]/41
g82$Wins_H[273:288] <- g82$Wins_H[273:288]/33
g82$Wins_A[1:272] <- g82$Wins_A[1:272]/41
g82$Wins_A[273:288] <- g82$Wins_A[273:288]/33
g82 <- g82[c(1:176,273:288,177:272),]
rownames(g82) <- NULL
library(sqldf)
conf <-  sqldf("select DISTINCT Team as 'Team' from bases")
confer <- c("E","E","W","E","E","W","W","W","W","W","E","E","E","E","E","E","W","W","E","W","E","E","W","W","W","W","E","E","W","W","W","E","W","W","E","W")
confer <- as.factor(confer)
conf$Conferencia <- confer

library(plyr)
g82 <- join(g82,conf)
g82[which((g82[1:64,]$Team == "New Orleans Hornets")),]$Conferencia <- c("E","E")
#1:11, 100, 113:115, 117:124
#28:36, 40:48, 52:60, 64:72, 76:84, 88:96, 101:103, 105:107, 109:111 last3~7
#37:39, 49:51, 61:63, 73:75, 85:87, 97:99, 104, 108, 112 last10
i <- 1
l <- 1
pl_wins <- c()
for(j in 2001:2018){
  nam <- g82[i:(i+15),1]
  assign("base",get(paste("base",j,sep="")))
  for(k in 1:16){
    ind <- max(which(base[,1] == nam[k]))
    W_Pl <- base[ind,]$Wins_T
    if(j == 2012){
      W <- base[base$Team == nam[k] & base$Games_T == 66,]$Wins_T
    }else{
      W <- base[base$Team == nam[k] & base$Games_T == 82,]$Wins_T
    }
    pl_wins[l] <- W_Pl - W
    l <- l+1
  }
  i <- i+16
}

g82$pl_win <- pl_wins
g82[g82$Champion == T,]$pl_win <- g82[g82$Champion == T,]$pl_win+1
g82 <- g82[,-c(1:11, 100, 113, 115,117:124, 28:36, 40:48, 52:60, 64:72, 76:84, 88:96, 101:103, 105:107, 109:111, 37:39, 49:51, 61:63, 73:75, 85:87, 97:99, 104, 108, 112)]

i <- 1
for(j in 1:18){
  for(k in 1:19){
    if(k!=17){
      p18 <- g82[i:(i+15),k]
      ordem <- order(p18, decreasing = T)
      n <- 1
      dado <- rep(0,16)
      for(l in 1:16){
        if(l == 1){
          if(p18[ordem[l]] > p18[ordem[l+1]]){
            dado[ordem[l]] <- n
            n <- n+1
          }else{
            dado[ordem[l]] <- n
          }
        }else if(l == 16){
          dado[ordem[l]] <- n
        }else{
          if((p18[ordem[l]] > p18[ordem[l+1]]) & (p18[ordem[l]] < p18[ordem[l-1]])){
            dado[ordem[l]] <- n
            n <- n+1
          }else if((p18[ordem[l]] > p18[ordem[l+1]]) & (p18[ordem[l]] == p18[ordem[l-1]])){
            p <- l
            while((p18[ordem[p]] == p18[ordem[p-1]])){
              p <- p-1
              if(p==1){
                break
              }
            }
            dado[ordem[l]] <- n
            n <- n+1+(l-p)
          }else if((p18[ordem[l]] == p18[ordem[l+1]]) & (p18[ordem[l]] == p18[ordem[l-1]])){
            dado[ordem[l]] <- n
          }else if((p18[ordem[l]] == p18[ordem[l+1]]) & (p18[ordem[l]] < p18[ordem[l-1]])){
            dado[ordem[l]] <- n
          }
        }
      }
      g82[i:(i+15),k] <- dado
    }
  }
  i <- i+16
}

####
mod <- lm(pl_win~., data=g82[,-c(19,20)])
summary(mod)
p18 <- g19
probabilities <- predict(mod, newdata=p18)

data.frame(base2019[base2019$Games_T==82,]$Team, probabilities)

i <- 1
nom <- c()
for(j in 1:18){
  p18 <- g82[i:(i+15),]
  mod <- lm(formula = pl_win ~ ., data = g82[-c(i:(i+15)), -c(17,19,20)])
  probabilities <- predict(mod, newdata=p18)
  nom <- c(nom,probabilities)
  i <- i+16
}

g82$Champion[as.numeric(nom)]

sum(abs(nom-g82$pl_win))/288
nom[nom<0] <- 0
sum(abs(nom[g82$pl_win<4]-g82[g82$pl_win<4,]$pl_win))/sum(g82$pl_win<4)

sum(nom[g82$pl_win<4]<4)/145

require(leaps)
step(mod, direction='backward', scope=( ~ 1))
mod2 <- lm(formula = pl_win ~ Wins_T + Min_Pts_A_H + Min_Pts_A_A + Seed_4 + 
             Conferencia, data = g82[-c(i:(i+15)), -19])

modmin <- lm(pl_win~1, data=g82[,-19])
step(modmin, direction='forward', scope=(~ Wins_T + Wins_H + Mean_Pts_S_H + Max_Pts_S_H + Min_Pts_S_H + 
                                           Mean_Pts_S_A + Max_Pts_S_A + Min_Pts_S_A + Mean_Pts_S_T + 
                                           Mean_Pts_A_H + Max_Pts_A_H + Min_Pts_A_H + Mean_Pts_A_A + 
                                           Max_Pts_A_A + Min_Pts_A_A + Mean_Pts_A_T + Seed_4 + Str_Sch + 
                                           Conferencia))
mod3 <- lm(formula = pl_win ~ Wins_T + Conferencia + Seed_4 + Min_Pts_A_A + 
             Min_Pts_A_H, data = g82[, -19])
####

a <- glm(Champion~., data = g82[,-c(19,22)], family=binomial(link = "logit"))
summary(a)
library(dplyr)
probabilities <- a %>% predict(p18, type = "response")
probabilities <- probabilities/sum(probabilities)


g82[g82$Champion==T,19]

g82[c(246,252),]

p18 <- g82[241:256,]
i <- 1
nom <- c()
for(j in 1:18){
  p18 <- g82[i:(i+15),]
  a <- glm(Champion~., data = g82[-(i:(i+15)),-22], family=binomial(link = "logit"))
  probabilities <- a %>% predict(p18, type = "response")
  probabilities <- probabilities/sum(probabilities)
  nom[j] <- names(sort(probabilities, decreasing = T)[1])
  i <- i+16
}

g82$Champion[as.numeric(nom)]
as.numeric(nom)
nom2

require(leaps)
step(a, direction='backward', scope=( ~ 1))

tes <- glm(formula = Champion ~ Wins_T + Mean_Pts_S_H + Mean_Pts_S_A + 
             Max_Pts_S_A + Min_Pts_S_A + Max_Pts_A_H + Mean_Pts_A_A + 
             Streak_T + Streak_H, family = binomial(link = "logit"), data = g82)
probabilities <- tes %>% predict(p18, type = "response")
probabilities <- probabilities/sum(probabilities)
probabilities
names(sort(probabilities, decreasing = T)[1])

modmin <- glm(Champion~1, data = g82, family=binomial(link = "logit"))
step(modmin, direction='forward', scope=(~ Wins_T + Wins_H + Mean_Pts_S_H + Max_Pts_S_H + Min_Pts_S_H + 
                                           Mean_Pts_S_A + Max_Pts_S_A + Min_Pts_S_A + Mean_Pts_S_T + 
                                           Mean_Pts_A_H + Max_Pts_A_H + Min_Pts_A_H + Mean_Pts_A_A + 
                                           Max_Pts_A_A + Min_Pts_A_A + Mean_Pts_A_T + Seed_4 + Streak_T + 
                                           Streak_H + Streak_A + Str_Sch))

tes2 <- glm(formula = Champion ~ Wins_T + Max_Pts_A_H + Max_Pts_S_A + 
              Min_Pts_S_A + Streak_A, family = binomial(link = "logit"), 
            data = g82)
probabilities <- tes2 %>% predict(p18, type = "response")
probabilities <- probabilities/sum(probabilities)
probabilities

library(e1071)

tes3 <- svm(pl_win~., data=g82[,-19], probability=T)

i <- 1
nom <- c()
for(j in 1:18){
  p18 <- g82[i:(i+15),]
  tes3 <- svm(pl_win~., data=g82[-c(i:(i+15)),-19], probability=T)
  probs <- predict(tes3, newdata=p18)
  nom[j] <- names(which.max(probs))
  i <- i+16
}

g82$Champion[as.numeric(nom)]

p18 <- g82[257:272,]
probs <- predict(tes3, newdata=p18, probability=T)
probs <- attr(probs,"probabilities")

b <- probs[,2]/sum(probs[,2])
b


#######

g19 <- base2019[base2019$Games_T==82,]
rownames(g19) <- NULL
for(i in 1:length(g19$mean_attend)){
  if(g19$mean_attend[i] > 0){
    g19$mean_attend[i] <- TRUE
  }else{
    g19$mean_attend[i] <- FALSE
  }
}
g19$mean_attend <- as.logical(g19$mean_attend)
names(g19)[114] <- "Seed_4"
g19$Wins_T <- g19$Wins_T/82
g19$Wins_H <- g19$Wins_H/41
g19$Wins_A <- g19$Wins_A/41
rownames(g19) <- NULL
library(sqldf)
conf <-  sqldf("select DISTINCT Team as 'Team' from bases")
confer <- c("E","E","W","E","E","W","W","W","W","W","E","E","E","E","E","E","W","W","E","W","E","E","W","W","W","W","E","E","W","W","W","E","W","W","E","W")
confer <- as.factor(confer)
conf$Conferencia <- confer

library(plyr)
g19 <- join(g19,conf)

#1:11, 100, 113:115, 117:124
#28:36, 40:48, 52:60, 64:72, 76:84, 88:96, 101:103, 105:107, 109:111 last3~7
#37:39, 49:51, 61:63, 73:75, 85:87, 97:99, 104, 108, 112 last10

g19 <- g19[,-c(1:11, 100, 113, 115,117:124, 28:36, 40:48, 52:60, 64:72, 76:84, 88:96, 101:103, 105:107, 109:111, 37:39, 49:51, 61:63, 73:75, 85:87, 97:99, 104, 108, 112)]

  for(k in 1:18){
    if(k!=17){
      p18 <- g19[,k]
      ordem <- order(p18, decreasing = T)
      n <- 1
      dado <- rep(0,16)
      for(l in 1:16){
        if(l == 1){
          if(p18[ordem[l]] > p18[ordem[l+1]]){
            dado[ordem[l]] <- n
            n <- n+1
          }else{
            dado[ordem[l]] <- n
          }
        }else if(l == 16){
          dado[ordem[l]] <- n
        }else{
          if((p18[ordem[l]] > p18[ordem[l+1]]) & (p18[ordem[l]] < p18[ordem[l-1]])){
            dado[ordem[l]] <- n
            n <- n+1
          }else if((p18[ordem[l]] > p18[ordem[l+1]]) & (p18[ordem[l]] == p18[ordem[l-1]])){
            p <- l
            while((p18[ordem[p]] == p18[ordem[p-1]])){
              p <- p-1
              if(p==1){
                break
              }
            }
            dado[ordem[l]] <- n
            n <- n+1+(l-p)
          }else if((p18[ordem[l]] == p18[ordem[l+1]]) & (p18[ordem[l]] == p18[ordem[l-1]])){
            dado[ordem[l]] <- n
          }else if((p18[ordem[l]] == p18[ordem[l+1]]) & (p18[ordem[l]] < p18[ordem[l-1]])){
            dado[ordem[l]] <- n
          }
        }
      }
      g19[,k] <- dado
    }
  }
