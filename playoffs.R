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
#1:11, 100, 113:115, 117, 121:124
#28:36, 40:48, 52:60, 64:72, 76:84, 88:96, 101:103, 105:107, 109:111 last3~7
#37:39, 49:51, 61:63, 73:75, 85:87, 97:99, 104, 108, 112 last10
#116 wins_a
g82 <- g82[,-c(1:11, 100, 113, 115:117, 121:124, 28:36, 40:48, 52:60, 64:72, 76:84, 88:96, 101:103, 105:107, 109:111, 37:39, 49:51, 61:63, 73:75, 85:87, 97:99, 104, 108, 112)]


i <- 1
for(j in 1:18){
  for(k in 1:21){
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

a <- glm(Champion~., data = g82, family=binomial(link = "logit"))
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
  tes <- glm(formula = Champion ~ Wins_T + Mean_Pts_S_H + Mean_Pts_S_A + 
               Max_Pts_S_A + Min_Pts_S_A + Max_Pts_A_H + Mean_Pts_A_A + 
               Streak_T + Streak_H, family = binomial(link = "logit"), data = g82[-(i:(i+15)),])
  probabilities <- tes %>% predict(p18, type = "response")
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

tes3 <- svm(as.factor(Champion)~., data=g82, probability=T)

i <- 1
nom <- c()
for(j in 1:18){
  p18 <- g82[i:(i+15),]
  probs <- predict(tes3, newdata=p18, probability=T)
  probs <- attr(probs,"probabilities")
  nom[j] <- names(sort(probs[,2], decreasing = T)[1])
  i <- i+16
}

p18 <- g82[257:272,]
probs <- predict(tes3, newdata=p18, probability=T)
probs <- attr(probs,"probabilities")

b <- probs[,2]/sum(probs[,2])
b


#######

b19 <- base2019[c(2305,2315,2322:2324,2326,2329:2352),]
rownames(b19) <- NULL
for(i in 1:length(b19$mean_attend)){
  if(b19$mean_attend[i] > 0){
    b19$mean_attend[i] <- TRUE
  }else{
    b19$mean_attend[i] <- FALSE
  }
}
b19$mean_attend <- as.logical(b19$mean_attend)
names(b19)[114] <- "Seed_4"
b19$Wins_T <- b19$Wins_T/b19$Games_T
b19$Wins_H <- b19$Wins_H/b19$Games_H
b19$Wins_A <- b19$Wins_A/b19$Games_A
b19 <- b19[,-c(1:11, 100, 113, 115:117, 121:124, 28:36, 40:48, 52:60, 64:72, 76:84, 88:96, 101:103, 105:107, 109:111, 37:39, 49:51, 61:63, 73:75, 85:87, 97:99, 104, 108, 112)]
for(k in 1:21){
  if(k!=17){
    p18 <- b19[,k]
    ordem <- order(p18, decreasing = T)
    n <- 1
    dado <- rep(0,30)
    for(l in 1:30){
      if(l == 1){
        if(p18[ordem[l]] > p18[ordem[l+1]]){
          dado[ordem[l]] <- n
          n <- n+1
        }else{
          dado[ordem[l]] <- n
        }
      }else if(l == 30){
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
    b19[,k] <- dado
  }
}

a <- glm(Champion~., data = g82, family=binomial(link = "logit"))
summary(a)
library(dplyr)
probabilities <- a %>% predict(b19, type = "response")
probabilities <- probabilities/sum(probabilities)

probs <- data.frame(base2019[c(2305,2315,2322:2324,2326,2329:2352),1], round(probabilities,5))
colnames(probs) <- c("Team", "Probability")
probs[order(probs$Probability, decreasing = T),]
