ord <- match(rownames(final2019[1:1230,]), names(vet2))
vet2 <- vet2[ord]

jogos_2019 <- jogos_2019[1:1230,]
final2019 <- final2019[1:1230,]

times <- data.frame(jogos_2019$Visitor, jogos_2019$Home, vet2 == final2019$Win, final2019$Win)
colnames(times) <- c("Away","Home","Previsao","Resultado")

library(sqldf)
tab_acerto <-  sqldf("select DISTINCT Visitor as 'Team' from jogos_2019")
for(i in 1:30){
  fora <- times[(times[,1] == tab_acerto[i,1]),3]
  casa <- times[(times[,2] == tab_acerto[i,1]),3]
  tab_acerto$Prev_Acerto[i] <- sum(fora,casa)
  tab_acerto$Prev_Acerto_Home[i] <- sum(casa)
  tab_acerto$Prev_Acerto_Away[i] <- sum(fora)
  
  teste <- times[,1] == tab_acerto[i,1]
  vit_fora <- times[teste,4]
  previsoes_vit_fora <- times[teste,][vit_fora,3]
  tab_acerto$Away_Win_Prev[i] <- sum(previsoes_vit_fora)
  tab_acerto$Away_Win[i] <- sum(vit_fora)
  previsoes_loss_fora <- times[teste,][!vit_fora,3]
  tab_acerto$Away_Loss_Prev[i] <- sum(previsoes_loss_fora)
  tab_acerto$Away_Loss[i] <- sum(!vit_fora)
  
  teste2 <- times[,2] == tab_acerto[i,1]
  loss_casa <- times[teste2,4]
  previsoes_loss_casa <- times[teste2,][loss_casa,3]
  tab_acerto$Home_Loss_Prev[i] <- sum(previsoes_loss_casa)
  tab_acerto$Home_Loss[i] <- sum(loss_casa)
  previsoes_win_casa <- times[teste2,][!loss_casa,3]
  tab_acerto$Home_Win_Prev[i] <- sum(previsoes_win_casa)
  tab_acerto$Home_Win[i] <- sum(!loss_casa)
}


tab_acerto <- tab_acerto[order(tab_acerto$Prev_Acerto, decreasing = T),]

tab_acerto_pct <- data.frame(tab_acerto$Team,tab_acerto$Prev_Acerto/82, tab_acerto$Prev_Acerto_Home/41, tab_acerto$Prev_Acerto_Away/41,
                             (tab_acerto$Away_Win_Prev+tab_acerto$Home_Win_Prev)/(tab_acerto$Away_Win+tab_acerto$Home_Win),
                             (tab_acerto$Away_Loss_Prev+tab_acerto$Home_Loss_Prev)/(tab_acerto$Away_Loss+tab_acerto$Home_Loss),
           tab_acerto$Away_Win_Prev/tab_acerto$Away_Win, tab_acerto$Away_Loss_Prev/tab_acerto$Away_Loss,
           tab_acerto$Home_Loss_Prev/tab_acerto$Home_Loss, tab_acerto$Home_Win_Prev/tab_acerto$Home_Win)

tab_acerto_pct[,2:10] <- round(tab_acerto_pct[,2:10], 3)
colnames(tab_acerto_pct) <- c("Team", "%_acerto_T", "%_acerto_H", "%_acerto_A", "%_acerto_W", "%_acerto_L",
                              "%_ac_A_W", "%_ac_A_L", "%_ac_H_L", "%_ac_H_W")


class <- data.frame(tab_acerto[,1], tab_acerto[,6]+tab_acerto[,12], 
                    tab_acerto[,5]+tab_acerto[,11]+(tab_acerto[,8]-tab_acerto[,7])+(tab_acerto[,10]-tab_acerto[,9]))
colnames(class) <- c("Time", "Vitórias reais", "Vitórias previstas")
class <- class[order(class$`Vitórias reais`, decreasing = T),]
mean(abs(class$`Vitórias reais` - class$`Vitórias previstas`))

real <- class[,1:2]
prevista <- class[,c(1,3)]
prevista <- prevista[order(prevista$`Vitórias previstas`, decreasing = T),]
rownames(real) <- NULL
rownames(prevista) <- NULL

library(sqldf)
bases <- rbind(base2001,base2002,base2003,base2004,base2005,base2006,base2007,base2008,base2009,base2010,base2011,base2013,base2014,base2015,base2016,base2017,base2018)
conf <-  sqldf("select DISTINCT Team as 'Team' from bases")
confer <- c("E","E","W","E","E","W","W","W","W","W","E","E","E","E","E","E","W","W","E","W","E","E","W","W","W","W","E","E","W","W","W","E","W","W","E","W")
confer <- as.factor(confer)
conf$Conferencia <- confer

conf <- conf[match(prevista$Time, conf$Team),]
prevista$Conf <- conf$Conferencia
prevista_east <- prevista[prevista$Conf=="E",1:2]
prevista_west <- prevista[prevista$Conf=="W",1:2]
rownames(prevista_east) <- NULL
rownames(prevista_west) <- NULL
