load(file="jogos.rda")

##Leitura dos dados da internet

library(rvest)

ler_jogos <- function(url){
  html <- read_html(url)
  #nodes obtidos com o SelectorGadget, extensao do google chrome
  games <- html_nodes(html, ".center+ .center , .left:nth-child(5) , td+ .right , .left:nth-child(3) , .poptip.right , .left:nth-child(1)")
  texto <- html_text(games)
  tabela <- data.frame()
  j <- 1
  for(i in 1:(length(texto)/7)){
    tabela[i,1] <- texto[j]
    tabela[i,2] <- texto[j+1]
    tabela[i,3] <- texto[j+2]
    tabela[i,4] <- texto[j+3]
    tabela[i,5] <- texto[j+4]
    tabela[i,6] <- texto[j+5]
    tabela[i,7] <- texto[j+6]
    j <- j+7
  }
  names(tabela) <- tabela[1,]
  tabela <- tabela[2:length(tabela[,1]),]
  return(tabela)
}

tabela_temporada <- function(year){
  if(year == 2019){
    months <- c("october","november","december","january","february","march","april")
  }else{
    months <- c("october","november","december","january","february","march","april","may","june")
  }
  cola1 <- paste("https://www.basketball-reference.com/leagues/NBA_",year,sep="")
  cola2 <- paste(cola1,"_games-",months,sep="")
  url <- paste(cola2,".html",sep="")
  teste <- data.frame()
  for(i in 1:length(url)){
    b <- ler_jogos(url[i])
    teste <- rbind(teste,b)
  }
  names(teste) <- c("Date", "Visitor", "PTS_Visitor", "Home", "PTS_Home", "OT", "Attend")
  teste$PTS_Visitor <- as.numeric(teste$PTS_Visitor)
  teste$PTS_Home <- as.numeric(teste$PTS_Home)
  teste$Attend <- as.numeric(gsub(",", "", teste$Attend))
  Sys.setlocale("LC_ALL","English")
  teste$Date <- as.Date(teste$Date, format="%a, %b %d, %Y")
  teste$OT[teste$OT == ""] <- NA
  return(teste)
}

#bases das temporadas anteriores, ja estao salvas
jogos_2013 <- tabela_temporada(2013)
jogos_2014 <- tabela_temporada(2014)
jogos_2015 <- tabela_temporada(2015)
jogos_2016 <- tabela_temporada(2016)
jogos_2017 <- tabela_temporada(2017)
jogos_2018 <- tabela_temporada(2018)
save(jogos_2013,jogos_2014,jogos_2015,jogos_2016,jogos_2017,jogos_2018, file="jogos.rda")

##Temporada atual, unica q precisa rodar novamente
jogos_2019 <- tabela_temporada(2019)

##Criacao da base de dados
#cada linha e referente a um time e a um jogo
##glossario: colunas
##variaveis referentes ao jogo
#1: time referente a linha. 2: adversario do jogo
#3: pontos do time referencia nesse jogo. 4: pts do adversario
#5: logical, se o time referencia jogou esse jogo em casa ou nao
#6: publico presente do time referencia nesse jogo (0 se o time nao joga em casa)
#7: logi, se esse jogo teve prorroga??o ou n
#8: logi, se o time referencia venceu esse jogo ou n

##variaveis referentes a temporada
#9: quantos dias atras foi o ultimo jogo do time referencia (0 se ? o primeiro jogo da temporada)
#10: total de jogos concluidos do time referencia ate o momento na temporada
#11: mesmo de acima, mas apenas jogos em casa
#12: total de vitorias do time referencia ate o momento na temporada
#13: mesmo de acima, mas apenas vitorias em jogos em casa
#14-16: media, min, max de pontos marcados PELO time referencia em jogos em casa ate o momento da temporada
#17-19: mesmo de acima, mas jogos fora de casa
#20: media de pontos marcados PELO time referencia em todos os jogos ja concluidos
#(nao ha min e max dos jogos totais pois apenas ia repetir de uma das colunas casa/fora)
#21-23: media de pontos marcados CONTRA o time referencia em jogos em casa ate o momento da temporada
#24-26: mesmo de acima, mas jogos fora de casa
#27: media de pontos marcados CONTRA o time referencia em todos os jogos ja concluidos
#28-39: media, max e min de pontos marcados PELO time referencia nos ultimos 3/5/7/10 jogos fora de casa
#(? 0 at? o time ter concluido 4/6/8/11 jogos fora de casa)
#40-51: media, max e min de pontos marcados PELO time referencia nos ultimos 3/5/7/10 jogos em casa
#(? 0 at? o time ter concluido 4/6/8/11 jogos em casa)
#52-63: media, max e min de pontos marcados PELO time referencia nos ultimos 3/5/7/10 jogos
#(? 0 at? o time ter concluido 4/6/8/11 jogos)
#64-75: media, max e min de pontos marcados CONTRA o time referencia nos ultimos 3/5/7/10 jogos fora de casa
#(? 0 at? o time ter concluido 4/6/8/11 jogos fora de casa)
#76-87: media, max e min de pontos marcados CONTRA o time referencia nos ultimos 3/5/7/10 jogos em casa
#(? 0 at? o time ter concluido 4/6/8/11 jogos em casa)
#88-99: media, max e min de pontos marcados CONTRA o time referencia nos ultimos 3/5/7/10 jogos
#(? 0 at? o time ter concluido 4/6/8/11 jogos)
#100: logi, se o ultimo jogo do time referencia foi para alguma prorroga??o ou n
#101-104: numero de vitorias do time referencia nos ultimos 3/5/7/10 jogos fora de casa
#105-108: numero de vitorias do time referencia nos ultimos 3/5/7/10 jogos em casa
#109-112: numero de vitorias do time referencia nos ultimos 3/5/7/10 jogos

#volta a ter variaveis do jogo
#113: dia da semana em que o jogo ocorreu
#118: diferen?a de pontos entre os times ao fim do jogo (com base no time referencia)
#(result negativo significa derrota do time referencia, result positivo significa vitoria)

#114: media de publico do time referencia ate agora na temporada (0 se o time estiver jogando fora de casa)
#115: total de jogos do time referencia fora de casa ate o momento
#116: total de vitorias do time referencia fora de casa ate o momento
#117: logi, se o time viajou do ultimo jogo para esse 
#(so nao tera viajado se estiver jogando 2 ou mais jogos seguidos em casa)
#119: strength of schedule do time referencia (referente a temporada)
#(porcentagem de vitorias de TODOS os adversarios que o time referencia ja enfrentou ate o momento)

base_var <- function(oct_games){
  dia_semana <- weekdays(oct_games[,1])
  a <- data.frame()
  j <- 1
  for(i in 1:length(oct_games$PTS_Visitor)){
    #comeca em linha impar
    #time referencia das linhas impares esta jogando fora de casa
    #copiando times e pontos da tabela de resultados
    a[j,1] <- oct_games[i,2]
    a[j,2] <- oct_games[i,4]
    a[j,3] <- oct_games[i,3]
    a[j,4] <- oct_games[i,5]
    #HOME sempre falso pois o time referencia esta jogando fora de casa
    a[j,5] <- FALSE
    #publico sempre -1 pois o time referencia esta jogando fora de casa
    a[j,6] <- -1
    #pega da tabela de resultados pra saber se esse jogo foi pra prorrogacao
    if(is.na(oct_games[i,6]) == T) a[j,7] <- FALSE; if(is.na(oct_games[i,6]) == F) a[j,7] <- TRUE
    #calculo da diferenca de pontos entre os times para saber o vencedor
    result <- a[j,3] - a[j,4]
    if(result<0) a[j,8] <- FALSE; if(result>0) a[j,8] <- TRUE

    #calculo de quantos dias atras foi o ultimo jogo do time referencia
    #(0 se for o primeiro jogo da temporada)
    if(i>1){
      temp <- oct_games[1:i,2] == a[j,1] | oct_games[1:i,4] == a[j,1]
      if(sum(temp) > 1){
        temp2 <- which(temp == TRUE)
        ind <- max(temp2[-length(temp2)])
        a[j,9] <- as.numeric(oct_games[i,1]-oct_games[ind,1])
      }else{
        a[j,9] <- -1
      }
    }else{
      a[j,9] <- -1
    }
    #total de jogos
    a[j,10] <- sum(oct_games[1:i,2] == a[j,1]) + sum(oct_games[1:i,4] == a[j,1]) - 1
    #total de jogos em casa
    tp <- oct_games[1:i,4] == a[j,1]
    a[j,11] <- sum(tp[-length(tp)])
    #total de vitorias
    tp3 <- a[(a[,1] == a[j,1]), 8]
    a[j,12] <- sum(tp3[-length(tp3)])
    #se o total de jogos do time ainda for 0
    #tudo daqui pra frente nao tem dado
    #(significa q e o primeiro jogo do time na temporada, ou seja, nao ha info de jogos anteriores)
    if(a[j,10] == 0){
      a[j,13] <- 0
      for(ind in 14:99){
        a[j,ind] <- -1
      }
    #se o total de jogos em casa for 0
    #(time so jogou fora de casa por enquanto)
    }else if(a[j,11] == 0){
      #as variaveis referentes a jogos em casa sao 0
      a[j,13] <- 0
      for(ind in 14:16){
        a[j,ind] <- -1
      }
      #as variaveis de pontos marcados fora e total, pelo time e contra o time sao preenchidas normalmente
      x <- a[j,10] - a[j,11]
      tp6 <- a[(a[,1] == a[j,1] & a[,5] == FALSE), 3]
      a[j,17] <- round(mean(tp6[1:x]),2)
      a[j,18] <- max(tp6[1:x])
      a[j,19] <- min(tp6[1:x])
      a[j,20] <- round((a[j,14]*a[j,11]+a[j,17]*(a[j,10]-a[j,11]))/a[j,10],2)
      for(ind in 21:23){
        a[j,ind] <- -1
      }
      tp7 <- a[(a[,1] == a[j,1] & a[,5] == FALSE), 4]
      a[j,24] <- round(mean(tp7[1:x]),2)
      a[j,25] <- max(tp7[1:x])
      a[j,26] <- min(tp7[1:x])
      a[j,27] <- round((a[j,21]*a[j,11]+a[j,24]*(a[j,10]-a[j,11]))/a[j,10],2)
      #aqui comecam os pontos marcados nos ultimos x jogos
      #se o time jogou x ou menos jogos ate agora na temporada, elas sao 0
      if((a[j,10]-a[j,11]) > 3){
        a[j,28] <- round(mean(tail(tp6[1:x],3)),2)
        a[j,29] <- max(tail(tp6[1:x],3))
        a[j,30] <- min(tail(tp6[1:x],3))
      }else{
        for(ind in 28:30){
          a[j,ind] <- -1
        }
      }
      if((a[j,10]-a[j,11]) > 5){
        a[j,31] <- round(mean(tail(tp6[1:x],5)),2)
        a[j,32] <- max(tail(tp6[1:x],5))
        a[j,33] <- min(tail(tp6[1:x],5))
      }else{
        for(ind in 31:33){
          a[j,ind] <- -1
        }
      }
      if((a[j,10]-a[j,11]) > 7){
        a[j,34] <- round(mean(tail(tp6[1:x],7)),2)
        a[j,35] <- max(tail(tp6[1:x],7))
        a[j,36] <- min(tail(tp6[1:x],7))
      }else{
        for(ind in 34:36){
          a[j,ind] <- -1
        }
      }
      if((a[j,10]-a[j,11]) > 10){
        a[j,37] <- round(mean(tail(tp6[1:x],10)),2)
        a[j,38] <- max(tail(tp6[1:x],10))
        a[j,39] <- min(tail(tp6[1:x],10))
      }else{
        for(ind in 37:39){
          a[j,ind] <- -1
        }
      }
      for(ind in 40:63){
        a[j,ind] <- -1
      }
      if((a[j,10]-a[j,11]) > 3){
        a[j,64] <- round(mean(tail(tp7[1:x],3)),2)
        a[j,65] <- max(tail(tp7[1:x],3))
        a[j,66] <- min(tail(tp7[1:x],3))
      }else{
        for(ind in 64:66){
          a[j,ind] <- -1
        }
      }
      if((a[j,10]-a[j,11]) > 5){
        a[j,67] <- round(mean(tail(tp7[1:x],5)),2)
        a[j,68] <- max(tail(tp7[1:x],5))
        a[j,69] <- min(tail(tp7[1:x],5))
      }else{
        for(ind in 67:69){
          a[j,ind] <- -1
        }
      }
      if((a[j,10]-a[j,11]) > 7){
        a[j,70] <- round(mean(tail(tp7[1:x],7)),2)
        a[j,71] <- max(tail(tp7[1:x],7))
        a[j,72] <- min(tail(tp7[1:x],7))
      }else{
        for(ind in 70:72){
          a[j,ind] <- -1
        }
      }
      if((a[j,10]-a[j,11]) > 10){
        a[j,73] <- round(mean(tail(tp7[1:x],10)),2)
        a[j,74] <- max(tail(tp7[1:x],10))
        a[j,75] <- min(tail(tp7[1:x],10))
      }else{
        for(ind in 73:75){
          a[j,ind] <- -1
        }
      }
      for(ind in 76:99){
        a[j,ind] <- -1
      }
    #caso o numero de jogos em casa seja igual aos jogos totais
    #(time so jogou em casa)
    }else if(a[j,10] == a[j,11]){
      #as variaveis de jogo em casa e totais sao preenchidas normalmente
      tp5 <- a[(a[,1] == a[j,1] & a[,5] == TRUE),8]
      a[j,13] <- sum(tp5)
      x <- a[j,11]
      tp6 <- a[(a[,1] == a[j,1] & a[,5] == TRUE), 3]
      a[j,14] <- round(mean(tp6[1:x]),2)
      a[j,15] <- max(tp6[1:x])
      a[j,16] <- min(tp6[1:x])
      #as variaveis de jogos fora sao -1
      for(ind in 17:19){
        a[j,ind] <- -1
      }
      a[j,20] <- round((a[j,14]*a[j,11]+a[j,17]*(a[j,10]-a[j,11]))/a[j,10],2)
      tp7 <- a[(a[,1] == a[j,1] & a[,5] == TRUE), 4]
      a[j,21] <- round(mean(tp7[1:x]),2)
      a[j,22] <- max(tp7[1:x])
      a[j,23] <- min(tp7[1:x])
      for(ind in 24:26){
        a[j,ind] <- -1
      }
      a[j,27] <- round((a[j,21]*a[j,11]+a[j,24]*(a[j,10]-a[j,11]))/a[j,10],2)
      for(ind in 28:39){
        a[j,ind] <- -1
      }
      if((a[j,11]) > 3){
        a[j,40] <- round(mean(tail(tp6[1:x],3)),2)
        a[j,41] <- max(tail(tp6[1:x],3))
        a[j,42] <- min(tail(tp6[1:x],3))
      }else{
        for(ind in 40:42){
          a[j,ind] <- -1
        }
      }
      if((a[j,11]) > 5){
        a[j,43] <- round(mean(tail(tp6[1:x],5)),2)
        a[j,44] <- max(tail(tp6[1:x],5))
        a[j,45] <- min(tail(tp6[1:x],5))
      }else{
        for(ind in 43:45){
          a[j,ind] <- -1
        }
      }
      if((a[j,11]) > 7){
        a[j,46] <- round(mean(tail(tp6[1:x],7)),2)
        a[j,47] <- max(tail(tp6[1:x],7))
        a[j,48] <- min(tail(tp6[1:x],7))
      }else{
        for(ind in 46:48){
          a[j,ind] <- -1
        }
      }
      if((a[j,11]) > 10){
        a[j,49] <- round(mean(tail(tp6[1:x],10)),2)
        a[j,50] <- max(tail(tp6[1:x],10))
        a[j,51] <- min(tail(tp6[1:x],10))
      }else{
        for(ind in 49:51){
          a[j,ind] <- -1
        }
      }
      for(ind in 52:75){
        a[j,ind] <- -1
      }
      if((a[j,11]) > 3){
        a[j,76] <- round(mean(tail(tp7[1:x],3)),2)
        a[j,77] <- max(tail(tp7[1:x],3))
        a[j,78] <- min(tail(tp7[1:x],3))
      }else{
        for(ind in 76:78){
          a[j,ind] <- -1
        }
      }
      if((a[j,11]) > 5){
        a[j,79] <- round(mean(tail(tp7[1:x],5)),2)
        a[j,80] <- max(tail(tp7[1:x],5))
        a[j,81] <- min(tail(tp7[1:x],5))
      }else{
        for(ind in 79:81){
          a[j,ind] <- -1
        }
      }
      if((a[j,11]) > 7){
        a[j,82] <- round(mean(tail(tp7[1:x],7)),2)
        a[j,83] <- max(tail(tp7[1:x],7))
        a[j,84] <- min(tail(tp7[1:x],7))
      }else{
        for(ind in 82:84){
          a[j,ind] <- -1
        }
      }
      if((a[j,11]) > 10){
        a[j,85] <- round(mean(tail(tp7[1:x],10)),2)
        a[j,86] <- max(tail(tp7[1:x],10))
        a[j,87] <- min(tail(tp7[1:x],10))
      }else{
        for(ind in 85:87){
          a[j,ind] <- -1
        }
      }
      for(ind in 88:99){
        a[j,ind] <- -1
      }
    #pra quando o time ja teve jogo em casa E ja teve jogo fora
    }else{
      #todas as variaveis precisam ser preenchidas
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
      tp10 <- a[(a[,1] == a[j,1]), 3]
      tp11 <- a[(a[,1] == a[j,1]), 4]
      z <- a[j,10]
      #as de ultimos x jogos ainda sao 0 quando o time jogou x ou menos jogos
      if((a[j,10]-a[j,11]) > 3){
        a[j,28] <- round(mean(tail(tp7[1:y],3)),2)
        a[j,29] <- max(tail(tp7[1:y],3))
        a[j,30] <- min(tail(tp7[1:y],3))
      }else{
        for(ind in 28:30){
          a[j,ind] <- -1
        }
      }
      if((a[j,10]-a[j,11]) > 5){
        a[j,31] <- round(mean(tail(tp7[1:y],5)),2)
        a[j,32] <- max(tail(tp7[1:y],5))
        a[j,33] <- min(tail(tp7[1:y],5))
      }else{
        for(ind in 31:33){
          a[j,ind] <- -1
        }
      }
      if((a[j,10]-a[j,11]) > 7){
        a[j,34] <- round(mean(tail(tp7[1:y],7)),2)
        a[j,35] <- max(tail(tp7[1:y],7))
        a[j,36] <- min(tail(tp7[1:y],7))
      }else{
        for(ind in 34:36){
          a[j,ind] <- -1
        }
      }
      if((a[j,10]-a[j,11]) > 10){
        a[j,37] <- round(mean(tail(tp7[1:y],10)),2)
        a[j,38] <- max(tail(tp7[1:y],10))
        a[j,39] <- min(tail(tp7[1:y],10))
      }else{
        for(ind in 37:39){
          a[j,ind] <- -1
        }
      }
      if((a[j,11]) > 3){
        a[j,40] <- round(mean(tail(tp6[1:x],3)),2)
        a[j,41] <- max(tail(tp6[1:x],3))
        a[j,42] <- min(tail(tp6[1:x],3))
      }else{
        for(ind in 40:42){
          a[j,ind] <- -1
        }
      }
      if((a[j,11]) > 5){
        a[j,43] <- round(mean(tail(tp6[1:x],5)),2)
        a[j,44] <- max(tail(tp6[1:x],5))
        a[j,45] <- min(tail(tp6[1:x],5))
      }else{
        for(ind in 43:45){
          a[j,ind] <- -1
        }
      }
      if((a[j,11]) > 7){
        a[j,46] <- round(mean(tail(tp6[1:x],7)),2)
        a[j,47] <- max(tail(tp6[1:x],7))
        a[j,48] <- min(tail(tp6[1:x],7))
      }else{
        for(ind in 46:48){
          a[j,ind] <- -1
        }
      }
      if((a[j,11]) > 10){
        a[j,49] <- round(mean(tail(tp6[1:x],10)),2)
        a[j,50] <- max(tail(tp6[1:x],10))
        a[j,51] <- min(tail(tp6[1:x],10))
      }else{
        for(ind in 49:51){
          a[j,ind] <- -1
        }
      }
      if((a[j,10]) > 3){
        a[j,52] <- round(mean(tail(tp10[1:z],3)),2)
        a[j,53] <- max(tail(tp10[1:z],3))
        a[j,54] <- min(tail(tp10[1:z],3))
      }else{
        for(ind in 52:54){
          a[j,ind] <- -1
        }
      }
      if((a[j,10]) > 5){
        a[j,55] <- round(mean(tail(tp10[1:z],5)),2)
        a[j,56] <- max(tail(tp10[1:z],5))
        a[j,57] <- min(tail(tp10[1:z],5))
      }else{
        for(ind in 55:57){
          a[j,ind] <- -1
        }
      }
      if((a[j,10]) > 7){
        a[j,58] <- round(mean(tail(tp10[1:z],7)),2)
        a[j,59] <- max(tail(tp10[1:z],7))
        a[j,60] <- min(tail(tp10[1:z],7))
      }else{
        for(ind in 58:60){
          a[j,ind] <- -1
        }
      }
      if((a[j,10]) > 10){
        a[j,61] <- round(mean(tail(tp10[1:z],10)),2)
        a[j,62] <- max(tail(tp10[1:z],10))
        a[j,63] <- min(tail(tp10[1:z],10))
      }else{
        for(ind in 61:63){
          a[j,ind] <- -1
        }
      }
      if((a[j,10]-a[j,11]) > 3){
        a[j,64] <- round(mean(tail(tp9[1:y],3)),2)
        a[j,65] <- max(tail(tp9[1:y],3))
        a[j,66] <- min(tail(tp9[1:y],3))
      }else{
        for(ind in 64:66){
          a[j,ind] <- -1
        }
      }
      if((a[j,10]-a[j,11]) > 5){
        a[j,67] <- round(mean(tail(tp9[1:y],5)),2)
        a[j,68] <- max(tail(tp9[1:y],5))
        a[j,69] <- min(tail(tp9[1:y],5))
      }else{
        for(ind in 67:69){
          a[j,ind] <- -1
        }
      }
      if((a[j,10]-a[j,11]) > 7){
        a[j,70] <- round(mean(tail(tp9[1:y],7)),2)
        a[j,71] <- max(tail(tp9[1:y],7))
        a[j,72] <- min(tail(tp9[1:y],7))
      }else{
        for(ind in 70:72){
          a[j,ind] <- -1
        }
      }
      if((a[j,10]-a[j,11]) > 10){
        a[j,73] <- round(mean(tail(tp9[1:y],10)),2)
        a[j,74] <- max(tail(tp9[1:y],10))
        a[j,75] <- min(tail(tp9[1:y],10))
      }else{
        for(ind in 73:75){
          a[j,ind] <- -1
        }
      }
      if((a[j,11]) > 3){
        a[j,76] <- round(mean(tail(tp8[1:x],3)),2)
        a[j,77] <- max(tail(tp8[1:x],3))
        a[j,78] <- min(tail(tp8[1:x],3))
      }else{
        for(ind in 76:78){
          a[j,ind] <- -1
        }
      }
      if((a[j,11]) > 5){
        a[j,79] <- round(mean(tail(tp8[1:x],5)),2)
        a[j,80] <- max(tail(tp8[1:x],5))
        a[j,81] <- min(tail(tp8[1:x],5))
      }else{
        for(ind in 79:81){
          a[j,ind] <- -1
        }
      }
      if((a[j,11]) > 7){
        a[j,82] <- round(mean(tail(tp8[1:x],7)),2)
        a[j,83] <- max(tail(tp8[1:x],7))
        a[j,84] <- min(tail(tp8[1:x],7))
      }else{
        for(ind in 82:84){
          a[j,ind] <- -1
        }
      }
      if((a[j,11]) > 10){
        a[j,85] <- round(mean(tail(tp8[1:x],10)),2)
        a[j,86] <- max(tail(tp8[1:x],10))
        a[j,87] <- min(tail(tp8[1:x],10))
      }else{
        for(ind in 85:87){
          a[j,ind] <- -1
        }
      }
      if((a[j,10]) > 3){
        a[j,88] <- round(mean(tail(tp11[1:z],3)),2)
        a[j,89] <- max(tail(tp11[1:z],3))
        a[j,90] <- min(tail(tp11[1:z],3))
      }else{
        for(ind in 88:90){
          a[j,ind] <- -1
        }
      }
      if((a[j,10]) > 5){
        a[j,91] <- round(mean(tail(tp11[1:z],5)),2)
        a[j,92] <- max(tail(tp11[1:z],5))
        a[j,93] <- min(tail(tp11[1:z],5))
      }else{
        for(ind in 91:93){
          a[j,ind] <- -1
        }
      }
      if((a[j,10]) > 7){
        a[j,94] <- round(mean(tail(tp11[1:z],7)),2)
        a[j,95] <- max(tail(tp11[1:z],7))
        a[j,96] <- min(tail(tp11[1:z],7))
      }else{
        for(ind in 94:96){
          a[j,ind] <- -1
        }
      }
      if((a[j,10]) > 10){
        a[j,97] <- round(mean(tail(tp11[1:z],10)),2)
        a[j,98] <- max(tail(tp11[1:z],10))
        a[j,99] <- min(tail(tp11[1:z],10))
      }else{
        for(ind in 97:99){
          a[j,ind] <- -1
        }
      }
    }
    #pega a variavel de se o jogo foi pra prorrogacao
    #para criar a se o jogo anterior do time foi pra prorrogacao
    tp1 <- a[(a[,1] == a[j,1]), 7]
    if(length(tp1) > 1){
      tp2 <- tail(tp1[-length(tp1)], 1)
      a[j,100] <- tp2
    }else{
      a[j,100] <- FALSE
    }

    #calculo do numero de vitorias nos ultimos x jogos casa/fora/total
    #tambem vale 0 se o time ainda so teve x jogos ou menos
    x <- a[j,10] - a[j,11]
    tp12 <- a[(a[,1] == a[j,1] & a[,5] == FALSE), 8]
    if((a[j,10]-a[j,11]) > 3){
      a[j,101] <- sum(tail(tp12[1:x],3))
    }else{
      a[j,101] <- -1
    }
    if((a[j,10]-a[j,11]) > 5){
      a[j,102] <- sum(tail(tp12[1:x],5))
    }else{
      a[j,102] <- -1
    }
    if((a[j,10]-a[j,11]) > 7){
      a[j,103] <- sum(tail(tp12[1:x],7))
    }else{
      a[j,103] <- -1
    }
    if((a[j,10]-a[j,11]) > 10){
      a[j,104] <- sum(tail(tp12[1:x],10))
    }else{
      a[j,104] <- -1
    }

    y <- a[j,11]
    tp13 <- a[(a[,1] == a[j,1] & a[,5] == TRUE), 8]
    if((a[j,11]) > 3){
      a[j,105] <- sum(tail(tp13[1:y],3))
    }else{
      a[j,105] <- -1
    }
    if((a[j,11]) > 5){
      a[j,106] <- sum(tail(tp13[1:y],5))
    }else{
      a[j,106] <- -1
    }
    if((a[j,11]) > 7){
      a[j,107] <- sum(tail(tp13[1:y],7))
    }else{
      a[j,107] <- -1
    }
    if((a[j,11]) > 10){
      a[j,108] <- sum(tail(tp13[1:y],10))
    }else{
      a[j,108] <- -1
    }

    z <- a[j,10]
    tp14 <- a[(a[,1] == a[j,1]), 8]
    if((a[j,10]) > 3){
      a[j,109] <- sum(tail(tp14[1:z],3))
    }else{
      a[j,109] <- -1
    }
    if((a[j,10]) > 5){
      a[j,110] <- sum(tail(tp14[1:z],5))
    }else{
      a[j,110] <- -1
    }
    if((a[j,10]) > 7){
      a[j,111] <- sum(tail(tp14[1:z],7))
    }else{
      a[j,111] <- -1
    }
    if((a[j,10]) > 10){
      a[j,112] <- sum(tail(tp14[1:z],10))
    }else{
      a[j,112] <- -1
    }

    #pega o dia da semana do jogo
    a[j,113] <- dia_semana[i]
    #media de publico sera 0 pois o time referencia esta fora de casa na linha impar
    a[j,114] <- 0
    
    #jogos fora de casa
    a[j,115] <- a[j,10]-a[j,11]
    #vitorias fora de casa
    a[j,116] <- a[j,12]-a[j,13]
    
    #o time que joga fora de casa sempre vem de uma viagem
    a[j,117] <- TRUE
    
    #streak: quantas vitorias/derrotas consecutivas do time
    tp20 <- a[(a[,1] == a[j,1]), 8]
    streak <- tp20[-length(tp20)]
    if(length(streak)<1){
      a[j,118] <- 0
    }else{
      last <- tail(streak,1)
      k <- length(streak)-1
      if(k>0){
        while(streak[k] == last && k>0){
          k <- k-1
        }
        st <- length(streak)-k
        if(last==TRUE){
          a[j,118] <- st
        }else{
          a[j,118] <- -st
        }
      }else{
        if(last==TRUE){
          a[j,118] <- 1
        }else{
          a[j,118] <- -1
        }
      }
    }
    
    #streak em casa
    tp21 <- a[(a[,1] == a[j,1] & a[,5]==TRUE), 8]
    streakH <- tp21
    if(length(streakH)<1){
      a[j,119] <- 0
    }else{
      lastH <- tail(streakH,1)
      kH <- length(streakH)-1
      if(kH>0){
        while(streakH[kH] == lastH && kH>0){
          kH <- kH-1
        }
        stH <- length(streakH)-kH
        if(lastH==TRUE){
          a[j,119] <- stH
        }else{
          a[j,119] <- -stH
        }
      }else{
        if(lastH==TRUE){
          a[j,119] <- 1
        }else{
          a[j,119] <- -1
        }
      }
    }
    
    #streak fora de casa
    tp22 <- a[(a[,1] == a[j,1] & a[,5]==FALSE), 8]
    streakA <- tp22[-length(tp22)]
    if(length(streakA)<1){
      a[j,120] <- 0
    }else{
      lastA <- tail(streakA,1)
      kA <- length(streakA)-1
      if(kA>0){
        while(streakA[kA] == lastA && kA>0){
          kA <- kA-1
        }
        stA <- length(streakA)-kA
        if(lastA==TRUE){
          a[j,120] <- stA
        }else{
          a[j,120] <- -stA
        }
      }else{
        if(lastA==TRUE){
          a[j,120] <- 1
        }else{
          a[j,120] <- -1
        }
      }
    }
    
    #explicitando a diferenca de pontos q sera a variavel resposta na regressao
    a[j,121] <- result

    #passa para linha par
    #time referencia das linhas pares esta jogando em casa
    j <- j+1

    #mesmo procedimento, mas dessa vez sabendo que o time referencia esta jogando em casa
    #entao so mudam poucas variaveis
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
        a[j,9] <- -1
      }
    }else{
      a[j,9] <- -1
    }
    a[j,10] <- sum(oct_games[1:i,2] == a[j,1]) + sum(oct_games[1:i,4] == a[j,1]) - 1
    tp2 <- oct_games[1:i,4] == a[j,1]
    a[j,11] <- sum(tp2[-length(tp2)])
    tp4 <- a[(a[,1] == a[j,1]), 8]
    a[j,12] <- sum(tp4[-length(tp4)])
    tp5 <- a[(a[j,1] == a[,1] & a[j,5] == a[,5]),8]
    a[j,13] <- sum(tp5[-length(tp5)])
    if(a[j,10] == 0){
      for(ind in 14:99){
        a[j,ind] <- -1
      }
    }else if(a[j,11] == 0){
      for(ind in 14:16){
        a[j,ind] <- -1
      }
      x <- a[j,10] - a[j,11]
      tp6 <- a[(a[,1] == a[j,1] & a[,5] == FALSE), 3]
      a[j,17] <- round(mean(tp6[1:x]),2)
      a[j,18] <- max(tp6[1:x])
      a[j,19] <- min(tp6[1:x])
      a[j,20] <- round((a[j,14]*a[j,11]+a[j,17]*(a[j,10]-a[j,11]))/a[j,10],2)
      for(ind in 21:23){
        a[j,ind] <- -1
      }
      tp7 <- a[(a[,1] == a[j,1] & a[,5] == FALSE), 4]
      a[j,24] <- round(mean(tp7[1:x]),2)
      a[j,25] <- max(tp7[1:x])
      a[j,26] <- min(tp7[1:x])
      a[j,27] <- round((a[j,21]*a[j,11]+a[j,24]*(a[j,10]-a[j,11]))/a[j,10],2)
      if((a[j,10]-a[j,11]) > 3){
        a[j,28] <- round(mean(tail(tp6[1:x],3)),2)
        a[j,29] <- max(tail(tp6[1:x],3))
        a[j,30] <- min(tail(tp6[1:x],3))
      }else{
        for(ind in 28:30){
          a[j,ind] <- -1
        }
      }
      if((a[j,10]-a[j,11]) > 5){
        a[j,31] <- round(mean(tail(tp6[1:x],5)),2)
        a[j,32] <- max(tail(tp6[1:x],5))
        a[j,33] <- min(tail(tp6[1:x],5))
      }else{
        for(ind in 31:33){
          a[j,ind] <- -1
        }
      }
      if((a[j,10]-a[j,11]) > 7){
        a[j,34] <- round(mean(tail(tp6[1:x],7)),2)
        a[j,35] <- max(tail(tp6[1:x],7))
        a[j,36] <- min(tail(tp6[1:x],7))
      }else{
        for(ind in 34:36){
          a[j,ind] <- -1
        }
      }
      if((a[j,10]-a[j,11]) > 10){
        a[j,37] <- round(mean(tail(tp6[1:x],10)),2)
        a[j,38] <- max(tail(tp6[1:x],10))
        a[j,39] <- min(tail(tp6[1:x],10))
      }else{
        for(ind in 37:39){
          a[j,ind] <- -1
        }
      }
      for(ind in 40:63){
        a[j,ind] <- -1
      }
      if((a[j,10]-a[j,11]) > 3){
        a[j,64] <- round(mean(tail(tp7[1:x],3)),2)
        a[j,65] <- max(tail(tp7[1:x],3))
        a[j,66] <- min(tail(tp7[1:x],3))
      }else{
        for(ind in 64:66){
          a[j,ind] <- -1
        }
      }
      if((a[j,10]-a[j,11]) > 5){
        a[j,67] <- round(mean(tail(tp7[1:x],5)),2)
        a[j,68] <- max(tail(tp7[1:x],5))
        a[j,69] <- min(tail(tp7[1:x],5))
      }else{
        for(ind in 67:69){
          a[j,ind] <- -1
        }
      }
      if((a[j,10]-a[j,11]) > 7){
        a[j,70] <- round(mean(tail(tp7[1:x],7)),2)
        a[j,71] <- max(tail(tp7[1:x],7))
        a[j,72] <- min(tail(tp7[1:x],7))
      }else{
        for(ind in 70:72){
          a[j,ind] <- -1
        }
      }
      if((a[j,10]-a[j,11]) > 10){
        a[j,73] <- round(mean(tail(tp7[1:x],10)),2)
        a[j,74] <- max(tail(tp7[1:x],10))
        a[j,75] <- min(tail(tp7[1:x],10))
      }else{
        for(ind in 73:75){
          a[j,ind] <- -1
        }
      }
      for(ind in 76:99){
        a[j,ind] <- -1
      }
    }else if(a[j,10] == a[j,11]){
      x <- a[j,11]
      tp6 <- a[(a[,1] == a[j,1] & a[,5] == TRUE), 3]
      a[j,14] <- round(mean(tp6[1:x]),2)
      a[j,15] <- max(tp6[1:x])
      a[j,16] <- min(tp6[1:x])
      for(ind in 17:19){
        a[j,ind] <- -1
      }
      a[j,20] <- round((a[j,14]*a[j,11]+a[j,17]*(a[j,10]-a[j,11]))/a[j,10],2)
      tp7 <- a[(a[,1] == a[j,1] & a[,5] == TRUE), 4]
      a[j,21] <- round(mean(tp7[1:x]),2)
      a[j,22] <- max(tp7[1:x])
      a[j,23] <- min(tp7[1:x])
      for(ind in 24:26){
        a[j,ind] <- -1
      }
      a[j,27] <- round((a[j,21]*a[j,11]+a[j,24]*(a[j,10]-a[j,11]))/a[j,10],2)
      for(ind in 28:39){
        a[j,ind] <- -1
      }
      if((a[j,11]) > 3){
        a[j,40] <- round(mean(tail(tp6[1:x],3)),2)
        a[j,41] <- max(tail(tp6[1:x],3))
        a[j,42] <- min(tail(tp6[1:x],3))
      }else{
        for(ind in 40:42){
          a[j,ind] <- -1
        }
      }
      if((a[j,11]) > 5){
        a[j,43] <- round(mean(tail(tp6[1:x],5)),2)
        a[j,44] <- max(tail(tp6[1:x],5))
        a[j,45] <- min(tail(tp6[1:x],5))
      }else{
        for(ind in 43:45){
          a[j,ind] <- -1
        }
      }
      if((a[j,11]) > 7){
        a[j,46] <- round(mean(tail(tp6[1:x],7)),2)
        a[j,47] <- max(tail(tp6[1:x],7))
        a[j,48] <- min(tail(tp6[1:x],7))
      }else{
        for(ind in 46:48){
          a[j,ind] <- -1
        }
      }
      if((a[j,11]) > 10){
        a[j,49] <- round(mean(tail(tp6[1:x],10)),2)
        a[j,50] <- max(tail(tp6[1:x],10))
        a[j,51] <- min(tail(tp6[1:x],10))
      }else{
        for(ind in 49:51){
          a[j,ind] <- -1
        }
      }
      for(ind in 52:75){
        a[j,ind] <- -1
      }
      if((a[j,11]) > 3){
        a[j,76] <- round(mean(tail(tp7[1:x],3)),2)
        a[j,77] <- max(tail(tp7[1:x],3))
        a[j,78] <- min(tail(tp7[1:x],3))
      }else{
        for(ind in 76:78){
          a[j,ind] <- -1
        }
      }
      if((a[j,11]) > 5){
        a[j,79] <- round(mean(tail(tp7[1:x],5)),2)
        a[j,80] <- max(tail(tp7[1:x],5))
        a[j,81] <- min(tail(tp7[1:x],5))
      }else{
        for(ind in 79:81){
          a[j,ind] <- -1
        }
      }
      if((a[j,11]) > 7){
        a[j,82] <- round(mean(tail(tp7[1:x],7)),2)
        a[j,83] <- max(tail(tp7[1:x],7))
        a[j,84] <- min(tail(tp7[1:x],7))
      }else{
        for(ind in 82:84){
          a[j,ind] <- -1
        }
      }
      if((a[j,11]) > 10){
        a[j,85] <- round(mean(tail(tp7[1:x],10)),2)
        a[j,86] <- max(tail(tp7[1:x],10))
        a[j,87] <- min(tail(tp7[1:x],10))
      }else{
        for(ind in 85:87){
          a[j,ind] <- -1
        }
      }
      for(ind in 88:99){
        a[j,ind] <- -1
      }
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
      tp10 <- a[(a[,1] == a[j,1]), 3]
      tp11 <- a[(a[,1] == a[j,1]), 4]
      z <- a[j,10]
      if((a[j,10]-a[j,11]) > 3){
        a[j,28] <- round(mean(tail(tp7[1:y],3)),2)
        a[j,29] <- max(tail(tp7[1:y],3))
        a[j,30] <- min(tail(tp7[1:y],3))
      }else{
        for(ind in 28:30){
          a[j,ind] <- -1
        }
      }
      if((a[j,10]-a[j,11]) > 5){
        a[j,31] <- round(mean(tail(tp7[1:y],5)),2)
        a[j,32] <- max(tail(tp7[1:y],5))
        a[j,33] <- min(tail(tp7[1:y],5))
      }else{
        for(ind in 31:33){
          a[j,ind] <- -1
        }
      }
      if((a[j,10]-a[j,11]) > 7){
        a[j,34] <- round(mean(tail(tp7[1:y],7)),2)
        a[j,35] <- max(tail(tp7[1:y],7))
        a[j,36] <- min(tail(tp7[1:y],7))
      }else{
        for(ind in 34:36){
          a[j,ind] <- -1
        }
      }
      if((a[j,10]-a[j,11]) > 10){
        a[j,37] <- round(mean(tail(tp7[1:y],10)),2)
        a[j,38] <- max(tail(tp7[1:y],10))
        a[j,39] <- min(tail(tp7[1:y],10))
      }else{
        for(ind in 37:39){
          a[j,ind] <- -1
        }
      }
      if((a[j,11]) > 3){
        a[j,40] <- round(mean(tail(tp6[1:x],3)),2)
        a[j,41] <- max(tail(tp6[1:x],3))
        a[j,42] <- min(tail(tp6[1:x],3))
      }else{
        for(ind in 40:42){
          a[j,ind] <- -1
        }
      }
      if((a[j,11]) > 5){
        a[j,43] <- round(mean(tail(tp6[1:x],5)),2)
        a[j,44] <- max(tail(tp6[1:x],5))
        a[j,45] <- min(tail(tp6[1:x],5))
      }else{
        for(ind in 43:45){
          a[j,ind] <- -1
        }
      }
      if((a[j,11]) > 7){
        a[j,46] <- round(mean(tail(tp6[1:x],7)),2)
        a[j,47] <- max(tail(tp6[1:x],7))
        a[j,48] <- min(tail(tp6[1:x],7))
      }else{
        for(ind in 46:48){
          a[j,ind] <- -1
        }
      }
      if((a[j,11]) > 10){
        a[j,49] <- round(mean(tail(tp6[1:x],10)),2)
        a[j,50] <- max(tail(tp6[1:x],10))
        a[j,51] <- min(tail(tp6[1:x],10))
      }else{
        for(ind in 49:51){
          a[j,ind] <- -1
        }
      }
      if((a[j,10]) > 3){
        a[j,52] <- round(mean(tail(tp10[1:z],3)),2)
        a[j,53] <- max(tail(tp10[1:z],3))
        a[j,54] <- min(tail(tp10[1:z],3))
      }else{
        for(ind in 52:54){
          a[j,ind] <- -1
        }
      }
      if((a[j,10]) > 5){
        a[j,55] <- round(mean(tail(tp10[1:z],5)),2)
        a[j,56] <- max(tail(tp10[1:z],5))
        a[j,57] <- min(tail(tp10[1:z],5))
      }else{
        for(ind in 55:57){
          a[j,ind] <- -1
        }
      }
      if((a[j,10]) > 7){
        a[j,58] <- round(mean(tail(tp10[1:z],7)),2)
        a[j,59] <- max(tail(tp10[1:z],7))
        a[j,60] <- min(tail(tp10[1:z],7))
      }else{
        for(ind in 58:60){
          a[j,ind] <- -1
        }
      }
      if((a[j,10]) > 10){
        a[j,61] <- round(mean(tail(tp10[1:z],10)),2)
        a[j,62] <- max(tail(tp10[1:z],10))
        a[j,63] <- min(tail(tp10[1:z],10))
      }else{
        for(ind in 61:63){
          a[j,ind] <- -1
        }
      }
      if((a[j,10]-a[j,11]) > 3){
        a[j,64] <- round(mean(tail(tp9[1:y],3)),2)
        a[j,65] <- max(tail(tp9[1:y],3))
        a[j,66] <- min(tail(tp9[1:y],3))
      }else{
        for(ind in 64:66){
          a[j,ind] <- -1
        }
      }
      if((a[j,10]-a[j,11]) > 5){
        a[j,67] <- round(mean(tail(tp9[1:y],5)),2)
        a[j,68] <- max(tail(tp9[1:y],5))
        a[j,69] <- min(tail(tp9[1:y],5))
      }else{
        for(ind in 67:69){
          a[j,ind] <- -1
        }
      }
      if((a[j,10]-a[j,11]) > 7){
        a[j,70] <- round(mean(tail(tp9[1:y],7)),2)
        a[j,71] <- max(tail(tp9[1:y],7))
        a[j,72] <- min(tail(tp9[1:y],7))
      }else{
        for(ind in 70:72){
          a[j,ind] <- -1
        }
      }
      if((a[j,10]-a[j,11]) > 10){
        a[j,73] <- round(mean(tail(tp9[1:y],10)),2)
        a[j,74] <- max(tail(tp9[1:y],10))
        a[j,75] <- min(tail(tp9[1:y],10))
      }else{
        for(ind in 73:75){
          a[j,ind] <- -1
        }
      }
      if((a[j,11]) > 3){
        a[j,76] <- round(mean(tail(tp8[1:x],3)),2)
        a[j,77] <- max(tail(tp8[1:x],3))
        a[j,78] <- min(tail(tp8[1:x],3))
      }else{
        for(ind in 76:78){
          a[j,ind] <- -1
        }
      }
      if((a[j,11]) > 5){
        a[j,79] <- round(mean(tail(tp8[1:x],5)),2)
        a[j,80] <- max(tail(tp8[1:x],5))
        a[j,81] <- min(tail(tp8[1:x],5))
      }else{
        for(ind in 79:81){
          a[j,ind] <- -1
        }
      }
      if((a[j,11]) > 7){
        a[j,82] <- round(mean(tail(tp8[1:x],7)),2)
        a[j,83] <- max(tail(tp8[1:x],7))
        a[j,84] <- min(tail(tp8[1:x],7))
      }else{
        for(ind in 82:84){
          a[j,ind] <- -1
        }
      }
      if((a[j,11]) > 10){
        a[j,85] <- round(mean(tail(tp8[1:x],10)),2)
        a[j,86] <- max(tail(tp8[1:x],10))
        a[j,87] <- min(tail(tp8[1:x],10))
      }else{
        for(ind in 85:87){
          a[j,ind] <- -1
        }
      }
      if((a[j,10]) > 3){
        a[j,88] <- round(mean(tail(tp11[1:z],3)),2)
        a[j,89] <- max(tail(tp11[1:z],3))
        a[j,90] <- min(tail(tp11[1:z],3))
      }else{
        for(ind in 88:90){
          a[j,ind] <- -1
        }
      }
      if((a[j,10]) > 5){
        a[j,91] <- round(mean(tail(tp11[1:z],5)),2)
        a[j,92] <- max(tail(tp11[1:z],5))
        a[j,93] <- min(tail(tp11[1:z],5))
      }else{
        for(ind in 91:93){
          a[j,ind] <- -1
        }
      }
      if((a[j,10]) > 7){
        a[j,94] <- round(mean(tail(tp11[1:z],7)),2)
        a[j,95] <- max(tail(tp11[1:z],7))
        a[j,96] <- min(tail(tp11[1:z],7))
      }else{
        for(ind in 94:96){
          a[j,ind] <- -1
        }
      }
      if((a[j,10]) > 10){
        a[j,97] <- round(mean(tail(tp11[1:z],10)),2)
        a[j,98] <- max(tail(tp11[1:z],10))
        a[j,99] <- min(tail(tp11[1:z],10))
      }else{
        for(ind in 97:99){
          a[j,ind] <- -1
        }
      }
    }
    tp1 <- a[(a[,1] == a[j,1]), 7]
    if(length(tp1) > 1){
      tp2 <- tail(tp1[-length(tp1)], 1)
      a[j,100] <- tp2
    }else{
      a[j,100] <- FALSE
    }

    x <- a[j,10] - a[j,11]
    tp12 <- a[(a[,1] == a[j,1] & a[,5] == FALSE), 8]
    if((a[j,10]-a[j,11]) > 3){
      a[j,101] <- sum(tail(tp12[1:x],3))
    }else{
      a[j,101] <- -1
    }
    if((a[j,10]-a[j,11]) > 5){
      a[j,102] <- sum(tail(tp12[1:x],5))
    }else{
      a[j,102] <- -1
    }
    if((a[j,10]-a[j,11]) > 7){
      a[j,103] <- sum(tail(tp12[1:x],7))
    }else{
      a[j,103] <- -1
    }
    if((a[j,10]-a[j,11]) > 10){
      a[j,104] <- sum(tail(tp12[1:x],10))
    }else{
      a[j,104] <- -1
    }

    y <- a[j,11]
    tp13 <- a[(a[,1] == a[j,1] & a[,5] == TRUE), 8]
    if((a[j,11]) > 3){
      a[j,105] <- sum(tail(tp13[1:y],3))
    }else{
      a[j,105] <- -1
    }
    if((a[j,11]) > 5){
      a[j,106] <- sum(tail(tp13[1:y],5))
    }else{
      a[j,106] <- -1
    }
    if((a[j,11]) > 7){
      a[j,107] <- sum(tail(tp13[1:y],7))
    }else{
      a[j,107] <- -1
    }
    if((a[j,11]) > 10){
      a[j,108] <- sum(tail(tp13[1:y],10))
    }else{
      a[j,108] <- -1
    }

    z <- a[j,10]
    tp14 <- a[(a[,1] == a[j,1]), 8]
    if((a[j,10]) > 3){
      a[j,109] <- sum(tail(tp14[1:z],3))
    }else{
      a[j,109] <- -1
    }
    if((a[j,10]) > 5){
      a[j,110] <- sum(tail(tp14[1:z],5))
    }else{
      a[j,110] <- -1
    }
    if((a[j,10]) > 7){
      a[j,111] <- sum(tail(tp14[1:z],7))
    }else{
      a[j,111] <- -1
    }
    if((a[j,10]) > 10){
      a[j,112] <- sum(tail(tp14[1:z],10))
    }else{
      a[j,112] <- -1
    }
    a[j,113] <- dia_semana[i]

    #calculo da media de publico
    tp15 <- a[(a[,1] == a[j,1] & a[,5] == TRUE), 6]
    x <- a[j,11]
    if(x>0){
      a[j,114] <- mean(tp15[1:x])
    }else{
      a[j,114] <- -1
    }
    
    a[j,115] <- a[j,10]-a[j,11]
    a[j,116] <- a[j,12]-a[j,13]
    
    tp16 <- a[(a[,1] == a[j,1]), 5]
    if(length(tp16)>1){
      last_home <- tail(tp16[-length(tp16)],1)
      if(last_home == TRUE){
        a[j,117] <- FALSE
      }else{
        a[j,117] <- TRUE
      }
    }else{
      a[j,117] <- FALSE
    }
    
    tp20 <- a[(a[,1] == a[j,1]), 8]
    streak <- tp20[-length(tp20)]
    if(length(streak)<1){
      a[j,118] <- 0
    }else{
      last <- tail(streak,1)
      k <- length(streak)-1
      if(k>0){
        while(streak[k] == last && k>0){
          k <- k-1
        }
        st <- length(streak)-k
        if(last==TRUE){
          a[j,118] <- st
        }else{
          a[j,118] <- -st
        }
      }else{
        if(last==TRUE){
          a[j,118] <- 1
        }else{
          a[j,118] <- -1
        }
      }
    }
    
    #streak em casa
    tp21 <- a[(a[,1] == a[j,1] & a[,5]==TRUE), 8]
    streakH <- tp21[-length(tp21)]
    if(length(streakH)<1){
      a[j,119] <- 0
    }else{
      lastH <- tail(streakH,1)
      kH <- length(streakH)-1
      if(kH>0){
        while(streakH[kH] == lastH && kH>0){
          kH <- kH-1
        }
        stH <- length(streakH)-kH
        if(lastH==TRUE){
          a[j,119] <- stH
        }else{
          a[j,119] <- -stH
        }
      }else{
        if(lastH==TRUE){
          a[j,119] <- 1
        }else{
          a[j,119] <- -1
        }
      }
    }
    
    #streak fora de casa
    tp22 <- a[(a[,1] == a[j,1] & a[,5]==FALSE), 8]
    streakA <- tp22
    if(length(streakA)<1){
      a[j,120] <- 0
    }else{
      lastA <- tail(streakA,1)
      kA <- length(streakA)-1
      if(kA>0){
        while(streakA[kA] == lastA && kA>0){
          kA <- kA-1
        }
        stA <- length(streakA)-kA
        if(lastA==TRUE){
          a[j,120] <- stA
        }else{
          a[j,120] <- -stA
        }
      }else{
        if(lastA==TRUE){
          a[j,120] <- 1
        }else{
          a[j,120] <- -1
        }
      }
    }
    
    a[j,121] <- -result

    j <- j+1
  }

  names(a) <- c("Team", "Opp", "Pts_S", "Pts_A", "Home", "Attend", "OT", "Win", "Days_LG", "Games_T", "Games_H",
                "Wins_T", "Wins_H", "Mean_Pts_S_H", "Max_Pts_S_H", "Min_Pts_S_H", "Mean_Pts_S_A", "Max_Pts_S_A", "Min_Pts_S_A",
                "Mean_Pts_S_T", "Mean_Pts_A_H", "Max_Pts_A_H", "Min_Pts_A_H", "Mean_Pts_A_A", "Max_Pts_A_A", "Min_Pts_A_A",
                "Mean_Pts_A_T", "Mean_Last3_away", "Max_Last3_away", "Min_Last3away", "Mean_Last5_away", "Max_Last5_away", "Min_Last5away",
                "Mean_Last7_away", "Max_Last7_away", "Min_Last7away","Mean_Last10_away", "Max_Last10_away", "Min_Last10away",
                "Mean_Last3_home", "Max_Last3_home", "Min_Last3home", "Mean_Last5_home", "Max_Last5_home", "Min_Last5home",
                "Mean_Last7_home", "Max_Last7_home", "Min_Last7home","Mean_Last10_home", "Max_Last10_home", "Min_Last10home",
                "Mean_Last3_total", "Max_Last3_total", "Min_Last3total", "Mean_Last5_total", "Max_Last5_total", "Min_Last5total",
                "Mean_Last7_total", "Max_Last7_total", "Min_Last7total","Mean_Last10_total", "Max_Last10_total", "Min_Last10total",
                "Mean_Last3_away_opp", "Max_Last3_away_opp", "Min_Last3away_opp", "Mean_Last5_away_opp", "Max_Last5_away_opp", "Min_Last5away_opp",
                "Mean_Last7_away_opp", "Max_Last7_away_opp", "Min_Last7away_opp","Mean_Last10_away_opp", "Max_Last10_away_opp", "Min_Last10away_opp",
                "Mean_Last3_home_opp", "Max_Last3_home_opp", "Min_Last3home_opp", "Mean_Last5_home_opp", "Max_Last5_home_opp", "Min_Last5home_opp",
                "Mean_Last7_home_opp", "Max_Last7_home_opp", "Min_Last7home_opp","Mean_Last10_home_opp", "Max_Last10_home_opp", "Min_Last10home_opp",
                "Mean_Last3_total_opp", "Max_Last3_total_opp", "Min_Last3total_opp", "Mean_Last5_total_opp", "Max_Last5_total_opp", "Min_Last5total_opp",
                "Mean_Last7_total_opp", "Max_Last7_total_opp", "Min_Last7total_opp","Mean_Last10_total_opp", "Max_Last10_total_opp", "Min_Last10total_opp",
                "OT_last","Win_Last3_away","Win_Last5_away","Win_Last7_away","Win_Last10_away","Win_Last3_home","Win_Last5_home","Win_Last7_home",
                "Win_Last10_home","Win_Last3_total","Win_Last5_total","Win_Last7_total","Win_Last10_total","weekday","mean_attend","Games_A","Wins_A",
                "Travel","Streak_T","Streak_H","Streak_A","result")


  #calculo do strength of schedule
  a[1,122] <- -1
  a[2,122] <- -1
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
          w <- w+a[e,12]+(a[e,121]>0)
          g <- g+a[e,10]+1
        }
      }
      if(g > 0){
        a[i,122] <- w/g
      }else{
        a[i,122] <- -1
      }
    }else{
      a[i,122] <- -1
    }
  }
  names(a)[122] <- "Str_Sch"

  return(a)
}

# salva as bases
base2013 <- base_var(jogos_2013)
base2014 <- base_var(jogos_2014)
base2015 <- base_var(jogos_2015)
base2016 <- base_var(jogos_2016)
base2017 <- base_var(jogos_2017)
base2018 <- base_var(jogos_2018)

save(base2013,base2014,base2015,base2016,base2017,base2018, file="bases.rda")


################# bases finais

load(file="bases.rda")

junto <- function(a){
  b <- data.frame()
  for(i in seq(1,length(a[,1]),by=2)){
    d <- cbind(a[i,],a[i+1,])
    b <- rbind(b,d)
  }
  return(b)
}

final_base <- function(base){
  dados <- junto(base)
  dados <- dados[,-c(1:8,113:114,121,123:129)]
  return(dados)
}

final2013 <- final_base(base2013)
final2014 <- final_base(base2014)
final2015 <- final_base(base2015)
final2016 <- final_base(base2016)
final2017 <- final_base(base2017)
final2018 <- final_base(base2018)

save(final2013,final2014,final2015,final2016,final2017,final2018, file="final.rda")

######## testes - arrumando algumas coisas na base
###tirar a coluna de win p/ reg linear
setwd("~/tcc")
load(file="final.rda")

#base full
final <- rbind(final2013[1:1230,],final2014[1:1230,],final2015[1:1230,],final2016[1:1230,],final2017[1:1230,])
#112 win
#225 result

#tirando variaveis de casa pro time de fora e de fora pro time de casa
final <- final[,-c(3, 5:8, 13:15, 32:43, 68:79, 97:100, 107, 109, 121:123, 128:130, 132:143, 168:179, 205:208, 219:220, 224)]
#74 win
#150 result


#encontrando os padroes
padrao <- final[,-c(71:72,74,148:150)] < 0

for(i in 1:6150){
  for(j in 1:145){
    padrao[i,j] <- as.numeric(padrao[i,j])
  }
}

oi <- apply(padrao,1,paste,collapse="")
summary(as.factor(oi))

#pegando apenas as linhas que tem info de todas as varaiveis
full <- oi=="0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000" 
td <- final[full,]

final2018 <- final2018[,-c(3, 5:8, 13:15, 32:43, 68:79, 97:100, 107, 109, 121:123, 128:130, 132:143, 168:179, 205:208, 219:220, 224)]
padrao18 <- final2018[1:1230,-c(71:72,74,148:150)] < 0

for(i in 1:1230){
  for(j in 1:145){
    padrao18[i,j] <- as.numeric(padrao18[i,j])
  }
}

oi18 <- apply(padrao18,1,paste,collapse="")
summary(as.factor(oi18))

full18 <- oi18=="0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000" 
td18 <- final2018[full18,]

td <- td[,(!td[1,] == -1)]
td18 <- td18[,(!td18[1,] == -1)]


#######REG LINEAR
mod <- lm(result~., data=td[,-74])
summary(mod)
win <- (predict(mod, newdata=td18[,-74]) > 0) == (td18$result > 0)
mean(win)

#sem tirar nenhuma variavel
mod2 <- lm(result~., data=final[,-106])
summary(mod2)
win2 <- (predict(mod2, newdata=final2018[,-106]) > 0) == (final2018$result > 0)
mean(win2)

#selecao de variaveis
require(leaps)
#com o primeiro (com menos variaveis)
modmin <- lm(result ~ 1, data=td[,-74])
step(modmin, direction='forward', scope=( ~ Days_LG + Games_T + Wins_T + Mean_Pts_S_A + Max_Pts_S_A + 
                                            Min_Pts_S_A + Mean_Pts_S_T + Mean_Pts_A_A + Max_Pts_A_A + 
                                            Min_Pts_A_A + Mean_Pts_A_T + Mean_Last3_away + Max_Last3_away + 
                                            Min_Last3away + Mean_Last5_away + Max_Last5_away + Min_Last5away + 
                                            Mean_Last7_away + Max_Last7_away + Min_Last7away + Mean_Last10_away + 
                                            Max_Last10_away + Min_Last10away + Mean_Last3_total + Max_Last3_total + 
                                            Min_Last3total + Mean_Last5_total + Max_Last5_total + Min_Last5total + 
                                            Mean_Last7_total + Max_Last7_total + Min_Last7total + Mean_Last10_total + 
                                            Max_Last10_total + Min_Last10total + Mean_Last3_away_opp + 
                                            Max_Last3_away_opp + Min_Last3away_opp + Mean_Last5_away_opp + 
                                            Max_Last5_away_opp + Min_Last5away_opp + Mean_Last7_away_opp + 
                                            Max_Last7_away_opp + Min_Last7away_opp + Mean_Last10_away_opp + 
                                            Max_Last10_away_opp + Min_Last10away_opp + Mean_Last3_total_opp + 
                                            Max_Last3_total_opp + Min_Last3total_opp + Mean_Last5_total_opp + 
                                            Max_Last5_total_opp + Min_Last5total_opp + Mean_Last7_total_opp + 
                                            Max_Last7_total_opp + Min_Last7total_opp + Mean_Last10_total_opp + 
                                            Max_Last10_total_opp + Min_Last10total_opp + OT_last + Win_Last3_away + 
                                            Win_Last5_away + Win_Last7_away + Win_Last10_away + Win_Last3_total + 
                                            Win_Last5_total + Win_Last7_total + Win_Last10_total + Games_A + 
                                            Wins_A + Streak_T + Streak_A + Str_Sch + Days_LG.1 + Games_T.1 + 
                                            Games_H.1 + Wins_T.1 + Wins_H.1 + Mean_Pts_S_H.1 + Max_Pts_S_H.1 + 
                                            Min_Pts_S_H.1 + Mean_Pts_S_T.1 + Mean_Pts_A_H.1 + Max_Pts_A_H.1 + 
                                            Min_Pts_A_H.1 + Mean_Pts_A_T.1 + Mean_Last3_home.1 + Max_Last3_home.1 + 
                                            Min_Last3home.1 + Mean_Last5_home.1 + Max_Last5_home.1 + 
                                            Min_Last5home.1 + Mean_Last7_home.1 + Max_Last7_home.1 + 
                                            Min_Last7home.1 + Mean_Last10_home.1 + Max_Last10_home.1 + 
                                            Min_Last10home.1 + Mean_Last3_total.1 + Max_Last3_total.1 + 
                                            Min_Last3total.1 + Mean_Last5_total.1 + Max_Last5_total.1 + 
                                            Min_Last5total.1 + Mean_Last7_total.1 + Max_Last7_total.1 + 
                                            Min_Last7total.1 + Mean_Last10_total.1 + Max_Last10_total.1 + 
                                            Min_Last10total.1 + Mean_Last3_home_opp.1 + Max_Last3_home_opp.1 + 
                                            Min_Last3home_opp.1 + Mean_Last5_home_opp.1 + Max_Last5_home_opp.1 + 
                                            Min_Last5home_opp.1 + Mean_Last7_home_opp.1 + Max_Last7_home_opp.1 + 
                                            Min_Last7home_opp.1 + Mean_Last10_home_opp.1 + Max_Last10_home_opp.1 + 
                                            Min_Last10home_opp.1 + Mean_Last3_total_opp.1 + Max_Last3_total_opp.1 + 
                                            Min_Last3total_opp.1 + Mean_Last5_total_opp.1 + Max_Last5_total_opp.1 + 
                                            Min_Last5total_opp.1 + Mean_Last7_total_opp.1 + Max_Last7_total_opp.1 + 
                                            Min_Last7total_opp.1 + Mean_Last10_total_opp.1 + Max_Last10_total_opp.1 + 
                                            Min_Last10total_opp.1 + OT_last.1 + Win_Last3_home.1 + Win_Last5_home.1 + 
                                            Win_Last7_home.1 + Win_Last10_home.1 + Win_Last3_total.1 + 
                                            Win_Last5_total.1 + Win_Last7_total.1 + Win_Last10_total.1 + 
                                            weekday + mean_attend + Travel.1 + Streak_T.1 + Streak_H.1 + 
                                            Str_Sch.1))

modforw <- lm(formula = result ~ Streak_T + Streak_T.1 + Wins_T + Wins_T.1 + 
                OT_last + Travel.1 + Days_LG + Mean_Pts_A_T + Mean_Pts_S_T + 
                Games_H.1 + OT_last.1 + Min_Pts_A_A + Min_Last3total.1 + 
                Min_Last3total_opp.1 + Win_Last5_total.1 + Min_Last5total.1 + 
                Min_Last5total_opp.1 + Max_Last3_total.1 + Mean_Last5_total_opp.1, 
              data = td[, -74])
summary(modforw)
winfor <- (predict(modforw, newdata=td18[,-74]) > 0) == (td18$result > 0)
mean(winfor)

step(mod, direction='backward', scope=( ~ 1))
modback <- lm(formula = result ~ Days_LG + Games_T + Wins_T + Mean_Pts_S_T + 
                Max_Pts_A_A + Mean_Pts_A_T + Min_Last3total + Min_Last5total + 
                Mean_Last7_total + Max_Last7_total + Min_Last7total + Mean_Last10_total + 
                Max_Last10_total + Min_Last10total + Min_Last5away_opp + 
                Min_Last3total_opp + Min_Last10total_opp + Win_Last3_away + 
                Win_Last7_total + Str_Sch + Days_LG.1 + Games_H.1 + Mean_Pts_S_T.1 + 
                Mean_Pts_A_T.1 + Min_Last3home.1 + Mean_Last7_total.1 + Min_Last7total.1 + 
                Min_Last10total.1 + Max_Last3_home_opp.1 + Max_Last5_home_opp.1 + 
                Min_Last10home_opp.1 + Mean_Last3_total_opp.1 + Max_Last5_total_opp.1 + 
                Min_Last7total_opp.1 + Min_Last10total_opp.1 + Win_Last3_home.1 + 
                Win_Last10_home.1 + Win_Last3_total.1 + Str_Sch.1, data = td[,-72])
summary(modback)
winback <- (predict(modback, newdata=td18[,-72]) > 0) == (td18$result > 0)
mean(winback)


####REG LOGISTICA
a <- glm(Win~., data = td[,-150], family=binomial(link = "logit"))
summary(a)
library(dplyr)
probabilities <- a %>% predict(td18[,-150], type = "response")
predicted.classes <- ifelse(probabilities > 0.5, "TRUE", "FALSE")
mean(predicted.classes == td18$Win)

#sem tirar nenhuma variavel
a2 <- glm(Win~., data = final[,-213], family=binomial(link = "logit"))
summary(a2)
library(dplyr)
probabilities2 <- a2 %>% predict(final2018[1:1230,-213], type = "response")
predicted.classes2 <- ifelse(probabilities2 > 0.5, "TRUE", "FALSE")
mean(predicted.classes2 == final2018[1:1230,]$Win)

#selecao de variaveis
modmin2 <- glm(Win~1, data = td[,-150], family=binomial(link = "logit"))
step(modmin2, direction='forward', scope=( ~ Days_LG + Games_T + Wins_T + Mean_Pts_S_A + Max_Pts_S_A + 
                                             Min_Pts_S_A + Mean_Pts_S_T + Mean_Pts_A_A + Max_Pts_A_A + 
                                             Min_Pts_A_A + Mean_Pts_A_T + Mean_Last3_away + Max_Last3_away + 
                                             Min_Last3away + Mean_Last5_away + Max_Last5_away + Min_Last5away + 
                                             Mean_Last7_away + Max_Last7_away + Min_Last7away + Mean_Last10_away + 
                                             Max_Last10_away + Min_Last10away + Mean_Last3_total + Max_Last3_total + 
                                             Min_Last3total + Mean_Last5_total + Max_Last5_total + Min_Last5total + 
                                             Mean_Last7_total + Max_Last7_total + Min_Last7total + Mean_Last10_total + 
                                             Max_Last10_total + Min_Last10total + Mean_Last3_away_opp + 
                                             Max_Last3_away_opp + Min_Last3away_opp + Mean_Last5_away_opp + 
                                             Max_Last5_away_opp + Min_Last5away_opp + Mean_Last7_away_opp + 
                                             Max_Last7_away_opp + Min_Last7away_opp + Mean_Last10_away_opp + 
                                             Max_Last10_away_opp + Min_Last10away_opp + Mean_Last3_total_opp + 
                                             Max_Last3_total_opp + Min_Last3total_opp + Mean_Last5_total_opp + 
                                             Max_Last5_total_opp + Min_Last5total_opp + Mean_Last7_total_opp + 
                                             Max_Last7_total_opp + Min_Last7total_opp + Mean_Last10_total_opp + 
                                             Max_Last10_total_opp + Min_Last10total_opp + OT_last + Win_Last3_away + 
                                             Win_Last5_away + Win_Last7_away + Win_Last10_away + Win_Last3_total + 
                                             Win_Last5_total + Win_Last7_total + Win_Last10_total + Games_A + 
                                             Wins_A + Streak_T + Streak_A + Str_Sch + Days_LG.1 + Games_T.1 + 
                                             Games_H.1 + Wins_T.1 + Wins_H.1 + Mean_Pts_S_H.1 + Max_Pts_S_H.1 + 
                                             Min_Pts_S_H.1 + Mean_Pts_S_T.1 + Mean_Pts_A_H.1 + Max_Pts_A_H.1 + 
                                             Min_Pts_A_H.1 + Mean_Pts_A_T.1 + Mean_Last3_home.1 + Max_Last3_home.1 + 
                                             Min_Last3home.1 + Mean_Last5_home.1 + Max_Last5_home.1 + 
                                             Min_Last5home.1 + Mean_Last7_home.1 + Max_Last7_home.1 + 
                                             Min_Last7home.1 + Mean_Last10_home.1 + Max_Last10_home.1 + 
                                             Min_Last10home.1 + Mean_Last3_total.1 + Max_Last3_total.1 + 
                                             Min_Last3total.1 + Mean_Last5_total.1 + Max_Last5_total.1 + 
                                             Min_Last5total.1 + Mean_Last7_total.1 + Max_Last7_total.1 + 
                                             Min_Last7total.1 + Mean_Last10_total.1 + Max_Last10_total.1 + 
                                             Min_Last10total.1 + Mean_Last3_home_opp.1 + Max_Last3_home_opp.1 + 
                                             Min_Last3home_opp.1 + Mean_Last5_home_opp.1 + Max_Last5_home_opp.1 + 
                                             Min_Last5home_opp.1 + Mean_Last7_home_opp.1 + Max_Last7_home_opp.1 + 
                                             Min_Last7home_opp.1 + Mean_Last10_home_opp.1 + Max_Last10_home_opp.1 + 
                                             Min_Last10home_opp.1 + Mean_Last3_total_opp.1 + Max_Last3_total_opp.1 + 
                                             Min_Last3total_opp.1 + Mean_Last5_total_opp.1 + Max_Last5_total_opp.1 + 
                                             Min_Last5total_opp.1 + Mean_Last7_total_opp.1 + Max_Last7_total_opp.1 + 
                                             Min_Last7total_opp.1 + Mean_Last10_total_opp.1 + Max_Last10_total_opp.1 + 
                                             Min_Last10total_opp.1 + OT_last.1 + Win_Last3_home.1 + Win_Last5_home.1 + 
                                             Win_Last7_home.1 + Win_Last10_home.1 + Win_Last3_total.1 + 
                                             Win_Last5_total.1 + Win_Last7_total.1 + Win_Last10_total.1 + 
                                             weekday + mean_attend + Travel.1 + Streak_T.1 + Streak_H.1 + 
                                             Str_Sch.1))

modforw2 <- glm(formula = Win ~ Streak_T.1 + Streak_T + Wins_T + Wins_T.1 + 
                  Days_LG + OT_last + Travel.1 + Mean_Pts_A_T + Mean_Pts_S_T + 
                  Games_H.1 + Min_Pts_A_A + OT_last.1 + Max_Last3_total.1 + 
                  Min_Last3total_opp.1 + Win_Last5_total.1 + Min_Last5total.1 + 
                  Min_Last5total_opp.1 + Max_Last3_total_opp.1, family = binomial(link = "logit"), 
                data = td[, -150])
summary(modforw2)
probabilities3 <- modforw2 %>% predict(td18[,-150], type = "response")
predicted.classes3 <- ifelse(probabilities3 > 0.5, "TRUE", "FALSE")
mean(predicted.classes3 == td18$Win)

#########SVM
library(e1071)
teste <- svm(result~., data=td[,-74])
summary(teste)
winsvm <- (predict(teste, newdata=td18[,-74]) > 0) == (td18$result > 0)
mean(winsvm)

teste1 <- svm(result ~ Win_Last10_home.1 + Win_Last10_total + Wins_T.1 + 
                Wins_T + Wins_H.1 + Max_Last3_total_opp.1 + Min_Last5home.1 + 
                Win_Last3_home.1 + Mean_Pts_A_T.1 + Mean_Last10_total.1 + 
                Games_T + Mean_Pts_S_H.1 + Win_Last7_total + Games_H.1 + 
                Win_Last3_away + Mean_Last10_away_opp + Mean_Pts_S_T + Mean_Pts_A_T + 
                Min_Last3total + Min_Last7home.1 + Streak_T + Min_Last3total_opp + 
                Min_Last5total_opp.1 + Win_Last3_total.1 + Mean_Last5_total.1 + 
                Min_Last10home_opp.1 + Max_Last5_home_opp.1 + Mean_Pts_A_H.1 + 
                Games_T.1 + Streak_H.1 + Min_Last10total_opp + Min_Last7total.1 + 
                Mean_Last5_home.1 + Max_Pts_S_H.1 + Min_Last3total.1, data = td[, -74])
summary(teste1)
win1 <- (predict(teste1, newdata=td18[,-74]) > 0) == (td18$result > 0)
mean(win1)


teste2 <- svm(as.factor(Win) ~ Win_Last10_home.1 + Win_Last10_total + Wins_T.1 + 
                Wins_T + Wins_H.1 + Max_Last3_total_opp.1 + Min_Last5home.1 + 
                Win_Last3_home.1 + Mean_Pts_A_T.1 + Mean_Last10_total.1 + 
                Games_T + Mean_Pts_S_H.1 + Win_Last7_total + Games_H.1 + 
                Win_Last3_away + Mean_Last10_away_opp + Mean_Pts_S_T + Mean_Pts_A_T + 
                Min_Last3total + Min_Last7home.1 + Streak_T + Min_Last3total_opp + 
                Min_Last5total_opp.1 + Win_Last3_total.1 + Mean_Last5_total.1 + 
                Min_Last10home_opp.1 + Max_Last5_home_opp.1 + Mean_Pts_A_H.1 + 
                Games_T.1 + Streak_H.1 + Min_Last10total_opp + Min_Last7total.1 + 
                Mean_Last5_home.1 + Max_Pts_S_H.1 + Min_Last3total.1, data = td[, -150])
summary(teste2)
win2 <- predict(teste2, newdata=td18[,-150]) == td18$Win
mean(win2)


####################TESTES - MUITO DEMORADO
#melhores parametros para o svm
tuneResult <- tune(svm, result ~ Win_Last10_home.1 + Win_Last10_total + Wins_T.1 + 
                     Wins_T + Wins_H.1 + Max_Last3_total_opp.1 + Min_Last5home.1 + 
                     Win_Last3_home.1 + Mean_Pts_A_T.1 + Mean_Last10_total.1 + 
                     Games_T + Mean_Pts_S_H.1 + Win_Last7_total + Games_H.1 + 
                     Win_Last3_away + Mean_Last10_away_opp + Mean_Pts_S_T + Mean_Pts_A_T + 
                     Min_Last3total + Min_Last7home.1 + Streak_T + Min_Last3total_opp + 
                     Min_Last5total_opp.1 + Win_Last3_total.1 + Mean_Last5_total.1 + 
                     Min_Last10home_opp.1 + Max_Last5_home_opp.1 + Mean_Pts_A_H.1 + 
                     Games_T.1 + Streak_H.1 + Min_Last10total_opp + Min_Last7total.1 + 
                     Mean_Last5_home.1 + Max_Pts_S_H.1 + Min_Last3total.1, data = td[, -74],
                   ranges = list(epsilon = seq(0,1,0.1), cost = 2^(2:9)))


####melhores modelos com x variaveis
library(leaps)
modelos<-regsubsets(result ~ .,data=td[,-72],nbest=1,nvmax=20,really.big=T)
summary(leaps)

###selecao stepwise
library(MASS)
stepAIC(mod, direction = "both")