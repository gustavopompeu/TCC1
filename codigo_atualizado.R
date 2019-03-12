setwd("C:/Gustavo/Trabalhos/UnB/TCC/LATEX/tcc_git")
load(file="jogos.rda")

##Leitura dos dados da internet
ler_jogos <- function(url){
  library(rvest)
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
  }else if(year == 2012){
    months <- c("december","january","february","march","april","may","june")
  }else if(year == 2006 | year == 2005 | year == 2000){
    months <- c("november","december","january","february","march","april","may","june")
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

#bases dos jogos das temporadas anteriores, ja estao salvas
#jogos_2001 <- tabela_temporada(2001)
#jogos_2002 <- tabela_temporada(2002)
#jogos_2003 <- tabela_temporada(2003)
#jogos_2004 <- tabela_temporada(2004)
#jogos_2005 <- tabela_temporada(2005)
#jogos_2006 <- tabela_temporada(2006)
#jogos_2007 <- tabela_temporada(2007)
#jogos_2008 <- tabela_temporada(2008)
#jogos_2009 <- tabela_temporada(2009)
#jogos_2010 <- tabela_temporada(2010)
#jogos_2011 <- tabela_temporada(2011)
#jogos_2012 <- tabela_temporada(2012)
#jogos_2013 <- tabela_temporada(2013)
#jogos_2014 <- tabela_temporada(2014)
#jogos_2015 <- tabela_temporada(2015)
#jogos_2016 <- tabela_temporada(2016)
#jogos_2017 <- tabela_temporada(2017)
#jogos_2018 <- tabela_temporada(2018)

##Temporada atual, atualizando diariamente
atualiza <- function(){
  load(file="jogos.rda")
  jogos_2019 <- tabela_temporada(2019)
  ind <- which(is.na(jogos_2019$PTS_Visitor))[1]
  dat <- jogos_2019[ind,1]
  jog <- which(jogos_2019$Date == dat)
  ind2 <- jog[length(jog)]
  jogos_2019 <- jogos_2019[1:ind2,]
  save(jogos_2001,jogos_2002,jogos_2003,jogos_2004,jogos_2005,jogos_2006,jogos_2007,jogos_2008,
       jogos_2009,jogos_2010,jogos_2011,jogos_2012,jogos_2013,jogos_2014,
       jogos_2015,jogos_2016,jogos_2017,jogos_2018,jogos_2019, file="jogos.rda")
  load(file="bases.rda")
  base2019 <- base_var(jogos_2019)
  save(base2001,base2002,base2003,base2004,base2005,base2006,base2007,base2008,
       base2009,base2010,base2011,base2012,base2013,base2014,
       base2015,base2016,base2017,base2018,base2019, file="bases.rda")
  load(file="final.rda")
  final2019 <- final_base(base2019)
  save(final2001,final2002,final2003,final2004,final2005,final2006,final2007,
       final2008,final2009,final2010,final2011,final2012,
       final2013,final2014,final2015,final2016,final2017,
       final2018,final2019, file="final.rda")
}

atualiza()

jogos_novos$X__1 <- as.Date(jogos_novos$X__1)
names(jogos_novos) <- c("Date", "Visitor", "PTS_Visitor", "Home", "PTS_Home", "OT", "Attend")
jogos_2019 <- jogos_2019[1:872,]
jogos_2019 <- rbind(jogos_2019,jogos_novos)

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
#124: diferenca de pontos entre os times ao fim do jogo (com base no time referencia)
#(result negativo significa derrota do time referencia, result positivo significa vitoria)

#continua da temporada
#114: media de publico do time referencia ate agora na temporada (0 se o time estiver jogando fora de casa)
#115: total de jogos do time referencia fora de casa ate o momento
#116: total de vitorias do time referencia fora de casa ate o momento
#117: logi, se o time viajou do ultimo jogo para esse 
#(so nao tera viajado se estiver jogando jogos consecutivos em casa)
#118-120: streak (vitorias ou derrotas consecutivas do time referencia) - total, em casa e fora
#(positivo pra vitorias e negativo pra derrotas)
#121-123: derrotas do time referencia ate agora na temporada (total, casa e fora)
#125: strength of schedule do time referencia (referente a temporada)
#(porcentagem de vitorias de TODOS os adversarios que o time referencia ja enfrentou ate o momento)

base_var <- function(oct_games){
  Sys.setlocale("LC_TIME","Portuguese")
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
    #publico sempre NA pois o time referencia esta jogando fora de casa
    a[j,6] <- NA
    #pega da tabela de resultados pra saber se esse jogo foi pra prorrogacao
    if(is.na(oct_games[i,6]) == T) a[j,7] <- FALSE; if(is.na(oct_games[i,6]) == F) a[j,7] <- TRUE
    #calculo da diferenca de pontos entre os times para saber o vencedor
    result <- a[j,3] - a[j,4]
    if(is.na(result)){
      a[j,8] <- NA
    }else{
      if(result<0){
        a[j,8] <- FALSE
      }else if(result>0){
        a[j,8] <- TRUE
      }
    }

    #calculo de quantos dias atras foi o ultimo jogo do time referencia
    #(NA se for o primeiro jogo da temporada)
    if(i>1){
      temp <- oct_games[1:i,2] == a[j,1] | oct_games[1:i,4] == a[j,1]
      if(sum(temp) > 1){
        temp2 <- which(temp == TRUE)
        ind <- max(temp2[-length(temp2)])
        a[j,9] <- as.numeric(oct_games[i,1]-oct_games[ind,1])
      }else{
        a[j,9] <- NA
      }
    }else{
      a[j,9] <- NA
    }
    #total de jogos
    a[j,10] <- sum(oct_games[1:i,2] == a[j,1]) + sum(oct_games[1:i,4] == a[j,1]) - 1
    #total de jogos em casa
    tp <- oct_games[1:i,4] == a[j,1]
    a[j,11] <- sum(tp[-length(tp)])
    #total de vitorias
    tp3 <- a[(a[,1] == a[j,1]), 8]
    if(length(tp3)>1){
      a[j,12] <- sum(tp3[-length(tp3)])
    }else{
      a[j,12] <- NA
    }
    #se o total de jogos do time ainda for 0
    #tudo daqui pra frente nao tem dado
    #(significa q e o primeiro jogo do time na temporada, ou seja, nao ha info de jogos anteriores)
    if(a[j,10] == 0){
      a[j,13] <- NA
      for(ind in 14:99){
        a[j,ind] <- NA
      }
    #se o total de jogos em casa for 0
    #(time so jogou fora de casa por enquanto)
    }else if(a[j,11] == 0){
      #as variaveis referentes a jogos em casa sao NA
      for(ind in 13:16){
        a[j,ind] <- NA
      }
      #as variaveis de pontos marcados fora e total, pelo time e contra o time sao preenchidas normalmente
      x <- a[j,10] - a[j,11]
      tp6 <- a[(a[,1] == a[j,1] & a[,5] == FALSE), 3]
      a[j,17] <- round(mean(tp6[1:x]),2)
      a[j,18] <- max(tp6[1:x])
      a[j,19] <- min(tp6[1:x])
      a[j,20] <- round((a[j,14]*a[j,11]+a[j,17]*(a[j,10]-a[j,11]))/a[j,10],2)
      for(ind in 21:23){
        a[j,ind] <- NA
      }
      tp7 <- a[(a[,1] == a[j,1] & a[,5] == FALSE), 4]
      a[j,24] <- round(mean(tp7[1:x]),2)
      a[j,25] <- max(tp7[1:x])
      a[j,26] <- min(tp7[1:x])
      a[j,27] <- round((a[j,21]*a[j,11]+a[j,24]*(a[j,10]-a[j,11]))/a[j,10],2)
      #aqui comecam os pontos marcados nos ultimos x jogos
      #se o time jogou x ou menos jogos ate agora na temporada, elas sao NA
      if((a[j,10]-a[j,11]) > 3){
        a[j,28] <- round(mean(tail(tp6[1:x],3)),2)
        a[j,29] <- max(tail(tp6[1:x],3))
        a[j,30] <- min(tail(tp6[1:x],3))
      }else{
        for(ind in 28:30){
          a[j,ind] <- NA
        }
      }
      if((a[j,10]-a[j,11]) > 5){
        a[j,31] <- round(mean(tail(tp6[1:x],5)),2)
        a[j,32] <- max(tail(tp6[1:x],5))
        a[j,33] <- min(tail(tp6[1:x],5))
      }else{
        for(ind in 31:33){
          a[j,ind] <- NA
        }
      }
      if((a[j,10]-a[j,11]) > 7){
        a[j,34] <- round(mean(tail(tp6[1:x],7)),2)
        a[j,35] <- max(tail(tp6[1:x],7))
        a[j,36] <- min(tail(tp6[1:x],7))
      }else{
        for(ind in 34:36){
          a[j,ind] <- NA
        }
      }
      if((a[j,10]-a[j,11]) > 10){
        a[j,37] <- round(mean(tail(tp6[1:x],10)),2)
        a[j,38] <- max(tail(tp6[1:x],10))
        a[j,39] <- min(tail(tp6[1:x],10))
      }else{
        for(ind in 37:39){
          a[j,ind] <- NA
        }
      }
      for(ind in 40:63){
        a[j,ind] <- NA
      }
      if((a[j,10]-a[j,11]) > 3){
        a[j,64] <- round(mean(tail(tp7[1:x],3)),2)
        a[j,65] <- max(tail(tp7[1:x],3))
        a[j,66] <- min(tail(tp7[1:x],3))
      }else{
        for(ind in 64:66){
          a[j,ind] <- NA
        }
      }
      if((a[j,10]-a[j,11]) > 5){
        a[j,67] <- round(mean(tail(tp7[1:x],5)),2)
        a[j,68] <- max(tail(tp7[1:x],5))
        a[j,69] <- min(tail(tp7[1:x],5))
      }else{
        for(ind in 67:69){
          a[j,ind] <- NA
        }
      }
      if((a[j,10]-a[j,11]) > 7){
        a[j,70] <- round(mean(tail(tp7[1:x],7)),2)
        a[j,71] <- max(tail(tp7[1:x],7))
        a[j,72] <- min(tail(tp7[1:x],7))
      }else{
        for(ind in 70:72){
          a[j,ind] <- NA
        }
      }
      if((a[j,10]-a[j,11]) > 10){
        a[j,73] <- round(mean(tail(tp7[1:x],10)),2)
        a[j,74] <- max(tail(tp7[1:x],10))
        a[j,75] <- min(tail(tp7[1:x],10))
      }else{
        for(ind in 73:75){
          a[j,ind] <- NA
        }
      }
      for(ind in 76:99){
        a[j,ind] <- NA
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
      #as variaveis de jogos fora sao NA
      for(ind in 17:19){
        a[j,ind] <- NA
      }
      a[j,20] <- round((a[j,14]*a[j,11]+a[j,17]*(a[j,10]-a[j,11]))/a[j,10],2)
      tp7 <- a[(a[,1] == a[j,1] & a[,5] == TRUE), 4]
      a[j,21] <- round(mean(tp7[1:x]),2)
      a[j,22] <- max(tp7[1:x])
      a[j,23] <- min(tp7[1:x])
      for(ind in 24:26){
        a[j,ind] <- NA
      }
      a[j,27] <- round((a[j,21]*a[j,11]+a[j,24]*(a[j,10]-a[j,11]))/a[j,10],2)
      for(ind in 28:39){
        a[j,ind] <- NA
      }
      if((a[j,11]) > 3){
        a[j,40] <- round(mean(tail(tp6[1:x],3)),2)
        a[j,41] <- max(tail(tp6[1:x],3))
        a[j,42] <- min(tail(tp6[1:x],3))
      }else{
        for(ind in 40:42){
          a[j,ind] <- NA
        }
      }
      if((a[j,11]) > 5){
        a[j,43] <- round(mean(tail(tp6[1:x],5)),2)
        a[j,44] <- max(tail(tp6[1:x],5))
        a[j,45] <- min(tail(tp6[1:x],5))
      }else{
        for(ind in 43:45){
          a[j,ind] <- NA
        }
      }
      if((a[j,11]) > 7){
        a[j,46] <- round(mean(tail(tp6[1:x],7)),2)
        a[j,47] <- max(tail(tp6[1:x],7))
        a[j,48] <- min(tail(tp6[1:x],7))
      }else{
        for(ind in 46:48){
          a[j,ind] <- NA
        }
      }
      if((a[j,11]) > 10){
        a[j,49] <- round(mean(tail(tp6[1:x],10)),2)
        a[j,50] <- max(tail(tp6[1:x],10))
        a[j,51] <- min(tail(tp6[1:x],10))
      }else{
        for(ind in 49:51){
          a[j,ind] <- NA
        }
      }
      for(ind in 52:75){
        a[j,ind] <- NA
      }
      if((a[j,11]) > 3){
        a[j,76] <- round(mean(tail(tp7[1:x],3)),2)
        a[j,77] <- max(tail(tp7[1:x],3))
        a[j,78] <- min(tail(tp7[1:x],3))
      }else{
        for(ind in 76:78){
          a[j,ind] <- NA
        }
      }
      if((a[j,11]) > 5){
        a[j,79] <- round(mean(tail(tp7[1:x],5)),2)
        a[j,80] <- max(tail(tp7[1:x],5))
        a[j,81] <- min(tail(tp7[1:x],5))
      }else{
        for(ind in 79:81){
          a[j,ind] <- NA
        }
      }
      if((a[j,11]) > 7){
        a[j,82] <- round(mean(tail(tp7[1:x],7)),2)
        a[j,83] <- max(tail(tp7[1:x],7))
        a[j,84] <- min(tail(tp7[1:x],7))
      }else{
        for(ind in 82:84){
          a[j,ind] <- NA
        }
      }
      if((a[j,11]) > 10){
        a[j,85] <- round(mean(tail(tp7[1:x],10)),2)
        a[j,86] <- max(tail(tp7[1:x],10))
        a[j,87] <- min(tail(tp7[1:x],10))
      }else{
        for(ind in 85:87){
          a[j,ind] <- NA
        }
      }
      for(ind in 88:99){
        a[j,ind] <- NA
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
      #as de ultimos x jogos ainda sao NA quando o time jogou x ou menos jogos
      if((a[j,10]-a[j,11]) > 3){
        a[j,28] <- round(mean(tail(tp7[1:y],3)),2)
        a[j,29] <- max(tail(tp7[1:y],3))
        a[j,30] <- min(tail(tp7[1:y],3))
      }else{
        for(ind in 28:30){
          a[j,ind] <- NA
        }
      }
      if((a[j,10]-a[j,11]) > 5){
        a[j,31] <- round(mean(tail(tp7[1:y],5)),2)
        a[j,32] <- max(tail(tp7[1:y],5))
        a[j,33] <- min(tail(tp7[1:y],5))
      }else{
        for(ind in 31:33){
          a[j,ind] <- NA
        }
      }
      if((a[j,10]-a[j,11]) > 7){
        a[j,34] <- round(mean(tail(tp7[1:y],7)),2)
        a[j,35] <- max(tail(tp7[1:y],7))
        a[j,36] <- min(tail(tp7[1:y],7))
      }else{
        for(ind in 34:36){
          a[j,ind] <- NA
        }
      }
      if((a[j,10]-a[j,11]) > 10){
        a[j,37] <- round(mean(tail(tp7[1:y],10)),2)
        a[j,38] <- max(tail(tp7[1:y],10))
        a[j,39] <- min(tail(tp7[1:y],10))
      }else{
        for(ind in 37:39){
          a[j,ind] <- NA
        }
      }
      if((a[j,11]) > 3){
        a[j,40] <- round(mean(tail(tp6[1:x],3)),2)
        a[j,41] <- max(tail(tp6[1:x],3))
        a[j,42] <- min(tail(tp6[1:x],3))
      }else{
        for(ind in 40:42){
          a[j,ind] <- NA
        }
      }
      if((a[j,11]) > 5){
        a[j,43] <- round(mean(tail(tp6[1:x],5)),2)
        a[j,44] <- max(tail(tp6[1:x],5))
        a[j,45] <- min(tail(tp6[1:x],5))
      }else{
        for(ind in 43:45){
          a[j,ind] <- NA
        }
      }
      if((a[j,11]) > 7){
        a[j,46] <- round(mean(tail(tp6[1:x],7)),2)
        a[j,47] <- max(tail(tp6[1:x],7))
        a[j,48] <- min(tail(tp6[1:x],7))
      }else{
        for(ind in 46:48){
          a[j,ind] <- NA
        }
      }
      if((a[j,11]) > 10){
        a[j,49] <- round(mean(tail(tp6[1:x],10)),2)
        a[j,50] <- max(tail(tp6[1:x],10))
        a[j,51] <- min(tail(tp6[1:x],10))
      }else{
        for(ind in 49:51){
          a[j,ind] <- NA
        }
      }
      if((a[j,10]) > 3){
        a[j,52] <- round(mean(tail(tp10[1:z],3)),2)
        a[j,53] <- max(tail(tp10[1:z],3))
        a[j,54] <- min(tail(tp10[1:z],3))
      }else{
        for(ind in 52:54){
          a[j,ind] <- NA
        }
      }
      if((a[j,10]) > 5){
        a[j,55] <- round(mean(tail(tp10[1:z],5)),2)
        a[j,56] <- max(tail(tp10[1:z],5))
        a[j,57] <- min(tail(tp10[1:z],5))
      }else{
        for(ind in 55:57){
          a[j,ind] <- NA
        }
      }
      if((a[j,10]) > 7){
        a[j,58] <- round(mean(tail(tp10[1:z],7)),2)
        a[j,59] <- max(tail(tp10[1:z],7))
        a[j,60] <- min(tail(tp10[1:z],7))
      }else{
        for(ind in 58:60){
          a[j,ind] <- NA
        }
      }
      if((a[j,10]) > 10){
        a[j,61] <- round(mean(tail(tp10[1:z],10)),2)
        a[j,62] <- max(tail(tp10[1:z],10))
        a[j,63] <- min(tail(tp10[1:z],10))
      }else{
        for(ind in 61:63){
          a[j,ind] <- NA
        }
      }
      if((a[j,10]-a[j,11]) > 3){
        a[j,64] <- round(mean(tail(tp9[1:y],3)),2)
        a[j,65] <- max(tail(tp9[1:y],3))
        a[j,66] <- min(tail(tp9[1:y],3))
      }else{
        for(ind in 64:66){
          a[j,ind] <- NA
        }
      }
      if((a[j,10]-a[j,11]) > 5){
        a[j,67] <- round(mean(tail(tp9[1:y],5)),2)
        a[j,68] <- max(tail(tp9[1:y],5))
        a[j,69] <- min(tail(tp9[1:y],5))
      }else{
        for(ind in 67:69){
          a[j,ind] <- NA
        }
      }
      if((a[j,10]-a[j,11]) > 7){
        a[j,70] <- round(mean(tail(tp9[1:y],7)),2)
        a[j,71] <- max(tail(tp9[1:y],7))
        a[j,72] <- min(tail(tp9[1:y],7))
      }else{
        for(ind in 70:72){
          a[j,ind] <- NA
        }
      }
      if((a[j,10]-a[j,11]) > 10){
        a[j,73] <- round(mean(tail(tp9[1:y],10)),2)
        a[j,74] <- max(tail(tp9[1:y],10))
        a[j,75] <- min(tail(tp9[1:y],10))
      }else{
        for(ind in 73:75){
          a[j,ind] <- NA
        }
      }
      if((a[j,11]) > 3){
        a[j,76] <- round(mean(tail(tp8[1:x],3)),2)
        a[j,77] <- max(tail(tp8[1:x],3))
        a[j,78] <- min(tail(tp8[1:x],3))
      }else{
        for(ind in 76:78){
          a[j,ind] <- NA
        }
      }
      if((a[j,11]) > 5){
        a[j,79] <- round(mean(tail(tp8[1:x],5)),2)
        a[j,80] <- max(tail(tp8[1:x],5))
        a[j,81] <- min(tail(tp8[1:x],5))
      }else{
        for(ind in 79:81){
          a[j,ind] <- NA
        }
      }
      if((a[j,11]) > 7){
        a[j,82] <- round(mean(tail(tp8[1:x],7)),2)
        a[j,83] <- max(tail(tp8[1:x],7))
        a[j,84] <- min(tail(tp8[1:x],7))
      }else{
        for(ind in 82:84){
          a[j,ind] <- NA
        }
      }
      if((a[j,11]) > 10){
        a[j,85] <- round(mean(tail(tp8[1:x],10)),2)
        a[j,86] <- max(tail(tp8[1:x],10))
        a[j,87] <- min(tail(tp8[1:x],10))
      }else{
        for(ind in 85:87){
          a[j,ind] <- NA
        }
      }
      if((a[j,10]) > 3){
        a[j,88] <- round(mean(tail(tp11[1:z],3)),2)
        a[j,89] <- max(tail(tp11[1:z],3))
        a[j,90] <- min(tail(tp11[1:z],3))
      }else{
        for(ind in 88:90){
          a[j,ind] <- NA
        }
      }
      if((a[j,10]) > 5){
        a[j,91] <- round(mean(tail(tp11[1:z],5)),2)
        a[j,92] <- max(tail(tp11[1:z],5))
        a[j,93] <- min(tail(tp11[1:z],5))
      }else{
        for(ind in 91:93){
          a[j,ind] <- NA
        }
      }
      if((a[j,10]) > 7){
        a[j,94] <- round(mean(tail(tp11[1:z],7)),2)
        a[j,95] <- max(tail(tp11[1:z],7))
        a[j,96] <- min(tail(tp11[1:z],7))
      }else{
        for(ind in 94:96){
          a[j,ind] <- NA
        }
      }
      if((a[j,10]) > 10){
        a[j,97] <- round(mean(tail(tp11[1:z],10)),2)
        a[j,98] <- max(tail(tp11[1:z],10))
        a[j,99] <- min(tail(tp11[1:z],10))
      }else{
        for(ind in 97:99){
          a[j,ind] <- NA
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
      a[j,100] <- NA
    }

    #calculo do numero de vitorias nos ultimos x jogos casa/fora/total
    #tambem vale NA se o time ainda so teve x jogos ou menos
    x <- a[j,10] - a[j,11]
    tp12 <- a[(a[,1] == a[j,1] & a[,5] == FALSE), 8]
    if((a[j,10]-a[j,11]) > 3){
      a[j,101] <- sum(tail(tp12[1:x],3))
    }else{
      a[j,101] <- NA
    }
    if((a[j,10]-a[j,11]) > 5){
      a[j,102] <- sum(tail(tp12[1:x],5))
    }else{
      a[j,102] <- NA
    }
    if((a[j,10]-a[j,11]) > 7){
      a[j,103] <- sum(tail(tp12[1:x],7))
    }else{
      a[j,103] <- NA
    }
    if((a[j,10]-a[j,11]) > 10){
      a[j,104] <- sum(tail(tp12[1:x],10))
    }else{
      a[j,104] <- NA
    }

    y <- a[j,11]
    tp13 <- a[(a[,1] == a[j,1] & a[,5] == TRUE), 8]
    if((a[j,11]) > 3){
      a[j,105] <- sum(tail(tp13[1:y],3))
    }else{
      a[j,105] <- NA
    }
    if((a[j,11]) > 5){
      a[j,106] <- sum(tail(tp13[1:y],5))
    }else{
      a[j,106] <- NA
    }
    if((a[j,11]) > 7){
      a[j,107] <- sum(tail(tp13[1:y],7))
    }else{
      a[j,107] <- NA
    }
    if((a[j,11]) > 10){
      a[j,108] <- sum(tail(tp13[1:y],10))
    }else{
      a[j,108] <- NA
    }

    z <- a[j,10]
    tp14 <- a[(a[,1] == a[j,1]), 8]
    if((a[j,10]) > 3){
      a[j,109] <- sum(tail(tp14[1:z],3))
    }else{
      a[j,109] <- NA
    }
    if((a[j,10]) > 5){
      a[j,110] <- sum(tail(tp14[1:z],5))
    }else{
      a[j,110] <- NA
    }
    if((a[j,10]) > 7){
      a[j,111] <- sum(tail(tp14[1:z],7))
    }else{
      a[j,111] <- NA
    }
    if((a[j,10]) > 10){
      a[j,112] <- sum(tail(tp14[1:z],10))
    }else{
      a[j,112] <- NA
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
    if(a[j,10]>0){
      a[j,117] <- TRUE
    }else{
      a[j,117] <- NA
    }
    
    #streak: quantas vitorias/derrotas consecutivas do time
    tp20 <- a[(a[,1] == a[j,1]), 8]
    streak <- tp20[-length(tp20)]
    if(length(streak)<1){
      a[j,118] <- NA
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
      a[j,119] <- NA
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
      a[j,120] <- NA
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
    
    #derrotas totais
    a[j,121] <- a[j,10]-a[j,12]
    #derrotas em casa
    a[j,122] <- a[j,11]-a[j,13]
    #derrotas fora
    a[j,123] <- a[j,115]-a[j,116]
    
    #explicitando a diferenca de pontos q sera a variavel resposta na regressao
    a[j,124] <- result

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
    if(is.na(result)){
      a[j,8] <- NA
    }else{
      if(result>0){
        a[j,8] <- FALSE
      }else if(result<0){
        a[j,8] <- TRUE
      }
    }
    if(i>1){
      temp <- oct_games[1:i,2] == a[j,1] | oct_games[1:i,4] == a[j,1]
      if(sum(temp) > 1){
        temp2 <- which(temp == TRUE)
        ind <- max(temp2[-length(temp2)])
        a[j,9] <- as.numeric(oct_games[i,1]-oct_games[ind,1])
      }else{
        a[j,9] <- NA
      }
    }else{
      a[j,9] <- NA
    }
    a[j,10] <- sum(oct_games[1:i,2] == a[j,1]) + sum(oct_games[1:i,4] == a[j,1]) - 1
    tp2 <- oct_games[1:i,4] == a[j,1]
    a[j,11] <- sum(tp2[-length(tp2)])
    tp4 <- a[(a[,1] == a[j,1]), 8]
    if(length(tp4)>1){
      a[j,12] <- sum(tp4[-length(tp4)])
    }else{
      a[j,12] <- NA
    }
    tp5 <- a[(a[j,1] == a[,1] & a[j,5] == a[,5]),8]
    if(length(tp5)>1){
      a[j,13] <- sum(tp5[-length(tp5)])
    }else{
      a[j,13] <- NA
    }
    if(a[j,10] == 0){
      for(ind in 14:99){
        a[j,ind] <- NA
      }
    }else if(a[j,11] == 0){
      for(ind in 14:16){
        a[j,ind] <- NA
      }
      x <- a[j,10] - a[j,11]
      tp6 <- a[(a[,1] == a[j,1] & a[,5] == FALSE), 3]
      a[j,17] <- round(mean(tp6[1:x]),2)
      a[j,18] <- max(tp6[1:x])
      a[j,19] <- min(tp6[1:x])
      a[j,20] <- round((a[j,14]*a[j,11]+a[j,17]*(a[j,10]-a[j,11]))/a[j,10],2)
      for(ind in 21:23){
        a[j,ind] <- NA
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
          a[j,ind] <- NA
        }
      }
      if((a[j,10]-a[j,11]) > 5){
        a[j,31] <- round(mean(tail(tp6[1:x],5)),2)
        a[j,32] <- max(tail(tp6[1:x],5))
        a[j,33] <- min(tail(tp6[1:x],5))
      }else{
        for(ind in 31:33){
          a[j,ind] <- NA
        }
      }
      if((a[j,10]-a[j,11]) > 7){
        a[j,34] <- round(mean(tail(tp6[1:x],7)),2)
        a[j,35] <- max(tail(tp6[1:x],7))
        a[j,36] <- min(tail(tp6[1:x],7))
      }else{
        for(ind in 34:36){
          a[j,ind] <- NA
        }
      }
      if((a[j,10]-a[j,11]) > 10){
        a[j,37] <- round(mean(tail(tp6[1:x],10)),2)
        a[j,38] <- max(tail(tp6[1:x],10))
        a[j,39] <- min(tail(tp6[1:x],10))
      }else{
        for(ind in 37:39){
          a[j,ind] <- NA
        }
      }
      for(ind in 40:63){
        a[j,ind] <- NA
      }
      if((a[j,10]-a[j,11]) > 3){
        a[j,64] <- round(mean(tail(tp7[1:x],3)),2)
        a[j,65] <- max(tail(tp7[1:x],3))
        a[j,66] <- min(tail(tp7[1:x],3))
      }else{
        for(ind in 64:66){
          a[j,ind] <- NA
        }
      }
      if((a[j,10]-a[j,11]) > 5){
        a[j,67] <- round(mean(tail(tp7[1:x],5)),2)
        a[j,68] <- max(tail(tp7[1:x],5))
        a[j,69] <- min(tail(tp7[1:x],5))
      }else{
        for(ind in 67:69){
          a[j,ind] <- NA
        }
      }
      if((a[j,10]-a[j,11]) > 7){
        a[j,70] <- round(mean(tail(tp7[1:x],7)),2)
        a[j,71] <- max(tail(tp7[1:x],7))
        a[j,72] <- min(tail(tp7[1:x],7))
      }else{
        for(ind in 70:72){
          a[j,ind] <- NA
        }
      }
      if((a[j,10]-a[j,11]) > 10){
        a[j,73] <- round(mean(tail(tp7[1:x],10)),2)
        a[j,74] <- max(tail(tp7[1:x],10))
        a[j,75] <- min(tail(tp7[1:x],10))
      }else{
        for(ind in 73:75){
          a[j,ind] <- NA
        }
      }
      for(ind in 76:99){
        a[j,ind] <- NA
      }
    }else if(a[j,10] == a[j,11]){
      x <- a[j,11]
      tp6 <- a[(a[,1] == a[j,1] & a[,5] == TRUE), 3]
      a[j,14] <- round(mean(tp6[1:x]),2)
      a[j,15] <- max(tp6[1:x])
      a[j,16] <- min(tp6[1:x])
      for(ind in 17:19){
        a[j,ind] <- NA
      }
      a[j,20] <- round((a[j,14]*a[j,11]+a[j,17]*(a[j,10]-a[j,11]))/a[j,10],2)
      tp7 <- a[(a[,1] == a[j,1] & a[,5] == TRUE), 4]
      a[j,21] <- round(mean(tp7[1:x]),2)
      a[j,22] <- max(tp7[1:x])
      a[j,23] <- min(tp7[1:x])
      for(ind in 24:26){
        a[j,ind] <- NA
      }
      a[j,27] <- round((a[j,21]*a[j,11]+a[j,24]*(a[j,10]-a[j,11]))/a[j,10],2)
      for(ind in 28:39){
        a[j,ind] <- NA
      }
      if((a[j,11]) > 3){
        a[j,40] <- round(mean(tail(tp6[1:x],3)),2)
        a[j,41] <- max(tail(tp6[1:x],3))
        a[j,42] <- min(tail(tp6[1:x],3))
      }else{
        for(ind in 40:42){
          a[j,ind] <- NA
        }
      }
      if((a[j,11]) > 5){
        a[j,43] <- round(mean(tail(tp6[1:x],5)),2)
        a[j,44] <- max(tail(tp6[1:x],5))
        a[j,45] <- min(tail(tp6[1:x],5))
      }else{
        for(ind in 43:45){
          a[j,ind] <- NA
        }
      }
      if((a[j,11]) > 7){
        a[j,46] <- round(mean(tail(tp6[1:x],7)),2)
        a[j,47] <- max(tail(tp6[1:x],7))
        a[j,48] <- min(tail(tp6[1:x],7))
      }else{
        for(ind in 46:48){
          a[j,ind] <- NA
        }
      }
      if((a[j,11]) > 10){
        a[j,49] <- round(mean(tail(tp6[1:x],10)),2)
        a[j,50] <- max(tail(tp6[1:x],10))
        a[j,51] <- min(tail(tp6[1:x],10))
      }else{
        for(ind in 49:51){
          a[j,ind] <- NA
        }
      }
      for(ind in 52:75){
        a[j,ind] <- NA
      }
      if((a[j,11]) > 3){
        a[j,76] <- round(mean(tail(tp7[1:x],3)),2)
        a[j,77] <- max(tail(tp7[1:x],3))
        a[j,78] <- min(tail(tp7[1:x],3))
      }else{
        for(ind in 76:78){
          a[j,ind] <- NA
        }
      }
      if((a[j,11]) > 5){
        a[j,79] <- round(mean(tail(tp7[1:x],5)),2)
        a[j,80] <- max(tail(tp7[1:x],5))
        a[j,81] <- min(tail(tp7[1:x],5))
      }else{
        for(ind in 79:81){
          a[j,ind] <- NA
        }
      }
      if((a[j,11]) > 7){
        a[j,82] <- round(mean(tail(tp7[1:x],7)),2)
        a[j,83] <- max(tail(tp7[1:x],7))
        a[j,84] <- min(tail(tp7[1:x],7))
      }else{
        for(ind in 82:84){
          a[j,ind] <- NA
        }
      }
      if((a[j,11]) > 10){
        a[j,85] <- round(mean(tail(tp7[1:x],10)),2)
        a[j,86] <- max(tail(tp7[1:x],10))
        a[j,87] <- min(tail(tp7[1:x],10))
      }else{
        for(ind in 85:87){
          a[j,ind] <- NA
        }
      }
      for(ind in 88:99){
        a[j,ind] <- NA
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
          a[j,ind] <- NA
        }
      }
      if((a[j,10]-a[j,11]) > 5){
        a[j,31] <- round(mean(tail(tp7[1:y],5)),2)
        a[j,32] <- max(tail(tp7[1:y],5))
        a[j,33] <- min(tail(tp7[1:y],5))
      }else{
        for(ind in 31:33){
          a[j,ind] <- NA
        }
      }
      if((a[j,10]-a[j,11]) > 7){
        a[j,34] <- round(mean(tail(tp7[1:y],7)),2)
        a[j,35] <- max(tail(tp7[1:y],7))
        a[j,36] <- min(tail(tp7[1:y],7))
      }else{
        for(ind in 34:36){
          a[j,ind] <- NA
        }
      }
      if((a[j,10]-a[j,11]) > 10){
        a[j,37] <- round(mean(tail(tp7[1:y],10)),2)
        a[j,38] <- max(tail(tp7[1:y],10))
        a[j,39] <- min(tail(tp7[1:y],10))
      }else{
        for(ind in 37:39){
          a[j,ind] <- NA
        }
      }
      if((a[j,11]) > 3){
        a[j,40] <- round(mean(tail(tp6[1:x],3)),2)
        a[j,41] <- max(tail(tp6[1:x],3))
        a[j,42] <- min(tail(tp6[1:x],3))
      }else{
        for(ind in 40:42){
          a[j,ind] <- NA
        }
      }
      if((a[j,11]) > 5){
        a[j,43] <- round(mean(tail(tp6[1:x],5)),2)
        a[j,44] <- max(tail(tp6[1:x],5))
        a[j,45] <- min(tail(tp6[1:x],5))
      }else{
        for(ind in 43:45){
          a[j,ind] <- NA
        }
      }
      if((a[j,11]) > 7){
        a[j,46] <- round(mean(tail(tp6[1:x],7)),2)
        a[j,47] <- max(tail(tp6[1:x],7))
        a[j,48] <- min(tail(tp6[1:x],7))
      }else{
        for(ind in 46:48){
          a[j,ind] <- NA
        }
      }
      if((a[j,11]) > 10){
        a[j,49] <- round(mean(tail(tp6[1:x],10)),2)
        a[j,50] <- max(tail(tp6[1:x],10))
        a[j,51] <- min(tail(tp6[1:x],10))
      }else{
        for(ind in 49:51){
          a[j,ind] <- NA
        }
      }
      if((a[j,10]) > 3){
        a[j,52] <- round(mean(tail(tp10[1:z],3)),2)
        a[j,53] <- max(tail(tp10[1:z],3))
        a[j,54] <- min(tail(tp10[1:z],3))
      }else{
        for(ind in 52:54){
          a[j,ind] <- NA
        }
      }
      if((a[j,10]) > 5){
        a[j,55] <- round(mean(tail(tp10[1:z],5)),2)
        a[j,56] <- max(tail(tp10[1:z],5))
        a[j,57] <- min(tail(tp10[1:z],5))
      }else{
        for(ind in 55:57){
          a[j,ind] <- NA
        }
      }
      if((a[j,10]) > 7){
        a[j,58] <- round(mean(tail(tp10[1:z],7)),2)
        a[j,59] <- max(tail(tp10[1:z],7))
        a[j,60] <- min(tail(tp10[1:z],7))
      }else{
        for(ind in 58:60){
          a[j,ind] <- NA
        }
      }
      if((a[j,10]) > 10){
        a[j,61] <- round(mean(tail(tp10[1:z],10)),2)
        a[j,62] <- max(tail(tp10[1:z],10))
        a[j,63] <- min(tail(tp10[1:z],10))
      }else{
        for(ind in 61:63){
          a[j,ind] <- NA
        }
      }
      if((a[j,10]-a[j,11]) > 3){
        a[j,64] <- round(mean(tail(tp9[1:y],3)),2)
        a[j,65] <- max(tail(tp9[1:y],3))
        a[j,66] <- min(tail(tp9[1:y],3))
      }else{
        for(ind in 64:66){
          a[j,ind] <- NA
        }
      }
      if((a[j,10]-a[j,11]) > 5){
        a[j,67] <- round(mean(tail(tp9[1:y],5)),2)
        a[j,68] <- max(tail(tp9[1:y],5))
        a[j,69] <- min(tail(tp9[1:y],5))
      }else{
        for(ind in 67:69){
          a[j,ind] <- NA
        }
      }
      if((a[j,10]-a[j,11]) > 7){
        a[j,70] <- round(mean(tail(tp9[1:y],7)),2)
        a[j,71] <- max(tail(tp9[1:y],7))
        a[j,72] <- min(tail(tp9[1:y],7))
      }else{
        for(ind in 70:72){
          a[j,ind] <- NA
        }
      }
      if((a[j,10]-a[j,11]) > 10){
        a[j,73] <- round(mean(tail(tp9[1:y],10)),2)
        a[j,74] <- max(tail(tp9[1:y],10))
        a[j,75] <- min(tail(tp9[1:y],10))
      }else{
        for(ind in 73:75){
          a[j,ind] <- NA
        }
      }
      if((a[j,11]) > 3){
        a[j,76] <- round(mean(tail(tp8[1:x],3)),2)
        a[j,77] <- max(tail(tp8[1:x],3))
        a[j,78] <- min(tail(tp8[1:x],3))
      }else{
        for(ind in 76:78){
          a[j,ind] <- NA
        }
      }
      if((a[j,11]) > 5){
        a[j,79] <- round(mean(tail(tp8[1:x],5)),2)
        a[j,80] <- max(tail(tp8[1:x],5))
        a[j,81] <- min(tail(tp8[1:x],5))
      }else{
        for(ind in 79:81){
          a[j,ind] <- NA
        }
      }
      if((a[j,11]) > 7){
        a[j,82] <- round(mean(tail(tp8[1:x],7)),2)
        a[j,83] <- max(tail(tp8[1:x],7))
        a[j,84] <- min(tail(tp8[1:x],7))
      }else{
        for(ind in 82:84){
          a[j,ind] <- NA
        }
      }
      if((a[j,11]) > 10){
        a[j,85] <- round(mean(tail(tp8[1:x],10)),2)
        a[j,86] <- max(tail(tp8[1:x],10))
        a[j,87] <- min(tail(tp8[1:x],10))
      }else{
        for(ind in 85:87){
          a[j,ind] <- NA
        }
      }
      if((a[j,10]) > 3){
        a[j,88] <- round(mean(tail(tp11[1:z],3)),2)
        a[j,89] <- max(tail(tp11[1:z],3))
        a[j,90] <- min(tail(tp11[1:z],3))
      }else{
        for(ind in 88:90){
          a[j,ind] <- NA
        }
      }
      if((a[j,10]) > 5){
        a[j,91] <- round(mean(tail(tp11[1:z],5)),2)
        a[j,92] <- max(tail(tp11[1:z],5))
        a[j,93] <- min(tail(tp11[1:z],5))
      }else{
        for(ind in 91:93){
          a[j,ind] <- NA
        }
      }
      if((a[j,10]) > 7){
        a[j,94] <- round(mean(tail(tp11[1:z],7)),2)
        a[j,95] <- max(tail(tp11[1:z],7))
        a[j,96] <- min(tail(tp11[1:z],7))
      }else{
        for(ind in 94:96){
          a[j,ind] <- NA
        }
      }
      if((a[j,10]) > 10){
        a[j,97] <- round(mean(tail(tp11[1:z],10)),2)
        a[j,98] <- max(tail(tp11[1:z],10))
        a[j,99] <- min(tail(tp11[1:z],10))
      }else{
        for(ind in 97:99){
          a[j,ind] <- NA
        }
      }
    }
    tp1 <- a[(a[,1] == a[j,1]), 7]
    if(length(tp1) > 1){
      tp2 <- tail(tp1[-length(tp1)], 1)
      a[j,100] <- tp2
    }else{
      a[j,100] <- NA
    }

    x <- a[j,10] - a[j,11]
    tp12 <- a[(a[,1] == a[j,1] & a[,5] == FALSE), 8]
    if((a[j,10]-a[j,11]) > 3){
      a[j,101] <- sum(tail(tp12[1:x],3))
    }else{
      a[j,101] <- NA
    }
    if((a[j,10]-a[j,11]) > 5){
      a[j,102] <- sum(tail(tp12[1:x],5))
    }else{
      a[j,102] <- NA
    }
    if((a[j,10]-a[j,11]) > 7){
      a[j,103] <- sum(tail(tp12[1:x],7))
    }else{
      a[j,103] <- NA
    }
    if((a[j,10]-a[j,11]) > 10){
      a[j,104] <- sum(tail(tp12[1:x],10))
    }else{
      a[j,104] <- NA
    }

    y <- a[j,11]
    tp13 <- a[(a[,1] == a[j,1] & a[,5] == TRUE), 8]
    if((a[j,11]) > 3){
      a[j,105] <- sum(tail(tp13[1:y],3))
    }else{
      a[j,105] <- NA
    }
    if((a[j,11]) > 5){
      a[j,106] <- sum(tail(tp13[1:y],5))
    }else{
      a[j,106] <- NA
    }
    if((a[j,11]) > 7){
      a[j,107] <- sum(tail(tp13[1:y],7))
    }else{
      a[j,107] <- NA
    }
    if((a[j,11]) > 10){
      a[j,108] <- sum(tail(tp13[1:y],10))
    }else{
      a[j,108] <- NA
    }

    z <- a[j,10]
    tp14 <- a[(a[,1] == a[j,1]), 8]
    if((a[j,10]) > 3){
      a[j,109] <- sum(tail(tp14[1:z],3))
    }else{
      a[j,109] <- NA
    }
    if((a[j,10]) > 5){
      a[j,110] <- sum(tail(tp14[1:z],5))
    }else{
      a[j,110] <- NA
    }
    if((a[j,10]) > 7){
      a[j,111] <- sum(tail(tp14[1:z],7))
    }else{
      a[j,111] <- NA
    }
    if((a[j,10]) > 10){
      a[j,112] <- sum(tail(tp14[1:z],10))
    }else{
      a[j,112] <- NA
    }
    a[j,113] <- dia_semana[i]

    #calculo da media de publico
    tp15 <- a[(a[,1] == a[j,1] & a[,5] == TRUE), 6]
    x <- a[j,11]
    if(x>0){
      a[j,114] <- mean(tp15[1:x])
    }else{
      a[j,114] <- NA
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
      a[j,117] <- NA
    }
    
    tp20 <- a[(a[,1] == a[j,1]), 8]
    streak <- tp20[-length(tp20)]
    if(length(streak)<1){
      a[j,118] <- NA
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
      a[j,119] <- NA
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
      a[j,120] <- NA
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
    
    #derrotas totais
    a[j,121] <- a[j,10]-a[j,12]
    #derrotas em casa
    a[j,122] <- a[j,11]-a[j,13]
    #derrotas fora
    a[j,123] <- a[j,115]-a[j,116]
    
    a[j,124] <- -result

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
                "Travel","Streak_T","Streak_H","Streak_A","Loss_T","Loss_H","Loss_A","result")


  #calculo do strength of schedule
  a[1,125] <- NA
  a[2,125] <- NA
  for(i in 3:length(a$Team)){
    p <- a[1:(i-1),]$Team == a[i,1]
    if(sum(p) > 1){
      c <- which(p)
      w <- 0
      g <- 0
      lin <- ceiling(i/2)
      dat <- oct_games[lin,1]
      ate <- min(which(oct_games[,1] == dat))-1
      if(ate == Inf){
        a[i,125] <- NA
      }else{
        for(j in 1:length(c)){
          y <- a[c[j], 2]
          z <- a[1:(ate*2),]$Team == y
          if(sum(z) > 0){
            d <- which(z)
            e <- max(d)
            w <- w+a[e,12]+(a[e,124]>0)
            g <- g+a[e,10]+1
          }
        }
        if(g > 0){
          a[i,125] <- w/g
        }else{
          a[i,125] <- NA
        }
      }
    }else{
      a[i,125] <- NA
    }
  }
  names(a)[125] <- "Str_Sch"

  return(a)
}

# salva as bases
base2001 <- base_var(jogos_2001)
base2002 <- base_var(jogos_2002)
base2003 <- base_var(jogos_2003)
base2004 <- base_var(jogos_2004)
base2005 <- base_var(jogos_2005)
base2006 <- base_var(jogos_2006)
base2007 <- base_var(jogos_2007)
base2008 <- base_var(jogos_2008)
base2009 <- base_var(jogos_2009)
base2010 <- base_var(jogos_2010)
base2011 <- base_var(jogos_2011)
base2012 <- base_var(jogos_2012)
base2013 <- base_var(jogos_2013)
base2014 <- base_var(jogos_2014)
base2015 <- base_var(jogos_2015)
base2016 <- base_var(jogos_2016)
base2017 <- base_var(jogos_2017)
base2018 <- base_var(jogos_2018)
base2019 <- base_var(jogos_2019)

save(base2001,base2002,base2003,base2004,base2005,base2006,base2007,base2008,
     base2009,base2010,base2011,base2012,base2013,base2014,
     base2015,base2016,base2017,base2018,base2019, file="bases.rda")


################# bases finais
load(file="bases.rda")

junto <- function(a){
  b <- data.frame()
  a[,113] <- as.factor(a[,113])
  a <- a[,c(1:8,124,9:11,115,12:13,116,121:123,118:120,100,113,114,117,125,14:99,101:112)]
  for(i in seq(1,length(a[,1]),by=2)){
    d <- cbind(a[i,],a[i+1,])
    b <- rbind(b,d)
  }
  return(b)
}

final_base <- function(base){
  dados <- junto(base)
  dados <- dados[,-c(1:7,11:13,24:25,126:134,136:138)]
  return(dados)
}

final2001 <- final_base(base2001)
final2002 <- final_base(base2002)
final2003 <- final_base(base2003)
final2004 <- final_base(base2004)
final2005 <- final_base(base2005)
final2006 <- final_base(base2006)
final2007 <- final_base(base2007)
final2008 <- final_base(base2008)
final2009 <- final_base(base2009)
final2010 <- final_base(base2010)
final2011 <- final_base(base2011)
final2012 <- final_base(base2012)
final2013 <- final_base(base2013)
final2014 <- final_base(base2014)
final2015 <- final_base(base2015)
final2016 <- final_base(base2016)
final2017 <- final_base(base2017)
final2018 <- final_base(base2018)
final2019 <- final_base(base2019)

save(final2001,final2002,final2003,final2004,final2005,final2006,final2007,
     final2008,final2009,final2010,final2011,final2012,
     final2013,final2014,final2015,final2016,final2017,
     final2018,final2019, file="final.rda")

######## testes - arrumando algumas coisas na base
###tirar a coluna de win p/ reg linear
setwd("~/tcc")
load(file="final.rda")

######testando "peso"
final2001$temporadas <- 0
final2002$temporadas <- 1
final2003$temporadas <- 2
final2004$temporadas <- 3
final2005$temporadas <- 4
final2006$temporadas <- 5
final2007$temporadas <- 6
final2008$temporadas <- 7
final2009$temporadas <- 8
final2010$temporadas <- 9
final2011$temporadas <- 10
final2012$temporadas <- 11
final2013$temporadas <- 12
final2014$temporadas <- 13
final2015$temporadas <- 14
final2016$temporadas <- 15
final2017$temporadas <- 16
final2018$temporadas <- 17
final2019$temporadas <- 18
#####

#base full
final <- rbind(final2001[1:1189,],final2002[1:1189,],final2003[1:1189,],final2004[1:1189,],
               final2005[1:1230,],final2006[1:1230,],final2007[1:1230,],final2008[1:1230,],
               final2009[1:1230,],final2010[1:1230,],final2011[1:1230,],final2012[1:990,],
               final2013[1:1230,],final2014[1:1230,],final2015[1:1230,],final2016[1:1230,],
               final2017[1:1230,],final2018[1:1230,])
#1 win
#2 result

#tirando variaveis de casa pro time de fora e de fora pro time de casa
final <- final[,-c(5,8,11,14,16:18,23:25,42:53,78:89,106:109,117,120,123,132:134,139:141,143:154,179:190,215:218)]

#encontrando os padroes
teste19 <- final2019[1:1000,-c(5,8,11,14,16:18,23:25,42:53,78:89,106:109,117,120,123,132:134,139:141,143:154,179:190,215:218)]
padrao19 <- is.na(teste19[,-c(1,2)])

for(i in 1:length(teste19$Win)){
  for(j in 1:149){
    padrao19[i,j] <- as.numeric(padrao19[i,j])
  }
}

oi19 <- apply(padrao19,1,paste,collapse="")
nomes <- names(summary(as.factor(oi19)))

full19 <- oi19=="00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
td19 <- teste19[full19,]
colun <- !is.na(td19[1,])
td19 <- td19[,colun]
td <- final[,colun]
td <- td[rowSums(is.na(td)) == 0,]

##predicoes para cada padrao
#reg linear
vet <- c()
for(i in 1:length(nomes)){
    full19 <- oi19==nomes[i]
    td19 <- teste19[full19,]
    colun <- !is.na(td19[1,])
    td19 <- td19[,colun]
    td <- final[,colun]
    td <- td[rowSums(is.na(td)) == 0,]
    mod <- lm(result~., data=td[,-1])
    win <- (predict(mod, newdata=td19[,-1]) > 0) == (td19$result > 0)
    vet <- c(vet,win)
}

#reg logistica
vet2 <- c()
for(i in 1:length(nomes)){
    library(dplyr)
    full19 <- oi19==nomes[i]
    td19 <- teste19[full19,]
    colun <- !is.na(td19[1,])
    td19 <- td19[,colun]
    td <- final[,colun]
    td <- td[rowSums(is.na(td)) == 0,]
    a <- glm(Win~., data = td[,-2], family=binomial(link = "logit"))
    library(dplyr)
    probabilities <- a %>% predict(td19[,-2], type = "response")
    predicted.classes <- ifelse(probabilities > 0.5, "TRUE", "FALSE")
    win <- predicted.classes == td19$Win
    vet2 <- c(vet2,win)
}

#svm - demora umas 8h
vet3 <- c()
for(i in 1:length(nomes)){
    library(e1071)
    full19 <- oi19==nomes[i]
    td19 <- teste19[full19,]
    colun <- !is.na(td19[1,])
    td19 <- td19[,colun]
    td <- final[,colun]
    td <- td[rowSums(is.na(td)) == 0,]
    mod <- svm(result~., data=td[,-1])
    win <- (predict(mod, newdata=td19[,-1]) > 0) == (td19$result > 0)
    vet3 <- c(vet3,win)
}

#randomforest - demora demais, nao chegou a completar
vet4 <- c()
for(i in 1:length(nomes)){
  library(randomForest)
  full19 <- oi19==nomes[i]
  td19 <- teste19[full19,]
  colun <- !is.na(td19[1,])
  td19 <- td19[,colun]
  td <- final[,colun]
  td <- td[rowSums(is.na(td)) == 0,]
  mod <- randomForest(result~., data=td[,-1])
  win <- (predict(mod, newdata=td19[,-1]) > 0) == (td19$result > 0)
  vet4 <- c(vet4,win)
}

mean(vet2)

#######REG LINEAR
mod <- lm(result~., data=td[,-1])
summary(mod)
win <- (predict(mod, newdata=td19[,-1]) > 0) == (td19$result > 0)
mean(win)

#selecao de variaveis
require(leaps)
#com o primeiro (com menos variaveis)
step(mod, direction='backward', scope=( ~ 1))
modmin <- lm(result ~ 1, data=td[,-1])
step(modmin, direction='forward', scope=(~ Days_LG + Wins_T + Wins_A + Loss_T + Loss_A + Streak_T + 
                                           Streak_A + OT_last + Str_Sch + Mean_Pts_S_A + Max_Pts_S_A + 
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
                                           Max_Last10_total_opp + Min_Last10total_opp + Win_Last3_away + 
                                           Win_Last5_away + Win_Last7_away + Win_Last10_away + Win_Last3_total + 
                                           Win_Last5_total + Win_Last7_total + Win_Last10_total + Days_LG.1 + 
                                           Wins_T.1 + Wins_H.1 + Loss_T.1 + Loss_H.1 + Streak_T.1 + 
                                           Streak_H.1 + OT_last.1 + weekday + mean_attend + Travel.1 + 
                                           Str_Sch.1 + Mean_Pts_S_H.1 + Max_Pts_S_H.1 + Min_Pts_S_H.1 + 
                                           Mean_Pts_S_T.1 + Mean_Pts_A_H.1 + Max_Pts_A_H.1 + Min_Pts_A_H.1 + 
                                           Mean_Pts_A_T.1 + Mean_Last3_home.1 + Max_Last3_home.1 + Min_Last3home.1 + 
                                           Mean_Last5_home.1 + Max_Last5_home.1 + Min_Last5home.1 + 
                                           Mean_Last7_home.1 + Max_Last7_home.1 + Min_Last7home.1 + 
                                           Mean_Last10_home.1 + Max_Last10_home.1 + Min_Last10home.1 + 
                                           Mean_Last3_total.1 + Max_Last3_total.1 + Min_Last3total.1 + 
                                           Mean_Last5_total.1 + Max_Last5_total.1 + Min_Last5total.1 + 
                                           Mean_Last7_total.1 + Max_Last7_total.1 + Min_Last7total.1 + 
                                           Mean_Last10_total.1 + Max_Last10_total.1 + Min_Last10total.1 + 
                                           Mean_Last3_home_opp.1 + Max_Last3_home_opp.1 + Min_Last3home_opp.1 + 
                                           Mean_Last5_home_opp.1 + Max_Last5_home_opp.1 + Min_Last5home_opp.1 + 
                                           Mean_Last7_home_opp.1 + Max_Last7_home_opp.1 + Min_Last7home_opp.1 + 
                                           Mean_Last10_home_opp.1 + Max_Last10_home_opp.1 + Min_Last10home_opp.1 + 
                                           Mean_Last3_total_opp.1 + Max_Last3_total_opp.1 + Min_Last3total_opp.1 + 
                                           Mean_Last5_total_opp.1 + Max_Last5_total_opp.1 + Min_Last5total_opp.1 + 
                                           Mean_Last7_total_opp.1 + Max_Last7_total_opp.1 + Min_Last7total_opp.1 + 
                                           Mean_Last10_total_opp.1 + Max_Last10_total_opp.1 + Min_Last10total_opp.1 + 
                                           Win_Last3_home.1 + Win_Last5_home.1 + Win_Last7_home.1 + 
                                           Win_Last10_home.1 + Win_Last3_total.1 + Win_Last5_total.1 + 
                                           Win_Last7_total.1 + Win_Last10_total.1))

modforw <- lm(formula = result ~ Days_LG + Wins_T + Streak_T + Mean_Pts_S_A + 
                Max_Pts_S_A + Mean_Pts_A_A + Max_Pts_A_A + Days_LG.1 + Wins_T.1 + 
                Loss_T.1 + Streak_T.1 + Str_Sch.1, data = td[, -1])
summary(modforw)
winfor <- (predict(modforw, newdata=td19[,-1]) > 0) == (td19$result > 0)
mean(winfor)

library(standardize)
pad <- standardize(formula = result ~ Win_Last10_home.1 + Win_Last10_total + 
                     Wins_T + Wins_T.1 + Mean_Last10_total.1 + Mean_Pts_A_T.1 + 
                     Mean_Pts_S_H.1 + Games_T + Min_Last3total + Min_Last3total_opp + 
                     Mean_Pts_S_T.1 + Games_H.1 + Str_Sch.1 + Streak_A + Max_Pts_A_A + 
                     Mean_Pts_S_T + Mean_Pts_A_T + Days_LG + Win_Last3_home.1 + 
                     Mean_Last5_total_opp.1 + Days_LG.1 + Min_Last10home_opp.1 + 
                     Str_Sch + Min_Last5away_opp + Min_Last10total_opp + Min_Last5home.1 + 
                     Min_Last3away, data = td[, -74])

novo <- pad$data

modnovo <- lm(td$result~Win_Last10_home.1 + Win_Last10_total + 
                Wins_T + Wins_T.1 + Mean_Last10_total.1 + Mean_Pts_A_T.1 + 
                Mean_Pts_S_H.1 + Games_T + Min_Last3total + Min_Last3total_opp + 
                Mean_Pts_S_T.1 + Games_H.1 + Str_Sch.1 + Streak_A + Max_Pts_A_A + 
                Mean_Pts_S_T + Mean_Pts_A_T + Days_LG + Win_Last3_home.1 + 
                Mean_Last5_total_opp.1 + Days_LG.1 + Min_Last10home_opp.1 + 
                Str_Sch + Min_Last5away_opp + Min_Last10total_opp + Min_Last5home.1 + 
                Min_Last3away, data = novo[, -1])

summary(modnovo)

step(mod, direction='backward', scope=( ~ 1))
modback <- lm(formula = result ~ Days_LG + Wins_T + Wins_A + Max_Pts_S_A + 
                Min_Pts_S_A + Mean_Pts_S_T + Mean_Pts_A_T + Max_Last3_away + 
                Min_Last5away + Mean_Last7_away + Max_Last7_away + Min_Last7away + 
                Max_Last10_away + Min_Last10away + Min_Last3total + Max_Last5_total + 
                Mean_Last10_total + Mean_Last3_away_opp + Max_Last3_away_opp + 
                Min_Last3away_opp + Max_Last5_away_opp + Mean_Last7_away_opp + 
                Mean_Last3_total_opp + Max_Last3_total_opp + Min_Last5total_opp + 
                Max_Last7_total_opp + Min_Last7total_opp + Mean_Last10_total_opp + 
                Win_Last7_away + Win_Last10_away + Win_Last10_total + Days_LG.1 + 
                Loss_T.1 + OT_last.1 + Mean_Pts_S_H.1 + Min_Pts_S_H.1 + Mean_Last3_home.1 + 
                Max_Last3_home.1 + Min_Last3home.1 + Max_Last5_home.1 + Mean_Last3_total.1 + 
                Max_Last3_total.1 + Max_Last7_total.1 + Min_Last7total.1 + 
                Max_Last10_total.1 + Max_Last3_home_opp.1 + Min_Last3home_opp.1 + 
                Mean_Last5_home_opp.1 + Min_Last5home_opp.1 + Mean_Last7_home_opp.1 + 
                Max_Last3_total_opp.1 + Mean_Last5_total_opp.1 + Max_Last5_total_opp.1 + 
                Mean_Last7_total_opp.1 + Max_Last7_total_opp.1 + Max_Last10_total_opp.1 + 
                Win_Last3_home.1 + Win_Last7_home.1 + Win_Last7_total.1, 
              data = td[, -1])
summary(modback)
winback <- (predict(modback, newdata=td18[,-1]) > 0) == (td18$result > 0)
mean(winback)


####REG LOGISTICA
a <- glm(Win~., data = td[,-2], family=binomial(link = "logit"))
summary(a)
library(dplyr)
probabilities <- a %>% predict(td19[,-2], type = "response")
predicted.classes <- ifelse(probabilities > 0.5, "TRUE", "FALSE")
mean(predicted.classes == td19$Win)

#selecao de variaveis
modmin2 <- glm(Win~1, data = td[,-2], family=binomial(link = "logit"))
step(modmin2, direction='forward', scope=(~Days_LG + Wins_T + Wins_A + Loss_T + Loss_A + Streak_T + 
                                            Streak_A + OT_last + Str_Sch + Mean_Pts_S_A + Max_Pts_S_A + 
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
                                            Max_Last10_total_opp + Min_Last10total_opp + Win_Last3_away + 
                                            Win_Last5_away + Win_Last7_away + Win_Last10_away + Win_Last3_total + 
                                            Win_Last5_total + Win_Last7_total + Win_Last10_total + Days_LG.1 + 
                                            Wins_T.1 + Wins_H.1 + Loss_T.1 + Loss_H.1 + Streak_T.1 + 
                                            Streak_H.1 + OT_last.1 + weekday + mean_attend + Travel.1 + 
                                            Str_Sch.1 + Mean_Pts_S_H.1 + Max_Pts_S_H.1 + Min_Pts_S_H.1 + 
                                            Mean_Pts_S_T.1 + Mean_Pts_A_H.1 + Max_Pts_A_H.1 + Min_Pts_A_H.1 + 
                                            Mean_Pts_A_T.1 + Mean_Last3_home.1 + Max_Last3_home.1 + Min_Last3home.1 + 
                                            Mean_Last5_home.1 + Max_Last5_home.1 + Min_Last5home.1 + 
                                            Mean_Last7_home.1 + Max_Last7_home.1 + Min_Last7home.1 + 
                                            Mean_Last10_home.1 + Max_Last10_home.1 + Min_Last10home.1 + 
                                            Mean_Last3_total.1 + Max_Last3_total.1 + Min_Last3total.1 + 
                                            Mean_Last5_total.1 + Max_Last5_total.1 + Min_Last5total.1 + 
                                            Mean_Last7_total.1 + Max_Last7_total.1 + Min_Last7total.1 + 
                                            Mean_Last10_total.1 + Max_Last10_total.1 + Min_Last10total.1 + 
                                            Mean_Last3_home_opp.1 + Max_Last3_home_opp.1 + Min_Last3home_opp.1 + 
                                            Mean_Last5_home_opp.1 + Max_Last5_home_opp.1 + Min_Last5home_opp.1 + 
                                            Mean_Last7_home_opp.1 + Max_Last7_home_opp.1 + Min_Last7home_opp.1 + 
                                            Mean_Last10_home_opp.1 + Max_Last10_home_opp.1 + Min_Last10home_opp.1 + 
                                            Mean_Last3_total_opp.1 + Max_Last3_total_opp.1 + Min_Last3total_opp.1 + 
                                            Mean_Last5_total_opp.1 + Max_Last5_total_opp.1 + Min_Last5total_opp.1 + 
                                            Mean_Last7_total_opp.1 + Max_Last7_total_opp.1 + Min_Last7total_opp.1 + 
                                            Mean_Last10_total_opp.1 + Max_Last10_total_opp.1 + Min_Last10total_opp.1 + 
                                            Win_Last3_home.1 + Win_Last5_home.1 + Win_Last7_home.1 + 
                                            Win_Last10_home.1 + Win_Last3_total.1 + Win_Last5_total.1 + 
                                            Win_Last7_total.1 + Win_Last10_total.1))

modforw2 <- glm(formula = Win ~ Win_Last10_home.1 + Win_Last10_total + Loss_T.1 + 
                  Loss_T + Min_Pts_A_H.1 + Min_Last5home.1 + Min_Last3total_opp.1 + 
                  Mean_Last7_total.1 + Mean_Pts_A_T.1 + Mean_Pts_S_T.1 + Wins_A + 
                  Mean_Pts_A_T + Mean_Pts_S_T + Wins_T + Win_Last5_home.1 + 
                  Win_Last3_total + Min_Last3total + Max_Last7_total_opp + 
                  Max_Pts_S_H.1 + Min_Last3home_opp.1 + Min_Last7home_opp.1 + 
                  Loss_H.1 + mean_attend + Mean_Last10_home_opp.1 + Max_Pts_S_A + 
                  OT_last.1, family = binomial(link = "logit"), data = td[, 
                                                                          -2])
summary(modforw2)
probabilities3 <- modforw2 %>% predict(td19[,-2], type = "response")
predicted.classes3 <- ifelse(probabilities3 > 0.5, "TRUE", "FALSE")
mean(predicted.classes3 == td19$Win)

#########SVM
library(e1071)
teste <- svm(result~., data=td[,-1])
summary(teste)
winsvm <- (predict(teste, newdata=td19[,-1]) > 0) == (td19$result > 0)
mean(winsvm)

teste1 <- svm(result ~ Win_Last10_home.1 + Win_Last10_total + 
                Wins_T.1 + Wins_T + Mean_Pts_S_T + Mean_Pts_A_T + Loss_T.1 + 
                Streak_T + Win_Last5_total.1 + Min_Pts_A_H.1 + Mean_Last10_home.1 + 
                Mean_Pts_A_T.1 + Mean_Pts_S_T.1 + Days_LG + Days_LG.1 + Travel.1 + 
                Mean_Last5_away_opp + Max_Last5_away + Mean_Last10_away_opp + 
                Max_Last10_away + Min_Last3total + Min_Last5total.1 + Mean_Last3_total_opp.1 + 
                Min_Last5home_opp.1 + Mean_Last7_home_opp.1 + Max_Last10_home_opp.1 + 
                OT_last + Max_Pts_S_A + Str_Sch
              , data = td[, -1])
summary(teste1)
win1 <- (predict(teste1, newdata=td19[,-1]) > 0) == (td19$result > 0)
mean(win1)

teste2 <- svm(result ~ Win_Last10_home.1 + Win_Last10_total + 
                Wins_A + Wins_T.1 + Wins_T + Days_LG + Mean_Last7_total.1 + 
                Mean_Pts_A_T.1 + Mean_Pts_S_T.1 + Loss_T + Str_Sch.1 + Days_LG.1 + 
                Max_Pts_A_A + Mean_Pts_S_T + Mean_Pts_A_T + Min_Last5total_opp.1 + 
                Min_Last3total + Mean_Last7_total_opp + Streak_T + Str_Sch + 
                Win_Last3_home.1 + Min_Last3home_opp.1 + Min_Last5away + 
                Loss_A + Loss_H.1 + Mean_Last7_home.1 + mean_attend + Mean_Last7_total_opp.1 + 
                Min_Last3total_opp + Max_Last10_away + Max_Last7_total, data = td[, -1])
summary(teste2)
win2 <- (predict(teste2, newdata=td18[,-1]) > 0) == (td18$result > 0)
mean(win2)


##tuning svm
pkgs <- c('foreach', 'doParallel')
lapply(pkgs, require, character.only = T)
registerDoParallel(cores = 4)
### PREPARE FOR THE DATA ###
df2 <- final
df2 <- df2[rowSums(is.na(df2)) == 0,]
x <- paste("Days_LG + Wins_T + Wins_A + Loss_T + Loss_A + Streak_T + 
           Streak_A + OT_last + Str_Sch + Mean_Pts_S_A + Max_Pts_S_A + 
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
           Max_Last10_total_opp + Min_Last10total_opp + Win_Last3_away + 
           Win_Last5_away + Win_Last7_away + Win_Last10_away + Win_Last3_total + 
           Win_Last5_total + Win_Last7_total + Win_Last10_total + Days_LG.1 + 
           Wins_T.1 + Wins_H.1 + Loss_T.1 + Loss_H.1 + Streak_T.1 + 
           Streak_H.1 + OT_last.1 + weekday + mean_attend + Travel.1 + 
           Str_Sch.1 + Mean_Pts_S_H.1 + Max_Pts_S_H.1 + Min_Pts_S_H.1 + 
           Mean_Pts_S_T.1 + Mean_Pts_A_H.1 + Max_Pts_A_H.1 + Min_Pts_A_H.1 + 
           Mean_Pts_A_T.1 + Mean_Last3_home.1 + Max_Last3_home.1 + Min_Last3home.1 + 
           Mean_Last5_home.1 + Max_Last5_home.1 + Min_Last5home.1 + 
           Mean_Last7_home.1 + Max_Last7_home.1 + Min_Last7home.1 + 
           Mean_Last10_home.1 + Max_Last10_home.1 + Min_Last10home.1 + 
           Mean_Last3_total.1 + Max_Last3_total.1 + Min_Last3total.1 + 
           Mean_Last5_total.1 + Max_Last5_total.1 + Min_Last5total.1 + 
           Mean_Last7_total.1 + Max_Last7_total.1 + Min_Last7total.1 + 
           Mean_Last10_total.1 + Max_Last10_total.1 + Min_Last10total.1 + 
           Mean_Last3_home_opp.1 + Max_Last3_home_opp.1 + Min_Last3home_opp.1 + 
           Mean_Last5_home_opp.1 + Max_Last5_home_opp.1 + Min_Last5home_opp.1 + 
           Mean_Last7_home_opp.1 + Max_Last7_home_opp.1 + Min_Last7home_opp.1 + 
           Mean_Last10_home_opp.1 + Max_Last10_home_opp.1 + Min_Last10home_opp.1 + 
           Mean_Last3_total_opp.1 + Max_Last3_total_opp.1 + Min_Last3total_opp.1 + 
           Mean_Last5_total_opp.1 + Max_Last5_total_opp.1 + Min_Last5total_opp.1 + 
           Mean_Last7_total_opp.1 + Max_Last7_total_opp.1 + Min_Last7total_opp.1 + 
           Mean_Last10_total_opp.1 + Max_Last10_total_opp.1 + Min_Last10total_opp.1 + 
           Win_Last3_home.1 + Win_Last5_home.1 + Win_Last7_home.1 + 
           Win_Last10_home.1 + Win_Last3_total.1 + Win_Last5_total.1 + 
           Win_Last7_total.1 + Win_Last10_total.1")
fml <- as.formula(paste("as.factor(Win) ~ ", x))
### SPLIT DATA INTO K FOLDS ###
set.seed(2016)
df2$fold <- caret::createFolds(1:nrow(df2), k = 4, list = FALSE)
### PARAMETER LIST ###
cost <- c(1, 5)
gamma <- c(10^-10, 10^-5)
parms <- expand.grid(cost = cost, gamma = gamma)
### LOOP THROUGH PARAMETER VALUES ###
result <- foreach(i = 1:nrow(parms), .combine = rbind) %do% {
  c <- parms[i, ]$cost
  g <- parms[i, ]$gamma
  ### K-FOLD VALIDATION ###
  out <- foreach(j = 1:max(df2$fold), .combine = rbind, .inorder = FALSE) %dopar% {
    deve <- df2[df2$fold != j, ]
    test <- df2[df2$fold == j, ]
    mdl <- e1071::svm(fml, data = deve, type = "C-classification", kernel = "radial", cost = c, gamma = g, probability = TRUE)
    pred <- predict(mdl, test, decision.values = TRUE, probability = TRUE)
    data.frame(y = test$Win, prob = attributes(pred)$probabilities[, 2])
  }
  ### CALCULATE SVM PERFORMANCE ###
  roc <- pROC::roc(as.factor(out$y), out$prob) 
  data.frame(parms[i, ], roc = roc$auc[1])
}

result

print(roc)


###########RANDOM FOREST
require(randomForest)
mod <- randomForest(result~., data=td[,-1])
mod
mean((predict(mod, newdata=td19[,-1]) > 0) == (td19$result > 0))

mod2 <- randomForest(result~Win_Last10_home.1 + Win_Last10_total + 
                       Wins_T.1 + Wins_T + Mean_Pts_S_T + Mean_Pts_A_T + Loss_T.1 + 
                       Streak_T + Win_Last5_total.1 + Min_Pts_A_H.1 + Mean_Last10_home.1 + 
                       Mean_Pts_A_T.1 + Mean_Pts_S_T.1 + Days_LG + Days_LG.1 + Travel.1 + 
                       Mean_Last5_away_opp + Max_Last5_away + Mean_Last10_away_opp + 
                       Max_Last10_away + Min_Last3total + Min_Last5total.1 + Mean_Last3_total_opp.1 + 
                       Min_Last5home_opp.1 + Mean_Last7_home_opp.1 + Max_Last10_home_opp.1 + 
                       OT_last + Max_Pts_S_A + Str_Sch, data=td[,-1])
mean((predict(mod2, newdata=td19[,-1]) > 0) == (td19$result > 0))

mod3 <- randomForest(result~Win_Last10_home.1 + Win_Last10_total + Wins_T.1 + 
                       Wins_T + Max_Last7_total_opp + Min_Last3total + Min_Last5total_opp.1 + 
                       Mean_Last10_total.1 + Mean_Pts_A_T.1 + Mean_Pts_S_H.1 + Loss_T + 
                       Mean_Pts_A_T + Mean_Pts_S_T + Days_LG + Win_Last3_total.1 + 
                       Streak_T + Min_Last3away + Mean_Pts_S_T.1 + Min_Last10total_opp.1 + 
                       Mean_Last5_total.1 + Mean_Last3_total_opp.1 + Days_LG.1 + 
                       Str_Sch + Loss_H.1 + Wins_H.1 + Mean_Last3_total.1 + Win_Last7_total, data=td[,-1])
mean((predict(mod3, newdata=td18[,-1]) > 0) == (td18$result > 0))

####################TESTES - MUITO DEMORADO
####melhores modelos com x variaveis
library(leaps)
modelos<-regsubsets(result ~ .,data=td[,-72],nbest=1,nvmax=20,really.big=T)
summary(leaps)

###selecao stepwise
library(MASS)
stepAIC(mod, direction = "both")
