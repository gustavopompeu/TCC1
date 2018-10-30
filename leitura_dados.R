library(rvest)

ler_jogos <- function(url){
  html <- read_html(url)
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
  months <- c("october","november","december","january","february","march","april","may","june")
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
  return(teste)
}

jogos_2013 <- tabela_temporada(2013)
jogos_2014 <- tabela_temporada(2014)
jogos_2015 <- tabela_temporada(2015)
jogos_2016 <- tabela_temporada(2016)
jogos_2017 <- tabela_temporada(2017)
jogos_2018 <- tabela_temporada(2018)