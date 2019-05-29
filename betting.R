### carrega os arquivos
load(file="lasvegas.rda")
load(file="final.rda")

#porcentagem de acerto das casas de aposta \/
mean((prev > 0) == (final2019$result[1:1230] > 0))


############# como cheguei no las vegas

###web scraping das lines das casas de aposta
tabela <- data.frame()
ini <- 401070213
library(rvest)
for(i in 1:5000){
  tryCatch({
  a <- "http://www.espn.com/nba/game?gameId="
  url <- paste(a,ini,sep="")
  if(ini == 401070242){
    ini <- ini+239
  }
  download.file(url, destfile = "scrapedpage.html", quiet=TRUE)
  html <- read_html("scrapedpage.html")
  #nodes obtidos com o SelectorGadget, extensao do google chrome
  games <- html_nodes(html, ".short-name , #gamepackage-matchup-wrap .icon-font-before , #gamepackage-matchup-wrap .icon-font-after , .long-name , #gamepackage-game-information li")
  texto <- html_text(games)
  if(!is.na(texto[1])){
    tabela[i,1] <- paste(texto[1],texto[2])
    tabela[i,2] <- texto[3]
    tabela[i,3] <- texto[4]
    tabela[i,4] <- paste(texto[5],texto[6])
    tabela[i,5] <- texto[20]
  }
  ini <- ini+1
  if(sum(!is.na(tabela[,1])) == 1230){
    break
  }
  }, error=function(e){})
}

#arrumacao da base
lasvegas <- tabela[!is.na(tabela[,1]),]
colnames(lasvegas) <- c("Team", "PTS", "PTS_Opp", "Opponent", "Odds")
rownames(lasvegas) <- NULL
lasvegas$Team[which(lasvegas$Team == "LA Clippers")] <- "Los Angeles Clippers"
lasvegas$Opponent[which(lasvegas$Opponent == "LA Clippers")] <- "Los Angeles Clippers"
lasvegas[,6] <- 0
for(i in 1:1230){
  if(i != 152 & i != 717){
    lasvegas[i,6] <- which(lasvegas$Team[i] == jogos_2019$Visitor & lasvegas$Opponent[i] == jogos_2019$Home & 
      lasvegas$PTS[i] == jogos_2019$PTS_Visitor & lasvegas$PTS_Opp[i] == jogos_2019$PTS_Home)
  }
}
#jogos 152 e 717 foram entre os mesmos times e teve o mesmo placar, tive que ver manualmente qual era qual
lasvegas[152,6] <- 146
lasvegas[717,6] <- 717
colnames(lasvegas)[6] <- "Indice"
lasvegas <- lasvegas[order(lasvegas$Indice),]


#associando os times as siglas
library(sqldf)
siglas <-  sqldf("select DISTINCT Visitor as 'Team' from jogos_2019")
siglas[,2] <- c("PHI", "OKC", "MIL", "BKN", "NO", "MEM", "DEN", "ATL", "MIA", "DAL",
                "UTAH", "MIN", "CLE", "CHI", "LAL", "NY", "IND", "SAC", "CHA", "BOS",
                "GS", "DET", "PHX", "HOU", "ORL", "SA", "TOR", "WSH", "LAC", "POR")
colnames(siglas)[2] <- "Sigla"

library(stringr)
numextract <- function(string){ 
  str_extract(string, "\\-*\\d+\\.*\\d*")
}

#extraindo o numero da previsao das casas de aposta
prev <- c()
for(i in 1:1230){
  temp <- substring(lasvegas$Odds[i],7)
  time <- str_extract(temp, "[aA-zZ]+")
  ind <- which(lasvegas$Team[i] == siglas$Team)
  if(time == siglas[ind,2]){
    prev[i] <- abs(as.numeric(numextract(temp)))
  }else{
    prev[i] <- as.numeric(numextract(temp))
  }
}
prev[which(is.na(prev))] <- 0



#save(tabela, lasvegas, siglas, prev, file="lasvegas.rda")

overunder <- which(prev>200)
even <- which(prev == 0)
mean((prev[-c(overunder,even)] > 0) == (teste19$result_Vis[-c(overunder,even)] > 0))

