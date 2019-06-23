### carrega os arquivos
load(file="lasvegas.rda")
load(file="final.rda")

#prev sao as previsoes das casas de aposta

##4 jogos nao tem a line, veio o over/under
overunder <- which(prev>200)
##alguns jogos era line even
even <- which(prev == 0)

#tirando esses jogos
prev[c(overunder,even)] <- NaN

#porcentagem de acerto das casas de aposta
mean((prev > 0) == (teste19$result_Vis > 0), na.rm=T)

b2 <- c()
for(i in 1:length(prev)){
  wr <- (prev[1:i] > 0) == (teste19$result_Vis[1:i] > 0)
  b2[i] <- mean(wr, na.rm=T)
}

plot(b2, main="Porcentagem de Acerto das Previsões Durante a Temporada",
     xlab="Número do Jogo", ylab="% Acerto", type="l", ylim=c(0.55,1))
text(600,0.85,paste("% Acerto Final = ",round(b2[1230],3),sep=""))
text(600,.95,"Casas de Aposta",cex=1.5)

num_last2 <- 61
ult2 <- c()
ult2[1:(num_last2-1)] <- NaN
for(j in num_last2:1230){
  esp2 <- (prev_apostas[(j-num_last2+1):j] > 0) == (teste19$result_Vis[(j-num_last2+1):j] > 0)
  ult2[j] <- mean(esp2, na.rm=T)
}

plot(ult2, type="l", xlim=c(0,1230), main="Porcentagem de Acerto das Previsões nos Últimos 61 Jogos",
     xlab="Número do Jogo", ylab="% Acerto", ylim=c(0.45,0.9))
text(600,.88,"Casas de Aposta",cex=1.5)
abline(h=b2[1230], lty=2, col="blue")