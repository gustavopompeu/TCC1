#1) funcoes.R (Não precisa rodar) : as funções usadas para pegar os dados dos jogos da internet, 
#e criar as bases de dados usadas no trabalho, e no final a extração das linhas das casas de aposta.

#2) previsoes.R : organiza os dados, checa os padrões dos NA, e faz as previsões para todos os métodos, 
#também o standardize, e o tune svm.

#3) betting.R : a porcentagem de acerto das previsões das casas de aposta.

#4) serie_log : faz as previsões mudando o numero de temporadas pra regressão logística,
#e os gráficos do modelo campeão.

#5) serie_linear : previsões mudando o numero de temporadas pra regressão linear, e
#compara os saldos previstos com os resultados reais e as linhas das casas de aposta.

#6) serie_lda, serie_probit, serie_class_arvore e serie_reg_arvore : faz as previsões mudando o numero 
#de temporadas na modelagem, para esses outros métodos que não são demorados.

#7) acertos_por_time.R : faz as tabelas de acertos das previsões por equipe para o modelo campeão.

#8) adicionando_2019.R : faz as previsões do modelo campeão adicionando os jogos da temporada 18/19 
#gradualmente na modelagem.