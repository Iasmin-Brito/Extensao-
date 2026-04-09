#leitura do banco de dados 
dados <- read.csv("imoveis.csv", sep = ";")

#Gráficos 
hist(dados$metragem,
     main="Histograma da metragem",
     xlab="Metragem",
     ylab="Frequência",
     col="pink")

#Medidas 