#leitura do banco de dados 
dados <- read.csv("imoveis.csv", sep = ";")

#Gráficos 
hist(dados$metragem,
     main="Histograma da metragem",
     xlab="Metragem",
     ylab="Frequência",
     col="pink")

#Medidas 
media_imposto <- mean(dados$imposto_anual, na.rm = TRUE)
mediana_imposto <- median(dados$imposto_anual, na.rm = TRUE)
sd_imposto <- sd(dados$imposto_anual, na.rm = TRUE)
