# Leitura dos dados
dados <- read.csv("imoveis.csv")

# Visualização inicial
head(dados)

# Estatísticas descritivas
summary(dados)

# Lendo o arquivo (ajustado para o padrão brasileiro)
dados <- read.csv2("imoveis.csv")
dados <- read.csv("imoveis.csv", sep = ";", dec = ",")
head(dados)

# Limpando pesado as colunas (remove R$, espaços e pontos)
limpar_numero <- function(x) {
  as.numeric(gsub("[^0-9,]", "", as.character(x)))
}

dados$preco <- limpar_numero(dados$preco)
dados$metragem <- as.numeric(as.character(dados$metragem))
dados$imposto <- as.numeric(as.character(dados$imposto))

# Gerando os gráficos
# Histograma
hist(na.omit(dados$preco), 
     main="Histograma do Preço", 
     col="lightblue", xlab="Preço")

# Dispersão 
plot(dados$metragem, dados$imposto, 
     main="Imposto vs Metragem", 
     xlab="Metragem (x)", ylab="Imposto (y)", 
     pch=19, col="blue")
