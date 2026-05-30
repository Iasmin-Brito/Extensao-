# Script para leitura de bancos de dados diversos para geração de um data frame de uma única linha referente as informações do estado do aluno

# Ao receber este script esqueleto colocá-lo no repositório LOCAL Extensao, que deve ter sido clonado do GitHub
# Enviar o script esqueleto para o repositório REMOTO com o nome extensao-esqueleto.R

# Para realizar as tarefas da ETAPA 1, ABRIR ANTES uma branch de nome SINASC no main de Extensao e ir para ela
# Após os alunos concluírem a ETAPA 1 a professora orientará fazer o merge into main e depois abrir outro branch. Aguarde...


####################################
# ETAPA 1: BANCO DE DADOS DO SINASC
####################################

# A ALTERAÇÃO DO SCRIPT ESQUELETO - ETAPA 1 - DEVERÁ SER FEITA DENTRO DA BRANCH SINASC

# Tarefa 1. Leitura do banco de dados do SINASC 2015  com 3017668 linhas e 61 colunas
# verificar se a leitura foi feita corretamente e a estrutura dos dados
# nomeie o banco de dados como dados_sinasc

dados_sinasc = read.csv("SINASC_2015.csv", sep=";", header=TRUE)
head(dados_sinasc)
str(dados_sinasc)


# Tarefa 2. Reduzir dados_sinasc apenas para as colunas que serão utilizadas, nomeando este novo banco de dados como dados_sinasc_1
# as colunas serão 1, 4, 5, 6, 7, 12, 13, 14, 15, 19, 21, 22, 23, 24, 35, 38, 44, 46, 48, 59, 60, 61
# nomes das respectivas variáveis: CONTADOR, CODMUNNASC, LOCNASC, IDADEMAE, ESTCIVMAE, CODMUNRES, GESTACAO, GRAVIDEZ, PARTO,
# SEXO, APGAR5, RACACOR, PESO, IDANOMAL, ESCMAE2010, RACACORMAE, SEMAGESTAC, CONSPRENAT, TPAPRESENT, TPROBSON, PARIDADE, KOTELCHUCK

dados_sinasc_1 = dados_sinasc[, c(1, 4, 5, 6, 7, 12, 13, 14, 15, 19, 21, 22, 23, 24, 35, 38, 44, 46, 48, 59, 60, 61)]
summary(dados_sinasc_1)

# Tarefa 3. Reduzir dados_sinasc_1 apenas para o estado que o aluno irá trabalhar (utilizar os dois primeiros dígitos de CODMUNRES), nomeando este novo banco de dados como dados_sinasc_2
# Códigos das UF: 11: RO, 12: AC, 13: AM, 14: RR, 15: PA, 16: AP, 17: TO, 21: MA, 22: PI, 23: CE, 24: RN
# 25: PB, 26: PE, 27: AL, 28: SE, 29: BA, 31: MG, 32: ES, 33: RJ, 35: SP, 41: PR, 42: SC, 43: RS
# 50: MS, 51: MT, 52: GO, 53: DF 

# observar abaixo o número de nascimentos por UF de residência para certificar-se que seu banco de dados está correto
# 11: 27918     12: 16980     13: 80097     14: 11409     15: 143657    16: 15750      17: 25110
# 21: 117564    22: 49253     23: 132516    24: 49099     25: 59089     26: 145024     27: 52257     28: 34917     29: 206655
# 31: 268305    32: 56941     33: 236960    35: 634026     
# 41: 160947    42: 97223     43: 148359
# 50: 44142     51: 56673     52: 100672    53: 46122 

# Exportar o arquivo com o nome dados_sinasc_2.csv


# Ao concluir a Tarefa 3 da Etapa 1 commite e envie para o repositório REMOTO o script e dados_sinasc_2.csv com o comentário "Dados do estado UF (coloque o nome da UF) e script de sua obtenção"

summary(dados_sinasc_1$CODMUNRES)
UF = substr(as.character(dados_sinasc_1$CODMUNRES), 1, 2)

dados_sinasc_2 = dados_sinasc_1[UF == "26",]

# Exportar o arquivo com o nome dados_sinasc_2.csv e apagando arquivos não mais necessários

write.csv(dados_sinasc_2, "dados_sinasc_2.csv", row.names = FALSE)



# Tarefa 4. Verificar em dados_sinasc_2 a frequência das categorias das seguintes variáveis: LOCNASC, ESTCIVMAE, GESTACAO, GRAVIDEZ, PARTO,
# SEXO, APGAR5, RACACOR, IDANOMAL, ESCMAE2010, RACACORMAE, TPAPRESENT, TPROBSON, PARIDADE, KOTELCHUCK

table(dados_sinasc_2$LOCNASC)
table(dados_sinasc_2$ESTCIVMAE)
table(dados_sinasc_2$GESTACAO)
table(dados_sinasc_2$GRAVIDEZ)
table(dados_sinasc_2$PARTO)
table(dados_sinasc_2$SEXO)
table(dados_sinasc_2$RACACOR)
table(dados_sinasc_2$IDANOMAL)
table(dados_sinasc_2$ESCMAE2010)
table(dados_sinasc_2$RACACORMAE)
table(dados_sinasc_2$TPAPRESENT)
table(dados_sinasc_2$TPROBSON)
table(dados_sinasc_2$PARIDADE)
table(dados_sinasc_2$KOTELCHUCK)

# Aproveitando para ver os valores das variáveis quantitativas
unique(dados_sinasc_2$IDADEMAE)
unique(dados_sinasc_2$CONSPRENAT)
unique(dados_sinasc_2$SEMAGESTAC)
unique(dados_sinasc_2$APGAR5)
unique(dados_sinasc_2$PESO)
summary(dados_sinasc_2$PESO)

# Tarefa 5. Atribuir para cada variável de dados_sinasc_2 como sendo NA a categoria de "Não informado ou Ignorado", geralmente com código 9
# KOTELCHUCK = 9 significa "não informado"   TPROBSON = 11 significa "não classificado por falta de informação"
# veja o dicionário do SINASC para identificar qual o código das categorias de cada variável
# Em variáveis quantitativas como IDADEMAE, APGAR5 e PESO e SEMAGESTAC verificar se existem valores como 99 para NA
dados_sinasc_2$LOCNASC[dados_sinasc_2$LOCNASC == 9] = NA
dados_sinasc_2$IDADEMAE[dados_sinasc_2$IDADEMAE == 99] = NA
dados_sinasc_2$ESTCIVMAE[dados_sinasc_2$ESTCIVMAE == 9] = NA
dados_sinasc_2$GESTACAO[dados_sinasc_2$GESTACAO == 9] = NA
dados_sinasc_2$GRAVIDEZ[dados_sinasc_2$GRAVIDEZ == 9] = NA
dados_sinasc_2$PARTO[dados_sinasc_2$PARTO == 9] = NA
dados_sinasc_2$SEXO[dados_sinasc_2$SEXO == 0] = NA
dados_sinasc_2$APGAR5[dados_sinasc_2$APGAR5 == 99] = NA
dados_sinasc_2$PESO[dados_sinasc_2$PESO == 9999] = NA
dados_sinasc_2$IDANOMAL[dados_sinasc_2$IDANOMAL == 9] = NA
dados_sinasc_2$ESCMAE2010[dados_sinasc_2$ESCMAE2010 == 9] = NA
dados_sinasc_2$CONSPRENAT[dados_sinasc_2$CONSPRENAT == 99] = NA
dados_sinasc_2$TPAPRESENT[dados_sinasc_2$TPAPRESENT == 9] = NA
dados_sinasc_2$TPROBSON[dados_sinasc_2$TPROBSON == 11] = NA
dados_sinasc_2$KOTELCHUCK[dados_sinasc_2$KOTELCHUCK == 9] = NA
summary(dados_sinasc_2)

# Por curiosidade, verificando o tamanho dos banco de dados referente ao estado e aos municípios com e sem NAs
n_total_nasc_UF = nrow(dados_sinasc_2)
n_total_nasc_UF_sem_missing = sum(complete.cases(dados_sinasc_2))
n_total_nasc_MUN = tapply(rep(1, nrow(dados_sinasc_2)), dados_sinasc_2$CODMUNRES, sum)
n_total_nasc_MUN_sem_missing = tapply(complete.cases(dados_sinasc_2), dados_sinasc_2$CODMUNRES, sum)

# Tarefa 6. Atribuir legendas para as categorias das variáveis investigadas na etapa 4.
# Exemplo: dados_sinasc_2$KOTELCHUCK = factor(dados_sinasc_2$KOTELCHUCK, levels = c(1,2,3,4,5), 
# labels = c("Não realizou pré-natal", "Inadequado", "Intermediário", "Adequado",  
# "Mais que adequado")

# ATENçÃO: 1. Na hora de escrever os labels, somente a primeira letra da palavra é maiúscula. Exemplo para SEXO: Feminino e Masculino
#          2. Nesta Tarefa 6 não crie novas variáveis no banco de dados

dados_sinasc_2$LOCNASC = factor(dados_sinasc_2$LOCNASC, levels = c(1,2,3,4,5), labels = c("Hospital", "Outros estabelecimentos de saúde", "Domicílio", "Outros", "Aldeia indígena"))
dados_sinasc_2$ESTCIVMAE = factor(dados_sinasc_2$ESTCIVMAE, levels = c(1,2,3,4,5), labels = c("Solteira", "Casada", "Viúva", "Separada judicialmente/divorciada", "União estável"))
dados_sinasc_2$GESTACAO = factor(dados_sinasc_2$GESTACAO, levels = c(1,2,3,4,5,6), labels = c("Menos de 22 semanas", "22 a 27 semanas", "28 a 31 semanas", "32 a 36 semanas", "37 a 41 semanas", "42 semanas e mais"))
dados_sinasc_2$GRAVIDEZ = factor(dados_sinasc_2$GRAVIDEZ, levels = c(1,2,3), labels = c("Única", "Dupla", "Tripla ou mais"))
dados_sinasc_2$PARTO = factor(dados_sinasc_2$PARTO, levels = c(1,2), labels = c("Vaginal", "Cesário"))
dados_sinasc_2$SEXO = factor(dados_sinasc_2$SEXO, levels = c(1,2), labels = c("Masculino", "Feminino"))
dados_sinasc_2$RACACOR = factor(dados_sinasc_2$RACACOR, levels = c(1,2,3,4,5), labels = c("Branca", "Preta", "Amarela", "Parda", "Indígena"))
dados_sinasc_2$IDANOMAL = factor(dados_sinasc_2$IDANOMAL, levels = c(1,2), labels = c("Sim", "Não"))
dados_sinasc_2$ESCMAE2010 = factor(dados_sinasc_2$ESCMAE2010, levels = c(0,1,2,3,4,5), labels = c("Sem escolaridade", "Fundamental I (1ª a 4ª série)", "Fundamental II (5ª a 8ª série)", "Médio (antigo 2º grau)", "Superior incompleto", "Superior completo"))
dados_sinasc_2$RACACORMAE = factor(dados_sinasc_2$RACACORMAE, levels = c(1,2,3,4,5), labels = c("Branca", "Preta", "Amarela", "Parda", "Indígena"))
dados_sinasc_2$TPAPRESENT = factor(dados_sinasc_2$TPAPRESENT, levels = c(1,2,3), labels = c("Cefálico", "Pélvica ou podálica", "Transversa"))
dados_sinasc_2$TPROBSON = factor(dados_sinasc_2$TPROBSON, levels = c(1,2,3,4,5,6,7,8,9,10), labels = c("Grupo 1", "Grupo 2", "Grupo 3", "Grupo 4", "Grupo 5", "Grupo 6", "Grupo 7", "Grupo 8", "Grupo 9", "Grupo 10"))
dados_sinasc_2$PARIDADE = factor(dados_sinasc_2$PARIDADE, levels = c(0,1), labels = c("Nulípara", "Multípara"))
dados_sinasc_2$KOTELCHUCK = factor(dados_sinasc_2$KOTELCHUCK, levels = c(1,2,3,4,5), labels = c("Não realizou pré-natal", "Inadequado", "Intermediário", "Adequado", "Mais que adequado"))

# Tarefa 7. Categorizar as variáveis IDADEMAE, PESO e APGAR5 e criar variáveis referentes ao deslocamento materno (peregrinação) e estado civil
# nova variável: dados_sinasc_2$F_PESO com PESO: < 2500: Baixo peso, >=2500 e < 4000: Peso normal, >= 4000: Macrossomia
# nova variável dados_sinasc_2$F_IDADE com IDADEMAE: <15, 15-19, 20-24, 25-29, 30-34, 35-39, 40-44, 45-49, 50+
# nova variável dados_sinasc_2$F_APGAR5 com APGAR5: < 7: Baixo, >= 7: Normal
# Atenção para casos de NA em IDADEMAE, PESO e APGAR5
dados_sinasc_2$F_IDADE = ifelse(dados_sinasc_2$IDADEMAE < 15, "<15",
                         ifelse(dados_sinasc_2$IDADEMAE <= 19, "15-19",
                         ifelse(dados_sinasc_2$IDADEMAE <= 24, "20-24",
                         ifelse(dados_sinasc_2$IDADEMAE <= 29, "25-29",
                         ifelse(dados_sinasc_2$IDADEMAE <= 34, "30-34",
                         ifelse(dados_sinasc_2$IDADEMAE <= 39, "35-39",
                         ifelse(dados_sinasc_2$IDADEMAE <= 44, "40-44",
                         ifelse(dados_sinasc_2$IDADEMAE <= 49, "45-49",
                         "50+"))))))))
dados_sinasc_2$F_IDADE = factor(dados_sinasc_2$F_IDADE,
                                levels = c("<15","15-19","20-24","25-29","30-34","35-39","40-44","45-49","50+"), ordered = TRUE)

dados_sinasc_2$F_PESO = ifelse(dados_sinasc_2$PESO < 2500, "Baixo peso",
                               ifelse(dados_sinasc_2$PESO < 4000, "Peso normal",
                                      "Macrossomia"))
dados_sinasc_2$F_PESO = factor(dados_sinasc_2$F_PESO, levels = c("Baixo peso","Peso normal","Macrossomia"))

dados_sinasc_2$F_APGAR5 = ifelse(dados_sinasc_2$APGAR5 < 7, "Baixo", "Normal")
dados_sinasc_2$F_APGAR5 = factor(dados_sinasc_2$F_APGAR5,levels = c("Baixo","Normal"))

dados_sinasc_2$PEREG = ifelse(is.na(dados_sinasc_2$CODMUNNASC) | is.na(dados_sinasc_2$CODMUNRES), NA,
                              ifelse(dados_sinasc_2$CODMUNNASC == dados_sinasc_2$CODMUNRES, "Não", "Sim"))
dados_sinasc_2$PEREG = factor(dados_sinasc_2$PEREG, levels = c("Não", "Sim"))

dados_sinasc_2$ESTCIV = ifelse(dados_sinasc_2$ESTCIVMAE %in% c("Solteira", "Viúva", "Separada judicialmente/divorciada"), "Sem companheiro",
                               ifelse(dados_sinasc_2$ESTCIVMAE %in% c("Casada", "União estável"), "Com companheiro", NA))
dados_sinasc_2$ESTCIV = factor(dados_sinasc_2$ESTCIV, levels = c("Sem companheiro","Com companheiro"))

# Tarefa 8. Agregar ao banco de dados_sinasc_2 as informações PESO_P10 e PESO_P90 a partir de Tabela_PIG_Brasil.csv
# a Tabela PIG informa P10 e P90 dos pesos, de acordo com a idade gestacional
# criar nova variável referente ao peso, de acordo com a idade gestacional, conforme indicado abaixo
# nova variável apenas para casos de GRAVIDEZ Única: dados_sinasc_2$F_PIG: PIG: PESO < PESO_P10, AIG: PESO_P10 <= PESO <= PESO_P90, GIG: PESO > PESO_P90
# Atenção para casos de NA em SEMAGESTAC, PESO ou SEXO. Lembre-se também que em dados_sinasc_2 SEXO está como fator com as categorias Feminino e Masculino.
tabela_pig = read.csv("Tabela_PIG_Brasil.csv", header = TRUE, sep=";")
tabela_pig$SEXO = factor(tabela_pig$SEXO, levels = c("Masculino", "Feminino"))
dados_sinasc_2 = merge(dados_sinasc_2, tabela_pig, by = c("SEMAGESTAC","SEXO"), all.x = TRUE)

dados_sinasc_2$F_PIG = ifelse(dados_sinasc_2$GRAVIDEZ != "Única", NA,
                              ifelse(is.na(dados_sinasc_2$PESO) | is.na(dados_sinasc_2$PESO_P10) | is.na(dados_sinasc_2$PESO_P90), NA,
                                     ifelse(dados_sinasc_2$PESO < dados_sinasc_2$PESO_P10, "PIG",
                                            ifelse(dados_sinasc_2$PESO <= dados_sinasc_2$PESO_P90, "AIG",
                                                   "GIG"))))

dados_sinasc_2$F_PIG = factor(dados_sinasc_2$F_PIG, levels = c("PIG","AIG","GIG"))

# Tarefas 9 e 10 (reformulada) do script esqueleto:

# Crie um banco de dados, de nome SINASC_UF.csv (Exemplo: SINASC_RJ.csv), contendo as 103 variáveis listadas no arquivo “Variáveis - Projeto - Tarefas 9 e 10 da Etapa 1.pdf”

# O banco final deverá possuir:
# • 103 colunas, correspondentes às variáveis especificadas;
# • n + 1 linhas, onde:
#  • n corresponde ao número de municípios distintos da UF em análise
# • a primeira linha corresponde aos valores agregados para a UF como um todo;
# • as demais linhas correspondem aos municípios da UF.
# As variáveis devem ser construídas a partir dos microdados do SINASC (dados_sinasc, dados_sinasc_1 e dados_sinasc_2), respeitando os nomes e a ordem especificados.

#################################################  

#################################################  
# Base inicial (municípios)
#################################################
base = data.frame(CODMUNRES = sort(unique(dados_sinasc_2$CODMUNRES)))

# TN - total de nascimentos
TN = as.data.frame(table(factor(dados_sinasc_2$CODMUNRES)))
names(TN) = c("CODMUNRES","TN")

base = merge(base, TN, by = "CODMUNRES", all.x = TRUE)


# TNRC - completos nas 61 variáveis
dados_UF = dados_sinasc[substr(as.character(dados_sinasc$CODMUNRES), 1, 2) == "26",]
dados_UF_comp = dados_UF[complete.cases(dados_UF), ]

TNRC = as.data.frame(table(factor(dados_UF_comp$CODMUNRES,levels = base$CODMUNRES)))
names(TNRC) = c("CODMUNRES","TNRC")

base = merge(base, TNRC, by = "CODMUNRES", all.x = TRUE)

# TNRCR - completos nas 22 variáveis
dados_UF_1 = dados_sinasc_1[substr(as.character(dados_sinasc_1$CODMUNRES), 1, 2) == "26",]
dados_UF_1_comp = dados_UF_1[complete.cases(dados_UF_1), ]

TNRCR = as.data.frame(table(factor(dados_UF_1_comp$CODMUNRES, levels = base$CODMUNRES)))
names(TNRCR) = c("CODMUNRES","TNRCR")

base = merge(base, TNRCR, by = "CODMUNRES", all.x = TRUE)

#################################################
# Informações das gestantes
#################################################
# Idade

# Frequências
tab = table(dados_sinasc_2$CODMUNRES, factor(dados_sinasc_2$F_IDADE, levels = c("<15","15-19","20-24","25-29", "30-34","35-39","40-44","45-49","50+")))
df = as.data.frame.matrix(tab)
names(df) = c("TGI_15","TGI_15_19","TGI_20_24","TGI_25_29", "TGI_30_34","TGI_35_39","TGI_40_44","TGI_45_49","TGI_50")
df$CODMUNRES = rownames(df)

df$TGIF= df$TGI_15_19 + df$TGI_20_24 + df$TGI_25_29 + df$TGI_30_34 +
  df$TGI_35_39 + df$TGI_40_44 + df$TGI_45_49

base = merge(base, df, by = "CODMUNRES", all.x = TRUE)

# Percentis
p_idade = aggregate(IDADEMAE ~ CODMUNRES,dados_sinasc_2, function(x) quantile(x, probs = c(0.25,0.5,0.75), na.rm = TRUE))
p_idade = do.call(data.frame, p_idade)
names(p_idade) = c("CODMUNRES","IM_P25","IM_P50","IM_P75")
p_idade[, c("IM_P25","IM_P50","IM_P75")] = round(p_idade[, c("IM_P25","IM_P50","IM_P75")], 2)

base = merge(base, p_idade, by="CODMUNRES", all.x=TRUE)

# Média
media_idade = aggregate(IDADEMAE ~ CODMUNRES, dados_sinasc_2, mean, na.rm = TRUE)
media_idade$IDADEMAE = round(media_idade$IDADEMAE, 2)
names(media_idade)[2] = "IM_MD"

# Desvio-padrão
dp_idade = aggregate(IDADEMAE ~ CODMUNRES, dados_sinasc_2, sd, na.rm = TRUE)
dp_idade$IDADEMAE = round(dp_idade$IDADEMAE, 2)
names(dp_idade)[2] = "IM_DP"
temp = merge(media_idade, dp_idade, by = "CODMUNRES")

base = merge(base, temp, by = "CODMUNRES", all.x = TRUE)

# Escolaridade
# Frequências
tab = table(dados_sinasc_2$CODMUNRES, factor(dados_sinasc_2$ESCMAE2010, levels = c("Sem escolaridade", "Fundamental I (1ª a 4ª série)", "Fundamental II (5ª a 8ª série)", "Médio (antigo 2º grau)", "Superior incompleto", "Superior completo")))
df = as.data.frame.matrix(tab)
names(df) = c("EM_S","EM_FI","EM_FII","EM_M", "EM_SI","EM_SC")
df$CODMUNRES = rownames(df)

base = merge(base, df, by = "CODMUNRES", all.x = TRUE)


# Raca/Cor da Mãe
# Frequências
tab = table(dados_sinasc_2$CODMUNRES, factor(dados_sinasc_2$RACACORMAE, levels = c("Branca", "Preta", "Amarela", "Parda", "Indígena")))
df = as.data.frame.matrix(tab)
names(df) = c("TGRC_B","TGRC_PT","TGRC_A","TGRC_PD", "TGRC_I")
df$CODMUNRES = rownames(df)

base = merge(base, df, by = "CODMUNRES", all.x = TRUE)

# Estado civil
tab = table(dados_sinasc_2$CODMUNRES, factor(dados_sinasc_2$ESTCIV, levels = c("Sem companheiro","Com companheiro")))
df = as.data.frame.matrix(tab)
names(df) = c("TGSC","TGCC")
df$CODMUNRES = rownames(df)

base = merge(base, df, by="CODMUNRES", all.x=TRUE)


# Primiparidade
tab = table(dados_sinasc_2$CODMUNRES, factor(dados_sinasc_2$PARIDADE, levels = c("Nulípara", "Multípara")))
df = as.data.frame.matrix(tab)
names(df) = c("TGPRI","TGNPRI")
df$CODMUNRES = rownames(df)

base = merge(base, df, by="CODMUNRES", all.x=TRUE)


#################################################
# Informações das gestações
#################################################
# Tipo
tab = table(dados_sinasc_2$CODMUNRES, factor(dados_sinasc_2$GRAVIDEZ, levels = c("Única", "Dupla", "Tripla ou mais")))
df = as.data.frame.matrix(tab)
names(df) = c("TGU","TEMP1","TEMP2")
df$TGG = df$TEMP1 + df$TEMP2
df$CODMUNRES = rownames(df)

base = merge(base, df[,c("CODMUNRES","TGU","TGG")], by = "CODMUNRES", all.x = TRUE)

# Duração da gestação
tab = table(dados_sinasc_2$CODMUNRES, factor(dados_sinasc_2$GESTACAO, levels = c("Menos de 22 semanas", "22 a 27 semanas", "28 a 31 semanas", "32 a 36 semanas", "37 a 41 semanas", "42 semanas e mais")))
df = as.data.frame.matrix(tab)
names(df) = c("TGD_22","TGD_22_27","TGD_28_31","TGD_32_36", "TGD_37_41", "TGD_42")
df$CODMUNRES = rownames(df)

df$TGD_PRT= df$TGD_22 + df$TGD_22_27 + df$TGD_28_31 + df$TGD_32_36 
df$TGD_AT= df$TGD_37_41
df$TGD_PST= df$TGD_42

base = merge(base, df, by = "CODMUNRES", all.x = TRUE)

# Percentis
p_duracao = aggregate(SEMAGESTAC ~ CODMUNRES,dados_sinasc_2, function(x) quantile(x, probs = c(0.25,0.5,0.75), na.rm = TRUE))
p_duracao = do.call(data.frame, p_duracao)
names(p_duracao) = c("CODMUNRES","DG_P25","DG_P50","DG_P75")
p_duracao[, c("DG_P25","DG_P50","DG_P75")] = round(p_duracao[, c("DG_P25","DG_P50","DG_P75")], 2)

base = merge(base, p_duracao, by="CODMUNRES", all.x=TRUE)

# Média
media_duracao = aggregate(SEMAGESTAC ~ CODMUNRES, dados_sinasc_2, mean, na.rm = TRUE)
media_duracao$SEMAGESTAC = round(media_duracao$SEMAGESTAC, 2)
names(media_duracao)[2] = "DG_MD"

# Desvio-padrão
dp_duracao = aggregate(SEMAGESTAC ~ CODMUNRES, dados_sinasc_2, sd, na.rm = TRUE)
dp_duracao$SEMAGESTAC = round(dp_duracao$SEMAGESTAC, 2)
names(dp_duracao)[2] = "DG_DP"
temp = merge(media_duracao, dp_duracao, by = "CODMUNRES")

base = merge(base, temp, by = "CODMUNRES", all.x = TRUE)


# Consultas de pre-natal
tab = table(dados_sinasc_2$CODMUNRES, factor(dados_sinasc_2$KOTELCHUCK, levels = c("Não realizou pré-natal", "Inadequado", "Intermediário", "Adequado", "Mais que adequado")))
df = as.data.frame.matrix(tab)
names(df) = c("TKC_NR","TKC_ID","TKC_IT","TKC_AD", "TKC_MAD")
df$CODMUNRES = rownames(df)

base = merge(base, df, by = "CODMUNRES", all.x = TRUE)


#################################################
# Informações dos partos
#################################################
# Peregrinação
tab = table(dados_sinasc_2$CODMUNRES, factor(dados_sinasc_2$PEREG, levels = c("Sim","Não")))
df = as.data.frame.matrix(tab)
names(df) = c("TGPRG_S","TGPRG_N")
df$CODMUNRES = rownames(df)

base = merge(base, df, by="CODMUNRES", all.x=TRUE)

# Tipo de parto
tab = table(dados_sinasc_2$CODMUNRES, factor(dados_sinasc_2$PARTO, levels = c("Vaginal", "Cesário")))
df = as.data.frame.matrix(tab)
names(df) = c("TPV","TPC")
df$CODMUNRES = rownames(df)

base = merge(base, df, by="CODMUNRES", all.x=TRUE)

# Posição do feto
tab = table(dados_sinasc_2$CODMUNRES, factor(dados_sinasc_2$TPAPRESENT, levels = c("Cefálico", "Pélvica ou podálica", "Transversa")))
df = as.data.frame.matrix(tab)
names(df) = c("TRAP_C", "TRAP_P", "TRAP_T")
df$CODMUNRES = rownames(df)

base = merge(base, df, by="CODMUNRES", all.x=TRUE)


# Grupo de Robson
tab = table(dados_sinasc_2$CODMUNRES, factor(dados_sinasc_2$TPROBSON, levels = c("Grupo 1", "Grupo 2", "Grupo 3", "Grupo 4", "Grupo 5", "Grupo 6", "Grupo 7", "Grupo 8", "Grupo 9", "Grupo 10")))
df = as.data.frame.matrix(tab)
names(df) = c("TGROB_1","TGROB_2", "TGROB_3", "TGROB_4", "TGROB_5", "TGROB_6", "TGROB_7", "TGROB_8", "TGROB_9", "TGROB_10")
df$CODMUNRES = rownames(df)

base = merge(base, df, by="CODMUNRES", all.x=TRUE)


# Local de nascimento
tab = table(dados_sinasc_2$CODMUNRES, factor(dados_sinasc_2$LOCNASC, levels = c("Hospital", "Outros estabelecimentos de saúde", "Domicílio", "Outros", "Aldeia indígena")))
df = as.data.frame.matrix(tab)
names(df) = c("TNLOC_H", "TNLOC_ES", "TNLOC_D", "TNLOC_O", "TNLOC_AI")
df$CODMUNRES = rownames(df)

base = merge(base, df, by="CODMUNRES", all.x=TRUE)


#################################################
# Informações dos recém-nascidos
#################################################
# Sexo
tab = table(dados_sinasc_2$CODMUNRES, factor(dados_sinasc_2$SEXO, levels = c("Masculino","Feminino")))
df = as.data.frame.matrix(tab)
names(df) = c("TRS_M", "TRS_F")
df$CODMUNRES = rownames(df)

base = merge(base, df, by="CODMUNRES", all.x=TRUE)


# Raça/Cor
tab = table(dados_sinasc_2$CODMUNRES, factor(dados_sinasc_2$RACACOR, levels = c("Branca", "Preta", "Amarela", "Parda", "Indígena")))
df = as.data.frame.matrix(tab)
names(df) = c("TRRC_B","TRRC_PT","TRRC_A", "TRRC_PD", "TRRC_I")
df$CODMUNRES = rownames(df)

base = merge(base, df, by="CODMUNRES", all.x=TRUE)


# Peso
# Frequências
tab = table(dados_sinasc_2$CODMUNRES, factor(dados_sinasc_2$F_PESO, levels = c("Baixo peso","Peso normal","Macrossomia")))
df = as.data.frame.matrix(tab)
names(df) = c("TRP_BP", "TRP_N", "TRP_M")
df$CODMUNRES = rownames(df)

base = merge(base, df, by="CODMUNRES", all.x=TRUE)

# Percentis
p_peso = aggregate(PESO ~ CODMUNRES,dados_sinasc_2, function(x) quantile(x, probs = c(0.25,0.5,0.75), na.rm = TRUE))
p_peso = do.call(data.frame, p_peso)
names(p_peso) = c("CODMUNRES","PESO_P25","PESO_P50","PESO_P75")
p_peso[, c("PESO_P25","PESO_P50","PESO_P75")] = round(p_peso[, c("PESO_P25","PESO_P50","PESO_P75")], 2)

base = merge(base, p_peso, by="CODMUNRES", all.x=TRUE)

# Média
media_peso = aggregate(PESO ~ CODMUNRES, dados_sinasc_2, mean, na.rm = TRUE)
media_peso$PESO = round(media_peso$PESO, 2)
names(media_peso)[2] = "PESO_MD"

# Desvio-padrão
dp_peso = aggregate(PESO ~ CODMUNRES, dados_sinasc_2, sd, na.rm = TRUE)
dp_peso$PESO = round(dp_peso$PESO, 2)
names(dp_peso)[2] = "PESO_DP"
temp = merge(media_peso, dp_peso, by = "CODMUNRES")

base = merge(base, temp, by = "CODMUNRES", all.x = TRUE)


# Peso por idade gestacional - gestações únicas
tab = table(dados_sinasc_2$CODMUNRES, factor(dados_sinasc_2$F_PIG, levels = c("PIG","AIG","GIG")))
df = as.data.frame.matrix(tab)
names(df) = c("TRPIG_P", "TRPIG_A", "TRPIG_G")
df$CODMUNRES = rownames(df)

base = merge(base, df, by="CODMUNRES", all.x=TRUE)


# Apgar ao 5º minuto
# Frequências
tab = table(dados_sinasc_2$CODMUNRES, factor(dados_sinasc_2$F_APGAR5, levels = c("Baixo","Normal")))
df = as.data.frame.matrix(tab)
names(df) = c("TRAPG5_B","TRAPG5_N")
df$CODMUNRES = rownames(df)

base = merge(base, df, by="CODMUNRES", all.x=TRUE)

# Média
media_apgar5 = aggregate(APGAR5 ~ CODMUNRES, dados_sinasc_2, mean, na.rm = TRUE)
media_apgar5$APGAR5 = round(media_apgar5$APGAR5, 2)
names(media_apgar5)[2] = "APG5_MD"

# Desvio-padrão
dp_apgar5 = aggregate(APGAR5 ~ CODMUNRES, dados_sinasc_2, sd, na.rm = TRUE)
dp_apgar5$APGAR5 = round(dp_apgar5$APGAR5, 2)
names(dp_apgar5)[2] = "APG5_DP"
temp = merge(media_apgar5, dp_apgar5, by = "CODMUNRES")

base = merge(base, temp, by = "CODMUNRES", all.x = TRUE)


# Anomalia congênita
tab = table(dados_sinasc_2$CODMUNRES, factor(dados_sinasc_2$IDANOMAL, levels = c("Sim", "Não")))
df = as.data.frame.matrix(tab)
names(df) = c("TRAC","TRSAC")
df$CODMUNRES = rownames(df)

base = merge(base, df, by="CODMUNRES", all.x=TRUE)


# Linha da UF
linha_estado = base[1, ]
linha_estado[,] = NA

# colunas de contagem: indicar as variáveis contínuas, que por exclusão não terão valores somados
cols_contagem = setdiff(names(base), c("CODMUNRES","IM_P25","IM_P50","IM_P75", "IM_MD","IM_DP", 
                                       "DG_P25", "DG_P50", "DG_P75", "DG_MD", "DG_DP", 
                                       "PESO_P25", "PESO_P50", "PESO_P75", "PESO_MD", "PESO_DP",
                                       "APG5_MD", "APG5_DP"))

linha_estado[cols_contagem] = colSums(base[cols_contagem], na.rm = TRUE)

# medidas para variáveis quantitativas 
# Idade da mãe
linha_estado$IM_MD = round(mean(dados_sinasc_2$IDADEMAE, na.rm = TRUE), 2)
linha_estado$IM_DP = round(sd(dados_sinasc_2$IDADEMAE, na.rm = TRUE), 2)

q = round(quantile(dados_sinasc_2$IDADEMAE, probs = c(0.25,0.5,0.75), na.rm = TRUE), 2)
linha_estado$IM_P25 = q[1]
linha_estado$IM_P50 = q[2]
linha_estado$IM_P75 = q[3]


# Duração da gestação
linha_estado$DG_MD = round(mean(dados_sinasc_2$SEMAGESTAC, na.rm = TRUE), 2)
linha_estado$DG_DP = round(sd(dados_sinasc_2$SEMAGESTAC, na.rm = TRUE), 2)

q = round(quantile(dados_sinasc_2$SEMAGESTAC, probs = c(0.25,0.5,0.75), na.rm = TRUE), 2)
linha_estado$DG_P25 = q[1]
linha_estado$DG_P50 = q[2]
linha_estado$DG_P75 = q[3]


# Peso
linha_estado$PESO_MD = round(mean(dados_sinasc_2$PESO, na.rm = TRUE), 2)
linha_estado$PESO_DP = round(sd(dados_sinasc_2$PESO, na.rm = TRUE), 2)

q = round(quantile(dados_sinasc_2$PESO, probs = c(0.25,0.5,0.75), na.rm = TRUE), 2)
linha_estado$PESO_P25 = q[1]
linha_estado$PESO_P50 = q[2]
linha_estado$PESO_P75 = q[3]


# Apgar ao 5º minuto
linha_estado$APG5_MD = round(mean(dados_sinasc_2$APGAR5, na.rm = TRUE), 2)
linha_estado$APG5_DP = round(sd(dados_sinasc_2$APGAR5, na.rm = TRUE), 2)


# código da UF e ordem das colunas
linha_estado$CODMUNRES = 26

# Banco de dados final para o Acre
SINASC_PE = rbind(linha_estado, base)

SINASC_PE$NIVEL = c("UF", rep("MUNICIPIO", nrow(SINASC_PE)-1))
SINASC_PE$ANO = 2015

SINASC_PE = SINASC_PE[, c("ANO","NIVEL","CODMUNRES", names(SINASC_PE)[!names(SINASC_PE) %in% c("ANO","NIVEL","CODMUNRES")])]


# Ao terminar a ETAPA 1 commite e envie para o repositório REMOTO com o comentário "Dados da UF e Script Etapa 1"



##################################
# ETAPA 2: BANCO DE DADOS DO SIM
##################################
# Só inicie esta Etapa quando a professora orientar
# Altere o script esqueleto nas partes que se refere a ETAPA 2 e envie para o repositório Extensao tendo feito o commite "Esqueleto atualizado na Etapa 2"
# A partir de main crie a branch SIM
# ESTANDO NA BRANCH SIM, NÃO ALTERE NADA NO SCRIPT REFERENTE A ETAPA 1 e só insira comandos na ETAPA 2
# Para realizar as tarefas da ETAPA 2, ABRIR ANTES uma branch de nome SIM no main de Extensao e ir para ela

# Tarefa 1. Leitura do banco de dados Mortalidade_Geral_2015 do SIM 2015 com 1264175 linhas e 87 colunas
# verificar se a leitura foi feita corretamente e a estrutura dos dados
# nomeie o banco de dados como dados_sim

library(dplyr)
library(readr)

dados_sim <- read_csv2("Mortalidade_Geral_2015.csv")
dim(dados_sim)
str(dados_sim)
summary(dados_sim)
dados_sim$codigouf = substr(dados_sim$CODMUNRES, 1, 2)
table(dados_sim$codigouf)

# Tarefa 2. Reduzir dados_sim apenas para as colunas que serão utilizadas, nomeando este novo banco de dados como dados_sim_1
# as colunas serão: 1, 3, 4, 8, 9, 10, 11, 14, 17, 35, 36, 37, 47, 77, 84
# nomes das respectivas variáveis: CONTADOR, TIPOBITO, DTOBITO, DTNASC, IDADE, SEXO, RACACOR, ESC2010, CODMUNRES, TPMORTEOCO, 
# OBITOGRAV, OBITOPUERP, CAUSABAS, TPOBITOCOR, MORTEPARTO

dados_sim_1 <- dados_sim[, c(1, 3, 4, 8, 9, 10, 11, 14, 17, 35, 36, 37, 47, 77, 84)]
dim(dados_sim_1)


# Tarefa 3. Reduzir dados_sim_1 apenas para o estado que o aluno irá trabalhar (utilizar os dois primeiros dígitos de CODMUNRES), nomeando este novo banco de dados como dados_sim_2
# Códigos das UF: 11: RO, 12: AC, 13: AM, 14: RR, 15: PA, 16: AP, 17: TO, 21: MA, 22: PI, 23: CE, 24: RN
# 25: PB, 26: PE, 27: AL, 28: SE, 29: BA, 31: MG, 32: ES, 33: RJ, 35: SP, 41: PR, 42: SC, 43: RS
# 50: MS, 51: MT, 52: GO, 53: DF 

# observar abaixo o número de óbitos por UF de residência para certificar-se que seu banco de dados está correto
# 11: 7948      12: 3517      13: 16675     14: 2091      15: 37365     16: 2946       17: 7402
# 21: 33666     22: 19366     23: 55258     24: 20153     25: 26422     26: 62556      27: 19756     28: 13453     29: 87083
# 31: 131274    32: 22332     33: 127714    35: 287645     
# 41: 70839     42: 37984     43: 82349
# 50: 15457     51: 17095     52: 38854     53: 11975

# Exportar o arquivo com o nome dados_sim_2.csv

dados_sim_1$CODMUNRES <- as.character(dados_sim_1$CODMUNRES)
dados_sim_2 <- dados_sim_1 %>%
  filter(substr(CODMUNRES, 1, 2) == "26")
nrow(dados_sim_2)
write_csv(dados_sim_2, "dados_sim_2.csv")

# Ao concluir a Tarefa 3 da Etapa 2 commite e envie para o repositório REMOTO o script e dados_sim_2.csv com o comentário "Dados do estado UF (coloque o nome da UF) e script de sua obtenção"


# Tarefa 4. Verificar em dados_sim_2 a frequência das categorias das seguintes variáveis: TIPOBITO, SEXO, RACACOR, 
# TPMORTEOCO, OBITOGRAV, OBITOPUERP, CAUSABAS, TPOBITOCOR, MORTEPARTO
# 1. Definir o vetor de variáveis corretamente (com todas as aspas e vírgulas)

vars_cat = c("TIPOBITO", "SEXO", "RACACOR", "TPMORTEOCO", "OBITOGRAV", "OBITOPUERP", "CAUSABAS", "TPOBITOCOR", "MORTEPARTO")
lapply(dados_sim_2[vars_cat], table)


# Tarefa 5. Atribuir para cada variável de dados_sim_2 como sendo NA a categoria de "Não informado ou Ignorado", geralmente com código 9
# veja o dicionário do SIM para identificar qual o código das categorias de cada variável
# Em variáveis quantitativas como IDADE verificar se existem valores como 99 para NA

dados_sim_2 <- dados_sim_2 %>%
  mutate(
    
    # IDADE → 9 = ignorado
    IDADE = ifelse(IDADE == 999, NA, IDADE),
    
    # SEXO → I, 0, 9 = ignorado
    SEXO = ifelse(SEXO %in% c("I", "0", "9"), NA, SEXO),
    
    # ESC2010 → 9 = ignorado
    ESC2010 = ifelse(ESC2010 == 9, NA, ESC2010),
    
    # TPMORTEOCO → 9 = ignorado
    TPMORTEOCO = ifelse(TPMORTEOCO == 9, NA, TPMORTEOCO),
    
    # OBITOGRAV → 9 = ignorado
    OBITOGRAV = ifelse(OBITOGRAV == 9, NA, OBITOGRAV),
    
    # OBITOPUERP → 9 = ignorado
    OBITOPUERP = ifelse(OBITOPUERP == 9, NA, OBITOPUERP),
    
    # TPOBITOCOR → 9 = ignorado
    TPOBITOCOR = ifelse(TPOBITOCOR == 9, NA, TPOBITOCOR),
    
    # MORTEPARTO → 9 = ignorado
    MORTEPARTO = ifelse(MORTEPARTO == 9, NA, MORTEPARTO)
  )

# Tarefa 6. Atribuir legendas para as categorias das variáveis qualitativas investigadas na tarefa 4.
# Exemplo: dados_sim_2$TIPOBITO = factor(dados_sim_2$TIPOBITO, levels = c(1,2), 
# labels = c("Fetal", "Não fetal")

# ATENçÃO: 1. Na hora de escrever os labels, somente a primeira letra da palavra é maiúscula. Exemplo para SEXO: Feminino e Masculino
#          2. Nesta Tarefa 6 não crie novas variáveis no banco de dados

# TIPOBITO
dados_sim_2$TIPOBITO <- factor(
  dados_sim_2$TIPOBITO,
  levels = c(1,2),
  labels = c(
    "Fetal",
    "Não fetal"
  )
)

# SEXO
dados_sim_2$SEXO <- factor(
  dados_sim_2$SEXO,
  levels = c("M","1","F","2"),
  labels = c(
    "Masculino",
    "Masculino",
    "Feminino",
    "Feminino"
  )
)

# RACACOR
dados_sim_2$RACACOR <- factor(
  dados_sim_2$RACACOR,
  levels = c(1,2,3,4,5),
  labels = c(
    "Branca",
    "Preta",
    "Amarela",
    "Parda",
    "Indígena"
  )
)

# TPMORTEOCO
dados_sim_2$TPMORTEOCO <- factor(
  dados_sim_2$TPMORTEOCO,
  levels = c(1,2,3,4,5,8),
  labels = c(
    "Na gravidez",
    "No parto",
    "No abortamento",
    "Até 42 dias após o parto",
    "De 43 dias a 1 ano",
    "Não ocorreu nestes períodos"
  )
)

# OBITOGRAV
dados_sim_2$OBITOGRAV <- factor(
  dados_sim_2$OBITOGRAV,
  levels = c(1,2),
  labels = c(
    "Sim",
    "Não"
  )
)

# OBITOPUERP
dados_sim_2$OBITOPUERP <- factor(
  dados_sim_2$OBITOPUERP,
  levels = c(1,2,3),
  labels = c(
    "Sim até 42 dias",
    "Sim de 43 dias a 1 ano",
    "Não"
  )
)

# TPOBITOCOR
dados_sim_2$TPOBITOCOR <- factor(
  dados_sim_2$TPOBITOCOR,
  levels = c(1,2,3,4,5,6,7,8,9),
  labels = c(
    "Durante a gestação",
    "Durante o abortamento",
    "Após o abortamento",
    "No parto ou até 1 hora após o parto",
    "No puerpério até 42 dias após o parto",
    "Entre 43 dias e até 1 ano após o parto",
    "A investigação não identificou o momento",
    "Mais de um ano após o parto",
    "Não ocorreu nas circunstâncias anteriores"
  )
)

# MORTEPARTO
dados_sim_2$MORTEPARTO <- factor(
  dados_sim_2$MORTEPARTO,
  levels = c(1,2,3),
  labels = c(
    "Antes",
    "Durante",
    "Após"
  )
)


# Tarefa 7. Crie um banco de dados, de nome SIM_UF.csv (Exemplo: SIM_RJ.csv), contendo as 41 variáveis listadas no arquivo “Variáveis - Projeto - Tarefa 7 da Etapa 2.pdf”
# Atenção:
# 1. Para informações gerais utilize CAUSABAS, SEXO e IDADE
# 2. Para informações fetais utilize TIPOBITO
# 3. Para informações neonatais utilize TIPOBITO não fetal e IDADE entre 0 e 27 dias e RACACOR
# 4. Para informações maternas utilize TPMORTEOCO, ESC e IDADE


# Função auxiliar para idade em anos
idade_anos <- function(x) {
  
  x <- sprintf("%03d", as.numeric(x))
  
  tipo <- substr(x, 1, 1)
  valor <- as.numeric(substr(x, 2, 3))
  
  idade <- case_when(
    
    # minutos → 0 anos
    tipo == "0" ~ 0,
    # horas → 0 anos
    tipo == "1" ~ 0,
    # dias → 0 anos
    tipo == "2" ~ 0,
    # meses → menor que 1 ano
    tipo == "3" ~ 0,
    # anos
    tipo == "4" ~ valor,
    # mais de 100 anos
    tipo == "5" ~ 100 + valor
  )
  
  return(idade)
}

dados_sim_2$IDADE_ANOS <- idade_anos(dados_sim_2$IDADE)

# Função resumo para UF e municípios
resumo_sim <- function(base, nivel, codigo) {
  
  data.frame(
    
    # IDENTIFICAÇÃO
    ANO = 2015,
    NIVEL = nivel,
    CODMUNRES = codigo,
    
    # INFORMAÇÕES GERAIS
    TO = nrow(base),
    
    TORC = sum(complete.cases(base), na.rm = TRUE),
    
    TORCR = sum(complete.cases(
      base[, c(
        "TIPOBITO","DTOBITO","DTNASC","IDADE",
        "SEXO","RACACOR","ESC2010","CODMUNRES",
        "TPMORTEOCO","OBITOGRAV","OBITOPUERP",
        "CAUSABAS","TPOBITOCOR","MORTEPARTO"
      )]
    )),
    
    TO_NN = sum(
      substr(base$CAUSABAS, 1, 1) %in% c("V","W","X","Y"),
      na.rm = TRUE
    ),
    
    TO_N = sum(
      !(substr(base$CAUSABAS, 1, 1) %in% c("V","W","X","Y")),
      na.rm = TRUE
    ),
    
    TO_CB_I = sum(
      substr(base$CAUSABAS, 1, 1) %in% c("A","B"),
      na.rm = TRUE
    ),
    
    TO_CB_N = sum(
      substr(base$CAUSABAS, 1, 1) %in% c("C","D"),
      na.rm = TRUE
    ),
    
    TO_CB_C = sum(
      substr(base$CAUSABAS, 1, 1) == "I",
      na.rm = TRUE
    ),
    
    TO_CB_R = sum(
      substr(base$CAUSABAS, 1, 1) == "J",
      na.rm = TRUE
    ),
    
    TO_CB_O = sum(
      !(substr(base$CAUSABAS, 1, 1) %in%
          c("A","B","C","D","I","J","V","W","X","Y")),
      na.rm = TRUE
    ),
    
    TO_M = sum(base$SEXO == "Masculino", na.rm = TRUE),
    
    TO_F = sum(base$SEXO == "Feminino", na.rm = TRUE),
    
    # mulher em idade fértil = 415 até 449
    TO_F_IF = sum(
      base$SEXO == "Feminino" &
        base$IDADE >= 415 &
        base$IDADE <= 449,
      na.rm = TRUE
    ),
    
    # FETAIS E NEONATAIS
    TO_FT = sum(
      base$TIPOBITO == "Fetal",
      na.rm = TRUE
    ),
    
    # neonatal = 0 a 27 dias
    TO_NT = sum(
      base$TIPOBITO == "Não fetal" &
        (
          (base$IDADE >= 200 & base$IDADE <= 227) |
            (base$IDADE < 123)
        ),
      na.rm = TRUE
    ),
    
    # neonatal precoce = 0 a 6 dias
    TO_NT_P = sum(
      base$TIPOBITO == "Não fetal" &
        (
          (base$IDADE >= 200 & base$IDADE <= 206) |
            (base$IDADE < 123)
        ),
      na.rm = TRUE
    ),
    
    # neonatal tardio = 7 a 27 dias
    TO_NT_T = sum(
      base$TIPOBITO == "Não fetal" &
        (
          base$IDADE >= 207 &
            base$IDADE <= 227
        ),
      na.rm = TRUE
    ),
    
    # pós-neonatal = 28 a 364 dias
    TO_PNT = sum(
      base$TIPOBITO == "Não fetal" &
        (
          base$IDADE >= 228 &
            base$IDADE <= 364
        ),
      na.rm = TRUE
    ),
    
    # durante a gestação
    TO_MT_G = sum(
      base$TPOBITOCOR == "Durante a gestação",
      na.rm = TRUE
    ),
    
    # neonatal por raça/cor
    TONT_B = sum(
      base$TIPOBITO == "Não fetal" &
        (
          (base$IDADE >= 200 & base$IDADE <= 227) |
            (base$IDADE < 123)
        ) &
        base$RACACOR == "Branca",
      na.rm = TRUE
    ),
    
    TONT_PT = sum(
      base$TIPOBITO == "Não fetal" &
        (
          (base$IDADE >= 200 & base$IDADE <= 227) |
            (base$IDADE < 123)
        ) &
        base$RACACOR == "Preta",
      na.rm = TRUE
    ),
    
    TONT_A = sum(
      base$TIPOBITO == "Não fetal" &
        (
          (base$IDADE >= 200 & base$IDADE <= 227) |
            (base$IDADE < 123)
        ) &
        base$RACACOR == "Amarela",
      na.rm = TRUE
    ),
    
    TONT_PD = sum(
      base$TIPOBITO == "Não fetal" &
        (
          (base$IDADE >= 200 & base$IDADE <= 227) |
            (base$IDADE < 123)
        ) &
        base$RACACOR == "Parda",
      na.rm = TRUE
    ),
    
    TONT_I = sum(
      base$TIPOBITO == "Não fetal" &
        (
          (base$IDADE >= 200 & base$IDADE <= 227) |
            (base$IDADE < 123)
        ) &
        base$RACACOR == "Indígena",
      na.rm = TRUE
    ),
    
    # MATERNAS
    TO_MT = sum(
      base$TPOBITOCOR %in% c(
        "Durante a gestação",
        "Durante o abortamento",
        "Após o abortamento",
        "No parto ou até 1 hora após o parto",
        "No puerpério até 42 dias após o parto",
        "Entre 43 dias e até 1 ano após o parto"
      ),
      na.rm = TRUE
    ),
    
    TO_MT_DG = sum(
      base$TPOBITOCOR == "Durante a gestação",
      na.rm = TRUE
    ),
    
    TO_MT_PT = sum(
      base$TPOBITOCOR == "No parto ou até 1 hora após o parto",
      na.rm = TRUE
    ),
    
    TO_MT_AB = sum(
      base$TPOBITOCOR %in% c(
        "Durante o abortamento",
        "Após o abortamento"
      ),
      na.rm = TRUE
    ),
    
    TO_MT_42 = sum(
      base$TPOBITOCOR == "No puerpério até 42 dias após o parto",
      na.rm = TRUE
    ),
    
    TO_MT_43 = sum(
      base$TPOBITOCOR == "Entre 43 dias e até 1 ano após o parto",
      na.rm = TRUE
    ),
    
    # maternos precoces
    TO_MT_P = sum(
      base$TPOBITOCOR %in% c(
        "Durante a gestação",
        "Durante o abortamento",
        "Após o abortamento",
        "No parto ou até 1 hora após o parto",
        "No puerpério até 42 dias após o parto"
      ),
      na.rm = TRUE
    ),
    
    TO_MT_P_I = sum(
      base$TPOBITOCOR %in% c(
        "Durante a gestação",
        "Durante o abortamento",
        "Após o abortamento",
        "No parto ou até 1 hora após o parto",
        "No puerpério até 42 dias após o parto"
      ) &
        base$IDADE >= 415 &
        base$IDADE <= 449,
      na.rm = TRUE
    ),
    
    # escolaridade
    TO_MT_P_ES = sum(
      base$TPOBITOCOR %in% c(
        "Durante a gestação",
        "Durante o abortamento",
        "Após o abortamento",
        "No parto ou até 1 hora após o parto",
        "No puerpério até 42 dias após o parto"
      ) &
        base$ESC2010 == 0,
      na.rm = TRUE
    ),
    
    TO_MT_P_EFI = sum(
      base$TPOBITOCOR %in% c(
        "Durante a gestação",
        "Durante o abortamento",
        "Após o abortamento",
        "No parto ou até 1 hora após o parto",
        "No puerpério até 42 dias após o parto"
      ) &
        base$ESC2010 %in% c(1,2),
      na.rm = TRUE
    ),
    
    TO_MT_P_EFII = sum(
      base$TPOBITOCOR %in% c(
        "Durante a gestação",
        "Durante o abortamento",
        "Após o abortamento",
        "No parto ou até 1 hora após o parto",
        "No puerpério até 42 dias após o parto"
      ) &
        base$ESC2010 %in% c(3,4),
      na.rm = TRUE
    ),
    
    TO_MT_P_EM = sum(
      base$TPOBITOCOR %in% c(
        "Durante a gestação",
        "Durante o abortamento",
        "Após o abortamento",
        "No parto ou até 1 hora após o parto",
        "No puerpério até 42 dias após o parto"
      ) &
        base$ESC2010 == 5,
      na.rm = TRUE
    ),
    
    TO_MT_P_ESI = sum(
      base$TPOBITOCOR %in% c(
        "Durante a gestação",
        "Durante o abortamento",
        "Após o abortamento",
        "No parto ou até 1 hora após o parto",
        "No puerpério até 42 dias após o parto"
      ) &
        base$ESC2010 == 6,
      na.rm = TRUE
    ),
    
    TO_MT_P_ESC = sum(
      base$TPOBITOCOR %in% c(
        "Durante a gestação",
        "Durante o abortamento",
        "Após o abortamento",
        "No parto ou até 1 hora após o parto",
        "No puerpério até 42 dias após o parto"
      ) &
        base$ESC2010 == 7,
      na.rm = TRUE
    )
    
  )
}

linha_uf <- resumo_sim(dados_sim_2,"UF",26)

linha_uf$CODMUNRES = as.character(linha_uf$CODMUNRES)

# Linhas dos municípios
lista_municipios <- split(
  dados_sim_2,
  dados_sim_2$CODMUNRES
)

linhas_municipios <- bind_rows(
  lapply(
    names(lista_municipios),
    function(cod) {
      resumo_sim(
        lista_municipios[[cod]],
        "MUNICIPIO",
        cod
      )
    }
  )
)

# Banco final
SIM_PE <- bind_rows(linha_uf,linhas_municipios)

# Exportar
write_csv(SIM_PE,"SIM_PE.csv")



# Tarefa 8: Exporte o banco de dados com o nome SIM_UF.csv

# Ao terminar a ETAPA 2 commite e envie para o repositório REMOTO com o comentário "Dados da UF e Script Etapa 2"
# Faça um merge de script de SIM para main

#####################################################
# ETAPA 3: OUTROS BANCOS DE DADOS: IBGE, SNIS, ...
#####################################################
# Só inicie esta Etapa quando a professora orientar
# Ao terminar a ETAPA 2 faça um merge de SIM para main
# Altere as orientações do script e commit (em main) "Script com orientações ETAPA 3 - SIDRA"
# Abra um branch OUTROS
# Na branch OUTROS escreva os comandos da Tarefa 1 abaixo

# Tarefa 1. Acesso aos bancos de dados do SIDRA e obtenção da informação
# Leia os arquivos:
# 1. população residente estimada - UF e municípios - 2015 - SIDRA - tabela_6579.csv  
# 2. população residente censo 2010 - UF e municípios - total e por sexo - SIDRA - tabela_1552.csv  
# 3. população residente censo 2010 - por faixa etária -  UF - SIDRA - tabela_1552.csv
# 4. população residente censo 2010 - por faixa etária e sexo -  municípios - SIDRA - tabela_1552.csv

# --- Leitura das Bases --------

# 1. População residente estimada
pop_estimada_2015 <- read.csv("população residente estimada - UF e municípios - 2015 - SIDRA - tabela_6579.csv", sep=";", header=TRUE)

# 2. População residente censo 2010 - Total e sexo
pop_censo_total_sexo <- read.csv("população residente censo 2010 - UF e municípios - total e por sexo - SIDRA - tabela_1552.csv", sep=";", header=TRUE)

# 3. População residente censo 2010 - Faixa etária UF
pop_censo_faixa_uf <- read.csv("população residente censo 2010 - por faixa etária -  UF - SIDRA - tabela_1552.csv", sep=";", header=TRUE)

# 4. População residente censo 2010 - Faixa etária e sexo municípios
pop_censo_faixa_sexo_mun <- read.csv("população residente censo 2010 - por faixa etária e sexo -  municípios - SIDRA - tabela_1552.csv", sep=";", header=TRUE)

#Verificando a estrutura
dim(pop_estimada_2015)
str(pop_estimada_2015)

dim(pop_censo_total_sexo)
str(pop_censo_total_sexo)

dim(pop_censo_faixa_uf)
str(pop_censo_faixa_uf)

dim(pop_censo_faixa_sexo_mun)
str(pop_censo_faixa_sexo_mun)

# A partir dos arquivos acima gere o banco de dados de nome SIDRA_UF com as seguintes variáveis:
# 1  ANO    
# 2  NIVEL
# 3  CODMUNRES
# 4 POPRE_T
# 5 POPRC_T
# 6 POPRC_M
# 7 POPRC_F
# 8 POPRC_15
# 9 POPRC_15_49
# 10 POPRC_50
# 11 POPRC_F_15
# 12 POPRC_F_15_49
# 13 POPRC_F_50

library(tidyverse)

# 1. Base com a Estimativa 2015
SIDRA_PE <- pop_estimada_2015 %>%
  filter(grepl("^26", CODMUNRES) | CODMUNRES == 26) %>%
  select(CODMUNRES, POPRE_T)

# 2. União com Censo 2010 (Total e Sexo)
SIDRA_PE <- SIDRA_PE %>%
  left_join(
    pop_censo_total_sexo %>%
      filter(grepl("^26", CODMUNRES) | CODMUNRES == 26) %>%
      select(CODMUNRES, POPRC_T, POPRC_M, POPRC_F), 
    by = "CODMUNRES"
  )

# 3. Criando base de Idades para a UF (Código 26)
idades_uf <- pop_censo_faixa_uf %>%
  filter(CODMUNRES == 26) %>%
  group_by(CODMUNRES) %>%
  summarise(
    POPRC_15 = sum(POP[F_IDADE %in% c("0 a 4 anos", "5 a 9 anos", "10 a 14 anos")]),
    POPRC_15_49 = sum(POP[F_IDADE %in% c("15 a 19 anos", "20 a 24 anos", "25 a 29 anos", "30 a 34 anos", "35 a 39 anos", "40 a 44 anos", "45 a 49 anos")]),
    POPRC_50 = sum(POP[!F_IDADE %in% c("0 a 4 anos", "5 a 9 anos", "10 a 14 anos", "15 a 19 anos", "20 a 24 anos", "25 a 29 anos", "30 a 34 anos", "35 a 39 anos", "40 a 44 anos", "45 a 49 anos")]),
    POPRC_F_15 = sum(POPF[F_IDADE %in% c("0 a 4 anos", "5 a 9 anos", "10 a 14 anos")]),
    POPRC_F_15_49 = sum(POPF[F_IDADE %in% c("15 a 19 anos", "20 a 24 anos", "25 a 29 anos", "30 a 34 anos", "35 a 39 anos", "40 a 44 anos", "45 a 49 anos")]),
    POPRC_F_50 = sum(POPF[!F_IDADE %in% c("0 a 4 anos", "5 a 9 anos", "10 a 14 anos", "15 a 19 anos", "20 a 24 anos", "25 a 29 anos", "30 a 34 anos", "35 a 39 anos", "40 a 44 anos", "45 a 49 anos")])
  )

# 4. Criando base de Idades para os MUNICIPIOS
idades_mun <- pop_censo_faixa_sexo_mun %>%
  filter(grepl("^26", CODMUNRES)) %>%
  group_by(CODMUNRES) %>%
  summarise(
    POPRC_15 = sum(POP[F_IDADE %in% c("0 a 4 anos", "5 a 9 anos", "10 a 14 anos")]),
    POPRC_15_49 = sum(POP[F_IDADE %in% c("15 a 19 anos", "20 a 24 anos", "25 a 29 anos", "30 a 34 anos", "35 a 39 anos", "40 a 44 anos", "45 a 49 anos")]),
    POPRC_50 = sum(POP[!F_IDADE %in% c("0 a 4 anos", "5 a 9 anos", "10 a 14 anos", "15 a 19 anos", "20 a 24 anos", "25 a 29 anos", "30 a 34 anos", "35 a 39 anos", "40 a 44 anos", "45 a 49 anos")]),
    POPRC_F_15 = sum(POPF[F_IDADE %in% c("0 a 4 anos", "5 a 9 anos", "10 a 14 anos")]),
    POPRC_F_15_49 = sum(POPF[F_IDADE %in% c("15 a 19 anos", "20 a 24 anos", "25 a 29 anos", "30 a 34 anos", "35 a 39 anos", "40 a 44 anos", "45 a 49 anos")]),
    POPRC_F_50 = sum(POPF[!F_IDADE %in% c("0 a 4 anos", "5 a 9 anos", "10 a 14 anos", "15 a 19 anos", "20 a 24 anos", "25 a 29 anos", "30 a 34 anos", "35 a 39 anos", "40 a 44 anos", "45 a 49 anos")])
  )

# 5. Unindo tudo e finalizando
SIDRA_PE <- SIDRA_PE %>%
  left_join(bind_rows(idades_uf, idades_mun), by = "CODMUNRES") %>%
  mutate(
    ANO = 2015,
    NIVEL = ifelse(nchar(as.character(CODMUNRES)) == 2, "UF", "MUNICIPIO")
  ) %>%
  select(ANO, NIVEL, CODMUNRES, POPRE_T, POPRC_T, POPRC_M, POPRC_F, 
         POPRC_15, POPRC_15_49, POPRC_50, POPRC_F_15, POPRC_F_15_49, POPRC_F_50)

# Verificando se a primeira linha (UF) e as outras (MUNICIPIO) estão completas
head(SIDRA_PE)


# Exporte o arquivo em formato CSV

write.csv(SIDRA_PE, "SIDRA_PE.csv", row.names = FALSE)

# Faça o commit com a mensagem "Script e dados TAREFA 3 - SIDRA"

# Tarefa 2: Acesso aos bancos de dados do SINISA e obtenção da informação
# Escreva os comandos da Tarefa 2 estando na branch OUTROS# Leia o arquivo agua e esgoto - município - 2015.csv 
# A partir do arquivo acima gere o banco de dados de nome SINISA_UF com as seguintes variáveis:
# 1  ANO    
# 2  NIVEL
# 3  CODMUNRES
# 4 POPR_RA
# 5 POPR_RE

library(tidyverse)

# Lendo o arquivo indicando que o ponto (.) é separador de milhar
sinisa_original <- read_delim(
  "agua e esgoto - município - 2015.csv", 
  delim = ";", 
  locale = locale(decimal_mark = ",", grouping_mark = ".") 
)


SINISA_PE <- sinisa_original %>%
  filter(Estado == "PE") %>%
  select(
    ANO = `Ano de Referência`,
    CODMUNRES = CODMUNRES,
    POPR_RA = POPR_RA,
    POPR_RE = POPR_RE
  ) %>%
  mutate(
    NIVEL = "Município",
    POPR_RA = as.numeric(gsub("[^0-9]", "", POPR_RA)),
    POPR_RE = as.numeric(gsub("[^0-9]", "", POPR_RE)),
    CODMUNRES = as.character(CODMUNRES)
  ) %>%
  select(ANO, NIVEL, CODMUNRES, POPR_RA, POPR_RE)

# Exporte o arquivo em formato CSV
write_csv(SINISA_PE, "SINISA_PE.csv")

# Faça o commit com a mensagem "Script e dados TAREFA 3 - SINISA"

# Tarefa 3: Acesso aos bancos de dados do ATLAS  e obtenção da informação
# Escreva os comandos da Tarefa 3 estando na branch OUTROS
# Leia os arquivos:
# 1. códigos dos municípios - 2010.csv      
# 2. IDHM - 2010 (CENSO) e 2015 (PNAD) - total e por sexo - UF - Atlas Brasil.csv
# 3. IDHM - 2010 - municípios - Atlas Brasil.csv
# A partir do arquivo acima gere o banco de dados de nome ATLAS_UF com as seguintes variáveis:
# 1  ANO    
# 2  NIVEL
# 3  CODMUNRES
# 4 IDHM_A
# 5 IDHM_CA
# 6 IDHM_CA_M
# 7 IDHM_CA_F


library(tidyverse)

# 1. Lendo os arquivos
codigos_municipios <- read_csv2("códigos dos municípios - 2010.csv")
idhm_municipios    <- read_csv2("IDHM - 2010 - municípios - Atlas Brasil.csv")
idhm_uf            <- read_csv2("IDHM - 2010 (CENSO) e 2015 (PNAD) - total e por sexo - UF - Atlas Brasil.csv")

# FILTRANDO APENAS PERNAMBUCO EM CADA BANCO ANTES DE JUNTAR

# No banco do Atlas, pegamos apenas linhas que terminam com "(PE)"
idhm_municipios_pe <- idhm_municipios %>% 
  filter(str_ends(município, "\\(PE\\)")) %>%
  mutate(
    municipio_limpo = substr(município, 1, nchar(município) - 5),
    municipio_limpo = str_trim(municipio_limpo)
  )

# No banco de códigos, pegamos apenas os códigos que começam com "26" (Pernambuco)
codigos_municipios_pe <- codigos_municipios %>%
  filter(str_starts(as.character(CODMUNRES), "26")) %>%
  mutate(
    municipio_limpo = str_trim(município),
    CODMUNRES = as.character(CODMUNRES)
  )

# CRUZANDO OS DADOS 
atlas_municipios_com_codigo <- left_join(
  idhm_municipios_pe, 
  codigos_municipios_pe, 
  by = "municipio_limpo"
)

# CRIANDO A TABELA DOS MUNICÍPIOS DE PE SELECIONANDO AS VARIÁVEIS
dados_municipios <- atlas_municipios_com_codigo %>%
  mutate(
    ANO = 2010,
    NIVEL = "Município",
    CODMUNRES = as.character(CODMUNRES),
    IDHM_A = as.numeric(gsub(",", ".", IDHM_2010)),
    IDHM_CA = NA_real_,
    IDHM_CA_M = NA_real_,
    IDHM_CA_F = NA_real_
  ) %>%
  select(ANO, NIVEL, CODMUNRES, IDHM_A, IDHM_CA, IDHM_CA_M, IDHM_CA_F)

# CRIANDO A TABELA DA UF
dados_uf <- idhm_uf %>%
  filter(UF == "Pernambuco") %>%
  reframe(
    ANO = c(2010, 2015),
    NIVEL = "UF",
    CODMUNRES = "26",
    IDHM_A = as.numeric(gsub(",", ".", c(IDHM_2010, IDHM_2015))),
    IDHM_CA = NA_real_, 
    IDHM_CA_M = as.numeric(gsub(",", ".", c(IDHM_2010_M, IDHM_2015_M))),
    IDHM_CA_F = as.numeric(gsub(",", ".", c(IDHM_2010_F, IDHM_2015_F)))
  )

# JUNTANDO AS DUAS TABELAS
ATLAS_PE <- bind_rows(dados_municipios, dados_uf)

# Exporte o arquivo em formato CSV

write_csv(ATLAS_PE, "ATLAS_PE.csv")

# Faça o commit com a mensagem "Script e dados TAREFA 3 - ATLAS"

################################################################
# ETAPA 4: GERAR BANCO DE DADOS FINAL DO ESTADO COM DADOS DO SIDRA, ATLAS, SINASC, SIM, SINISA E INDICADORES

# Tarefa 1: Fazer o merge dos bancos de dados criados nas etapas anteriores (SIDRA_UF, ATLAS_ UF,  SINASC_UF, SIM_UF e SINISA_UF), sendo que as variáveis deverão seguir a ordem

# ANO, NIVEL, CODMUNRES (uma única vez), variáveis do SIDRA, do ATLAS, do SINASC, do SIM e da SINISA. No merge deve constar qualquer município que esteja em pelo menos um dos bancos
# Chamar o banco de dados de DA_UF

library(tidyverse)

SIDRA_PE  <- read_csv("SIDRA_PE.csv")
ATLAS_PE  <- read_csv("ATLAS_PE.csv")
SINASC_PE <- read_csv("SINASC_PE.csv")
SIM_PE    <- read_csv("SIM_PE.csv")
SINISA_PE <- read_csv("SINISA_PE.csv")

# Garantindo que a coluna CODMUNRES seja caractere em todos para evitar erros
SIDRA_PE  <- SIDRA_PE %>% mutate(CODMUNRES = as.character(CODMUNRES))
ATLAS_PE  <- ATLAS_PE %>% mutate(CODMUNRES = as.character(CODMUNRES))
SINASC_PE <- SINASC_PE %>% mutate(CODMUNRES = as.character(CODMUNRES))
SIM_PE    <- SIM_PE %>% mutate(CODMUNRES = as.character(CODMUNRES))
SINISA_PE <- SINISA_PE %>% mutate(CODMUNRES = as.character(CODMUNRES))

# 1. Juntando os dois primeiros bancos
DA_PE <- merge(SIDRA_PE, ATLAS_PE, by = c("ANO", "NIVEL", "CODMUNRES"), all = TRUE)

# 2. Juntando o resultado com o terceiro banco
DA_PE <- merge(DA_PE, SINASC_PE, by = c("ANO", "NIVEL", "CODMUNRES"), all = TRUE)

# 3. Juntando com o quarto banco
DA_PE <- merge(DA_PE, SIM_PE, by = c("ANO", "NIVEL", "CODMUNRES"), all = TRUE)

# 4. Juntando com o quinto banco e finaliza o grande merge de PE
DA_PE <- merge(DA_PE, SINISA_PE, by = c("ANO", "NIVEL", "CODMUNRES"), all = TRUE)

# Após o merge dos bancos, fazer commit “Script e dados agregados da UF”


# Tarefa 2: Acrescentar no banco DA_UF os indicadores TFG, TMG, RMM, TMM, TMM_P, TMN, TMN_P, TMN_T e TMI e chamar o banco de BDEM_UF_2015

# Após a criação do banco, fazer commit “Script e dados BDEM_UF_2015”

# Exporte o arquivo em formato CSV
# Faça o commit com a mensagem "Script e dados BDEM"



############################################################################################
# ETAPA 5: EMPILHAMENTO DOS DATAFRAMES DE CADA ESTADO, GERANDO UM DATAFRAME DE 27 LINHAS
############################################################################################
# Só inicie esta Etapa quando a professora orientar
# ESTANDO NA BRANCH SINASC, NÃO ALTERE NADA NO SCRIPT REFERENTE A ETAPA 5

# 1. Enviar arquivos para as pastas do repositório da Professora no GitHUb
# 2. A professora fará o empilhamentos dos dataframes

