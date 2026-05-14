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

library(tidyverse)
dados_sinasc<-read.csv(file.choose(),sep=";",header = TRUE,encoding = "latin1")
names(dados_sinasc)
#Verificando a estrutura
dim(dados_sinasc)
str(dados_sinasc)


# Tarefa 2. Reduzir dados_sinasc apenas para as colunas que serão utilizadas, nomeando este novo banco de dados como dados_sinasc_1
# as colunas serão 1, 4, 5, 6, 7, 12, 13, 14, 15, 19, 21, 22, 23, 24, 35, 38, 44, 46, 48, 59, 60, 61
# nomes das respectivas variáveis: CONTADOR, CODMUNNASC, LOCNASC, IDADEMAE, ESTCIVMAE, CODMUNRES, GESTACAO, GRAVIDEZ, PARTO,
# SEXO, APGAR5, RACACOR, PESO, IDANOMAL, ESCMAE2010, RACACORMAE, SEMAGESTAC, CONSPRENAT, TPAPRESENT, TPROBSON, PARIDADE, KOTELCHUCK

dados_sinasc_1 <- dados_sinasc %>%
  select(
    CONTADOR, CODMUNNASC, LOCNASC, IDADEMAE, ESTCIVMAE, 
    CODMUNRES, GESTACAO, GRAVIDEZ, PARTO, SEXO, 
    APGAR5, RACACOR, PESO, IDANOMAL, ESCMAE2010, 
    RACACORMAE, SEMAGESTAC, CONSPRENAT, TPAPRESENT, 
    TPROBSON, PARIDADE, KOTELCHUCK
  )

dim(dados_sinasc_1)

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


dados_sinasc_2 <- dados_sinasc_1 %>%
  filter(str_starts(as.character(CODMUNRES), "26"))
nrow(dados_sinasc_2)
write_csv(dados_sinasc_2, "dados_sinasc_2.csv")



# Tarefa 4. Verificar em dados_sinasc_2 a frequência das categorias das seguintes variáveis: LOCNASC, ESTCIVMAE, GESTACAO, GRAVIDEZ, PARTO,
# SEXO, APGAR5, RACACOR, IDANOMAL, ESCMAE2010, RACACORMAE, TPAPRESENT, TPROBSON, PARIDADE, KOTELCHUCK

variaveis_freq <- c("LOCNASC", "ESTCIVMAE", "GESTACAO", "GRAVIDEZ", "PARTO", 
                    "SEXO", "RACACOR", "IDANOMAL", "ESCMAE2010", 
                    "RACACORMAE", "TPAPRESENT", "TPROBSON", "PARIDADE", "KOTELCHUCK")
frequencias <- lapply(dados_sinasc_2[variaveis_freq], table)
frequencias

# Tarefa 5. Atribuir para cada variável de dados_sinasc_2 como sendo NA a categoria de "Não informado ou Ignorado", geralmente com código 9
# KOTELCHUCK = 9 significa "não informado"   TPROBSON = 11 significa "não classificado por falta de informação"
# veja o dicionário do SINASC para identificar qual o código das categorias de cada variável
# Em variáveis quantitativas como IDADEMAE, APGAR5 e PESO e SEMAGESTAC verificar se existem valores como 99 para NA
dados_sinasc_2 <- dados_sinasc_2 %>%
  mutate(
    across(c(LOCNASC, ESTCIVMAE, GESTACAO, GRAVIDEZ, PARTO, 
             RACACOR, IDANOMAL, ESCMAE2010, RACACORMAE, 
             TPAPRESENT, KOTELCHUCK), 
           ~ na_if(as.character(.), "9")),
    SEXO = na_if(as.numeric(SEXO), 0),
    TPROBSON = na_if(as.character(TPROBSON), "11"),
    IDADEMAE = na_if(as.numeric(IDADEMAE), 99),
    APGAR5 = na_if(as.numeric(APGAR5), 99),
    SEMAGESTAC = na_if(as.numeric(SEMAGESTAC), 99),
    PESO = na_if(as.numeric(PESO), 9999)
  )

summary(dados_sinasc_2$SEXO)
summary(dados_sinasc_2$APGAR5)
summary(dados_sinasc_2$PESO)

# Tarefa 6. Atribuir legendas para as categorias das variáveis investigadas na etapa 4.
# Exemplo: dados_sinasc_2$KOTELCHUCK = factor(dados_sinasc_2$KOTELCHUCK, levels = c(1,2,3,4,5), 
# labels = c("Não realizou pré-natal", "Inadequado", "Intermediário", "Adequado",  
# "Mais que adequado")

# ATENçÃO: 1. Na hora de escrever os labels, somente a primeira letra da palavra é maiúscula. Exemplo para SEXO: Feminino e Masculino
#          2. Nesta Tarefa 6 não crie novas variáveis no banco de dados
dados_sinasc_2$SEXO <- factor(dados_sinasc_2$SEXO, levels = c(1, 2), 
                              labels = c("Masculino", "Feminino"))

dados_sinasc_2$PARTO <- factor(dados_sinasc_2$PARTO, levels = c(1, 2), 
                               labels = c("Vaginal", "Cesário"))

dados_sinasc_2$GESTACAO <- factor(dados_sinasc_2$GESTACAO, levels = c(1, 2, 3, 4, 5, 6), 
                                  labels = c("Menos de 22 semanas", "22 a 27 semanas", 
                                             "28 a 31 semanas", "32 a 36 semanas", 
                                             "37 a 41 semanas", "42 semanas e mais"))

dados_sinasc_2$GRAVIDEZ <- factor(dados_sinasc_2$GRAVIDEZ, levels = c(1, 2, 3), 
                                  labels = c("Única", "Dupla", "Tripla ou mais"))

dados_sinasc_2$ESTCIVMAE <- factor(dados_sinasc_2$ESTCIVMAE, levels = c(1, 2, 3, 4, 5), 
                                   labels = c("Solteira", "Casada", "Viúva", 
                                              "Separada judicialmente/divorciada", "União estável"))

dados_sinasc_2$RACACOR <- factor(dados_sinasc_2$RACACOR, levels = c(1, 2, 3, 4, 5), 
                                 labels = c("Branca", "Preta", "Amarela", "Parda", "Indígena"))

dados_sinasc_2$IDANOMAL <- factor(dados_sinasc_2$IDANOMAL, levels = c(1, 2), 
                                  labels = c("Sim", "Não"))

dados_sinasc_2$KOTELCHUCK <- factor(dados_sinasc_2$KOTELCHUCK, levels = c(1, 2, 3, 4, 5), 
                                    labels = c("Não realizou pré-natal", "Inadequado", 
                                               "Intermediário", "Adequado", "Mais que adequado"))
dados_sinasc_2$TPROBSON <- factor(dados_sinasc_2$TPROBSON, 
                                  levels = 1:10, 
                                  labels = c("Grupo 1", "Grupo 2", "Grupo 3", "Grupo 4", "Grupo 5", 
                                             "Grupo 6", "Grupo 7", "Grupo 8", "Grupo 9", "Grupo 10"))
#Testando 
table(dados_sinasc_2$GESTACAO)
table(dados_sinasc_2$PARTO)
table(dados_sinasc_2$KOTELCHUCK)
table(dados_sinasc_2$TPROBSON)

# Tarefa 7. Categorizar as variáveis IDADEMAE, PESO e APGAR5 e criar variáveis referentes ao deslocamento materno (peregrinação) e estado civil
# nova variável: dados_sinasc_2$F_PESO com PESO: < 2500: Baixo peso, >=2500 e < 4000: Peso normal, >= 4000: Macrossomia
# nova variável dados_sinasc_2$F_IDADE com IDADEMAE: <15, 15-19, 20-24, 25-29, 30-34, 35-39, 40-44, 45-49, 50+
# nova variável dados_sinasc_2$F_APGAR5 com APGAR5: < 7: Baixo, >= 7: Normal
# Atenção para casos de NA em IDADEMAE, PESO e APGAR5
dados_sinasc_2$F_PESO <- cut(dados_sinasc_2$PESO, 
                             breaks = c(0, 2500, 4000, Inf), 
                             labels = c("Baixo peso", "Peso normal", "Macrossomia"),
                             right = FALSE)
dados_sinasc_2$F_IDADE <- cut(dados_sinasc_2$IDADEMAE, 
                              breaks = c(0, 15, 20, 25, 30, 35, 40, 45, 50, Inf), 
                              labels = c("<15", "15-19", "20-24", "25-29", "30-34", 
                                         "35-39", "40-44", "45-49", "50+"),
                              right= FALSE)

dados_sinasc_2$F_APGAR5 <- ifelse(dados_sinasc_2$APGAR5 < 7, "Baixo", "Normal")

dados_sinasc_2$F_APGAR5 <- factor(dados_sinasc_2$F_APGAR5, levels = c("Baixo", "Normal"))

table(dados_sinasc_2$F_APGAR5, useNA = "always")

# nova variável: dados_sinasc_2$PERIG: Não: CODMUNNASC igual a CODMUNRES, Sim: CODMUNNASC diferente de CODMUNRES
# nova variável: dados_sinasc_2$ESTCIV: Sem companheiro: ESTCIVMAE 1, 3 ou 4, Com companheiro: ESTCIVMAE 2 ou 5
# Ao categorizar as variáveis, garantir que sejam transformadas em tipo fator

dados_sinasc_2$PERIG <- ifelse(dados_sinasc_2$CODMUNNASC == dados_sinasc_2$CODMUNRES, "Não", "Sim")
dados_sinasc_2$PERIG <- factor(dados_sinasc_2$PERIG, levels = c("Não", "Sim"))



dados_sinasc_2$ESTCIV = ifelse(dados_sinasc_2$ESTCIVMAE %in% c("Solteira", "Viúva", "Separada judicialmente/divorciada"), "Sem companheiro",
                               ifelse(dados_sinasc_2$ESTCIVMAE %in% c("Casada", "União estável"), "Com companheiro", NA))
dados_sinasc_2$ESTCIV = factor(dados_sinasc_2$ESTCIV, levels = c("Sem companheiro","Com companheiro"))

saveRDS(dados_sinasc_2,"dados_sinasc_2.rds")



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
                                            ifelse(dados_sinasc_2$PESO <= dados_sinasc_2$PESO_P90, "AIG", "GIG"))))
dados_sinasc_2$F_PIG = factor(dados_sinasc_2$F_PIG, levels = c("PIG","AIG","GIG"))

# Tarefa 9. Obter as frequências das categorias das variáveis qualitativas e medidas descritivas de variáveis quantitativas e salvar os resultados em novas variáveis.
# Exemplo: freq_SEXO = table(dados_sinasc_2$SEXO)   media_peso = mean(dados_sinasc_2$PESO)
# Medidas descritivas a serem calculadas para variáveis QUANTITATIVAS: P25, P50, P75, média e desvio-padrão. Atenção: usar na.rm = TRUE, quando necessário.


# Tarefa 10. Criar as colunas do novo banco de dados (de nome SINASC_UF.csv Exemplo: SINASC_RJ.csv) com base nas análises prévias, devendo as variáveis estar na ordem indicada abaixo
# ATENÇÃO aos nomes das variáveis e ordem das colunas
# 1. ANO: 2015  2. UFR (Estado de residência)   3. TN (total de nascimentos)   4. TNRC (total de nascimentos com registros completos, ou seja, sem NA em todas as variáveis do banco de dados)
# 5. TGI_15 (total de gestantes com idade inferior a 15 anos - F_IDADE)   6. TGI_15_19 (total de gestantes com idade >=15 e <=19 anos)
# 7: TGI_20_24 (total de gestantes com idade >=20 e <=24 anos)   8. TGI_25_29 (total de gestantes com idade >=25 e <=29 anos)
# 9: TGI_30_34 (total de gestantes com idade >=30 e <=34 anos)   10. TGI_35_39 (total de gestantes com idade >=35 e <=39 anos)
# 11: TGI_40_44 (total de gestantes com idade >=40 e <=44 anos)  12. TGI_45_49 (total de gestantes com idade >=45 e <=49 anos)
# 13: TGI_50 (total de gestantes com idade >=50)   14: TGIF (total de gestantes em idade fértil, idade >=15 e <=49 anos)
# 15: IM_P25 (percentil 25 da idade materna - IDADEMAE) 16: IM_P50 (percentil 50 da idade materna)   17: IM_P75 (percentil 75 da idade materna)
# 18. IM_MD (idade média materna)   19: IM_DP (desvio-padrão da idade materna)
# 20. EM_S (total de gestantes sem escolaridade, ESCMAE2010=0)   21: EM_FI (total de gestantes com escolaridade Fundamental I)
# 22. EM_FII (total de gestantes com escolaridade Fundamental II)   23. EM_M (total de gestantes com escolaridade Médio)   
# 24. EM_SI (total de gestantes com escolaridade Superior Incompleto)   25. EM_SC (total de gestantes com escolaridade Superior Completo) 
# 26. TGRC_B (total de gestantes da raça/cor branca - RACACORMAE)   27. TGRC_PT (total de gestantes da raça/cor preta)
# 28. TGRC_A (total de gestantes da raça/cor amarela)   29. TGRC_PD (total de gestantes da raça/cor parda)
# 30. TGRC_I (total de gestantes da raça/cor indígena)
# 31. TGSC (total de gestantes sem companheiro - ESTCIV)   32. TGCC (total de gestantes com companheiro)
# 33. TGPRI (total de gestantes primíparas - PARIDADE)     34. TGNPRI (total de gestantes não primíparas)
# 35. TGU (total de gestações única)   36. TGG (total de gestações gemelares)   37. TGD_22 (total de gestações com duração inferior a 22 semanas - GESTACAO)
# 38. TGD_22_27 (total de gestações com duração da gestação >=22 e <=27)   39. TGD_28_31 (total de gestações com duração da gestação >=28 e <=31)
# 40. TGD_32_36 (total de gestações com duração da gestação >=32 e <=36)   41. TGD_37_41 (total de gestações com duração da gestação >=37 e <=41)
# 42. TGD_42 (total de gestações com duração da gestação >=42)   43. TGD_PRT (total de gestações pre-termo, duração < 37 semanas)
# 44. TGD_AT (total de gestações a termo, duração >=37 e <=41)   45. TGD_PST  (total de gestações pós termo, duração >=42) 
# 46. DG_P25 (percentil 25 da duração da gestação - SEMAGESTAC)  47. DG_P50 (percentil 50 da duração da gestação)   
# 48. DG_P75 (percentil 75 da duração da gestação)   49. DG_MD (idade média da duração da gestação)   50. DG_DP (desvio-padrão da duração da gestação)
# 51. TKC_NR (total de consultas de pre-natal não realizado - KOTELCHUCK)   52. TKC_ID (total de consultas de pre-natal inadequado)
# 53. TKC_IT (total de consultas de pre-natal intermediário)   54. TKC_AD (total de consultas de pre-natal adequado)  
# 55. TKC_MAD (total de consultas de pre-natal mais que adequado)   56. TGPRG_S (total de gestantes que peregrinaram)  
# 57. TGPRG_N (total de gestantes que não peregrinaram)    58. TPV (total de partos vaginais)   59. TPC (total de partos cesáreos) 
# 60. TRAP_C (total de recém-nascidos na posição cefálica - TPAPRESENT)   61. TRAP_P (total de recém-nascidos na posição pélvica ou podálica)
# 62. TRAP_T (total de recém-nascidos na posição transversa)  63. TGROB_1 (total de gestantes do grupo de Robson 1 - TPROBSON)
# 64. TGROB_2 (total de gestantes do grupo de Robson 2)   65. TGROB_3 (total de gestantes do grupo de Robson 3)
# 66. TGROB_4 (total de gestantes do grupo de Robson 4)   67. TGROB_5 (total de gestantes do grupo de Robson 5)
# 68. TGROB_6 (total de gestantes do grupo de Robson 6)   69. TGROB_7 (total de gestantes do grupo de Robson 7)
# 70. TGROB_8 (total de gestantes do grupo de Robson 8)   71. TGROB_9 (total de gestantes do grupo de Robson 9)
# 72. TGROB_10 (total de gestantes do grupo de Robson 10)   
# 73. TNLOC_H (total de nascimentos em hospital)   74. TNLOC_ES (total de nascimentos em outros estabelecimentos de saúde)
# 75. TNLOC_D (total de nascimentos em domicílio)  76. TNLOC_O (total de nascimentos em outros locais) 
# 77. TNLOC_AI (total de nascimentos em aldeias indígenas)   
# 78. TRRC_B (total de recém-nascidos da raça/cor branca - RACACOR)   79. TRRC_PT (total de recém-nascidos da raça/cor preta)
# 80. TRRC_A (total de recém-nascidos da raça/cor amarela)   81. TRRC_PD (total de recém-nascidos da raça/cor parda)
# 82. TRRC_I (total de recém-nascidos da raça/cor indígena)  83. TRP_BP (total de recém nascidos com baixo peso - FPESO)
# 84. TRP_N (total de recém nascidos com peso normal)   85. TRP_M (total de recém nascidos com macrossomia)
# 86. PESO_P25 (percentil 25 do peso dos recém-nascidos - PESO)  87. PESO_P50 (percentil 50 do peso dos recém-nascidos)   
# 88. PESO_P75 (percentil 75 do peso dos recém-nascidos)  89. PESO_MD (peso médio dos recém-nascidos)   
# 90. PESO_DP (desvio-padrão dos pesos dos recém-nascidos)    91. TRPIG_P (total de recém-nascidos de GESTAÇÕES ÚNICAS com PIG) 
# 92. TRPIG_A (total de recém-nascidos de GESTAÇÕES ÚNICAS com AIG)   93. TRPIG_G (total de recém-nascidos de GESTAÇÕES ÚNICAS com GIG)
# 94: TRAPG5_B (total de recém-nascidos com Apgar5 baixo, ou seja, < 7)
# 95: TRAPG5_N (total de recém-nascidos com Apgar5 normal, ou seja, >= 7)   96. APG5_MD (Apgar5 médio dos recém-nascidos)   
# 97. APG5_DP (desvio-padrão dos Apgar5 dos recém-nascidos)   98. TRAC (total de recém-nascidos com anomalia congênita - IDANOMAL)
# 99. TRSAC (total de recém-nascidos sem anomalia congênita)


# TAREFA 9 E 10 REFORMULADA PARA MUNICIPIOS DO ESTADO 

# Base inicial (municípios)
# Criando um dataframe com uma única coluna (CODMUNRES) com valores ordenados e sem repetição
base = data.frame(CODMUNRES =sort(unique(dados_sinasc_2$CODMUNRES)))


# TNRC - completos nas 61 variáveis
dados_UF = dados_sinasc[substr(as.character(dados_sinasc$CODMUNRES), 1, 2) == "26",]
dados_UF_comp = dados_UF[complete.cases(dados_UF), ]
TNRC = as.data.frame(table(factor(dados_UF_comp$CODMUNRES, levels = base$CODMUNRES)))
names(TNRC) = c("CODMUNRES","TNRC")
base = merge(base, TNRC, by = "CODMUNRES", all.x = TRUE)

# TNRCR - completos nas 22 variáveis
dados_UF_1 = dados_sinasc_1[substr(as.character(dados_sinasc_1$CODMUNRES), 1, 2) == "26",]
dados_UF_1_comp = dados_UF_1[complete.cases(dados_UF_1), ]
TNRCR = as.data.frame(table(factor(dados_UF_1_comp$CODMUNRES, levels = base$CODMUNRES)))
names(TNRCR) = c("CODMUNRES","TNRCR")
base = merge(base, TNRCR, by = "CODMUNRES", all.x = TRUE)

# Frequências de variáveis categóricas
#Sexo
tab = table(dados_sinasc_2$CODMUNRES, factor(dados_sinasc_2$SEXO, levels = c("Feminino",
                                                                             "Masculino")))
df = as.data.frame.matrix(tab)
names(df) = c("TRSEXO_F","TRSEXO_M")
df$CODMUNRES = rownames(df)
base = merge(base, df, by = "CODMUNRES", all.x = TRUE)

# Tipo de Parto
tab = table( dados_sinasc_2$CODMUNRES, factor(dados_sinasc_2$PARTO, levels = c("Vaginal",
                                                                               "Cesário")))
df = as.data.frame.matrix(tab)
names(df) = c("TPV","TPC")
df$CODMUNRES = rownames(df)
base = merge(base, df, by = "CODMUNRES", all.x = TRUE)

# Idade categorizada
tab = table(dados_sinasc_2$CODMUNRES, factor(dados_sinasc_2$F_IDADE, levels = c("<15","15-
19","20-24","25-29", "30-34","35-39","40-44","45-49","50+")))
df = as.data.frame.matrix(tab)
names(df) = c( "TGI_15","TGI_15_19","TGI_20_24","TGI_25_29", "TGI_30_34","TGI_35_39","TGI_40_44",
               "TGI_45_49","TGI_50")
df$CODMUNRES = rownames(df)
base = merge(base, df, by = "CODMUNRES", all.x = TRUE)

# Medidas descritivas
# Idade da mãe
# 1. Calculando a Média
media_idade <- aggregate(IDADEMAE ~ CODMUNRES, dados_sinasc_2, mean, na.rm = TRUE)
names(media_idade)[2] <- "IM_MD"

# 2. Calculando o Desvio Padrão
dp_idade <- aggregate(IDADEMAE ~ CODMUNRES, dados_sinasc_2, sd, na.rm = TRUE)
dp_idade$IDADEMAE <- round(dp_idade$IDADEMAE, 2)
names(dp_idade)[2] <- "IM_DP"

# 3. Fazendo o merge
# Verificando se media_idade existe antes de rodar esta linha
temp <- merge(media_idade, dp_idade, by = "CODMUNRES")

# Escolaridade da Mãe (EM_S, EM_FI, EM_FII, EM_M, EM_SI, EM_SC)
tab_esc = table(dados_sinasc_2$CODMUNRES, factor(dados_sinasc_2$ESCMAE2010, levels = 1:5))
df_esc = as.data.frame.matrix(tab_esc)
names(df_esc) = c("EM_S", "EM_FI", "EM_FII", "EM_M", "EM_SI")
df_esc$CODMUNRES = rownames(df_esc)
base = merge(base, df_esc, by = "CODMUNRES", all.x = TRUE)

tab = table(dados_sinasc_2$CODMUNRES, factor(dados_sinasc_2$RACACOR, levels = c(1, 2, 4, 3, 5)))
df = as.data.frame.matrix(tab)
names(df) = c("TGRC_B", "TGRC_PT", "TGRC_PD", "TGRC_A", "TGRC_I")
df$CODMUNRES = rownames(df)
base = merge(base, df, by = "CODMUNRES", all.x = TRUE)

# Estado Civil: Solteiras (Código 1)
# Criamos a contagem para a sigla TGSC
base$TGSC <- as.vector(table(factor(dados_sinasc_2$CODMUNRES[dados_sinasc_2$ESTCIVMAE == 1], levels = base$CODMUNRES)))

# Estado Civil: Casadas ou União Consensual (Códigos 2 e 5)
# Criamos a contagem para a sigla TGCC
base$TGCC <- as.vector(table(factor(dados_sinasc_2$CODMUNRES[dados_sinasc_2$ESTCIVMAE %in% c(2, 5)], levels = base$CODMUNRES)))

# Gestações Únicas (TGU)
base$TGU <- as.vector(table(factor(dados_sinasc_2$CODMUNRES[dados_sinasc_2$GRAVIDEZ == 1], levels = base$CODMUNRES)))

# Gestações Múltiplas (TGG)
base$TGG <- as.vector(table(factor(dados_sinasc_2$CODMUNRES[dados_sinasc_2$GRAVIDEZ > 1], levels = base$CODMUNRES)))

#Duração ga gestação
tab = table(dados_sinasc_2$CODMUNRES, factor(dados_sinasc_2$GESTACAO, levels = 1:6))
df = as.data.frame.matrix(tab)
names(df) = c("TGD_22", "TGD_22_27", "TGD_28_31", "TGD_32_36", "TGD_37_41", "TGD_42")
df$CODMUNRES = rownames(df)

# Garantir que a duração seja numérica
dados_sinasc_2$SEMAGESTAC <- as.numeric(as.character(dados_sinasc_2$SEMAGESTAC))

# Contagens de Gestações (Pré-termo, A termo, Pós-termo)
base$TGD_PRT <- as.vector(table(factor(dados_sinasc_2$CODMUNRES[dados_sinasc_2$SEMAGESTAC < 37], levels = base$CODMUNRES)))
base$TGD_AT  <- as.vector(table(factor(dados_sinasc_2$CODMUNRES[dados_sinasc_2$SEMAGESTAC >= 37 & dados_sinasc_2$SEMAGESTAC <= 41], levels = base$CODMUNRES)))
base$TGD_PST <- as.vector(table(factor(dados_sinasc_2$CODMUNRES[dados_sinasc_2$SEMAGESTAC >= 42], levels = base$CODMUNRES)))

# Estatísticas de Duração (Percentis, Média e Desvio)
stats_gest <- aggregate(SEMAGESTAC ~ CODMUNRES, data = dados_sinasc_2, function(x) {
  c(P25 = quantile(x, 0.25, na.rm = TRUE), 
    P50 = median(x, na.rm = TRUE), 
    P75 = quantile(x, 0.75, na.rm = TRUE),
    MD = mean(x, na.rm = TRUE),
    DP = sd(x, na.rm = TRUE))
})
stats_gest <- do.call(data.frame, stats_gest)
names(stats_gest) <- c("CODMUNRES", "DG_P25", "DG_P50", "DG_P75", "DG_MD", "DG_DP")
base <- merge(base, stats_gest, by = "CODMUNRES", all.x = TRUE)

#Categorias de Kotelchuck
# 1: Não realizado | 2: Inadequado | 3: Intermediário | 4: Adequado | 5: Mais que adequado (se houver)
tab_kc <- table(dados_sinasc_2$CODMUNRES, factor(dados_sinasc_2$KOTELCHUCK, levels = 1:5))
df_kc <- as.data.frame.matrix(tab_kc)
names(df_kc) <- c("TKC_NR", "TKC_ID", "TKC_IT", "TKC_AD", "TKC_MAD")
df_kc$CODMUNRES <- rownames(df_kc)
base <- merge(base, df_kc, by = "CODMUNRES", all.x = TRUE)

# Perigrinação
dados_sinasc_2$PEREGRINA <- ifelse(dados_sinasc_2$CODMUNNASC != dados_sinasc_2$CODMUNRES, "S", "N")

# Total de gestantes que peregrinaram (TGPRG_S)
base$TGPRG_S <- as.vector(table(factor(dados_sinasc_2$CODMUNRES[dados_sinasc_2$PEREGRINA == "S"], levels = base$CODMUNRES)))

#Total de gestantes que não peregrinaram (TGPRG_N)
base$TGPRG_N <- as.vector(table(factor(dados_sinasc_2$CODMUNRES[dados_sinasc_2$PEREGRINA == "N"], levels = base$CODMUNRES)))

#Total de partos vaginais (TPV)
base$TPV <- as.vector(table(factor(dados_sinasc_2$CODMUNRES[dados_sinasc_2$PARTO == 1], levels = base$CODMUNRES)))

# Total de partos cesáreos (TPC)
base$TPC <- as.vector(table(factor(dados_sinasc_2$CODMUNRES[dados_sinasc_2$PARTO == 2], levels = base$CODMUNRES)))

# Apresentação (Cefálica, Pélvica, Transversa)
tab = table(dados_sinasc_2$CODMUNRES, factor(dados_sinasc_2$TPAPRESENT, levels = 1:3))
df = as.data.frame.matrix(tab)
names(df) = c("TRAP_C", "TRAP_P", "TRAP_T")
df$CODMUNRES = rownames(df)
base = merge(base, df, by = "CODMUNRES", all.x = TRUE)

# Grupos de Robson (1 a 10)
tab = table(dados_sinasc_2$CODMUNRES, factor(dados_sinasc_2$TPROBSON, levels = 1:10))
df = as.data.frame.matrix(tab)
names(df) = paste0("TGROB_", 1:10)
df$CODMUNRES = rownames(df)
base = merge(base, df, by = "CODMUNRES", all.x = TRUE)
base = merge(base, df, by = "CODMUNRES", all.x = TRUE)

# Local (Hospital, Outros Estab., Domicílio, Outros, Aldeia)
tab = table(dados_sinasc_2$CODMUNRES, factor(dados_sinasc_2$LOCNASC, levels = 1:5))
df = as.data.frame.matrix(tab)
names(df) = c("TNLOC_H", "TNLOC_ES", "TNLOC_D", "TNLOC_O", "TNLOC_AI")
df$CODMUNRES = rownames(df)
base = merge(base, df, by = "CODMUNRES", all.x = TRUE)
base = merge(base, df, by = "CODMUNRES", all.x = TRUE)

# Masculino (TRS_M)
base$TRS_M <- as.vector(table(factor(dados_sinasc_2$CODMUNRES[dados_sinasc_2$SEXO %in% c(1, "M", "Masculino")], levels = base$CODMUNRES)))

# Feminino (TRS_F)
base$TRS_F <- as.vector(table(factor(dados_sinasc_2$CODMUNRES[dados_sinasc_2$SEXO %in% c(2, "F", "Feminino")], levels = base$CODMUNRES)))

# Raça/Cor
base$TRRC_B  <- as.vector(table(factor(dados_sinasc_2$CODMUNRES[dados_sinasc_2$RACACOR == 1], levels = base$CODMUNRES)))
base$TRRC_PT <- as.vector(table(factor(dados_sinasc_2$CODMUNRES[dados_sinasc_2$RACACOR == 2], levels = base$CODMUNRES)))
base$TRRC_A  <- as.vector(table(factor(dados_sinasc_2$CODMUNRES[dados_sinasc_2$RACACOR == 3], levels = base$CODMUNRES)))
base$TRRC_PD <- as.vector(table(factor(dados_sinasc_2$CODMUNRES[dados_sinasc_2$RACACOR == 4], levels = base$CODMUNRES)))
base$TRRC_I  <- as.vector(table(factor(dados_sinasc_2$CODMUNRES[dados_sinasc_2$RACACOR == 5], levels = base$CODMUNRES)))

# Garantir que o peso é numérico
dados_sinasc_2$PESO <- as.numeric(as.character(dados_sinasc_2$PESO))

# Baixo Peso (TRP_BP: < 2500g)
base$TRP_BP <- as.vector(table(factor(dados_sinasc_2$CODMUNRES[dados_sinasc_2$PESO < 2500], levels = base$CODMUNRES)))

# Peso Normal (TRP_N: 2500g a 3999g)
base$TRP_N  <- as.vector(table(factor(dados_sinasc_2$CODMUNRES[dados_sinasc_2$PESO >= 2500 & dados_sinasc_2$PESO < 4000], levels = base$CODMUNRES)))

# Macrossomia (TRP_M: >= 4000g)
base$TRP_M  <- as.vector(table(factor(dados_sinasc_2$CODMUNRES[dados_sinasc_2$PESO >= 4000], levels = base$CODMUNRES)))

# Calculando Percentis, Média e Desvio Padrão do Peso
stats_peso <- aggregate(as.numeric(as.character(PESO)) ~ CODMUNRES, data = dados_sinasc_2, function(x) {
  c(P25 = quantile(x, 0.25, na.rm = TRUE), 
    P50 = median(x, na.rm = TRUE), 
    P75 = quantile(x, 0.75, na.rm = TRUE),
    MD = mean(x, na.rm = TRUE),
    DP = sd(x, na.rm = TRUE))
})
stats_peso <- do.call(data.frame, stats_peso)
names(stats_peso) <- c("CODMUNRES", "PESO_P25", "PESO_P50", "PESO_P75", "PESO_MD", "PESO_DP")
base <- merge(base, stats_peso, by = "CODMUNRES", all.x = TRUE)

# Filtrando apenas gestações únicas para as colunas TRPIG
gest_unicas <- dados_sinasc_2[dados_sinasc_2$GRAVIDEZ == 1, ]

base$TRPIG_P <- as.vector(table(factor(gest_unicas$CODMUNRES[gest_unicas$F_PIG == "PIG"], levels = base$CODMUNRES)))
base$TRPIG_A <- as.vector(table(factor(gest_unicas$CODMUNRES[gest_unicas$F_PIG == "AIG"], levels = base$CODMUNRES)))
base$TRPIG_G <- as.vector(table(factor(gest_unicas$CODMUNRES[gest_unicas$F_PIG == "GIG"], levels = base$CODMUNRES)))

# Contagens de Apgar5
base$TRAPG5_B <- as.vector(table(factor(dados_sinasc_2$CODMUNRES[as.numeric(as.character(dados_sinasc_2$APGAR5)) < 7], levels = base$CODMUNRES)))
base$TRAPG5_N <- as.vector(table(factor(dados_sinasc_2$CODMUNRES[as.numeric(as.character(dados_sinasc_2$APGAR5)) >= 7], levels = base$CODMUNRES)))

# Média e Desvio Padrão de Apgar5
stats_apg <- aggregate(as.numeric(as.character(APGAR5)) ~ CODMUNRES, data = dados_sinasc_2, function(x) {
  c(MD = mean(x, na.rm = TRUE), DP = sd(x, na.rm = TRUE))
})
stats_apg <- do.call(data.frame, stats_apg)
names(stats_apg) <- c("CODMUNRES", "APG5_MD", "APG5_DP")
base <- merge(base, stats_apg, by = "CODMUNRES", all.x = TRUE)

# Com anomalia (TRAC)
base$TRAC <- as.vector(table(factor(dados_sinasc_2$CODMUNRES[dados_sinasc_2$IDANOMAL == 1], levels = base$CODMUNRES)))

# Sem anomalia (TRSAC)
base$TRSAC <- as.vector(table(factor(dados_sinasc_2$CODMUNRES[dados_sinasc_2$IDANOMAL == 2], levels = base$CODMUNRES)))

# Lista completa de colunas que NÃO podem ser zeradas (estatísticas)
cols_protegidas = c("CODMUNRES", "IM_MD", "IM_DP", "IM_P25", "IM_P50", "IM_P75", 
                    "DG_P25", "DG_P50", "DG_P75", "DG_MD", "DG_DP", 
                    "PESO_P25", "PESO_P50", "PESO_P75", "PESO_MD", "PESO_DP",
                    "APG5_MD", "APG5_DP")

cols_contagem = setdiff(names(base), cols_protegidas)
base[cols_contagem][is.na(base[cols_contagem])] = 0


# Inserindo agora linha da UF
linha_estado = base[1, ]
linha_estado[,] = NA

# colunas de contagem:
cols_contagem = setdiff( names(base), c("CODMUNRES","IM_MD","IM_DP","IM_P25",
                                        "IM_P50","IM_P75"))
linha_estado[cols_contagem] = colSums(base[cols_contagem], na.rm = TRUE)

# medidas contínuas (idade da mãe)
consolidado_PE$IM_MD = round(mean(dados_sinasc_2$IDADEMAE, na.rm = TRUE), 2)
consolidado_PE$IM_DP = round(sd(dados_sinasc_2$IDADEMAE, na.rm = TRUE), 2)
consolidado_PE$IM_MD.y <- NULL
consolidado_PE$IM_DP.y <- NULL

# Cálculando os quartis
q = round(quantile(dados_sinasc_2$IDADEMAE, probs = c(0.25, 0.5, 0.75), na.rm = TRUE), 2)

consolidado_PE$IM_P25 = q[1]
consolidado_PE$IM_P50 = q[2]
consolidado_PE$IM_P75 = q[3]

# Código da UF considerando o estado do Pernambuco
# Definindo o codigo do estado
linha_estado$CODMUNRES = 26 

# Unindo a linha do estado com a base dos municípios
SINASC_PE = rbind(linha_estado, base)

# Criando a coluna NIVEL 
SINASC_PE$NIVEL = c("UF", rep("MUNICIPIO", nrow(SINASC_PE) - 1))


# Adicionando a coluna de ANO
SINASC_PE$ANO = 2015

# Reorganizando as colunas colocando ANO, NIVEL e CODMUNRES na frente
SINASC_PE = SINASC_PE[, c("ANO", "NIVEL", "CODMUNRES", names(SINASC_PE)[!names(SINASC_PE) %in% c("ANO", "NIVEL", "CODMUNRES")])]



# Tarefa 11: Exporte o banco de dados com o nome SINASC_UF.csv

write.csv(SINASC_PE, "SINASC_PE.csv", row.names = FALSE)


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

library(tidyverse)
dados_sim<-read.csv(file.choose(),sep=";",header = TRUE,encoding = "latin1")
names(dados_sim)
#Verificando a estrutura
dim(dados_sim)
str(dados_sim)

# Tarefa 2. Reduzir dados_sim apenas para as colunas que serão utilizadas, nomeando este novo banco de dados como dados_sim_1
# as colunas serão: 1, 3, 4, 8, 9, 10, 11, 14, 17, 35, 36, 37, 47, 77, 84
# nomes das respectivas variáveis: CONTADOR, TIPOBITO, DTOBITO, DTNASC, IDADE, SEXO, RACACOR, ESC2010, CODMUNRES, TPMORTEOCO, 
# OBITOGRAV, OBITOPUERP, CAUSABAS, TPOBITOCOR, MORTEPARTO

dados_sim_1 <- dados_sim %>%
  select(
    CONTADOR, TIPOBITO, DTOBITO, DTNASC, IDADE, SEXO, RACACOR, ESC2010, CODMUNRES, TPMORTEOCO, 
    OBITOGRAV, OBITOPUERP, CAUSABAS, TPOBITOCOR, MORTEPARTO)

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

dados_sim_2 <- dados_sim_1 %>%
  filter(str_starts(as.character(CODMUNRES), "26"))
nrow(dados_sim_2)
write_csv(dados_sim_2, "dados_sim_2.csv")

# Ao concluir a Tarefa 3 da Etapa 2 commite e envie para o repositório REMOTO o script e dados_sim_2.csv com o comentário "Dados do estado UF (coloque o nome da UF) e script de sua obtenção"


# Tarefa 4. Verificar em dados_sim_2 a frequência das categorias das seguintes variáveis: TIPOBITO, SEXO, RACACOR, 
# TPMORTEOCO, OBITOGRAV, OBITOPUERP, CAUSABAS, TPOBITOCOR, MORTEPARTO
# 1. Definir o vetor de variáveis corretamente (com todas as aspas e vírgulas)
variaveis_freq <- c("TIPOBITO", "SEXO", "RACACOR", "TPMORTEOCO", 
                    "OBITOGRAV", "OBITOPUERP", "CAUSABAS", 
                    "TPOBITOCOR", "MORTEPARTO")
frequencias <- lapply(dados_sim_2[variaveis_freq], table)
frequencias


# Tarefa 5. Atribuir para cada variável de dados_sim_2 como sendo NA a categoria de "Não informado ou Ignorado", geralmente com código 9
# veja o dicionário do SIM para identificar qual o código das categorias de cada variável
# Em variáveis quantitativas como IDADE verificar se existem valores como 99 para NA

# 1. Variáveis onde o código 9 é Ignorado (conforme o dicionário)
vars_para_limpar <- c("ESC2010", "ESC", "TIPOBITO", "TPMORTEOCO", 
                      "OBITOGRAV", "OBITOPUERP", "TPOBITOCOR", "MORTEPARTO")

for (var in vars_para_limpar) {
  # O código 9 é o padrão para "Ignorado" nestas variáveis
  dados_sim_2[[var]][dados_sim_2[[var]] == 9] <- NA
}

# 2. Tratamento do SEXO (Dicionário aponta 0 e 9 como ignorado)
dados_sim_2$SEXO[dados_sim_2$SEXO %in% c(0, 9)] <- NA

# 3. Tratamento da IDADE
# Conforme o dicionário, se o primeiro dígito for 9, é ignorado
dados_sim_2$IDADE[substr(dados_sim_2$IDADE, 1, 1) == "9"] <- NA

# Tarefa 6. Atribuir legendas para as categorias das variáveis qualitativas investigadas na tarefa 4.
# Exemplo: dados_sim_2$TIPOBITO = factor(dados_sim_2$TIPOBITO, levels = c(1,2), 
# labels = c("Fetal", "Não fetal")

# ATENçÃO: 1. Na hora de escrever os labels, somente a primeira letra da palavra é maiúscula. Exemplo para SEXO: Feminino e Masculino
#          2. Nesta Tarefa 6 não crie novas variáveis no banco de dados

# 1. Tipo do óbito (TIPOBITO)
dados_sim_2$TIPOBITO <- factor(dados_sim_2$TIPOBITO, levels = c(1, 2), 
                               labels = c("Fetal", "Não fetal"))

# 2. Sexo (SEXO)
# O dicionário cita M/1 para Masculino e F/2 para Feminino[cite: 1]
# Como você já limpou os ignorados (0 e 9), usamos os códigos 1 e 2.
dados_sim_2$SEXO <- factor(dados_sim_2$SEXO, levels = c(1, 2), 
                           labels = c("Masculino", "Feminino"))

# 3. Raça Cor (RACACOR)[cite: 1]
dados_sim_2$RACACOR <- factor(dados_sim_2$RACACOR, levels = c(1, 2, 3, 4, 5), 
                              labels = c("Branca", "Preta", "Amarela", "Parda", "Indígena"))

# 4. A morte ocorreu (TPMORTEOCO)[cite: 1]
dados_sim_2$TPMORTEOCO <- factor(dados_sim_2$TPMORTEOCO, levels = c(1, 2, 3, 4, 5, 8), 
                                 labels = c("Na gravidez", "No parto", "No abortamento", 
                                            "Até 42 dias após o parto", 
                                            "De 43 dias a 1 ano após gestação", "Não ocorreu nestes períodos"))

# 5. Óbito na gravidez (OBITOGRAV)[cite: 1]
dados_sim_2$OBITOGRAV <- factor(dados_sim_2$OBITOGRAV, levels = c(1, 2), 
                                labels = c("Sim", "Não"))

# 6. Óbito no puerpério (OBITOPUERP)[cite: 1]
dados_sim_2$OBITOPUERP <- factor(dados_sim_2$OBITOPUERP, levels = c(1, 2, 3), 
                                 labels = c("Sim, até 42 dias após o parto", 
                                            "Sim, de 43 dias a 1 ano", "Não"))

# 7. Tipo de local de ocorrência (TPOBITOCOR)[cite: 1]
dados_sim_2$TPOBITOCOR <- factor(dados_sim_2$TPOBITOCOR, levels = c(1, 2, 3, 4, 5), 
                                 labels = c("Via pública", "Endereço de residência", 
                                            "Outro domicílio", "Estabelecimento comercial", "Outros"))

# 8. Momento do óbito em relação ao parto (MORTEPARTO)[cite: 1]
dados_sim_2$MORTEPARTO <- factor(dados_sim_2$MORTEPARTO, levels = c(1, 2, 3), 
                                 labels = c("Antes", "Durante", "Após"))


# Tarefa 7. Crie um banco de dados, de nome SIM_UF.csv (Exemplo: SIM_RJ.csv), contendo as 41 variáveis listadas no arquivo “Variáveis - Projeto - Tarefa 7 da Etapa 2.pdf”
# Atenção:
# 1. Para informações gerais utilize CAUSABAS, SEXO e IDADE
# 2. Para informações fetais utilize TIPOBITO
# 3. Para informações neonatais utilize TIPOBITO não fetal e IDADE entre 0 e 27 dias e RACACOR
# 4. Para informações maternas utilize TPMORTEOCO, ESC e IDADE


# --- 1. Preparação dos Dados (Auxiliares) ---
# Extração da inicial da causa para filtros de mortalidade
dados_sim_2$inicial_causa <- substr(dados_sim_2$CAUSABAS, 1, 1)

# Conversão da idade para dias para filtros neonatais[cite: 1, 2]
obter_dias <- function(idade_vetor) {
  unidade <- as.numeric(substr(idade_vetor, 1, 1))
  valor <- as.numeric(substr(idade_vetor, 2, 3))
  dias <- ifelse(unidade <= 2, 0, 
                 ifelse(unidade == 3, valor, 
                        ifelse(unidade == 4, valor * 365, NA)))
  return(dias)
}
dados_sim_2$idade_em_dias <- obter_dias(dados_sim_2$IDADE)

# Identificação de Idade Fértil (15-49 anos)
dados_sim_2$idade_fertil <- with(dados_sim_2, 
                                 as.numeric(substr(IDADE, 1, 1)) == 4 & 
                                   as.numeric(substr(IDADE, 2, 3)) >= 15 & 
                                   as.numeric(substr(IDADE, 2, 3)) <= 49
)

# --- 2. Processamento por Município ---
library(dplyr)

# 1. Criar coluna auxiliar de casos completos antes do agrupamento para evitar o erro
dados_sim_2$completo_total <- complete.cases(dados_sim_2)
dados_sim_2$completo_selecionadas <- complete.cases(dados_sim_2[, 1:14])

# 2. Rodar o agrupamento revisado
library(dplyr)

SIM_MUN <- dados_sim_2 %>%
  group_by(CODMUNRES) %>%
  summarise(
    # Identificadores (Colunas 1-3)
    ANO = 2015,
    NIVEL = "MUNICIPIO",
    
    # Informações Gerais (Colunas 4-16)
    TO = n(),
    TORC = sum(completo_total, na.rm = TRUE),
    TORCR = sum(completo_selecionadas, na.rm = TRUE),
    TO_NN = sum(inicial_causa %in% c("V", "W", "X", "Y"), na.rm = TRUE),
    TO_N = sum(!inicial_causa %in% c("V", "W", "X", "Y"), na.rm = TRUE),
    TO_CBI = sum(inicial_causa %in% c("A", "B"), na.rm = TRUE),
    TO_CB_N = sum(inicial_causa %in% c("C", "D"), na.rm = TRUE),
    TO_CB_C = sum(inicial_causa == "I", na.rm = TRUE),
    TO_CB_R = sum(inicial_causa == "J", na.rm = TRUE),
    TO_CB_O = sum(!inicial_causa %in% c("A", "B", "C", "D", "I", "J", "V", "W", "X", "Y"), na.rm = TRUE),
    TO_M = sum(SEXO == "Masculino", na.rm = TRUE),
    TO_F = sum(SEXO == "Feminino", na.rm = TRUE),
    TO_F_IF = sum(SEXO == "Feminino" & idade_fertil, na.rm = TRUE),
    
    # Informações Fetais e Neonatais (Colunas 17-27)
    TO_FT = sum(TIPOBITO == "Fetal", na.rm = TRUE),
    TO_NT = sum(idade_em_dias >= 0 & idade_em_dias <= 27, na.rm = TRUE),
    TO_NT_P = sum(idade_em_dias >= 0 & idade_em_dias <= 6, na.rm = TRUE),
    TO_NT_T = sum(idade_em_dias >= 7 & idade_em_dias <= 27, na.rm = TRUE),
    TO_PNT = sum(idade_em_dias >= 28 & idade_em_dias <= 364, na.rm = TRUE),
    TO_MT_G = sum(TPMORTEOCO == "Na gravidez", na.rm = TRUE),
    TONT_B = sum(idade_em_dias <= 27 & RACACOR == "Branca", na.rm = TRUE),
    TONT_PT = sum(idade_em_dias <= 27 & RACACOR == "Preta", na.rm = TRUE),
    TONT_A = sum(idade_em_dias <= 27 & RACACOR == "Amarela", na.rm = TRUE),
    TONT_PD = sum(idade_em_dias <= 27 & RACACOR == "Parda", na.rm = TRUE),
    TONT_I = sum(idade_em_dias <= 27 & RACACOR == "Indígena", na.rm = TRUE),
    
    # Informações Maternas (Colunas 28-41)
    TO_MT = sum(!is.na(OBITOGRAV) | !is.na(OBITOPUERP), na.rm = TRUE),
    TO_MT_DG = sum(TPMORTEOCO == "Na gravidez", na.rm = TRUE),
    TO_MT_PT = sum(TPMORTEOCO == "No parto", na.rm = TRUE),
    TO_MT_AB = sum(TPMORTEOCO == "No abortamento", na.rm = TRUE),
    TO_MT_42 = sum(OBITOPUERP == "Sim, até 42 dias após o parto", na.rm = TRUE),
    TO_MT_43 = sum(OBITOPUERP == "Sim, de 43 dias a 1 ano", na.rm = TRUE),
    TO_MT_P = sum(TPMORTEOCO %in% c("Na gravidez", "No parto", "No abortamento") | 
                    OBITOPUERP == "Sim, até 42 dias após o parto", na.rm = TRUE),
    TO_MT_P_IF = sum((TPMORTEOCO %in% c("Na gravidez", "No parto", "No abortamento") | 
                        OBITOPUERP == "Sim, até 42 dias após o parto") & idade_fertil, na.rm = TRUE),
    TO_MT_P_ES = sum((TPMORTEOCO %in% c("Na gravidez", "No parto", "No abortamento") | 
                        OBITOPUERP == "Sim, até 42 dias após o parto") & ESC2010 == 0, na.rm = TRUE),
    TO_MT_P_EFI = sum((TPMORTEOCO %in% c("Na gravidez", "No parto", "No abortamento") | 
                         OBITOPUERP == "Sim, até 42 dias após o parto") & ESC2010 == 1, na.rm = TRUE),
    TO_MT_P_EFII = sum((TPMORTEOCO %in% c("Na gravidez", "No parto", "No abortamento") | 
                          OBITOPUERP == "Sim, até 42 dias após o parto") & ESC2010 == 2, na.rm = TRUE),
    TO_MT_P_EM = sum((TPMORTEOCO %in% c("Na gravidez", "No parto", "No abortamento") | 
                        OBITOPUERP == "Sim, até 42 dias após o parto") & ESC2010 == 3, na.rm = TRUE),
    TO_MT_P_ESI = sum((TPMORTEOCO %in% c("Na gravidez", "No parto", "No abortamento") | 
                         OBITOPUERP == "Sim, até 42 dias após o parto") & ESC2010 == 4, na.rm = TRUE),
    TO_MT_P_ESC = sum((TPMORTEOCO %in% c("Na gravidez", "No parto", "No abortamento") | 
                         OBITOPUERP == "Sim, até 42 dias após o parto") & ESC2010 == 5, na.rm = TRUE)
  )
# Reorganizar e exportar[cite: 2]
SIM_MUN <- SIM_MUN %>% select(ANO, NIVEL, CODMUNRES, everything())
write.csv(SIM_MUN, "SIM_PE_.csv", row.names = FALSE)


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



# Exporte o arquivo em formato CSV
# Faça o commit com a mensagem "Script e dados TAREFA 3 - SIDRA"

#3 - Faça um commit em main com a mensagem "Script com orientações ETAPA 3 - SIDRA"



#####################################################################################################
# ETAPA 4: GERAR BANCO DE DADOS FINAL DO ESTADO, BASEADO NAS ANÁLISES DE SINASC, SIM, IBGE, SNIS,...
######################################################################################################
# Só inicie esta Etapa quando a professora orientar
# ESTANDO NA BRANCH SINASC, NÃO ALTERE NADA NO SCRIPT REFERENTE A ETAPA 4

# Cada aluno gerar um dataframe de uma única linha (referente ao seu estado) com as variáveis na ordem indicada pela professora



############################################################################################
# ETAPA 5: EMPILHAMENTO DOS DATAFRAMES DE CADA ESTADO, GERANDO UM DATAFRAME DE 27 LINHAS
############################################################################################
# Só inicie esta Etapa quando a professora orientar
# ESTANDO NA BRANCH SINASC, NÃO ALTERE NADA NO SCRIPT REFERENTE A ETAPA 5

# 1. Enviar arquivos para as pastas do repositório da Professora no GitHUb
# 2. A professora fará o empilhamentos dos dataframes

