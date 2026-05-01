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


dados_sinasc_2$ESTCIVMAE <- factor(dados_sinasc_2$ESTCIVMAE, 
                                   levels = c(1, 2, 3, 4, 5),  
                                   labels = c("Solteira", "Casada", "Viúva",  
                                              "Separada judicialmente/divorciada", "União estável"))

dados_sinasc_2$ESTCIV <- ifelse(dados_sinasc_2$ESTCIVMAE %in% c("Solteira", "Viúva", "Separada judicialmente/divorciada"), 
                                "Sem companheiro", "Com companheiro")

dados_sinasc_2$ESTCIV <- factor(dados_sinasc_2$ESTCIV, levels = c("Sem companheiro", "Com companheiro"))

table(dados_sinasc_2$ESTCIVMAE, dados_sinasc_2$ESTCIV)


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

#1 a 6: IDENTIFICAÇÃO E TOTAIS DE NASCIMENTO
consolidado_PE <- data.frame(
  ANO = 2015,                                     
  NIVEL = "UF",                                   
  CODMUNRES = 26,                                 
  TN = nrow(dados_sinasc_2),                      
  TNRC = sum(complete.cases(dados_sinasc_2)),
  TNRCR = sum(complete.cases(dados_sinasc_2))     
)
)

# 7 a 15: Totais por faixa etária
consolidado_PE$TGI_15    <- sum(dados_sinasc_2$IDADEMAE < 15, na.rm = TRUE)
consolidado_PE$TGI_15_19 <- sum(dados_sinasc_2$IDADEMAE >= 15 & dados_sinasc_2$IDADEMAE <= 19, na.rm = TRUE)
consolidado_PE$TGI_20_24 <- sum(dados_sinasc_2$IDADEMAE >= 20 & dados_sinasc_2$IDADEMAE <= 24, na.rm = TRUE)
consolidado_PE$TGI_25_29 <- sum(dados_sinasc_2$IDADEMAE >= 25 & dados_sinasc_2$IDADEMAE <= 29, na.rm = TRUE)
consolidado_PE$TGI_30_34 <- sum(dados_sinasc_2$IDADEMAE >= 30 & dados_sinasc_2$IDADEMAE <= 34, na.rm = TRUE)
consolidado_PE$TGI_35_39 <- sum(dados_sinasc_2$IDADEMAE >= 35 & dados_sinasc_2$IDADEMAE <= 39, na.rm = TRUE)
consolidado_PE$TGI_40_44 <- sum(dados_sinasc_2$IDADEMAE >= 40 & dados_sinasc_2$IDADEMAE <= 44, na.rm = TRUE)
consolidado_PE$TGI_45_49 <- sum(dados_sinasc_2$IDADEMAE >= 45 & dados_sinasc_2$IDADEMAE <= 49, na.rm = TRUE)
consolidado_PE$TGI_50    <- sum(dados_sinasc_2$IDADEMAE >= 50, na.rm = TRUE)

# 16 a 21: Idade Fértil e Estatísticas
consolidado_PE$TGIF   <- sum(dados_sinasc_2$IDADEMAE >= 15 & dados_sinasc_2$IDADEMAE <= 49, na.rm = TRUE)
consolidado_PE$IM_P25 <- quantile(dados_sinasc_2$IDADEMAE, 0.25, na.rm = TRUE)
consolidado_PE$IM_P50 <- median(dados_sinasc_2$IDADEMAE, na.rm = TRUE)
consolidado_PE$IM_P75 <- quantile(dados_sinasc_2$IDADEMAE, 0.75, na.rm = TRUE)
consolidado_PE$IM_MD  <- mean(dados_sinasc_2$IDADEMAE, na.rm = TRUE)
consolidado_PE$IM_DP  <- sd(dados_sinasc_2$IDADEMAE, na.rm = TRUE)

# 22 a 27: Escolaridade Materna
consolidado_PE$EM_S   <- length(which(dados_sinasc_2$ESCMAE2010 == 0))
consolidado_PE$EM_FI  <- length(which(dados_sinasc_2$ESCMAE2010 == 1))
consolidado_PE$EM_FII <- length(which(dados_sinasc_2$ESCMAE2010 == 2))
consolidado_PE$EM_M   <- length(which(dados_sinasc_2$ESCMAE2010 == 3))
consolidado_PE$EM_SI  <- length(which(dados_sinasc_2$ESCMAE2010 == 4))
consolidado_PE$EM_SC  <- length(which(dados_sinasc_2$ESCMAE2010 == 5))

# 28 a 32: Raça/Cor da Mãe (TGRC)
consolidado_PE$TGRC_B  <- length(which(dados_sinasc_2$RACACORMAE == 1))
consolidado_PE$TGRC_PT <- length(which(dados_sinasc_2$RACACORMAE == 2))
consolidado_PE$TGRC_A  <- length(which(dados_sinasc_2$RACACORMAE == 3))
consolidado_PE$TGRC_PD <- length(which(dados_sinasc_2$RACACORMAE == 4))
consolidado_PE$TGRC_I  <- length(which(dados_sinasc_2$RACACORMAE == 5))

# 33 a 34: Estado Civil
consolidado_PE$TGSC <- length(which(dados_sinasc_2$ESTCIV == "Sem companheiro"))
consolidado_PE$TGCC <- length(which(dados_sinasc_2$ESTCIV == "Com companheiro"))

# 35 a 36: Paridade
consolidado_PE$TGPRI  <- length(which(dados_sinasc_2$QTDFILVIVO == 0 & dados_sinasc_2$QTDFILMORT == 0))
consolidado_PE$TGNPRI <- length(which(dados_sinasc_2$QTDFILVIVO > 0 | dados_sinasc_2$QTDFILMORT > 0))

# 37 e 38: Tipo de Gravidez
consolidado_PE$TGU <- length(which(dados_sinasc_2$GRAVIDEZ == "Única"))
consolidado_PE$TGG <- length(which(dados_sinasc_2$GRAVIDEZ %in% c("Dupla", "Tripla ou mais")))

# 39 a 44: Duração da Gestação em semanas (TGD)
consolidado_PE$TGD_22    <- length(which(dados_sinasc_2$SEMAGESTAC < 22))
consolidado_PE$TGD_22_27 <- length(which(dados_sinasc_2$SEMAGESTAC >= 22 & dados_sinasc_2$SEMAGESTAC <= 27))
consolidado_PE$TGD_28_31 <- length(which(dados_sinasc_2$SEMAGESTAC >= 28 & dados_sinasc_2$SEMAGESTAC <= 31))
consolidado_PE$TGD_32_36 <- length(which(dados_sinasc_2$SEMAGESTAC >= 32 & dados_sinasc_2$SEMAGESTAC <= 36))
consolidado_PE$TGD_37_41 <- length(which(dados_sinasc_2$SEMAGESTAC >= 37 & dados_sinasc_2$SEMAGESTAC <= 41))
consolidado_PE$TGD_42    <- length(which(dados_sinasc_2$SEMAGESTAC >= 42))

# 45 a 47: Agrupamentos de Duração
consolidado_PE$TGD_PRT <- length(which(dados_sinasc_2$SEMAGESTAC < 37))
consolidado_PE$TGD_AT  <- length(which(dados_sinasc_2$SEMAGESTAC >= 37 & dados_sinasc_2$SEMAGESTAC <= 41))
consolidado_PE$TGD_PST <- length(which(dados_sinasc_2$SEMAGESTAC >= 42))

# 48 a 52: Medidas da Duração da Gestação (DG)
consolidado_PE$DG_P25 <- quantile(dados_sinasc_2$SEMAGESTAC, 0.25, na.rm = TRUE)
consolidado_PE$DG_P50 <- median(dados_sinasc_2$SEMAGESTAC, na.rm = TRUE)
consolidado_PE$DG_P75 <- quantile(dados_sinasc_2$SEMAGESTAC, 0.75, na.rm = TRUE)
consolidado_PE$DG_MD  <- mean(dados_sinasc_2$SEMAGESTAC, na.rm = TRUE)
consolidado_PE$DG_DP  <- sd(dados_sinasc_2$SEMAGESTAC, na.rm = TRUE)

# 53 a 57: Consultas Pré-natal (TKC)
consolidado_PE$TKC_NR  <- length(which(dados_sinasc_2$CONSPRENAT == 0))
consolidado_PE$TKC_ID  <- length(which(dados_sinasc_2$CONSPRENAT >= 1 & dados_sinasc_2$CONSPRENAT <= 3))
consolidado_PE$TKC_IT  <- length(which(dados_sinasc_2$CONSPRENAT >= 4 & dados_sinasc_2$CONSPRENAT <= 6))
consolidado_PE$TKC_AD  <- length(which(dados_sinasc_2$CONSPRENAT >= 7))
consolidado_PE$TKC_MAD <- 0 

# 58 e 59: Peregrinação
consolidado_PE$TGPRG_S <- 0
consolidado_PE$TGPRG_N <- 0

# 60 e 61: Tipo de Parto
consolidado_PE$TPV <- length(which(dados_sinasc_2$PARTO == "Vaginal"))
consolidado_PE$TPC <- length(which(dados_sinasc_2$PARTO == "Cesáreo"))

# 62 a 64: Apresentação do Feto (TPAPRESENT)
consolidado_PE$TRAP_C <- length(which(dados_sinasc_2$TPAPRESENT == 1)) # Cefálica
consolidado_PE$TRAP_P <- length(which(dados_sinasc_2$TPAPRESENT == 2)) # Pélvica
consolidado_PE$TRAP_T <- length(which(dados_sinasc_2$TPAPRESENT == 3)) # Transversa

# 65 a 74: Grupos de Robson (TGROB)
for(i in 1:10) {
  nome_coluna <- paste0("TGROB_", i)
  consolidado_PE[[nome_coluna]] <- 0
}

# 75 a 79: Local de Nascimento (TNLOC)
consolidado_PE$TNLOC_H  <- length(which(dados_sinasc_2$LOCNASC == 1)) # Hospital
consolidado_PE$TNLOC_ES <- length(which(dados_sinasc_2$LOCNASC == 2)) # Outro estab saúde
consolidado_PE$TNLOC_D  <- length(which(dados_sinasc_2$LOCNASC == 3)) # Domicílio
consolidado_PE$TNLOC_O  <- length(which(dados_sinasc_2$LOCNASC == 4)) # Outros
consolidado_PE$TNLOC_AI <- length(which(dados_sinasc_2$LOCNASC == 5)) # Aldeia indígena

# 80 a 81: Sexo (TRS)
consolidado_PE$TRS_M <- length(which(dados_sinasc_2$SEXO == "Masculino"))
consolidado_PE$TRS_F <- length(which(dados_sinasc_2$SEXO == "Feminino"))

# 82 a 86: Raça/Cor do Recém-Nascido (TRRC)
consolidado_PE$TRRC_B  <- length(which(dados_sinasc_2$RACACOR == "Branca"))
consolidado_PE$TRRC_PT <- length(which(dados_sinasc_2$RACACOR == "Preta"))
consolidado_PE$TRRC_A  <- length(which(dados_sinasc_2$RACACOR == "Amarela"))
consolidado_PE$TRRC_PD <- length(which(dados_sinasc_2$RACACOR == "Parda"))
consolidado_PE$TRRC_I  <- length(which(dados_sinasc_2$RACACOR == "Indígena"))

# 87 a 89: Categorias de Peso (TRP)
consolidado_PE$TRP_BP <- length(which(dados_sinasc_2$PESO < 2500))
consolidado_PE$TRP_N  <- length(which(dados_sinasc_2$PESO >= 2500 & dados_sinasc_2$PESO < 4000))
consolidado_PE$TRP_M  <- length(which(dados_sinasc_2$PESO >= 4000))

# 90 a 94: Estatísticas de Peso (PESO)
consolidado_PE$PESO_P25 <- quantile(dados_sinasc_2$PESO, 0.25, na.rm = TRUE)
consolidado_PE$PESO_P50 <- median(dados_sinasc_2$PESO, na.rm = TRUE)
consolidado_PE$PESO_P75 <- quantile(dados_sinasc_2$PESO, 0.75, na.rm = TRUE)
consolidado_PE$PESO_MD  <- mean(dados_sinasc_2$PESO, na.rm = TRUE)
consolidado_PE$PESO_DP  <- sd(dados_sinasc_2$PESO, na.rm = TRUE)

# 95 a 97: Classificação Peso/Idade (TRPIG)
consolidado_PE$TRPIG_P <- length(which(dados_sinasc_2$F_PIG == "PIG"))
consolidado_PE$TRPIG_A <- length(which(dados_sinasc_2$F_PIG == "AIG"))
consolidado_PE$TRPIG_G <- length(which(dados_sinasc_2$F_PIG == "GIG"))

# 98 a 101: Apgar no 5º Minuto (TRAPG5)
consolidado_PE$TRAPG5_B <- length(which(dados_sinasc_2$APGAR5 < 7))
consolidado_PE$TRAPG5_N <- length(which(dados_sinasc_2$APGAR5 >= 7))
consolidado_PE$APG5_MD  <- mean(dados_sinasc_2$APGAR5, na.rm = TRUE)
consolidado_PE$APG5_DP  <- sd(dados_sinasc_2$APGAR5, na.rm = TRUE)

# 102 a 103: Anomalia Congênita (TRAC)
consolidado_PE$TRAC  <- length(which(dados_sinasc_2$IDANOMAL == "Sim"))
consolidado_PE$TRSAC <- length(which(dados_sinasc_2$IDANOMAL == "Não"))






# TAREFA 9 E 10 REFORMULADA PARA MUNICIPIOS DO ESTADO 
# TAREFA 9



# Tarefa 11: Exporte o banco de dados com o nome SINASC_UF.csv



# Ao terminar a ETAPA 1 commite e envie para o repositório REMOTO com o comentário "Dados da UF e Script Etapa 1"



##################################
# ETAPA 2: BANCO DE DADOS DO SIM
##################################
# Só inicie esta Etapa quando a professora orientar
# ESTANDO NA BRANCH SINASC, NÃO ALTERE NADA NO SCRIPT REFERENTE A ETAPA 2

# Tarefa 1. Leitura do banco de dados Mortalidade_Geral_2015 do SIM 2015 com 1216475 linhas e 87 colunas
# verificar se a leitura foi feita corretamente e a estrutura dos dados
# nomeie o banco de dados como dados_sim


# Tarefa 2. Reduzir dados_sim apenas para as colunas que serão utilizadas, nomeando este novo banco de dados como dados_sim_1
# as colunas serão (a informar)
# nomes das respectivas variáveis: CONTADOR, TIPOBITO, CODMUNNATU, IDADE,  SEXO,  RACACOR,  ESTCIV, ESC2010, 
# CODMUNRES,  LOCOCOR, CODMUNOCOR, TPMORTEOCO,  OBITOGRAV, OBITOPUERP, CAUSABAS, CAUSABAS_O, TPOBITOCOR, MORTEPARTO



#####################################################
# ETAPA 3: OUTROS BANCOS DE DADOS: IBGE, SNIS, ...
#####################################################
# Só inicie esta Etapa quando a professora orientar
# ESTANDO NA BRANCH SINASC, NÃO ALTERE NADA NO SCRIPT REFERENTE A ETAPA 3

# Tarefa 1. Acesso aos bancos de dados e obtenção da informação



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

