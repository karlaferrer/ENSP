setwd("/Users/karlaferreira/Documents/ENSP/Intro/Trabalho")
### ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
### Disciplina Introdução à Epidemiologia 
### Arrumando os dados do Questionario da turma
### Turma 2022
### ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#+++++++++++++++++++++++++++
# Carregando uma biblioteca com funcoes uteis
#+++++++++++++++++++++++++++
### essa biblioteca permite que você utilize as funções Freq() e PercTable().
### caso você não tenha ela instalada use o código 
##install.packages('DescTools')
# ou vá em Tools > Install packages
library(DescTools) 
# dessa vez, o arquivo esta em formato excel
library(readxl)

#+++++++++++++++++++++++++++
# Carregando os dados
#+++++++++++++++++++++++++++

d <- read_xlsx("dados_brutos_2022.xlsx", col_types = "text") 
#View(d)
class(d) # esse é um data.frame

# ++++++++++++++++++++++++++++++++++++
# Caracteristicas gerais desse data.frame ----
# ++++++++++++++++++++++++++++++++++++
names(d)  # nomes das variaveis sao os codigos das perguntas, podemos trocar os nomes
dim(d)   # Viva!!

# ++++++++++++++++++++++++
# Verificacao das variaveis: uma a uma ----
# ++++++++++++++++++++++++

# Roteiro: 
# Qual tipo de variável é essa?
# Será que tem algum valor faltante ou missing ou nosso NA?
# Será que tem algum valor discrepante?
# vamos trocar o nome dela?
# vamos trocar as categorias?

# ++++++++++++++++++++++++++++++++++++++++++++++++++
# Bloco A: sociodemograficas----
# ++++++++++++++++++++++++++++++++++++++++++++++++++

# A1. idade ----
class(d$A1)
d$A1 <- as.numeric(d$A1)  # transformando em numero
summary(d$A1)

# Vamos criar uma nova variável de faixa de idade?
d$fetaria <- cut(d$A1, breaks = c(18, 29, 39, 49, 60, 100), right = FALSE)

# tabela de frequencia
Freq(d$fetaria, useNA = "always")

# +++++++++++++++++++++++++++++++++++++++++++
# A2. Qual o seu sexo biológico? ----
class(d$A2)
d$A2 <- as.factor(d$A2)
Freq(d$A2, useNA = "always") ## calculamos frequência relativa e absoluta

# ++++++++++++++++++++++++++++++++++++++++++
# A3. Como você considera sua raça/cor? ----
class(d$A3)
d$A3 <- as.factor(d$A3)
levels(d$A3)
# transformando "Prefiro nao informar em NA"
levels(d$A3)[6] <-NA
Freq(d$A3, useNA = "always")

# +++++++++++++++++++++++++++++++++++++++++++++
# A4.  Qual o seu estado ou unidade de federação de residência? ----
class(d$A4)
d$A4 <- as.factor(d$A4)
Freq(d$A4, useNA = "always")

# ++++++++++++++++++++++++++++++++++++++++++++
# A5 estado civil antes ----
d$A5 <- as.factor(d$A5)
levels(d$A5)
# transformando "Prefiro nao informar em NA"
levels(d$A5)[4] <- NA
Freq(d$A5, useNA = "always")

# ++++++++++++++++++++++++++++++++++++++++++++
# A6 estado civil agora ----
d$A6 <- as.factor(d$A6)
levels(d$A6)
# transformando "Prefiro nao informar em NA"
levels(d$A6)[4] <- NA
Freq(d$A6, useNA = "always")

# ++++++++++++++++++++++++++++++++++++++++++++
# A7 escolaridade ----
d$A7 <- as.factor(d$A7)
levels(d$A7)
# transformando "Prefiro nao informar em NA"
levels(d$A7)[7] <- NA
# vamos colocar em ordem, da menor para a maior escolaridade : VARIÁVEL ORDINAL
Freq(d$A7, useNA = "always")
d$A7 <- ordered(d$A7, levels = c("Ensino Fundamental incompleto", "Ensino Fundamental completo",
                                 "Ensino Médio incompleto", "Ensino Medio completo",
                                 "Ensino Superior incompleto", "Ensino Superior completo"))
Freq(d$A7, useNA = "always")

# ++++++++++++++++++++++++++++++++++++++++++++
# A8 . pessoas que moravam junto antes da pandemia ----
class(d$A8)
d$A8 <- as.numeric(d$A8)
table(d$A8, useNA = "always")
hist(d$A8, breaks = c(0,2,4,6,8,10,12,14,16,50))

# ++++++++++++++++++++++++++++++++++++++++++++
# A9 . pessoas que moravam junto agora ----
d$A9 <- as.numeric(d$A9)
table(d$A9, useNA = "always")
hist(d$A9, breaks = c(0,2,4,6,8,10,12,14,16,50))

# ++++++++++++++++++++++++++++++++++++++++++++++++++
#A10 situacao de trabalho antes ----
d$A10 <- as.factor(d$A10)
levels(d$A10)
# PS. mesmo que nao tenha ninguem na categoria "Prefiro não informar" 
# essa categoria existe na variavel. como queremos exclui-la, vamos
# atribuir NA. nao e' o caso, pois tem 5 NAs
levels(d$A10)[9] <- NA
Freq(d$A10, useNA = "always")


# ++++++++++++++++++++++++++++++++++++++++++++++++++
#A11 situacao de trabalho agora ----
d$A11 <- as.factor(d$A11)
levels(d$A11)
# transformando "Prefiro nao informar em NA"
levels(d$A11)[9] <- NA
Freq(d$A11, useNA = "always")

# ++++++++++++++++++++++++++++++++++++++++++++++++++
#A12 renda antes ----
d$A12 <- as.factor(d$A12)
levels(d$A12)
# transformando "Prefiro nao informar em NA"
levels(d$A12)[6] <- NA

Freq(d$A12, useNA = "always")

# ++++++++++++++++++++++++++++++++++++++++++++++++++
#A13 renda agora ----
d$A13 <- as.factor(d$A13)
levels(d$A13)
# transformando "Prefiro nao informar em NA"
levels(d$A13)[6] <- NA
Freq(d$A13, useNA = "always")

# ++++++++++++++++++++++++++++++++++++++++++++++++++
#A14 auxilio emergencial ----
d$A14 <- as.factor(d$A14)
levels(d$A14)
# transformando "Prefiro nao informar em NA"
levels(d$A14)[2] <- NA
Freq(d$A14, useNA = "always")

# ++++++++++++++++++++++++++++++++++++++++++++++++++
# Criando variável A5 com novas categorias
levels(d$A5)
d$A5mod <- Recode(d$A5,  #aqui sao as mesmas categorias, e'isso mesmo?
                  "Em relacionamento" = levels(d$A5)[1],
                  "Separado" = levels(d$A5)[2],
                  "Outro" = levels(d$A5)[3],
                  "Solteiro" = levels(d$A5)[4],
                  "Viúvo" = levels(d$A5)[5]
)
Freq(d$A5mod, useNA = "always")

#Criando variável A10 com novas categorias
levels(d$A10)
d$A10mod <- Recode(d$A10,
                   "Autônomo/Trabalhador privado/empregador" = levels(d$A10)[c(2,4,6,9)],
                   "Trabalhador público/Aposentado" = levels(d$A10)[c(1,5,7)],
                   "Outros" = levels(d$A10)[c(3,8,10)]
)
Freq(d$A10mod, useNA = 'always')


# ++++++++++++++++++++++++++++++++++++++++++++++++++
# Bloco B: saude fisica e mental ----
# ++++++++++++++++++++++++++++++++++++++++++++++++++

#B1 estado de saude antes
d$B1 <- as.factor(d$B1)
Freq(d$B1, useNA = "always")
# transformar em NA o Prefiro nao informar
levels(d$B1)[4] <- NA
Freq(d$B1, useNA = "always")


# vamos criar uma variavel com duas categorias? 
d$estado.saude.antes <- Recode(d$B1,
                               "bom" = c("Bom", "Muito bom"),
                               "regular ou ruim" = c("Regular", "Muito ruim", "Ruim"))
Freq(d$estado.saude.antes, useNA = "always")
Freq(d$estado.saude.antes) #obter frequencias sem NAs

# ++++++++++++++++++++++++++++++++++++++++++++++++++
#B2 estado de saude agora
d$B2 <- as.factor(d$B2)
levels(d$B2)[4] <- NA
Freq(d$B2, useNA = "always")

# vamos criar uma variavel com duas categorias? 
d$estado.saude.agora <- Recode(d$B2,
                               "bom" = c("Bom", "Muito bom"),
                               "regular ou ruim" = c("Regular", "Muito ruim", "Ruim"))
Freq(d$estado.saude.antes, useNA = "always")

# mudanca de status de auto-percepcao saude
d$piorou.saude <- ifelse(d$estado.saude.antes == "bom" & d$estado.saude.agora == "regular ou ruim", "piorou", "igual")
Freq(d$piorou.saude, useNA = "always")

# ++++++++++++++++++++++++++++++++++++++++++++++++++
#B3 sono antes----
d$B3 <- as.factor(d$B3)
Freq(d$B3, useNA = "always")  #NÃO TEM NADA Q PRECISE SER TRANSFORMADO EM NA
levels(d$B3)
d$B3 <- ordered(d$B3, levels = c("Muito boa", "Boa",
                                 "Regular", "Ruim", "Muito ruim"))
Freq(d$B3, useNA = "always")

#criação de uma nova variavel com duas categorias

# vamos criar uma variavel com duas categorias 
d$sono.antes <- Recode(d$B3,"boa" = c("Boa", "Muito boa"),
                       "regular ou ruim" = c("Regular", "Muito ruim", "Ruim"))

Freq(d$sono.antes, useNA = "always")

# ++++++++++++++++++++++++++++++++++++++++++++++++++
#B4 sono agora----
d$B4 <- as.factor(d$B4)
summary(d$B4)
Freq(d$B4, useNA = "always")

# vamos criar uma variavel com duas categorias 
d$sono.agora <- Recode(d$B4,"boa" = c("Boa", "Muito boa"),
                       "regular ou ruim" = c("Regular", "Muito ruim", "Ruim"))

Freq(d$sono.agora, useNA = "always")

# criação da variável mudanca de status de qualidade do sono
levels(d$sono.antes)
table(antes = d$sono.antes, agora = d$sono.agora)

# PS. mexi no nome da categoria proposta pq tem 59 que melhoraram.
# como era: d$piorou.sono <- ifelse(d$sono.antes == "boa" & d$sono.agora == "regular ou ruim", "piorou", "igual")
d$piorou.sono <- ifelse(d$sono.antes == "boa" & d$sono.agora == "regular ou ruim", "piorou", "igual ou melhorou")
Freq(d$piorou.sono, useNA = "always") #24% tiveram uma piora da qualidade do sono

# ++++++++++++++++++++++++++++++++++++++++++++++++++
#B5 medicamentos antes ----
d$B5 <- as.factor(d$B5)
levels(d$B5)
Freq(d$B5, useNA = "always")  

#criar nova variavel com tres categorias

d$medicamentos_antes <- Recode(d$B5,"Frequentemente" = c("Frequentemente", "Sempre"),
                               "As vezes" = c("Ás vezes", "Raramente"), "Nunca"=c("Nunca"))

Freq(d$medicamentos_antes, useNA = "always") 

# ++++++++++++++++++++++++++++++++++++++++++++++++++
#B6 medicamento agora ----
d$B6 <- as.factor(d$B6)
summary(d$B6)

d$medicamentos_agora <- Recode(d$B6,"Frequentemente" = c("Frequentemente", "Sempre"),
                               "As vezes" = c("Ás vezes", "Raramente"), "Nunca"=c("Nunca"))

Freq(d$medicamentos_agora, useNA = "always") 

###criando uma variavel de aumento da frequencia de uso de medicamentos para dormir
table(antes = d$medicamentos_antes, agora = d$medicamentos_agora)

# ps. acrescentei a situacao de mudar de as vezes para frequentemente, veja se resolve
d$aumento_medicam <- ifelse((d$medicamentos_antes == "Nunca" & 
                               (d$medicamentos_agora == "Frequentemente"|d$medicamentos_agora == "As vezes")) |
                              (d$medicamentos_antes == "As vezes" & d$medicamentos_agora == "Frequentemente"),
                            "sim", "nao")
Freq(d$aumento_medicam, useNA = "always") #24% tiveram uma piora da qualidade do sono

# fiz a modificacao acima diretamente na condicao, veja se atende. do seu jeito tbm funciona
# d$aumento_medic_final <- ifelse((d$medicamentos_antes == "Nunca") & (d$medicamentos_agora == "As vezes")
#                                 ,   "sim", "") #criei essa variavel pq não consegui lidar com essa condição 
# 
# f <- which(d$aumento_medic_final == "")   #onde está vazio eu irei substituir pelos valores da variavel d$piorou.ansieda
# d$aumento_medic_final[f] <- d$aumento_medicam[f]
# 
# Freq(d$aumento_medic_final, useNA = "always")

#removendo a coluna d$aumento_medicam
#d$aumento_medicam <- NULL
#head(d)

# ++++++++++++++++++++++++++++++++++++++++++++++++++
#B7 ansioso antes ----
d$B7 <- as.factor(d$B7)
summary(d$B7)

d$ansiedade_antes<- Recode(d$B7,"Frequentemente" = c("Frequentemente", "Sempre"),
                           "As vezes" = c("Ás vezes", "Raramente"), "Nunca"=c("Nunca"))
Freq(d$ansiedade_antes, useNA = "always") 

# ++++++++++++++++++++++++++++++++++++++++++++++++++
#B8 ansioso agora ----
d$B8 <- as.factor(d$B8)
summary(d$B8)
Freq(d$B8, useNA = "always")

# transformar em NA o Prefiro nao informar
levels(d$B8)[4] <- NA
Freq(d$B8, useNA = "always")

d$ansiedade_agora <- Recode(d$B8,
                            "Frequentemente" = c("Frequentemente", "Sempre"),
                            "As vezes" = c("Ás vezes", "Raramente"), "Nunca"=c("Nunca"))
Freq(d$ansiedade_agora, useNA = "always")

#criando variavel ansioso com duas categorias: sim e nao

d$ansioso <- Recode(d$ansiedade_agora,
                    "sim" = c("Frequentemente","As vezes"),
                    "nao" = c( "Nunca"))
Freq(d$ansioso, useNA = "always")


#Criando a variavel: piora no estado de ansiedade (e'sempre bom ver a tabela antes)
table(antes = d$ansiedade_antes, agora = d$ansiedade_agora)

d$piorou.ansieda <- ifelse((d$ansiedade_antes == "Nunca"| d$ansiedade_antes=="As vezes") &
                             (d$ansiedade_agora == "Frequentemente"), "piorou", "nao_piorou")
# incluindo as pessoas que tinham as vezes e agora e' frequente
d$piorou.ansieda <- ifelse(((d$ansiedade_antes == "Nunca"| d$ansiedade_antes=="As vezes") &
                              (d$ansiedade_agora == "Frequentemente")) | 
                             (d$ansiedade_antes == "As vezes" & d$ansiedade_agora == "Frequentemente"),"piorou", "nao_piorou")
Freq(d$piorou.ansieda, useNA = "always")


# d$piorou.ansieda_final <- ifelse((d$ansiedade_antes == "Nunca") & (d$ansiedade_agora == "As vezes")
#                                  
#                                  ,   "piorou", "") #criei essa variavel pq não consegui lidar com essa condição 
# 
# pos <- which(d$piorou.ansieda_final == "")   #onde está vazio eu irei substituir pelos valores da variavel d$piorou.ansieda
# d$piorou.ansieda_final[pos] <- d$piorou.ansieda[pos]
# 
# Freq(d$piorou.ansieda_final, useNA = "always")
# 
# #removendo a coluna d$piorou_ansieda
# 
# d$piorou.ansieda <- NULL
# head(d)

# ++++++++++++++++++++++++++++++++++++++++++++++++++
#B9 isolado durante ----
d$B9 <- as.factor(d$B9)
summary(d$B9)
Freq(d$B9, useNA = "always")
levels(d$B9)[2] <- NA
Freq(d$B9, useNA = "always")

# ++++++++++++++++++++++++++++++++++++++++++++++++++
#B10 doenca cronica antes ----
d$B10 <- as.factor(d$B10)
summary(d$B10)
Freq(d$B10, useNA = "always")
levels(d$B10)[2] <- NA
Freq(d$B10, useNA = "always")

# ++++++++++++++++++++++++++++++++++++++++++++++++++
#B11 doenca cronica agora ----
d$B11 <- as.factor(d$B11)
summary(d$B11)
levels(d$B11)[2] <- NA
Freq(d$B11, useNA = "always")

#criando variavel de surgimento de doenças cronicas
# ** vejam que doido, 85 pessoas deixaram de ter doencas cronicas (mais do que o contrario)

table(antes = d$B10, agora = d$B11)
d$surgim_cronica <- ifelse((d$B10 == "Não") & (d$B11 == "Sim")
                           
                           , "sim", "não")

Freq(d$surgim_cronica, useNA = "always")

# ++++++++++++++++++++++++++++++++++++++++++++++++++
#B12 coluna antes ----
d$B12 <- as.factor(d$B12)
levels(d$B12)
levels(d$B12)[4]<- NA
Freq(d$B12, useNA = "always")

#NOVA VARIAVEL COM 3 CATEGORIAS
d$coluna_antes <- Recode(d$B12,
                         "Frequentemente" = c("Frequentemente", "Sempre"),
                         "As vezes" = c("Ás vezes", "Raramente"), "Nunca"=c("Nunca"))
Freq(d$coluna_antes, useNA = "always")

#coluna com duas categorias: "sim" e "nao"

d$coluna_antes2 <- Recode(d$coluna_antes,
                          "sim" = c("Frequentemente","As vezes"),
                          "nao" = c( "Nunca"))

Freq(d$coluna_antes2, useNA = "always")


# ++++++++++++++++++++++++++++++++++++++++++++++++++
#B13 coluna agora ----
d$B13 <- as.factor(d$B13)
levels(d$B13)
levels(d$B13)[4]<- NA
Freq(d$B13, useNA = "always")

d$coluna_agora <- Recode(d$B13,
                         "Frequentemente" = c("Frequentemente", "Sempre"),
                         "As vezes" = c("Ás vezes", "Raramente"), "Nunca"=c("Nunca"))
Freq(d$coluna_agora, useNA = "always")

#criando variavel aumento da frequencia das dores de coluna (corrigida)
table(anres = d$coluna_antes, agora = d$coluna_agora)
d$aument_dor_coluna <- ifelse((d$coluna_antes == "Nunca"| d$coluna_antes=="As vezes") &
                                (d$coluna_agora == "Frequentemente") | 
                                (d$coluna_antes=="As vezes" &
                                   d$coluna_agora == "Frequentemente"),
                              "aumentou", "nao_aumentou")
Freq(d$aument_dor_coluna, useNA = "always")

# ++++++++++++++++++++++++++++++++++++++++++++++++++
#B14 condicoes antes ----
d$B14 <- as.factor(d$B14)
levels(d$B14)
levels(d$B14)[43] <- NA

# criar variavel: tinha alguma
d$tinha.condicao <- ifelse(d$B14 == "Nenhuma dessas condições", "não","sim")
Freq(d$tinha.condicao, useNA = "always")

# tentando extrair uma condicao (dei uma completada)
library(stringr)  # biblioteca de funcoes para lidar com strings
d$gastrite.antes <- str_detect(d$B14, "Gastrite")  # detecta presenca de um texto   
class(d$gastrite.antes)
d$gastrite.antes <- as.factor(d$gastrite.antes)
levels(d$gastrite.antes)
levels(d$gastrite.antes) <- c("nao", "sim")
Freq(d$gastrite.antes, useNA = "always")

d$quedacab.antes <- str_detect(d$B14, "Queda de cabelo")  # detecta presenca de um texto   
d$quedacab.antes <- as.factor(d$quedacab.antes)
levels(d$quedacab.antes)
levels(d$quedacab.antes) <- c("nao","sim")
Freq(d$quedacab.antes, useNA = "always")


# ++++++++++++++++++++++++++++++++++++++++++++++++++
# B15 condicao agora ----
d$B15 <- as.factor(d$B15)
levels(d$B15)
levels(d$B15)[56] <- NA

# criar variavel: tem alguma das condições
d$tem.condicao <- ifelse(d$B15 == "Nenhuma dessas condições", "não","sim")
Freq(d$tem.condicao, useNA = "always")

#criar variavel piora nessas condições (mais pessoas com essas condições)
table(antes = d$tinha.condicao, agora = d$tem.condicao)
d$surg_condicao <- ifelse((d$tinha.condicao == "não") & (d$tem.condicao == "sim")
                          ,   "sim", "não ou ja tinha")

Freq(d$surg_condicao, useNA = "always")

# ++++++++++++++++++++++++++++++++++++++++++++++++++
# B16 diagnostico covid
d$B16 <- as.factor(d$B16)
summary(d$B16)
levels(d$B16)[2] <- NA
Freq(d$B16, useNA = "always")

# ++++++++++++++++++++++++++++++++++++++++++++++++++
# sintomas covid ----
d$B17 <- as.factor(d$B17)
summary(d$B17)
levels(d$B17)[c(2,3)] <- NA
Freq(d$B17, useNA = "always")

# ++++++++++++++++++++++++++++++++++++++++++++++++++
# B18 areas verdes----
d$B18 <- as.factor(d$B18)
summary(d$B18)

d$area_verd <- Recode(d$B18,
                      "Frequentemente" = c("Frequentemente", "Sempre"),
                      "As vezes" = c("Às vezes", "Raramente"), "Nunca"=c("Nunca"))
Freq(d$area_verd, useNA = "always")

# ++++++++++++++++++++++++++++++++++++++++++++++++++
# B19 areas verdes saude mental----
d$B19 <- as.factor(d$B19)
summary(d$B19)
levels(d$B19)[c(2,4)] <- NA
Freq(d$B19, useNA = "always")

# ++++++++++++++++++++++++++++++++++++++++++++++++++
# Bloco C: atividade fisica
# ++++++++++++++++++++++++++++++++++++++++++++++++++
# C1 exercicio antes----
d$C1 <- as.factor(d$C1)
summary(d$C1)

# transformando em NA
levels(d$C1)
levels(d$C1)[5] <- NA
# colocando em ordem : var ordinal
d$C1 <- ordered(d$C1, levels = c("Não praticava", "1 a 2 dias",
                                 "3 a 4 dias", "5 ou mais dias"))
Freq(d$C1)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# C2 - exercicio local antes ----
d$C2 <- as.factor(d$C2)
summary(d$C2)

##Se a pergunta é o local que praticava, quem não praticava
##será transformado em NA, assim como os "Não sei informar".
levels(d$C2)
levels(d$C2)[5] <- NA
levels(d$C2)
levels(d$C2)[4] <- NA
Freq(d$C2)

d$C2m<- Recode(d$C2, "Academias em geral" = "Academia/Estúdios/Clubes", 
               "Casa"= "Em casa", 
               "Ao ar livre" = c("Academia da terceira idade (Prefeitura)", "Parques/Praia e ruas"),
               elselevel = NA)
Freq(d$C2m, useNA = "always")

# ++++++++++++++++++++++++++++++++++++++++++++++++++
# C3 - duracao atividade antes ----
d$C3 <- as.factor(d$C3)
Freq(d$C3)

##Transoformando "Não sei informar" em NA
levels(d$C3) 
levels(d$C3)[5] <- NA

##Ordenando as categorias pois se trata de uma variável qualitativa ordinal
niveis.C3 <- levels(d$C3)
niveis.C3
##[1] "Entre 1 - 2 horas" "Mais de 2 horas"   "Menos de 1 hora"   "Não praticava" 

niveis.ordem.C3 <- c(4,3,1,2) ##Ordenando do menor para o maior
niveis.C3[niveis.ordem.C3] # ta certo? ##correto

##Recodificando a variável com os nívei ordenados
d$C3 <- ordered(d$C3, levels = niveis.C3[niveis.ordem.C3]) 
Freq(d$C3, useNA = "always")

# ++++++++++++++++++++++++++++++++++++++++++++++++++
# C4 - objetivo antes ----
d$C4 <- as.factor(d$C4)
summary(d$C4)

##Se a pergunta é qual o objetivo em praticar exercícios, quem não praticava
##será transformado em NA, assim como os "Não sei informar".
levels(d$C4)
levels(d$C4)[3] <- NA
levels(d$C4)
levels(d$C4)[3] <- NA
levels(d$C4)

Freq(d$C4)

d$C4m<- Recode(d$C4, "Saúde" = c("Melhora de alguma comorbidade", "Qualidade de vida ou gosto"),
               "Estética" = "Estética (Emagrecimento ou ganho de massa)",
               "Performance" = "Performance esportiva",elselevel = NA)

Freq(d$C4m, useNA = "always")

# ++++++++++++++++++++++++++++++++++++++++++++++++++
# C5 - exercício agora----
d$C5 <- as.factor(d$C5)
summary(d$C5)

##Transoformando "Não sei informar" em NA
levels(d$C5) ##"Não sei informar" é o level 5
levels(d$C5)[5] <- NA

##Ordenando as categorias pois se trata de uma variável qualitativa ordinal
niveis.C5 <- levels(d$C5)
niveis.C5
##[1] "1 a 2 dias"     "3 a 4 dias"     "5 ou mais dias" "Não pratico" 

niveis.ordem.C5 <- c(4,1,2,3) ##Ordenando do menor para o maior
niveis.C5[niveis.ordem.C5] # ta certo? ##correto

##Recodificando a variável com os nívei ordenados
d$C5 <- ordered(d$C5, levels = niveis.C5[niveis.ordem.C5]) 
Freq(d$C5, useNA = "always")

# ++++++++++++++++++++++++++++++++++++++++++++++++++
# C6 - local agora ----
d$C6 <- as.factor(d$C6)
summary(d$C6)

##Se a pergunta é o local que pratica, quem não pratica 
##será transformado em NA, assim como os "Não sei informar".
levels(d$C6)
levels(d$C6)[5] <- NA
levels(d$C6)
levels(d$C6)[4] <- NA
Freq(d$C6)

d$C6m<- Recode(d$C6, "Academias em geral" = "Academia/Estúdios/Clube", 
               "Casa"= "Em casa", 
               "Ao ar livre" = c("Academia da terceira idade (Prefeitura)", "Parques/Praia e ruas"),
               elselevel = NA)

Freq(d$C6m, useNA = "always")

# ++++++++++++++++++++++++++++++++++++++++++++++++++
# C7 - duracao agora ----
d$C7 <- as.factor(d$C7)
summary(d$C7)

##Transformando "Não sei informar" em NA
levels(d$C7) ##"Não sei informar" é o level 5
levels(d$C7)[5] <- NA

##Ordenando as categorias pois se trata de uma variável qualitativa ordinal
niveis.C7 <- levels(d$C7)
niveis.C7
##[1] "Entre 1 - 2 horas" "Mais de 2 horas"   "Menos de 1 hora"   "Não praticava" 

niveis.ordem.C7 <- c(4,3,1,2) ##Ordenando do menor para o maior
niveis.C7[niveis.ordem.C7] # ta certo? ##correto

##Recodificando a variável com os níveis ordenados
d$C7 <- ordered(d$C7, levels = niveis.C7[niveis.ordem.C7]) 
Freq(d$C7, useNA = "always")

# ++++++++++++++++++++++++++++++++++++++++++++++++++
# C8 - objetivo agora
d$C8 <- as.factor(d$C8)
summary(d$C8)

##Se a pergunta é qual o objetivo em praticar exercícios, quem não pratica
##será transformado em NA, assim como os "Não sei informar".
levels(d$C8)
levels(d$C8)[4] <- NA
levels(d$C8)
levels(d$C8)[3] <- NA
levels(d$C8)

Freq(d$C8)

d$C8m<- Recode(d$C8, "Saúde" = c("Melhora de alguma comorbidade", "Qualidade de vida ou gosto"),
               "Estética" = "Estética (Emagrecimento ou ganho de massa)",
               "Performance" = "Performance esportiva",elselevel = NA)

Freq(d$C8m, useNA = "always")

# ++++++++++++++++++++++++++++++++++++++++++++++++++
# tempo de tela antes
d$C9 # tentando corrigir!
table(d$C9)

library(readr)  # biblioteca de funcoes para lidar com strings
d$C9m <- parse_number(d$C9)  # extrai o numero da string  
d$C9m
summary(d$C9m)
table(d$C9m)

d$C9[is.na(d$C9m)]  # 35 casos ficaram NA,pq nao tinha numero
n <- which(is.na(d$C9m))  # posicao desses
d$C9m[n] <- c(8, NA, NA, 0, NA, NA, NA, NA, 0, NA, NA, NA, NA, NA, 0,
              NA, NA, 2, NA, 0, NA, NA, 1, 2, 1, 1.5, NA, NA, NA, NA, NA,
              8, NA, 1, 3) # substituindo por valores 
d$C9m
summary(d$C9m)

d$C9m <- as.factor(d$C9m)
levels(d$C9m)

##Como a pergunta foi quantas horas por dia (o dia tem 24h), todos os valores 
##estranhos e acima de 24, serão transformados em NA.
## a ideia e'boa, mas voces poderiam ter trabalho com ela de forma numerica. ver abaixo

levels(d$C9m)[3] <- NA
levels(d$C9m)
levels(d$C9m)[2] <- NA
levels(d$C9m)
levels(d$C9m)[20] <- NA
levels(d$C9m)
levels(d$C9m)[20] <- NA
levels(d$C9m)
levels(d$C9m)[20] <- NA
levels(d$C9m)
levels(d$C9m)[20] <- NA
levels(d$C9m)
levels(d$C9m)[20] <- NA
levels(d$C9m)
levels(d$C9m)[20]<- NA

summary(d$C9m)

Freq(d$C9m) ##Variável limpa

# refazendo , de forma numerica
d$C9m <- parse_number(d$C9)  # extrai o numero da string  
table(d$C9m)
d$C9m[d$C9m > 24] <- NA
summary(d$C9m)
hist(d$C9m)

# ++++++++++++++++++++++++++++++++++++++++++++++++++
# C10 tempo de tela agora ----
d$C10 
d$C10m <- parse_number(d$C10)  # extrai o numero da string  
d$C10[is.na(d$C10m)]  # 31 ficaram NA,pq nao tinha numero
n <- which(is.na(d$C10m))  # posicao desses
d$C10m[n] <- c(NA, 0, NA, NA, NA, 2, 0, NA, 0, NA, NA, NA, NA, 
               2, NA, 4, NA, 2, 1, NA, 2, NA, NA, 3, NA, 1, NA, 0, 1, NA, 3)
d$C10m
summary(d$C10m)

d$C10m <- as.factor(d$C10m)
levels(d$C10m)

##Como a pergunta foi quantas horas por dia (o dia tem 24h), todos os valores 
##estranhos e acima de 24, serão transformados em NA.

levels(d$C10m)[20] <- NA
levels(d$C10m)
levels(d$C10m)[20] <- NA
levels(d$C10m)

# refazendo , de forma numerica
d$C10m <- parse_number(d$C10)  # extrai o numero da string  
table(d$C10m)
d$C10m[d$C10m > 24] <- NA
summary(d$C10m)
hist(d$C10m)

# mudanca de tempo de tela (sugestao)
d$mudanca.tela <- d$C10m - d$C9m
hist(d$mudanca.tela)

# +++++++++++++++++++++++++++++++++++++++++++++++++++
# Bloco D: Alimentacao
# ++++++++++++++++++++++++++++++++++++++++++++++++++
# D1 consumo arroz e feijao antes ----
d$D1 <- as.factor(d$D1)
Freq(d$D1)
levels(d$D1)[4] <- NA
niveis <- levels(d$D1)
niveis.ordem <- c(4,1,2,3,5)
niveis[niveis.ordem]  # tá certinho?
d$D1_2 <- ordered(d$D1, levels = niveis[niveis.ordem])
Freq(d$D1_2)

#simplificar o nome das categorias
d$D1_2 <- Recode(d$D1_2,
                 "Raramente" = "Raramente ou nunca",
                 "1 a 2 dias" = "1 a 2 dias por semana",
                 "3 a 4 dias"  = "3 a 4 dias por semana",
                 "5 a 6 dias" = "5 a 6 dias por semana",
                 "Todos os dias" = "Todos os dias (inclusive sábado e domingo)"
)
tab1_2 <-as.data.frame(Freq(d$D1_2))
tab1_2

# ++++++++++++++++++++++++++++++++++++++++++++++++++
d$D2 <- as.factor(d$D2)
summary(d$D2)
Freq(d$D2)
levels(d$D2)[4] <- NA
niveis2 <- levels(d$D2)  # notem que como nao muda entre as perguntas, voces podiam ter usado o niveis aqui
niveis.ordem2 <- c(4,1,2,3,5)
niveis2[niveis.ordem2]  # tá certinho?
d$D2_2 <- ordered(d$D2, levels = niveis2[niveis.ordem2])
Freq(d$D2_2)

#simplificar o nome das categorias
d$D2_2 <- Recode(d$D2_2,
                 "Raramente" = "Raramente ou nunca",
                 "1 a 2 dias" = "1 a 2 dias por semana",
                 "3 a 4 dias"  = "3 a 4 dias por semana",
                 "5 a 6 dias" = "5 a 6 dias por semana",
                 "Todos os dias" = "Todos os dias (inclusive sábado e domingo)"
)

tab2_2 <-as.data.frame(Freq(d$D2_2))
tab2_2

#Tabela com as freq antes e em 2022  - Consumo de arroz e feijao
# tab_41 <- merge(tab1_2, tab2_2,by = "level", all.x = TRUE, all.y = TRUE, sort = FALSE)
# tab_41<- tab_41[c(1:3,6,7)]
# tab_41$perc.x <- tab_41$perc.x*100
# tab_41$perc.y <- tab_41$perc.y*100
# tab_41$perc.x <- round(tab_41$perc.x, digits = 1)
# tab_41$perc.y <- round(tab_41$perc.y, digits = 1)
# soma41 <- round(colSums(tab_41[,2:5]), digits = 0)
# soma41<- c("Total",soma41)
# tab_41<- rbind(tab_41,soma41)
# names(tab_41) <- c("Frequencia","Freq. antes", "Perc. antes", "Freq. 2022", "Perc. 2022")
# tab_41
#library("xlsx")
#write.xlsx(tab_41, "Tabela 41 Arroz.xlsx")

# ++++++++++++++++++++++++++++++++++++++++++++++++++
d$D3 <- as.factor(d$D3)
summary(d$D3)
Freq(d$D3)
levels(d$D3)[4] <- NA
niveis3 <- levels(d$D3)
niveis.ordem3 <- c(4,1,2,3,5)
niveis3[niveis.ordem3]  # tá certinho?
d$D3_2 <- ordered(d$D3, levels = niveis3[niveis.ordem3])
Freq(d$D3_2)

#simplificar o nome das categorias
d$D3_2 <- Recode(d$D3_2,
                 "Raramente" = "Raramente ou nunca",
                 "1 a 2 dias" = "1 a 2 dias por semana",
                 "3 a 4 dias"  = "3 a 4 dias por semana",
                 "5 a 6 dias" = "5 a 6 dias por semana",
                 "Todos os dias" = "Todos os dias (inclusive sábado e domingo)"
)
Freq(d$D3_2)

#Grafico de barras das proporcoes de consumo de FVL -antes
# tab_D3 <- round(prop.table(table(d$D3_2))*100, digits = 1)
# tab_D3
# barplot(tab_D3, col = "blue ", ylab = "Freq. relativa (%)",
#         main = "Consumo de frutas, verduras e legumes - Antes")


# ++++++++++++++++++++++++++++++++++++++++++++++++++
d$D4 <- as.factor(d$D4)
summary(d$D4)
Freq(d$D4, useNA = "always")
levels(d$D4)[4] <- NA
niveis4 <- levels(d$D4)
niveis.ordem4 <- c(4,1,2,3,5)
niveis4[niveis.ordem4]  # tá certinho?
d$D4_2 <- ordered(d$D4, levels = niveis4[niveis.ordem4])
Freq(d$D4_2)

#simplificar o nome das categorias
d$D4_2 <- Recode(d$D4_2,
                 "Raramente" = "Raramente ou nunca",
                 "1 a 2 dias" = "1 a 2 dias por semana",
                 "3 a 4 dias"  = "3 a 4 dias por semana",
                 "5 a 6 dias" = "5 ou mais dias por semana",
                 "Todos os dias" = "Todos os dias (inclusive sábado e domingo)"
)
Freq(d$D4_2)

#D4 em dois grupos de consumo de FVL - objetivo 5
d$D4_3 <- Recode(d$D4_2,
                 "Não Saudável" = "Raramente",
                 "Saudável" = c("1 a 2 dias por semana","1 a 2 dias" ,"3 a 4 dias" ,"5 a 6 dias", "Todos os dias")
)

Freq(d$D4_3,useNA = "always")
#tabela de contingencia - piorou e FLV saudavel e nao saudavel
addmargins(table(d$piorou.saude, d$D4_3))
#tabela de contingencia - percepcao antes e FLV saudavel e nao saudavel
tab_contFVL<- addmargins(table(d$estado.saude.antes, d$D4_3))
#library("xlsx")
as.data.frame.matrix(tab_contFVL)
write.xlsx(tab_contFVL, "Tabela Incidencia FLV.xlsx")
#grafico objetivo 5 - FVL

data5FVL <- matrix(c(22.8,30.9,23.4), nrow=3)
#colnames(data5FVL) <- c("Incidência")
rownames(data5FVL) <- c("Saudável","Não saudável","Geral")

# barplot
barplot(data5FVL, 
        col=colors()[c(23,89,12)] , 
        border="white", 
        font.axis=2, 
        beside=T, 
        ylab="Incidência %", 
        xlab=NULL,
        ylim = c(0,40),
        font.lab=2,
        width = c(22, 30, 23)
) 
legend("top", # Coordinates
       legend = rownames(data5FVL),
       col = colors()[c(23,89,12)],
       pch = 15,
       title = "Consumo de FLV",
       xpd = TRUE,
       horiz = TRUE
) 

#Tabela com as freq antes e em 2022  - Consumo de FVL
# tab3_2 <-as.data.frame(Freq(d$D3_2))
# tab4_2 <-as.data.frame(Freq(d$D4_2))
# tab_42 <- merge(tab3_2, tab4_2,by = "level", all.x = TRUE, all.y = TRUE, sort = FALSE)
# tab_42<- tab_42[c(1:3,6,7)]
# tab_42$perc.x <- tab_42$perc.x*100
# tab_42$perc.y <- tab_42$perc.y*100
# tab_42$perc.x <- round(tab_42$perc.x, digits = 1)
# tab_42$perc.y <- round(tab_42$perc.y, digits = 1)
# soma42 <- round(colSums(tab_42[,2:5]), digits = 0)
# soma42<- c("Total",soma42)
# tab_42<- rbind(tab_42,soma42)
# names(tab_42) <- c("Frequencia","Freq. antes", "Perc. antes", "Freq. 2022", "Perc. 2022")
#library("xlsx")
#write.xlsx(tab_42, "Tabela 42 FLV.xlsx")

#Grafico de barras das proporcoes de consumo de FVL -2022
tab_D4 <- round(prop.table(table(d$D4_2))*100, digits = 1)
tab_D4
#barplot(tab_D4, col = "red", ylab = "Freq. relativa (%)",
#        main = "Consumo de frutas, verduras e legumes - 2022")

#Grafico de barras das propoorcoes de consumo de FVL antes e 2022
#tab_D3D4 <- merge(as.data.frame(tab_D3), as.data.frame(tab_D4), 
#                  by = "Var1", all.x = TRUE, all.y = TRUE)

#Tabela  de contingencia e Gráfico antes e depois - FVL
# tb_FLV<- table(antes= d$D3_2,depois=d$D4_2)
# addmargins(tb_FLV)
# prop_FLV<- prop.table(tb_FLV)*100
# addmargins(prop_FLV)
# barplot(prop_FLV, ylab = "Freq. rel. (%)", xlab= "Antes", beside = TRUE, 
#         main = "Consumo de frutas legumes e verduras em 2022 de acordo com o consumo de antes da pandemia",col = c("chocolate","cadetblue","antiquewhite4", "darkmagenta","dodgerblue4"), ylim = c(0,30))
# legend("topleft",levels(d$D3_2), col=c("chocolate","cadetblue","antiquewhite4", "darkmagenta","dodgerblue4") ,pch=16)

# ++++++++++++++++++++++++++++++++++++++++++++++++++
d$D5 <- as.factor(d$D5)
summary(d$D5)
Freq(d$D5)
levels(d$D5)[4] <- NA
niveis5 <- levels(d$D5)
niveis.ordem5 <- c(4,1,2,3,5)
niveis5[niveis.ordem5]  # tá certinho?
d$D5_2 <- ordered(d$D5, levels = niveis5[niveis.ordem5])
Freq(d$D5_2)

#Simplificar o nome das categorias
d$D5_2 <- Recode(d$D5_2,
                 "Raramente" = "Raramente ou nunca",
                 "1 a 2 dias" = "1 a 2 dias por semana",
                 "3 a 4 dias"  = "3 a 4 dias por semana",
                 "5 a 6 dias" = "5 a 6 dias por semana",
                 "Todos os dias" = "Todos os dias (inclusive sábado e domingo)"
)

Freq(d$D5_2,useNA = "always")

#Grafico de barras das proporcoes de consumo de ultraprocessados -antes
tab_D5 <- round(prop.table(table(d$D5_2))*100, digits = 1)
tab_D5
#barplot(tab_D5, col = "green", ylab = "Freq. relativa (%)",
#        main = "Consumo de ultraprocessados - Antes")


# ++++++++++++++++++++++++++++++++++++++++++++++++++
d$D6 <- as.factor(d$D6)
summary(d$D6)
Freq(d$D6)
levels(d$D6)[4] <- NA
niveis6 <- levels(d$D6)
niveis.ordem6 <- c(4,1,2,3,5)
niveis6[niveis.ordem6]  # tá certinho?
d$D6_2 <- ordered(d$D6, levels = niveis6[niveis.ordem6])
Freq(d$D6_2)
#Simplificar o nome das categorias
d$D6_2 <- Recode(d$D6_2,
                 "Raramente" = "Raramente ou nunca",
                 "1 a 2 dias" = "1 a 2 dias por semana",
                 "3 a 4 dias"  = "3 a 4 dias por semana",
                 "5 a 6 dias" = "5 a 6 dias por semana",
                 "Todos os dias" = "Todos os dias (inclusive sábado e domingo)"
)
Freq(d$D6_2,useNA = "always")

#D6 em dois grupos de consumo de ultraprocessados - objetivo 5
d$D6_3 <- Recode(d$D6_2,
                 "Saudável" = "Raramente",
                 "Não Saudável" = c("1 a 2 dias por semana","1 a 2 dias" ,"3 a 4 dias" ,"5 a 6 dias", "Todos os dias")
)

Freq(d$D6_3,useNA = "always")

#tabela 2X2 piora e consumo de ultraprocessados
addmargins(table(d$piorou.saude, d$D6_3))
#tabela 2X2 estado de saúde antes e consumo de ultraprocessados
addmargins(table(d$estado.saude.antes, d$D6_3))

#incidencia de piora na percepção - categoria saudavel - ultraprocessados
(76/(470-(4+53+9)))*100 #18.81188
#incidencia de piora na percepção - categoria não saudavel - ultraprocessados
(121/(561-(4+103+14)))*100 #27.5
#incidencia de piora na percepção - geral - ultraprocessados
(197/(1031-(8+156+23)))*100  #23.34123

#grafico objetivo 6 - FVL

data6ultra <- matrix(c(18.8,27.5,23.4), nrow=3)
rownames(data6ultra) <- c("Saudável","Não saudável","Geral")

# barplot
barplot(data6ultra, 
        col=colors()[c(23,89,12)] , 
        border="white", 
        font.axis=2, 
        beside=T, 
        ylab="Incidência %", 
        xlab=NULL,
        ylim = c(0,40),
        font.lab=2
        ) 
legend("top", # Coordinates
       legend = rownames(data6ultra),
       col = colors()[c(23,89,12)],
       pch = 15,
       title = "Consumo de Ultraprocessados",
       xpd = TRUE,
       horiz = TRUE
) 


#Grafico de barras das proporcoes de consumo de ultraprocessados -antes
tab_D6 <- round(prop.table(table(d$D6_2))*100, digits = 1)
tab_D6
barplot(tab_D6, col = "orange", ylab = "Freq. relativa (%)",
        main = "Consumo de ultraprocessados - 2022")


#Tabela com as freq antes e em 2022  - Consumo de ultraprocessados
# tab5_2 <-as.data.frame(Freq(d$D5_2))
# tab6_2 <-as.data.frame(Freq(d$D6_2))
# tab_43 <- merge(tab5_2, tab6_2,by = "level", all.x = TRUE, all.y = TRUE, sort = FALSE)
# tab_43<- tab_43[c(1:3,6,7)]
# tab_43$perc.x <- tab_43$perc.x*100
# tab_43$perc.y <- tab_43$perc.y*100
# tab_43$perc.x <- round(tab_43$perc.x, digits = 1)
# tab_43$perc.y <- round(tab_43$perc.y, digits = 1)
# soma43 <- round(colSums(tab_43[,2:5]), digits = 0)
# soma43<- c("Total",soma43)
# tab_43<- rbind(tab_43,soma43)
# names(tab_43) <- c("Frequencia","Freq. antes", "Perc. antes", "Freq. 2022", "Perc. 2022")
#library("xlsx")
#write.xlsx(tab_43, "Tabela 43 Ultraprocessados.xlsx")

#Tabela  de contingencia e Gráfico antes e depois - Ultraprocessados
# tb_Ultra<- table(antes= d$D5_2,depois=d$D6_2)
# addmargins(tb_Ultra)
# prop_Ultra<- prop.table(tb_Ultra)*100
# addmargins(prop_Ultra)
# barplot(prop_Ultra, ylab = "Freq. rel. (%)", xlab= "Antes", beside = TRUE, 
#         main = "Consumo de alimentos ultraprocessados em 2022 de acordo com o consumo de antes da pandemia",col = c("chocolate","cadetblue","antiquewhite4", "darkmagenta","dodgerblue4"), ylim = c(0,40))
# legend("topright",levels(d$D5_2), col=c("chocolate","cadetblue","antiquewhite4", "darkmagenta","dodgerblue4") ,pch=16)

# ++++++++++++++++++++++++++++++++++++++++++++++++++
d$D7 <- as.factor(d$D7)
summary(d$D7)
Freq(d$D7)
levels(d$D7)[4] <- NA
niveis7 <- levels(d$D7)
niveis.ordem7 <- c(4,1,2,3,5)
niveis7[niveis.ordem7]  # tá certinho?
d$D7_2 <- ordered(d$D7, levels = niveis7[niveis.ordem7])
Freq(d$D7_2)

#Simplificar o nome das categorias
d$D7_2 <- Recode(d$D7_2,
                 "Raramente" = "Raramente ou nunca",
                 "1 a 2 dias" = "1 a 2 dias por semana",
                 "3 a 4 dias"  = "3 a 4 dias por semana",
                 "5 a 6 dias" = "5 a 6 dias por semana",
                 "Todos os dias" = "Todos os dias (inclusive sábado e domingo)"
)
Freq(d$D7_2,useNA = "always")

#Grafico de barras das proporcoes de consumo de bebidas açucaradas -antes
# tab_D7 <- round(prop.table(table(d$D7_2))*100, digits = 1)
# tab_D7
# barplot(tab_D7, col = "pink", ylab = "Freq. relativa (%)",
#         main = "Consumo de bebidas açucaradas - Antes")
# ++++++++++++++++++++++++++++++++++++++++++++++++++
d$D8 <- as.factor(d$D8)
summary(d$D8)
Freq(d$D8, useNA = "always")
levels(d$D8)[4] <- NA
niveis8 <- levels(d$D8)
niveis.ordem8 <- c(4,1,2,3,5)
niveis8[niveis.ordem8]  # tá certinho?
d$D8_2 <- ordered(d$D8, levels = niveis8[niveis.ordem8])
Freq(d$D8_2)

#Simplificar o nome das categorias
d$D8_2 <- Recode(d$D8_2,
                 "Raramente" = "Raramente ou nunca",
                 "1 a 2 dias" = "1 a 2 dias por semana",
                 "3 a 4 dias"  = "3 a 4 dias por semana",
                 "5 a 6 dias" = "5 a 6 dias por semana",
                 "Todos os dias" = "Todos os dias (inclusive sábado e domingo)"
)
Freq(d$D8_2,useNA = "always")

#Grafico de barras das proporcoes de consumo de bebidas açucaradas -2022
tab_D8 <- round(prop.table(table(d$D8_2))*100, digits = 1)
tab_D8
barplot(tab_D8, col = "aquamarine4", ylab = "Freq. relativa (%)",
        main = "Consumo de bebidas açucaradas - 2022")

#Tabela com as freq antes e em 2022  - Consumo de bebidas acucaradas
# tab7_2 <-as.data.frame(Freq(d$D7_2))
# tab8_2 <-as.data.frame(Freq(d$D8_2))
# tab_44 <- merge(tab7_2, tab8_2,by = "level", all.x = TRUE, all.y = TRUE, sort = FALSE)
# tab_44<- tab_44[c(1:3,6,7)] #selecionar colunas
# tab_44$perc.x <- tab_44$perc.x*100
# tab_44$perc.y <- tab_44$perc.y*100
# tab_44$perc.x <- round(tab_44$perc.x, digits = 1)
# tab_44$perc.y <- round(tab_44$perc.y, digits = 1)
# soma44 <- round(colSums(tab_44[,2:5]), digits = 0)
# soma44<- c("Total",soma44)
# tab_44<- rbind(tab_44,soma44)
# names(tab_44) <- c("Frequencia","Freq. antes", "Perc. antes", "Freq. 2022", "Perc. 2022")
# #library("xlsx")
# write.xlsx(tab_44, "Tabela 44 Refirigerantes.xlsx")

#Tabela  de contingencia e Gráfico antes e depois - Bebidas acucaradas
# tb_Refri<- table(antes= d$D7_2,depois=d$D8_2)
# addmargins(tb_Refri)
# prop_Refri<- prop.table(tb_Refri)*100
# addmargins(prop_Refri)
# barplot(prop_Refri, ylab = "Freq. rel. (%)", xlab= "Antes", beside = TRUE, 
#         main = "Consumo de refrigerantes ou sucos artificiais em 2022 de acordo com o consumo de antes da pandemia",col = c("chocolate","cadetblue","antiquewhite4", "darkmagenta","dodgerblue4"), ylim = c(0,40))
# legend("topright",levels(d$D7_2), col=c("chocolate","cadetblue","antiquewhite4", "darkmagenta","dodgerblue4") ,pch=16)
# 
# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++
d$D9 <- as.factor(d$D9)
summary(d$D9)
Freq(d$D9)
levels(d$D9)[4] <- NA
niveis9 <- levels(d$D9)
niveis.ordem9 <- c(4,1,2,3,5)
niveis9[niveis.ordem9]  # tá certinho?
d$D9_2 <- ordered(d$D9, levels = niveis9[niveis.ordem9])
Freq(d$D9_2)

#Simplificar o nome das categorias
d$D9_2 <- Recode(d$D9_2,
                 "Raramente" = "Raramente ou nunca",
                 "1 a 2 dias" = "1 a 2 dias por semana",
                 "3 a 4 dias"  = "3 a 4 dias por semana",
                 "5 a 6 dias" = "5 a 6 dias por semana",
                 "Todos os dias" = "Todos os dias (inclusive sábado e domingo)"
)
Freq(d$D9_2,useNA = "always")

# ++++++++++++++++++++++++++++++++++++++++++++++++++
d$D10 <- as.factor(d$D10)
summary(d$D10)
Freq(d$D10)
levels(d$D10)[4] <- NA
niveis10 <- levels(d$D10)
niveis.ordem10 <- c(4,1,2,3,5)
niveis10[niveis.ordem10]  # tá certinho?
d$D10_2 <- ordered(d$D10, levels = niveis10[niveis.ordem10])
Freq(d$D10_2)

#Simplificar o nome das categorias
d$D10_2 <- Recode(d$D10_2,
                  "Raramente" = "Raramente ou nunca",
                  "1 a 2 dias" = "1 a 2 dias por semana",
                  "3 a 4 dias"  = "3 a 4 dias por semana",
                  "5 a 6 dias" = "5 a 6 dias por semana",
                  "Todos os dias" = "Todos os dias (inclusive sábado e domingo)"
)
Freq(d$D10_2,useNA = "always")


#Tabela com as freq antes e em 2022  - Almoco preparado em casa
# tab9_2 <-as.data.frame(Freq(d$D9_2))
# tab10_2 <-as.data.frame(Freq(d$D10_2))
# tab_45 <- merge(tab9_2, tab10_2,by = "level", all.x = TRUE, all.y = TRUE, sort = FALSE)
# tab_45<- tab_45[c(1:3,6,7)] #selecionar colunas
# tab_45$perc.x <- tab_45$perc.x*100
# tab_45$perc.y <- tab_45$perc.y*100
# tab_45$perc.x <- round(tab_45$perc.x, digits = 1)
# tab_45$perc.y <- round(tab_45$perc.y, digits = 1)
# soma45 <- round(colSums(tab_45[,2:5]), digits = 0)
# soma45<- c("Total",soma45)
# tab_45<- rbind(tab_45,soma45)
# names(tab_45) <- c("Frequencia","Freq. antes", "Perc. antes", "Freq. 2022", "Perc. 2022")
# #library("xlsx")
# write.xlsx(tab_45, "Tabela 45 almoco casa.xlsx")

# ++++++++++++++++++++++++++++++++++++++++++++++++++
d$D11 <- as.factor(d$D11)
summary(d$D11)
Freq(d$D11)
levels(d$D11)[2] <- NA
niveis11 <- levels(d$D11)
niveis.ordem11 <- c(2,1,3,4,5)
niveis11[niveis.ordem11]  # tá certinho?
d$D11_2 <- ordered(d$D11, levels = niveis11[niveis.ordem11])
Freq(d$D11_2)

# ++++++++++++++++++++++++++++++++++++++++++++++++++
d$D12 <- as.factor(d$D12)
summary(d$D12)
Freq(d$D12)
levels(d$D12)[2] <- NA
niveis12 <- levels(d$D12)
niveis.ordem12 <- c(2,1,3,4,5)
niveis12[niveis.ordem12]  # tá certinho?
d$D12_2 <- ordered(d$D12, levels = niveis12[niveis.ordem12])
Freq(d$D12_2)


#Tabela com as freq antes e em 2022  - Diminuiu quantidade
# tab11_2 <-as.data.frame(Freq(d$D11_2))
# tab12_2 <-as.data.frame(Freq(d$D12_2))
# tab_46 <- merge(tab11_2, tab12_2,by = "level", all.x = TRUE, all.y = TRUE, sort = FALSE)
# tab_46<- tab_46[c(1:3,6,7)] #selecionar colunas
# tab_46$perc.x <- tab_46$perc.x*100
# tab_46$perc.y <- tab_46$perc.y*100
# tab_46$perc.x <- round(tab_46$perc.x, digits = 1)
# tab_46$perc.y <- round(tab_46$perc.y, digits = 1)
# soma46 <- round(colSums(tab_46[,2:5]), digits = 0)
# soma46<- c("Total",soma46)
# tab_46<- rbind(tab_46,soma46)
# names(tab_46) <- c("Frequencia","Freq. antes", "Perc. antes", "Freq. 2022", "Perc. 2022")
# #library("xlsx")
# write.xlsx(tab_46, "Tabela 46 diminuiu quant.xlsx")

# ++++++++++++++++++++++++++++++++++++++++++++++++++
d$D13 <- as.factor(d$D13)
summary(d$D13)
Freq(d$D13)
levels(d$D13)[2] <- NA
niveis13 <- levels(d$D13)
niveis.ordem13 <- c(2,3,1,4)
niveis13[niveis.ordem13]  # tá certinho?
d$D13_2 <- ordered(d$D13, levels = niveis13[niveis.ordem13])
Freq(d$D13_2)

# ++++++++++++++++++++++++++++++++++++++++++++++++++
d$D14 <- as.factor(d$D14)
summary(d$D14)
Freq(d$D14)
levels(d$D14)[2] <- NA
niveis14 <- levels(d$D14)
niveis.ordem14 <- c(2,3,1,4)
niveis14[niveis.ordem14]  # tá certinho?
d$D14_2 <- ordered(d$D14, levels = niveis14[niveis.ordem14])
Freq(d$D14_2)

#Tabela com as freq antes e em 2022  - Faltou dinheiro
# tab13_2 <-as.data.frame(Freq(d$D13_2))
# tab14_2 <-as.data.frame(Freq(d$D14_2))
# tab_47 <- merge(tab13_2, tab14_2,by = "level", all.x = TRUE, all.y = TRUE, sort = FALSE)
# tab_47<- tab_47[c(1:3,6,7)] #selecionar colunas
# tab_47$perc.x <- tab_47$perc.x*100
# tab_47$perc.y <- tab_47$perc.y*100
# tab_47$perc.x <- round(tab_47$perc.x, digits = 1)
# tab_47$perc.y <- round(tab_47$perc.y, digits = 1)
# soma47 <- round(colSums(tab_47[,2:5]), digits = 0)
# soma47<- c("Total",soma47)
# tab_47<- rbind(tab_47,soma47)
# names(tab_47) <- c("Frequencia","Freq. antes", "Perc. antes", "Freq. 2022", "Perc. 2022")
# #library("xlsx")
# write.xlsx(tab_47, "Tabela 47 faltou dinheiro.xlsx")

# ++++++++++++++++++++++++++++++++++++++++++++++++++
d$D15 <- as.factor(d$D15)
summary(d$D15)
levels(d$D15)[2] <- NA
niveis15 <- levels(d$D15)
niveis.ordem15 <- c(2,3,1,4)
niveis15[niveis.ordem15]  # tá certinho?
d$D15_2 <- ordered(d$D15, levels = niveis15[niveis.ordem15])
Freq(d$D15_2)

#Frequencia de Auxilio
tab_48<-as.data.frame(Freq(d$D15_2))
#library("xlsx")
#write.xlsx(tab_48, "Tabela 48 auxilio.xlsx")

# ++++++++++++++++++++++++++++++++++++++++++++++++++
d$D16 <- as.factor(d$D16)
summary(d$D16)

#Nao sei dizer como NA
d$D16_1 <- Recode(d$D16,
                  "Piorou" = c("Piorou muito","Piorou um pouco"),
                  "Melhorou" = "Melhorou",
                  "Permaneceu o mesmo" = "Permaneceu o mesmo",
                  elselevel = NA
)

summary(d$D16_1)

#tabela de contingencia - padrao de consumo e saude antes da pandemia
addmargins(table(d$estado.saude.antes,d$D16_1))

#tabela de contingencia - padrao de consumo e saude depoisda pandemia
addmargins(table(d$estado.saude.agora,d$D16_1))

#Frequencia padrao de consumo alimentar
tab_49<-as.data.frame(Freq(d$D16_1))
#library("xlsx")
#write.xlsx(tab_49, "Tabela 49 padrao alimentar.xlsx")

#Grafico de barras sobre mudanças no padrao de consumo alimentar
tab_D16 <- round(prop.table(table(d$D16_1))*100, digits = 1)
tab_D16
barplot(tab_D16, col = "coral4", ylab = "Freq. relativa (%)",
        main = "Mudanças no padrão de consumo alimentar", ylim = c(0,50))

#tabela estado de saude antes e mudanca no padrao alimentar
tabela344 <- table(est_antes = d$estado.saude.antes, alimentacao = d$D16_1)
addmargins(tabela344)
#Freq. relativa estado de saude antes e mudanca no padrao alimentar
prop_tabela344 <- prop.table(tabela344, margin = 2)*100
addmargins(prop_tabela344, margin = 1)

#tabela estado de saude agora e mudanca no padrao alimentar
tab_agora <- table(est_agora = d$estado.saude.agora, alimentacao = d$D16_1)
addmargins(tab_agora)
#Freq. relativa estado de saude agora e mudanca no padrao alimentar
prop_agora <- prop.table(tab_agora, margin = 2)*100
addmargins(prop_agora, margin = 1)

# ++++++++++++++++++++++++++++++++++++++++++++++++++
d$D17 <- as.factor(d$D17)
levels(d$D17)

#grafico objetivo 4

data <- matrix(c(79.3,78.9,86.9,53.7,76.5,76.9), nrow=3)
colnames(data) <- c("Antes", "Em 2022")
rownames(data) <- c("Piorou","Melhorou","Permaneceu o mesmo")

# Grouped barplot
# Window taller than wider
windows(width = 4, height = 4)

# Save current graphical parameters
opar <- par(no.readonly = TRUE)

# Change the margins of the plot (the first is the bottom margin)
par(mar = c(6, 4.1, 4.1, 2.1))

barplot(data, 
        col=colors()[c(23,89,12)] , 
        border="white", 
        font.axis=2, 
        beside=T, 
        ylab="Prevalência %", 
        ylim = c(0,100),
        font.lab=2)


legend(x = "bottom",
       inset = c(0, -0.4), # You will need to fine-tune the second
       # value depending on the windows size
       legend = rownames(data),
       title = "Padrão alimentar",
       #lty = c(1, 2),
       col = colors()[c(23,89,12)],
       pch = 15,
       xpd = TRUE, # You need to specify this graphical parameter to add
       # the legend outside the plot area
       horiz = TRUE) # Horizontal legend. You can also set the number
# of columns with the argument ncol
# if horiz = FALSE

# Back to the default graphical parameters
on.exit(par(opar))

## salvando ----

save(d, file = "dados2022_limpos.RData")

# use um script novo para fazer as analises. para abrir o arquivo, use o comando
load("dados2022_limpos.RData")


