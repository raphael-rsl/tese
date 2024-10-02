install.packages("dplyr")
install.packages("survey")
install.packages("PNADcIBGE")
library(dplyr)
library(survey)
library(PNADcIBGE)
library(haven)
library(srvyr)
library(ggplot2)
library(ggridges)
library(viridis)
library(forcats)

#Nota Técnica - https://biblioteca.ibge.gov.br/visualizacao/livros/liv102033.pdf
#Informativo - https://biblioteca.ibge.gov.br/visualizacao/livros/liv102035_informativo.pdf
#Autor do pacote - https://rpubs.com/gabriel-assuncao-ibge/pnadc

#S14009 - Prestou serviço por meio de um dos aplicativos listados ou outro tipo de aplicativo não citado anteriomente
#1 = Sim - Esse é o primeiro filtro

#Variáveis de Interesse:
#UF
#V2007 - Sexo
#V2009 - Idade
#V2010 - Raça
#V4006A - Afastamento do trabalho. #3 para Doença/Acidente
#V4009 - Quantos trabalhos?
#V4010 - Código da Ocupação
#V4013 - CNAE
#VD3004 - Nível de Instrução
#VD4002 - Pessoas Ocupadas - 1º Filtro
#VD4009 - Posição na Ocupação - 2º Filtro
#VD4010 - Grupamento de Atividade
#VD4019 - Renda habitual
#VD4031 - Jornada habitual

#S140091 - Táxi
#S140092 - App Transporte
#S140093 - App Entrega
#S140094 - Serviços Gerais

#Era o app que determinava:
#S140111 - Valor
#S140112 - Cliente
#S140113 - Prazo
#S140114 - Forma de Pagamento

#A jornada era influenciada por:
#S140121 - Incentivos/Bônus
#S140122 - Ameaças de bloqueios
#S140123 - Jornada sugerida pelo app
#S140124 - Escolha de jornada independente

variaveis = c("UF", "V2007", "V2009", "V2010", "V4006A", "V4009",
              "V4010", "V4013", "VD3004", "VD4002", "VD4009",
              "VD4010", "VD4019", "VD4031",
              "S140091", "S140092", "S140093", "S140094",
              "S140111", "S140112", "S140113", "S140114",
              "S140114", "S140121", "S140122", "S140123", "S140124")

#pnadc_2022 = get_pnadc(year=2022, topic=4, selected=TRUE, savedir = "C:/Users/raphael.rsl/Desktop/R/PNAD_Plat_Tel")

setwd("C:/Users/raphael.rsl/Desktop/R/PNAD_Plat_Tel")

#pnadc_2022 <- read_pnadc(microdata="PNADC_2022_trimestre4.txt", vars = variaveis, input_txt="input_PNADC_trimestre4_20240425.txt")

pnadc_2022 = read_dta("C:/Users/raphael.rsl/Desktop/R/PNAD_Plat_Tel/pnadc_2022.dta")

pnadc_2022 = pnadc_2022 %>% 
  as_factor()


#SOMENTE APLICATIVO - A BASE EM .DTA JÁ ESTÁ FILTRADA
aplicativo = pnadc_2022 %>% 
  filter(S14009 == "Prestou serviço por meio de um dos aplicativos listados ou outro tipo de aplicativo não citado anteriomente") %>% 
  as_factor()

library(forcats)
###CRIANDO CATEGORIA DE TRABALHO DIGITAL
aplicativo = aplicativo %>% 
  mutate(categoria = (case_when(S140091 == "Sim" | S140092 == "Sim" ~ "Transporte",
                                S140093 == "Sim" ~ "Entrega",
                                S140094 == "Sim" ~ "Outros"))) %>% 
  mutate(categoria = fct_relevel(categoria, "Transporte", "Entrega", "Outros"))
#Número bate com a informação da página 3 do informativo do IBGE

#Teste de Categorias
categorias_numeros = aplicativo %>%
  mutate(count=1) %>%
  group_by(categoria) %>% 
  summarise(peso = sum(V1028, na.rm = T), absoluto = sum(count, na.rm = T))

#Número bate com a tabela 9432 do SIDRA
#https://sidra.ibge.gov.br/tabela/9432#resultado

categorias_bruto = aplicativo %>%
  mutate(count=1) %>%
  group_by(S140091, S140092, S140093, S140094) %>% 
  summarise(peso = sum(V1028, na.rm = T), absoluto = sum(count, na.rm = T))
#Cerca de 200 mil que trabalham para mais de uma categoria

####CRIANDO HORA/RENDA
aplicativo = aplicativo %>%
  mutate(valor_hora = VD4019/(VD4031*4))

#####CRIANDO CAPITAL/INTERIOR
aplicativo = aplicativo %>% 
  mutate(capital_interior = (case_when(V1023 == "Capital" ~ "Capital/RM/RIDE",
                                       V1023 == "Resto da RM (Região Metropolitana, excluindo a capital)" ~ "Capital/RM/RIDE",
                                       V1023 == "Resto da RIDE (Região Integrada de Desenvolvimento Econômico, excluindo a capital) " ~ "Capital/RM/RIDE",
                                       V1023 == "Resto da UF  (Unidade da Federação, excluindo a região metropolitana e a RIDE)" ~ "Interior"))) %>% 
        drop_na(V1023)

capital = aplicativo %>%
  mutate(count=1) %>%
  group_by(capital_interior) %>% 
  summarise(peso = sum(V1028, na.rm = T), absoluto = sum(count, na.rm = T))

#####CRIANDO ESCOLARIDADE MAIS RESTRITA

aplicativo = aplicativo %>% 
  mutate(escolaridade_compacta = (case_when(VD3004 == "Sem instrução e menos de 1 ano de estudo" ~ "Até Fundamental",
                                            VD3004 == "Fundamental incompleto ou equivalente" ~ "Até Fundamental",
                                            VD3004 == "Fundamental completo ou equivalente" ~ "Até Fundamental",
                                            VD3004 == "Médio incompleto ou equivalente" ~ "Até Médio",
                                            VD3004 == "Médio completo ou equivalente" ~ "Até Médio",
                                            VD3004 == "Superior incompleto ou equivalente" ~ "Superior Incompleto/Completo",
                                            VD3004 == "Superior completo" ~ "Superior Incompleto/Completo")))

escolaridade_num = aplicativo %>%
  mutate(count=1) %>%
  group_by(escolaridade_compacta) %>% 
  summarise(peso = sum(V1028, na.rm = T), absoluto = sum(count, na.rm = T))

escolaridade_num2 = aplicativo %>%
  mutate(count=1) %>%
  group_by(VD3004) %>% 
  summarise(peso = sum(V1028, na.rm = T), absoluto = sum(count, na.rm = T))

####CRIANDO RAÇA COMPACTA
aplicativo = aplicativo %>%
  mutate(raca = (case_when(V2010 == "Branca" ~ "Não-Negros",
                           V2010 == "Preta" ~ "Negros",
                           V2010 == "Amarela" ~ "Não-Negros",
                           V2010 == "Parda" ~ "Negros",
                           V2010 == "Indígena" ~ "Não-Negros")))

#####CRIANDO RACA_GENERO
aplicativo <- aplicativo %>%
  mutate(
    one = 1,
    sexo_e_raca = factor(
      case_when(
        V2007 == "Homem" & V2010 == "Branca" ~ "Homens brancos",
        V2007 == "Mulher" & V2010 == "Branca" ~ "Mulheres brancas",
        V2007 == "Homem" & V2010 %in% c("Preta","Parda") ~ "Homens negros",
        V2007 == "Mulher" & V2010 %in% c("Preta","Parda") ~ "Mulheres negras"
      ),
      levels = c(
        "Homens brancos", "Mulheres brancas", "Homens negros", "Mulheres negras"
      )
    )
  )


###CRIANDO MACRORREGIÕES
aplicativo = aplicativo %>% 
  mutate (regiao = factor(case_when(
    UF %in% c("Rondônia", "Acre", "Amazonas", "Roraima", "Pará", "Amapá","Tocantins") ~ "Norte",
    UF %in% c("Maranhão", "Piauí", "Ceará", "Rio Grande do Norte", "Paraíba", "Pernambuco", "Alagoas", "Sergipe", "Bahia") ~ "Nordeste",
    UF %in% c("Minas Gerais", "Espírito Santo",  "Rio de Janeiro", "São Paulo") ~ "Sudeste",
    UF %in% c("Paraná", "Santa Catarina", "Rio Grande do Sul") ~ "Sul",
    UF %in% c("Mato Grosso do Sul", "Mato Grosso", "Goiás", "Distrito Federal") ~ "Centro-Oeste"),
  levels = c("Norte", "Nordeste", "Sudeste", "Sul", "Centro-Oeste")))


#####CRIA PROXY DE EXPERIÊNCIA NO MUNDO DO TRABALHO
#aplicativo = aplicativo %>%
#  mutate(experiencia = V2009^2)


########CRIA REGIÕES ESPECIFICIAS
aplicativo = aplicativo %>% 
  mutate (regiao_especial = factor(case_when(
    UF == "São Paulo" ~ "São Paulo",
    UF == "Rio de Janeiro" ~ "Rio de Janeiro",
    UF %in% c("Minas Gerais", "Espírito Santos", "Distrito Federal") ~ "MG/ES/DF",
    UF %in% c("Rondônia", "Acre", "Amazonas", "Roraima", "Pará", "Amapá","Tocantins") ~ "Norte",
    UF %in% c("Maranhão", "Piauí", "Ceará", "Rio Grande do Norte", "Paraíba", "Pernambuco", "Alagoas", "Sergipe", "Bahia") ~ "Nordeste",
    UF %in% c("Paraná", "Santa Catarina", "Rio Grande do Sul") ~ "Sul",
    UF %in% c("Mato Grosso do Sul", "Mato Grosso", "Goiás") ~ "Centro-Oeste - DF"),
    levels = c("São Paulo", "Rio de Janeiro", "MG/ES/DF", "Norte", "Nordeste","Sul", "Centro-Oeste - DF")))

###CRIA RAÇA BINÁRIO
aplicativo <- aplicativo %>%
  mutate(
    one = 1,
    raca_binario = factor(
      case_when(
        V2010 == "Branca" ~ "Brancos",
        V2010 %in% c("Preta", "Parda") ~ "Negros"
      ),
      levels = c(
        "Brancos", "Negros"
      )
    )
  )

####TESTES COM RELATÓRIO IBGE
renda = aplicativo %>%
  group_by(S140091, S140092, S140093, S140094) %>% 
  summarise(total = mean(VD4019, vartype = "ci"), na.rm=T)

UF = aplicativo %>%
  mutate(count=1) %>%
  group_by(UF) %>% 
  summarise(peso = sum(V1028, na.rm = T), absoluto = sum(count, na.rm = T))

regiao = aplicativo %>%
  mutate(count=1) %>%
  group_by(regiao) %>% 
  summarise(peso = sum(V1028, na.rm = T), absoluto = sum(count, na.rm = T))

sexo = aplicativo %>%
  mutate(count=1) %>%
  group_by(V2007) %>% 
  summarise(peso = sum(V1028, na.rm = T), absoluto = sum(count, na.rm = T))

raca = aplicativo %>%
  mutate(count=1) %>%
  group_by(raca) %>% 
  summarise(peso = sum(V1028, na.rm = T), absoluto = sum(count, na.rm = T))

trabalhos = aplicativo %>%
  mutate(count=1) %>%
  group_by(V4009) %>% 
  summarise(peso = sum(V1028, na.rm = T), absoluto = sum(count, na.rm = T))

profissoes = aplicativo %>%
  mutate(count=1) %>%
  group_by(V4013) %>% 
  summarise(peso = sum(V1028, na.rm = T), absoluto = sum(count, na.rm = T))
#78 profissões

profissoes_taxi = aplicativo %>%
  mutate(count=1) %>%
  filter(S140091 == "Sim") %>% 
  group_by(V4013) %>% 
  summarise(peso = sum(V1028, na.rm = T), absoluto = sum(count, na.rm = T))
#2 profissões

profissoes_transporte = aplicativo %>%
  mutate(count=1) %>%
  filter(S140092 == "Sim") %>% 
  group_by(V4013) %>% 
  summarise(peso = sum(V1028, na.rm = T), absoluto = sum(count, na.rm = T))
#2 profissões

profissoes_entrega = aplicativo %>%
  mutate(count=1) %>%
  filter(S140093 == "Sim") %>% 
  group_by(V4013) %>% 
  summarise(peso = sum(V1028, na.rm = T), absoluto = sum(count, na.rm = T))
#18 profissões

profissoes_outro = aplicativo %>%
  mutate(count=1) %>%
  filter(S140094 == "Sim") %>% 
  group_by(V4013) %>% 
  summarise(peso = sum(V1028, na.rm = T), absoluto = sum(count, na.rm = T))
#74 profissões

total = aplicativo %>% 
  mutate(count=1) %>%
  summarise(peso = sum(V1028, na.rm = T), absoluto = sum(count, na.rm = T))
#1.489.891
#2.404
#1490000 na tabela 9432 do SIDRA/IBGE - https://sidra.ibge.gov.br/tabela/9432#resultado

taxi = aplicativo %>%
  mutate(count=1) %>%
  group_by(S140091) %>% 
  summarise(peso = sum(V1028, na.rm = T), absoluto = sum(count, na.rm = T))
#206.666

app_transporte = aplicativo %>%
  mutate(count=1) %>%
  group_by(S140092) %>% 
  summarise(peso = sum(V1028, na.rm = T), absoluto = sum(count, na.rm = T))
#703.784

app_entrega = aplicativo %>%
  mutate(count=1) %>%
  group_by(S140093) %>% 
  summarise(peso = sum(V1028, na.rm = T), absoluto = sum(count, na.rm = T))
#588.628

app_outros = aplicativo %>%
  mutate(count=1) %>%
  group_by(S140094) %>% 
  summarise(peso = sum(V1028, na.rm = T), absoluto = sum(count, na.rm = T))
#196.943

#Até o momento as informações estão batendo com o relatório do IBGE - Tabelas 1 e 2 do Informativo

#######TAXI - DEPENDENCIA ###############
#As tabelas das páginas 8 e 9 do informativo suprem essa análise

dependencia_taxi_1 = aplicativo %>% 
  mutate(count=1) %>%
  filter(S140091 == "Sim") %>% 
  #filter(S140111 == "Sim") %>% 
  group_by(S140111) %>% 
  summarise(peso = sum(V1028, na.rm = T), absoluto = sum(count, na.rm = T))

dependencia_taxi_2 = aplicativo %>% 
  mutate(count=1) %>%
  filter(S140091 == "Sim") %>% 
  filter(S140112 == "Sim") %>% 
  summarise(peso = sum(V1028, na.rm = T), absoluto = sum(count, na.rm = T))

dependencia_taxi_3 = aplicativo %>% 
  mutate(count=1) %>%
  filter(S140091 == "Sim") %>% 
  filter(S140113 == "Sim") %>% 
  summarise(peso = sum(V1028, na.rm = T), absoluto = sum(count, na.rm = T))

dependencia_taxi_4 = aplicativo %>% 
  mutate(count=1) %>%
  filter(S140091 == "Sim") %>% 
  filter(S140114 == "Sim") %>% 
  summarise(peso = sum(V1028, na.rm = T), absoluto = sum(count, na.rm = T))

dependencia_taxi_geral = full_join(dependencia_taxi_1, dependencia_taxi_2)

#######TAXI - JORNADA ###############
#As tabelas das páginas 8 e 9 do informativo suprem essa análise

jornada_taxi_1 = aplicativo %>% 
  mutate(count=1) %>%
  filter(S140091 == "Sim") %>% 
  filter(S140121 == "Sim") %>% 
  summarise(peso = sum(V1028, na.rm = T), absoluto = sum(count, na.rm = T))

jornada_taxi_2 = aplicativo %>% 
  mutate(count=1) %>%
  filter(S140091 == "Sim") %>% 
  filter(S140122 == "Sim") %>% 
  summarise(peso = sum(V1028, na.rm = T), absoluto = sum(count, na.rm = T))

jornada_taxi_3 = aplicativo %>% 
  mutate(count=1) %>%
  filter(S140091 == "Sim") %>% 
  filter(S140123 == "Sim") %>% 
  summarise(peso = sum(V1028, na.rm = T), absoluto = sum(count, na.rm = T))

jornada_taxi_4 = aplicativo %>% 
  mutate(count=1) %>%
  filter(S140091 == "Sim") %>% 
  filter(S140124 == "Sim") %>% 
  summarise(peso = sum(V1028, na.rm = T), absoluto = sum(count, na.rm = T))

#######REGRESSÕES###############
####NÃO SERVEM POIS NÃO ACEITAM PESO############

library(psych)
library(MKinfer)

#describeBy(aplicativo, aplicativo$V2007, omit = TRUE)

#Teste t independente
#Gênero (VI) prediz renda (VD)?
teste_sexo = t.test(VD4019 ~ V2007, aplicativo)
teste_sexo
#t = -1.3708, df = 494.09, p-value = 0.1711
#Gênero não prediz renda para trabalhadores por aplicativo

efeito = cohen.d(aplicativo$VD4019, aplicativo$V2007)
efeito

#ANOVA SIMPLES
library(rstatix)

describeBy(aplicativo$VD4019, group = aplicativo$V2010)

#Raça (VI) impacta renda (VD)?
anova = oneway.test(formula = VD4019 ~ V2010, data = aplicativo, 
                    var.equal = FALSE)
anova
#p<0,05

#Impacta. Em qual relação?
#Realizar testes posthoc

post_hoc = pairwise_t_test(formula = VD4019 ~ V2010, data = aplicativo, 
                           p.adjust.method = "bonferroni")
post_hoc
#Na relação Branca-Parda e levemente na relação Branca-Preta
#Tamanho do Efeito

efeito = cohens_d(formula = VD4019 ~ V2010, data = aplicativo,
                  ci=TRUE, ci.type = "bca", nboot = 1000)
efeito
#não funciona

#d de Cohen para r de Pearson
d2r(efeito$effsize)


# #############LOGLINEAR
#O tipo de trabalho (VI) impacta na percepção de que a jornada de trabalho é influenciada por (i) incentivos/bônus?
#O tipo de trabalho (VI) impacta na percepção de que a jornada de trabalho é influenciada por (i) ameaças?
#O tipo de trabalho (VI) impacta na percepção de que a jornada de trabalho é influenciada por (i) turnos do app?
#O tipo de trabalho (VI) impacta na percepção de que a jornada de trabalho é influenciada por (i) escolha independente?

#A escolaridade (VI) impacta na percepção de que a jornada de trabalho é influenciada por (i) incentivos/bônus? 
#A escolaridade (VI) impacta na percepção de que a jornada de trabalho é influenciada por (i) ameaças?
#A escolaridade (VI) impacta na percepção de que a jornada de trabalho é influenciada por (i) turnos do app?
#A escolaridade (VI) impacta na percepção de que a jornada de trabalho é influenciada por (i) escolha independente?

#########FIM DE TESTES DE REGRESSÃO SEM PESO###########


################SURVEY#################


options(survey.lonely.psu="adjust")
aplicativo_survey <- 
  survey::svydesign(id = ~UPA,
                    strata = ~Estrato,
                    weights = ~V1028,
                    nest=TRUE,
                    data=aplicativo)

aplicativo_survey = as_survey(aplicativo_survey)

svymean(~VD4019, aplicativo_survey, na.rm = T, vartype = "cv")
# R$ 2.794,90 - VD4019 - HABITUAL - TODOS TRABALHOS
svymean(~VD4020, aplicativo_survey, na.rm = T, vartype = "cv")
# R$ 2.739,70 - VD4020 - EFETIVO - TODOS TRABALHOS


###########IBGE USA ESSA VARIÁVEL NO INFORMATIVO##############
svymean(~VD4016, aplicativo_survey, na.rm = T, vartype = "cv")
# R$ 2645.1 - VD4016 - HABITUAL - TRABALHO PRINCIPAL

svyby(~VD4016, ~categoria, aplicativo_survey, svymean, na.rm = T, 
      keep.var = TRUE)
########################################################

svymean(~VD4017, aplicativo_survey, na.rm = T, vartype = "cv")
# R$ 2.606,1 - VD4017 - EFETIVO - TRABALHO PRINCIPAL


svyby(~VD4016, ~categoria, aplicativo_survey, svyquantile,
      quantiles = c(0, 0.25, 0.5, 0.75, 1), ci=TRUE, na.rm = TRUE, 
      keep.var = TRUE)

svyby(~VD4031, ~categoria, aplicativo_survey, svyquantile,
      quantiles = c(0, 0.25, 0.5, 0.75, 1), ci=TRUE, na.rm = TRUE, 
      keep.var = TRUE)

#####RETIRAR OUTLIERS

aplicativo_survey = aplicativo_survey %>% 
  filter(VD4019 > 1) %>% 
  filter(VD4019 != "NA") %>% 
  filter(VD4019 < 10000) %>% 
  filter(VD4016 > 1) %>% 
  filter(VD4016 != "NA") %>% 
  filter(VD4016 < 10000) %>%
  filter(VD4031 > 1) %>% 
  filter(VD4031 != "NA") %>% 
  filter(VD4031 < 100) %>% 
  filter(valor_hora <50)


###HISTOGRAMAS E BOXPLOTS

###USADO TESE
svyboxplot(formula=VD4016~categoria, aplicativo_survey, main="Boxplot Renda Mensal Habitual \n por Categoria",
           col= c("grey", "red", "purple", "yellow"),horizontal = T)

#all.outliers = TRUE foi testado


svyboxplot(formula=VD4031~categoria, aplicativo_survey, main="Boxplot Jornada Semanal Habitual \n por Categoria",
           col= c("grey", "red", "purple", "yellow"),horizontal = T)


svyboxplot(formula=valor_hora~categoria, aplicativo_survey, main="Boxplot Jornada Semanal Habitual \n por Categoria",
           col= c("grey", "red", "purple", "yellow"),horizontal = T)



#########NÃO USADO A PARTIR DAQUI
svyhist(~as.numeric(VD4031), aplicativo_survey, main = "Histograma", xlab = "Jornada")
svyboxplot(formula=VD4031~1, aplicativo_survey, main="Boxplot Jornada Habitual", 
           col="purple", horizontal = T)
svyboxplot(formula=valor_hora~1, aplicativo_survey, main="Boxplot Valor-Hora Habitual")
svyboxplot(formula=VD4019~V2007, aplicativo_survey, main="Boxplot Renda Habitual por Gênero",
           col= c("grey", "red"),horizontal = T)
svyboxplot(formula=VD4019~categoria, aplicativo_survey, main="Boxplot Renda Habitual por Categoria",
           col= c("grey", "red", "purple", "yellow"),horizontal = T)
svyboxplot(formula=VD4019~capital_interior, aplicativo_survey, main="Boxplot da Renda")
svyplot(formula=VD4019~VD4031, aplicativo_survey, style="bubble", 
        xlab="Número de Horas Efetivamente Trabalhadas", ylab="Rendimento Efetivo")

#testes
svytotal(~V2007, aplicativo_survey, na.rm = T)

svyby(~VD4019, ~V2010, aplicativo_survey, svymean, na.rm = T, vartype = "cv")
svyby(~VD4019, ~raca, aplicativo_survey, svymean, na.rm = T, vartype = "cv")
svyby(~VD4019, ~categoria, aplicativo_survey, svymean, na.rm = T, vartype = "cv")
svyby(~VD4019, ~V2010, aplicativo_survey, svymean, na.rm = T, vartype = "cv")
svymean(~VD4019, aplicativo_survey, na.rm = T, vartype = "cv")
svyby(~VD4019, ~sexo_e_raca, aplicativo_survey, svymean, na.rm = T, vartype = "cv")


svyby(~S140121, ~V2007, aplicativo_survey, svytotal, na.rm = T)

#####TESTES T
#renda por sexo #não
svyttest(formula=VD4019~V2007, aplicativo_survey)
#t = -0.50119, df = 1499, p-value = 0.6163

#jornada por sexo #sim
svyttest(formula=VD4031~V2007, aplicativo_survey)
#t = -5.1653, df = 1488, p-value = 2.725e-07

#renda por graduação #p = 0.02
svyttest(formula=VD4019~V3007, aplicativo_survey)
#t = -1.9023, df = 28, p-value = 0.06747


#REGRESSÃO LOGÍSTICA MULTINOMIAL (GLM)


#Nesse trabalho, a jornada de trabalho era influenciada por quais motivos:
#S140121 = Incentivos/bônus, promoções que mudam os preços?
#S140122 = Ameaças de punições, bloqueios realizados pelo aplicativo/plataforma?
#S140123 = Turnos, dias sugeridos pelo aplicativo/plataforma?
#S140124 = Possibilidade de escolha dos dias e horários de forma independente? 

library(jtools)

#aplicativo_survey = aplicativo_survey %>% 
#  filter (S140121 != "Não sabe")

# aplicativo_survey_novo <- aplicativo_survey %>% 
#   mutate(S140121 = case_when(S140121 == "Não sabe" ~ NA_real_,
#                              FALSE ~ as.numeric(as.character(S140121)))) %>% 
#   mutate(S140122 = case_when(S140122 == "Não sabe" ~ NA_real_,
#                              TRUE ~ as.numeric(as.character(S140122)))) %>% 
#   mutate(S140123 = case_when(S140123 == "Não sabe" ~ NA_real_,
#                              TRUE ~ as.numeric(as.character(S140123)))) %>% 
#   mutate(S140124 = case_when(S140124 == "Não sabe" ~ NA_real_,
#                              TRUE ~ as.numeric(as.character(S140124))))


#https://cran.r-project.org/web/packages/jtools/vignettes/summ.html

incentivos = svyglm(S140121 ~ categoria + capital_interior + V2010 + VD4012 + regiao +
                      escolaridade_compacta + V2007 + V2009 + VD4019 + VD4031,
                    aplicativo_survey, family = "quasibinomial")
summary(incentivos)
summ(incentivos, exp = T)

ameacas = svyglm(S140122 ~ categoria + capital_interior + V2010 + VD4012 + regiao +
                   escolaridade_compacta + V2007 + V2009 + VD4019 + VD4031,
                 aplicativo_survey, family = "quasibinomial")
summary(ameacas)
summ(ameacas, exp = T)

turnos_app = svyglm(S140123 ~ categoria + capital_interior + V2010 + VD4012 + regiao +
                      escolaridade_compacta + V2007 + V2009 + VD4019 + VD4031 + experiencia,
                    aplicativo_survey, family = "quasibinomial")
summary(turnos_app)
summ(turnos_app, exp = T)

# independencia = svyglm(S140124 ~ VD2006 + V3007 + V2010, aplicativo_survey,family = "binomial")
# independencia = svyglm(S140124 ~ V2010, aplicativo_survey,family = "binomial")
# independencia = svyglm(S140124 ~ VD2006 + V3007, aplicativo_survey,family = "binomial")

aplicativo_survey = aplicativo_survey %>% 
  filter (S140124 != "Não sabe") %>%
  filter (S140121 != "Não sabe") %>%
  filter (S140122 != "Não sabe") %>%
  filter (S140123 != "Não sabe") %>%
  filter (categoria != "Outros")

independencia = svyglm(S140124 ~ categoria + capital_interior + V2010 + VD4012 + regiao +
                         escolaridade_compacta + V2007 + V2009 + VD4019 + VD4031 + experiencia,
                       aplicativo_survey, family = "quasibinomial")
summary(independencia)
summ(independencia)
summ(independencia, exp = T)
confint(independencia)

library(broom.mixed)
plot_summs(independencia, robust = TRUE)
plot_summs(independencia, plot.distributions = TRUE, inner_ci_level = .5,
           omit.coefs = c("(Intercept)","V2010Preta", "V2010Amarela","V2010Parda",
                          "V2010Indígena", "regiaoNordeste", "regiaoSudeste",
                          "regiaoSul", "regiaoCentro-Oeste",
                          "escolaridade_compactaSuperior Incompleto/Completo",
                          "V2009", "VD4019", "VD4031", "experiencia"))

plot_summs(incentivos, plot.distributions = TRUE, inner_ci_level = .5,
           omit.coefs = c("(Intercept)","V2010Preta", "V2010Amarela","V2010Parda",
                          "V2010Indígena", "regiaoNordeste", "regiaoSudeste",
                          "regiaoSul", "regiaoCentro-Oeste",
                          "escolaridade_compactaAté Médio",
                          "V2009", "VD4019", "VD4031", "experiencia"))

plot_summs(incentivos, plot.distributions = TRUE, inner_ci_level = .5,
           omit.coefs = c("(Intercept)","V2010Preta", "V2010Amarela","V2010Parda",
                          "V2010Indígena", "regiaoNordeste", "regiaoSudeste",
                          "regiaoSul", "regiaoCentro-Oeste",
                          "escolaridade_compactaAté Médio",
                          "V2009", "VD4019", "VD4031", "experiencia"))


#renda
teste = svyglm(formula = VD4019 ~ categoria + capital_interior + VD4012 + 
                 escolaridade_compacta + V2007 + V2009 + VD4019 + VD4031+
                 regiao + raca_binario, aplicativo_survey)
# teste = svyglm(formula = VD4010 ~ categoria, aplicativo_survey)
# teste = svyglm(formula = VD4019 ~ categoria + capital_interior + V2010, aplicativo_survey)

summary(teste)
summ(teste, exp = F, confint = T)

plot_summs(teste, plot.distributions = TRUE, inner_ci_level = .9, rescale.distributions = T,
           omit.coefs = c("(Intercept)","V2010Preta", "V2010Amarela", "V2010Indígena",
                          "V2007Mulher", "capital_interiorInterior"))
svyby(~ VD4019, ~ categoria, aplicativo_survey, svymean, na.rm = T)





##########GRÁFICOS
library(ggplot2)
library(ggridges)

aplicativo = aplicativo %>% 
  filter(VD4020 < 10000) %>% 
  filter(VD4019 < 10000)

#renda por categoria - violin
  ggplot(aplicativo,aes(x=VD4019, color=categoria, fill=categoria)) +
  geom_density(alpha=0.4) +
  scale_fill_viridis(discrete=TRUE) +
  scale_color_viridis(discrete=TRUE) +
  #geom_text(aes(label=categoria, color=categoria), hjust=0, size=4.5) +
  #theme_ipsum() +
  facet_wrap(~categoria) +
  theme(
    legend.position="top"
  ) +
  ylab("") +
  xlab("Renda")

  
  #jornada por categoria - violin
  ggplot(aplicativo,aes(x=VD4031, color=categoria, fill=categoria)) +
    geom_density(alpha=0.4) +
    scale_fill_viridis(discrete=TRUE) +
    scale_color_viridis(discrete=TRUE) +
    #geom_text(aes(label=categoria, color=categoria), hjust=0, size=4.5) +
    #theme_ipsum() +
    facet_wrap(~categoria) +
    theme(
      legend.position="top"
    ) +
    ylab("") +
    xlab("Renda")
  
    
#renda por categoria - ridgeline
  ggplot(aplicativo,aes(x=VD4019, y=categoria, fill=categoria)) +
    geom_density_ridges() +
    theme_ridges() + 
    theme(legend.position = "none")

#violin
  #RENDA E DISTRIBUIÇÃO POR CATEGORIA
  ggplot(aplicativo, aes(x=categoria, y=VD4019, fill=categoria, color=categoria)) +
    geom_violin(width=2.1, size=0.2) +
    geom_jitter(color="black", size=0.4, alpha=0.9) +
    scale_fill_viridis(discrete=TRUE) +
    scale_color_viridis(discrete=TRUE) +
    #theme_ipsum() +
    theme(
      legend.position="none"
    ) +
    #coord_flip() + # This switch X and Y axis and allows to get the horizontal version
    xlab("") +
    ylab("Renda")
  
#valor_hora por categoria - violin
aplicativo = aplicativo %>% 
    filter(valor_hora < 50)
  
  ggplot(aplicativo,aes(x=valor_hora, color=categoria, fill=categoria)) +
    geom_density(alpha=0.6) +
    scale_fill_viridis(discrete=TRUE) +
    scale_color_viridis(discrete=TRUE) +
    #geom_text(aes(label=categoria, color=categoria), hjust=0, size=4.5) +
    #theme_ipsum() +
    facet_wrap(~categoria) +
    theme(
      legend.position="top"
    ) +
    ylab("") +
    xlab("Valor da Hora")

  #valor_hora por categoria - ridgeline
  ggplot(aplicativo,aes(x=valor_hora, y=categoria, fill=categoria)) +
    geom_density_ridges() +
    theme_ridges() + 
    theme(legend.position = "none")
  
  #valor_hora por raca - ridgeline
  ggplot(aplicativo,aes(x=valor_hora, y=raca, fill=raca)) +
    geom_density_ridges() +
    theme_ridges() + 
    theme(legend.position = "none")
  

  #RENDA E DISTRIBUIÇÃO POR RAÇA
  ggplot(aplicativo, aes(x=raca, y=VD4019, fill=raca, color=raca)) +
    geom_violin(width=2.1, size=0.2) +
    geom_jitter(color="black", size=0.4, alpha=0.9) +
    scale_fill_viridis(discrete=TRUE) +
    scale_color_viridis(discrete=TRUE) +
    #theme_ipsum() +
    theme(
      legend.position="none"
    ) +
    #coord_flip() + # This switch X and Y axis and allows to get the horizontal version
    xlab("") +
    ylab("Renda")
  
  #VALOR HORA POR DISTRIBUIÇÃO RACIAL
  ggplot(aplicativo, aes(x=raca, y=valor_hora, fill=raca, color=raca)) +
    geom_violin(width=2.1, size=0.2) +
    geom_jitter(color="black", size=0.4, alpha=0.9) +
    scale_fill_viridis(discrete=TRUE) +
    scale_color_viridis(discrete=TRUE) +
    #theme_ipsum() +
    theme(
      legend.position="none"
    ) +
    #coord_flip() + # This switch X and Y axis and allows to get the horizontal version
    xlab("") +
    ylab("Renda")

  #DISTRIBUIÇÃO DE RENDA POR ESCOLARIDADE COMPACTA
  ggplot(aplicativo, aes(x=escolaridade_compacta, y=VD4019, fill=escolaridade_compacta, color=escolaridade_compacta)) +
    geom_violin(width=2.1, size=0.2) +
    geom_jitter(color="black", size=0.4, alpha=0.9) +
    scale_fill_viridis(discrete=TRUE) +
    scale_color_viridis(discrete=TRUE) +
    #theme_ipsum() +
    theme(
      legend.position="none"
    ) +
    #coord_flip() + # This switch X and Y axis and allows to get the horizontal version
    xlab("") +
    ylab("Rendimento")

#jornada por categoria  
  library(ggridges)
  ggplot(aplicativo, aes(y=categoria, x=VD4031,  fill=categoria)) +
    geom_density_ridges(alpha=0.6, stat="binline", bins=20) +
    theme_ridges() +
    theme(
      legend.position="none",
      panel.spacing = unit(0.1, "lines"),
      strip.text.x = element_text(size = 2)
    ) +
    xlab("Jornada") +
    ylab("Categoria")

  
#####CORRELAÇÃO  
  aplicativo_correlacao = aplicativo %>% 
    select(VD4019, VD4031, V2009, categoria, escolaridade_compacta)
  
library(GGally)
  ggpairs(aplicativo_correlacao, title="Correlograma", cardinality_threshold = NULL,
          ggplot2::aes(colour=categoria))   
  
  
#CONFIANÇA

  #S090051 - Familiares
  #S090052 - Amigos
  #S090053 - Colegas
  #S090054 - Vizinhos
  
  familiares = aplicativo %>%
    mutate(count=1) %>%
    group_by(S090051) %>% 
    summarise(peso = sum(V1028, na.rm = T), absoluto = sum(count, na.rm = T))
  