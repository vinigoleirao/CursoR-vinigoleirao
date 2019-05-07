##Analise de Dados - Lista 4 ####
#Vinicius Silva Santana

##Questao 1####

#Adicionar link

##Questao 2####

#instalando pacotes e carregando pacotes já instalados

require(dplyr)
library(ffbase)
install.packages("readxl")
install.packages("magrittr")
install.packages("tidyverse")
install.packages("GGally")
install.packages("readr")
install.packages("rlang")
library(readxl)
library(magrittr)
library(tidyverse)
library(GGally)
library(readr)
library(rlang)

#selecionando diretorio para carregamento de dados
setwd("C:/Users/vinig/Documents/Curso R/dados_encontro_2_ufpe")

#carregando os dados necessarios
load("matricula_pe_censo_escolar_2016.RData")
load("docentes_pe_censo_escolar_2016.RData")
load("turmas_pe_censo_escolar_2016.RData")
load("escolas_pe_censo_escolar_2016.RData")

#carregando dados do pnud no formato xlsx (quando a funcao read_xlsx e usada, aparece a mensagem: cannot allocate vector of size 1.5 Gb)
pnud_data <- read_excel("atlas2013_dadosbrutos_pt.xlsx", sheet = 2)

#checando informacoes da base
summary(pnud_data)
head(pnud_data)
summary(pnud_data$ANO)
head(pnud_data$ANO)
unique(pnud_data$ANO)

#filtrando dados do ano para 2010 apenas, criando uma nova base de dados
pnud_pe_2010 <- pnud_data %>% filter (ANO == 2010 & UF == 26)

#criando nova base de dados para as turmas

turmas_pe_mun <- turmas_pe %>% group_by(CO_MUNICIPIO) %>%
  summarise(n_turmas = n(), 
            turmas_disc_prof = sum(IN_DISC_PROFISSIONALIZANTE, na.rm = T),
            turmas_disc_inf = sum(IN_DISC_INFORMATICA_COMPUTACAO, na.rm = T),
            turmas_disc_mat = sum(IN_DISC_MATEMATICA, na.rm = T),
            turmas_disc_pt = sum(IN_DISC_LINGUA_PORTUGUESA, na.rm = T),
            turmas_disc_en = sum(IN_DISC_LINGUA_INGLES, na.rm = T))

#verificando nova base de dados

dim(turmas_pe_mun)[1] == length(unique(turmas_pe$CO_MUNICIPIO))

summary(turmas_pe_mun)              
head(turmas_pe_mun)

#Recriando base de dados referente as escolas

escolas_pe_mun <- escolas_pe %>% group_by(CO_MUNICIPIO) %>%
  summarise(n_escolas = n(), 
            n_escolas_priv = sum(TP_DEPENDENCIA == 4, na.rm = T),
            escolas_func = sum(TP_SITUACAO_FUNCIONAMENTO == 1, na.rm = T),
            escolas_agua_inex = sum(IN_AGUA_INEXISTENTE, na.rm = T),
            escolas_energia_inex = sum(IN_ENERGIA_INEXISTENTE, na.rm = T),
            escolas_esgoto_inex = sum(IN_ESGOTO_INEXISTENTE, na.rm = T),
            escolas_internet = sum(IN_INTERNET, na.rm = T),
            escolas_alimentacao = sum(IN_ALIMENTACAO, na.rm = T))


#verificando a nova base escolas_pe_mun
dim(escolas_pe_mun)[1] == length(unique(escolas_pe$CO_MUNICIPIO))
summary(escolas_pe_mun)
head(escolas_pe_mun)

#Recriando base de docentes

docentes_pe_mun <- docentes_pe %>% group_by(CO_MUNICIPIO) %>%
  summarise(n_docentes = n(),
            docentes_media_idade = mean(NU_IDADE),
            docentes_fem_sx = sum(TP_SEXO == 2, na.rm = T),
            docentes_superior = sum(TP_ESCOLARIDADE == 4, na.rm = T),
            docentes_contrato = sum(TP_TIPO_CONTRATACAO %in% c(1, 4), na.rm = T))

# verifacando informacoes da nova base
summary(docentes_pe_mun)
head(docentes_pe_mun)

#Criando nova base de dados para matriculas
matriculas_pe_mun <- matricula_pe %>% group_by(CO_MUNICIPIO) %>%
  summarise(n_matriculas = n(), 
            alunos_media_idade = mean(NU_IDADE),
            alunos_fem_sx = sum(TP_SEXO == 2, na.rm = T),
            alunos_negros = sum(TP_COR_RACA %in% c(2, 3), na.rm = T),
            alunos_indigenas = sum(TP_COR_RACA == 5, na.rm = T),
            alunos_cor_nd = sum(TP_COR_RACA == 0, na.rm = T),
            matriculas_educ_inf = sum(TP_ETAPA_ENSINO %in% c(1, 2), na.rm = T),
            matriculas_educ_fund = sum(TP_ETAPA_ENSINO %in% c(4:21, 41), na.rm = T),
            matriculas_educ_medio = sum(TP_ETAPA_ENSINO %in% c(25:38), na.rm = T))

#Verificando informacoes da nova base de dados criada
dim(matriculas_pe_mun)[1] == length(unique(matricula_pe$CO_MUNICIPIO))
summary(matriculas_pe_mun)
head(matriculas_pe_mun)


#Unindo as bases de dados do censo e do pnud: 

#primeira uniao, por matriculas

censo_pnud_pe_mun <- pnud_pe_2010 %>% full_join(matriculas_pe_mun, 
                                                by = c("Codmun7" = "CO_MUNICIPIO")
)

# checando informacoes da nova base
dim(pnud_pe_2010)

dim(matriculas_pe_mun)

dim(censo_pnud_pe_mun)

names(censo_pnud_pe_mun)

#segunda uniao, por escolas


censo_pnud_pe_mun <- censo_pnud_pe_mun %>% full_join(escolas_pe_mun, 
                                                     by = c("Codmun7" = "CO_MUNICIPIO")
)

#checando a dimensao e informacoes da nova base
dim(escolas_pe_mun)

dim(censo_pnud_pe_mun)

names(censo_pnud_pe_mun)

#terceira uniao, por turmas

censo_pnud_pe_mun <- censo_pnud_pe_mun %>% full_join(turmas_pe_mun, 
                                                     by = c("Codmun7" = "CO_MUNICIPIO")
)

#checando a dimensao e informacoes da nova base
dim(turmas_pe_mun)

dim(censo_pnud_pe_mun)

names(censo_pnud_pe_mun)

#quarta uniao, por docentes

censo_pnud_pe_mun <- censo_pnud_pe_mun %>% full_join(docentes_pe_mun, 
                                                     by = c("Codmun7" = "CO_MUNICIPIO")
)

#checando informacoes da nova base
dim(docentes_pe_mun)

dim(censo_pnud_pe_mun)

names(censo_pnud_pe_mun)

#checando diretorio para salvar dados
getwd()

#Salvando nova base de dados em formato .RData
save(censo_pnud_pe_mun, file = "censo_pnud_pe_mun_2010_2016.RData")

#filtrando docentes por idade (entre 18 e 70) e checando informacoes
docentes_pe_filt <- docentes_pe%>% filter(NU_IDADE > 18, NU_IDADE < 70)
head(docentes_pe_filt)
summary(docentes_pe_filt)
dim(docentes_pe_filt)
summary(docentes_pe_filt$NU_IDADE)

#filtrando alunos por idade (entre 1 e 25) e checando informacoes
matricula_pe_filt <- matricula_pe%>% filter(NU_IDADE > 1, NU_IDADE < 25)

dim(matricula_pe_filt)
head(matricula_pe_filt)
summary(matricula_pe_filt$NU_IDADE)

#Criando valores para relacionar a base de docentes e alunos e checando as estatísticas de tal
Relacao_docentes_alunos <- censo_pnud_pe_mun$n_matriculas/censo_pnud_pe_mun$n_docentes

summary(Relacao_docentes_alunos)

censo_pnud_pe_mun_docentesalunos <- censo_pnud_pe_mun %>%  mutate(Relacao_docentes_alunos)

View(censo_pnud_pe_mun_docentesalunos)

censo_pnud_pe_mun_docentesalunos["177", ]

#A cidade de Tupanatinga possui um IDHM de 0.519

# teste de correlacao entre as variaveis
cor(censo_pnud_pe_mun_docentesalunos$Relacao_docentes_alunos, censo_pnud_pe_mun_docentesalunos$IDHM)

#resultado do teste -0.4796604

cor.test(censo_pnud_pe_mun_docentesalunos$Relacao_docentes_alunos, censo_pnud_pe_mun_docentesalunos$IDHM)

#resultado do teste de correlacao de Pearson
#Pearson's product-moment correlation

#data:  censo_pnud_pe_mun_docentesalunos$Relacao_docentes_alunos and censo_pnud_pe_mun_docentesalunos$IDHM
#t = -7.3949, df = 183, p-value = 4.917e-12
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
#  -0.5835475 -0.3603263
#sample estimates:
#  cor 
# -0.4796604 


##Questao 3####

#Grafico de dispersao usando o ggplot

ggplot(censo_pnud_pe_mun_docentesalunos, aes(Relacao_docentes_alunos, IDHM, color = IDHM))+geom_point()
