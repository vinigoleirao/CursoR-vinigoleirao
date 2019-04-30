## Analise de Dados (2019.1) - Lista 3 - "R"####
# Vinicius Silva Santana

##Questao 1####

#link: https://github.com/vinigoleirao/CursoR-vinigoleirao

##Questao 2####
# Variavel x com valor 17; e variavel y com valor 6; e variavel z com valor do somat?rio dae x e y.
#CPF: 04937078593

#atribuindo valor 17 a x
x <- c(17)

#checando o valor de x
x

#atribuindo o valor 6 a y
y <- c(6)

#checando o valor de y
y

#soma de x e y
x+y

#resultado de x+y ? 23

#atribuindo o somat?rio de x e y como valor de z
z <- c(sum(x+y))

#checando o valor de z
z

#valor de z ? 23

#atribuindo o valor de CPF
CPF <- c(04937078593)

#Multiplicando z pelo CPF
z*CPF

#O resultado final da atividade 2 ? 113552807639.

##Questao 3####

#coletando a base de dados mtcars
head(mtcars)

#checando os tipos de variaveis da base mtcars
class(mtcars$mpg)
class(mtcars$cyl)
class(mtcars$disp)
class(mtcars$hp)
class(mtcars$drat)
class(mtcars$wt)
class(mtcars$qsec)
class(mtcars$vs)
class(mtcars$am)
class(mtcars$gear)
class(mtcars$carb)

#todas as vari?veis sao num?ricas

#checagem do n?mero de dimensoes da base

dim(mtcars)

#a dimensao da base ? de 32 linhas e 11 colunas

#imprimindo a terceira coluna da base

head(mtcars$disp)

# a terceira coluna (disp) fica assim disposta: [1] 160 160 108 258 360 225

#imprimindo a segunda linha da base
#PRECISO FAZER AINDA

#identificando o quarto elemento presente na vari?vel "cyl"
mtcars$cyl[4]

#apresentando o resumo descritivo da base de dados
summary(mtcars)

##Questao 4####
#solicitando o pacote ffbase
require(ffbase)

#checando diretÃ³rio de acesso do R
getwd()

#configurando diretÃ³rio de acesso do R
setwd("C:/Users/vinig/Documents/Curso_R_UFPE_1/dados_encontro_1_ufpe")

#criacao do data.frame "turmas"
turmas <- read.csv2("TURMAS.csv", sep = "|")

setwd('. .')
setwd("C:/Users/vinig/Documents/Curso_R_UFPE_1/dados_encontro_1_ufpe")

#extraindo os dados de Pernambuco
turmas_pe <- subset(turmas, CO_UF == 26)

#comparando ambos os data frames
dim(turmas)

dim(turmas_pe)

#checando os dados de Pernambuco
turmas_pe

#Checando se o arquivo gerado realmente Ã© um data.frame (na primeira vez, veio como function)
class(turmas)

class(turmas_pe)

#apÃ³s checagem, turmas_pe e um data.frame

#selecionando diretÃ³rio
setwd("C:/Users/vinig/Documents/Curso_R_UFPE_1/dados_encontro_1_ufpe")

#salvando arquivo data.frame em formato R.
save(turmas_pe, file ="turmas_pe.RData")

##Questao 5####

#definindo diretorio para carregamento do arquivo turmas_pe.RData
setwd("C:/Users/vinig/Documents/Curso_R_UFPE_1/dados_encontro_1_ufpe")

#carregando arquivo turmas_pe.RData
load("turmas_pe.RData")

#checando os nomes e as caracteristicas das colunas
names(turmas_pe)

head(turmas_pe)

#checando o resumo do numero de matriculas por turma
summary(turmas_pe$NU_MATRICULAS)

#confirmando a media do numero de matriculas por turma
mean(turmas_pe$NU_MATRICULAS)

#a media e de 23,07089 matriculas para cada turma.

##Questao 6####

#carregando o pacote ffbase
require(ffbase)

#definindo diretorio
setwd("C:/Users/vinig/Documents/Curso_R_UFPE_1/dados_encontro_1_ufpe")

#carregando dados em ffdf
docentes_ne <- read.csv2.ffdf(file = "DOCENTES_NORDESTE.csv", sep = "|", first.rows=1000000)

#filtrando dados para Pernambuco
docentes_pe <- subset(docentes_ne, CO_UF == 26)

#checando a dimensao das listas
dim(docentes_pe)
dim(docentes_ne)

#criando tabela para observar os dados sobre cor e raca
table.ff(docentes_pe$TP_COR_RACA)

#O INEP define 0 como cor/raca nao declarada; 1 como Branca; 2 como Preta; 3 como Parda; 4 como Amarela; e 5 como Indigena

#Resultado Cor e Raca    
#0      1      2      3      4      5 
#181573  98141  14710 114718   1419   2102

#checando a proporcao dos dados em TP_COR_RACA
prop.table(table(docentes_pe$TP_COR_RACA))

#convertendo a proporcao em dados percentuais
prop.table(table(docentes_pe$TP_COR_RACA))*100

#somando pretos e pardos
3.564+27.799

#a) Os professores que nao declararam cor ou raca contabilizam 44%.
#b) Os professores que se declararam pretos contabilizam 3,564% e pardos, 27,799%, juntos somam 31,363% do total.
