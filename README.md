# ==== Instrucoes ====
library(dplyr)
library(tidyr)
library(tidyverse)
library(data.table)
library(PNADcIBGE)
library(openxlsx)
#' -- Salve todos os arquivos na pasta que contém o seu nome.
#' -- Consulte materiais postados, digitais ou impressos, mas não consulte outra pessoa.
#' -- Faça as tarefas na ordem, utilizando a tabela da tarefa anterior como ponto de partida
#'    para a próxima tarefa.


# ==== Leitura de dados ====

#' Importe o arquivo PNAD_COVID_052020.csv que está na pasta "./PNAD_COVID/Dados" para uma tabela do R.
library(readr)
PNAD_COVID_052020 <- read_csv("C:/Users/athay/OneDrive/Área de Trabalho/Raul/PNAD_COVID/Dados/PNAD_COVID_052020.csv")
PNAD_COVID_052020 = PNAD_COVID_052020 %>% mutate_all (as.numeric)

View(PNAD_COVID_052020)
#' Essa tabela deve conter 349306 observações e 114 variáveis.


# ==== Manipulação de dados ====

#' Filtre somente as pessoas com 16 anos ou mais de idade (variável "A002") que residem
#' no Distrito Federal (registros em que a variável "UF" é igual a 53).
amostra = PNAD_COVID_052020 %>% filter (A002 >16, UF == "53")

#' Crie a variável "cod_cond_ativ" para identificar se um indivíduo é ocupado (cod_cond_ativ = 1),
#' desocupado (cod_cond_ativ = 2) ou fora da força de trabalho (cod_cond_ativ = 3), sendo que:
amostra = cod_cond_ativ %>% mutate(cod_cond_ativ = ifelse(!(C007==9 & is.na(C006)), 1, 0),
                           cod_cond_ativ = ifelse(C015 == 2, 2, cod_cond_ativ),
                           cod_cond_ativ = ifelse(is.na(cod_cond_ativ) & A002 > 16 , 3, cod_cond_ativ))

#' -- Ocupados podem ser identificados como aqueles em que a variável "C006" foi respondida (não é missing)
#'    e a variável "C007" é diferente de 9;
#' -- Desocupados podem ser identificados como aqueles em que a variável "C015" é igual a 1; e
#' -- Fora da força de trabalho são todos aqueles indivíduos maiores de 16 anos de idade
#'    que não são nem ocupados nem desocupados.



#' Mantenha somente as variáveis "V1032", "A003" e "cod_cond_ativ" na tabela e descarte as demais.
amostra = amostra %>% select(V1032, A003, cod_cond_ativ)
# ==== Análise de dados ====

#' Calcule a frequência de pessoas ocupadas, desocupadas e fora da força de trabalho da amostra.

t = amostra %>% group_by(cod_cond_ativ) %>% 
            summarize (Pessoas = sum(V1032))

#' Calcule a frequência de pessoas ocupadas, desocupadas e fora da força de trabalho da população,
#' desagregada por sexo (não se esqueça de usar o peso ou fator de expansão amostral, variável "V1032").
#' 
POPULA = PNAD_COVID_052020 %>% mutate(cod_cond_ativ = ifelse(!(C007==9 & is.na(C006)), 1, 0),
                                   cod_cond_ativ = ifelse(C015 == 2, 2, cod_cond_ativ),
                                   cod_cond_ativ = ifelse(is.na(cod_cond_ativ) & A002 > 16 , 3, cod_cond_ativ))

freq = POPULA %>% group_by(cod_cond_ativ, A003) %>%
                  summarise(Pessoas = sum(V1032))
#' Imprima a tabela no console e copie e cole o resultado no final desse arquivo.

cod_cond_ativ  A003   Pessoas
<dbl> <dbl>     <dbl>
1             1     1   157095.
2             1     2   121448.
3             2     1 28772415.
4             2     2 46587585.
5             3     1 52756845.
6             3     2 40704181.
7            NA     1 21404118.
8            NA     2 20365713.
