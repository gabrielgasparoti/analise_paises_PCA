######################################### 

# ANÁLISE POR COMPONENTES PRINCIPAIS EM R

#########################################
## ANO 2
##Dataset: dados de 50 países com variáveis relacionadas com corrupção, 
  #violência, renda e educação em dois anos consecutivos.
  #(Fonte: Fávero et al, 2017).

## Dicionário de Variáveis
  #Pais: o nome do País
  #CPI: Corruption Perception Index:
    #corresponde à percepção dos cidadãos em relação ao abuso do setor público 
    #sobre os beneficios privados de uma nação, 
    #cobrindo aspectos administrativos e políticos. 
  #Violencia: Quantidade de assassinatos a cada 100.000 habitantes 
    #(Fontes: Organização Mundial da Saúde, 
    #Escritório das Nações Unidas para Drogas 
    #Crime e GIMD Global Burden of Injuries.  
  #PIB: PIB per capita em US$ ajustado pela inflação, com ano base 2000 
    #(Fonte: Banco Mundial). 
  #Escolaridade: Quantidade média de anos de escolaridade por pessoas com mais de 25 anos,
    #incluindo ensinos primário, secundário e superior
    #(Fonte: Institute for Health Metrics and Evaluation).

##1. Importando as bibliotecas
library(tidyverse)  #manipulação de dados
library(psych)      #Teste de Bartlett
library(REdaS)      #KMOS, MSA
library(FactoMineR) #PCA
library(corrplot)   #plot da matriz de correlação
library(factoextra) #plot da % de variabilidade dos dados por fator

##2. Importando a base de dados

dados <- read_rds('Indicador_Paises_ano2.rds')
view(dados)


##3. Teste KMO E MSA
KMOS(dados[2:5])
  #Como KMO = 0.7 e MSA de cada variável > 0.5
  #Aplicaremos Bartlett para confirmar a possibilidade
  #da técnica de PCA

##4. Teste Estatístico de Bartlett
cortest.bartlett(dados[2:5])
  #Rejeitamos H0
  #matriz de correlação das variáveis não é a matriz identidade
  #Podemos aplicar PCA

##5. Aplicando PCA com FactoMineR para visualização dos fatores
# plotando as 2 dimensoes que melhor representam a variabilidade dos dados
dados_pca <- PCA(dados[,2:5],
                 scale.unit = T,
                 graph = T)

#pela análise gráfica dos fatores, podemos entender que as variáveis 
#Escolaridade, CPI e PIB tem maior correlação com o fator 1
#enquanto a variável Violência tem maior correlação com o fator 2


##6. Visualizar a % da variância dos dados acumulada em cada fator
fviz_screeplot(dados_pca)


## Vamos iniciar com 2 fatores principais e depois analisar os autovalores

######################### PARTE 1 - SEM ROTACAO ###################################

##7. Aplicando PCA com psych SEM rotação
dados_pca_sem_rotacao <- principal(dados[,2:5],
                       nfactors = 2,
                       scores = T, 
                       rotate = 'none')

##8. Analise dos autovalores
#Critério da raiz latente: descartar componentes com autovalor menor que 1

dados_pca_sem_rotacao$values
  #nesse ponto verifiquei que somente 1 componente poderia ser usado
  #pois somente o autovalor do primeiro fator foi maior que 1


##9. Aplicando PCA com psych SEM rotação com UM fator
dados_pca_sem_rotacao <- principal(dados[,2:5],
                                   nfactors = 1,
                                   scores = T, 
                                   rotate = 'none')

#10 Percentual da variância nesse fator
dados_pca_sem_rotacao$Vaccounted
#64% da variancia

##11. Cargas Fatoriais
#correlação da variável original com cada fator
dados_pca_sem_rotacao$loadings
  #nesse momento comprovamos que CPI, PIB e Escolaridade 
  #tem forte correlação positiva com o fator 1
  #enquanto Violência tem forte correlação negativa

##12. Visualizando o fator
dados_pca_sem_rotacao$scores


##13. Adicionando o fator na tabela original para criar um ranking
dados$ranking <- dados_pca_sem_rotacao$scores


##14. Visualizando o ranking criado
view(dados)

##15. Conclusao
  #ordenar a coluna ranking em ordem decrescente.
  #país com maior pontuação no ano 2: Noruega
  #Suíça fica em segundo lugar no ano 2.


#################### PARTE 2 - COM ROTACAO #############################

##15. Aplicando PCA com psych com rotação com 2 fatores
dados_pca_com_rotacao <- principal(dados[,2:5],
                                   nfactors = 2,
                                   scores = T)


dados_pca_com_rotacao$Vaccounted
  #com rotacao o primeiro autovalor satisfaz a condição da raiz latente
  # o segundo autovalor ficou muito próximo de 1
  # isso significa que somente uma variável explica bem o fator 2
  # dessa forma, podemos manter um único fator, conforme realizamos anteriormente
  # sem rotação.
library(openxlsx)
dados <- dados %>% 
  arrange(desc(ranking))
write.xlsx(dados, file='ranking_ano2.xlsx')
view(dados)
