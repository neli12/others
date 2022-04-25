#### DETERMINAÇÃO DA COR DE AMOSTRAS DE SOLO COM BASE EM SEU ESPECTRO DE REFLECTÂNCIA

##   Considerações gerais: A função aqui desenvolvida corresponde a um conjunto de cálculos que determina a cor de solos
#                          de acordo com o sistema munsell, utilizando como base apenas o espectro de reflectância 
#                          dos solos em questão.      

## Importante: Três arquivos são necessários para, posteriormente, rodar o exemplo da página help, sendo eles: "cieob2nd.RData"; "IlD65.RData" e 
#              "data.RData". Por favor, certifique-se de te-los na sua área de trabalho.  

### SCRIPT DA FUNÇÃO spec2hvc

# 1. Para a correta execução da função, dois pacotes são necessários.
#     Estes correspondem ao pracma e ao CircStats. O pracma possui uma função chamada trapz, 
#     a qual calcula a integral das curvas espectrais. Tal dado é essencial para que os
#     valores triestimulu XYZ sejam calculados.
#     Já o pacote CirscStats possui a função deg, esta é necessária para converter o valor de 
#     de matiz (Hue) de radianos para graus.

install.packages("pracma")
install.packages("CircStats")

require (pracma)
require (CircStats)

## 2. A função será definida como spec2hvc (spectrum to hue,value,chroma). 
#     Para que seja possível roda-la corretamente, são necessários 3 dados de entrada (3 matrizes de dados): 
#     (a) data - Matriz correspondente aos valores de reflectância espectral de solos;
#     (b) cieob - Matrix de dados correspondente a função de ajuste do ângulo de visada do observador;
#     (c) IlD65 - Matrix de dados correspondente a função de ajuste do iluminante padrão, de acordo com normas da CIE

spec2hvc <- function(data, cieob, Il)
  
{
  
  ## 3. Conforme descrito na página do help, os dados espectrais devem apresentar valores de reflectância, ao menos, nos 
  #     comprimentos de onda de 380 a 780 nm, com resolução espectral de 1 nm ou 5 nm. Caso o usuário não ajuste seus dados
  #     corretamente, e portanto alguns comprimentos de onda corretos estejam ausentes, a função stop irá interromper o processamento
  #     e mostrar um aviso.
  
  if(length(row.names(data[row.names(cieob),]))!=length(row.names(cieob))) 
    stop("Dados não apresentam comprimentos de onda necessários! Requisito: 380 a 780 nm, resolução de 1 nm ou 5 nm")
  
  ## 4. Uma vez checada a integridade dos dados, o próximo passo corresponde a converter as curvas de reflectância espectral,
  #     para o sistema de cores XYZ. Para tanto, o valores de reflectância devem ser multiplicados pelo iluminante padrão e pela 
  #     função de ajuste para observador CIE. O resultado da multiplicação é uma curva, na qual é aplicado o cálculo integral com base 
  #     no método dos trapézios (função trapz - pacote pracma). Para maiores detalhes checar referências na página de help.
  #     Vale ressaltar que, o intuito é realizar o cálculo para diversas amostras de solo, simultâneamente. Sendo assim, para que o cálculo fosse
  #     realizado repetidamente para todas as amostras, implementou-se a função apply().
  
  K <- 100/trapz(as.numeric(row.names(cieob)),Il[row.names(cieob),1]*cieob[,2])
  
  X <- K*apply(data[row.names(cieob),]*Il[row.names(cieob),1]*cieob[,1], 2, trapz, x=as.numeric(row.names(cieob)))
  Y <- K*apply(data[row.names(cieob),]*Il[row.names(cieob),1]*cieob[,2], 2, trapz, x=as.numeric(row.names(cieob)))
  Z <- K*apply(data[row.names(cieob),]*Il[row.names(cieob),1]*cieob[,3], 2, trapz, x=as.numeric(row.names(cieob)))
  
  ## 5. Em seguida, as variáveis XYZ precisam ser convertidas para outro sistema de cores, o L*a*b*. Para tanto, os valores X,Y e Z
  #     devem ser divididos por seus respectivos valores triestimulus de iluminante (95.045,100,108.892). Conforme o valor resultante 
  #     da razão, a fórmula de conversão do sistema XYZ para o Lab varia. Para solucionar tal empasse, utilizou-se a função ifelse(), 
  #     sendo que, se a razão for menor que 0.008856 aplica-se uma fórmula, caso contrário aplica-se outra.
  
  
  var_X2 <- ifelse ( X/95.045>0.008856, (X/95.045)^(1/3), (7.787*(X/95.045))+(16/116))
  var_Y2 <- ifelse ( Y/100.000>0.008856, (Y/100.000)^(1/3), (7.787*(Y/100))+(16/116))
  var_Z2 <- ifelse ( Z/108.892>0.008856, (Z/108.892)^(1/3), (7.787*(Z/108.892))+(16/116))
  
  L <- (116*var_Y2)-16
  a <- 500*(var_X2-var_Y2)
  b <- 200*(var_Y2-var_Z2)
  
  ## 6. Uma vez obtidos os valores L*a*b*, pode-se finalmente calcular a cor Munsell das amostras de solo. Para tanto, os valores das coordenadas
  #     b* são divididos pelos valores de a*, e o arcotangente desta razão é calculado. O resultado é o ângulo da matiz, este expresso em radianos. 
  #     Foi necessária a conversão do ângulo para graus (função deg - pacote CircStat) e depois uma normalização (adaptado de Centore et al., 2012)(referência na página help).
  
  Hap <- round(((((deg(atan(b/a)))-26)*20)/66)-5,digits=2)
  
  ## 7. O sistema Munsell possui notações para as mais diversas cores (e.g. azul, verde, amarelo, vermelho), sendo que a maioria delas não ocorre em solos. 
  #     Sendo assim, a função foi ajustada para analisar somente solos com as cores vermelho, vermelho-amarelo e amarelo (estas que abrangem a grande maioria 
  #     dos casos.). Caso a amostra de solo analisada apresente cor diferente de vermelho, verm.-amarelo ou amarelo, a função retorna o valor "NA".  
  #     
  Ha <- ifelse(deg(atan(b/a))>92 | deg(atan(b/a))<26,NA, ifelse (Hap > 10, Hap-10, ifelse(Hap<0, 10-(Hap*-1), Hap)))      
  
  ## 8. O sistema munsell de cores apresenta uma letra, a qual juntamente com um ângulo designa a matiz (e.g., 2.5 Y). Esta letra pode ser definida de acordo
  #     com o ângulo da matiz. Para cumprir esta tarefa utilizou a função ifelse(). 
  
  Hr <- ifelse( Ha == "NA", "NA", ifelse (Hap > 10, "Y" , ifelse(Hap > 0, "YR", "R")))
  
  ## 9. Para a obtenção do valor (v) e do chroma (c), foram aplicadas as fórmulas abaixo. O valor é calculado a partir da Lightness (L) do sistema L*a*b*
  #      Já o chroma é obtido a partir das coordenados a* e b*. Ambos os valores foram arrendondados para 1 casa decimal, visando a praticidade.   
  
  v <- round(L/10, digits=1)
  
  c <- round(sqrt((a^2) + (b^2))/5.5, digits=1)
  
  ## 10. Antes do término da função foi definido que os resultados deveriam ser indicados ao final do cálculo.
  
  return(data.frame(Ha,Hr,v,c))
  
}

#   A FUNÇÃO TERMINA AQUI 

#   Defina seu diretório de trabalho

setwd("G:/Google Drive - Ariane/Cor/Artigo cor/Geocis_TCC/Função R")
load("Il.Rdata")
load("cieob2nd.Rdata")
data1<-read.csv("data1.csv",sep=";",head=T)
row.names(data1)<-data1[,"Id"]
data2<-data1[,-1]
spec2hvc(data2,cieob, Il)
