#### DETERMINA��O DA COR DE AMOSTRAS DE SOLO COM BASE EM SEU ESPECTRO DE REFLECT�NCIA

##   Considera��es gerais: A fun��o aqui desenvolvida corresponde a um conjunto de c�lculos que determina a cor de solos
#                          de acordo com o sistema munsell, utilizando como base apenas o espectro de reflect�ncia 
#                          dos solos em quest�o.      

## Importante: Tr�s arquivos s�o necess�rios para, posteriormente, rodar o exemplo da p�gina help, sendo eles: "cieob2nd.RData"; "IlD65.RData" e 
#              "data.RData". Por favor, certifique-se de te-los na sua �rea de trabalho.  

### SCRIPT DA FUN��O spec2hvc

# 1. Para a correta execu��o da fun��o, dois pacotes s�o necess�rios.
#     Estes correspondem ao pracma e ao CircStats. O pracma possui uma fun��o chamada trapz, 
#     a qual calcula a integral das curvas espectrais. Tal dado � essencial para que os
#     valores triestimulu XYZ sejam calculados.
#     J� o pacote CirscStats possui a fun��o deg, esta � necess�ria para converter o valor de 
#     de matiz (Hue) de radianos para graus.

install.packages("pracma")
install.packages("CircStats")

require (pracma)
require (CircStats)

## 2. A fun��o ser� definida como spec2hvc (spectrum to hue,value,chroma). 
#     Para que seja poss�vel roda-la corretamente, s�o necess�rios 3 dados de entrada (3 matrizes de dados): 
#     (a) data - Matriz correspondente aos valores de reflect�ncia espectral de solos;
#     (b) cieob - Matrix de dados correspondente a fun��o de ajuste do �ngulo de visada do observador;
#     (c) IlD65 - Matrix de dados correspondente a fun��o de ajuste do iluminante padr�o, de acordo com normas da CIE

spec2hvc <- function(data, cieob, Il)
  
{
  
  ## 3. Conforme descrito na p�gina do help, os dados espectrais devem apresentar valores de reflect�ncia, ao menos, nos 
  #     comprimentos de onda de 380 a 780 nm, com resolu��o espectral de 1 nm ou 5 nm. Caso o usu�rio n�o ajuste seus dados
  #     corretamente, e portanto alguns comprimentos de onda corretos estejam ausentes, a fun��o stop ir� interromper o processamento
  #     e mostrar um aviso.
  
  if(length(row.names(data[row.names(cieob),]))!=length(row.names(cieob))) 
    stop("Dados n�o apresentam comprimentos de onda necess�rios! Requisito: 380 a 780 nm, resolu��o de 1 nm ou 5 nm")
  
  ## 4. Uma vez checada a integridade dos dados, o pr�ximo passo corresponde a converter as curvas de reflect�ncia espectral,
  #     para o sistema de cores XYZ. Para tanto, o valores de reflect�ncia devem ser multiplicados pelo iluminante padr�o e pela 
  #     fun��o de ajuste para observador CIE. O resultado da multiplica��o � uma curva, na qual � aplicado o c�lculo integral com base 
  #     no m�todo dos trap�zios (fun��o trapz - pacote pracma). Para maiores detalhes checar refer�ncias na p�gina de help.
  #     Vale ressaltar que, o intuito � realizar o c�lculo para diversas amostras de solo, simult�neamente. Sendo assim, para que o c�lculo fosse
  #     realizado repetidamente para todas as amostras, implementou-se a fun��o apply().
  
  K <- 100/trapz(as.numeric(row.names(cieob)),Il[row.names(cieob),1]*cieob[,2])
  
  X <- K*apply(data[row.names(cieob),]*Il[row.names(cieob),1]*cieob[,1], 2, trapz, x=as.numeric(row.names(cieob)))
  Y <- K*apply(data[row.names(cieob),]*Il[row.names(cieob),1]*cieob[,2], 2, trapz, x=as.numeric(row.names(cieob)))
  Z <- K*apply(data[row.names(cieob),]*Il[row.names(cieob),1]*cieob[,3], 2, trapz, x=as.numeric(row.names(cieob)))
  
  ## 5. Em seguida, as vari�veis XYZ precisam ser convertidas para outro sistema de cores, o L*a*b*. Para tanto, os valores X,Y e Z
  #     devem ser divididos por seus respectivos valores triestimulus de iluminante (95.045,100,108.892). Conforme o valor resultante 
  #     da raz�o, a f�rmula de convers�o do sistema XYZ para o Lab varia. Para solucionar tal empasse, utilizou-se a fun��o ifelse(), 
  #     sendo que, se a raz�o for menor que 0.008856 aplica-se uma f�rmula, caso contr�rio aplica-se outra.
  
  
  var_X2 <- ifelse ( X/95.045>0.008856, (X/95.045)^(1/3), (7.787*(X/95.045))+(16/116))
  var_Y2 <- ifelse ( Y/100.000>0.008856, (Y/100.000)^(1/3), (7.787*(Y/100))+(16/116))
  var_Z2 <- ifelse ( Z/108.892>0.008856, (Z/108.892)^(1/3), (7.787*(Z/108.892))+(16/116))
  
  L <- (116*var_Y2)-16
  a <- 500*(var_X2-var_Y2)
  b <- 200*(var_Y2-var_Z2)
  
  ## 6. Uma vez obtidos os valores L*a*b*, pode-se finalmente calcular a cor Munsell das amostras de solo. Para tanto, os valores das coordenadas
  #     b* s�o divididos pelos valores de a*, e o arcotangente desta raz�o � calculado. O resultado � o �ngulo da matiz, este expresso em radianos. 
  #     Foi necess�ria a convers�o do �ngulo para graus (fun��o deg - pacote CircStat) e depois uma normaliza��o (adaptado de Centore et al., 2012)(refer�ncia na p�gina help).
  
  Hap <- round(((((deg(atan(b/a)))-26)*20)/66)-5,digits=2)
  
  ## 7. O sistema Munsell possui nota��es para as mais diversas cores (e.g. azul, verde, amarelo, vermelho), sendo que a maioria delas n�o ocorre em solos. 
  #     Sendo assim, a fun��o foi ajustada para analisar somente solos com as cores vermelho, vermelho-amarelo e amarelo (estas que abrangem a grande maioria 
  #     dos casos.). Caso a amostra de solo analisada apresente cor diferente de vermelho, verm.-amarelo ou amarelo, a fun��o retorna o valor "NA".  
  #     
  Ha <- ifelse(deg(atan(b/a))>92 | deg(atan(b/a))<26,NA, ifelse (Hap > 10, Hap-10, ifelse(Hap<0, 10-(Hap*-1), Hap)))      
  
  ## 8. O sistema munsell de cores apresenta uma letra, a qual juntamente com um �ngulo designa a matiz (e.g., 2.5 Y). Esta letra pode ser definida de acordo
  #     com o �ngulo da matiz. Para cumprir esta tarefa utilizou a fun��o ifelse(). 
  
  Hr <- ifelse( Ha == "NA", "NA", ifelse (Hap > 10, "Y" , ifelse(Hap > 0, "YR", "R")))
  
  ## 9. Para a obten��o do valor (v) e do chroma (c), foram aplicadas as f�rmulas abaixo. O valor � calculado a partir da Lightness (L) do sistema L*a*b*
  #      J� o chroma � obtido a partir das coordenados a* e b*. Ambos os valores foram arrendondados para 1 casa decimal, visando a praticidade.   
  
  v <- round(L/10, digits=1)
  
  c <- round(sqrt((a^2) + (b^2))/5.5, digits=1)
  
  ## 10. Antes do t�rmino da fun��o foi definido que os resultados deveriam ser indicados ao final do c�lculo.
  
  return(data.frame(Ha,Hr,v,c))
  
}

#   A FUN��O TERMINA AQUI 

#   Defina seu diret�rio de trabalho

setwd("G:/Google Drive - Ariane/Cor/Artigo cor/Geocis_TCC/Fun��o R")
load("Il.Rdata")
load("cieob2nd.Rdata")
data1<-read.csv("data1.csv",sep=";",head=T)
row.names(data1)<-data1[,"Id"]
data2<-data1[,-1]
spec2hvc(data2,cieob, Il)
