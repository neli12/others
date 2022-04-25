setwd("C:/Users/KRATOS/Desktop/Dados_Luizao11/Dados_Luizao/Rasters")

library(raster)
library(parallel)
library(spup)
library(purrr)
library(spdep)
library(ncf)

shp<-shapefile("C:/Users/KRATOS/Desktop/proc_rodnei/ESCRITA_LUIZAO/DADOS_LUIZAO_NOVOS1/HA_B1.shp")
sr<-crs(shp)
###### VARIÁVEIS DE ENTRADA DA FUNÇÃO
 sl<-raster("LG_Slope.tif")
 names(sl)<-"sl"
#SLOPE
 cec<-raster("LG_Map_CTC_Layer_B.tif")
 names(cec)<-"cec"
#CTC
 arg_a<-raster("LG_Map_Argila_Layer_A.tif")
 names(arg_a)<-"arg_a"
#TEXTURA CAM A
 arg_b<-raster("LG_Map_Argila_Layer_B.tif")
 names(arg_b)<-"arg_b"
#TEXTURA CAM B
 lf<- (raster("LG_NormalizeHeight.tif")<0.35 & sl<3)
 names(lf)<-"lf"
#ALAGAMENTO
 om<-raster("LG_Map_MO.tif")
 names(om)<-"om"
#MATERIA ORGANICA
 Matiz<-raster("LG_CorSolo_Matiz_Layer_B.tif")
 names(Matiz)<-"Matiz"
# matiz COR SOLO
 Valor<-raster("LG_Valor_B.tif")
 names(Valor)<-"Valor"
#valor
 Croma<-raster("LG_Map_Croma.tif")
 names(Croma)<-"Croma"
#croma
 V_A<-raster("LG_V_Layer_A.tif")
 names(V_A)<-"V_A"
 V_B<-raster("LG_V_Layer_B.tif")
 names(V_B)<-"V_B"
#saturacao_bases
 AL_B<-raster("LG_Al_Layer_B.tif")
 names(AL_B)<-"AL_B"
#teor aluminio
 M_B<-raster("LG_m_perc_Layer_B.tif")
 names(M_B)<-"M_B"
#aluminio sat.

cc<-stack(cec,arg_a,arg_b,om,V_A,V_B,M_B,AL_B) 
cc1<-as.data.frame(cc)
correl<-cor(cc1,use="complete.obs") 

######################################### FUNCAO CLASSIFICACAO SOLO ###############################################

class_solo <- function (diret,sl,cec,arg_a,arg_b,lf,om,V_A,V_B,M_B,AL_B){
  
  # Ha CHERNO (0 -1)
  chermic <- (om*0.58)>6 & V_A >= 65    
  names(chermic)<-"chermic"
  # ALITICO (0 -1)
  alic <- ( AL_B>40 & ((cec/10)*(1000/arg_b))>20 & (V_B<=50|M_B>50) )
  names(alic)<-"alic"
  #GRADIENTE TEXTURAL
  grad<-arg_b/arg_a
  names(grad)<-"grad"
  #HOR. ARGIC.
  arghor<-(grad > 1.8 & arg_a<= 150)|(grad > 1.7 & arg_a>150 & arg_a<= 400)|(grad > 1.5 & arg_a>400) #Tirei o critério de arg_a>400, O luizao não utilizou ?!?!?!? Mapas não batiam.
  names(arghor)<-"arghor"
  # TA (0 -1)
  ta<-(((cec/10)*(1000/arg_b))>65) 
  names(ta)<-"ta"
  # EUTROFICO (0 -1)
  dystric<-(V_B>50)
  names(dystric)<-"dystric"
  
  ####### CONVERTENDO RASTERS EM DATA FRAME PARA CLASSIFICACAO DOS SOLOS
  
  vars<-stack(sl,cec,arg_a,arg_b,lf,om,Matiz,Valor,Croma,chermic,alic,grad,arghor,ta,dystric)
  vars1<-as.data.frame(vars,xy=T)
  colnames(vars1)<-c("x","y","sl","cec","arg_a","arg_b","lf","om","Matiz","Valor","Croma","chermic","alic","grad","arghor","ta","dystric")
  t<-!is.na(apply(vars1[,c(-1,-2)],1,sum))
  vars2<-vars1[t,]
  
  ##LEGENDA CLASSES DE SOLO DA CHAVE BOOLEANA
  #1.  Leptosol
  #2.  Leptosol + Cambisol
  #3.  Regosol
  #4.  Gleysol + Fluvisol
  #5.  Histosol
  #6.  Chernozem
  #7.  Luvisol
  #8.  Abruptic Acrisol
  #9. Haplic Acrisol
  #10. Arenosol
  #11. Nitisol
  #12. Arenic Ferrasol
  #13. Ferralsol
  
  ######### CHAVE BOOLEANA  ##########  
  NV1 <- ifelse(vars2$sl>40 & vars2$arghor==0,1,
                ifelse(40>vars2$sl & vars2$sl>9 & vars2$arghor==0,2,
                       ifelse(vars2$cec>100 & vars2$arg_b<150,3,
                              ifelse(vars2$lf==1,4,
                                     ifelse(vars2$om>130,5,
                                            ifelse(vars2$arghor==1,
                                                   ifelse(vars2$ta==1,
                                                          ifelse(vars2$dystric==0,
                                                                 ifelse(vars2$chermic==1,6,7),
                                                                 ifelse(vars2$grad>2,8,9)
                                                          ),ifelse(vars2$grad>2,8,9)),
                                                   ifelse(vars2$arg_a<150 & vars2$arg_b<150,10,
                                                          ifelse(vars2$arg_a>350 & vars2$arg_b>350 & (vars2$ta==0|vars2$alic==1) & vars2$sl>4.5 & vars2$sl<9,11,
                                                                 ifelse(vars2$arg_a<150 & vars2$arg_b>150,12,13)
                                                          ))))))))
  
  # NV2 <- ifelse(NV1 == "Abruptic Acrisol"| NV1 == "Haplic Acrisol" & vars2$Matiz >= 15 & vars2$Valor >= 3 & vars2$Valor<4 & vars2$Croma<= 4, "Bruno Acinzentado",
  #              ifelse(NV1 == "Abruptic Acrisol"| NV1 == "Haplic Acrisol" & vars2$Matiz >= 17.5 & vars2$Valor >= 5 & vars2$Croma<= 4,"Acinzentado",
  #                     ifelse(NV1 == "Abruptic Acrisol"| NV1 == "Haplic Acrisol" & vars2$Matiz >= 20 & vars2$Valor >4 & vars2$Croma >4,"Amarelo",
  #                            ifelse(NV1 == "Abruptic Acrisol"| NV1 == "Haplic Acrisol" & vars2$Matiz<= 12.5,"Vermelho",
  #                                   ifelse(NV1 == "Abruptic Acrisol"| NV1 == "Haplic Acrisol" & vars2$Matiz<17.5 & vars2$Matiz>15 & vars2$Valor <3 & vars2$Valor>4 & vars2$Croma>4,"Vermelho-Amarelo",
  #                                          ifelse (NV1 == "Nitisol" & vars2$Matiz >= 17.5 & vars2$Valor<= 4 & vars2$Croma<= 6,"Bruno",
  #                                                  ifelse (NV1 == "Nitisol" & vars2$Matiz<= 12.5,"Vermelho",
  #                                                          ifelse (NV1 == "Nitisol" & vars2$Matiz<17.5 & vars2$Matiz>12.5,"Háplico",
  #                                                                  ifelse (NV1 == "Arenic Ferrasol"| NV1 == "Ferralsol" & vars2$Matiz >= 17.5 & vars2$Valor <= 4 & vars2$Croma <= 6,"Bruno",
  #                                                                          ifelse  (NV1 == "Arenic Ferrasol"| NV1 == "Ferralsol" &  vars2$Matiz >= 17.5 & vars2$Valor >4 & vars2$Croma >6,"Amarelo",
  #                                                                                   ifelse  (NV1 == "Arenic Ferrasol"| NV1 == "Ferralsol" &  vars2$Matiz <= 12.5,"Vermelho",
  #                                                                                            ifelse  (NV1 == "Arenic Ferrasol"| NV1 == "Ferralsol","Vermelho-Amarelo","")))))))))))) 
  ms<-cbind(vars2[,c(1,2)],NV1)#,NV2)
  r<-rasterFromXYZ(ms)
  writeRaster(r,diret,overwrite=T)
}

#LEGENDA CLASSES DE SOLO DA CHAVE BOOLEANA
#1.  Leptosol
#2.  Leptosol + Cambisol
#3.  Regosol
#4.  Gleysol + Fluvisol
#5.  Histosol
#6.  Chernozem
#7.  Luvisol
#8.  Abruptic Acrisol
#9. Haplic Acrisol
#10. Arenosol
#11. Nitisol
#12. Arenic Ferrasol
#13. Ferralsol

#####################################   FUNÇÃO TERMINA AQUI #############################################