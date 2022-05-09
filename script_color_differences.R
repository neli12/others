setwd("C:/Users/FREY/Google Drive/GEOCIS/IC/Merylin")
list.files()

dados <- read.csv("dados_definitivo_SEN_LAND_refl.csv", h=TRUE, sep=";")
dados <- dados[complete.cases(dados),]
str(dados)
summary(dados)

spec.pira <- dados[,43:2193]
summary(spec.pira)
colnames(spec.pira ) <- seq(from = 350, to = 2500, by = 1)

### organizar tabela para simular os dados para landsat "Speclib"
#install.packages("hsdar")
require(hsdar) 
#chamar pacote hsdar
str(spec.pira)

#matrix para gerar o speclib
spec.pira.matrix <- as.matrix(spec.pira)

str(spec.pira)

#comprimentos de onde fieldspec
wave.fielspec <- seq(from = 350, to = 2500, by = 1)

#speclib dos dados
require(speclib)
spec.pira.speclib <- speclib(spec.pira.matrix, wave.fielspec)

str(spec.pira.speclib)

#ID dos dados e colocar na speclib
##Id na 1 coluna, para usar desativa linha 25 do código 
#bellinaso.speclib <- bellinaso.speclib (as.character)
#IDs.bellinaso.pira.A <- bellinaso.pira.A.spec[, 1]
#idSpeclib(bellinaso.speclib) <- as.character(IDs.bellinaso.pira.A)


#plotar a speclib

plot(spec.pira.speclib, FUN = "mean", main = "Mean spectrum")

### Resampling of bands to various satellite sensors
get.sensor.characteristics(0)

#Sentinel2

spec.pira.sentinel2 <- spectralResampling(spec.pira.speclib, "Sentinel2",
                                            response_function = TRUE)
summary(spec.pira.sentinel2)
plot(spec.pira.sentinel2)

#### Sentinel2 L*a*b*
rownames(dados) <- dados[,1]
spec.sentinel <- dados[,21:29]

rgb <- spec.sentinel[, c(3, 2, 1)]
summary(rgb)

var_R <- rgb[, 1]
var_G <- rgb[, 2]
var_B <- rgb[, 3]


#var_R1<-ifelse (var_R>0.04045, (((var_R+0.055)/1.055)^2.4), var_R/12.92)
#var_G1<-ifelse (var_G>0.04045, (((var_G+0.055)/1.055)^2.4), var_G/12.92)
#var_B1<-ifelse (var_B>0.04045, (((var_B+0.055)/1.055)^2.4), var_B/12.92)

var_R2 <- var_R*100
var_G2 <- var_G*100
var_B2 <- var_B*100

X <- var_R2*0.412453+var_G2*0.357580+var_B2*0.180423
Y <- var_R2*0.212671+var_G2*0.715160+var_B2*0.072169
Z <- var_R2*0.019334+var_G2*0.119194+var_B2*0.950227



### X Y Z para Lab
var_X2 <- ifelse ( X/95.045>0.008856, (X/95.045)^(1/3), (7.787*(X/95.045))+(16/116))
var_Y2 <- ifelse ( Y/100.000>0.008856, (Y/100.000)^(1/3), (7.787*(Y/100))+(16/116))
var_Z2 <- ifelse ( Z/108.892>0.008856, (Z/108.892)^(1/3), (7.787*(Z/108.892))+(16/116))

L <- (116*var_Y2)-16
a <- 500*(var_X2-var_Y2)
b <- 200*(var_Y2-var_Z2)


Sentinel2.L.a.b <- cbind(as.data.frame(L), as.data.frame(a), as.data.frame(b))
row.names(Sentinel2.L.a.b) <- dados[,1]
Sentinel2.L.a.b

Hap <- round(((((deg(atan(b/a)))-26)*20)/66)-5,digits=2)
Ha <- ifelse(deg(atan(b/a))>92 | deg(atan(b/a))<26,NA, ifelse (Hap > 10, Hap-10, ifelse(Hap<0, 10-(Hap*-1), Hap)))
Hr <- ifelse( Ha == "NA", "NA", ifelse (Hap > 10, "Y" , ifelse(Hap > 0, "YR", "R")))
v <- round(L/10, digits=1)
c <- round(sqrt((a^2) + (b^2))/5.5, digits=1)

hvc.SEN <- cbind(Ha, Hr, v, c)
row.names(hvc) <- dados[,1]

color.SEN <- cbind(Sentinel2.L.a.b, hvc.SEN)



#### Landsat2 L*a*b*
spec.LAND <- dados[,36:41]

rgb <- spec.LAND[, c(3, 2, 1)]


var_R <- rgb[, 1]
var_G <- rgb[, 2]
var_B <- rgb[, 3]


#var_R1<-ifelse (var_R>0.04045, (((var_R+0.055)/1.055)^2.4), var_R/12.92)
#var_G1<-ifelse (var_G>0.04045, (((var_G+0.055)/1.055)^2.4), var_G/12.92)
#var_B1<-ifelse (var_B>0.04045, (((var_B+0.055)/1.055)^2.4), var_B/12.92)

var_R2 <- var_R*100
var_G2 <- var_G*100
var_B2 <- var_B*100

X <- var_R2*0.412453+var_G2*0.357580+var_B2*0.180423
Y <- var_R2*0.212671+var_G2*0.715160+var_B2*0.072169
Z <- var_R2*0.019334+var_G2*0.119194+var_B2*0.950227



### X Y Z para Lab
var_X2 <- ifelse ( X/95.045>0.008856, (X/95.045)^(1/3), (7.787*(X/95.045))+(16/116))
var_Y2 <- ifelse ( Y/100.000>0.008856, (Y/100.000)^(1/3), (7.787*(Y/100))+(16/116))
var_Z2 <- ifelse ( Z/108.892>0.008856, (Z/108.892)^(1/3), (7.787*(Z/108.892))+(16/116))

L <- (116*var_Y2)-16
a <- 500*(var_X2-var_Y2)
b <- 200*(var_Y2-var_Z2)


LAND.L.a.b <- cbind(as.data.frame(L), as.data.frame(a), as.data.frame(b))
row.names(LAND.L.a.b) <- dados[,1]
LAND.L.a.b

Hap <- round(((((deg(atan(b/a)))-26)*20)/66)-5,digits=2)
Ha <- ifelse(deg(atan(b/a))>92 | deg(atan(b/a))<26,NA, ifelse (Hap > 10, Hap-10, ifelse(Hap<0, 10-(Hap*-1), Hap)))
Hr <- ifelse( Ha == "NA", "NA", ifelse (Hap > 10, "Y" , ifelse(Hap > 0, "YR", "R")))
v <- round(L/10, digits=1)
c <- round(sqrt((a^2) + (b^2))/5.5, digits=1)

hvc <- cbind(Ha, Hr, v, c)
row.names(hvc) <- dados[,1]

color <- cbind(LAND.L.a.b, hvc)

###distancia de cores


#delta CIE1976


D.76.PIRA.sen.land <- deltaE1976(LAND.L.a.b[, ], Sentinel2.L.a.b[, ])

hist(D.76.PIRA.sen.land)



