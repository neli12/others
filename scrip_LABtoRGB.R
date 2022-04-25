list.files()
library(raster)
SYSI <- stack("SYSI_20cloud.tif" )
SYSI[SYSI == 0] <- NA
plot(SYSI)

SYSI.df <- as.data.frame(SYSI)
SYSI.df <- SYSI.df[complete.cases(SYSI.df),]

var_R <- SYSI.df[, 3]
var_G <- SYSI.df[, 2]
var_B <- SYSI.df[, 1]

var_R1 <- var_R/10000
var_G1 <- var_G/10000
var_B1 <- var_B/10000

#var_R1<-ifelse (var_R>0.04045, (((var_R+0.055)/1.055)^2.4), var_R/12.92)
#var_G1<-ifelse (var_G>0.04045, (((var_G+0.055)/1.055)^2.4), var_G/12.92)
#var_B1<-ifelse (var_B>0.04045, (((var_B+0.055)/1.055)^2.4), var_B/12.92)

var_R2 <- var_R1*100
var_G2 <- var_G1*100
var_B2 <- var_B1*100

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
Lab <- cbind(as.data.frame(L), as.data.frame(a), as.data.frame(b))

#Munsell A
Hap.A <- round(((((deg(atan(b/a)))-26)*20)/66)-5,digits=2)


Ha.A <- ifelse(deg(atan(b/a))>92 | deg(atan(b/a))<26,NA, ifelse (Hap.A > 10, Hap.A-10, ifelse(Hap.A<0, 10-(Hap.A*-1), Hap.A)))


Hr.A <- ifelse( Ha.A == "NA", "NA", ifelse (Hap.A > 10, "Y" , ifelse(Hap.A > 0, "YR", "R")))


v.A <- round(L/10, digits=1)

c.A <- round(sqrt((a^2) + (b^2))/5.5, digits=1)

hvc <-  cbind(as.data.frame(Ha.A), as.data.frame(Hr.A), as.data.frame(v.A), as.data.frame(c.A))
