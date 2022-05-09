setwd("C:/Users/FREY/Google Drive/GEOCIS/IC/Merylin")
list.files()
spec.pira <- read.csv("espectros_por_textura.csv", h=TRUE, sep = ";")
rownames(spec.pira) <- spec.pira[,2] 
spec.pira <- spec.pira[,3:2153]
colnames(spec.pira) <- seq(from = 350, to = 2500, by = 1)

sensor1 <- read.csv("sensor.csv", h=TRUE, sep = ";")
sensor2 <- read.csv("sensor_landsat.csv", h=TRUE, sep = ";")


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

plot(spec.pira.speclib[8], col = "red", main = "Texture Plot")
plot(spec.pira.speclib[7], col = "blue", new = FALSE)
plot(spec.pira.speclib[6], col = "black", new = FALSE)
plot(spec.pira.speclib[5], col = "green", new = FALSE)
plot(spec.pira.speclib[4], col = "yellow", new = FALSE)
plot(spec.pira.speclib[3], col = "black", new = FALSE)
plot(spec.pira.speclib[2], col = "green", new = FALSE)
plot(spec.pira.speclib[1], col = "yellow", new = FALSE)

### Resampling of bands to various satellite sensors

### Resampling of bands to various satellite sensors

sentinel <- get.sensor.characteristics("Sentinel2", TRUE)
landsat <- get.sensor.characteristics("Landsat8", TRUE)
plot(sentinel$response)


par(mfrow=c(1,3))
#Sentinel2

spec.pira.sentinel2.1 <- spectralResampling(spec.pira.speclib[1], sensor1,
                                          response_function = TRUE)
spec.pira.sentinel2.2 <- spectralResampling(spec.pira.speclib[2], sensor1,
                                          response_function = TRUE)
spec.pira.sentinel2.3 <- spectralResampling(spec.pira.speclib[3], sensor1,
                                          response_function = TRUE)
spec.pira.sentinel2.4 <- spectralResampling(spec.pira.speclib[4], sensor1,
                                          response_function = TRUE)
spec.pira.sentinel2.5 <- spectralResampling(spec.pira.speclib[5], sensor1,
                                          response_function = TRUE)
spec.pira.sentinel2.6 <- spectralResampling(spec.pira.speclib[6], sensor1,
                                          response_function = TRUE)
spec.pira.sentinel2.7 <- spectralResampling(spec.pira.speclib[7], sensor1,
                                          response_function = TRUE)
spec.pira.sentinel2.8 <- spectralResampling(spec.pira.speclib[8], sensor1,
                                          response_function = TRUE)

spec.sentinel1 <- as.data.frame(spec.pira.sentinel2.1)
spec.sentinel2 <- as.data.frame(spec.pira.sentinel2.2)
spec.sentinel3 <- as.data.frame(spec.pira.sentinel2.3)
spec.sentinel4 <- as.data.frame(spec.pira.sentinel2.4)
spec.sentinel5 <- as.data.frame(spec.pira.sentinel2.5)
spec.sentinel6 <- as.data.frame(spec.pira.sentinel2.6)
spec.sentinel7 <- as.data.frame(spec.pira.sentinel2.7)
spec.sentinel8 <- as.data.frame(spec.pira.sentinel2.8)

spec.sentinel2 <- rbind(spec.sentinel1, spec.sentinel2, spec.sentinel3, spec.sentinel4, spec.sentinel5, spec.sentinel6, spec.sentinel7, spec.sentinel8)
write.csv(spec.sentinel2, "spectra_convolution_SENTINEL.csv")

#LANDSAT 8
spec.pira.landsat8.1 <- spectralResampling(spec.pira.speclib[1], sensor2,
                                            response_function = TRUE)
spec.pira.landsat8.2 <- spectralResampling(spec.pira.speclib[2], sensor2,
                                            response_function = TRUE)
spec.pira.landsat8.3 <- spectralResampling(spec.pira.speclib[3], sensor2,
                                            response_function = TRUE)
spec.pira.landsat8.4 <- spectralResampling(spec.pira.speclib[4], sensor2,
                                            response_function = TRUE)
spec.pira.landsat8.5 <- spectralResampling(spec.pira.speclib[5], sensor2,
                                            response_function = TRUE)
spec.pira.landsat8.6 <- spectralResampling(spec.pira.speclib[6], sensor2,
                                            response_function = TRUE)
spec.pira.landsat8.7 <- spectralResampling(spec.pira.speclib[7], sensor2,
                                            response_function = TRUE)
spec.pira.landsat8.8 <- spectralResampling(spec.pira.speclib[8], sensor2,
                                            response_function = TRUE)

spec.landsat1 <- as.data.frame(spec.pira.landsat8.1)
spec.landsat2 <- as.data.frame(spec.pira.landsat8.2)
spec.landsat3 <- as.data.frame(spec.pira.landsat8.3)
spec.landsat4 <- as.data.frame(spec.pira.landsat8.4)
spec.landsat5 <- as.data.frame(spec.pira.landsat8.5)
spec.landsat6 <- as.data.frame(spec.pira.landsat8.6)
spec.landsat7 <- as.data.frame(spec.pira.landsat8.7)
spec.landsat8 <- as.data.frame(spec.pira.landsat8.8)

spec.landsat8 <- rbind(spec.landsat1, spec.landsat2, spec.landsat3, spec.landsat4, spec.landsat5, spec.landsat6, spec.landsat7, spec.landsat8)
write.csv(spec.landsat8, "spectra_convolution_landsat.csv")

#### Sentinel2 L*a*b*

spectra(spec.pira.sentinel2)


idSpeclib(spec.pira.sentinel2)


spectra(spec.pira.sentinel2[, c(1, 2, 3)])


rgb <- spectra(spec.pira.sentinel2[, c(3, 2, 1)])


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

row.names(Sentinel2.L.a.b) <- idSpeclib(spec.pira.speclib)

Sentinel2.L.a.b

Hap <- round(((((deg(atan(b/a)))-26)*20)/66)-5,digits=2)
Ha <- ifelse(deg(atan(b/a))>92 | deg(atan(b/a))<26,NA, ifelse (Hap > 10, Hap-10, ifelse(Hap<0, 10-(Hap*-1), Hap)))
Hr <- ifelse( Ha == "NA", "NA", ifelse (Hap > 10, "Y" , ifelse(Hap > 0, "YR", "R")))
v <- round(L/10, digits=1)
c <- round(sqrt((a^2) + (b^2))/5.5, digits=1)

hvc <- cbind(Ha, Hr, v, c)




library(reshape2)
library(plotly)

all.spectra <- read.csv("spectra_sat_lab_all.csv", h=TRUE, sep = ";")
all.spectra$Bands <- as.factor(all.spectra$Bands)
str(all.spectra)
spectra1 <- all.spectra[1:24,]
spectra2 <- all.spectra[25:48,]
spectra3 <- all.spectra[49:72,]
spectra4 <- all.spectra[73:96,]
spectra5 <- all.spectra[97:120,]
spectra6 <- all.spectra[121:144,]
spectra7 <- all.spectra[145:168,]
spectra8 <- all.spectra[169:192,]

# Base Plot
gg <- ggplot(midwest, aes(x=area, y=poptotal)) + 
  geom_point(aes(col=state, size=popdensity)) + 
  geom_smooth(method="loess", se=F) + xlim(c(0, 0.1)) + ylim(c(0, 500000)) + 
  labs(title="Area Vs Population", y="Population", x="Area", caption="Source: midwest")

# Define and add annotation -------------------------------------
library(grid)
my_text1 <- "a."
my_grob1 = grid.text(my_text1, x=0.05,  y=0.9, gp=gpar(col="black", fontsize=10, fontface="bold"))
p1 <- ggplot(data=spectra1, aes(x=Bands, y=Reflectance, group = Texture, colour = Class)) +  geom_line() + theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  scale_y_continuous(limits = c(0, 0.4)) + theme(
    plot.title = element_text(color = "black", size = 9, face = "bold")) + theme(axis.title.x = element_blank()) + annotation_custom(my_grob1) + 
  theme(axis.ticks.x=element_blank(), axis.text.x=element_blank())

my_text2 <- "b."
my_grob2 = grid.text(my_text2, x=0.05,  y=0.9, gp=gpar(col="black", fontsize=10, fontface="bold"))
p2 <- ggplot(data=spectra2, aes(x=Bands, y=Reflectance, group = Texture, colour = Class)) +  geom_line() + theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  scale_y_continuous(limits = c(0, 0.4))  + theme(axis.title.x = element_blank()) + theme(axis.title.y = element_blank()) +
  annotation_custom(my_grob2) + theme(axis.ticks.x=element_blank(), axis.text.x=element_blank())

my_text3 <- "c."
my_grob3 = grid.text(my_text3, x=0.05,  y=0.9, gp=gpar(col="black", fontsize=10, fontface="bold"))
p3 <- ggplot(data=spectra3, aes(x=Bands, y=Reflectance, group = Texture, colour = Class)) +  geom_line() + theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  scale_y_continuous(limits = c(0, 0.5)) + theme(axis.title.x = element_blank()) + annotation_custom(my_grob3) + theme(axis.ticks.x=element_blank(), axis.text.x=element_blank())

my_text4 <- "d."
my_grob4 = grid.text(my_text4, x=0.05,  y=0.9, gp=gpar(col="black", fontsize=10, fontface="bold"))
p4 <- ggplot(data=spectra4, aes(x=Bands, y=Reflectance, group = Texture, colour = Class)) +  geom_line() + theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  scale_y_continuous(limits = c(0, 0.5)) + theme(axis.title.x = element_blank()) + theme(axis.title.y = element_blank()) + annotation_custom(my_grob4) + 
  theme(axis.ticks.x=element_blank(), axis.text.x=element_blank())

my_text5 <- "e."
my_grob5 = grid.text(my_text5, x=0.05,  y=0.9, gp=gpar(col="black", fontsize=10, fontface="bold"))
p5 <- ggplot(data=spectra5, aes(x=Bands, y=Reflectance, group = Texture, colour = Class)) +  geom_line() + theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  scale_y_continuous(limits = c(0, 0.6)) + theme(axis.title.x = element_blank()) + theme(axis.ticks.x=element_blank(), axis.text.x=element_blank()) + annotation_custom(my_grob5)

my_text6 <- "f."
my_grob6 = grid.text(my_text6, x=0.05,  y=0.9, gp=gpar(col="black", fontsize=10, fontface="bold"))
p6 <- ggplot(data=spectra6, aes(x=Bands, y=Reflectance, group = Texture, colour = Class)) +  geom_line() + theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  scale_y_continuous(limits = c(0, 0.6)) + theme(axis.title.x = element_blank()) + theme(axis.title.y = element_blank()) + annotation_custom(my_grob6) + 
  theme(axis.ticks.x=element_blank(), axis.text.x=element_blank())

my_text7 <- "g."
my_grob7 = grid.text(my_text7, x=0.05,  y=0.9, gp=gpar(col="black", fontsize=10, fontface="bold"))
p7 <- ggplot(data=spectra7, aes(x=Bands, y=Reflectance, group = Texture, colour = Class)) +  geom_line() + theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  scale_y_continuous(limits = c(0, 0.6)) + annotation_custom(my_grob7)

my_text8 <- "h."
my_grob8 = grid.text(my_text8, x=0.05,  y=0.9, gp=gpar(col="black", fontsize=10, fontface="bold"))
p8 <- ggplot(data=spectra8, aes(x=Bands, y=Reflectance, group = Texture, colour = Class)) +  geom_line() + theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"))  + 
  scale_y_continuous(limits = c(0, 0.6))  + theme(axis.title.y = element_blank()) + annotation_custom(my_grob8)


tiff("plot15.tiff", width = 2800, height = 2500, res = 500)
ggarrange(p1, p2, p3, p4, p5, p6, p7, p8, ncol=2, nrow=4, common.legend = TRUE, legend="right", font.label = list(size = 6, color = "black"), label.y = 0)
dev.off()

