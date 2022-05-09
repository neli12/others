list.files()
spec.pira <- read.csv("dados_definitivo_SEN_LAND.csv", h=TRUE, sep = ";")
rownames(spec.pira) <- spec.pira[,1] 
spec.pira <- spec.pira[,26:456]

spec.pira <- cbind(spec.pira[,4], spec.pira[,43:2193])
spec.pira1 <- spec.pira[complete.cases(spec.pira),]
spec.pira1 <- spec.pira1[,-1]
colnames(spec.pira1) <- seq(from = 350, to = 2500, by = 5)
texture <-  spec.pira1[,1]
### organizar tabela para simular os dados para landsat "Speclib"
#install.packages("hsdar")
require(hsdar) 
#chamar pacote hsdar
str(spec.pira1)

#matrix para gerar o speclib
spec.pira.matrix <- as.matrix(spec.pira1)

str(spec.pira)

#comprimentos de onde fieldspec
wave.fielspec <- seq(from = 350, to = 2500, by = 5)

#speclib dos dados
spec.pira.speclib <- speclib(spec.pira.matrix, wave.fielspec)

str(spec.pira.speclib)

#ID dos dados e colocar na speclib
##Id na 1 coluna, para usar desativa linha 25 do código 
#bellinaso.speclib <- bellinaso.speclib (as.character)
#IDs.bellinaso.pira.A <- bellinaso.pira.A.spec[, 1]
#idSpeclib(bellinaso.speclib) <- as.character(IDs.bellinaso.pira.A)

#Sentinel2
require(hsdar)
get.sensor.characteristics("Sentinel2", response_function = TRUE)

spec.pira.sentinel2.1 <- spectralResampling(spec.pira.speclib, "Sentinel2",
                                            response_function = TRUE)
plot(spec.pira.sentinel2.1[1])
summary(spec.pira.sentinel2.1)

SENTINEL <- as.data.frame(spec.pira.sentinel2.1)
SENTINEL <- cbind(texture, SENTINEL)

write.csv(SENTINEL, "spectra_convolued_SENTINEL.csv")
 

#LANDSAT 5
spec.pira.landsat <- spectralResampling(spec.pira.speclib, "Landsat5",
                                        response_function = TRUE)

LANDSAT <- as.data.frame(spec.pira.landsat)
LANDSAT <- cbind(texture, LANDSAT)

write.csv(LANDSAT, "spectra_convolued_LANDSAT.csv")

LANDSAT.mean <- aggregate(LANDSAT, list(LANDSAT$texture), mean)
write.csv(LANDSAT.mean, "spectra_convolued_LANDSAT5_by_texture.csv")

#LANDSAT 8
spec.pira.landsat8 <- spectralResampling(spec.pira.speclib, "Landsat8",
                                         response_function = TRUE)

LANDSAT8 <- as.data.frame(spec.pira.landsat8)
LANDSAT8 <- cbind(texture, LANDSAT8)

write.csv(LANDSAT8, "spectra_convolued_LANDSAT.csv")

LANDSAT8.mean <- aggregate(LANDSAT8, list(LANDSAT8$texture), mean)
write.csv(LANDSAT8.mean, "spectra_convolued_LANDSAT8_by_texture.csv")



