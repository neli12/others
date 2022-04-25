######Load packages######
require(FRK)
require(caret)
require(sp)
require(raster)

######Set working directory######
setwd()
list.files()

######Load datasets####
###SYSIs data###
dados1 <- read.csv("dados_RSE2.csv", sep=";", h=TRUE)   ###clay, silt, sand and OM
dados1.cor <- read.csv("cor_PIRA_A_withSYSIFused_102230.csv", h=TRUE, sep=";")   ##Hue, value and chroma

##Dat partirtion dados1##
train.ind <- createDataPartition(1:nrow(dados1), p = .75, list = FALSE)
data.train <- dados1[ train.ind,]
data.test <- dados1[-train.ind,]
write.csv(train.cor, "train_dadosTESEdefinitivo.csv")
write.csv(test.cor, "test_dadosTESEdefinitivo.csv")

##Dat partirtion dados1.cor##
train.ind <- createDataPartition(1:nrow(dados1.cor), p = .75, list = FALSE)
data.train.cor <- dados1.cor[ train.ind,]
data.test.cor <- dados1.cor[-train.ind,]
write.csv(data.train.cor, "train_dados_cor.csv")
write.csv(data.train.cor, "test_dados_cor.csv")


##Single-date images data##
dados2 <- dados1[,-10:-30]
dados2 <- dados2[complete.cases(dados2), ]

dados2.cor <- dados1.cor[,-9:-29]
dados2.cor <- dados2.cor[complete.cases(dados2.cor), ]


train.ind1 <- createDataPartition(1:nrow(dados2), p = .75, list = FALSE)
data.train1 <- dados2[ train.ind1,] 
data.test1 <- dados2[-train.ind1,]
data.train1 <- read.csv("train_dados_singles.csv")
data.test1 <- read.csv("test_dados_singles.csv")

##Dat partirtion dados1.cor##
train.ind2 <- createDataPartition(1:nrow(dados2.cor), p = .75, list = FALSE)
data.train.cor2 <- dados2.cor[ train.ind2,]
data.test.cor2 <- dados2.cor[-train.ind2,]
data.train.cor2 <- read.csv("train_dados_cor_singles.csv")
data.test.cor2 <- read.csv("test_dados_cor_singles.csv")


####sATLLITE IMAGES###
L8.OLI <- stack("L8_masked_recorado.tif")
plot(L8.OLI)
names(L8.OLI) <- c("L8OLI.1", "L8OLI.2", "L8OLI.3", "L8OLI.4", "L8OLI.5", "L8OLI.6")

S2.MSI <- stack("S2_masked_rec.tif")
plot(S2.MSI)
names(S2.MSI) <-  c("S2MSI.1", "S2MSI.2", "S2MSI.3", "S2MSI.4", "S2MSI.5", "S2MSI.6", "S2MSI.7", "S2MSI.8", "S2MSI.9")


####################Soil attributes prediction######################
######Clay#######

clayS2.MSI <- train(x=cbind(data.train1[,17:19],data.train1[,23:25]), y=data.train1$Clay.gkg, method="cubist")
clayS2.MSI.pred <- predict(S2.MSI[[-4:-6]], clayS2.MSI)
clayS2.MSI.pred.data <- predict(clayS2.MSI, newdata = cbind(dados2[,16:18], dados2[,22:24]))
plot(clayS2.MSI.pred)
writeRaster(clayS2.MSI.pred, "clay_S2MSI_single.tif")
clayS2.MSI.pred.df <- as.data.frame(clayS2.MSI.pred)
clayS2.MSI.pred.df <- clayS2.MSI.pred.df[complete.cases(clayS2.MSI.pred.df), ]
mean(clayS2.MSI.pred.df)
sd(clayS2.MSI.pred.df)


clayL8.OLI <- train(x=data.train1[,11:16], y=data.train1$Clay.gkg, method="cubist")
clayL8.OLI.pred <- predict(L8.OLI, clayL8.OLI)
plot(clayL8.OLI.pred)
writeRaster(clayL8.OLI.pred, "clay_L8OLI_single.tif")
clayL8.OLI.pred.data <- predict(clayL8.OLI, newdata = dados2[,10:15])
clayL8.OLI.df <- as.data.frame(clayL8.OLI.pred)
clayL8.OLI.df <- clayL8.OLI.df[complete.cases(clayL8.OLI.df), ]
mean(clayL8.OLI.df)
sd(clayL8.OLI.df)

######Sand#######
SandS2.MSI <- train(x=cbind(data.train1[,17:19],data.train1[,23:25]), y=data.train1$Sand.gkg, method="cubist")
SandS2.MSI.pred <- predict(S2.MSI[[-4:-6]], SandS2.MSI)
plot(SandS2.MSI.pred)
writeRaster(SandS2.MSI.pred, "Sand_S2MSI_single.tif")
sandS2.MSI.pred.data <- predict(SandS2.MSI, newdata = cbind(dados2[,16:18], dados2[,22:24]))
plot(sandS2.MSI.pred)
SandS2.MSI.pred.df <- as.data.frame(SandS2.MSI.pred)
SandS2.MSI.pred.df <- SandS2.MSI.pred.df[complete.cases(SandS2.MSI.pred.df), ]
mean(SandS2.MSI.pred.df)
sd(SandS2.MSI.pred.df)


SandL8.OLI <- train(x=data.train1[,11:16], y=data.train1$Sand.gkg, method="cubist")
SandL8.OLI.pred <- predict(L8.OLI, SandL8.OLI)
plot(SandL8.OLI.pred)
writeRaster(SandL8.OLI.pred, "Sand_L8OLI_single.tif")
sandL8.OLI.pred.data <- predict(SandL8.OLI, newdata = dados2[,10:15])
SandL8.OLI.pred.df <- as.data.frame(SandL8.OLI.pred)
SandL8.OLI.pred.df <- SandL8.OLI.pred.df[complete.cases(SandL8.OLI.pred.df), ]
mean(SandL8.OLI.pred.df)
sd(SandL8.OLI.pred.df)

######Silt#######
SiltS2.MSI <- train(x=cbind(data.train1[,17:19],data.train1[,23:25]), y=data.train1$Silt.gkg, method="cubist")
SiltS2.MSI.pred <- predict(S2.MSI[[-4:-6]], SiltS2.MSI)
plot(SiltS2.MSI.pred)
writeRaster(SiltS2.MSI.pred, "Silt_S2MSI_single.tif")
SiltS2.MSI.pred.data <- predict(SiltS2.MSI, newdata = cbind(dados2[,16:18], dados2[,22:24]))
SiltS2.MSI.pred.df <- as.data.frame(SiltS2.MSI.pred)
SiltS2.MSI.pred.df <- SiltS2.MSI.pred.df[complete.cases(SiltS2.MSI.pred.df), ]
mean(SiltS2.MSI.pred.df)
sd(SiltS2.MSI.pred.df)



SiltL8.OLI <- train(x=data.train1[,11:16], y=data.train1$Silt.gkg, method="cubist")
SiltL8.OLI.pred <- predict(L8.OLI, SiltL8.OLI)
plot(SiltL8.OLI.pred)
writeRaster(SiltL8.OLI.pred, "Silt_L8OLI_single.tif")
siltL8.OLI.pred.data <- predict(SiltL8.OLI, newdata = dados2[,10:15])
SandL8.OLI.pred.df <- as.data.frame(SandL8.OLI.pred)
SandL8.OLI.pred.df <- SandL8.OLI.pred.df[complete.cases(SandL8.OLI.pred.df), ]
mean(SandL8.OLI.pred.df)
sd(SandL8.OLI.pred.df)

######SOM#######
OMS2.MSI <- train(x=cbind(data.train1[,17:19],data.train1[,23:25]), y=data.train1$OM.gkg, method="cubist")
OMS2.MSI.pred <- predict(S2.MSI[[-4:-6]], OMS2.MSI)
plot(OMS2.MSI.pred)
writeRaster(OMS2.MSI.pred, "OM_S2MSI_single.tif")
OMS2.MSI.pred.data <- predict(OMS2.MSI, newdata = cbind(dados2[,16:18], dados2[,22:24]))
mean(OMS2.MSI.pred.data)
sd(OMS2.MSI.pred.data)



OML8.OLI <- train(x=data.train1[,11:16], y=data.train1$OM.gkg, method="cubist")
OML8.OLI.pred <- predict(L8.OLI, OML8.OLI)
plot(OML8.OLI.pred)
writeRaster(OML8.OLI.pred, "OM_L8OLI_single.tif")
OML8.OLI.pred.data <- predict(OML8.OLI, newdata = dados2[,10:15])
OML8.OLI.pred.df <- as.data.frame(OML8.OLI.pred)
OML8.OLI.pred.df <- OML8.OLI.pred.df[complete.cases(OML8.OLI.pred.df), ]
mean(OML8.OLI.pred.df)
sd(OML8.OLI.pred.df)


L8.OLI.S2MSI.data <- as.data.frame(cbind(dados2$Long, dados2$Lat, dados2$X, dados2$Y, dados2$Lab_ID, clayS2.MSI.pred.data, sandS2.MSI.pred.data, siltS2.MSI.pred.data,
                        OMS2.MSI.pred.data, clayL8.OLI.pred.data, sandL8.OLI.pred.data, siltL8.OLI.pred.data, 
                        OML8.OLI.pred.data))
write.csv(L8.OLI.S2MSI.data, "dados_preditos_p_semivariogram.csv")

####################Soil color prediction######################
######Hue#######
hueS2.MSI <- train(x=cbind(data.train.cor2[,16:18],data.train.cor2[,22:24]), y=data.train.cor2$Hue, method="cubist")
summary(hueS2.MSI)
hueS2.MSI.pred <- predict(S2.MSI[[-4:-6]], hueS2.MSI)
plot(hueS2.MSI.pred)
writeRaster(hueS2.MSI.pred, "hue_S2MSI_single2.tif", overwrite=TRUE)
hueS2.MSI.pred.data <- predict(hueS2.MSI, newdata = cbind(dados2.cor[,15:17], dados2.cor[,21:23]))
hueS2.MSI.pred.df <- as.data.frame(hueS2.MSI.pred)
hueS2.MSI.pred.df <- hueS2.MSI.pred.df[complete.cases(hueS2.MSI.pred.df), ]
mean(hueS2.MSI.pred.df)
sd(hueS2.MSI.pred.df)


hueL8.OLI <- train(x=data.train.cor2[,10:15], y=data.train.cor2$Hue, method="cubist")
hueL8.OLI.pred <- predict(L8.OLI, hueL8.OLI)
plot(hueL8.OLI.pred)
writeRaster(hueL8.OLI.pred, "hue_L8OLI_single.tif", overwrite=TRUE)
hueL8.OLI.pred.data <- predict(hueL8.OLI, newdata = dados2.cor[,9:14])
hueL8.OLI.pred.df <- as.data.frame(hueL8.OLI.pred)
hueL8.OLI.pred.df <- hueL8.OLI.pred.df[complete.cases(hueL8.OLI.pred.df), ]
mean(hueL8.OLI.pred.df)
sd(hueL8.OLI.pred.df)

######value#######
valueS2.MSI <- train(x=cbind(data.train.cor2[,16:18],data.train.cor2[,22:24]), y=data.train.cor2$value, method="cubist")
valueS2.MSI.pred <- predict(S2.MSI[[-4:-6]], valueS2.MSI)
plot(valueS2.MSI.pred)
writeRaster(valueS2.MSI.pred, "value_S2MSI_single.tif", overwrite=TRUE)
valueS2.MSI.pred.data <- predict(valueS2.MSI, newdata = cbind(dados2.cor[,15:17], dados2.cor[,21:23]))
valueS2.MSI.pred.df <- as.data.frame(valueS2.MSI.pred)
valueS2.MSI.pred.df <- valueS2.MSI.pred.df[complete.cases(valueS2.MSI.pred.df), ]
mean(valueS2.MSI.pred.df)
sd(valueS2.MSI.pred.df)


valueL8.OLI <- train(x=data.train.cor2[,10:15], y=data.train.cor2$value, method="cubist")
valueL8.OLI.pred <- predict(L8.OLI, valueL8.OLI)
plot(valueL8.OLI.pred)
writeRaster(valueL8.OLI.pred, "value_L8OLI_single.tif", overwrite=TRUE)
valueL8.OLI.pred.data <- predict(valueL8.OLI, newdata =  dados2.cor[,9:14])
valueL8.OLI.pred.df <- as.data.frame(valueL8.OLI.pred)
valueL8.OLI.pred.df <- valueL8.OLI.pred.df[complete.cases(valueL8.OLI.pred.df), ]
mean(valueL8.OLI.pred.df)
sd(valueL8.OLI.pred.df)

######chroma#######
chromaS2.MSI <- train(x=cbind(data.train.cor2[,16:18],data.train.cor2[,22:24]), y=data.train.cor2$chroma, method="cubist")
chromaS2.MSI.pred <- predict(S2.MSI[[-4:-6]], chromaS2.MSI)
plot(chromaS2.MSI.pred)
writeRaster(chromaS2.MSI.pred, "chroma_S2MSI_single.tif", overwrite=TRUE)
chromaS2.MSI.pred.data <- predict(chromaS2.MSI, newdata = cbind(dados2.cor[,15:17], dados2.cor[,21:23]))
chromaS2.MSI.pred.df <- as.data.frame(chromaS2.MSI.pred)
chromaS2.MSI.pred.df <- chromaS2.MSI.pred.df[complete.cases(chromaS2.MSI.pred.df), ]
mean(chromaS2.MSI.pred.df)
sd(chromaS2.MSI.pred.df)


chromaL8.OLI <- train(x=data.train.cor2[,10:15], y=data.train.cor2$chroma, method="cubist")
chromaL8.OLI.pred <- predict(L8.OLI, chromaL8.OLI)
plot(chromaL8.OLI.pred)
writeRaster(chromaL8.OLI.pred, "chroma_L8OLI_single.tif", overwrite=TRUE)
chromaL8.OLI.pred.data <- predict(chromaL8.OLI, newdata =  dados2.cor[,9:14])
chromaL8.OLI.pred.df <- as.data.frame(chromaL8.OLI.pred)
chromaL8.OLI.pred.df <- chromaL8.OLI.pred.df[complete.cases(chromaL8.OLI.pred.df), ]
mean(chromaL8.OLI.pred.df)
sd(chromaL8.OLI.pred.df)

L8.OLI.S2MSI.data.cor <- as.data.frame(cbind(dados2.cor[,1:8], hueS2.MSI.pred.data, hueL8.OLI.pred.data, valueS2.MSI.pred.data,
                                       valueL8.OLI.pred.data, chromaS2.MSI.pred.data, chromaL8.OLI.pred.data))


list.files()

clay <- raster("chromaMapSEN.tif")
clay.df <- as.data.frame(clay)
clay.df <- clay.df[complete.cases(clay.df),]
mean(clay.df)
sd(clay.df)



write.csv(L8.OLI.S2MSI.data.cor, "dados_preditos_cor_p_semivariogram.csv")
########carregar imagens SYSI####
setwd("C:/Users/FREY/Google Drive/TESE/Projeto PIRA/raster/SYSI_pira") # Adjusting directory
list.files()
mosaico.sysi = list.files(paste(getwd()), pattern = "SYSI_Landsat_3years_20cloud.tif")
mosaico.sen = list.files(paste(getwd()), pattern = "SYSI_Sentinel_WGS84_254.tif")
mosaico.fused = list.files(paste(getwd()), pattern = "SYSI_FUSED_S2MSI_L8OLI_STACK_semREDEDGE.tif")

#Spliting band of SEN

SEN_1 = raster(mosaico.sen[[1]], band = 1)
SEN_2 = raster(mosaico.sen[[1]], band = 2)
SEN_3 = raster(mosaico.sen[[1]], band = 3)
SEN_4 = raster(mosaico.sen[[1]], band = 4)
SEN_5 = raster(mosaico.sen[[1]], band = 5)
SEN_6 = raster(mosaico.sen[[1]], band = 6)
SEN_7 = raster(mosaico.sen[[1]], band = 7)
SEN_8 = raster(mosaico.sen[[1]], band = 8)
SEN_9 = raster(mosaico.sen[[1]], band = 9)
plot(SEN_1)
plot(SYSI_1)
plot(Fused_1)

#Spliting band of SYSI (Synthetic Soil Image)
SYSI_1 = raster(mosaico.sysi[[1]], band = 1)
SYSI_1[SYSI_1 == 0] <- NA
SYSI_2 = raster(mosaico.sysi[[1]], band = 2)
SYSI_2[SYSI_2 == 0] <- NA
SYSI_3 = raster(mosaico.sysi[[1]], band = 3)
SYSI_3[SYSI_3 == 0] <- NA
SYSI_4 = raster(mosaico.sysi[[1]], band = 4)
SYSI_4[SYSI_4 == 0] <- NA
SYSI_5 = raster(mosaico.sysi[[1]], band = 5)
SYSI_5[SYSI_5 == 0] <- NA
SYSI_6 = raster(mosaico.sysi[[1]], band = 6)
SYSI_6[SYSI_6 == 0] <- NA

#Spliting band of Fused (Synthetic Soil Image)
Fused_1 = raster(mosaico.fused[[1]], band = 1)
plot(Fused_1)
Fused_1[Fused_1 == 0] <- NA
Fused_2 = raster(mosaico.fused[[1]], band = 2)
Fused_2[Fused_2 == 0] <- NA
Fused_3 = raster(mosaico.fused[[1]], band = 3)
Fused_3[Fused_3 == 0] <- NA
Fused_4 = raster(mosaico.fused[[1]], band = 4)
Fused_4[Fused_4 == 0] <- NA
Fused_5 = raster(mosaico.fused[[1]], band = 5)
Fused_5[Fused_5 == 0] <- NA
Fused_6 = raster(mosaico.fused[[1]], band = 6)
Fused_6[Fused_6 == 0] <- NA



CovStack = stack(SYSI_1, SYSI_2, SYSI_3, SYSI_4, SYSI_5, SYSI_6)
tempD <- data.frame(cellNos = seq(1:ncell(CovStack)))
vals <- as.data.frame(getValues(CovStack))
tempD <- cbind(tempD, vals)
tempD <- tempD[complete.cases(tempD), ]
cellNos <- c(tempD$cellNos)
gXY <- data.frame(xyFromCell(CovStack, cellNos, spatial = F))
tempD <- cbind(gXY, tempD)
str(tempD)
summary(tempD)
names(tempD)[4:9] <- c("SYSI1", "SYSI2", "SYSI3", "SYSI4", "SYSI5", "SYSI6")       
crs(CovStack)


#####DF STACKS
CovStack.SEN = stack(SEN_1, SEN_2, SEN_3, SEN_4, SEN_5, SEN_6, SEN_7, SEN_8, SEN_9)
tempD.SEN <- data.frame(cellNos = seq(1:ncell(CovStack.SEN)))
vals.SEN <- as.data.frame(getValues(CovStack.SEN))
tempD.SEN <- cbind(tempD.SEN, vals.SEN)
tempD.SEN <- tempD.SEN[complete.cases(tempD.SEN), ]
cellNos.SEN <- c(tempD.SEN$cellNos)
gXY.SEN <- data.frame(xyFromCell(CovStack.SEN, cellNos.SEN, spatial = F))
tempD.SEN <- cbind(gXY.SEN, tempD.SEN)

str(tempD.SEN)
summary(tempD.SEN)
names(tempD.SEN)[4:12] <- c("SYSI_Sen_1", "SYSI_Sen_2", "SYSI_Sen_3", "SYSI_Sen_4",  "SYSI_Sen_5",  "SYSI_Sen_6", "SYSI_Sen_7", "SYSI_Sen_8", "SYSI_Sen_9")       
crs(CovStack.SEN)
head(tempD.SEN)
tempD.SEN.six <- tempD.SEN[,-7:-9]
tempD.SEN.six <- tempD.SEN.six[,-9]




CovStack.Fused = stack(Fused_1, Fused_2, Fused_3, Fused_4, Fused_5, Fused_6)
tempD.Fused <- data.frame(cellNos = seq(1:ncell(CovStack.Fused)))
vals.Fused <- as.data.frame(getValues(CovStack.Fused))
tempD.Fused <- cbind(tempD.Fused, vals.Fused)
tempD.Fused <- tempD.Fused[complete.cases(tempD.Fused), ]
cellNos.Fused <- c(tempD.Fused$cellNos)
gXY.Fused <- data.frame(xyFromCell(CovStack.Fused, cellNos.Fused, spatial = F))
tempD.Fused <- cbind(gXY.Fused, tempD.Fused)
str(tempD.Fused)
summary(tempD.Fused)
names(tempD.Fused)[4:9] <- c("FUSED1", "FUSED2", "FUSED3", "FUSED4", "FUSED5", "FUSED6")       
crs(CovStack.Fused)


claySEN9bands
claySEN
clay.SYSI
clay.Fused

##Predicoes##
summary(claySEN9bands)      #resumen de la prediccion
head(claySEN9bands)
summary(train.cor$Clay.gkg)
map.claySEN9bands <- predict(claySEN9bands, newdata = tempD.SEN)      #nueva prediccion con datos del area de interes 
map.claySEN9bands<- cbind(data.frame(tempD.SEN[, c("x", "y")]), map.claySEN9bands)
summary(map.claySEN9bands)
map.claySEN9bands<- map.claySEN9bands[which(map.claySEN9bands$map.claySEN9bands >=0),]
map.claySEN9bands.Raster <- rasterFromXYZ(as.data.frame(map.claySEN9bands[, 1:3]))
plot(map.claySEN9bands.Raster, main = "Clay Sen Sentinel map")
crs(map.claySEN9bands.Raster) <- "+init=epsg:4326"

writeRaster(map.claySEN9bands.Raster, "ClayMapSEN9bands.tif", overwrite=TRUE)

summary(claySEN)      #resumen de la prediccion
head(claySEN)
summary(train.cor$Clay.gkg)
map.claySEN <- predict(claySEN, newdata = tempD.SEN.six)      #nueva prediccion con datos del area de interes 
map.claySEN <- cbind(data.frame(tempD.SEN.six[, c("x", "y")]), map.claySEN)
summary(map.claySEN)
map.claySEN<- map.claySEN[which(map.claySEN$map.claySEN >=0),]
map.claySEN.Raster <- rasterFromXYZ(as.data.frame(map.claySEN[, 1:3]))
plot(map.claySEN.Raster, main = "Clay Sen Sentinel map six bands")
crs(map.claySEN.Raster) <- "+init=epsg:4326"

writeRaster(map.claySEN.Raster, "ClayMapSEN.tif", overwrite=TRUE)

summary(clay.SYSI)      #resumen de la prediccion
head(clay.SYSI)
summary(train.cor$Clay.gkg)
map.clay.SYSI <- predict(clay.SYSI, newdata = tempD)      #nueva prediccion con datos del area de interes 
map.clay.SYSI <- cbind(data.frame(tempD[, c("x", "y")]), map.clay.SYSI)
summary(map.clay.SYSI)
map.clay.SYSI<- map.clay.SYSI[which(map.clay.SYSI$map.clay.SYSI >=0),]
map.claySYSI.Raster <- rasterFromXYZ(as.data.frame(map.clay.SYSI[, 1:3]))
plot(map.claySYSI.Raster, main = "Clay SYSI map six bands")
crs(map.claySYSI.Raster) <- "+init=epsg:4326"

writeRaster(map.claySYSI.Raster, "ClayMapSYSI.tif")

summary(clay.Fused)      #resumen de la prediccion
head(clay.Fused)
summary(train.cor$Clay.gkg)
map.clay.Fused <- predict(clay.Fused, newdata = tempD.Fused)      #nueva prediccion con datos del area de interes 
map.clay.Fused <- cbind(data.frame(tempD.Fused[, c("x", "y")]), map.clay.Fused)
summary(map.clay.Fused)
map.clay.Fused <- map.clay.Fused[which(map.clay.Fused$map.clay.Fused >=0),]
map.clay.Fused.Raster <- rasterFromXYZ(as.data.frame(map.clay.Fused[, 1:3]))
plot(map.clay.Fused.Raster, main = "Clay clay.Fused map")
crs(map.clay.Fused.Raster) <- "+init=epsg:4326"

writeRaster(map.clay.Fused.Raster, "ClayMapFused.tif")



###SAND####
##Predicoes##
summary(sandSEN9bands)      #resumen de la prediccion
head(sandSEN9bands)
summary(train.cor$Sand.gkg)
map.sandSEN9bands <- predict(sandSEN9bands, newdata = tempD.SEN)      #nueva prediccion con datos del area de interes 
map.sandSEN9bands<- cbind(data.frame(tempD.SEN[, c("x", "y")]), map.sandSEN9bands)
summary(map.sandSEN9bands)
map.sandSEN9bands<- map.sandSEN9bands[which(map.sandSEN9bands$map.sandSEN9bands >=0),]
map.sandSEN9bands.Raster <- rasterFromXYZ(as.data.frame(map.sandSEN9bands[, 1:3]))
plot(map.sandSEN9bands.Raster, main = "sand Sen Sentinel map")
crs(map.sandSEN9bands.Raster) <- "+init=epsg:4326"

writeRaster(map.sandSEN9bands.Raster, "sandMapSEN9bands.tif", overwrite=TRUE)

summary(sandSEN)      #resumen de la prediccion
head(sandSEN)
summary(train.cor$sand.gkg)
map.sandSEN <- predict(sandSEN, newdata = tempD.SEN.six)      #nueva prediccion con datos del area de interes 
map.sandSEN <- cbind(data.frame(tempD.SEN.six[, c("x", "y")]), map.sandSEN)
summary(map.sandSEN)
map.sandSEN<- map.sandSEN[which(map.sandSEN$map.sandSEN >=0),]
map.sandSEN.Raster <- rasterFromXYZ(as.data.frame(map.sandSEN[, 1:3]))
plot(map.sandSEN.Raster, main = "sand Sen Sentinel map six bands")
crs(map.sandSEN.Raster) <- "+init=epsg:4326"

writeRaster(map.sandSEN.Raster, "sandMapSEN.tif", overwrite=TRUE)

summary(sand.SYSI)      #resumen de la prediccion
head(sand.SYSI)
summary(train.cor$Sand.gkg)
map.sand.SYSI <- predict(sand.SYSI, newdata = tempD)      #nueva prediccion con datos del area de interes 
map.sand.SYSI <- cbind(data.frame(tempD[, c("x", "y")]), map.sand.SYSI)
summary(map.sand.SYSI)
map.sand.SYSI<- map.sand.SYSI[which(map.sand.SYSI$map.sand.SYSI >=0),]
map.sandSYSI.Raster <- rasterFromXYZ(as.data.frame(map.sand.SYSI[, 1:3]))
plot(map.sandSYSI.Raster, main = "sand SYSI map six bands")
crs(map.sandSYSI.Raster) <- "+init=epsg:4326"

writeRaster(map.sandSYSI.Raster, "sandMapSYSI.tif")

summary(sand.Fused)      #resumen de la prediccion
head(sand.Fused)
summary(train.cor$sand.gkg)
map.sand.Fused <- predict(sand.Fused, newdata = tempD.Fused)      #nueva prediccion con datos del area de interes 
map.sand.Fused <- cbind(data.frame(tempD.Fused[, c("x", "y")]), map.sand.Fused)
summary(map.sand.Fused)
map.sand.Fused <- map.sand.Fused[which(map.sand.Fused$map.sand.Fused >=0),]
map.sand.Fused.Raster <- rasterFromXYZ(as.data.frame(map.sand.Fused[, 1:3]))
plot(map.sand.Fused.Raster, main = "sand sandFused map")
crs(map.sand.Fused.Raster) <- "+init=epsg:4326"

writeRaster(map.sand.Fused.Raster, "sandMapFused.tif")




###silt####
##Predicoes##
summary(siltSEN9bands)      #resumen de la prediccion
head(siltSEN9bands)
summary(train.cor$Silt.gkg)
map.siltSEN9bands <- predict(siltSEN9bands, newdata = tempD.SEN)      #nueva prediccion con datos del area de interes 
map.siltSEN9bands<- cbind(data.frame(tempD.SEN[, c("x", "y")]), map.siltSEN9bands)
summary(map.siltSEN9bands)
map.siltSEN9bands<- map.siltSEN9bands[which(map.siltSEN9bands$map.siltSEN9bands >=0),]
map.siltSEN9bands.Raster <- rasterFromXYZ(as.data.frame(map.siltSEN9bands[, 1:3]))
plot(map.siltSEN9bands.Raster, main = "silt Sen Sentinel map")
crs(map.siltSEN9bands.Raster) <- "+init=epsg:4326"

writeRaster(map.siltSEN9bands.Raster, "siltMapSEN9bands.tif", overwrite=TRUE)

summary(tempD.SEN.six)      #resumen de la prediccion
head(siltSEN)
summary(train.cor$Silt.gkg)
map.siltSEN <- predict(siltSEN, newdata = tempD.SEN.six)      #nueva prediccion con datos del area de interes 
map.siltSEN <- cbind(data.frame(tempD.SEN.six[, c("x", "y")]), map.siltSEN)
summary(map.siltSEN)
map.siltSEN<- map.siltSEN[which(map.siltSEN$map.siltSEN >=0),]
map.siltSEN.Raster <- rasterFromXYZ(as.data.frame(map.siltSEN[, 1:3]))
plot(map.siltSEN.Raster, main = "silt Sen Sentinel map six bands")
crs(map.siltSEN.Raster) <- "+init=epsg:4326"

writeRaster(map.siltSEN.Raster, "siltMapSEN.tif", overwrite=TRUE)

summary(silt.SYSI)      #resumen de la prediccion
head(silt.SYSI)
summary(train.cor$silt.gkg)
map.silt.SYSI <- predict(silt.SYSI, newdata = tempD)      #nueva prediccion con datos del area de interes 
map.silt.SYSI <- cbind(data.frame(tempD[, c("x", "y")]), map.silt.SYSI)
summary(map.silt.SYSI)
map.silt.SYSI<- map.silt.SYSI[which(map.silt.SYSI$map.silt.SYSI >=0),]
map.siltSYSI.Raster <- rasterFromXYZ(as.data.frame(map.silt.SYSI[, 1:3]))
plot(map.siltSYSI.Raster, main = "silt SYSI map six bands")
crs(map.siltSYSI.Raster) <- "+init=epsg:4326"

writeRaster(map.siltSYSI.Raster, "siltMapSYSI.tif")

summary(silt.Fused)      #resumen de la prediccion
head(silt.Fused)
summary(train.cor$silt.gkg)
map.silt.Fused <- predict(silt.Fused, newdata = tempD.Fused)      #nueva prediccion con datos del area de interes 
map.silt.Fused <- cbind(data.frame(tempD.Fused[, c("x", "y")]), map.silt.Fused)
summary(map.silt.Fused)
map.silt.Fused <- map.silt.Fused[which(map.silt.Fused$map.silt.Fused >=0),]
map.silt.Fused.Raster <- rasterFromXYZ(as.data.frame(map.silt.Fused[, 1:3]))
plot(map.silt.Fused.Raster, main = "silt siltFused map")
crs(map.silt.Fused.Raster) <- "+init=epsg:4326"

writeRaster(map.silt.Fused.Raster, "siltMapFused.tif")



###som####
##Predicoes##
summary(somSEN9bands)      #resumen de la prediccion
head(somSEN9bands)
summary(train.cor$OM.gkg)
map.somSEN9bands <- predict(somSEN9bands, newdata = tempD.SEN)      #nueva prediccion con datos del area de interes 
map.somSEN9bands<- cbind(data.frame(tempD.SEN[, c("x", "y")]), map.somSEN9bands)
summary(map.somSEN9bands)
map.somSEN9bands<- map.somSEN9bands[which(map.somSEN9bands$map.somSEN9bands >=0),]
map.somSEN9bands.Raster <- rasterFromXYZ(as.data.frame(map.somSEN9bands[, 1:3]))
plot(map.somSEN9bands.Raster, main = "som Sen Sentinel map")
crs(map.somSEN9bands.Raster) <- "+init=epsg:4326"

writeRaster(map.somSEN9bands.Raster, "somMapSEN9bands.tif", overwrite=TRUE)

summary(somSEN)      #resumen de la prediccion
head(somSEN)
summary(train.cor$som.gkg)
map.somSEN <- predict(somSEN, newdata = tempD.SEN.six)      #nueva prediccion con datos del area de interes 
map.somSEN <- cbind(data.frame(tempD.SEN.six[, c("x", "y")]), map.somSEN)
summary(map.somSEN)
map.somSEN.Raster <- rasterFromXYZ(as.data.frame(map.somSEN[, 1:3]))
plot(map.somSEN.Raster, main = "som Sen Sentinel map six bands")
crs(map.somSEN.Raster) <- "+init=epsg:4326"

writeRaster(map.somSEN.Raster, "somMapSEN.tif", overwrite=TRUE)

summary(som.SYSI)      #resumen de la prediccion
head(som.SYSI)
summary(train.cor$som.gkg)
map.som.SYSI <- predict(som.SYSI, newdata = tempD)      #nueva prediccion con datos del area de interes 
map.som.SYSI <- cbind(data.frame(tempD[, c("x", "y")]), map.som.SYSI)
summary(map.som.SYSI)
map.som.SYSI<- map.som.SYSI[which(map.som.SYSI$map.som.SYSI >=0),]
map.somSYSI.Raster <- rasterFromXYZ(as.data.frame(map.som.SYSI[, 1:3]))
plot(map.somSYSI.Raster, main = "som SYSI map six bands")
crs(map.somSYSI.Raster) <- "+init=epsg:4326"

writeRaster(map.somSYSI.Raster, "somMapSYSI.tif")

summary(som.Fused)      #resumen de la prediccion
head(som.Fused)
summary(train.cor$som.gkg)
map.som.Fused <- predict(som.Fused, newdata = tempD.Fused)      #nueva prediccion con datos del area de interes 
map.som.Fused <- cbind(data.frame(tempD.Fused[, c("x", "y")]), map.som.Fused)
summary(map.som.Fused)
map.som.Fused <- map.som.Fused[which(map.som.Fused$map.som.Fused >=0),]
map.som.Fused.Raster <- rasterFromXYZ(as.data.frame(map.som.Fused[, 1:3]))
plot(map.som.Fused.Raster, main = "som somFused map")
crs(map.som.Fused.Raster) <- "+init=epsg:4326"

writeRaster(map.som.Fused.Raster, "somMapFused.tif")


colnames(tempD.SEN)[4:12] <- c("SEN1", "SEN2", "SEN3", "SEN4", "SEN5", "SEN6", "SEN7", "SEN8", "SEN9")
tempD.SEN.six <- tempD.SEN[,-7:-9]
colnames(tempD) <- c("SYSI1", "SYSI2", "SYSI3", "SYSI4", "SYSI5", "SYSI6")
###som####
##Predicoes##
summary(hueSEN9bands)      #resumen de la prediccion
head(hueSEN9bands)
summary(train.color$Hue)
map.hueSEN9bands <- predict(hueSEN9bands, newdata = tempD.SEN)      #nueva prediccion con datos del area de interes 
map.hueSEN9bands<- cbind(data.frame(tempD.SEN[, c("x", "y")]), map.hueSEN9bands)
summary(map.hueSEN9bands)
map.hueSEN9bands<- map.hueSEN9bands[which(map.hueSEN9bands$map.hueSEN9bands >=0),]
map.hueSEN9bands.Raster <- rasterFromXYZ(as.data.frame(map.hueSEN9bands[, 1:3]))
plot(map.hueSEN9bands.Raster, main = "hue Sen Sentinel map")
crs(map.hueSEN9bands.Raster) <- "+init=epsg:4326"

writeRaster(map.hueSEN9bands.Raster, "hueMapSEN9bands1.tif", overwrite=TRUE)

summary(tempD.SEN.six)      #resumen de la prediccion
head(hueSEN)
summary(train.cor$hue.gkg)
map.hueSEN <- predict(hueSEN, newdata = tempD.SEN.six)      #nueva prediccion con datos del area de interes 
map.hueSEN <- cbind(data.frame(tempD.SEN.six[, c("x", "y")]), map.hueSEN)
summary(map.hueSEN)
map.hueSEN.Raster <- rasterFromXYZ(as.data.frame(map.hueSEN[, 1:3]))
plot(map.hueSEN.Raster, main = "hue Sen Sentinel map six bands")
crs(map.hueSEN.Raster) <- "+init=epsg:4326"

writeRaster(map.hueSEN.Raster, "hueMapSEN1.tif", overwrite=TRUE)

summary(hue.SYSI)      #resumen de la prediccion
head(hue.SYSI)
summary(train.cor$hue.gkg)
map.hue.SYSI <- predict(hue.SYSI, newdata = tempD)      #nueva prediccion con datos del area de interes 
map.hue.SYSI <- cbind(data.frame(tempD[, c("x", "y")]), map.hue.SYSI)
summary(map.hue.SYSI)
map.hue.SYSI<- map.hue.SYSI[which(map.hue.SYSI$map.hue.SYSI >=0),]
map.hueSYSI.Raster <- rasterFromXYZ(as.data.frame(map.hue.SYSI[, 1:3]))
plot(map.hueSYSI.Raster, main = "hue SYSI map six bands")
crs(map.hueSYSI.Raster) <- "+init=epsg:4326"

writeRaster(map.hueSYSI.Raster, "hueMapSYSI.tif", overwrite=TRUE)

summary(hue.Fused)      #resumen de la prediccion
head(hue.Fused)
summary(train.cor$hue.gkg)
map.hue.Fused <- predict(hue.Fused, newdata = tempD.Fused)      #nueva prediccion con datos del area de interes 
map.hue.Fused <- cbind(data.frame(tempD.Fused[, c("x", "y")]), map.hue.Fused)
summary(map.hue.Fused)
map.hue.Fused <- map.hue.Fused[which(map.hue.Fused$map.hue.Fused >=0),]
map.hue.Fused.Raster <- rasterFromXYZ(as.data.frame(map.hue.Fused[, 1:3]))
plot(map.hue.Fused.Raster, main = "hue hueFused map")
crs(map.hue.Fused.Raster) <- "+init=epsg:4326"

writeRaster(map.hue.Fused.Raster, "hueMapFused.tif", overwrite=TRUE)


###som####
##Predicoes##
summary(valueSEN9bands)      #resumen de la prediccion
head(valueSEN9bands)
summary(train.color$value)
map.valueSEN9bands <- predict(valueSEN9bands, newdata = tempD.SEN)      #nueva prediccion con datos del area de interes 
map.valueSEN9bands<- cbind(data.frame(tempD.SEN[, c("x", "y")]), map.valueSEN9bands)
summary(map.valueSEN9bands)
map.valueSEN9bands<- map.valueSEN9bands[which(map.valueSEN9bands$map.valueSEN9bands >=0),]
map.valueSEN9bands.Raster <- rasterFromXYZ(as.data.frame(map.valueSEN9bands[, 1:3]))
plot(map.valueSEN9bands.Raster, main = "value Sen Sentinel map")
crs(map.valueSEN9bands.Raster) <- "+init=epsg:4326"

writeRaster(map.valueSEN9bands.Raster, "valueMapSEN9bands.tif", overwrite=TRUE)

summary(tempD.SEN.six)      #resumen de la prediccion
head(valueSEN)
summary(train.cor$value.gkg)
map.valueSEN <- predict(valueSEN, newdata = tempD.SEN.six)      #nueva prediccion con datos del area de interes 
map.valueSEN <- cbind(data.frame(tempD.SEN.six[, c("x", "y")]), map.valueSEN)
summary(map.valueSEN)
map.valueSEN.Raster <- rasterFromXYZ(as.data.frame(map.valueSEN[, 1:3]))
plot(map.valueSEN.Raster, main = "value Sen Sentinel map six bands")
crs(map.valueSEN.Raster) <- "+init=epsg:4326"

writeRaster(map.valueSEN.Raster, "valueMapSEN2.tif", overwrite=TRUE)

summary(value.SYSI)      #resumen de la prediccion
head(value.SYSI)
summary(train.cor$value.gkg)
map.value.SYSI <- predict(value.SYSI, newdata = tempD)      #nueva prediccion con datos del area de interes 
map.value.SYSI <- cbind(data.frame(tempD[, c("x", "y")]), map.value.SYSI)
summary(map.value.SYSI)
map.value.SYSI<- map.value.SYSI[which(map.value.SYSI$map.value.SYSI >=0),]
map.valueSYSI.Raster <- rasterFromXYZ(as.data.frame(map.value.SYSI[, 1:3]))
plot(map.valueSYSI.Raster, main = "value SYSI map six bands")
crs(map.valueSYSI.Raster) <- "+init=epsg:4326"

writeRaster(map.valueSYSI.Raster, "valueMapSYSI.tif", overwrite=TRUE)

summary(value.Fused)      #resumen de la prediccion
head(value.Fused)
summary(train.cor$value.gkg)
map.value.Fused <- predict(value.Fused, newdata = tempD.Fused)      #nueva prediccion con datos del area de interes 
map.value.Fused <- cbind(data.frame(tempD.Fused[, c("x", "y")]), map.value.Fused)
summary(map.value.Fused)
map.value.Fused <- map.value.Fused[which(map.value.Fused$map.value.Fused >=0),]
map.value.Fused.Raster <- rasterFromXYZ(as.data.frame(map.value.Fused[, 1:3]))
plot(map.value.Fused.Raster, main = "value valueFused map")
crs(map.value.Fused.Raster) <- "+init=epsg:4326"

writeRaster(map.value.Fused.Raster, "valueMapFused.tif", overwrite=TRUE)



###som####
##Predicoes##
summary(chromaSEN9bands)      #resumen de la prediccion
head(chromaSEN9bands)
summary(train.color$chroma)
map.chromaSEN9bands <- predict(chromaSEN9bands, newdata = tempD.SEN)      #nueva prediccion con datos del area de interes 
map.chromaSEN9bands<- cbind(data.frame(tempD.SEN[, c("x", "y")]), map.chromaSEN9bands)
summary(map.chromaSEN9bands)
map.chromaSEN9bands<- map.chromaSEN9bands[which(map.chromaSEN9bands$map.chromaSEN9bands >=0),]
map.chromaSEN9bands.Raster <- rasterFromXYZ(as.data.frame(map.chromaSEN9bands[, 1:3]))
plot(map.chromaSEN9bands.Raster, main = "chroma Sen Sentinel map")
crs(map.chromaSEN9bands.Raster) <- "+init=epsg:4326"

writeRaster(map.chromaSEN9bands.Raster, "chromaMapSEN9bands.tif", overwrite=TRUE)

summary(tempD.SEN.six)      #resumen de la prediccion
head(chromaSEN)
summary(train.cor$chroma.gkg)
map.chromaSEN <- predict(chromaSEN, newdata = tempD.SEN.six)      #nueva prediccion con datos del area de interes 
map.chromaSEN <- cbind(data.frame(tempD.SEN.six[, c("x", "y")]), map.chromaSEN)
summary(map.chromaSEN)
map.chromaSEN.Raster <- rasterFromXYZ(as.data.frame(map.chromaSEN[, 1:3]))
plot(map.chromaSEN.Raster, main = "chroma Sen Sentinel map six bands")
crs(map.chromaSEN.Raster) <- "+init=epsg:4326"

writeRaster(map.chromaSEN.Raster, "chromaMapSEN.tif", overwrite=TRUE)

summary(chroma.SYSI)      #resumen de la prediccion
head(chroma.SYSI)
summary(train.cor$chroma.gkg)
map.chroma.SYSI <- predict(chroma.SYSI, newdata = tempD)      #nueva prediccion con datos del area de interes 
map.chroma.SYSI <- cbind(data.frame(tempD[, c("x", "y")]), map.chroma.SYSI)
summary(map.chroma.SYSI)
map.chroma.SYSI<- map.chroma.SYSI[which(map.chroma.SYSI$map.chroma.SYSI >=0),]
map.chromaSYSI.Raster <- rasterFromXYZ(as.data.frame(map.chroma.SYSI[, 1:3]))
plot(map.chromaSYSI.Raster, main = "chroma SYSI map six bands")
crs(map.chromaSYSI.Raster) <- "+init=epsg:4326"

writeRaster(map.chromaSYSI.Raster, "chromaMapSYSI.tif", overwrite=TRUE)

summary(chroma.Fused)      #resumen de la prediccion
head(chroma.Fused)
summary(train.cor$chroma.gkg)
map.chroma.Fused <- predict(chroma.Fused, newdata = tempD.Fused)      #nueva prediccion con datos del area de interes 
map.chroma.Fused <- cbind(data.frame(tempD.Fused[, c("x", "y")]), map.chroma.Fused)
summary(map.chroma.Fused)
map.chroma.Fused <- map.chroma.Fused[which(map.chroma.Fused$map.chroma.Fused >=0),]
map.chroma.Fused.Raster <- rasterFromXYZ(as.data.frame(map.chroma.Fused[, 1:3]))
plot(map.chroma.Fused.Raster, main = "chroma chromaFused map")
crs(map.chroma.Fused.Raster) <- "+init=epsg:4326"

writeRaster(map.chroma.Fused.Raster, "chromaMapFused.tif", overwrite=TRUE)



summary(claySEN9bands)
claySEN
clay.SYSI
clay.Fused

#####Variable Importance
##Clay##
claySEN.IMP <- varImp(claySEN)
claySEN.IMP1 <- as.data.frame(claySEN.IMP$importance)
claySEN.IMP1$X1 <- rownames(claySEN.IMP1)
rownames(claySEN.IMP1) <- NULL
claySEN.IMP1 <- claySEN.IMP1[order(claySEN.IMP1$Overall),]
claySEN.IMP1$X1 <- as.factor(c("SWIR1", "Blue", "Green", "NIR","Red","SWIR2"))
claySEN.IMP1$X1 <- factor(claySEN.IMP1$X1)
claySEN.IMP1$Class <- as.factor(c("Red", "Red", "Red", "Red", "Red", "Black"))
claySEN.IMP1$X1 <- factor(claySEN.IMP1$X1, levels=unique(as.character(claySEN.IMP1$X1)))
levels(claySEN.IMP1$X1)

require(ggplot2)
require(farver)
p1 <- ggplot(claySEN.IMP1, aes(x=X1, y = Overall, color = Class)) + 
  geom_segment(aes(x = claySEN.IMP1$X1, xend = claySEN.IMP1$X1, y = 0, yend = claySEN.IMP1$Overall), size =1) + 
  geom_point(size=3) +  ylab("") + theme_minimal() +
  xlab("Clay content") + coord_flip() + theme(legend.position = "none")  + theme(axis.text.y = element_text(size=9, color = "black"))
p1



##sand##
clay.SYSI.IMP <- varImp(clay.SYSI)
clay.SYSI.IMP1 <- as.data.frame(clay.SYSI.IMP$importance)
clay.SYSI.IMP1$X1 <- rownames(clay.SYSI.IMP1)
rownames(clay.SYSI.IMP1) <- NULL
clay.SYSI.IMP1 <- clay.SYSI.IMP1[order(clay.SYSI.IMP1$Overall),]
clay.SYSI.IMP1$X1 <- as.factor(c("NIR", "SWIR1", "Blue", "Red","Green","SWIR2"))
clay.SYSI.IMP1$X1 <- factor(clay.SYSI.IMP1$X1)
clay.SYSI.IMP1$Class <- as.factor(c("Red", "Red", "Red", "Red", "Red", "Black"))
clay.SYSI.IMP1$X1 <- factor(clay.SYSI.IMP1$X1, levels=unique(as.character(clay.SYSI.IMP1$X1)))
levels(clay.SYSI.IMP1$X1)

require(ggplot2)
require(farver)
p2 <- ggplot(clay.SYSI.IMP1, aes(x=X1, y = Overall, color = Class)) + 
  geom_segment(aes(x = clay.SYSI.IMP1$X1, xend = clay.SYSI.IMP1$X1, y = 0, yend = clay.SYSI.IMP1$Overall), size =1) + 
  geom_point(size=3) +  ylab("") + theme_minimal() +
  xlab("") + coord_flip() + theme(legend.position = "none")  + theme(axis.text.y = element_text(size=9, color = "black"))
p2


##Clay##
clay.Fused.IMP <- varImp(clay.Fused)
clay.Fused.IMP1 <- as.data.frame(clay.Fused.IMP$importance)
clay.Fused.IMP1$X1 <- rownames(clay.Fused.IMP1)
rownames(clay.Fused.IMP1) <- NULL
clay.Fused.IMP1 <- clay.Fused.IMP1[order(clay.Fused.IMP1$Overall),]
clay.Fused.IMP1$X1 <- as.factor(c("Blue", "NIR", "Green", "Red","SWIR1","SWIR2"))
clay.Fused.IMP1$X1 <- factor(clay.Fused.IMP1$X1)
clay.Fused.IMP1$Class <- as.factor(c("Red", "Red", "Red", "Red", "Red", "Black"))
clay.Fused.IMP1$X1 <- factor(clay.Fused.IMP1$X1, levels=unique(as.character(clay.Fused.IMP1$X1)))
levels(clay.Fused.IMP1$X1)

require(ggplot2)
require(farver)
p3 <- ggplot(clay.Fused.IMP1, aes(x=X1, y = Overall, color = Class)) + 
  geom_segment(aes(x = clay.Fused.IMP1$X1, xend = clay.Fused.IMP1$X1, y = 0, yend = clay.Fused.IMP1$Overall), size =1) + 
  geom_point(size=3) +  ylab("") + theme_minimal() +
  xlab("") + coord_flip() + theme(legend.position = "none")  + theme(axis.text.y = element_text(size=9, color = "black"))
p3


##sand##
sandSEN.IMP <- varImp(sandSEN)
sandSEN.IMP1 <- as.data.frame(sandSEN.IMP$importance)
sandSEN.IMP1$X1 <- rownames(sandSEN.IMP1)
rownames(sandSEN.IMP1) <- NULL
sandSEN.IMP1 <- sandSEN.IMP1[order(sandSEN.IMP1$Overall),]
sandSEN.IMP1$X1 <- as.factor(c("SWIR1", "NIR", "Red", "Blue","Green","SWIR2"))
sandSEN.IMP1$X1 <- factor(sandSEN.IMP1$X1)
sandSEN.IMP1$Class <- as.factor(c("Red", "Red", "Red", "Red", "Red", "Black"))
sandSEN.IMP1$X1 <- factor(sandSEN.IMP1$X1, levels=unique(as.character(sandSEN.IMP1$X1)))
levels(sandSEN.IMP1$X1)

require(ggplot2)
require(farver)
p4 <- ggplot(sandSEN.IMP1, aes(x=X1, y = Overall, color = Class)) + 
  geom_segment(aes(x = sandSEN.IMP1$X1, xend = sandSEN.IMP1$X1, y = 0, yend = sandSEN.IMP1$Overall), size =1) + 
  geom_point(size=3) +  ylab("") + theme_minimal() +
  xlab("Sand content") + coord_flip() + theme(legend.position = "none")  + theme(axis.text.y = element_text(size=9, color = "black"))
p4



##sand##
sand.SYSI.IMP <- varImp(sand.SYSI)
sand.SYSI.IMP1 <- as.data.frame(sand.SYSI.IMP$importance)
sand.SYSI.IMP1$X1 <- rownames(sand.SYSI.IMP1)
rownames(sand.SYSI.IMP1) <- NULL
sand.SYSI.IMP1 <- sand.SYSI.IMP1[order(sand.SYSI.IMP1$Overall),]
sand.SYSI.IMP1$X1 <- as.factor(c("Red", "Blue", "NIR", "Green","SWIR1","SWIR2"))
sand.SYSI.IMP1$X1 <- factor(sand.SYSI.IMP1$X1)
sand.SYSI.IMP1$Class <- as.factor(c("Red", "Red", "Red", "Red", "Red", "Black"))
sand.SYSI.IMP1$X1 <- factor(sand.SYSI.IMP1$X1, levels=unique(as.character(sand.SYSI.IMP1$X1)))
levels(sand.SYSI.IMP1$X1)

require(ggplot2)
require(farver)
p5 <- ggplot(sand.SYSI.IMP1, aes(x=X1, y = Overall, color = Class)) + 
  geom_segment(aes(x = sand.SYSI.IMP1$X1, xend = sand.SYSI.IMP1$X1, y = 0, yend = sand.SYSI.IMP1$Overall), size =1) + 
  geom_point(size=3) +  ylab("") + theme_minimal() +
  xlab("") + coord_flip() + theme(legend.position = "none")  + theme(axis.text.y = element_text(size=9, color = "black"))
p5


##sand##
sand.Fused.IMP <- varImp(sand.Fused)
sand.Fused.IMP1 <- as.data.frame(sand.Fused.IMP$importance)
sand.Fused.IMP1$X1 <- rownames(sand.Fused.IMP1)
rownames(sand.Fused.IMP1) <- NULL
sand.Fused.IMP1 <- sand.Fused.IMP1[order(sand.Fused.IMP1$Overall),]
sand.Fused.IMP1$X1 <- as.factor(c("Red", "NIR", "Green", "SWIR1","Blue","SWIR2"))
sand.Fused.IMP1$X1 <- factor(sand.Fused.IMP1$X1)
sand.Fused.IMP1$Class <- as.factor(c("Red", "Red", "Red", "Red", "Red", "Black"))
sand.Fused.IMP1$X1 <- factor(sand.Fused.IMP1$X1, levels=unique(as.character(sand.Fused.IMP1$X1)))
levels(sand.Fused.IMP1$X1)

require(ggplot2)
require(farver)
p6 <- ggplot(sand.Fused.IMP1, aes(x=X1, y = Overall, color = Class)) + 
  geom_segment(aes(x = sand.Fused.IMP1$X1, xend = sand.Fused.IMP1$X1, y = 0, yend = sand.Fused.IMP1$Overall), size =1) + 
  geom_point(size=3) +  ylab("") + theme_minimal() +
  xlab("") + coord_flip() + theme(legend.position = "none")  + theme(axis.text.y = element_text(size=9, color = "black"))
p6


##silt##
siltSEN.IMP <- varImp(siltSEN)
siltSEN.IMP1 <- as.data.frame(siltSEN.IMP$importance)
siltSEN.IMP1$X1 <- rownames(siltSEN.IMP1)
rownames(siltSEN.IMP1) <- NULL
siltSEN.IMP1 <- siltSEN.IMP1[order(siltSEN.IMP1$Overall),]
siltSEN.IMP1$X1 <- as.factor(c("SWIR1", "Blue", "Green", "Red","SWIR2","NIR"))
siltSEN.IMP1$X1 <- factor(siltSEN.IMP1$X1)
siltSEN.IMP1$Class <- as.factor(c("Red", "Black","Black","Black","Black", "Black"))
siltSEN.IMP1$X1 <- factor(siltSEN.IMP1$X1, levels=unique(as.character(siltSEN.IMP1$X1)))
levels(siltSEN.IMP1$X1)

require(ggplot2)
require(farver)
p7 <- ggplot(siltSEN.IMP1, aes(x=X1, y = Overall, color = Class)) + 
  geom_segment(aes(x = siltSEN.IMP1$X1, xend = siltSEN.IMP1$X1, y = 0, yend = siltSEN.IMP1$Overall), size =1) + 
  geom_point(size=3) +  ylab("") + theme_minimal() +
  xlab("Silt content") + coord_flip() + theme(legend.position = "none")  + theme(axis.text.y = element_text(size=9, color = "black"))
p7



##silt##
silt.SYSI.IMP <- varImp(silt.SYSI)
silt.SYSI.IMP1 <- as.data.frame(silt.SYSI.IMP$importance)
silt.SYSI.IMP1$X1 <- rownames(silt.SYSI.IMP1)
rownames(silt.SYSI.IMP1) <- NULL
silt.SYSI.IMP1 <- silt.SYSI.IMP1[order(silt.SYSI.IMP1$Overall),]
silt.SYSI.IMP1$X1 <- as.factor(c("Red", "Blue", "NIR", "SWIR1", "Green","SWIR2"))
silt.SYSI.IMP1$X1 <- factor(silt.SYSI.IMP1$X1)
silt.SYSI.IMP1$Class <- as.factor(c("Red", "Black", "Black", "Black", "Black", "Black"))
silt.SYSI.IMP1$X1 <- factor(silt.SYSI.IMP1$X1, levels=unique(as.character(silt.SYSI.IMP1$X1)))
levels(silt.SYSI.IMP1$X1)

require(ggplot2)
require(farver)
p8 <- ggplot(silt.SYSI.IMP1, aes(x=X1, y = Overall, color = Class)) + 
  geom_segment(aes(x = silt.SYSI.IMP1$X1, xend = silt.SYSI.IMP1$X1, y = 0, yend = silt.SYSI.IMP1$Overall), size =1) + 
  geom_point(size=3) +  ylab("") + theme_minimal() +
  xlab("") + coord_flip() + theme(legend.position = "none")  + theme(axis.text.y = element_text(size=9, color = "black"))
p8


##silt##
silt.Fused.IMP <- varImp(silt.Fused)
silt.Fused.IMP1 <- as.data.frame(silt.Fused.IMP$importance)
silt.Fused.IMP1$X1 <- rownames(silt.Fused.IMP1)
rownames(silt.Fused.IMP1) <- NULL
silt.Fused.IMP1 <- silt.Fused.IMP1[order(silt.Fused.IMP1$Overall),]
silt.Fused.IMP1$X1 <- as.factor(c("Green", "Red", "NIR", "SWIR1","Blue","SWIR2"))
silt.Fused.IMP1$X1 <- factor(silt.Fused.IMP1$X1)
silt.Fused.IMP1$Class <- as.factor(c("Red", "Red", "Red", "Red", "Red", "Black"))
silt.Fused.IMP1$X1 <- factor(silt.Fused.IMP1$X1, levels=unique(as.character(silt.Fused.IMP1$X1)))
levels(silt.Fused.IMP1$X1)

require(ggplot2)
require(farver)
p9 <- ggplot(silt.Fused.IMP1, aes(x=X1, y = Overall, color = Class)) + 
  geom_segment(aes(x = silt.Fused.IMP1$X1, xend = silt.Fused.IMP1$X1, y = 0, yend = silt.Fused.IMP1$Overall), size =1) + 
  geom_point(size=3) +  ylab("") + theme_minimal() +
  xlab("") + coord_flip() + theme(legend.position = "none")  + theme(axis.text.y = element_text(size=9, color = "black"))
p9



##som##
somSEN.IMP <- varImp(somSEN)
somSEN.IMP1 <- as.data.frame(somSEN.IMP$importance)
somSEN.IMP1$X1 <- rownames(somSEN.IMP1)
rownames(somSEN.IMP1) <- NULL
somSEN.IMP1 <- somSEN.IMP1[order(somSEN.IMP1$Overall),]
somSEN.IMP1$X1 <- as.factor(c("NIR", "Blue", "SWIR1", "Green","Red","SWIR2"))
somSEN.IMP1$X1 <- factor(somSEN.IMP1$X1)
somSEN.IMP1$Class <- as.factor(c("Red", "Red","Red","Red","Red", "Black"))
somSEN.IMP1$X1 <- factor(somSEN.IMP1$X1, levels=unique(as.character(somSEN.IMP1$X1)))
levels(somSEN.IMP1$X1)

require(ggplot2)
require(farver)
p10 <- ggplot(somSEN.IMP1, aes(x=X1, y = Overall, color = Class)) + 
  geom_segment(aes(x = somSEN.IMP1$X1, xend = somSEN.IMP1$X1, y = 0, yend = somSEN.IMP1$Overall), size =1) + 
  geom_point(size=3) +  ylab("") + theme_minimal() +
  xlab("OM content") + coord_flip() + theme(legend.position = "none")  + theme(axis.text.y = element_text(size=9, color = "black"))
p10



##som##
som.SYSI.IMP <- varImp(som.SYSI)
som.SYSI.IMP1 <- as.data.frame(som.SYSI.IMP$importance)
som.SYSI.IMP1$X1 <- rownames(som.SYSI.IMP1)
rownames(som.SYSI.IMP1) <- NULL
som.SYSI.IMP1 <- som.SYSI.IMP1[order(som.SYSI.IMP1$Overall),]
som.SYSI.IMP1$X1 <- as.factor(c("NIR", "Red", "Blue", "SWIR1", "Green","SWIR2"))
som.SYSI.IMP1$X1 <- factor(som.SYSI.IMP1$X1)
som.SYSI.IMP1$Class <- as.factor(c("Red", "Red","Red","Red","Black", "Black"))
som.SYSI.IMP1$X1 <- factor(som.SYSI.IMP1$X1, levels=unique(as.character(som.SYSI.IMP1$X1)))
levels(som.SYSI.IMP1$X1)

require(ggplot2)
require(farver)
p11 <- ggplot(som.SYSI.IMP1, aes(x=X1, y = Overall, color = Class)) + 
  geom_segment(aes(x = som.SYSI.IMP1$X1, xend = som.SYSI.IMP1$X1, y = 0, yend = som.SYSI.IMP1$Overall), size =1) + 
  geom_point(size=3) +  ylab("") + theme_minimal() +
  xlab("") + coord_flip() + theme(legend.position = "none")  + theme(axis.text.y = element_text(size=9, color = "black"))
p11


##som##
som.Fused.IMP <- varImp(som.Fused)
som.Fused.IMP1 <- as.data.frame(som.Fused.IMP$importance)
som.Fused.IMP1$X1 <- rownames(som.Fused.IMP1)
rownames(som.Fused.IMP1) <- NULL
som.Fused.IMP1 <- som.Fused.IMP1[order(som.Fused.IMP1$Overall),]
som.Fused.IMP1$X1 <- as.factor(c("Blue", "SWIR1", "NIR", "Red","Green","SWIR2"))
som.Fused.IMP1$X1 <- factor(som.Fused.IMP1$X1)
som.Fused.IMP1$Class <- as.factor(c("Red", "Red", "Red", "Red", "Red", "Black"))
som.Fused.IMP1$X1 <- factor(som.Fused.IMP1$X1, levels=unique(as.character(som.Fused.IMP1$X1)))
levels(som.Fused.IMP1$X1)

require(ggplot2)
require(farver)
p12 <- ggplot(som.Fused.IMP1, aes(x=X1, y = Overall, color = Class)) + 
  geom_segment(aes(x = som.Fused.IMP1$X1, xend = som.Fused.IMP1$X1, y = 0, yend = som.Fused.IMP1$Overall), size =1) + 
  geom_point(size=3) +  ylab("") + theme_minimal() +
  xlab("") + coord_flip() + theme(legend.position = "none")  + theme(axis.text.y = element_text(size=9, color = "black"))
p12

##hue##
hueSEN.IMP <- varImp(hueSEN)
hueSEN.IMP1 <- as.data.frame(hueSEN.IMP$importance)
hueSEN.IMP1$X1 <- rownames(hueSEN.IMP1)
rownames(hueSEN.IMP1) <- NULL
hueSEN.IMP1 <- hueSEN.IMP1[order(hueSEN.IMP1$Overall),]
hueSEN.IMP1$X1 <- as.factor(c("SWIR2", "NIR", "SWIR1", "Red","Blue","Green"))
hueSEN.IMP1$X1 <- factor(hueSEN.IMP1$X1)
hueSEN.IMP1$Class <- as.factor(c("Red", "Red","Red","Black","Black", "Black"))
hueSEN.IMP1$X1 <- factor(hueSEN.IMP1$X1, levels=unique(as.character(hueSEN.IMP1$X1)))
levels(hueSEN.IMP1$X1)

require(ggplot2)
require(farver)
p13 <- ggplot(hueSEN.IMP1, aes(x=X1, y = Overall, color = Class)) + 
  geom_segment(aes(x = hueSEN.IMP1$X1, xend = hueSEN.IMP1$X1, y = 0, yend = hueSEN.IMP1$Overall), size =1) + 
  geom_point(size=3) +  ylab("") + theme_minimal() +
  xlab("Hue") + coord_flip() + theme(legend.position = "none")  + theme(axis.text.y = element_text(size=9, color = "black"))
p13


dev.off()
##hue##
hue.SYSI.IMP <- varImp(hue.SYSI)
hue.SYSI.IMP1 <- as.data.frame(hue.SYSI.IMP$importance)
hue.SYSI.IMP1$X1 <- rownames(hue.SYSI.IMP1)
rownames(hue.SYSI.IMP1) <- NULL
hue.SYSI.IMP1 <- hue.SYSI.IMP1[order(hue.SYSI.IMP1$Overall),]
hue.SYSI.IMP1$X1 <- as.factor(c("SWIR1", "NIR", "SWIR2", "Blue", "Green","Red"))
hue.SYSI.IMP1$X1 <- factor(hue.SYSI.IMP1$X1)
hue.SYSI.IMP1$Class <- as.factor(c("Red", "Red","Black","Black","Black", "Black"))
hue.SYSI.IMP1$X1 <- factor(hue.SYSI.IMP1$X1, levels=unique(as.character(hue.SYSI.IMP1$X1)))
levels(hue.SYSI.IMP1$X1)

require(ggplot2)
require(farver)
p14 <- ggplot(hue.SYSI.IMP1, aes(x=X1, y = Overall, color = Class)) + 
  geom_segment(aes(x = hue.SYSI.IMP1$X1, xend = hue.SYSI.IMP1$X1, y = 0, yend = hue.SYSI.IMP1$Overall), size =1) + 
  geom_point(size=3) +  ylab("") + theme_minimal() +
  xlab("") + coord_flip() + theme(legend.position = "none")  + theme(axis.text.y = element_text(size=9, color = "black"))
p14


##hue##
hue.Fused.IMP <- varImp(hue.Fused)
hue.Fused.IMP1 <- as.data.frame(hue.Fused.IMP$importance)
hue.Fused.IMP1$X1 <- rownames(hue.Fused.IMP1)
rownames(hue.Fused.IMP1) <- NULL
hue.Fused.IMP1 <- hue.Fused.IMP1[order(hue.Fused.IMP1$Overall),]
hue.Fused.IMP1$X1 <- as.factor(c("NIR", "Blue", "SWIR1", "SWIR2","Red","Green"))
hue.Fused.IMP1$X1 <- factor(hue.Fused.IMP1$X1)
hue.Fused.IMP1$Class <- as.factor(c("Red", "Red", "Red", "Red", "Black", "Black"))
hue.Fused.IMP1$X1 <- factor(hue.Fused.IMP1$X1, levels=unique(as.character(hue.Fused.IMP1$X1)))
levels(hue.Fused.IMP1$X1)

require(ggplot2)
require(farver)
p15 <- ggplot(hue.Fused.IMP1, aes(x=X1, y = Overall, color = Class)) + 
  geom_segment(aes(x = hue.Fused.IMP1$X1, xend = hue.Fused.IMP1$X1, y = 0, yend = hue.Fused.IMP1$Overall), size =1) + 
  geom_point(size=3) +  ylab("") + theme_minimal() +
  xlab("") + coord_flip() + theme(legend.position = "none")  + theme(axis.text.y = element_text(size=9, color = "black"))
p15


##value##
valueSEN.IMP <- varImp(valueSEN)
valueSEN.IMP1 <- as.data.frame(valueSEN.IMP$importance)
valueSEN.IMP1$X1 <- rownames(valueSEN.IMP1)
rownames(valueSEN.IMP1) <- NULL
valueSEN.IMP1 <- valueSEN.IMP1[order(valueSEN.IMP1$Overall),]
valueSEN.IMP1$X1 <- as.factor(c("SWIR1", "SWIR2", "NIR", "Red","Blue","Green"))
valueSEN.IMP1$X1 <- factor(valueSEN.IMP1$X1)
valueSEN.IMP1$Class <- as.factor(c("Red", "Red","Red","Red","Black", "Black"))
valueSEN.IMP1$X1 <- factor(valueSEN.IMP1$X1, levels=unique(as.character(valueSEN.IMP1$X1)))
levels(valueSEN.IMP1$X1)

require(ggplot2)
require(farver)
p16 <- ggplot(valueSEN.IMP1, aes(x=X1, y = Overall, color = Class)) + 
  geom_segment(aes(x = valueSEN.IMP1$X1, xend = valueSEN.IMP1$X1, y = 0, yend = valueSEN.IMP1$Overall), size =1) + 
  geom_point(size=3) +  ylab("") + theme_minimal() +
  xlab("value") + coord_flip() + theme(legend.position = "none")  + theme(axis.text.y = element_text(size=9, color = "black"))
p16



##value##
value.SYSI.IMP <- varImp(value.SYSI)
value.SYSI.IMP1 <- as.data.frame(value.SYSI.IMP$importance)
value.SYSI.IMP1$X1 <- rownames(value.SYSI.IMP1)
rownames(value.SYSI.IMP1) <- NULL
value.SYSI.IMP1 <- value.SYSI.IMP1[order(value.SYSI.IMP1$Overall),]
value.SYSI.IMP1$X1 <- as.factor(c("NIR", "SWIR2", "SWIR1", "Red", "Blue","Green"))
value.SYSI.IMP1$X1 <- factor(value.SYSI.IMP1$X1)
value.SYSI.IMP1$Class <- as.factor(c("Red", "Red","Red","Red","Black", "Black"))
value.SYSI.IMP1$X1 <- factor(value.SYSI.IMP1$X1, levels=unique(as.character(value.SYSI.IMP1$X1)))
levels(value.SYSI.IMP1$X1)

require(ggplot2)
require(farver)
p17 <- ggplot(value.SYSI.IMP1, aes(x=X1, y = Overall, color = Class)) + 
  geom_segment(aes(x = value.SYSI.IMP1$X1, xend = value.SYSI.IMP1$X1, y = 0, yend = value.SYSI.IMP1$Overall), size =1) + 
  geom_point(size=3) +  ylab("") + theme_minimal() +
  xlab("") + coord_flip() + theme(legend.position = "none")  + theme(axis.text.y = element_text(size=9, color = "black"))
p17


##value##
value.Fused.IMP <- varImp(value.Fused)
value.Fused.IMP1 <- as.data.frame(value.Fused.IMP$importance)
value.Fused.IMP1$X1 <- rownames(value.Fused.IMP1)
rownames(value.Fused.IMP1) <- NULL
value.Fused.IMP1 <- value.Fused.IMP1[order(value.Fused.IMP1$Overall),]
value.Fused.IMP1$X1 <- as.factor(c("SWIR1", "SWIR2","NIR", "Red","Green", "Blue"))
value.Fused.IMP1$X1 <- factor(value.Fused.IMP1$X1)
value.Fused.IMP1$Class <- as.factor(c("Red", "Red", "Red", "Black", "Black", "Black"))
value.Fused.IMP1$X1 <- factor(value.Fused.IMP1$X1, levels=unique(as.character(value.Fused.IMP1$X1)))
levels(value.Fused.IMP1$X1)

require(ggplot2)
require(farver)
p18 <- ggplot(value.Fused.IMP1, aes(x=X1, y = Overall, color = Class)) + 
  geom_segment(aes(x = value.Fused.IMP1$X1, xend = value.Fused.IMP1$X1, y = 0, yend = value.Fused.IMP1$Overall), size =1) + 
  geom_point(size=3) +  ylab("") + theme_minimal() +
  xlab("") + coord_flip() + theme(legend.position = "none")  + theme(axis.text.y = element_text(size=9, color = "black"))
p18


##chroma##
chromaSEN.IMP <- varImp(chromaSEN)
chromaSEN.IMP1 <- as.data.frame(chromaSEN.IMP$importance)
chromaSEN.IMP1$X1 <- rownames(chromaSEN.IMP1)
rownames(chromaSEN.IMP1) <- NULL
chromaSEN.IMP1 <- chromaSEN.IMP1[order(chromaSEN.IMP1$Overall),]
chromaSEN.IMP1$X1 <- as.factor(c("NIR", "SWIR1", "SWIR2", "Green","Red","Blue"))
chromaSEN.IMP1$X1 <- factor(chromaSEN.IMP1$X1)
chromaSEN.IMP1$Class <- as.factor(c("Red", "Red","Red","Red","Black", "Black"))
chromaSEN.IMP1$X1 <- factor(chromaSEN.IMP1$X1, levels=unique(as.character(chromaSEN.IMP1$X1)))
levels(chromaSEN.IMP1$X1)

require(ggplot2)
require(farver)
p19 <- ggplot(chromaSEN.IMP1, aes(x=X1, y = Overall, color = Class)) + 
  geom_segment(aes(x = chromaSEN.IMP1$X1, xend = chromaSEN.IMP1$X1, y = 0, yend = chromaSEN.IMP1$Overall), size =1) + 
  geom_point(size=3) +  ylab("") + theme_minimal() +
  xlab("chroma") + coord_flip() + theme(legend.position = "none")  + theme(axis.text.y = element_text(size=9, color = "black"))
p19



##chroma##
chroma.SYSI.IMP <- varImp(chroma.SYSI)
chroma.SYSI.IMP1 <- as.data.frame(chroma.SYSI.IMP$importance)
chroma.SYSI.IMP1$X1 <- rownames(chroma.SYSI.IMP1)
rownames(chroma.SYSI.IMP1) <- NULL
chroma.SYSI.IMP1 <- chroma.SYSI.IMP1[order(chroma.SYSI.IMP1$Overall),]
chroma.SYSI.IMP1$X1 <- as.factor(c("NIR", "SWIR1", "SWIR2", "Red", "Green","Blue"))
chroma.SYSI.IMP1$X1 <- factor(chroma.SYSI.IMP1$X1)
chroma.SYSI.IMP1$Class <- as.factor(c("Red", "Red","Red","Black","Black", "Black"))
chroma.SYSI.IMP1$X1 <- factor(chroma.SYSI.IMP1$X1, levels=unique(as.character(chroma.SYSI.IMP1$X1)))
levels(chroma.SYSI.IMP1$X1)

require(ggplot2)
require(farver)
p20 <- ggplot(chroma.SYSI.IMP1, aes(x=X1, y = Overall, color = Class)) + 
  geom_segment(aes(x = chroma.SYSI.IMP1$X1, xend = chroma.SYSI.IMP1$X1, y = 0, yend = chroma.SYSI.IMP1$Overall), size =1) + 
  geom_point(size=3) +  ylab("") + theme_minimal() +
  xlab("") + coord_flip() + theme(legend.position = "none")  + theme(axis.text.y = element_text(size=9, color = "black"))
p20


##chroma##
chroma.Fused.IMP <- varImp(chroma.Fused)
chroma.Fused.IMP1 <- as.data.frame(chroma.Fused.IMP$importance)
chroma.Fused.IMP1$X1 <- rownames(chroma.Fused.IMP1)
rownames(chroma.Fused.IMP1) <- NULL
chroma.Fused.IMP1 <- chroma.Fused.IMP1[order(chroma.Fused.IMP1$Overall),]
chroma.Fused.IMP1$X1 <- as.factor(c("NIR", "SWIR1","SWIR2", "Red","Green", "Blue"))
chroma.Fused.IMP1$X1 <- factor(chroma.Fused.IMP1$X1)
chroma.Fused.IMP1$Class <- as.factor(c("Red", "Red", "Red", "Black", "Black", "Black"))
chroma.Fused.IMP1$X1 <- factor(chroma.Fused.IMP1$X1, levels=unique(as.character(chroma.Fused.IMP1$X1)))
levels(chroma.Fused.IMP1$X1)

require(ggplot2)
require(farver)
p21 <- ggplot(chroma.Fused.IMP1, aes(x=X1, y = Overall, color = Class)) + 
  geom_segment(aes(x = chroma.Fused.IMP1$X1, xend = chroma.Fused.IMP1$X1, y = 0, yend = chroma.Fused.IMP1$Overall), size =1) + 
  geom_point(size=3) +  ylab("") + theme_minimal() +
  xlab("") + coord_flip() + theme(legend.position = "none")  + theme(axis.text.y = element_text(size=9, color = "black"))
p21


tiff("var_imp_plot2.tif", width = 2200, height = 4000, res = 300)
ggarrange(p1, p2, p3,p4, p5, p6,p7, p8, p9,p10, p11, p12, p13, p14, p15,p16, p17, p18,p19, p20, p21, nrow=7, ncol = 3)
dev.off()

##Definir dados.A como objeto espacial##
coordinates(map.clay.Fused) <- ~ x + y
hist(map.clay.Fused$map.clay.Fused)
solo_atr <- map.clay.Fused$map.clay.Fused
g <- gstat(id="solo_atr", formula = solo_atr~1, data = map.clay.Fused)
print(max(dist(map.clay.Fused@coords/2)))
print(min(dist(map.clay.Fused@coords)))

varClayFused <- gstat::variogram(g, width = 60)
plot(varClayFused)

fit.exp <- fit.variogram(var_exp, vgm(4, "Sph", 120, 0))   #vgm(psill, model, range, nugget)

