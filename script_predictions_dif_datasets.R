####################Cubist in R#################################################
##Load required packages####
library(caret)
library(raster)
library(topmodel)
library(SciViews)
library(factoextra)
list.files()
# Funcao para R2_RMSE_RPD_RPIQ ----
goof <- function(observed,predicted, plot.it = FALSE, type="DSM"){
  # Coefficient of determination
  rLM <- lm(predicted ~ observed)
  R2 <- as.matrix(summary(rLM)$adj.r.squared)
  
  # Standard error of prediction ^2
  SEP2 <- mean((observed - predicted)^2)
  
  # Standard error of prediction
  SEP <- sqrt(SEP2)
  
  #Bias
  bias <- mean(predicted) - mean(observed)
  
  # residual  variance
  SEP2c <- sum(((predicted - bias - observed)^2) / length(observed))
  SEPc <- sqrt(SEP2c)
  
  # ratio of performance to deviation
  RPD <- sd(observed) / SEP
  
  # Ratio of performance to interquartile distance
  IQ <- c(quantile(observed))[3] - c(quantile(observed))[2]
  RPIQ <- IQ / SEP
  
  # Concordance
  mx <- mean(observed)
  my <- mean(predicted)
  s2x <- var(observed)
  s2y <- var(predicted)
  sxy <- mean((observed-mx) * (predicted-my))
  ccc <- 2 * sxy / (s2x + s2y + (mx - my)^2)
  
  if (plot.it==TRUE){eqscplot(observed, predicted)
    abline(a = 0, b = 1, col = "brown4")}
  
  if (type == "DSM"){ gf <- data.frame(R2=R2, concordance=ccc, MSE=SEP2, RMSE=SEP, bias=bias, row.names=NULL)}
  else if (type == "spec"){ gf <- data.frame(R2=R2, concordance=ccc, MSE=SEP2, RMSE=SEP, bias=bias, 
                                             MSEc=SEP2c,RMSEc=SEPc, RPD=RPD, RPIQ=RPIQ, row.names=NULL)}
  else {stop("ERROR: Revise the type of output you require. Select from either DSM or spec")} 
  
  return(gf)
}

###Set working directory###
setwd("C:/Users/Usuario/Google Drive/GEOCIS/IC/Merylin/dados_2nd_revision")

###Load raster covariates, in our case, satellite image###
load("covsSYSICombined_WGS84.RData")
load("covsSYSIL8.RData")
load("covsSYSIS2MSI.RData")
Combined
covs.SYSI.L8
covs.SYSI.S2MSI

###Read dataset###
###Set working directory###
setwd("C:/Users/Usuario/Google Drive/GEOCIS/IC/Merylin/dados_3rd_revision")
dat1 <- read.csv("dados_parciais_pira.csv", sep=",")

##Separate the data##
dat.combined <- dat1[c("Lab_ID","X", "Y", "Clay.gkg", "Sand.gkg", "Silt.gkg", "OM.gkg", "C1", "C2", 
                       "C3", "C4", "C5", "C6")]
dat.S2MSI <- dat1[c("Lab_ID","X", "Y", "Clay.gkg", "Sand.gkg", "Silt.gkg", "OM.gkg", "SYSI.S2.1", "SYSI.S2.2", 
                 "SYSI.S2.3", "SYSI.S2.4", "SYSI.S2.5", "SYSI.S2.6", "SYSI.S2.7", "SYSI.S2.8", "SYSI.S2.9")]
dat.L8OLI <- dat1[c("Lab_ID","X", "Y", "Clay.gkg", "Sand.gkg", "Silt.gkg", "OM.gkg", "SYSI.L8.1", "SYSI.L8.2", 
                   "SYSI.L8.3", "SYSI.L8.4", "SYSI.L8.5", "SYSI.L8.6")]

dat.combined <- dat.combined[complete.cases(dat.combined), ]  #exclude NA's
dat.S2MSI <- dat.S2MSI[complete.cases(dat.S2MSI), ]  #exclude NA's
dat.L8OLI <- dat.L8OLI[complete.cases(dat.L8OLI), ]  #exclude NA's


###Separate again the covariates###
S2MSI.cov <- dat.S2MSI[,8:16] ##the numbers after the comma represent the column number of your dataset. You should change them if your covariates are in different column numbers
L8OLI.cov <- dat.L8OLI[,8:13]
Combined.cov <- dat.combined[,8:13]

##Select 30% of the data
val_S2MSI <- sample(nrow(dat.S2MSI), 315)
val_L8OLI <- sample(nrow(dat.L8OLI), 238)
val_Combined <- sample(nrow(dat.combined), 350)

###Split dataset and covariates into training and validation sets###
##Training##
dat.S2MSI.train <- dat.S2MSI[-val_S2MSI, ]   #dat3 corresponds to the data loaded in R (all of it)
dat.L8OLI.train <- dat.L8OLI[-val_L8OLI, ]   #dat3 corresponds to the data loaded in R (all of it)
dat.combined.train <- dat.combined[-val_Combined, ]   #dat3 corresponds to the data loaded in R (all of it)
hist(dat.S2MSI.train$Clay.gkg)
hist(dat.L8OLI.train$Clay.gkg)
hist(dat.combined.train$Clay.gkg)


S2MSI.cov.train <- S2MSI.cov[-val_S2MSI, ]  #The SYSI.cov data contains six columns corresponding to the SYSI vis-NIR-SWIR bands
L8OLI.cov.train <- L8OLI.cov[-val_L8OLI, ] 
Combined.cov.train <- Combined.cov[-val_Combined, ] 

##Test##
dat.S2MSI.test <- dat.S2MSI[val_S2MSI, ]   #dat3 corresponds to the data loaded in R (all of it)
dat.L8OLI.test <- dat.L8OLI[val_L8OLI, ]   #dat3 corresponds to the data loaded in R (all of it)
dat.combined.test <- dat.combined[val_Combined, ]   #dat3 corresponds to the data loaded in R (all of it)


S2MSI.cov.test <- S2MSI.cov[val_S2MSI, ]  #The SYSI.cov data contains six columns corresponding to the SYSI vis-NIR-SWIR bands
L8OLI.cov.test <- L8OLI.cov[val_L8OLI, ] 
Combined.cov.test <- Combined.cov[val_Combined, ] 


###Fit a cubist model to your data###
##The hyperparameters committees and neighbors were used by default. No hyperparameters were tunned##
clay.fit.S2MSI <- train(x = S2MSI.cov.train, y = dat.S2MSI.train$Clay.gkg, method = "cubist")
summary(clay.fit.S2MSI)   ##to see the models and rules created
clay.fit.S2MSI            ##to see the performance of the models

clay.fit.L8OLI <- train(x = L8OLI.cov.train, y = dat.L8OLI.train$Clay.gkg, method = "cubist")
summary(clay.fit.L8OLI)   ##to see the models and rules created
clay.fit.L8OLI            ##to see the performance of the models

clay.fit.Combined <- train(x = Combined.cov.train, y = dat.combined.train$Clay.gkg, method = "cubist")
summary(clay.fit.Combined)   ##to see the models and rules created
clay.fit.Combined            ##to see the performance of the models


##Sand
sand.fit.S2MSI <- train(x = S2MSI.cov.train, y = dat.S2MSI.train$Sand.gkg, method = "cubist")
summary(sand.fit.S2MSI)   ##to see the models and rules created
sand.fit.S2MSI            ##to see the performance of the models

sand.fit.L8OLI <- train(x = L8OLI.cov.train, y = dat.L8OLI.train$Sand.gkg, method = "cubist")
summary(sand.fit.L8OLI)   ##to see the models and rules created
sand.fit.L8OLI            ##to see the performance of the models

sand.fit.Combined <- train(x = Combined.cov.train, y = dat.combined.train$Sand.gkg, method = "cubist")
summary(sand.fit.Combined)   ##to see the models and rules created
sand.fit.Combined  

##silt
silt.fit.S2MSI <- train(x = S2MSI.cov.train, y = dat.S2MSI.train$Silt.gkg, method = "cubist")
summary(silt.fit.S2MSI)   ##to see the models and rules created
silt.fit.S2MSI            ##to see the performance of the models

silt.fit.L8OLI <- train(x = L8OLI.cov.train, y = dat.L8OLI.train$Silt.gkg, method = "cubist")
summary(silt.fit.L8OLI)   ##to see the models and rules created
silt.fit.L8OLI            ##to see the performance of the models

silt.fit.Combined <- train(x = Combined.cov.train, y = dat.combined.train$Silt.gkg, method = "cubist")
summary(silt.fit.Combined)   ##to see the models and rules created
silt.fit.Combined 

om.fit <- train(x = covariates_train, y = dat3_train$b, method = "cubist")
summary(om.fit)   ##to see the models and rules created
om.fit            ##to see the performance of the models

###Predictions training###
predicted.clay.train <- predict(clay.fit, covariates_train)   ##training
plot(dat3_train$Clay~predicted.clay.train)

predicted.L.train <- predict(L.fit, covariates_train)   ##training
plot(dat3_train$L~predicted.L.train)

predicted.a.train <- predict(a.fit, covariates_train)   ##training
plot(dat3_train$a~predicted.a.train)

predicted.b.train <- predict(b.fit, covariates_train)   ##training
plot(dat3_train$b~predicted.b.train)

##Performance of training set
goof(dat3_train$Clay, predicted.clay.train, type="spec")  
goof(dat3_train$L, predicted.L.train, type="spec")  
goof(dat3_train$a, predicted.a.train, type="spec")  
goof(dat3_train$b, predicted.b.train, type="spec")  

###Predictions validation###
predicted.clay.test <- predict(clay.fit, covariates_test)  ##validation
predicted.L.test <- predict(L.fit, covariates_test)  ##validation
predicted.a.test <- predict(a.fit, covariates_test)  ##validation
predicted.b.test <- predict(b.fit, covariates_test)  ##validation

##Performance of validation set
goof(dat3_test$Clay, predicted.clay.test, type="spec")
goof(dat3_test$L, predicted.L.test, type="spec")
goof(dat3_test$a, predicted.a.test, type="spec")
goof(dat3_test$b, predicted.b.test, type="spec")


###Predict to the entire study area using the raster covariates###
clay.raster <- raster::predict(SYSIStack, clay.fit)
crs(clay.raster) <- "+init=epsg:4326"   ##Set coordinates reference system to WGS 1984(degrees)
plot(clay.raster)


L.raster <- raster::predict(SYSIStack, L.fit)
crs(L.raster) <- "+init=epsg:4326"   ##Set coordinates reference system to WGS 1984(degrees)
plot(L.raster)

a.raster <- raster::predict(SYSIStack, a.fit)
crs(a.raster) <- "+init=epsg:4326"   ##Set coordinates reference system to WGS 1984(degrees)
plot(a.raster)

b.raster <- raster::predict(SYSIStack, b.fit)
crs(b.raster) <- "+init=epsg:4326"   ##Set coordinates reference system to WGS 1984(degrees)
plot(b.raster)

Lab <- stack(L.raster, a.raster, b.raster)
names(Lab) <- c("L", "a", "b")
plot(Lab)

##Save the predicted map##
writeRaster(clay.raster, "raster/clay.tif", overwrite=TRUE)
writeRaster(Lab, "raster/Lab.tif", overwrite=TRUE)


###Reclassify clay raster map###
clay.reclass <- reclassify(clay.raster, matrix(c(0,150,1,150,250,2,250,350,3,350,600,4,600,Inf,5), 
                                               ncol=3, byrow=TRUE))
plot(clay.reclass)

##Save the reclassified clay map##
writeRaster(clay.reclass, "raster/clay_reclass.tif", overwrite=TRUE)