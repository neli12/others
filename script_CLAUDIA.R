memory.limit(size=)
memory.size(max = TRUE)
library(raster)
library(rgdal) 
library(cluster)
list.files()


if (!require("factoextra")){install.packages("factoextra");require("factoextra")}
if (!require("corrplot")){install.packages("corrplot");require("corrplot")}
#Define working directory
getwd()
#setwd("D:\\Wanderson_Backup_28_09_2017\\Artigos_Publicacoes\\Artigo_Degradacao_Ongoing\\Artigo_Degradacao\\BD\\R\\Tabelas_R_Stats\\Database")
setwd("H:/Cluster") # Adjusting directory ### 

#list all raster files, im asuming they are tif files, change if needed
list.files()

## Pads.tif
Pads = list.files(paste(getwd()), pattern = "Pads_2014.tif$")
Pads <- raster(Pads)
plot(Pads)
extent(Pads)

## FED.tif
FED = list.files(paste(getwd()), pattern = "FED_2014.tif$")
FED <- raster(FED)
plot(FED)
extent(FED)

## SM.tif
SM = list.files(paste(getwd()), pattern = "SM_2016.tif$")
SM <- raster(SM)
plot(SM)
extent(SM)

## Argila.tif
Argila = list.files(paste(getwd()), pattern = "Argila_2014.tif$")
Argila <- raster(Argila)
plot(Argila)
extent(Argila)

## FD.tif
FD = list.files(paste(getwd()), pattern = "FD_2014.tif$")
FD <- raster(FD)
plot(FD)
extent(FD)

## FEO.tif
FEO = list.files(paste(getwd()), pattern = "FEO_2014.tif$")
FEO <- raster(FEO)
plot(FEO)
extent(FEO)

## MO.tif
MO = list.files(paste(getwd()), pattern = "MO_2014.tif$")
MO <- raster(MO)
plot(MO)
extent(MO)

## P.tif
P = list.files(paste(getwd()), pattern = "P_2014.tif$")
P <- raster(P)
plot(P)
extent(P)

## Mh.tif
Mh = list.files(paste(getwd()), pattern = "Mh_2014.tif$")
Mh <- raster(Mh)
plot(Mh)
extent(Mh)


## Mt.tif
Mt = list.files(paste(getwd()), pattern = "Mt_2014.tif$")
Mt <- raster(Mt)
plot(Mt)
extent(Mt)

## ProdMilho.tif
ProdMilho = list.files(paste(getwd()), pattern = "ProdMilho_2014.tif$")
ProdMilho <- raster(ProdMilho)
plot(ProdMilho)
extent(ProdMilho)

## Grouping the raster and coverting them to DF
CovStack = stack(Argila, FD, FEO, Mh, Mt, MO, P, SM, Pads, FED)

summary(CovStack)
#tempD <- data.frame(cellNos = seq(1:ncell(CovStack)))
#vals <- as.data.frame(getValues(CovStack))
#tempD <- cbind(tempD, vals)
#tempD <- tempD[complete.cases(tempD), ]
#cellNos <- c(tempD$cellNos)
#gXY <- data.frame(xyFromCell(CovStack, cellNos, spatial = F))
#tempD <- cbind(gXY, tempD)
#str(tempD)
#names(tempD)[4:22] <- c("DEM", "LS_Factor", "LST_RTE", "CEC", "Clay", "Sand", "Silt", 
#                       "NDVI", "Rainfall", "Slope", "Total_Insolation", "TWI", "VDTCN",
#                       "SYSI_1", "SYSI_2", "SYSI_3", "SYSI_4", "SYSI_5" ,"SYSI_7")
crs(CovStack)

#str(tempD)
#summary(tempD)
#class(tempD)



#rm(cluster.MLDI) <- kmeans(tempD, 6, iter.max = 1000) ### kmeans, with 6 clusters
#if (cluster.MLDI$ifault==4) { stop("Failed in Quick-Transfer"); }
str(CovStack)
rvals <- getValues(CovStack[[1:10]])
summary(rvals)
str(rvals)
rvals[1,][27] <- NA

#fviz_nbclust(na.omit(rvals), FUNcluster = "clara", method = "silhouette") + theme_classic()

#Elbow Method for finding the optimal number of clusters
set.seed(123)
# Compute and plot wss for k = 2 to k = 15.
k.max <- 10
data <- (na.omit(scale(rvals)))
wss <- sapply(1:k.max, 
              function(k){kmeans(data, k, nstart=50,iter.max = 15 )$tot.withinss})
wss
tiff("plot1.tif", width =1500, height = 1800,  res = 300)
plot(1:k.max, wss,
     type="b", pch = 16, frame = FALSE, 
     xlab="Numero de agrupamentos",
     ylab="SST dentro do agrupamento")
dev.off()

idx <- 1:ncell(CovStack)
idx <- idx[-unique(which(is.na(rvals), arr.ind=TRUE)[,1])] 
str(idx)

clus <- cluster::clara(na.omit(scale(rvals)), k=5, metric = "euclidean", stand = TRUE, samples = 100, pamLike = T)
summary(clus)
print(clus)


clus3 <- cluster::clara(na.omit(scale(rvals)), k=3, metric = "euclidean", stand = TRUE, samples = 100, pamLike = T)


#Plot clus
tiff("plot1.tif", width =1000, height = 1500,  res = 300)
fviz_cluster(clus,
             palette = c("red", "blue", "black", "green", "yellow"), # color palette
             ellipse.type = "t", # Concentration ellipse
             geom = "point", pointsize = 1,
             ggtheme = theme_classic())
dev.off()

#Plot Clus3
tiff("plot2.tif", width =1500, height = 1000,  res = 300)
fviz_cluster(clus3,
             palette = c("red", "blue", "green", "yellow", "black"), # color palette
             ellipse.type = "t", # Concentration ellipse
             geom = "point", pointsize = 1,
             ggtheme = theme_classic())
dev.off()

#fviz_cluster(clus, rvals, ellipse.type = "norm", repel = T)

r.clust <- CovStack[[1]]
plot(r.clust)
r.clust[] <- NA

r.clust[idx] <- clus$clustering
plot(r.clust) 
crs(r.clust)
getwd()
writeRaster(r.clust, "Clusters_Simone_5CLUS", format = "GTiff", datatype = "FLT4S", overwrite = T)

# Statistics

#----------------------Just checking the method -----
## MLDI.tif
MLDI.stats = list.files(paste(getwd()), pattern = "Clusters_Simone_5CLUS.tif$")
MLDI.stats <- raster(MLDI.stats)
extent(MLDI.stats)
plot(MLDI.stats)


## Grouping the raster and coverting them to DF

CovStack_Final <- stack(Argila, P, MO, FEO, FD, Mt, Mh, FED, SM, Pads, ProdMilho, MLDI.stats)
tempD_Final <- data.frame(cellNos = seq(1:ncell(CovStack_Final)))
vals <- as.data.frame(getValues(CovStack_Final))
tempD_Final <- cbind(tempD_Final, vals)
tempD_Final <- tempD_Final[complete.cases(tempD_Final), ]
cellNos <- c(tempD_Final$cellNos)
gXY <- data.frame(xyFromCell(CovStack_Final, cellNos, spatial = F))
tempD_Final <- cbind(gXY, tempD_Final)
str(tempD_Final)
names(tempD_Final)[4:15] <- c("Argila", "P", "MO", "FEO", "FD", "Mt", "Mh", "FED", "SM", "Pads", "Produtividade", "MLDI")
class(tempD_Final)
summary(tempD_Final)
tempD_Final$MLDI <- as.factor(tempD_Final$MLDI)
names(tempD_Final)
hist(tempD_Final$Produtividade)

tapply(tempD_Final$Produtividade, tempD_Final$MLDI, mean)
ajuste <- lm(tempD_Final$Produtividade ~ tempD_Final$MLDI)
summary(ajuste)
anova(ajuste)
d <- HSD.test(ajuste, 'tempD_Final$MLDI')

ANOVA = aov(ajuste)
TUKEY <- TukeyHSD(x=ANOVA, 'tempD_Final$MLDI', conf.level=0.95)

# Tuckey test representation :
plot(TUKEY , las=1 , col="brown")



a1 <- aov(tempD_Final$SM ~ tempD_Final$MLDI)
plot(a1)
summary(a1)
posthoc <- TukeyHSD(x=a1, 'tempD$MLDI', conf.level=0.95)





library(corrplot)
corrplot(cor(tempD_Final[4:14]), method = "num", type = "upper")
DB.cor <- cor(tempD_Final[4:14])
res1 <- cor.mtest(tempD_Final[4:14], conf.level = .95, method = "pearson") # cor.mtest - Produces p-values and confidence intervals for each pair of input features
res2 <- cor.mtest(tempD_Final[4:14], conf.level = .99)
corrplot(DB.cor, p.mat = res1$p, sig.level = .05)
corrplot(DB.cor, p.mat = res1$p, insig = "label_sig", pch.col = "black", sig.level = .05,
         method = "number", type = "upper")

summary(tempD_Final)
# Creating tables
MLDI.1 = subset(tempD_Final, MLDI == 1)
summary(MLDI.1)
class(MLDI.1)

par(mfrow=c(1,5))
boxplot(MLDI.1$SM, ylim = c(0,14))

MLDI.2 = subset(tempD_Final, MLDI == 2)
summary(MLDI.2)
boxplot(MLDI.2$SM, ylim = c(0,14))

#lapply(MLDI.2, sd, na.rm = TRUE)

MLDI.3 = subset(tempD_Final, MLDI == 3)
summary(MLDI.3)
boxplot(MLDI.3$SM, ylim = c(0,14))
#lapply(MLDI.3, sd, na.rm = TRUE)

MLDI.4 = subset(tempD_Final, MLDI == 4)
summary(MLDI.4)
boxplot(MLDI.4$SM, ylim = c(0,14))
#lapply(MLDI.4, sd, na.rm = TRUE)

MLDI.5 = subset(tempD_Final, MLDI == 5)
summary(MLDI.5)
boxplot(MLDI.5$SM, ylim = c(0,14))
#lapply(MLDI.5, sd, na.rm = TRUE)

p1 <- density(MLDI.1$Produtividade)
p2 <- density(MLDI.2$Produtividade)
p3 <- density(MLDI.3$Produtividade)
p4 <- density(MLDI.4$Produtividade)
p5 <- density(MLDI.5$Produtividade)
plot(p1, col=rgb(0,0,0,0), lwd = 2, ylim = c(0,8))
polygon(p1, col=rgb(0,0,0,2/4))
polygon(p2, col=rgb(0,0,0,1/4))
polygon(p3, col=rgb(1,0,1,1/2))
polygon(p4, col=rgb(1,1,0,1/4), density = -2)
polygon(p5, col=rgb(1,0,1,1/4), density = -2)


c1 <- rgb(0,0,0,2/4)
c2 <- rgb(0,0,0,1/4)
c3 <- rgb(1,0,1,1/2)
c4 <- rgb(1,1,0,1/4)
c5 <- rgb(1,0,1,1/4)

legend('topleft',c('Cluster1','Cluster2', 'Cluster3', 'Cluster4', 'Cluster5'),
       fill = c(c1, c2, c3, c4, c5), bty = 'n',
       border = NA)

MLDI.1 <- do.call(cbind, lapply(MLDI.1, summary))
write.csv(MLDI.1, "MLDI3_1.csv" )

MLDI.2 <- do.call(cbind, lapply(MLDI.2, summary))
write.csv(MLDI.2, "MLDI3_2.csv")

MLDI.3 <- do.call(cbind, lapply(MLDI.3, summary))
write.csv(MLDI.3, "MLDI3_3.csv")

MLDI.4 <- do.call(cbind, lapply(MLDI.4, summary))
write.csv(MLDI.4, "MLDI_4.csv")

MLDI.5 <- do.call(cbind, lapply(MLDI.5, summary))
write.csv(MLDI.5, "MLDI_5.csv")


#----------------------Validation with Organic Matter and Bare soil frequency -----
## Validation of the models --------
DB.val <- read.csv("teste1.csv", header = T, sep = ";")
str(DB.val)
row.has.na <- apply(DB.val, 1, function(x){any(is.na(x))}) # Verifica linhas c/ NAs
sum(row.has.na)
DB.val <- DB.val[complete.cases(DB.val),]
colnames(DB.val)
DB.val1 <- DB.val[, c(3, 4)]
str(DB.val1)
names(DB.val1)[1:2] <- c("MO", "SDI")
colnames(DB.val1)
summary(DB.val1)

plot(DB.val1$SDI, DB.val1$MO, main = "Cubist", xlab="SDI", ylab="MO",
     xlim=c(0,40), ylim=c(0,40))
abline(0,1, lty=2, col="black")
abline(lm(DB.val1$SDI ~ DB.val$MO), col="red", lty=2)

# Funcao para R2_RMSE_RPD_RPIQ
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
goof(observed = DB.val1$SDI, predicted = DB.val1$MO, type = "DSM")



## OM_SVM.tif

OM = list.files(paste(getwd()), pattern = "Mapa_MO_gkg_SVM.tif$")
OM <- raster(OM)
extent(OM)
plot(OM)
extent(SYSI_7)
OM <- resample(OM, SYSI_7, method = "ngb")

OM1 <- read.csv("dados_MO_SDI.csv", h=TRUE, sep = ",")


OM5 <- OM1[complete.cases(OM1),]
str(OM5)
summary(OM5)

OM5$Clusters_M <- as.factor(OM5$Clusters_M )
colnames(OM5) <- c("ID", "X", "Y", "OM.gkg", "Cluster")
str(OM5)
levels(OM5$Cluster)
group_by(OM5, Cluster) %>%
  summarise(
    count = n(),
    mean = mean(OM.gkg, na.rm = TRUE),
    sd = sd(OM.gkg, na.rm = TRUE)
  )

levels(OM5$Cluster) <- c("Very Low", "Medium", "Low", "High", "Very High")

dev.off()
ggboxplot(OM5, x = "Cluster", y = "OM.gkg", 
          color = "Cluster", palette = c("red", "green", "blue", "yellow", "black"),
          order = c("Very Low",  "Low", "Medium", "High", "Very High"),
          ylab = "OM.gkg", xlab = "Cluster")

ggline(OM5, x = "Cluster", y = "OM.gkg",
       add = c("mean_se", 'jitter'),
       order = c("1", "3", "2", "4", "5"),
       ylab = "OM.gkg", xlab = "Cluster")



res.aov <- aov(OM.gkg ~ Cluster,data = OM5)
summary(res.aov)
tuk <- TukeyHSD(res.aov)
tiff("Tukeyplot.tif", res = 500, width = 2500, height = 2000)
plot(tuk, cex.axis = 0.5, cex.lab = 0.8, cex.main = 0.6)
dev.off()


pairwise.t.test(OM5$OM.gkg, OM5$Cluster,
                p.adjust.method = "BH")
plot(res.aov, 1)
library(car)
leveneTest(OM.gkg ~ Cluster, data = OM5)
oneway.test(OM.gkg ~ Cluster, data = OM5)
plot(res.aov, 2)
# Extract the residuals
aov_residuals <- residuals(object = res.aov )
# Run Shapiro-Wilk test
shapiro.test(x = aov_residuals )
kruskal.test(OM.gkg ~ Cluster, data = OM5)

hist(OM5$OM.gkg)
OM5$om.log <- log(OM5$OM.gkg)
boxplot(OM5$om.log)

res1 <- aov(om.log ~ Cluster,data = OM5)
aov_residuals1 <- residuals(object = res1)
shapiro.test(x = aov_residuals1)
plot(res1,2)

## Grouping the raster and coverting them to DF
CovStack_Val = stack(MLDI.stats, OM)
tempD_Val <- data.frame(cellNos = seq(1:ncell(CovStack_Val)))
vals <- as.data.frame(getValues(CovStack_Val))
tempD_Val <- cbind(tempD_Val, vals)
tempD_Val <- tempD_Val[complete.cases(tempD_Val), ]
cellNos <- c(tempD_Val$cellNos)
gXY <- data.frame(xyFromCell(CovStack_Val, cellNos, spatial = F))
tempD_Val <- cbind(gXY, tempD_Val)
str(tempD_Val)
names(tempD_Val)[4:5] <- c("SDI", "OM")
class(tempD_Val)
summary(tempD_Val)
corrplot(cor(tempD_Val[4:5]), method = "num", type = "upper")
DB.cor <- cor(tempD_Val[4:5])
res1 <- cor.mtest(tempD_Val[4:5], conf.level = .95, method = "pearson") # cor.mtest - Produces p-values and confidence intervals for each pair of input features
res2 <- cor.mtest(tempD_Val[4:5], conf.level = .99)
corrplot(DB.cor, p.mat = res1$p, sig.level = .05)
corrplot(DB.cor, p.mat = res1$p, insig = "label_sig", pch.col = "gray", sig.level = .05,
         method = "number", type = "upper")


# Graphs with confidence interval
if (!require(ggplot2)){install.packages("ggplot2"); require(ggplot2)}
if (!require(boot)){install.packages("boot"); require(boot)} 
if (!require(forcats)){install.packages("forcats"); require(forcats)} 

outlierKD <- function(dt, var) {
  var_name <- eval(substitute(var),eval(dt))
  na1 <- sum(is.na(var_name))
  m1 <- mean(var_name, na.rm = T)
  par(mfrow=c(2, 2), oma=c(0,0,3,0))
  boxplot(var_name, main="With outliers")
  hist(var_name, main="With outliers", xlab=NA, ylab=NA)
  outlier <- boxplot.stats(var_name)$out
  mo <- mean(outlier)
  var_name <- ifelse(var_name %in% outlier, NA, var_name)
  boxplot(var_name, main="Without outliers")
  hist(var_name, main="Without outliers", xlab=NA, ylab=NA)
  title("Outlier Check", outer=TRUE)
  na2 <- sum(is.na(var_name))
  cat("Outliers identified:", na2 - na1, "n")
  cat("Propotion (%) of outliers:", round((na2 - na1) / sum(!is.na(var_name))*100, 1), "n")
  cat("Mean of the outliers:", round(mo, 2), "n")
  m2 <- mean(var_name, na.rm = T)
  cat("Mean without removing outliers:", round(m1, 2), "n")
  cat("Mean if we remove outliers:", round(m2, 2), "n")
  response <- readline(prompt="Do you want to remove outliers and to replace with NA? [yes/no]: ")
  if(response == "y" | response == "yes"){
    dt[as.character(substitute(var))] <- invisible(var_name)
    assign(as.character(as.list(match.call())$dt), dt, envir = .GlobalEnv)
    cat("Outliers successfully removed", "n")
    return(invisible(dt))
  } else{
    cat("Nothing changed", "n")
    return(invisible(var_name))
  }
}
outlierKD(tempD_Val, OM)
dev.off()
summary(tempD_Val)
str(tempD_Val)
tempD_Val$value <- tempD_Val$OM + rnorm(2143957, 0, 1)
tempD_Val <- tempD_Val[complete.cases(tempD_Val), ]
d <- data.frame(group = tempD_Val$SDI, OM = tempD_Val$OM)
summary(d)
str(d)
d$value <- d$OM + rnorm(2143957, 0, 1)
d$group <- factor(d$group)
levels(d$group) <- c("Light", "Moderate", "Slight")

colnames(OM5)[5] <- "SDI"

p <- ggplot(OM5, aes(x= SDI , y=OM.gkg))

tiff("boxplot.tif", width = 2200, height = 1700, res = 500)
g_box <- p + geom_boxplot(aes(colour = SDI)) + aes(x = fct_reorder(SDI , OM.gkg)) + 
  labs(colour = "SDI") + labs(x = "SDI") + labs(y = expression(paste("Organic matter (g ", kg^{-1}, ")"))) + 
  theme(axis.text=element_text(size=8))
dev.off()

median_cl_boot <- function(x, conf = 0.95) {
  lconf <- (1 - conf)/2
  uconf <- 1 - lconf
  require(boot)
  bmedian <- function(x, ind) median(x[ind])
  bt <- boot(x, bmedian, 1000)
  bb <- boot.ci(bt, type = "perc")
  data.frame(y = median(x), ymin = quantile(bt$t, lconf), ymax = quantile(bt$t, 
                                                                          uconf))
}

g_box <- g_box + stat_summary(fun.data = median_cl_boot, geom = "errorbar", 
                              colour = "black") + stat_summary(fun.y = median, geom = "point", colour = "black")
g_box


g_box <- g_box + stat_summary(aes(label = round(..y.., 1)), fun.y = median, 
                              geom = "text", size = 4, vjust = -0.5)
g_box <- g_box + stat_summary(aes(label = round(..y.., 1)), fun.y = function(x) quantile(x, 
                                                                                         0.75), geom = "text", size = 4, vjust = -1)
g_box <- g_box + stat_summary(aes(label = round(..y.., 1)), fun.y = function(x) quantile(x, 
                                                                                         0.25), geom = "text", size = 4, vjust = 2)
g_box

str(d)
res1 <- cor.mtest(d[2:3], conf.level = .95, method = "pearson") # cor.mtest - Produces p-values and confidence intervals for each pair of input features
PI.cor <- cor(d[2:3])
corrplot(PI.cor, p.mat = res1$p, sig.level = .05)
corrplot(PI.cor, p.mat = res1$p, insig = "label_sig", pch.col = "gray", sig.level = .05,
         method = "number", type = "upper")

data.frame(unclass(summary(tempD_Final)), check.names = FALSE, stringsAsFactors = FALSE)
str(.Last.value)
t <- do.call(cbind, lapply(tempD_Final, summary))
write.csv(DB.cor, "./PI_SemNaN_Okay3CLUS.csv")
write.csv(t, "Descriptive_stats_Ok3LUS.csv")
