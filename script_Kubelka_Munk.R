library(prospectr)
list.files()


data.spectra <- read.csv(file = "SSL_brazil_spectra.csv", header=TRUE, sep = ";")[,-1:-11]
rownames(data.spectra) <- data.spectra[,1]

colnames(data.spectra) <- seq(from=350, to=2500, by=1)

#Transformação Kubelka-Munk
espectrotrans =
  ((1-data.spectra)^2)/(2*data.spectra)
matplot(seq(1,2151,by=1), data.spectra[,1:2151], type =
          "l")

write.table(espectrotrans, "espectro_trans.txt")
str(espectrotrans)


#Continuum removal#
CR <- continuumRemoval(data.spectra[10001:17382,], wav = seq(350, 2500, by=1), "R")
plot(colnames(data.spectra), CR[23,], type = "l")
CR_all <- cbind(data.spectra[,1:18], CR)
CR.A <- aggregate(CR_all [, 1:2169], list(CR_all$Texture), mean)
CR.A <- as.data.frame(CR)
write.csv(CR.A.round,"CR_spectra10000_17000.csv")
CR.A.round <- round(CR.A,5)

#Segunda Derivada (m=2)
#Função polinomial de segunda ordem (p=3) e janela de 15 nm (w=15)
segundaderivada55 =
  as.data.frame(savitzkyGolay(espectrotrans[10001:17382,],p=2, m=2, w=45))
str(segundaderivada55)

segundaderivada25 =
  savitzkyGolay(espectrotrans,p=2, m=2, w=55)

segundaderivada15 =
  savitzkyGolay(espectrotrans,p=2, m=2, w=15)


SG.15 <-cbind(data.spectra[,1:18], segundaderivada15)
SG.25 <-cbind(data.spectra[,1:18], segundaderivada25)
SG.55 <-cbind(data.spectra[,1:18], segundaderivada55)


SG.A.15 <- aggregate(SG.15 [, 1:2115], list(SG.15$Texture), mean)
SG.A.25 <- aggregate(SG.25 [, 1:2115], list(SG.25$Texture), mean)
SG.A.55 <- aggregate(SG.55 [, 1:2115], list(SG.55$Texture), mean)



head(segundaderivada55)
SG.A.25 <- t(SG.A.25)
SG.A.55 <- t(SG.A.25)


SG <- as.data.frame(segundaderivada55)

SG.round <- round(SG,5)
write.csv(SG.round, "SG55_10001_17382.csv")
write.csv(SG.A.25, "Spectra_mean_by_textureW25.csv")
write.csv(SG.A.55, "Spectra_mean_by_textureW55.csv")



plot(seq(400,800, by=1), segundaderivada55[2,400:800], type = "l", xlab = "wavelenght", ylab="Second derivative of Kubelka-Munk function")
lines(seq(400,800, by=1), segundaderivada55[1,400:800], col = "blue")
lines(seq(400,800, by=1), segundaderivada55[3,400:800], col = "green")
lines(seq(400,800, by=1), segundaderivada55[4,400:800], col = "brown")
lines(seq(400,800, by=1), segundaderivada55[5,400:800], col = "pink")
lines(seq(400,800, by=1), segundaderivada55[6,400:800], col = "orange")
lines(seq(400,800, by=1), segundaderivada55[7,400:800], col = "yellow")
lines(seq(400,800, by=1), segundaderivada55[8,400:800], col = "red")


write.csv(d1, "firstder.csv")
write.csv(d2, "secondder.csv")

legend("topleft", legend = c("1st der", "2nd der"), lty = c(1, 1), col = 1:2)

