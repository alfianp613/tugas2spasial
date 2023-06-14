library(spgwr)
library(ggplot2)
library(maptools)
library(lmtest)
library(car)
library(nortest)
library(DescTools)
library(spdep)
library(sf)
library(mapview)
library(tibble)
library(AER)
library(GWmodel)

setwd("D:/All about Collage/Perkuliahan/Semester 6/Analisis Data Spasial/Tugas + Kuis")
library(openxlsx)
covidjatim = read.xlsx("Spasial Covid 1_new.xlsx",sheet="Sheet1")
covidjatim
# covidjatim <- read.csv("Spasial Covid 1 (test).txt",sep=';')

#preprocessing
sum(is.na(covidjatim))
str(covidjatim)
covidjatim <- covidjatim[complete.cases(covidjatim), ]

#EDA
# -----GWR-----
## -----Regresi linier klasik-----
## Dataset:
## y = skor risiko
## X1 = Kepadatan Penduduk
## X2 = Jumlah Fasilitas Kesehatan
## X3 = Jumlah Masyarakat Miskin (ribu)
## X4 = Persentase keluhan kesehatan masyarakat

# ============= Logistic Regression ==================


#Logistic Regression
logisticreg = glm(status_risiko2  ~ kepadatan_penduduk+jumlah_faskes+jumlah_miskin
                  +persentase_keluhan_kesehatan,
                  data=covidjatim, family=binomial(link="logit"))
summary(logisticreg)

AIC(logisticreg)

### -----Uji Asumsi-----
err.regklasik2<- residuals(logisticreg)

# -----Normalitas-----
ad.test(err.regklasik2)
hist(err.regklasik2)
qqnorm(err.regklasik2,datax=T)
qqline(rnorm(length(err.regklasik2),mean(err.regklasik2),sd(err.regklasik2)),datax=T, col="red")

# -----Autokorelasi-----
dwtest(logisticreg)

# -----Heterogenitas-----
bptest(logisticreg)

# -----Multikolinieritas-----
vif(logisticreg)

#install and load pscl package
library(pscl)

#calculate McFadden's R-squared for model
pR2(logisticreg)['McFadden']



### =========== GLWR ==============
# Dependensi Spasial
library(sp)
data.sp.GWR=covidjatim
coordinates(data.sp.GWR) <-10:11 #kolom menyatakan letak Long-Lat
class(data.sp.GWR)  

library(GWmodel)
DM = gw.dist(dp.locat = coordinates(data.sp.GWR))

#Fixed Gaussian
bw_fixgas <- bw.ggwr(status_risiko2 ~ kepadatan_penduduk+jumlah_faskes+jumlah_miskin
                     +persentase_keluhan_kesehatan,
                     data=data.sp.GWR, dMat=DM,approach="AICc",
                     kernel="gaussian",adaptive=FALSE,family = "binomial")


res.fixgas <-ggwr.basic(status_risiko2 ~ kepadatan_penduduk+jumlah_faskes+jumlah_miskin
                        +persentase_keluhan_kesehatan, 
                        bw=bw_fixgas,data=data.sp.GWR, dMat=DM,
                        kernel="gaussian",adaptive=FALSE, family = "binomial")
res.fixgas

##==== FIXED BISQUARE
DM = gw.dist(dp.locat = coordinates(data.sp.GWR))

bw.gwr.ab <- bw.ggwr(status_risiko2 ~ kepadatan_penduduk+jumlah_faskes+jumlah_miskin
                     +persentase_keluhan_kesehatan,
                     data=data.sp.GWR, dMat=DM,approach="AICc",
                     kernel="bisquare",adaptive=TRUE,family = "binomial")

bgwr.res.ab <- ggwr.basic(status_risiko2 ~ kepadatan_penduduk+jumlah_faskes+jumlah_miskin
                          +persentase_keluhan_kesehatan, 
                          bw=bw_fixgas,data=data.sp.GWR, dMat=DM,
                          kernel="bisquare",adaptive=TRUE, family = "binomial")

##==== ADAPTIVE TRICUBE
DM = gw.dist(dp.locat = coordinates(data.sp.GWR)) #bandiwth optimum
bw.gwr.at = bw.ggwr(status_risiko2 ~ kepadatan_penduduk+jumlah_faskes+jumlah_miskin
                    +persentase_keluhan_kesehatan,
                    data = data.sp.GWR,
                    family = "binomial",
                    approach = "AICc",
                    kernel = "tricube",
                    adaptive = TRUE,
                    dMat = DM)

bgwr.res.at = ggwr.basic(status_risiko2 ~ kepadatan_penduduk+jumlah_faskes+jumlah_miskin
                         +persentase_keluhan_kesehatan,
                         data = data.sp.GWR,
                         family = "binomial",
                         bw = bw.gwr.at, 
                         kernel = "tricube", 
                         adaptive = TRUE,
                         dMat = DM)

##==== ADAPTIVE BISQUARE
DM = gw.dist(dp.locat = coordinates(data.sp.GWR))
bw.gwr.ab <- bw.ggwr(status_risiko2 ~ kepadatan_penduduk+jumlah_faskes+jumlah_miskin
                    +persentase_keluhan_kesehatan,
                    data = data.sp.GWR,
                    family = "binomial",
                    approach = "AICc",
                    kernel = "bisquare",
                    adaptive = TRUE,
                    dMat = DM)


bgwr.res.ab <- ggwr.basic(status_risiko2 ~ kepadatan_penduduk+jumlah_faskes+jumlah_miskin
                         +persentase_keluhan_kesehatan,
                         data = data.sp.GWR,
                         family = "binomial",
                         bw = bw.gwr.ab, 
                         kernel = "bisquare", 
                         adaptive = TRUE,
                         dMat = DM)
bgwr.res.ab



### Evaluasi Model

#model = c("LogisticReg","GWLR")
#R2 = c(0.1561,0.1303773)
#AIC = c(AIC(model1),-51.63156)

#evaluasi = data.frame(model,R2,AIC)
#evaluasi



