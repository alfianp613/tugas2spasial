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
library(MASS)
library(dplyr)
library(raster)

setwd("D:/Kuliah/Tugas, PPT, Buku Kuliah/Semester 6/Analisis Data Spasial/Tugas + Kuis")
library(openxlsx)
covidjatim = read.xlsx("Spasial Covid 1.xlsx",sheet="Sheet1")
# covidjatim <- read.csv("Spasial Covid 1 (test).txt",sep=';')
df <- as_tibble(covidjatim)
df_spasial <- st_as_sf(df, coords = c("Lon", "Lat"), crs = 4326)
df_spasial_sp <- as(df_spasial, "Spatial")
# EDA
# Correlation Plot
plot(covidjatim$kepadatan_penduduk,
     covidjatim$skor_risiko, xlab="Kepadatan Penduduk", 
     ylab="Skor Risiko COVID-19", pch=20, col="orange", cex=2)
plot(covidjatim$jumlah_faskes,
     covidjatim$skor_risiko, xlab="Jumlah Faskes", 
     ylab="Skor Risiko COVID-19", pch=20, col="orange", cex=2)

plot(covidjatim$jumlah_miskin,
     covidjatim$skor_risiko, xlab="Jumlah Penduduk Miskin (ribu)", 
     ylab="Skor Risiko COVID-19", pch=20, col="orange", cex=2)
plot(covidjatim$persentase_keluhan_kesehatan,
     covidjatim$skor_risiko, xlab="Persentase Keluhan Kesehatan Masyarakat", 
     ylab="Skor Risiko COVID-19", pch=20, col="orange", cex=2)

plot(covidjatim$kepadatan_penduduk,
     covidjatim$jumlah_kasus_covid, xlab="Kepadatan Penduduk", 
     ylab="Jumlah Kasus COVID-19", pch=20, col="orange", cex=2)
plot(covidjatim$jumlah_faskes,
     covidjatim$jumlah_kasus_covid, xlab="Jumlah Faskes", 
     ylab="Jumlah Kasus COVID-19", pch=20, col="orange", cex=2)

plot(covidjatim$jumlah_miskin,
     covidjatim$jumlah_kasus_covid, xlab="Jumlah Penduduk Miskin (ribu)", 
     ylab="Jumlah Kasus COVID-19", pch=20, col="orange", cex=2)
plot(covidjatim$persentase_keluhan_kesehatan,
     covidjatim$jumlah_kasus_covid, xlab="Persentase Keluhan Kesehatan Masyarakat", 
     ylab="Jumlah Kasus COVID-19", pch=20, col="orange", cex=2)

# Peta Tematik
mapview(df_spasial[,c("KabupatenKota","jumlah_kasus_covid")], zcol = "KabupatenKota", cex="jumlah_kasus_covid", 
        layer.name="KabupatenKota", alpha.regions = 0.6)
mapview(df_spasial[,c("KabupatenKota","skor_risiko")], zcol = "KabupatenKota", cex="skor_risiko", 
        layer.name="KabupatenKota", alpha.regions = 0.6)
mapview(df_spasial[,c("KabupatenKota","kepadatan_penduduk")], zcol = "KabupatenKota", cex="kepadatan_penduduk", 
        layer.name="KabupatenKota", alpha.regions = 0.6)
mapview(df_spasial[,c("KabupatenKota","jumlah_faskes")], zcol = "KabupatenKota", cex="jumlah_faskes", 
        layer.name="KabupatenKota", alpha.regions = 0.6)
mapview(df_spasial[,c("KabupatenKota","jumlah_miskin")], zcol = "KabupatenKota", cex="jumlah_miskin", 
        layer.name="KabupatenKota", alpha.regions = 0.6)
mapview(df_spasial[,c("KabupatenKota","persentase_keluhan_kesehatan")], zcol = "KabupatenKota", cex="persentase_keluhan_kesehatan", 
        layer.name="KabupatenKota", alpha.regions = 0.6)


# -----GWR-----
## -----Regresi linier klasik-----
## Dataset:
## y = skor risiko
## X1 = Kepadatan Penduduk
## X2 = Jumlah Fasilitas Kesehatan
## X3 = Jumlah Masyarakat Miskin (ribu)
## X4 = Persentase keluhan kesehatan masyarakat
model1 <- lm(skor_risiko  ~ kepadatan_penduduk+jumlah_faskes+jumlah_miskin
             +persentase_keluhan_kesehatan, data=covidjatim)
summary(model1)
err.regklasik<- residuals(model1)
AIC(model1)
### -----Uji Asumsi-----
#### -----Normalitas-----
ad.test(err.regklasik)
hist(err.regklasik)
qqnorm(err.regklasik,datax=T)
qqline(rnorm(length(err.regklasik),mean(err.regklasik),sd(err.regklasik)),datax=T, col="red")
#### -----Heteroscedastics-----
bptest(model1,studentize = F)
#### -----Multikolinieritas-----
vif(model1)

## -----GWR Model-----
### Dependensi Spasial
coords <- coordinates(df_spasial_sp)
bobot <- nb2listw(knn2nb(knearneigh(coords)))
moran.test(df_spasial_sp$skor_risiko, bobot, alternative="greater")

### Perhitungan bandwith menggunakan fungsi pembobot kernel Gaussian
# band = bw.gwr(skor_risiko~kepadatan_penduduk+jumlah_faskes+jumlah_miskin
#               +persentase_keluhan_kesehatan, data=df_spasial_sp, approach="CV", kernel="gaussian",
#               adaptive=TRUE)
# gwr_model<-gwr.basic(skor_risiko~kepadatan_penduduk+jumlah_faskes+jumlah_miskin
#                      +persentase_keluhan_kesehatan, data=df_spasial_sp, bw=band,kernel = "gaussian")


b.gwr <- gwr.sel(skor_risiko~kepadatan_penduduk+jumlah_faskes+jumlah_miskin
                 +persentase_keluhan_kesehatan, data=covidjatim, coords = cbind(covidjatim$Lon,covidjatim$Lat),
                 gweight = gwr.Gauss)
### Run GWR Model
gwr.model <- gwr(skor_risiko~kepadatan_penduduk+jumlah_faskes+jumlah_miskin
                 +persentase_keluhan_kesehatan, data=covidjatim, coords = cbind(covidjatim$Lon,covidjatim$Lat),
                 gweight = gwr.Gauss,bandwidth = b.gwr,hatmatrix=TRUE) 
### Hasil Analisis
gwr.model
### Uji ASumsi
err.rtg<-gwr.model$SDF$gwr.e
#### Normalitas
ad.test(err.rtg) 
hist(err.rtg)
qqnorm(err.rtg,datax=T)
qqline(rnorm(length(err.rtg),mean(err.rtg),sd(err.rtg)),datax=T, col="red")
# Homogenitas
bptest(gwr.model$lm, weights = gwr.model$gweight) 
# Autokorelasi Spasial
gwr.morantest(gwr.model, bobot, zero.policy = TRUE)
### Evaluasi Model

BFC99.gwr.test(gwr.model)
anova(gwr.model)

data.frame("MODEL" = c("GWR","Regresi Klasik"),
           "AIC" = c(gwr.model[["results"]][["AICh"]],AIC(model1)))%>% arrange(AIC)
# ----GWPR----
## ----Fit GLM Poisson----
## Dataset:
## y = Jumlah Kasus COVID-19
## X1 = Kepadatan Penduduk
## X2 = Jumlah Fasilitas Kesehatan
## X3 = Jumlah Masyarakat Miskin (ribu)
## X4 = Persentase keluhan kesehatan masyarakat
pois = glm(jumlah_kasus_covid  ~ kepadatan_penduduk+jumlah_faskes+jumlah_miskin
           +persentase_keluhan_kesehatan, data=covidjatim, family = poisson)
summary(pois)
## ----Overdispersion test----
dispersiontest(pois, trafo = NULL, alternative = c("greater", "two.sided", "less"))
### Terdapat overdispersion maka dilakukan pemodelan GWNBR
## ----GWNBR----
Data.spdf=SpatialPointsDataFrame(cbind(covidjatim$Lon,covidjatim$Lat),covidjatim)
DM=gw.dist(dp.locat=coordinates(Data.spdf))
nc = ncol(Data.spdf)
nr = nrow(Data.spdf)
mydata.sp = Data.spdf[,-c(nc-1,nc)]
mydata.nb = covidjatim[,-c(nc-1,nc)]

### ----Negative Binomial Regression----
nb.model =  glm.nb(jumlah_kasus_covid  ~ kepadatan_penduduk+jumlah_faskes+jumlah_miskin
                   +persentase_keluhan_kesehatan,data=mydata.nb)
### Hyperparameter
kernel = "bisquare"
adaptive = TRUE
bdwt=bw.gwr(jumlah_kasus_covid  ~ kepadatan_penduduk+jumlah_faskes+jumlah_miskin
            +persentase_keluhan_kesehatan,data=mydata.sp,kernel=kernel,adaptive=adaptive,dMat=DM)
W = gw.weight(DM,bdwt,kernel,adaptive = adaptive)
### ----GWNBR----
  # Program Estimasi GWNBR lokasi ke-i
gwnbr.i=function(X,y,Wi,phi1,b1)
{
  nobs = nrow(X)
  nvar = ncol(X)
  npar = nvar+1
  beta=matrix(c(0),npar,npar,byrow=T)
  beta[1,1]=phi1
  beta[1,2:npar]=c(b1)
  
  
  for(i in 1:npar){
    satu=rep(1,nobs)
    satu=as.matrix(satu)
    b01=rbind(c(phi1,beta[i,2:npar]))
    Xb1=as.matrix(X)%*%as.matrix(beta[i,2:npar])
    mu1=exp(Xb1)
    delta11=((log(1+phi1*mu1)-digamma(y+(1/phi1))+digamma(1/phi1))/phi1^2)+
      ((y-mu1)/((1+phi1*mu1)*phi1))
    delta11=as.matrix(delta11)
    p11=t(satu)%*%Wi%*%delta11
    delta21=(y-mu1)/(1+phi1*mu1)
    delta21=as.matrix(delta21)
    p21=t(X)%*%as.matrix(Wi)%*%delta21
    p21=as.matrix(p21)
    gt1=rbind(p11,p21)
    delta31=((trigamma(y+(1/phi1))-trigamma(1/phi1))/phi1^4)+
      ((2*digamma(y+(1/phi1))-2*digamma(1/phi1)-2*log(1+phi1*mu1))/phi1^3)+
      ((2*mu1)/(phi1^2*(1+phi1*mu1)))+(((y+(1/phi1))*mu1^2)/(1+phi1*mu1)^2)-(y/phi1^2)
    delta31=as.matrix(delta31)
    p31=t(satu)%*%Wi%*%delta31
    p31=as.matrix(p31)
    delta41=mu1*(mu1-y)/(1+phi1*mu1)^2
    delta41=as.matrix(delta41)
    p41=t(X)%*%Wi%*%delta41
    p41=as.matrix(p41)
    h11=rbind(p31,p41)
    delta51=mu1*(phi1*y+1)/(1+phi1*mu1)^2
    delta51=t(delta51)
    delta51=c(delta51)
    delta51=as.matrix(diag(delta51))
    p51=t(X)%*%as.matrix(Wi)%*%delta51%*%as.matrix(X)
    p51=-1*p51
    p51=as.matrix(p51)
    h21=rbind(t(p41),p51)
    H1=cbind(h11,h21)
    H11=ginv(H1)
    beta[i,]=(t(b01)-H11%*%gt1)
  }
  return(list(beta=beta,hessian=H1))
}
  # Fungsi GWNBR
gwnbr=function(X,y,W,teta,beta)
{
  nobs = nrow(X)
  nvar = ncol(X)
  npar = nvar+1
  param=matrix(c(0),nrow(X),npar,byrow=T)
  zhit=matrix(c(0),nrow(X),ncol(X),byrow=T)
  zprob=matrix(c(0),nrow(X),ncol(X),byrow=T)
  for(i in 1:nobs){
    wi=as.matrix(diag(W[i,]))
    hit=gwnbr.i(X,y,wi,teta,beta)	
    parameter=hit$beta	
    param[i,]=hit$beta[npar,]
    invh=-ginv(as.matrix(hit$hessian))
    for(j in 1:ncol(X)){
      zhit[i,j]=param[i,j+1]/invh[j+1,j+1]
      zprob[i,j]=2*(1-pnorm(abs(zhit[i,j])))
    }
  }
  return(list(koefisien=param,Z_hitung=zhit,Z_prob=zprob,parameter=parameter))
}
  # GWNBR
satu=rep(1,nr)
satu=as.matrix(satu)
X = cbind(satu,mydata.nb[,4:7])
npar = ncol(X)
y = mydata.nb$jumlah_kasus_covid
phx1=nb.model$theta
b1 =as.matrix(nb.model$coefficients)
model=gwnbr(X,y,W,phx1,b1)

  # Calculate Deviance
muw=as.matrix(rep(exp(b1[1]),nr))
tetanb = phx1
slr=matrix(0,nr,1)
for(i in 1:nr){
  slr[i]=0
  for(r in 1:y[i]){
    slr[i]=slr[i]+log(r+(1/tetanb))
  }
}
X = as.matrix(X)
Lw=sum(slr-lgamma(y+1)+y*log(tetanb*muw)-(y+(1/tetanb))*
         log(1+tetanb*muw))
muo=exp(X%*%b1)
Lo=sum(slr-lgamma(y+1)+y*log(tetanb*muo)-(y+(1/tetanb))*
         log(1+tetanb*muo))
DNBR=2*(Lo-Lw)


  # Deviance GWNBR
tetagw=as.matrix(model$koefisien[,1])
betagw=as.matrix(model$koefisien[,-1])
muwgw=as.matrix(exp(model$koefisien[,2]))
mM = X*betagw
muogw=as.matrix(exp(apply(mM,1,sum)))
Lwgw=sum(slr-lgamma(y+1)+y*log(tetagw*muwgw)-(y+(1/tetagw))*
           log(1+tetagw*muwgw))
Logw=sum(slr-lgamma(y+1)+y*log(tetagw*muogw)-(y+(1/tetagw))*
           log(1+tetagw*muogw))
DGWNBR=2*(Logw-Lwgw)

  # Fstat
Fstat=DNBR/DGWNBR
Fprob = 1-pf(Fstat,nr-npar,nr-npar)

  # Menghitung AIC GWNBR
ssegw=sum((y-muogw)^2)
aicgw=nr*log(ssegw/nr) +(2*npar)

## ---- Evaluasi Model ----
### Uji Serentak
dev = cbind(DNBR,DGWNBR,Fstat,Fprob)
colnames(dev)=c("NBR","GWNBR","Fstat","Sig.")
rownames(dev)=c("Deviance")
print(dev)
### AIC
data.frame("MODEL" = c("NBR","GWNBR"),
           "AIC" = c(AIC(nb.model),aicgw))%>% arrange(AIC)

# ----GWLR----
pasiencovid = read.xlsx("Spasial Covid 2.xlsx",sheet="fix")

## ---- Fit Logistic Regression ----
## Dataset:
## Y = Hasil perawatan (meninggal/sembuh)
## X1 = usia
## X2 = jumlah komorbid
## X3 = batuk (0/1)
## X4 = pilek (0/1)
## X5 = demam (0/1)
## X6 = thorax (0/1)
## X7 = ECG (0/1)
## X8 = oksigen (0/1)
## X9 = pneumonia (0/1)
## X10 = Hipertensi (0/1)
## X11 = Diabetes Meillitus (0/1)
logisticreg = glm(kode_hasil_perwatan  ~ usia +jumlah_kokmorbid +batuk 
           +pilek +demam+thorax+ECG +oksigen +pneumonia +Hipertensi +Diabetes.Meillitus
           , data=pasiencovid, family=binomial(link="logit"))
summary(logisticreg)
#### -----Multikolinieritas-----
vif(logisticreg)
## ----GWNBR----
df2 <- as_tibble(pasiencovid)
df_spasial2 <- st_as_sf(df2, coords = c("lon", "lat"), crs = 4326)
df_spasial_sp2 <- as(df_spasial2, "Spatial")

DM<-gw.dist(dp.locat=coordinates(df_spasial_sp2))

bw.ggwr(kode_hasil_perwatan  ~ usia +jumlah_kokmorbid +batuk 
        +pilek +demam+thorax+ECG +oksigen +pneumonia +Hipertensi +Diabetes.Meillitus, 
        data=df_spasial_sp2, family =binomial(link="logit"), approach="CV",
        kernel="bisquare",adaptive=TRUE,longlat=F,dMat=DM)
