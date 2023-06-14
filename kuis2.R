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
     covidjatim$fatality_rate, xlab="Kepadatan Penduduk", 
     ylab="Fatality Rate COVID-19", pch=20, col="orange", cex=2)
plot(covidjatim$jumlah_faskes,
     covidjatim$fatality_rate, xlab="Jumlah Faskes", 
     ylab="Fatality Rate COVID-19", pch=20, col="orange", cex=2)

plot(covidjatim$jumlah_miskin,
     covidjatim$fatality_rate, xlab="Jumlah Penduduk Miskin (ribu)", 
     ylab="Fatality Rate COVID-19", pch=20, col="orange", cex=2)
plot(covidjatim$persentase_keluhan_kesehatan,
     covidjatim$fatality_rate, xlab="Persentase Keluhan Kesehatan Masyarakat", 
     ylab="Fatality Rate COVID-19", pch=20, col="orange", cex=2)

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
mapview(df_spasial[,c("KabupatenKota","fatality_rate")], zcol = "KabupatenKota", cex="fatality_rate", 
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
## y = Fatality rate
## X1 = Kepadatan Penduduk
## X2 = Jumlah Fasilitas Kesehatan
## X3 = Jumlah Masyarakat Miskin (ribu)
## X4 = Persentase keluhan kesehatan masyarakat
model1 <- lm(fatality_rate  ~ kepadatan_penduduk+jumlah_faskes+jumlah_miskin
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
moran.test(df_spasial_sp$fatality_rate, bobot, alternative="greater")

### Perhitungan bandwith menggunakan fungsi pembobot kernel Gaussian

b.gwr <- gwr.sel(fatality_rate~kepadatan_penduduk+jumlah_faskes+jumlah_miskin
                 +persentase_keluhan_kesehatan, data=covidjatim, coords = cbind(covidjatim$Lon,covidjatim$Lat),
                 gweight = gwr.Gauss)
### Run GWR Model
gwr.model <- gwr(fatality_rate~kepadatan_penduduk+jumlah_faskes+jumlah_miskin
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

df_gwlr = as.data.frame(res_adpbi$SDF)
rownames(df_gwlr) = covidjatim$'KabupatenKota'

#intercept
intercept = gwr.model$SDF$`(Intercept)`
intercept

x1 = gwr.model$SDF$kepadatan_penduduk
x2 = gwr.model$SDF$jumlah_faskes
x3 = gwr.model$SDF$jumlah_miskin
x4 = gwr.model$SDF$persentase_keluhan_kesehatan


##=== MENDAPATKAN NILAI PVALUE
#TV --> T-Value

pvalx1 = 2*pt(abs(gwr.model$SDF$kepadatan_penduduk_se_EDF),df=38,lower.tail = FALSE) #ubah t-value menjadi p-value
pvalx1
pvalx2 = 2*pt(abs(gwr.model$SDF$jumlah_faskes_se_EDF),df=38,lower.tail = FALSE)
pvalx2
pvalx3 = 2*pt(abs(gwr.model$SDF$jumlah_miskin_se_EDF),df=38,lower.tail = FALSE)
pvalx3
pvalx4 = 2*pt(abs(gwr.model$SDF$persentase_keluhan_kesehatan_se_EDF),df=38,lower.tail = FALSE)
pvalx4

library(writexl)
outputGWLR = data.frame(covidjatim$KabupatenKota,intercept,x1,x2,x3,x4,pvalx1,pvalx2,pvalx3,pvalx4)
writexl::write_xlsx(outputGWLR,"outputGWR.xlsx")


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

# Output GWNBR
Weight.gwr = data.frame(W)
Coeff.GWNBR = data.frame(model$koefisien)
Zstat.GWNBR = data.frame(model$Z_hitung)
Zprob.GWNBR = data.frame(model$Z_prob)
writexl::write_xlsx(Weight.gwr,"weightGWNBR.xlsx")
writexl::write_xlsx(Coeff.GWNBR,"CoeffGWNBR.xlsx")
writexl::write_xlsx(Zstat.GWNBR,"ZstatGWNBR.xlsx")
writexl::write_xlsx(Zprob.GWNBR,"ZprobGWNBR.xlsx")
# ----GWLR----
## ---- Fit Logistic Regression ----
## Dataset:
## y = Status Risiko (Risiko Rendah (0)/Risiko Sedang(1))
## X1 = Kepadatan Penduduk
## X2 = Jumlah Fasilitas Kesehatan
## X3 = Jumlah Masyarakat Miskin (ribu)
## X4 = Persentase keluhan kesehatan masyarakat


#preprocessing
sum(is.na(covidjatim))
str(covidjatim)
covidjatim <- covidjatim[complete.cases(covidjatim), ]

# ============= Logistic Regression ==================


#Logistic Regression
logisticreg = glm(status_risiko  ~ kepadatan_penduduk+jumlah_faskes+jumlah_miskin
                  +persentase_keluhan_kesehatan,
                  data=covidjatim, family=binomial(link="logit"))
summary(logisticreg)

AIC(logisticreg)

### -----Uji Asumsi-----
err.regklasik2<- residuals(logisticreg)

# -----Normalitas-----
shapiro.test(err.regklasik2) #pake uji ini karena data <40

hist(err.regklasik2)
qqnorm(err.regklasik2,datax=T)
qqline(rnorm(length(err.regklasik2),mean(err.regklasik2),sd(err.regklasik2)),datax=T, col="red")

# -----Autokorelasi-----
dwtest(logisticreg)

# -----Heterogenitas-----
bptest(logisticreg)

# -----Multikolinieritas-----
vif(logisticreg)


#calculate McFadden's R-squared for model
pR2(logisticreg)['McFadden']



### =========== GLWR ==============

# Buat distance matrix berdasarkan latitude dan longitude
library(sp)
data.sp.GWR=covidjatim
coordinates(data.sp.GWR) <-10:11 #kolom menyatakan letak Long-Lat

DM = gw.dist(dp.locat = coordinates(data.sp.GWR))

#---- FIXED Gaussian
bw_fixgas <- bw.ggwr(logisticreg$formula,
                     data=data.sp.GWR,
                     approach="AICc",
                     kernel="gaussian",
                     adaptive=FALSE,
                     family = "binomial",
                     dMat=DM)


res_fixgas <-ggwr.basic(logisticreg$formula,
                        data=data.sp.GWR,
                        bw=bw_fixgas,
                        kernel="gaussian",
                        adaptive=FALSE,
                        family = "binomial",
                        dMat=DM)
res_fixgas

#---- FIXED Gaussian
bw_fixbi <- bw.ggwr(logisticreg$formula,
                    data=data.sp.GWR,
                    approach="AICc",
                    kernel="bisquare",
                    adaptive=FALSE,
                    family = "binomial",
                    dMat=DM)


res_fixbi <-ggwr.basic(logisticreg$formula,
                       data=data.sp.GWR,
                       bw=bw_fixbi,
                       kernel="bisquare",
                       adaptive=FALSE,
                       family = "binomial",
                       dMat=DM)
res_fixbi



#Adaptive Gaussian tidak dapat digunakan karena
#karena parameter adaptive = TRUE hanya dapat digunakan dengan kernel yang memiliki bobot non-negatif.

#Namun, ketika menggunakan kernel Gaussian dengan `adaptive = FALSE`, 
#tidak ada masalah karena dalam kasus tersebut, tidak ada pendekatan adaptif yang digunakan. 
#Bandwidth lokal tetap konstan untuk setiap observasi, 
#dan karena tidak ada perhitungan bobot adaptif yang melibatkan nilai negatif, error tidak muncul.



#----- ADAPTIVE BISQUARE
bw_adpbi <- bw.ggwr(logisticreg$formula,
                    data=data.sp.GWR,
                    approach="AICc",
                    kernel="bisquare",
                    adaptive=TRUE,
                    family = "binomial",
                    dMat=DM)

res_adpbi <- ggwr.basic(logisticreg$formula,
                        data=data.sp.GWR,
                        bw=bw_adpbi,
                        kernel="bisquare",
                        adaptive=TRUE,
                        family = "binomial",
                        dMat=DM)

res_adpbi


### Evaluasi Model

model = c("LogisticReg","GWLR-Fix Gauss", "GWLR-Fix Bisquare", "GWLR-Adpv Bisquare")
R2 = c(0.1406, 0.3023, 0.3060,0.3630)
AIC = c(AIC(logisticreg),50.6924,50.4305,49.7580)

evaluasi = data.frame(model,R2,AIC)
evaluasi

## Dari evaluasi didapat model terbaik yaitu gwlr adaptive bisquare


#================ UJI SIGNIFIKANSI PARAMETER TIAP MODEL
##=== ESTIMASI PARAMETER
df_gwlr = as.data.frame(res_adpbi$SDF)
rownames(df_gwlr) = covidjatim$'KabupatenKota'

#intercept
intercept = res_adpbi$SDF$Intercept
intercept

x1 = res_adpbi$SDF$kepadatan_penduduk
x2 = res_adpbi$SDF$jumlah_faskes
x3 = res_adpbi$SDF$jumlah_miskin
x4 = res_adpbi$SDF$persentase_keluhan_kesehatan


##=== MENDAPATKAN NILAI PVALUE
#TV --> T-Value

pvalx1 = 2*pt(abs(res_adpbi$SDF$kepadatan_penduduk_TV),df=38,lower.tail = FALSE) #ubah t-value menjadi p-value
pvalx1
pvalx2 = 2*pt(abs(res_adpbi$SDF$jumlah_faskes_TV),df=38,lower.tail = FALSE)
pvalx2
pvalx3 = 2*pt(abs(res_adpbi$SDF$jumlah_miskin_TV),df=38,lower.tail = FALSE)
pvalx3
pvalx4 = 2*pt(abs(res_adpbi$SDF$persentase_keluhan_kesehatan_TV),df=38,lower.tail = FALSE)
pvalx4

##--- EXPORT HASIL DALAM FORMAT XLSX
setwd("D:/All about Collage/Perkuliahan/Semester 6/Analisis Data Spasial/Tugas + Kuis")
library(writexl)
outputGWLR = data.frame(covidjatim$KabupatenKota,intercept,x1,x2,x3,x4,pvalx1,pvalx2,pvalx3,pvalx4)
writexl::write_xlsx(outputGWLR,"outputGWLR.xlsx")

