library(data.table)
library(haven)
library(dplyr)
library(boot)
library(tableone)
library(sandwich)
library(lmtest)

#load data
dem<-fread("I:/2020-09-30/Demographics/dem_ctos_inv.dat")
outcome<-fread("I:/2020-09-30/Outcomes/outc_ct_os_inv.dat")
psy<-fread("I:/2020-09-30/Psychosocial/f34_ctos_inv.dat")
MedHis<-fread("I:/2020-09-30/Med History/f30_ctos_inv.dat")
meno<-fread("I:/2020-09-30/Med History/f31_ctos_inv.dat")
horm<-fread("I:/2020-09-30/Medications/f43_ctos_inv.dat")
diab<-fread("I:/2020-09-30/Demographics/f2_ctos_inv.dat")
measure<-fread("I:/2020-09-30/Measurements/f80_ctos_inv.dat")
measure_sig<-measure %>% group_by(ID) %>% filter(row_number()==1)


#remove patients with prior cancaer
agreement<-read_sas('U:/crctable_vde.sas7bdat')
agreement_clean<-subset(agreement,agreement$crc_na !='X')

#matching baseline characteristics with target population
dem_match<-dem[dem$ID %in% agreement_clean$id]
out_match<-outcome[outcome$ID %in% agreement_clean$id]
psy_match<-psy[psy$ID %in% agreement_clean$id]
MedHis_match<-MedHis[MedHis$ID %in% agreement_clean$id]
meno_match<-meno[meno$ID %in% agreement_clean$id]


#measure_match<-measure[measure$ID %in% agreement_clean$id]


#merge on id
agreement_clean$ID<-agreement_clean$id



#only 107743 patients have complete baseline charactersitcs

dat<-Reduce(function(x,y) merge(x=x,y=y,by='ID',all.x=TRUE), list(agreement_clean,dem,outcome,psy,MedHis,meno,horm,measure_sig,diab))


#seperate patients that were in hormone trials
ht<-dem_match[dem_match$HRTFLAG==1]
ht_merge<-merge(agreement_clean,ht,by='ID')

#observational study and dietary modifications
os<-dem_match[dem_match$OSFLAG==1]
dm<-dem_match[dem_match$DMFLAG==1]

#age
median(dat$AGE)
age_0<-dat[dat$AGE<=64,]
age_1<-dat[dat$AGE>64,]

cal_kappa<-function(data){
  a<-sum(data$crc_yy=='X',na.rm = TRUE)
  b<-sum(data$crc_yn=='X',na.rm = TRUE)
  c<-sum(data$crc_ny=='X',na.rm = TRUE)
  d<-sum(data$crc_nn=='X',na.rm = TRUE)
  x.tab<-as.table(rbind(c(a,b),c(c,d)))
  diagnal.count<-diag(x.tab)
  N<-sum(x.tab)
  row.p<-rowSums(x.tab)/N
  col.p<-colSums(x.tab)/N
  po<-sum(diagnal.count)/N
  pe<-sum(row.p*col.p)
  k<-(po-pe)/(1-pe)
  return(k)
}



k_age_0<-cal_kappa(age_0)
k_age_1<-cal_kappa(age_1)

#meno years, median=15
dat$menoyear<-dat$AGE-dat$MENO
dat$menoyear[dat$menoyear<0]<-NA
dat$menoind<-if_else(dat$menoyear<=15,0,1,missing = 3)
table(dat$menoind)
meno_0<-dat[dat$menoind==0,]
meno_1<-dat[dat$menoind==1,]

k_meno_0<-cal_kappa(meno_0)
k_meno_1<-cal_kappa(meno_1)


#hormone therapy
dat$hormind<-if_else(dat$TOTH==1,1,0,missing = 3)
horm_1<-dat[dat$hormind==1,]
horm_0<-dat[dat$hormind==0,]
k_horm_1<-cal_kappa(horm_1)
k_horm_0<-cal_kappa(horm_0)

#diabetes
dat$diabind<-if_else(dat$DIABTRT==1,1,0,missing = 3)
diab_1<-dat[dat$diabind==1,]
diab_0<-dat[dat$diabind==0,]
k_diab_0<-cal_kappa(diab_0)
k_diab_1<-cal_kappa(diab_1)

#hypertension
dat$hypind<-if_else(dat$HYPT==1,1,0,missing = 3)
hyper_1<-dat[dat$hypind==1,]
hyper_0<-dat[dat$hypind==0,]
k_hyper_0<-cal_kappa(hyper_0)
k_hyper_1<-cal_kappa(hyper_1)


#hypercholesterol
dat$hchol<-if_else(dat$HICHOLRP==1,1,0,missing = 3)
hc_1<-dat[dat$hchol==1,]
hc_0<-dat[dat$hchol==0,]
k_hc_0<-cal_kappa(hc_0)
k_hc_1<-cal_kappa(hc_1)

#smoking
dat$smokind<-if_else(dat$SMOKEVR==1,1,0,missing = 3)
smoke_1<-dat[dat$smokind==1,]
smoke_0<-dat[dat$smokind==0,]

k_smok_0<-cal_kappa(smoke_0)
k_smok_1<-cal_kappa(smoke_1)

#alcohol
dat$alcind<-if_else(dat$ALC12DR==1,1,0,missing = 3)
alc_1<-dat[dat$alcind==1,]
alc_0<-dat[dat$alcind==0,]
k_alc_0<-cal_kappa(alc_0)
k_alc_1<-cal_kappa(alc_1)

#BMI
dat$bmiind<-if_else(dat$BMI<25,1,0,missing = 3)
bmi_1<-dat[dat$bmiind==1,]
bmi_0<-dat[dat$bmiind==0,]
#bmi_3<-dat[dat$BMI>=30,]
#bmi_normal<-bmi_1
#bmi_ow_ob<-rbind(bmi_2,bmi_3)
k_bmi_0<-cal_kappa(bmi_0)
k_bmi_1<-cal_kappa(bmi_1)
#cal_kappa(bmi_3)



cor_cancer<-outc_match[outc_match$COLORECTAL==1]
cor_cancer$COLORECTALDY

table(dem_match$CTFLAG)
table(dem_match$HRTFLAG)
table(dem_match$DMFLAG)
table(dem_match$OSFLAG)
table(dem_match$HRTARM)


table(outc_match$COLORECTAL)
head(outc_match$COLORECTALDY) 

dem_ht<-dem[dem$HRTFLAG==1]
h<-merge(dem_ht,agreement_clean,by='ID')

head(dem$AGER)

## the function to calculate CI using cohen's sd
cal_kappa_ci<-function(data){
  a<-sum(data$crc_yy=='X')
  b<-sum(data$crc_yn=='X')
  c<-sum(data$crc_ny=='X')
  d<-sum(data$crc_nn=='X')
  x.tab<-as.table(rbind(c(a,b),c(c,d)))
  diagnal.count<-diag(x.tab)
  N<-sum(x.tab)
  row.p<-rowSums(x.tab)/N
  col.p<-colSums(x.tab)/N
  po<-sum(diagnal.count)/N
  pe<-sum(row.p*col.p)
  sd<-sqrt(po*(1-po)/((1-pe)^2))
  k<-(po-pe)/(1-pe)
  n <- nrow(data)
  se <- sd/sqrt(n)
  ci <- k + c(-1,1)*1.96*se
  ci
}
# for age groups
cal_kappa_ci(age_young)
cal_kappa_ci(age_old)
# for menopause
cal_kappa_ci(meno_0)
cal_kappa_ci((meno_1))
# for prior hormone therapy
cal_kappa_ci(horm_yes)
cal_kappa_ci(horm_no)
# for diabetes ever
cal_kappa_ci(diab_no)
cal_kappa_ci(diab_yes)
# for hypertension
cal_kappa_ci(hyper_no)
cal_kappa_ci(hyper_yes)
# for hypercholesterolemia
cal_kappa_ci(hc_no)
cal_kappa_ci(hc_yes)
#smoking
cal_kappa_ci(smoke_no)
cal_kappa_ci(smoke_yes)
#alcohol
cal_kappa_ci(alc_no)
cal_kappa_ci(alc_yes)
# BMI
cal_kappa_ci(bmi_1)
cal_kappa_ci(bmi_2)
cal_kappa_ci(bmi_3)

#regression on crc_ny & crc_yy
sens<-dat[dat$crc_ny=='X'|dat$crc_yy=='X',]
sens$ind<-ifelse(sens$crc_yy=='X',0,1)

lr1<-glm(ind~AGE+MENO+TOTH+DIABTRT+HYPT+SMOKEVR+ALC12DR+BMI,data=sens,family = 'binomial')
summary(lr1)

summary(glm(ind~AGE,data=ppv,family = 'binomial'))

#regression on crc_yn & crc_nn
spec<-dat[dat$crc_yn=='X'|dat$crc_nn=='X',]
spec$ind<-ifelse(spec$crc_nn=='X',0,1)


lr2<-glm(ind~AGE+MENO+TOTH+DIABTRT+HYPT+SMOKEVR+ALC12DR+BMI,data=spec,family = 'binomial')
summary(lr2)

#crc_yy & crc_yn
ppv<-dat[dat$crc_yn=='X'|dat$crc_yy=='X',]
ppv$ind<-ifelse(ppv$crc_yy=='X',0,1)
lr3<-glm(ind~AGE+MENO+TOTH+DIABTRT+HYPT+SMOKEVR+ALC12DR+BMI,data=ppv,family = 'binomial')
summary(lr3)

#crc_nn & crc_ny
npv<-dat[dat$crc_ny=='X'|dat$crc_nn=='X',]
npv$ind<-ifelse(npv$crc_nn=='X',0,1)
lr4<-glm(ind~AGE+MENO+TOTH+DIABTRT+HYPT+SMOKEVR+ALC12DR+BMI,data=npv,family = 'binomial')
summary(lr4)



#uni_pval
lr<-function(data,feature){
  lr<-glm(ind~feature,data=data,family = poisson(link='log'))
  return(coeftest(lr,vcov=sandwich))
}


uni_pval<-function(data){
  age<-lr(data,data$AGE)
  meno<-lr(data,data$MENO)
  toth<-lr(data,data$TOTH)
  diab<-lr(data,data$DIABTRT)
  hypt<-lr(data,data$HYPT)
  smok<-lr(data,data$SMOKEVR)
  alc<-lr(data,data$ALC12DR)
  bmi<-lr(data,data$BMI)
 pvalres<-c(age[8],meno[8],toth[8],diab[8],hypt[8],smok[8],alc[8],bmi[8])
 est<-exp(c(age[5],meno[5],toth[5],diab[5],hypt[5],smok[5],alc[5],bmi[5]))
 res<-data.frame(est,pvalres)
 rownames(res)<-c('age','meno','toth','diab','hypt','smok','alc','bmi')
 return(res)
}

(sens_pval<-uni_pval(sens))
(spec_pval<-uni_pval(spec))
(ppv_pval<-uni_pval(ppv))
(npv_pval<-uni_pval(npv))


#bootstrapping
cal_kappa_boot<-function(data,idx){
  cal_kappa(data[idx,])
}

boot_age_0<-boot(age_0,statistic = cal_kappa_boot,R=1000)
boot_age_1<-boot(age_1,statistic = cal_kappa_boot,R=1000)
boot.ci(boot_young,type = 'norm')
boot.ci(boot_old,type = 'norm')

boot_meno_0<-boot(meno_0,statistic = cal_kappa_boot,R=1000)
boot_meno_1<-boot(meno_1,statistic = cal_kappa_boot,R=1000)
boot.ci(boot_meno_0,type = 'norm')
boot.ci(boot_meno_1,type = 'norm')

boot_horm_0<-boot(horm_0,statistic = cal_kappa_boot,R=1000)
boot_horm_1<-boot(horm_1,statistic = cal_kappa_boot,R=1000)
boot.ci(boot_horm_no,type = 'norm')
boot.ci(boot_horm_yes,type = 'norm')

boot_diab_0<-boot(diab_0,statistic = cal_kappa_boot,R=1000)
boot_diab_1<-boot(diab_1,statistic = cal_kappa_boot,R=1000)


boot_hyper_0<-boot(hyper_0,statistic = cal_kappa_boot,R=1000)
boot_hyper_1<-boot(hyper_1,statistic = cal_kappa_boot,R=1000)

boot_hc_0<-boot(hc_0,statistic = cal_kappa_boot,R=1000)
boot_hc_1<-boot(hc_1,statistic = cal_kappa_boot,R=1000)

boot_smoke_0<-boot(smoke_0,statistic = cal_kappa_boot,R=1000)
boot_smoke_1<-boot(smoke_1,statistic = cal_kappa_boot,R=1000)

boot_alc_0<-boot(alc_0,statistic = cal_kappa_boot,R=1000)
boot_alc_1<-boot(alc_1,statistic = cal_kappa_boot,R=1000)


boot_bmi_0<-boot(bmi_0,statistic = cal_kappa_boot,R=1000)
boot_bmi_1<-boot(bmi_1,statistic = cal_kappa_boot,R=1000)

#boot CI
quantile(boot_age_1$t-boot_age_0$t,c(0.025,0.975))
quantile(boot_meno_1$t-boot_meno_0$t,c(0.025,0.975))
quantile(boot_horm_1$t-boot_horm_0$t,c(0.025,0.975))
quantile(boot_diab_1$t-boot_diab_0$t,c(0.025,0.975))
quantile(boot_hyper_1$t-boot_hyper_0$t,c(0.025,0.975))
quantile(boot_hc_1$t-boot_hc_0$t,c(0.025,0.975))
quantile(boot_smoke_1$t-boot_smoke_0$t,c(0.025,0.975))
quantile(boot_alc_1$t-boot_alc_0$t,c(0.025,0.975))
quantile(boot_bmi_1$t-boot_bmi_0$t,c(0.025,0.975))

#FFS vs. non-FFS ht
dat2<-Reduce(function(x,y) merge(x=x,y=y,by='ID',all.x=TRUE), list(dem,outcome,psy,MedHis,meno,horm,measure_sig,diab)) #total 
dat3<-dat2[(dat2$AGE>=65)&(dat2$HRTFLAG==1),]
dat3$ffs<-ifelse(dat3$ID %in% dat$ID,1,0)
table(dat3$ffs) #ffs:9055/non-ffs: 3191


#categorize features
library(tableone)
dat3$E.alone<-ifelse(dat3$HRTARM==1|dat3$HRTARM==2,1,0)
dat3$EandP<-ifelse(dat3$HRTARM==3|dat3$HRTARM==4,1,0)
dat3$bmi.cat<-cut(dat3$BMI,breaks = c(0,25,Inf),include.lowest = TRUE,labels = c('<25','>=25'))
table1<-CreateTableOne(vars =c('AGE','MENO','TOTH','E.alone','EandP','DIABTRT','HYPT','SMOKEVR','ALC12DR','bmi.cat'),
                       strata = c('ffs'),data=dat3,factorVars = c('TOTH','E.alone','EandP','DIABTRT','HYPT','SMOKEVR','ALC12DR','bmi.cat') )

# ffs vs non-ffs all patients
dat4<-dat2
dat4$ffs<-ifelse(dat4$ID %in% dat$ID,1,0)
dat4$menoyear<-dat4$AGE-dat4$MENO
dat4$menoyear[dat4$menoyear<0]<-NA

table(dat4$ffs) #20513/50526
dat4$E.alone<-ifelse(dat4$HRTARM==1|dat4$HRTARM==2,1,0)
dat4$EandP<-ifelse(dat4$HRTARM==3|dat4$HRTARM==4,1,0)
dat4$bmi.cat<-ifelse(dat4$BMI>=25,1,0)

table1(~AGE+MENO+factor(TOTH)+factor(E.alone)+factor(EandP)+factor(SMOKEVR)+factor(ALC12DR)+factor(bmi.cat)|ffs,data=dat4)
CreateTableOne(vars =c('AGE','menoyear','TOTH','DIABTRT','HYPT','HICHOLRP','SMOKEVR','ALC12DR','bmi.cat'),
                       strata = c('ffs'),data=dat4,factorVars = c('TOTH','DIABTRT','HYPT','HICHOLRP','SMOKEVR','ALC12DR','bmi.cat') )
#forest plot
library(forestplot)
label<-c('Age<=64 ','Age>64','Year Since Menopause<=15',
         'Year Since Menopause>15','No Prior Hormone Therapy',
         'Hormone Therapy','No Diabetes','Diabetes','No Hypertension','Hypertension',
         'No Hypercholeterolemia','Hypercholeterolemia',
         'Never Smoked','Ever Smoked','Never Drink Alcohol',
         'Drink Alcohol','Normal BMI','Overweight BMI')
mean<-c(0.7301,0.7967,0.7483,0.7878,0.7882,0.7611,0.7759,0.7723,0.7592,0.8007,0.7727,0.7845,0.7806,0.7693,0.8106,0.7715,0.7481,0.7880)
lower<-c(0.6970,0.7768,0.7170,0.7661,0.7654,0.7351,0.7581,0.7037,0.7364,0.7743,0.7531,0.7418,0.7560,0.7450,0.7639,0.7530,0.7157,0.7677)
upper<-c(0.7633,0.8166,0.7795,0.8095,0.8111,0.7871,0.7936,0.8409,0.7821,0.8270,0.7923,0.8273,0.8053,0.7936,0.8573,0.7900,0.7806,0.8083)

df<-data.frame(label,mean,lower,upper)
df$label<-factor(df$label,levels = rev(df$label))
library(ggplot2)
fp<-ggplot(data=df,aes(x=label,y=mean,ymin=lower,ymax=upper))+geom_pointrange()+coord_flip()+xlab('Baseline Characteristics')+ylab('Kappa(CI)')+theme_bw()
print(fp)


