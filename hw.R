library(lmtest)
library(stargazer)
library(sandwich)
library(plm)
library(clubSandwich)


#setwd('C:/Users/Cheng Qian/Dropbox/phd_year2_study/koedel_class/group_work/')  
#load("C:/Users/Cheng Qian/Dropbox/phd_year2_study/koedel_class/group_work/NELS_1988-00_v1_0_R_Datasets/NELS_88_00_BYF4STU_V1_0.rdata")
windows version



setwd('/Users/qncn007/Dropbox/phd_year2_study/koedel_class/group_work')  #mac version
load("/Users/qncn007/Dropbox/phd_year2_study/koedel_class/group_work/NELS_1988-00_v1_0_R_Datasets/NELS_88_00_BYF4STU_V1_0.rdata")


data<-BYF4STU

#data1<-subset(data,F4UNI2A==1)



data2<-subset(data,F4UNI2A==1,select=c('STU_ID','BYS31A','SEX','BYS34B',
                                       'BYS35H','BYS35J','BYS35L','BYFAMINC','BY2XCOMP',
                                       'BYGRADS'))
  #F4UNI2A==1 means only include base-year eligible records,
  #BYS31A is race,sex is gender, BYS34B is mother's edu, BYS35H is computer, BYS35J is dryer,BYS35L is microwave oven,
  #BYFAMINC is income,	BY2XCOMP is standardized test score.
#data_sex<-subset(data,F4UNI2A==1,select=c('STU_ID','SEX','F1SEX','F2SEX','F3SEX'))
#data_sex[,"test"]=data_sex$SEX-data_sex$F1SEX
#data_sex[,"test2"]=data_sex$SEX-data_sex$F2SEX
#data2$SEX=factor(data2$SEX)

#data3<-data.frame(data2$STU_ID)


data2[,'male']<-ifelse(data2$SEX==1,1,0)
#data2[,'male']<- as.numeric((data2$SEX==1))
data2[,'female']<-ifelse(data2$SEX==2,1,0)
data2[,'sex_missing']<-ifelse(data2$SEX==9,1,0)


n<-c(1,2,3,4,5)
j=1
for (i in c('asian','hispanic','black','white','indian')){
  data2[,i]<-ifelse(data2$BYS31A==n[j], 1,0)
  j=j+1
  }
data2[,'race_missing']<-ifelse(data2$BYS31A==8,1,0)
data2[,'race_unknown']<-ifelse(data2$BYS31A==6|data2$BYS31A==7|data2$BYS31A==9,1,0)



m<-c(1,2)
j=1
for (i in c('less_high','high')){
  data2[,i]<-ifelse(data2$BYS34B==n[j], 1,0)
  j=j+1
}

data2[,'somecol']<-ifelse(data2$BYS34B==3|data2$BYS34B==4,1,0)
data2[,'col4']<-ifelse(data2$BYS34B==5,1,0)

data2[,'grad']<-ifelse(data2$BYS34B==6 | data2$BYS34B==7 ,1,0)
data2[,'edu_unknown']<-ifelse(data2$BYS34B==8 | data2$BYS34B==97 | data2$BYS34B==99 ,1,0)
data2[,'edu_missing']<-ifelse(data2$BYS34B==98 ,1,0)


data2[,'com_yes']<-ifelse(data2$BYS35H==1 ,1,0)
data2[,'com_no']<-ifelse(data2$BYS35H==2 ,1,0)
data2[,'com_missing']<-ifelse(data2$BYS35H==6|data2$BYS35H==8|data2$BYS35H==9 ,1,0)

data2[,'dryer_yes']<-ifelse(data2$BYS35J==1 ,1,0)
data2[,'dryer_no']<-ifelse(data2$BYS35J==2 ,1,0)
data2[,'dryer_missing']<-ifelse(data2$BYS35J==6|data2$BYS35J==8|data2$BYS35J==9 ,1,0)

data2[,'mic_yes']<-ifelse(data2$BYS35L==1 ,1,0)
data2[,'mic_no']<-ifelse(data2$BYS35L==2 ,1,0)
data2[,'mic_missing']<-ifelse(data2$BYS35L==6|data2$BYS35L==8|data2$BYS35L==9 ,1,0)

data2[,'less10']<-ifelse(data2$BYFAMINC<=6,1,0)
data2[,'in1020']<-ifelse(data2$BYFAMINC==7|data2$BYFAMINC==8,1,0)
data2[,'in2035']<-ifelse(data2$BYFAMINC==9|data2$BYFAMINC==10,1,0)
data2[,'in3550']<-ifelse(data2$BYFAMINC==11,1,0)
data2[,'in5075']<-ifelse(data2$BYFAMINC==12,1,0)
data2[,'in75100']<-ifelse(data2$BYFAMINC==13,1,0)
data2[,'in100200']<-ifelse(data2$BYFAMINC==14,1,0)
data2[,'in200more']<-ifelse(data2$BYFAMINC==15,1,0)
data2[,'income_missing']<-ifelse(data2$BYFAMINC==98,1,0)
data2[,'income_unknown']<-ifelse(data2$BYFAMINC==99,1,0)  #all =0 in arter drop test=.

data2[,'bygrads_new']<-ifelse(data2$BYGRADS==9.8|data2$BYGRADS==9.9,NA,data2$BYGRADS)   #no longer need it
#data2[,'test_new']<-ifelse(data2$BY2XCOMP==99.98|data2$BY2XCOMP==99.99|data2$BY2XCOMP==-9,'.',data2$BY2XCOMP)

#data_pure<-subset(data2,data2$test_new!='.')

r1<-lm(data=data2,as.numeric(bygrads_new)~com_missing+com_no+mic_missing+mic_no+dryer_missing+dryer_no)
summary(r1)

r2<-lm(data=data2,as.numeric(bygrads_new)~com_missing+com_no+mic_missing+mic_no+dryer_missing+
         dryer_no+asian+hispanic+black+indian+race_missing+race_unknown+male)
summary(r2)

r3<-lm(data=data2,as.numeric(bygrads_new)~com_missing+com_no+mic_missing+mic_no+dryer_missing+
         dryer_no+in1020+in2035 +in3550+in5075+in75100+in100200+in200more+income_missing)
summary(r3)



#datatest<-data_pure[,11:47]

#r4<-lm(data=datatest,as.numeric(test_new)~.)

r4<-lm(data=data2,as.numeric(bygrads_new)~com_missing+com_no+mic_missing+mic_no+dryer_missing+
         dryer_no+asian+hispanic+black+indian+race_missing+race_unknown+male+
         in1020+in2035 +in3550+in5075+in75100+in100200+in200more+income_missing+high+somecol+col4+grad+edu_unknown+edu_missing,robust= TRUE)
robust4<-coeftest(r4,vcov. = vcovHC(r4,'HC1')) # robust SE in stata
summary(r4)


r5<-lm(data=data2,as.numeric(bygrads_new)~com_missing+com_no+mic_missing+mic_no+dryer_missing+
         dryer_no+asian+hispanic+black+indian+race_missing+race_unknown+race_missing+male)
summary(r5)

stargazer(r1,r2,r3,r4,title = "Results")



#t<-lm(data=data2,as.numeric(bygrads_new)~male+female+sex_missing)

#lrtest(r4,c("dryer_yes","dryer_no"))

#################################################################################################
#################################################################################################


hw2<-subset(data,F4UNI2A==1,select=c('STU_ID','BY2XMSTD','F12XMSTD','F22XMSTD','F1S99C','F2S96B'))
hw2[,'math_base']=ifelse(hw2$BY2XMSTD==99.98|hw2$BY2XMSTD==99.99|hw2$BY2XMSTD==-9,NA,hw2$BY2XMSTD)
hw2[,'math_f12']=ifelse(hw2$F12XMSTD==99.98|hw2$F12XMSTD==99.99|hw2$F12XMSTD==-9,NA,hw2$F12XMSTD)
hw2[,'math_f22']=ifelse(hw2$F22XMSTD==99.98|hw2$F22XMSTD==99.99|hw2$F22XMSTD==-9,NA,hw2$F22XMSTD)

hw2[,'div_1']=ifelse(hw2$F1S99C==1,1,0)
hw2[,'div_1_missing']=ifelse(hw2$F1S99C==8,1,0)
hw2[,'div_1_unknown']=ifelse(hw2$F1S99C==7|hw2$F1S99C==9,1,0)


hw2[,'div_2']=ifelse(hw2$F2S96B==1,1,0)
hw2[,'div_2_missing']=ifelse(hw2$F2S96B==8,1,0)
hw2[,'div_2_unknown']=ifelse(hw2$F2S96B==7|hw2$F2S96B==9,1,0)

t1=data.frame(hw2[,'STU_ID'])
t1[,'test_gain']=hw2$math_f12-hw2$math_base
t1[,'divorce']=hw2$div_1
t1[,'divorce_missing']=hw2$div_1_missing
t1[,'divorce_unknown']=hw2$div_1_unknown
t1[,'t']=1


t2=data.frame(hw2[,'STU_ID'])
t2[,'test_gain']=hw2$math_f22-hw2$math_f12
t2[,'divorce']=hw2$div_2
t2[,'divorce_missing']=hw2$div_2_missing
t2[,'divorce_unknown']=hw2$div_2_unknown
t2[,'t']=2

data3=rbind(t1,t2)
colnames(data3)[1]='STU_ID'

data3=data3[order(data3$STU_ID),]
data3=subset(data3,F4UNI2A==1)

data4=merge(data3,data2,by='STU_ID')

write.csv(file = 'stata.csv',data4)


ols_1=lm(data = data4,test_gain~divorce+divorce_missing+divorce_unknown+
           asian+hispanic+black+indian+race_missing+race_unknown+race_missing+male+sex_missing)
summary(ols_1)

#cluster error bootstrap oneway clustered
vCL_BS<- vcovBS(ols_1, cluster = t1$STU_ID,R=200)
vcl<-vcovCL(ols_1, cluster = t1$STU_ID)
vc_2=vcov(ols_1)
coeftest(ols_1, vcov = vCL_BS)
coeftest(ols_1, vcov = vcl)
coeftest(ols_1, vcov = vc_2)



fe_1=plm(data = data4,test_gain~divorce+divorce_missing+divorce_unknown+
      asian+hispanic+black+indian+race_missing+race_unknown+race_missing+male+sex_missing,index = c('STU_ID','t'),model = 'within')

summary(fe_1)


