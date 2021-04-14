#Si richiama il data base
setwd("C:/Users/Gabriele/Documents/Data Science_R_Python/Corso Data Science Harvard/Capstone project/Mio progetto")
dat<-read.csv("insurance.csv")

#------------------------------------------------------------------------------------
#Analisi della struttura del dataset
head(dat,10)
str(dat) #Structure
dim(dat) #Dimensione 1338 rows 7 columns
colSums(is.na(dat)) #Are there any Nans

#Trasformazione delle variabili factor in numeric per i calcoli successivi
library(dplyr)
library(ggplot2)
library(grid)
library(caret)
library(car)

#-----------------------------------------------------------------------------
#Correlation among variables

d1<-dat %>% mutate(sex_num=as.numeric(dat$sex)) %>%
            mutate(smok_num=as.numeric(dat$smoker)) %>%
            mutate(reg_num=as.numeric(dat$region))
dat_numeric<-data.frame(d1[1],d1[3],d1[4],d1[8],d1[9],d1[10],d1[7])
correlation<-round(cor(dat_numeric),5)
#In particular we notice the correlation among charges and the other variables
cbind(sort(correlation[,7],decreasing=TRUE))
#correl<-data.frame(Charges=correlation[,7])
#colnames(correlation1)<-"Charges"
#rownames(correlation1)<-rownames(correlation)

#Plot a heatmap
heatmap(correlation, Colv = NA, Rowv = NA, scale="column",cexCol =1,cexRow = 1, main="Heatmap")
#oppure
library(corrplot)
corrplot(correlation,method="color",
         addCoef.col="black")

#----------------------------------------------------------------------------------
#Exploratory analysis

#Distribution of charges to evaluate the overall cost
dat %>% ggplot(aes(charges))+
  geom_histogram(col="White")+
  labs(title="Distribution of charges",col="blue")+
  geom_vline(aes(xintercept=mean(charges)))+
  annotation_custom(grobTree(textGrob("Average of charges", x=0.25,  y=0.7, hjust=0,
  gp=gpar(col="black", fontsize=13, fontface="italic"))))

#Charges in relation to smokers and non smokers
dat %>% ggplot(aes(x=smoker,y=charges,fill=smoker))+
        geom_boxplot()+
        xlab("Smokers")
#Between smokers how many females and males
dat %>% filter(smoker=="yes") %>% ggplot(aes(x=sex,y=charges,fill=sex))+
        geom_boxplot()+
        ggtitle("Distribuzione per sesso tra i fumatori")
#How age affect the charges
dat %>% ggplot(aes(x=age,y=charges))+
        geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4)+
        ggtitle("Charges vs age")
#Charges for smokers and non smokers through age
#The cost of treatment increases with age
dat %>% ggplot(aes(x=age,y=charges,color=smoker))+
  geom_point(size=1.5) #it seems there's an interconnection between age and smoker

#Smokers and non smokers at 18 years old
library(gridExtra)
fig1<-dat %>% ggplot(aes(x=age,y=charges))+
  geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4)+
  ggtitle("Charges vs age")
fig2<-dat %>% filter(age==18) %>% ggplot(aes(x=smoker,y=charges,fill=smoker))+
  geom_boxplot()+
  ggtitle("Charges for 18 years old")
grid.arrange(fig1,fig2,ncol=2)

#Charges and age stratified
dat1<-dat;
dat1$age[dat1$age<20]="<20";
dat1$age[dat1$age>=20 & dat1$age<=30]="20-30";
dat1$age[dat1$age>30 & dat1$age<=40]="30-40";
dat1$age[dat1$age>40 & dat1$age<=50]="40-50";
dat1$age[dat1$age>50 & dat1$age<=60]="50-60";
dat1$age[dat1$age>60 & dat1$age<=70]="60-70";
dat1$age[dat1$age>70]=">70"
fig3<-dat1 %>% ggplot(aes(x=age,y=charges,fill=smoker))+geom_boxplot()+ggtitle("Ripartizione per età")
grid.arrange(fig1,fig3,ncol=2) #we can see better that with increasing age we have more charges

#Let's consider Bmi
dat %>% ggplot(aes(x=bmi))+
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666")+
  geom_vline(aes(xintercept=median(bmi)),color="blue", linetype="dashed", size=1)
#Charges vs Bmi
dat %>% ggplot(aes(x=bmi,y=charges))+
        stat_density_2d(aes(fill=..density..),geom="raster",contour=FALSE)+
        scale_fill_distiller(palette=4, direction=1)+
        ggtitle("Major concentration of bmi")
#Charges vs Bmi >30 and <30. Patients with BMI above 30 spend more on treatment
fig3<-dat %>% filter(bmi>30) %>% ggplot(aes(x=charges))+
          geom_histogram(aes(y=..density..), colour="black", fill="white")+
          geom_density(alpha=.2,fill="#FF6666")+
          scale_x_continuous(limits=c(-10000,70000))+
          ggtitle("Charges for Bmi>30")
fig4<-dat %>% filter(bmi<30) %>% ggplot(aes(x=charges))+
          geom_histogram(aes(y=..density..), colour="black", fill="white")+
          geom_density(alpha=.2,fill="#FF6666")+
          scale_x_continuous(limits=c(-10000,70000))+
          ggtitle("Charges for Bmi<30")
grid.arrange(fig3,fig4,ncol=2)
#Let's put all together, Charges, Bmi and smokers
dat %>% ggplot(aes(x=bmi,y=charges,color=smoker))+
               geom_point()+
               geom_hline(yintercept=mean(dat$charges))+
               annotation_custom(grobTree(textGrob("Average of charges", x=0.6,  y=0.25, hjust=0,
               gp=gpar(col="black", fontsize=13, fontface="italic"))))  #For bmi>30 there's an increasing charge expecially for smokers

#---------------------------------------------------------------------------
#Building the train and test set
index<-createDataPartition(dat$charges,p=0.8,times=1,list=FALSE)
trainset<-dat[index,]
testset<-dat[-index,]

#---------------------------------------------------------------------------

#Building a regression model
preproc_dat<-preProcess(x=trainset,method = c("center", "scale"))
model<-lm(charges~.,data=preproc_dat)
summary(model)
model1<-lm(charges~age+bmi+children+smoker,data=trainset)
summary(model1) #R-squared is 0.75

#con train
train_control<-trainControl(method="cv",number=10)
a<-train(charges~.,
         data=trainset,
         method="lm",
         trcontrol=train_control,
         preProcess=c("scale","center"))

#Residual analysis
residuals<-model1$residuals

#1 Linearity
plot(model1,1) #Not linear
pairs(model1)
#Let's apply log(y)
model2<-lm(log(charges)~age+bmi+children+smoker,data=trainset)
plot(model2,1) #Roughly linear

#2 Normality
shapiro.test(residuals) #Not normality
plot(model1,2)                 #Not normal
model2<-lm(log(charges)~age+bmi+children+smoker,data=trainset)
summary(model2)   #R-squared is 0.76
shapiro.test(model2$residuals) #Not normal

#2 Omoschedasticity
plot(model2,3)   #No Omoschedasticity

#Collinearity
car::vif(model2)   #No multicollinearity

#Leverage,Outliers, Influential points
plot(model2,4,id.n=7)  #Influential points
plot(model2,5)
