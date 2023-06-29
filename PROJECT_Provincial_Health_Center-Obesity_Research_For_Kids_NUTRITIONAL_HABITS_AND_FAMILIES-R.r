linregboykg1=lm(formula=BOY1~AGIRLIK1,data =Data_Obesitiy_Research_NutritionalHabits_1)
linregboykg1
shapiro.test(Data_Obesitiy_Research_NutritionalHabits_1$BKI1)
shapiro.test(Data_Obesitiy_Research_NutritionalHabits_2$BKI2)
#	Normality assumption of errors.(Shapiro-Wilk Test)

# Errors have constant variance. (Breusch-Pagan Test)
#	Errors are uncorrelated.(Durbin-Watson Test)
#	There is no multicollinearity. (Checking VIF)

# 2. ANALİZ Kombinasyon
choose(9,3)

kume=c("1. Beslenme Alıskanligi","2. Vucut Agirligi","3. Sigara icme","4. Cevresel Faktorler","5. Fiziksel Aktivite Durumu","6. Alkol Kullanimi","7. Stres","8. Genetik Yapi","9. Aile Yasam Tarzı ")
combn(kume,3)
saglik_etki=c(combn(kume,3))
kombinasyonlar
kumevariable=c(Data_Obesitiy_Research_NutritionalHabits_1$`B2-AİLE Y.`,Data_Obesitiy_Research_NutritionalHabits_1$`B2-ALKOL`,Data_Obesitiy_Research_NutritionalHabits_1$`B2-STRES`,Data_Obesitiy_Research_NutritionalHabits_1$`B2-BES`,Data_Obesitiy_Research_NutritionalHabits_1$`B2-FİZ.AK`,Data_Obesitiy_Research_NutritionalHabits_1$`B2-GEN`,Data_Obesitiy_Research_NutritionalHabits_1$`B2-SİGAR`,Data_Obesitiy_Research_NutritionalHabits_1$`B2-VUC.A`,Data_Obesitiy_Research_NutritionalHabits_1$`B2-ÇEVRE`)
combn(kumevariable,3)

tabl=table(SORU2ANALIZOGLEYEMEGI$`B2-BES`,SORU2ANALIZOGLEYEMEGI$C11)
tabl

barplot(Analiz1$BKI1)
barplot(Analiz1$BKI2)
barplot(Analiz1$Olcek1)
barplot(Analiz2$Olcek2)
summary(Analiz1$BKI1)
summary(Analiz1$BKI2)
sd(Analiz1$BKI1)
sd(Analiz1$BKI2)
summary(Analiz1$BKI1)
summary(Analiz1$BKI2)
summary(Analiz12$BKI2)
plot(Analiz1$Cinsiyet1)
table(Analiz1$Olcek1,Analiz1$Cinsiyet1)
pie(table(Analiz1$Olcek1,Analiz1$Cinsiyet1))
pie(table(Analiz1$Cinsiyet1,Analiz1$Olcek1))
table(Analiz1$Cinsiyet1,Analiz1$Olcek1)
table(Analiz12$Cinsiyet2,Analiz12$Olcek2)

pie(table(Analiz1$Cinsiyet1,Analiz1$Olcek1))

counts<-table(Analiz1$Cinsiyet1,Analiz1$BKI1)
library(vcd)
barplot(counts,main="Cinsiyetlere göre BKI",xlab="Cinsiyet",ylab="BKI Ölçeği", col=c("red","yellow"),legend=row.names(counts))
library(shiny)

count.fields()

lbls1=c("Zayıf","Normal Kilolu","Fazla Kilolu","Obez")
hepsis1=c(17,64,7,2)
erkeks1=c(7,38,6,2)
kadins1=c(10,26,1,0)
pie3D(hepsis1,labels=lbls1,explode=0.1,main="BKI Oranları")
pie(hepsis1,labels=lbls1,explode=0.1,main="İlk gözlemlere göre BKI Oranları",col = c("Yellow","deepskyblue","Orange","Red"))
barchart(hepsis1,lbls1)

ggplot(Analiz1$BKI1,Analiz1$BKI2,geom="boxplot",data=Analiz1,main="Grup I ve Grup II karşılaştırması")
summary(Analiz1$BKI1)
#KADIN
boxplot(Analiz1$BKI1[which=Analiz1$Cinsiyet1=="KADIN"],main="Grup 1: Kadınlar")
boxplot(Analiz12$BKI2[which=Analiz12$Cinsiyet2=="KADIN"],main="Grup 2: Kadınlar")
summary(Analiz1$BKI1[which=Analiz1$Cinsiyet1=="KADIN"])
sd(Analiz1$BKI1[which=Analiz1$Cinsiyet1=="KADIN"])
summary(Analiz12$BKI2[which=Analiz12$Cinsiyet2=="KADIN"])
sd(Analiz12$BKI2[which=Analiz12$Cinsiyet2=="KADIN"])
#ERKEK
boxplot(Analiz1$BKI1[which=Analiz1$Cinsiyet1=="ERKEK"],main="Grup 1: Erkekler")
boxplot(Analiz12$BKI2[which=Analiz12$Cinsiyet2=="ERKEK"],main="Grup 2: Erkekler")
summary(Analiz1$BKI1[which=Analiz1$Cinsiyet1=="ERKEK"])
sd(Analiz1$BKI1[which=Analiz1$Cinsiyet1=="ERKEK"])
summary(Analiz12$BKI2[which=Analiz12$Cinsiyet2=="ERKEK"])
sd(Analiz12$BKI2[which=Analiz12$Cinsiyet2=="ERKEK"])

########
pie(A1S$C6[which=A1S$Olcek1=="Fazla Kilolu"])
table(A1S$Olcek1[which=A1S$C6==1])
table(A2S$Olcek2[which=A2S$C6==1])
table(A1S$Olcek1[which=A1S$C6==2][which=A1S$Cinsiyet1=="ERKEK"])
table(A1S$Olcek1[which=A1S$C6==2][which=A1S$Cinsiyet1=="KADIN"])
###
table(A1S$Olcek1[which=A2S$C6==1][which=A2S$Cinsiyet2=="ERKEK"])
table(A1S$Olcek1[which=A2S$C6==1][which=A2S$Cinsiyet2=="KADIN"])

table(A1S$Olcek1[which=A2S$C6==1][which=A2S$Cinsiyet2=="ERKEK"])
table(A1S$Olcek1[which=A2S$C6==1][which=A2S$Cinsiyet2=="KADIN"])

table(A2S$Olcek2[which=A2S$C6==1][which=A2S$Cinsiyet2=="ERKEK"])
table(A2S$Olcek2[which=A2S$C6==1][which=A2S$Cinsiyet2=="KADIN"])

summary(A1S$BKI1[which=A1S$C6==1])
summary(A2S$BKI2[which=A1S$C6==1])
summary(A1S$BKI1[which=A2S$C6==1][which=A2S$Cinsiyet2=="ERKEK"])
summary(A1S$BKI1[which=A2S$C6==1][which=A2S$Cinsiyet2=="KADIN"])

#isaret testi
SIGN.test(A1S$BKI1[which=A1S$C6==1], md = 27.45, alternative = "two.sided", conf.level = 0.95)#Grup 1
SIGN.test(A2S$BKI2[which=A2S$C6==1], md = 27.45, alternative = "two.sided", conf.level = 0.95)#Grup 2
SIGN.test(A1S$BKI1[which=A1S$C6==1][which=A1S$Cinsiyet1=="ERKEK"], md = 27.45, alternative = "two.sided", conf.level = 0.95)#Grup 1: Erkek
SIGN.test(A1S$BKI1[which=A1S$C6==1][which=A1S$Cinsiyet1=="KADIN"], md = 27.45, alternative = "two.sided", conf.level = 0.95)#Grup 1: Kadın
SIGN.test(A2S$BKI2[which=A2S$C6==1][which=A2S$Cinsiyet2=="ERKEK"], md = 27.45, alternative = "two.sided", conf.level = 0.95)#Grup 2: Erkek
SIGN.test(A2S$BKI2[which=A2S$C6==1][which=A2S$Cinsiyet2=="KADIN"], md = 27.45, alternative = "two.sided", conf.level = 0.95)#Grup 2: Kadın

#kantinden urun alanların meyve satisi olmasini istemesi
table(A1S$D2)#Grup 1
table(A1S$D2[which=A1S$Cinsiyet1=="ERKEK"])#Grup 1: Erkek
table(A1S$D2[which=A1S$Cinsiyet1=="KADIN"]) #Grup 1:Kadın
table(A2S$D2) #Grup 2
table(A2S$D2[which=A2S$Cinsiyet2=="KADIN"]) #Grup 2: Kadın
table(A2S$D2[which=A2S$Cinsiyet2=="ERKEK"]) #Gru 2: Erkek

table(A1S$D2[which=A1S$Cinsiyet1=="ERKEK"],A1S$D2[which=A1S$Cinsiyet1=="KADIN"])

grup1=table(A1S$D2,A1S$Cinsiyet1)#tablo conting
grup2=table(A2S$D2,A2S$Cinsiyet2)
chisq.test(grup1)
chisq.test(grup2)

lm(formula=regboykg2$X0~regboykg2$X1,data =regboykg2)
lm(formula=regboykg2$X1~regboykg2$X0,data =regboykg2)

glm(formula=A1S$D3~A1S$C3,data=A1S)
length(A1S$D3)
A1S$
  

install.packages("nnet")
library(nnet)

test<-multinom(A1S$D3~A1S$C3,data=A1S)
summary(test)

a4i=table(A1S$D3,A1S$C3)
a4i2=table(A2S$D3,A2S$C3)
chisq.test(a4i)
chisq.test(a4i2)
 #GRUP 1
a4iKad=table(A1S$D3[which=A1S$Cinsiyet1=="KADIN"],A1S$C3[which=A1S$Cinsiyet1=="KADIN"])
a4ierk=table(A1S$D3[which=A1S$Cinsiyet1=="ERKEK"],A1S$C3[which=A1S$Cinsiyet1=="ERKEK"])
a4iKad
a4ierk
chisq.test(a4iKad)
chisq.test(a4ierk)

#GRUP 2
a4i2Kad=table(A2S$D3[which=A2S$Cinsiyet2=="KADIN"],A2S$C3[which=A2S$Cinsiyet2=="KADIN"])
a4i2erk=table(A2S$D3[which=A2S$Cinsiyet2=="ERKEK"],A2S$C3[which=A2S$Cinsiyet2=="ERKEK"])
a4i2Kad
a4i2erk
chisq.test(a4i2Kad)
chisq.test(a4i2erk)

a4i2=table(A2S$D3,A2S$C3)


table(A1S$`F3-DI_F_A`[which=A1S$F1==1],ylab="Sıklık")
table(A1S$`F3-DIYET`[which=A1S$F1==1],ylab="Sıklık")
table(A1S$`F3-DI_F_A`[which=A1S$F1==1],ylab="Sıklık")

#Fazla kilolu olduğunu düşünenler(F1:EVET:1)/kilo vermek isteyenler(F2:EVET:1)/Ne yapıyor(F3: 1-8 ölçek)/sıklık
table(A1S$F1)#Fazla kilolu olduğunu düşünenler EVET=26(kadın 14; erkek 12) HAYIR=62(kadın 22, erkek:44) GRUP 1
table(A2S$F1)#Fazla kilolu olduğunu düşünenler EVET=20(kadın 12; erkek 8) HAYIR=42(kadın 17, erkek:25) GRUP 2



GS1<-c(4,16,9,1,2,4,9,3,10)
GS2<-c(5,7,6,0,2,3,4,2,8)
GSN<-c("I","II","III","IV","V","VI","VII","VIII","IX")
CGS=c("Sadece diyet","Diyet ile birlikte fiziksel aktivite","Sadece fiziksel aktivite","Zayıflama ilacı","Zayıflama ürünleri","Diyet ürünler","Ekmek tüketimini kesme","Karbonhidrat grubundan tamamen uzak","Hiçbir şey ")
names(GS1)=GSN
names(GS2)=GSN
barplot(GS1,GS2,main = "Fazla Kilolu olduğunu düşünen öğrencilerin bunun için ne yaptıkları ",col=c("blue","red"),beside="TRUE")
barplot(GS2,main = "Fazla Kilolu olduğunu düşünen öğrencilerin bunun için ne yaptıkları ",col="blue")
means.barplot <- qplot(x=group, y=mean, fill=variable,
                       data=means, geom="bar", stat="identity",
                       position="dodge")


table(A2S$F3)
1.Sadece diyet yapıyorum 
2.Diyet ile birlikte fiziksel aktivite yapıyorum OK
3.Sadece fiziksel aktivite yapıyorum OK
4.Zayıflama ilacı kullanıyorum OK
5.Zayıflama ürünleri (bitkisel çay, zayıflama bandı vb. kullanıyorum) OK
6.Diyet ürünler (az yağlı süt-yoğurt, diyet bisküvi, diyet reçel vb.) kullanıyorum OK
7.Ekmek tüketmiyorum OK
8.Karbonhidrat grubundan tamamen uzak duruyorum OK

#yeni if
for(i in 1:dim(A1S)){
if(A1S$`F3-DI_F_A`==1) {A1S$F3BIR1="Diyet ile birlikte fiziksel aktivite yapıyorum"} #2
else if(A1S$`F3-DIY_URUN`=1) {A1S$F3BIR1="Diyet ürünler kullanıyorum"}
else if(A1S$`F3-DIYET`==1) {A1S$F3BIR1="Sadece diyet yapıyorum"}
else if(A1S$`F3-EKMEK`==1) {A1S$F3BIR1="Ekmek tüketmiyorum"}
else if(A1S$`F3-HIC`==1) {A1S$F3BIR1="Hiçbir şey yapmıyorum"}
else if(A1S$`F3-KHO`==1) {A1S$F3BIR1="Karbonhidrat grubundan tamamen uzak duruyorum"}
else if(A1S$`F3-S_FA_K`==1) {A1S$F3BIR1="Sadece fiziksel aktivite yapıyorum"}
else if(A1S$`F3-Z_ILAÇ`==1) {A1S$F3BIR1="Zayıflama ilacı kullanıyorum"}
else if(A1S$`F3-Z_URUN`==1) {A1S$F3BIR1="Zayıflama ürünleri kullanıyorum"}

  table((A1S$`F3-DIYET`==1)[which=A1S$Cinsiyet1=="KADIN"])   #1
  table((A1S$`F3-DI_F_A`==1)[which=A1S$Cinsiyet1=="KADIN"])  #2
  table((A1S$`F3-S_FA_K`==1)[which=A1S$Cinsiyet1=="KADIN"])  #3
  table((A1S$`F3-Z_ILAÇ`==1)[which=A1S$Cinsiyet1=="KADIN"])  #4
  table((A1S$`F3-Z_URUN`==1)[which=A1S$Cinsiyet1=="KADIN"])  #5
  table((A1S$`F3-DIY_URUN`==1)[which=A1S$Cinsiyet1=="KADIN"]) #6
  table((A1S$`F3-EKMEK`==1)[which=A1S$Cinsiyet1=="KADIN"])   #7
  table((A1S$`F3-KHO`==1)[which=A1S$Cinsiyet1=="KADIN"])     #8
  table((A1S$`F3-HIC`==1)[which=A1S$Cinsiyet1=="KADIN"])     #9

  table((A2S$`F3-DIYET`==1)[which=A2S$Cinsiyet2=="KADIN"])   #1
  table((A2S$`F3-DI_F_A`==1)[which=A2S$Cinsiyet2=="KADIN"])  #2
  table((A2S$`F3-S_FA_K`==1)[which=A2S$Cinsiyet2=="KADIN"])  #3
  table((A2S$`F3-Z_ILAÇ`==1)[which=A2S$Cinsiyet2=="KADIN"])  #4
  table((A2S$`F3-Z_URUN`==1)[which=A2S$Cinsiyet2=="KADIN"])  #5
  table((A2S$`F3-DIY_URUN`==1)[which=A2S$Cinsiyet2=="KADIN"]) #6
  table((A2S$`F3-EKMEK`==1)[which=A2S$Cinsiyet2=="KADIN"])   #7
  table((A2S$`F3-KHO`==1)[which=A2S$Cinsiyet2=="KADIN"])     #8
  table((A2S$`F3-HIC`==1)[which=A2S$Cinsiyet2=="KADIN"])     #9
 
}

install.packages("devtools")
library("yarr")
install.packages("ndphillips")
install.packages("yarrr")
install.packages("nphillips")
library(ggplot2)
qplot(GS1, GS2,  geom="boxplot")
dodge <- position_dodge(width=0.9) 

#now add the error bars to the plot
geom_linerange(aes(ymax=GS1.avg+GS2.sd, ymin=GS2.avg-GS1.sd), position=dodge)+theme_bw()

counts <- table(GS1, GS2)
barplot(counts, main="Grup 1 vs Grup 2",
        xlab="Seçimler", col=c("darkblue","red"),
        legend = rownames(counts), beside=TRUE)

######


library(ggplot2)
ggplot(means.long,aes(x=variable,y=value,fill=factor(gender)))+
  geom_bar(stat="identity",position="dodge")+
  scale_fill_discrete(name="Gender",
                      breaks=c(1, 2),
                      labels=c("Male", "Female"))+
  xlab("Beverage")+ylab("Mean Percentage")

barplot(x,border=”tan2”,names.arg=c(”Jan”,”Feb”
                                     ,”Mar”,”Apr”,”May”),+xlab=
          ”Month”,ylab = ”Revenue”,density = 
          c(0,5,20,50,100))

C<-matrix(c(10,40,22.5,2.5,5,10,22.5,7.5,25,12,27,23,0,7.5,11.5,15,7.5,30),nrow=2, ncol =9,byrow=TRUE)

A<-matrix(c(4,5,16,7,9,6,1,0,2,2,4,3,9,4,3,2,10,8),nrow=2, ncol =9,byrow=TRUE)
barplot(A,main= ”Fazla Kilolu”,names.arg=c("I","II","III","IV","V","VI","VII","VIII","IX"),xlab=”Öğrencilerin Seçimleri”, ylab=”Seçim Sayısı”, col =c(”red”,”blue”))



GS1<-c(4,16,9,1,2,4,9,3,10)
GS2<-c(5,7,6,0,2,3,4,2,8)
GSN<-c("I","II","III","IV","V","VI","VII","VIII","IX")
names(GS1)=GSN
names(GS2)=GSN

barplot(C,border="black",main = "Tercihlerin gruplara göre % oranlarının karşılaştırması",names.arg=c("I","II","III","IV","V","VI","VII","VIII","IX"),col=c("blue","red"),beside=TRUE)
legend("topright", fill= c("blue","red"), c("Grup 1", "Grup 2"),cex=0.7  );


legend(c("Grup 1","Grup 2"),col=("blue","red"))
barplot(C,border="black",main = "Fazla Kilolu olduğunu düşünen öğrencilerin bunun için ne yaptıkları",names.arg=c("I","II","III","IV","V","VI","VII","VIII","IX"),
        xlab=”month”, ylab=”revenue”, col=c("blue","red"),beside=TRUE)

#Açıklamalar
#-  x, eldeki verilerimiz yani y eksenine karşılık gelen değerlerimiz
#- border, çubukların kenar kalınlığını belirler
#- names.arg, y eksenindeki verilerin x eksenindeki isimleri temsil eder
#- xlab, x eksenin genel adını temsil eder
#- ylab, y eksenin genel adını temsil eder
#- density, çubukların yo^gunluklarını ayarlar  

#Kahvaltı yapma oranları
table(A1S$C6)
table(A1S$C6[which=A1S$Cinsiyet1=="ERKEK"])
table(A1S$C6[which=A1S$Cinsiyet1=="KADIN"])
table(A2S$C6)
table(A2S$C6[which=A2S$Cinsiyet2=="ERKEK"])
table(A2S$C6[which=A2S$Cinsiyet2=="KADIN"])

#Tokluk hissi vS Yemek yeme süresi #D3: Yeme süresi, D4: Tokluk beyne
#Grup 1
tablo1=table(A1S$D3,A1S$D4)
table(A1S$D3[which=A1S$Cinsiyet1=="KADIN"],A1S$D4[which=A1S$Cinsiyet1=="KADIN"])
table(A1S$D3[which=A1S$Cinsiyet1=="ERKEK"],A1S$D4[which=A1S$Cinsiyet1=="ERKEK"])

#Grup 2
tablo2=table(A2S$D3,A2S$D4)
table(A2S$D3[which=A2S$Cinsiyet2=="KADIN"],A2S$D4[which=A2S$Cinsiyet2=="KADIN"])
table(A2S$D3[which=A2S$Cinsiyet2=="ERKEK"],A2S$D4[which=A2S$Cinsiyet2=="ERKEK"])

tabloT=table(A1S$D3+A2S$D3,A1S$D4+A2S$D4)

chisq.test(tablo1)



##
Grup1ana<-matrix(c(1,10,28,1,0,2,22,0,0,2,8,1,0,5,8,1),nrow=4, ncol =4,byrow=TRUE)
Grup1ara<-matrix(c(3,12,20,3,1,1,0,11,9,4,0,0,1,3,5,2,0,0,2,5,4,1,0,0),nrow=4, ncol =6,byrow=TRUE)
#Grup 1
table(A1S$C15,A1S$C2)
table(A1S$C15,A1S$C18)
table(A1S$C15[which=A1S$Cinsiyet1=="KADIN"],A1S$C2[which=A1S$Cinsiyet1=="KADIN"])
table(A1S$C15[which=A1S$Cinsiyet1=="ERKEK"],A1S$C18[which=A1S$Cinsiyet1=="ERKEK"])
#Grup 2
table(A2S$C15,A2S$C2)#ana
table(A2S$C15,A2S$C18)
table(A2S$C15[which=A2S$Cinsiyet2=="KADIN"],A2S$C2[which=A2S$Cinsiyet2=="KADIN"])
table(A2S$C15[which=A2S$Cinsiyet2=="ERKEK"],A2S$C18[which=A2S$Cinsiyet2=="ERKEK"])
#Testler
#Grup 1
G1Ana=table(A1S$C15,A1S$C2)
G1Ara=table(A1S$C15,A1S$C18)
G1AnaKADIN=table(A1S$C15[which=A1S$Cinsiyet1=="KADIN"],A1S$C2[which=A1S$Cinsiyet1=="KADIN"])
G1AraKADIN=table(A1S$C15[which=A1S$Cinsiyet1=="KADIN"],A1S$C18[which=A1S$Cinsiyet1=="KADIN"])
G1AnaERKEK=table(A1S$C15[which=A1S$Cinsiyet1=="ERKEK"],A1S$C2[which=A1S$Cinsiyet1=="ERKEK"])
G1AraERKEK=table(A1S$C15[which=A1S$Cinsiyet1=="ERKEK"],A1S$C18[which=A1S$Cinsiyet1=="ERKEK"])
chisq.test(G1Ana)
chisq.test(G1Ara)
chisq.test(G1AnaKADIN)
chisq.test(G1AraKADIN)
chisq.test(G1AnaERKEK)
chisq.test(G1AraERKEK)


#Grup 2
G2Ana=table(A2S$C15,A2S$C2)
G2Ara=table(A2S$C15,A2S$C18)
G1AnaKADIN=table(A2S$C15[which=A2S$Cinsiyet2=="KADIN"],A2S$C2[which=A2S$Cinsiyet2=="KADIN"])
G2AraKADIN=table(A2S$C15[which=A2S$Cinsiyet2=="KADIN"],A2S$C18[which=A2S$Cinsiyet2=="KADIN"])
G1AnaERKEK=table(A2S$C15[which=A2S$Cinsiyet2=="ERKEK"],A2S$C2[which=A2S$Cinsiyet2=="ERKEK"])
G2AraERKEK=table(A2S$C15[which=A2S$Cinsiyet2=="ERKEK"],A2S$C18[which=A2S$Cinsiyet2=="ERKEK"])
chisq.test(G2Ana)
chisq.test(G2Ara)
chisq.test(G2AnaKADIN)
chisq.test(G2AraKADIN)
chisq.test(G2AnaERKEK)
chisq.test(G2AraERKEK)

##




chisq.test(T1+T2)
glm(A1S$C15~A1S$C3)
chisq.test(table(A1S$C15,A1S$C18))
test6<-multinom(A1S$D3~A1S$C3,data=A1S)
summary(test)

table(A1S$C4==1,A1S$C11)
table(A1S$C4==2,A1S$C14)
table(A1S$C4==3,A1S$C8)
barplot(A1S$C4==1,A1S$C11)

table(A2S$C4==1,A2S$C11)
table(A2S$C4==2,A2S$C14)
table(A2S$C4==3,A2S$C8)
barplot(A2S$C4==1,A2S$C11)

K<-matrix(c(4,5,16,7,9,6,1,0,2,2,4,3,9,4,3,2,10,8),nrow=2, ncol =9,byrow=TRUE)
#Grup 1
k<-c(12,10,6,0)
barplot(k,names.arg=c("I","II","III","IV"),col="blue")
o<-c(14,2,0)
barplot(o,names.arg=c("I","II","III"),col="blue")
a<-c(52,27,2,24,5)
barplot(a,names.arg=c("I","II","III","IV","V"),col="blue")
#Grup 2
k2<-c(5,5,0,1)
barplot(k2,names.arg=c("I","II","III","IV"),col="red")
o2<-c(10,0,0)
barplot(o2,names.arg=c("I","II","III"),col="red")
a2<-c(33,15,1,10,7)
barplot(a2,names.arg=c("I","II","III","IV","V"),col="red")
#Test
t1=table(A1S$C4==1,A1S$C11)
t2=table(A1S$C4==2,A1S$C11)
t3=table(A1S$C4==3,A1S$C11)
chisq.test(t1)
chisq.test(t2)
chisq.test(t3)

table(A2S$`C8-PEYZE`) 
table(A2S$`C8-POGA`)
table(A2S$`C8-ICECEK`)
table(A2S$`C8-GEVRE`)
table(A2S$`C8-DIGER`)

#Analiz 11
#Grup 1
table(A1S$`D1-CIKOL`[which=A1S$Olcek1=="Fazla Kilolu"])
table(A1S$`D1-CIPS`[which=A1S$Olcek1=="Fazla Kilolu"])
table(A1S$`D1-BISKU`[which=A1S$Olcek1=="Fazla Kilolu"])
table(A1S$`D1-SIMIT`[which=A1S$Olcek1=="Fazla Kilolu"])
table(A1S$`D1-MEYSU`[which=A1S$Olcek1=="Fazla Kilolu"])
table(A1S$`D1-SUT`[which=A1S$Olcek1=="Fazla Kilolu"])
table(A1S$`D1-COLA`[which=A1S$Olcek1=="Fazla Kilolu"])

#Grup 2
table(A2S$`D1-CIKOL`[which=A2S$Olcek2=="Fazla Kilolu"])
table(A2S$`D1-CIPS`[which=A2S$Olcek2=="Fazla Kilolu"])
table(A2S$`D1-BISKU`[which=A2S$Olcek2=="Fazla Kilolu"])
table(A2S$`D1-SIMIT`[which=A2S$Olcek2=="Fazla Kilolu"])
table(A2S$`D1-MEYSU`[which=A2S$Olcek2=="Fazla Kilolu"])
table(A2S$`D1-SUT`[which=A2S$Olcek2=="Fazla Kilolu"])
table(A2S$`D1-COLA`[which=A2S$Olcek2=="Fazla Kilolu"])

table(A1S$`D1-CIKOL`)
table(A1S$`D1-CIPS`)
table(A1S$`D1-BISKU`)
table(A1S$`D1-SIMIT`)
table(A1S$`D1-MEYSU`)
table(A1S$`D1-SUT`)
table(A1S$`D1-COLA`)

table(A2S$`D1-CIKOL`)
table(A2S$`D1-CIPS`)
table(A2S$`D1-BISKU`)
table(A2S$`D1-SIMIT`)
table(A2S$`D1-MEYSU`)
table(A2S$`D1-SUT`)
table(A2S$`D1-COLA`)

SM<-matrix(c(44.5,1.5,17,12,10,7,8,44,4,15,8.5,13.5,6.5,8.5),nrow=2, ncol =7,byrow=TRUE)
barplot(SM,border="black",main = "Gruplara göre satılan ürünler(%)",names.arg=c("Çikolata","Cips","Bisküvi","Simit, Poaça vb","Meyve Suyu","Süt, ayran vb.","Gazlı içecekler"),col=c("blue","red"),beside=TRUE)
legend("topright", fill= c("blue","red"), c("Grup 1", "Grup 2"),cex=0.6  );
S1<-c(44.5,1.5,17,12,10,7,8)
S2<-c(44,4,15,8.5,13.5,6.5,8.5)
SN<-c("I","II","III","IV","V","VI","VII")
barplot()

#Grup 1
summary(A1S$`C8-PEYZE`==1)#52 kişi
summary((A1S$`C8-PEYZE`==1)[which=A1S$C11==2]) # kişi fastfood yiyor peynir zeytinci (ÖĞLE)
summary((A1S$`C8-PEYZE`==1)[which=A1S$C14==2]) # kişi fastfood yiyor peynir zeytinci (AKŞAM)

summary(A1S$`C8-POGA`==1) #27 KİŞİ
summary((A1S$`C8-POGA`==1)[which=A1S$C11==2])  # kişi fastfood yiyor poğaça simitci (ÖĞLE)
summary((A1S$`C8-POGA`==1)[which=A1S$C14==2])  # kişi fastfood yiyor poğaça simitçi (AKŞAM)

#Grup 2
summary(A2S$`C8-PEYZE`==1)
summary((A2S$`C8-PEYZE`==1)[which=A2S$C11==2]) # kişi fastfood yiyor peynir zeytinci (ÖĞLE)
summary((A2S$`C8-PEYZE`==1)[which=A2S$C14==2]) # kişi fastfood yiyor peynir zeytinci (AKŞAM)

summary(A2S$`C8-POGA`==1)
summary((A2S$`C8-POGA`==1)[which=A2S$C11==2])  # kişi fastfood yiyor poğaça simitci (ÖĞLE)
summary((A2S$`C8-POGA`==1)[which=A2S$C14==2])  # kişi fastfood yiyor poğaça simitçi (AKŞAM)



summary((A1S$C11==2)[which=A1S$`C8-PEYZE`==1]) 

#Analiz 12
#Grup 1
#Beyaz Ekmek
summary(A1S$C15==1)
summary((A1S$C15==1)[which=A1S$Olcek1=="Normal Kilolu"])
table((A1S$C15==1)[which=A1S$Olcek1=="Fazla Kilolu"])
#Tambuğday Ekmek
summary(A1S$C15==2)
summary((A1S$C15==2)[which=A1S$Olcek1=="Normal Kilolu"])
summary((A1S$C15==2)[which=A1S$Olcek1=="Fazla Kilolu"])
#Kepekli, çavdarlı, yulaflı
summary(A1S$C15==3)
summary((A1S$C15==3)[which=A1S$Olcek1=="Normal Kilolu"])
summary((A1S$C15==3)[which=A1S$Olcek1=="Fazla Kilolu"])
#Ekmek yemiyorum
summary(A1S$C15==4)
summary((A1S$C15==4)[which=A1S$Olcek1=="Normal Kilolu"])
summary((A1S$C15==4)[which=A1S$Olcek1=="Fazla Kilolu"])

#Grup 2
#Beyaz Ekmek
summary(A2S$C15==1)
summary((A2S$C15==1)[which=A2S$Olcek2=="Normal Kilolu"])
table((A2S$C15==1)[which=A2S$Olcek2=="Fazla Kilolu"])
#Tambuğday Ekmek
summary(A2S$C15==2)
summary((A2S$C15==2)[which=A2S$Olcek2=="Normal Kilolu"])
summary((A2S$C15==2)[which=A2S$Olcek2=="Fazla Kilolu"])
#Kepekli, çavdarlı, yulaflı
summary(A2S$C15==3)
summary((A2S$C15==3)[which=A2S$Olcek2=="Normal Kilolu"])
summary((A2S$C15==3)[which=A2S$Olcek2=="Fazla Kilolu"])
#Ekmek yemiyorum
summary(A2S$C15==4)
summary((A2S$C15==4)[which=A2S$Olcek2=="Normal Kilolu"])
summary((A2S$C15==4)[which=A2S$Olcek2=="Fazla Kilolu"])

ekt=matrix(c(84.5,15.5,86,14),nrow=2,ncol=2,byrow=TRUE)
chisq.test(ekt)
ekt2=matrix(c(37,8,5,0),nrow=2,ncol=2,byrow=TRUE)
chisq.test(ekt2)

#Analiz 14 
#GRUP1 TAMAMI
table((A1S$C8)[which=A1S$`B2-BES`==1]) #KAHVALTI
table((A1S$C11)[which=A1S$`B2-BES`==1]) #ÖĞLE
table((A1S$C14)[which=A1S$`B2-BES`==1]) #AKŞAM
#GRUP1 KADIN
table((A1S$C8)[which=A1S$`B2-BES`==1][which=A1S$Cinsiyet1=="KADIN"]) #KAHVALTI
table((A1S$C11)[which=A1S$`B2-BES`==1][which=A1S$Cinsiyet1=="KADIN"]) #ÖĞLE
table((A1S$C14)[which=A1S$`B2-BES`==1][which=A1S$Cinsiyet1=="KADIN"]) #AKŞAM
#GRUP2 ERKEK
table((A1S$C8)[which=A1S$`B2-BES`==1][which=A1S$Cinsiyet1=="ERKEK"]) #KAHVALTI
table((A1S$C11)[which=A1S$`B2-BES`==1][which=A1S$Cinsiyet1=="ERKEK"]) #ÖĞLE
table((A1S$C14)[which=A1S$`B2-BES`==1][which=A1S$Cinsiyet1=="ERKEK"]) #AKŞAM
######################################################################
#GRUP2 TAMAMI
table((A2S$C8)[which=A2S$`B2-BES`==1]) #KAHVALTI
table((A2S$C11)[which=A2S$`B2-BES`==1]) #ÖĞLE
table((A2S$C14)[which=A2S$`B2-BES`==1]) #AKŞAM
#GRUP2 KADIN
table((A2S$C8)[which=A2S$`B2-BES`==1][which=A2S$Cinsiyet2=="KADIN"]) #KAHVALTI
table((A2S$C11)[which=A2S$`B2-BES`==1][which=A2S$Cinsiyet2=="KADIN"]) #ÖĞLE
table((A2S$C14)[which=A2S$`B2-BES`==1][which=A2S$Cinsiyet2=="KADIN"]) #AKŞAM
#GRUP2 ERKEK
table((A2S$C8)[which=A2S$`B2-BES`==1][which=A2S$Cinsiyet2=="ERKEK"]) #KAHVALTI
table((A2S$C11)[which=A2S$`B2-BES`==1][which=A2S$Cinsiyet2=="ERKEK"]) #ÖĞLE
table((A2S$C14)[which=A2S$`B2-BES`==1][which=A2S$Cinsiyet2=="ERKEK"]) #AKŞAM

#Grup 1-2 Kahvaltı % veriler
barplot(YKahv,ylab="Yüzde Oranları %",beside=TRUE,main="Sağlığı etkileyen faktörlerden beslenme alışkanlığı seçenlerin kahvaltı oranları (%)",col=c("yellow","orange","skyblue","blue","green","darkgreen"),names.arg = c("Çay,peynir,zeytin,yumurta","Çay,poğaça,tost,simit,börek","Sadece içecek","Süt ile tahıl gevreği","Diğer"))
Kahv<-matrix(c(75,65,47,41.5,59,52.5,8.33,17.5,33,21,22,20,0,0,0,0,0,0,12.5,17.5,17,12.5,15,15,4.16,0,3,25,4,12.5),nrow=5,ncol=6,byrow=TRUE)
YKahv=t(Kahv)
legend("topright", fill= c("yellow","orange","skyblue","blue","green","darkgreen"), c("Kadın(Grup 1)", "Kadın(Grup 2)","Erkek(Grup 1)","Erkek(Grup 2)","Toplam (Grup 1)","Toplam (Grup 2)"),cex=0.6  );
ax.set_ylabel('Yüzde Oranı (%)')


#Grup1-2 Öğle öğünü % veriler
ogle<-t(matrix(c(40,47,27,35,33,40.5,36,31,40,39,38,36,24,5,33,22,29,14,0,16,0,4,0,9.5),nrow=4,ncol=6,byrow=TRUE))
barplot(ogle,beside=TRUE,ylab="Yüzde Oranları %",main="Sağlığı etkileyen faktörlerden beslenme alışkanlığı seçenlerin öğle yemeği oranları (%)",col=c("yellow","orange","skyblue","blue","green","darkgreen"),names.arg =c("Dört Kap","Fast food","Evden","Diğer"))
legend("topright", fill= c("yellow","orange","skyblue","blue","green","darkgreen"), c("Kadın(Grup 1)", "Kadın(Grup 2)","Erkek(Grup 1)","Erkek(Grup 2)","Toplam (Grup 1)","Toplam (Grup 2)"),cex=0.4  );

#Grup-1-2 aksam
aksam<-t(matrix(c(88,84,90.5,78,89.5,81,8,16,9.5,12,8.5,19,4,0,0,0,2,0),nrow=3,ncol=6,byrow=TRUE))
barplot(aksam,beside=TRUE,ylab="Yüzde Oranları %",main="Sağlığı etkileyen faktörlerden beslenme alışkanlığı seçenlerin akşam yemeği oranları (%)",col=c("yellow","orange","skyblue","blue","green","darkgreen"),names.arg =c("Dört Kap","Fast food","Diğer"))
legend("topright", fill= c("yellow","orange","skyblue","blue","green","darkgreen"), c("Kadın(Grup 1)", "Kadın(Grup 2)","Erkek(Grup 1)","Erkek(Grup 2)","Toplam (Grup 1)","Toplam (Grup 2)"),cex=0.5)


#Analiz 15
table((A1S$C11==1)[which=A1S$A5==1])#4KAP ÖĞLE 
table((A1S$C11==2)[which=A1S$A5==1])#FAST FOOD ÖĞLE
table(A1S$A5==1)
table((A1S$C14==1)[which=A1S$A5==1])#4KAP AKŞAM
table((A1S$C14==2)[which=A1S$A5==1])#FAST FOOD AKŞAM
#GRUP 2
table((A2S$C11==1)[which=A2S$A5==1]) #4KAP ÖĞLE
table((A2S$C11==2)[which=A2S$A5==1]) #FF ÖĞLE
table((A2S$C14==1)[which=A2S$A5==1])
table((A2S$C14==2)[which=A2S$A5==1])
table(A2S$A5==1)
prop.table(A2S$A5==1)
#Ailede şişman olmayanlar
#GRUP 1
table((A1S$C11==1)[which=A1S$A5==2])#4KAP ÖĞLE 
table((A1S$C11==2)[which=A1S$A5==2])#FAST FOOD ÖĞLE
table(A1S$A5==2)
table((A1S$C14==1)[which=A1S$A5==2])#4KAP AKŞAM
table((A1S$C14==2)[which=A1S$A5==2])#FAST FOOD AKŞAM
#GRUP 2
table((A2S$C11==1)[which=A2S$A5==2]) #4KAP ÖĞLE
table((A2S$C11==2)[which=A2S$A5==2]) #FF ÖĞLE
table((A2S$C14==1)[which=A2S$A5==2])
table((A2S$C14==2)[which=A2S$A5==2])
X=table(A2S$A5==2)
prop.test(X)

#Matrisler
G1O1<-matrix(c(9,24,11,18),nrow=2,ncol=2,byrow=TRUE)
G1A1<-matrix(c(24,55,3,4),nrow=2,ncol=2,byrow=TRUE)
G2O2<-matrix(c(10,14,3,6),nrow=2,ncol=2,byrow=TRUE)
G1A2<-matrix(c(18,32,3,8),nrow=2,ncol=2,byrow=TRUE)

#Testler
prop.test(G1O1)
chisq.test(G1O1)#Grup 1 Öğlen
chisq.test(G1A1)#Grup 1 Akşam
chisq.test(G2O2)#G2 Öğle
chisq.test(G1A2)#G2 Akşam

#Analiz 15
#Grup 1 
table(A1S$`B2-BES`)
table(A1S$`B2-VUC_A`)
table(A1S$`B2-SIGARA`)
table(A1S$`B2-ÇEVRE`)
table(A1S$`B2-FIZ_AK`)
table(A1S$`B2-ALKOL`)
table(A1S$`B2-STRES`)
table(A1S$`B2-GEN`)
table(A1S$`B2-AILE_Y`)
#en çok 3
A1S$`B2-BES`
A1S$`B2-STRES`
A1S$`B2-FIZ_AK`
#öĞLE
table((A1S$C11)[which=(A1S$`B2-BES`==1)&(A1S$`B2-STRES`==1)&(A1S$`B2-FIZ_AK`==1)])
table((A2S$C11)[which=(A2S$`B2-BES`==1)&(A2S$`B2-STRES`==1)&(A2S$`B2-FIZ_AK`==1)])
#AKŞAM
table((A1S$C14)[which=(A1S$`B2-BES`==1)&(A1S$`B2-STRES`==1)&(A1S$`B2-FIZ_AK`==1)])
table((A2S$C14)[which=(A2S$`B2-BES`==1)&(A2S$`B2-STRES`==1)&(A2S$`B2-FIZ_AK`==1)])
#Grup 2
table(A2S$`B2-BES`)
table(A2S$`B2-VUC_A`)
table(A2S$`B2-SIGARA`)
table(A2S$`B2-ÇEVRE`)
table(A2S$`B2-FIZ_AK`)
table(A2S$`B2-ALKOL`)
table(A2S$`B2-STRES`)
table(A2S$`B2-GEN`)
table(A2S$`B2-AILE_Y`)

#Matris ve çubuk grafiği için
ksef<-matrix(c(58,53,38,24,19,18,14,12,11,42,34,25,16,15,11,10,9,6),nrow = 2, ncol=9,byrow = T)
sef<-matrix(c(23.48178138,21.45748988,15.38461538,9.71659919,7.692307692,7.287449393,5.668016194,4.858299595,4.453441296,25,20.23809524,14.88095238,8.928571429,6.547619048,9.523809524,5.952380952,3.571428571,5.357142857),nrow = 2, ncol=9,byrow = T)

barplot(sef,main="Sağlığı en çok etkileyen faktörlerin yüzde oranları (%)\n(Segmentler Halinde)",ylab="Yüzde Oranları (%)",names.arg=c("Beslenme\nAlışkanlığı","Stres","Fiziksel\nAktivite","Sigara\nİçme","Çevresel\nFaktörler","Vücut\nAğırlığı","Aile Yaşam\nTarzı","Alkol\nKullanımı","Genetik\nYapı"),col=c("Blue","Red"))
legend("topright", fill= c("Blue","Red"),c("Grup 1","Grup 2"),cex=0.7)

s1=c(58,18,24,19,38,12,53,11,14)
s2=c(42,16,15,11,23,6,34,9,10)
wilcox.test(s1,s2,paired=T)

#öĞLE
#(3,3)
table((A1S$C11)[which=(A1S$`B2-BES`==1)&(A1S$`B2-STRES`==1)&(A1S$`B2-FIZ_AK`==1)])
#(3,2)
table((A1S$C11)[which=(A1S$`B2-BES`==1)&(A1S$`B2-STRES`==1)&(A1S$`B2-FIZ_AK`==2)])
table((A1S$C11)[which=(A1S$`B2-BES`==1)&(A1S$`B2-FIZ_AK`==1)&(A1S$`B2-STRES`==1)])
table((A1S$C11)[which=(A1S$`B2-FIZ_AK`==1)&(A1S$`B2-STRES`==1)])
#(3,1)
table((A1S$C11)[which=(A1S$`B2-BES`==1)])
table((A1S$C11)[which=(A1S$`B2-STRES`==1)])
table((A1S$C11)[which=(A1S$`B2-FIZ_AK`==1)])
#AKŞAM
#(3,3)
table((A1S$C14)[which=(A1S$`B2-BES`==1)&(A1S$`B2-STRES`==1)&(A1S$`B2-FIZ_AK`==1)])
#(3,2)
table((A1S$C14)[which=(A1S$`B2-BES`==1)&(A1S$`B2-STRES`==1)])
table((A1S$C14)[which=(A1S$`B2-BES`==1)&(A1S$`B2-FIZ_AK`==1)])
table((A1S$C14)[which=(A1S$`B2-FIZ_AK`==1)&(A1S$`B2-STRES`==1)])
#(3,1)
table((A1S$C14)[which=(A1S$`B2-BES`==1)])
table((A1S$C14)[which=(A1S$`B2-STRES`==1)])
table((A1S$C14)[which=(A1S$`B2-FIZ_AK`==1)])
plot((A1S$C11)[which=(A1S$`B2-BES`==1)&(A1S$`B2-STRES`==1)&(A1S$`B2-FIZ_AK`==1)
#GRUP 2
#öĞLE
#(3,3)
table((A2S$C11)[which=(A2S$`B2-BES`==1)&(A2S$`B2-STRES`==1)&(A2S$`B2-FIZ_AK`==1)])
#(3,2)
table((A2S$C11)[which=(A2S$`B2-BES`==1)&(A2S$`B2-STRES`==1)])
table((A2S$C11)[which=(A2S$`B2-BES`==1)&(A2S$`B2-FIZ_AK`==1)])
table((A2S$C11)[which=(A2S$`B2-FIZ_AK`==1)&(A2S$`B2-STRES`==1)])
#(3,1)
table((A2S$C11)[which=(A2S$`B2-BES`==1)])
table((A2S$C11)[which=(A2S$`B2-STRES`==1)])
table((A2S$C11)[which=(A2S$`B2-FIZ_AK`==1)])
#AKŞAM
#(3,3)
table((A2S$C14)[which=(A2S$`B2-BES`==1)&(A2S$`B2-STRES`==1)&(A2S$`B2-FIZ_AK`==1)])
#(3,2)
table((A2S$C14)[which=(A2S$`B2-BES`==1)&(A2S$`B2-STRES`==1)])
table((A2S$C14)[which=(A2S$`B2-BES`==1)&(A2S$`B2-FIZ_AK`==1)])
table((A2S$C14)[which=(A2S$`B2-FIZ_AK`==1)&(A2S$`B2-STRES`==1)])
#(3,1)
table((A2S$C14)[which=(A2S$`B2-BES`==1)])
table((A2S$C14)[which=(A2S$`B2-STRES`==1)])
table((A2S$C14)[which=(A2S$`B2-FIZ_AK`==1)])

#çoklu regresyon modelleri
BES1=multinom(A1S$`B2-BES`~A1S$C11+A1S$C14)

predict(BES1)
glm(A1S$`B2-BES`~A1S$C11+A1S$C14)

chisq.test((A1S$C11)([which=(A1S$`B2-BES`==1)&(A1S$`B2-STRES`==1)&(A1S$`B2-FIZ_AK`==1)]))
chisq.test((A1S$C11)[which=(A1S$`B2-BES`==1)&(A1S$`B2-STRES`==1)&(A1S$`B2-FIZ_AK`==1)])
tab=(A1S$C11)[which=(A1S$`B2-BES`==1)&(A1S$`B2-STRES`==1)&(A1S$`B2-FIZ_AK`==1)]
chisq.test(tab)

#GRUP1
BES1=multinom(A1S$`B2-BES`~A1S$C11+A1S$C14)
STRS1=multinom(A1S$`B2-STRES`~A1S$C11+A1S$C14)
FZAK1=multinom(A1S$`B2-FIZ_AK`~A1S$C11+A1S$C14)
summary(BES1)
summary(STRS1)
summary(FZAK1)
#GRUP2
BES2=multinom(A2S$`B2-BES`~A2S$C11+A2S$C14)
STRS2=multinom(A2S$`B2-STRES`~A2S$C11+A2S$C14)
FZAK2=multinom(A2S$`B2-FIZ_AK`~A2S$C11+A2S$C14)
summary(BES2)
summary(STRS2)
summary(FZAK2)
