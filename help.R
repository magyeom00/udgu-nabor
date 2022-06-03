library('shiny')
library("ggplot2")
library("XML")
#удгу
с = readLines ("http://io.udsu.ru/iias/sait.region")
с = readLines ("http://io.udsu.ru/iias/sait.region", encoding = "UTF-8")
сtree = htmlTreeParse(с, useInternalNodes = T)
y=xpathSApply(сtree, "//td[@style='font-weight:bold; color:navy']", xmlValue)
y
pa=xpathSApply(сtree, "//tr[@style='white-space: nowrap']/td", xmlValue)
pa[is.na(pa)]<-0
pa1 = strsplit (pa, "\n")

num=seq(1, length(pa1), 19)
god = as.numeric(pa1[num])
num=seq(2, length(pa1), 19)
izh1 = as.numeric(pa1[num])
num=seq(3, length(pa1), 19)
izh2 = pa1[num]
izh2 =as.numeric(sub("(.*)(.{6})", "\\1", izh2))
num=seq(4, length(pa1), 19)
sar1 = as.numeric(pa1[num])
num=seq(5, length(pa1), 19)
sar2 = pa1[num]
sar2 = as.numeric(sub("(.*)(.{6})", "\\1", sar2))
num=seq(6, length(pa1), 19)
vot1 = as.numeric(pa1[num])
num=seq(7, length(pa1), 19)
vot2 = pa1[num]
vot2 = as.numeric(sub("(.*)(.{6})", "\\1", vot2))
num=seq(8, length(pa1), 19)
moz1 = as.numeric(pa1[num])
num=seq(9, length(pa1), 19)
moz2 = pa1[num]
moz2=as.numeric(sub("(.*)(.{6})", "\\1", moz2))
num=seq(10, length(pa1), 19)
gla1 = as.numeric(pa1[num])
num=seq(11, length(pa1), 19)
gla2 = pa1[num]
gla2 = as.numeric(sub("(.*)(.{6})", "\\1", gla2))
num=seq(12, length(pa1), 19)
kam1 = as.numeric(pa1[num])
num=seq(13, length(pa1), 19)
kam2 = pa1[num]
kam2 = as.numeric(sub("(.*)(.{6})", "\\1", kam2))
num=seq(14, length(pa1), 19)
raj1 = as.numeric(pa1[num])
num=seq(15, length(pa1), 19)
raj2 = pa1[num]
raj2 = as.numeric(sub("(.*)(.{6})", "\\1", raj2))
num=seq(16, length(pa1), 19)
ost1 = as.numeric(pa1[num])
num=seq(17, length(pa1), 19)
ost2 = pa1[num]
ost2 = as.numeric(sub("(.*)(.{6})", "\\1", ost2))
num=seq(18, length(pa1), 19)
vse1 = as.numeric(pa1[num])
num=seq(19, length(pa1), 19)
vse2 = pa1[num]
vse2 = as.numeric(sub("(.*)(.{6})", "\\1", vse2))
izh = data.frame(Год = t(t(god)), 
                    Поступали_Ижевск = t(t(izh1)), Поступили_Ижевск = t(t(izh2)),
                    Поступали_Сарапул = t(t(sar1)), Поступили_Сарапул=t(t(sar2)), 
                    Поступали_Воткинкс=t(t(vot1)), Поступили_Воткинкс=t(t(vot2)),
                    Поступали_Можга=t(t(moz1)), Поступили_Можга=t(t(moz2)),
                    Поступали_Глазов=t(t(gla1)), Поступили_Глазов=t(t(gla2)),
                    Поступали_Камбарка=t(t(kam1)), Поступили_Камбарка=t(t(kam2)),
                    Поступали_Районы_Удмуртии=t(t(raj1)), Поступили_Районы_Удмуртии=t(t(raj2)),
                    Поступали_Остальное=t(t(ost1)), Поступили_Остальное=t(t(ost2)),
                    Поступали_По_УдГУ=t(t(vse1)), Поступили_По_УдГУ=t(t(vse2)))
izh=izh[order(izh$Год),]
IZH=t(izh[,1:ncol(izh)])
IZH1=izh.T[-(7:8),]
IZH2=izh1.T[-(7:8),]
write.xlsx(IZH1, "postupali.xlsx")
write.xlsx(IZH2, "postupili.xlsx")
library(openxlsx)
imap=data.frame(Год = t(t(god)),Поступили_Ижевск = t(t(izh2)),
                   Поступили_Сарапул=t(t(sar2)), Поступили_Воткинкс=t(t(vot2)),
                   Поступили_Можга=t(t(moz2)), Поступили_Глазов=t(t(gla2)),
                   Поступили_Камбарка=t(t(kam2)))
imap=imap[order(imap$Год),]
imapt <- t(imap[,2:ncol(imap)])
colnames(imapt) <- imap[,1] 


#создаем таблицу с данными о выпускниках 
np <- read.table("C:/Users/MARIN/census-app/data/выпускники.csv", sep=";")
np=np[-1,]
np=np[,-1]
np=as.numeric(np)
npp=vse1-ost1
npp=data.frame(Год=god, Поступающие_УР=npp)
npp=npp[order(npp$Год),]
npp=npp[-(1:16),]
npo=npp[,2]
rt=np-npo
we=data.frame(Год=npp[,1], Не_поступающие_УР=npo,Поступающие_УР=npp[,2])
wet <- t(we[,2:ncol(we)])
colnames(wet) <- we[,1] 
#создаем таблицу с данными о населении 
nm <- read.table("C:/Users/MARIN/census-app/data/население2.csv",
                 sep=";")
nm=nm[-1,]
nm=data.frame(nm)
names(nm)[1]='Год'
names(nm)[2]='Ижевск'
names(nm)[3]='Сарапул'
names(nm)[4]='Воткинск'
names(nm)[5]='Глазов'
names(nm)[6]='Можга'
names(nm)[7]='Камбарка'
nm1=nm[,2]
#ощищаем таблицу izh от ненужных строк,  оставляем строки только с поступающими
izhy=izh[,-(14:19)]
izhy=izhy[-(1:13),]
izhy=izhy[,-13]
izhy=izhy[,-11]
izhy=izhy[,-9]
izhy=izhy[,-7]
izhy=izhy[,-5]
izhy=izhy[,-3]
#создаем таблицу с населением и поступающими из разных городов
sd=data.frame(Год=izhy[,1], Население_Ижевска=nm[,2], Поступающие_Ижевск=izhy[,2],
                 Население_Сарапула=nm[,3], Поступающие_Сарапул=izhy[,3],
                 Население_Воткинска=nm[,4], Поступающие_Воткинск=izhy[,4],
                 Население_Глазова=nm[,5], Поступающие_Глазов=izhy[,6],
                 Население_Можга=nm[,6], Поступающие_Можга=izhy[,5],
                 Население_Камбарки=nm[,7], Поступающие_Камбарка=izhy[,7])
#транспонируем и ощищаем таблицу, оставляем строки только с поступившими
izh.T <- t(izh[,2:ncol(izh)])
colnames(izh.T) <- izh[,1] 
izh.T=izh.T[-18,]
izh.T=izh.T[-17,]
izh.T=izh.T[-16,]
izh.T=izh.T[-14,]
izh.T=izh.T[-12,]
izh.T=izh.T[-10,]
izh.T=izh.T[-8,]
izh.T=izh.T[-6,]
izh.T=izh.T[-4,]
izh.T=izh.T[-2,]
izh1.T <- t(izh[,2:ncol(izh)])
colnames(izh1.T) <- izh[,1] 
izh1.T=izh1.T[-18,]
izh1.T=izh1.T[-17,]
izh1.T=izh1.T[-15,]
izh1.T=izh1.T[-13,]
izh1.T=izh1.T[-11,]
izh1.T=izh1.T[-9,]
izh1.T=izh1.T[-7,]
izh1.T=izh1.T[-5,]
izh1.T=izh1.T[-3,]
izh1.T=izh1.T[-1,]
#вычитаем из поступающих поступивших, чтобы получить количество не поступивших
izh3=izh1-izh2
sar3=sar1-sar2
vot3=vot1-vot2
moz3=moz1-moz2
gla3=gla1-gla2
kam3=kam1-kam2
raj3=raj1-raj2
ost3=ost1-ost2
iz1=t(data.frame(Поступили=izh2, Не_поступили=izh3))
iz2=t(data.frame(Поступили=sar2, Не_поступили=sar3))
iz3=t(data.frame(Поступили=vot2, Не_поступили=vot3))
iz4=t(data.frame(Поступили=moz2, Не_поступили=moz3))
iz5=t(data.frame(Поступили=gla2, Не_поступили=gla3))
iz6=t(data.frame(Поступили=kam2, Не_поступили=kam3))
iz7=t(data.frame(Поступили=raj2, Не_поступили=raj3))
iz8=t(data.frame(Поступили=ost2, Не_поступили=ost3))
#создаем таблицу с данными о поступивших и не поступивших
iz=do.call(rbind, list(iz1,iz2,iz3,iz4,iz5,iz6,iz7,iz8))
colnames(iz) <- c(2021:1997) 

#спарсиваем данные по биолого-химическому факультету и очищаем данные
сc = readLines ("http://io.udsu.ru/iias/sait.fkt_info_region?var=6", encoding = "UTF-8")
сtreec = htmlTreeParse(сc, useInternalNodes = T)
yc=xpathSApply(сtreec, "//td[@style='font-weight:bold; color:navy']", xmlValue)
yc
pac=xpathSApply(сtreec, "//tr[@style='white-space: nowrap']/td", xmlValue)
pac=gsub("NA", "0",pac)
pa1c = strsplit (pac, "\n")

num=seq(1, length(pa1c), 19)
godc = as.numeric(pa1c[num])
num=seq(2, length(pa1c), 19)
izh1c = as.numeric(pa1c[num])
num=seq(3, length(pa1c), 19)
izh2c = pa1c[num]
izh2c =as.numeric(sub("(.*)(.{6})", "\\1", izh2c))
num=seq(4, length(pa1c), 19)
sar1c = as.numeric(pa1c[num])
num=seq(5, length(pa1c), 19)
sar2c = pa1c[num]
sar2c = as.numeric(sub("(.*)(.{6})", "\\1", sar2c))
sar2c[is.na(sar2c)]<-0
num=seq(6, length(pa1c), 19)
vot1c = as.numeric(pa1c[num])
num=seq(7, length(pa1c), 19)
vot2c = pa1c[num]
vot2c = as.numeric(sub("(.*)(.{6})", "\\1", vot2c))
vot2c[is.na(vot2c)]<-0
num=seq(8, length(pa1c), 19)
moz1c = as.numeric(pa1c[num])
num=seq(9, length(pa1c), 19)
moz2c = pa1c[num]
moz2c=as.numeric(sub("(.*)(.{6})", "\\1", moz2c))
moz2c[is.na(moz2c)]<-0
num=seq(10, length(pa1c), 19)
gla1c = as.numeric(pa1c[num])
num=seq(11, length(pa1c), 19)
gla2c = pa1c[num]
gla2c = as.numeric(sub("(.*)(.{5})", "\\1", gla2c))
gla2c[is.na(gla2c)]<-1
num=seq(12, length(pa1c), 19)
kam1c = as.numeric(pa1c[num])
num=seq(13, length(pa1c), 19)
kam2c = pa1c[num]
kam2c = as.numeric(sub("(.*)(.{6})", "\\1", kam2c))
kam2c[is.na(kam2c)]<-0
num=seq(14, length(pa1c), 19)
raj1c = as.numeric(pa1c[num])
num=seq(15, length(pa1c), 19)
raj2c = pa1c[num]
raj2c = as.numeric(sub("(.*)(.{6})", "\\1", raj2c))
num=seq(16, length(pa1c), 19)
ost1c = as.numeric(pa1c[num])
num=seq(17, length(pa1c), 19)
ost2c = pa1c[num]
ost2c = as.numeric(sub("(.*)(.{6})", "\\1", ost2c))
num=seq(18, length(pa1c), 19)
vse1c = as.numeric(pa1c[num])
num=seq(19, length(pa1c), 19)
vse2c = pa1c[num]
vse2c = as.numeric(sub("(.*)(.{6})", "\\1", vse2c))

#спарсиваем данные по колледжу высших полит наук и очищаем данные
с1 = readLines ("http://io.udsu.ru/iias/sait.fkt_info_region?var=1648", encoding = "UTF-8")
library("XML")
сtree1 = htmlTreeParse(с1, useInternalNodes = T)
y1=xpathSApply(сtree1, "//td[@style='font-weight:bold; color:navy']", xmlValue)
y1
pa1=xpathSApply(сtree1, "//tr[@style='white-space: nowrap']/td", xmlValue)
pa1=gsub("NA", "0",pa1)
pa11 = strsplit (pa1, "\n")

num=seq(1, length(pa11), 19)
god1 = as.numeric(pa11[num])
num=seq(2, length(pa11), 19)
izh11 = as.numeric(pa11[num])
num=seq(3, length(pa1), 19)
izh21 = pa11[num]
izh21 =as.numeric(sub("(.*)(.{6})", "\\1", izh21))
izh21[is.na(izh21)]<-0
num=seq(4, length(pa1), 19)
sar11 = as.numeric(pa1[num])
num=seq(5, length(pa1), 19)
sar21 = pa11[num]
sar21 = as.numeric(sub("(.*)(.{6})", "\\1", sar21))
sar21[is.na(sar21)]<-0
num=seq(6, length(pa11), 19)
vot11 = as.numeric(pa11[num])
num=seq(7, length(pa11), 19)
vot21 = pa11[num]
vot21 = as.numeric(sub("(.*)(.{6})", "\\1", vot21))
vot21[is.na(vot21)]<-0
num=seq(8, length(pa11), 19)
moz11 = as.numeric(pa11[num])
num=seq(9, length(pa11), 19)
moz21 = pa11[num]
moz21=as.numeric(sub("(.*)(.{6})", "\\1", moz21))
moz21[is.na(moz21)]<-0
num=seq(10, length(pa11), 19)
gla11 = as.numeric(pa11[num])
num=seq(11, length(pa11), 19)
gla21 = pa11[num]
gla21 = as.numeric(sub("(.*)(.{6})", "\\1", gla21))
gla21[is.na(gla21)]<-0
num=seq(12, length(pa11), 19)
kam11 = as.numeric(pa11[num])
num=seq(13, length(pa11), 19)
kam21 = pa11[num]
kam21= as.numeric(sub("(.*)(.{6})", "\\1", kam21))
num=seq(14, length(pa11), 19)
raj11 = as.numeric(pa11[num])
num=seq(15, length(pa11), 19)
raj21 = pa11[num]
raj21 = as.numeric(sub("(.*)(.{6})", "\\1", raj21))
raj21[is.na(raj21)]<-0
num=seq(16, length(pa11), 19)
ost11 = as.numeric(pa11[num])
num=seq(17, length(pa11), 19)
ost21 = pa11[num]
ost21= as.numeric(sub("(.*)(.{6})", "\\1", ost21))
ost21[is.na(ost21)]<-0
num=seq(18, length(pa11), 19)
vse11 = as.numeric(pa11[num])
num=seq(19, length(pa11), 19)
vse21 = pa11[num]
vse21 = as.numeric(sub("(.*)(.{6})", "\\1", vse21))
vse21[is.na(vse21)]<-0

#спарсиваем данные по географ факультету и очищаем данные
с2 = readLines ("http://io.udsu.ru/iias/sait.fkt_info_region?var=7", encoding = "UTF-8")
сtree2 = htmlTreeParse(с2, useInternalNodes = T)
y2=xpathSApply(сtree2, "//td[@style='font-weight:bold; color:navy']", xmlValue)
y2
pa2=xpathSApply(сtree2, "//tr[@style='white-space: nowrap']/td", xmlValue)
pa12 = strsplit (pa2, "\n")

num=seq(1, length(pa12), 19)
god2 = as.numeric(pa12[num])
num=seq(2, length(pa12), 19)
izh12 = as.numeric(pa12[num])
num=seq(3, length(pa12), 19)
izh22 = pa12[num]
izh22 =as.numeric(sub("(.*)(.{6})", "\\1", izh22))
num=seq(4, length(pa12), 19)
sar12 = as.numeric(pa12[num])
num=seq(5, length(pa12), 19)
sar22 = pa12[num]
sar22 = as.numeric(sub("(.*)(.{5})", "\\1", sar22))
sar22[is.na(sar22)]<-4
num=seq(6, length(pa12), 19)
vot12 = as.numeric(pa12[num])
num=seq(7, length(pa12), 19)
vot22 = pa12[num]
vot22 = as.numeric(sub("(.*)(.{6})", "\\1", vot22))
vot22[is.na(vot22)]<-0
num=seq(8, length(pa12), 19)
moz12 = as.numeric(pa12[num])
num=seq(9, length(pa12), 19)
moz22 = pa12[num]
moz22=as.numeric(sub("(.*)(.{6})", "\\1", moz22))
moz22[is.na(moz22)]<-0
num=seq(10, length(pa12), 19)
gla12 = as.numeric(pa12[num])
num=seq(11, length(pa12), 19)
gla22 = pa12[num]
gla22 = as.numeric(sub("(.*)(.{6})", "\\1", gla22))
gla22[is.na(gla22)]<-0
num=seq(12, length(pa12), 19)
kam12 = as.numeric(pa12[num])
num=seq(13, length(pa12), 19)
kam22 = pa12[num]
kam22 = as.numeric(sub("(.*)(.{6})", "\\1", kam22))
kam22[is.na(kam22)]<-0
num=seq(14, length(pa12), 19)
raj12 = as.numeric(pa12[num])
num=seq(15, length(pa12), 19)
raj22 = pa12[num]
raj22 = as.numeric(sub("(.*)(.{6})", "\\1", raj22))
num=seq(16, length(pa12), 19)
ost12 = as.numeric(pa12[num])
num=seq(17, length(pa12), 19)
ost22 = pa12[num]
ost22 = as.numeric(sub("(.*)(.{6})", "\\1", ost22))
ost22[is.na(ost22)]<-4
num=seq(18, length(pa12), 19)
vse12 = as.numeric(pa12[num])
num=seq(19, length(pa12), 19)
vse22 = pa12[num]
vse22= as.numeric(sub("(.*)(.{6})", "\\1", vse22))

#спарсиваем данные по институту гражданской защиты очищаем данные
с3 = readLines ("http://io.udsu.ru/iias/sait.fkt_info_region?var=1486", encoding = "UTF-8")
сtree3 = htmlTreeParse(с3, useInternalNodes = T)
y3=xpathSApply(сtree3, "//td[@style='font-weight:bold; color:navy']", xmlValue)
y3
pa3=xpathSApply(сtree3, "//tr[@style='white-space: nowrap']/td", xmlValue)
pa3=gsub("NA", "0",pa3)
pa13 = strsplit (pa3, "\n")

num=seq(1, length(pa13), 19)
god3 = as.numeric(pa13[num])
num=seq(2, length(pa13), 19)
izh13 = as.numeric(pa13[num])
num=seq(3, length(pa13), 19)
izh23 = pa13[num]
izh23 =as.numeric(sub("(.*)(.{5})", "\\1", izh23))
num=seq(4, length(pa13), 19)
sar13 = as.numeric(pa13[num])
num=seq(5, length(pa13), 19)
sar23 = pa13[num]
sar23 = as.numeric(sub("(.*)(.{5})", "\\1", sar23))
num=seq(6, length(pa13), 19)
vot13 = as.numeric(pa13[num])
num=seq(7, length(pa13), 19)
vot23 = pa13[num]
vot23 = as.numeric(sub("(.*)(.{5})", "\\1", vot23))
num=seq(8, length(pa13), 19)
moz13 = as.numeric(pa13[num])
num=seq(9, length(pa13), 19)
moz23 = pa13[num]
moz23 = as.numeric(sub("(.*)(.{5})", "\\1", moz23))
num=seq(10, length(pa13), 19)
gla13 = as.numeric(pa13[num])
num=seq(11, length(pa13), 19)
gla23 = pa13[num]
gla23 = as.numeric(sub("(.*)(.{5})", "\\1", gla23))
gla23[is.na(gla23)]<-1
num=seq(12, length(pa13), 19)
kam13 = as.numeric(pa13[num])
num=seq(13, length(pa13), 19)
kam23 = pa13[num]
kam23 = as.numeric(sub("(.*)(.{6})", "\\1", kam23))
kam23[is.na(kam23)]<-0
num=seq(14, length(pa13), 19)
raj13 = as.numeric(pa13[num])
num=seq(15, length(pa13), 19)
raj23 = pa13[num]
raj23 = as.numeric(sub("(.*)(.{6})", "\\1", raj23))
num=seq(16, length(pa13), 19)
ost13 = as.numeric(pa13[num])
num=seq(17, length(pa13), 19)
ost23 = pa13[num]
ost23 = as.numeric(sub("(.*)(.{5})", "\\1", ost23))
num=seq(18, length(pa13), 19)
vse13 = as.numeric(pa13[num])
num=seq(19, length(pa13), 19)
vse23 = pa13[num]
vse23 = as.numeric(sub("(.*)(.{5})", "\\1", vse23))

#спарсиваем данные по институту естественных наук и очищаем данные
с4 = readLines ("http://io.udsu.ru/iias/sait.fkt_info_region?var=7870", encoding = "UTF-8")
сtree4 = htmlTreeParse(с4, useInternalNodes = T)
y4=xpathSApply(сtree4, "//td[@style='font-weight:bold; color:navy']", xmlValue)
y4
pa4=xpathSApply(сtree4, "//tr[@style='white-space: nowrap']/td", xmlValue)
pa4=gsub("NA", "0",pa4)
pa14 = strsplit (pa4, "\n")

num=seq(1, length(pa14), 19)
god4 = as.numeric(pa14[num])
num=seq(2, length(pa14), 19)
izh14 = as.numeric(pa14[num])
num=seq(3, length(pa14), 19)
izh24 = pa14[num]
izh24 =as.numeric(sub("(.*)(.{6})", "\\1", izh24))
num=seq(4, length(pa14), 19)
sar14 = as.numeric(pa14[num])
num=seq(5, length(pa14), 19)
sar24 = pa14[num]
sar24 = as.numeric(sub("(.*)(.{6})", "\\1", sar24))
num=seq(6, length(pa14), 19)
vot14 = as.numeric(pa14[num])
num=seq(7, length(pa14), 19)
vot24 = pa14[num]
vot24 = as.numeric(sub("(.*)(.{6})", "\\1", vot24))
num=seq(8, length(pa14), 19)
moz14 = as.numeric(pa14[num])
num=seq(9, length(pa14), 19)
moz24 = pa14[num]
moz24 = as.numeric(sub("(.*)(.{6})", "\\1", moz24))
moz24[is.na(moz24)]<-0
num=seq(10, length(pa14), 19)
gla14 = as.numeric(pa14[num])
num=seq(11, length(pa14), 19)
gla24 = pa14[num]
gla24 = as.numeric(sub("(.*)(.{5})", "\\1", gla24))
num=seq(12, length(pa14), 19)
kam14 = as.numeric(pa14[num])
num=seq(13, length(pa14), 19)
kam24 = pa14[num]
kam24 = as.numeric(sub("(.*)(.{5})", "\\1", kam24))
num=seq(14, length(pa14), 19)
raj14 = as.numeric(pa14[num])
num=seq(15, length(pa14), 19)
raj24 = pa14[num]
raj24 = as.numeric(sub("(.*)(.{6})", "\\1", raj24))
num=seq(16, length(pa14), 19)
ost14 = as.numeric(pa14[num])
num=seq(17, length(pa14), 19)
ost24 = pa14[num]
ost24 = as.numeric(sub("(.*)(.{6})", "\\1", ost24))
num=seq(18, length(pa14), 19)
vse14 = as.numeric(pa14[num])
num=seq(19, length(pa14), 19)
vse24 = pa14[num]


vse24 = as.numeric(sub("(.*)(.{6})", "\\1", vse24))

#спарсиваем данные по институту иностранных языков и литературы и очищаем данные
с5 = readLines ("http://io.udsu.ru/iias/sait.fkt_info_region?var=4", encoding = "UTF-8")
сtree5 = htmlTreeParse(с5, useInternalNodes = T)
y5=xpathSApply(сtree5, "//td[@style='font-weight:bold; color:navy']", xmlValue)
y5
pa5=xpathSApply(сtree5, "//tr[@style='white-space: nowrap']/td", xmlValue)
pa5=gsub("NA", "0",pa5)
pa15 = strsplit (pa5, "\n")

num=seq(1, length(pa15), 19)
god5 = as.numeric(pa15[num])
num=seq(2, length(pa15), 19)
izh15 = as.numeric(pa1[num])
num=seq(3, length(pa1), 19)
izh25 = pa15[num]
izh25 =as.numeric(sub("(.*)(.{6})", "\\1", izh25))
num=seq(4, length(pa15), 19)
sar15 = as.numeric(pa15[num])
num=seq(5, length(pa15), 19)
sar25 = pa15[num]
sar25 = as.numeric(sub("(.*)(.{5})", "\\1", sar25))
num=seq(6, length(pa15), 19)
vot15 = as.numeric(pa15[num])
num=seq(7, length(pa15), 19)
vot25 = pa15[num]
vot25 = as.numeric(sub("(.*)(.{5})", "\\1", vot25))
num=seq(8, length(pa15), 19)
moz15 = as.numeric(pa15[num])
num=seq(9, length(pa15), 19)
moz25 = pa15[num]
moz25 = as.numeric(sub("(.*)(.{6})", "\\1", moz25))
moz25[is.na(moz25)]<-0
num=seq(10, length(pa15), 19)
gla15 = as.numeric(pa15[num])
num=seq(11, length(pa15), 19)
gla25 = pa15[num]
gla25 = as.numeric(sub("(.*)(.{5})", "\\1", gla25))
num=seq(12, length(pa15), 19)
kam15 = as.numeric(pa15[num])
num=seq(13, length(pa15), 19)
kam25 = pa15[num]
kam25 = as.numeric(sub("(.*)(.{6})", "\\1", kam25))
kam25[is.na(kam25)]<-0
num=seq(14, length(pa15), 19)
raj15 = as.numeric(pa15[num])
num=seq(15, length(pa15), 19)
raj25 = pa15[num]
raj25 = as.numeric(sub("(.*)(.{5})", "\\1", raj25))
num=seq(16, length(pa15), 19)
ost15 = as.numeric(pa15[num])
num=seq(17, length(pa15), 19)
ost25 = pa15[num]
ost25 = as.numeric(sub("(.*)(.{6})", "\\1", ost25))
num=seq(18, length(pa15), 19)
vse15 = as.numeric(pa15[num])
num=seq(19, length(pa15), 19)
vse25 = pa15[num]
vse25 = as.numeric(sub("(.*)(.{6})", "\\1", vse25))

#спарсиваем данные по институту искусства и дизайна и очищаем данные
с6 = readLines ("http://io.udsu.ru/iias/sait.fkt_info_region?var=5247", encoding = "UTF-8")
сtree6 = htmlTreeParse(с6, useInternalNodes = T)
y6=xpathSApply(сtree6, "//td[@style='font-weight:bold; color:navy']", xmlValue)
y6
pa6=xpathSApply(сtree6, "//tr[@style='white-space: nowrap']/td", xmlValue)
pa6=gsub("NA", "0",pa6)
pa16 = strsplit (pa6, "\n")

num=seq(1, length(pa16), 19)
god6 = as.numeric(pa16[num])
num=seq(2, length(pa16), 19)
izh16 = as.numeric(pa16[num])
num=seq(3, length(pa16), 19)
izh26 = pa16[num]
izh26 =as.numeric(sub("(.*)(.{6})", "\\1", izh26))
num=seq(4, length(pa16), 19)
sar16 = as.numeric(pa16[num])
num=seq(5, length(pa16), 19)
sar26 = pa16[num]
sar26 = as.numeric(sub("(.*)(.{6})", "\\1", sar26))
sar26[is.na(sar26)]<-0
num=seq(6, length(pa16), 19)
vot16 = as.numeric(pa16[num])
num=seq(7, length(pa16), 19)
vot26 = pa16[num]
vot26 = as.numeric(sub("(.*)(.{5})", "\\1", vot26))
vot26[is.na(vot26)]<-2
num=seq(8, length(pa16), 19)
moz16 = as.numeric(pa16[num])
num=seq(9, length(pa16), 19)
moz26 = pa16[num]
moz26 = as.numeric(sub("(.*)(.{5})", "\\1", moz26))
num=seq(10, length(pa16), 19)
gla16 = as.numeric(pa16[num])
num=seq(11, length(pa16), 19)
gla26 = pa16[num]
gla26 = as.numeric(sub("(.*)(.{5})", "\\1", gla26))
num=seq(12, length(pa16), 19)
kam16 = as.numeric(pa16[num])
num=seq(13, length(pa16), 19)
kam26 = pa16[num]
kam26 = as.numeric(sub("(.*)(.{6})", "\\1", kam26))
kam26[is.na(kam26)]<-0
num=seq(14, length(pa16), 19)
raj16 = as.numeric(pa16[num])
num=seq(15, length(pa16), 19)
raj26 = pa16[num]
raj26 = as.numeric(sub("(.*)(.{5})", "\\1", raj26))
num=seq(16, length(pa16), 19)
ost16 = as.numeric(pa16[num])
num=seq(17, length(pa16), 19)
ost26 = pa16[num]
ost26 = as.numeric(sub("(.*)(.{6})", "\\1", ost26))
num=seq(18, length(pa16), 19)
vse16= as.numeric(pa16[num])
num=seq(19, length(pa16), 19)
vse26 = pa16[num]
vse26 = as.numeric(sub("(.*)(.{6})", "\\1", vse26))

#спарсиваем данные по институту истории и социологии и очищаем данные
с7 = readLines ("http://io.udsu.ru/iias/sait.fkt_info_region?var=7872", encoding = "UTF-8")
сtree7 = htmlTreeParse(с7, useInternalNodes = T)
y7=xpathSApply(сtree7, "//td[@style='font-weight:bold; color:navy']", xmlValue)
y7
pa7=xpathSApply(сtree7, "//tr[@style='white-space: nowrap']/td", xmlValue)
pa7=gsub("NA", "0",pa7)
pa17 = strsplit (pa7, "\n")

num=seq(1, length(pa17), 19)
god7 = as.numeric(pa17[num])
num=seq(2, length(pa17), 19)
izh17 = as.numeric(pa17[num])
num=seq(3, length(pa17), 19)
izh27 = pa17[num]
izh27 =as.numeric(sub("(.*)(.{6})", "\\1", izh27))
num=seq(4, length(pa17), 19)
sar17 = as.numeric(pa17[num])
num=seq(5, length(pa17), 19)
sar27 = pa17[num]
sar27 = as.numeric(sub("(.*)(.{5})", "\\1", sar27))
num=seq(6, length(pa17), 19)
vot17 = as.numeric(pa17[num])
num=seq(7, length(pa17), 19)
vot27 = pa17[num]
vot27 = as.numeric(sub("(.*)(.{5})", "\\1", vot27))
num=seq(8, length(pa17), 19)
moz17 = as.numeric(pa17[num])
num=seq(9, length(pa17), 19)
moz27 = pa17[num]
moz27=as.numeric(sub("(.*)(.{5})", "\\1", moz27))
num=seq(10, length(pa17), 19)
gla17 = as.numeric(pa17[num])
num=seq(11, length(pa17), 19)
gla27 = pa17[num]
gla27 = as.numeric(sub("(.*)(.{5})", "\\1", gla27))
num=seq(12, length(pa17), 19)
kam17 = as.numeric(pa17[num])
num=seq(13, length(pa17), 19)
kam27 = pa17[num]
kam27 = as.numeric(sub("(.*)(.{5})", "\\1", kam27))
num=seq(14, length(pa17), 19)
raj17 = as.numeric(pa17[num])
num=seq(15, length(pa17), 19)
raj27 = pa17[num]
raj27 = as.numeric(sub("(.*)(.{6})", "\\1", raj27))
num=seq(16, length(pa17), 19)
ost17 = as.numeric(pa17[num])
num=seq(17, length(pa17), 19)
ost27 = pa17[num]
ost27 = as.numeric(sub("(.*)(.{6})", "\\1", ost27))
num=seq(18, length(pa17), 19)
vse17 = as.numeric(pa17[num])
num=seq(19, length(pa17), 19)
vse27 = pa17[num]
vse27 = as.numeric(sub("(.*)(.{6})", "\\1", vse27))


#спарсиваем данные по институту матем, информ тех и физики и очищаем данные
с8 = readLines ("http://io.udsu.ru/iias/sait.fkt_info_region?var=7873", encoding = "UTF-8")
сtree8 = htmlTreeParse(с8, useInternalNodes = T)
y8=xpathSApply(сtree8, "//td[@style='font-weight:bold; color:navy']", xmlValue)
y8
pa8=xpathSApply(сtree8, "//tr[@style='white-space: nowrap']/td", xmlValue)
pa8=gsub("NA", "0",pa8)
pa18 = strsplit (pa8, "\n")

num=seq(1, length(pa18), 19)
god = as.numeric(pa18[num])
num=seq(2, length(pa18), 19)
izh18 = as.numeric(pa18[num])
num=seq(3, length(pa18), 19)
izh28 = pa18[num]
izh28 =as.numeric(sub("(.*)(.{6})", "\\1", izh28))
num=seq(4, length(pa18), 19)
sar18 = as.numeric(pa18[num])
num=seq(5, length(pa18), 19)
sar28 = pa18[num]
sar28 = as.numeric(sub("(.*)(.{6})", "\\1", sar28))
num=seq(6, length(pa18), 19)
vot18 = as.numeric(pa18[num])
num=seq(7, length(pa18), 19)
vot28 = pa18[num]
vot28 = as.numeric(sub("(.*)(.{5})", "\\1", vot28))
num=seq(8, length(pa18), 19)
moz18 = as.numeric(pa18[num])
num=seq(9, length(pa18), 19)
moz28 = pa18[num]
moz28=as.numeric(sub("(.*)(.{6})", "\\1", moz28))
num=seq(10, length(pa18), 19)
gla18 = as.numeric(pa18[num])
num=seq(11, length(pa18), 19)
gla28 = pa18[num]
gla28 = as.numeric(sub("(.*)(.{6})", "\\1", gla28))
num=seq(12, length(pa18), 19)
kam18 = as.numeric(pa18[num])
num=seq(13, length(pa18), 19)
kam28 = pa18[num]
kam28 = as.numeric(sub("(.*)(.{6})", "\\1", kam28))
num=seq(14, length(pa18), 19)
raj18 = as.numeric(pa18[num])
num=seq(15, length(pa18), 19)
raj28 = pa18[num]
raj28 = as.numeric(sub("(.*)(.{6})", "\\1", raj28))
num=seq(16, length(pa18), 19)
ost18 = as.numeric(pa18[num])
num=seq(17, length(pa18), 19)
ost28 = pa18[num]
ost28 = as.numeric(sub("(.*)(.{6})", "\\1", ost28))
num=seq(18, length(pa18), 19)
vse18 = as.numeric(pa18[num])
num=seq(19, length(pa18), 19)
vse28 = pa18[num]
vse28 = as.numeric(sub("(.*)(.{6})", "\\1", vse28))

#спарсиваем данные по институт нефти и газа и очищаем данные
с9 = readLines ("http://io.udsu.ru/iias/sait.fkt_info_region?var=6897", encoding = "UTF-8")
сtree9 = htmlTreeParse(с9, useInternalNodes = T)
y9=xpathSApply(сtree9, "//td[@style='font-weight:bold; color:navy']", xmlValue)
y9
pa9=xpathSApply(сtree9, "//tr[@style='white-space: nowrap']/td", xmlValue)
pa9=gsub("NA", "0",pa9)
pa19 = strsplit (pa9, "\n")

num=seq(1, length(pa19), 19)
god9 = as.numeric(pa19[num])
num=seq(2, length(pa19), 19)
izh19 = as.numeric(pa19[num])
num=seq(3, length(pa19), 19)
izh29 = pa19[num]
izh29 =as.numeric(sub("(.*)(.{5})", "\\1", izh29))
num=seq(4, length(pa19), 19)
sar19 = as.numeric(pa19[num])
num=seq(5, length(pa19), 19)
sar29 = pa19[num]
sar29 = as.numeric(sub("(.*)(.{5})", "\\1", sar29))
num=seq(6, length(pa19), 19)
vot19 = as.numeric(pa19[num])
num=seq(7, length(pa19), 19)
vot29 = pa19[num]
vot29 = as.numeric(sub("(.*)(.{5})", "\\1", vot29))
num=seq(8, length(pa19), 19)
moz19 = as.numeric(pa19[num])
num=seq(9, length(pa19), 19)
moz29 = pa19[num]
moz29=as.numeric(sub("(.*)(.{5})", "\\1", moz29))
num=seq(10, length(pa19), 19)
gla19 = as.numeric(pa19[num])
num=seq(11, length(pa19), 19)
gla29 = pa19[num]
gla29 = as.numeric(sub("(.*)(.{5})", "\\1", gla29))
num=seq(12, length(pa19), 19)
kam19 = as.numeric(pa19[num])
num=seq(13, length(pa19), 19)
kam29 = pa19[num]
kam29 = as.numeric(sub("(.*)(.{5})", "\\1", kam29))
num=seq(14, length(pa19), 19)
raj19 = as.numeric(pa19[num])
num=seq(15, length(pa19), 19)
raj29 = pa19[num]
raj29 = as.numeric(sub("(.*)(.{5})", "\\1", raj29))
num=seq(16, length(pa19), 19)
ost19 = as.numeric(pa19[num])
num=seq(17, length(pa19), 19)
ost29 = pa19[num]
ost29 = as.numeric(sub("(.*)(.{5})", "\\1", ost29))
num=seq(18, length(pa19), 19)
vse19 = as.numeric(pa19[num])
num=seq(19, length(pa19), 19)
vse29 = pa19[num]
vse29 = as.numeric(sub("(.*)(.{5})", "\\1", vse29))

#спарсиваем данные по институту педагогики, психологи и соц тех и очищаем данные
с0 = readLines ("http://io.udsu.ru/iias/sait.fkt_info_region?var=8", encoding = "UTF-8")
сtree0 = htmlTreeParse(с0, useInternalNodes = T)
y0=xpathSApply(сtree0, "//td[@style='font-weight:bold; color:navy']", xmlValue)
y0
pa0=xpathSApply(сtree0, "//tr[@style='white-space: nowrap']/td", xmlValue)
pa0=gsub("NA", "0",pa0)
pa10 = strsplit (pa0, "\n")

num=seq(1, length(pa10), 19)
god0 = as.numeric(pa10[num])
num=seq(2, length(pa10), 19)
izh10 = as.numeric(pa10[num])
num=seq(3, length(pa10), 19)
izh20 = pa10[num]
izh20 =as.numeric(sub("(.*)(.{6})", "\\1", izh20))
num=seq(4, length(pa10), 19)
sar10 = as.numeric(pa10[num])
num=seq(5, length(pa10), 19)
sar20 = pa10[num]
sar20 = as.numeric(sub("(.*)(.{5})", "\\1", sar20))
sar20[is.na(sar20)]<-5
num=seq(6, length(pa10), 19)
vot10 = as.numeric(pa10[num])
num=seq(7, length(pa10), 19)
vot20 = pa10[num]
vot20 = as.numeric(sub("(.*)(.{5})", "\\1", vot20))
num=seq(8, length(pa10), 19)
moz10 = as.numeric(pa10[num])
num=seq(9, length(pa10), 19)
moz20 = pa10[num]
moz20 = as.numeric(sub("(.*)(.{5})", "\\1", moz20))
num=seq(10, length(pa10), 19)
gla10 = as.numeric(pa10[num])
num=seq(11, length(pa10), 19)
gla20 = pa10[num]
gla20 = sub("100%", "100", gla20)
gla20 = as.numeric(sub("(.*)(.{5})", "\\1", gla20))
num=seq(12, length(pa10), 19)
kam10 = as.numeric(pa10[num])
num=seq(13, length(pa10), 19)
kam20 = pa10[num]
kam20 = as.numeric(sub("(.*)(.{6})", "\\1", kam20))
kam20[is.na(kam20)]<-0
num=seq(14, length(pa10), 19)
raj10 = as.numeric(pa10[num])
num=seq(15, length(pa10), 19)
raj20 = pa10[num]
raj20 = as.numeric(sub("(.*)(.{6})", "\\1", raj20))
num=seq(16, length(pa10), 19)
ost10 = as.numeric(pa10[num])
num=seq(17, length(pa10), 19)
ost20 = pa10[num]
ost20 = as.numeric(sub("(.*)(.{6})", "\\1", ost20))
num=seq(18, length(pa10), 19)
vse10 = as.numeric(pa10[num])
num=seq(19, length(pa10), 19)
vse20 = pa10[num]
vse20 = as.numeric(sub("(.*)(.{6})", "\\1", vse20))

#спарсиваем данные по институту права, соц управ и безоп и очищаем данные
с01 = readLines ("http://io.udsu.ru/iias/sait.fkt_info_region?var=10", encoding = "UTF-8")
сtree01 = htmlTreeParse(с01, useInternalNodes = T)
y01=xpathSApply(сtree01, "//td[@style='font-weight:bold; color:navy']", xmlValue)
y01
pa01=xpathSApply(сtree01, "//tr[@style='white-space: nowrap']/td", xmlValue)
pa01=gsub("NA", "0",pa01)
pa101 = strsplit (pa01, "\n")

num=seq(1, length(pa101), 19)
god01 = as.numeric(pa101[num])
num=seq(2, length(pa101), 19)
izh101 = as.numeric(pa101[num])
num=seq(3, length(pa101), 19)
izh201 = pa101[num]
izh201 =as.numeric(sub("(.*)(.{6})", "\\1", izh201))
num=seq(4, length(pa101), 19)
sar101 = as.numeric(pa101[num])
num=seq(5, length(pa101), 19)
sar201 = pa101[num]
sar201 = as.numeric(sub("(.*)(.{6})", "\\1", sar201))
num=seq(6, length(pa101), 19)
vot101 = as.numeric(pa101[num])
num=seq(7, length(pa101), 19)
vot201 = pa101[num]
vot201 = as.numeric(sub("(.*)(.{5})", "\\1", vot201))
num=seq(8, length(pa101), 19)
moz101 = as.numeric(pa101[num])
num=seq(9, length(pa101), 19)
moz201 = pa101[num]
moz201 =as.numeric(sub("(.*)(.{5})", "\\1", moz201))
num=seq(10, length(pa101), 19)
gla101 = as.numeric(pa101[num])
num=seq(11, length(pa101), 19)
gla201 = pa101[num]
gla201 = as.numeric(sub("(.*)(.{5})", "\\1", gla201))
num=seq(12, length(pa101), 19)
kam101 = as.numeric(pa101[num])
num=seq(13, length(pa101), 19)
kam201 = pa101[num]
kam201 = sub("100%", "100", kam201)
kam201 = as.numeric(sub("(.*)(.{5})", "\\1", kam201))
num=seq(14, length(pa101), 19)
raj101 = as.numeric(pa101[num])
num=seq(15, length(pa101), 19)
raj201 = pa101[num]
raj201 = as.numeric(sub("(.*)(.{6})", "\\1", raj201))
num=seq(16, length(pa101), 19)
ost101 = as.numeric(pa101[num])
num=seq(17, length(pa101), 19)
ost201 = pa101[num]
ost201 = as.numeric(sub("(.*)(.{6})", "\\1", ost201))
num=seq(18, length(pa101), 19)
vse101 = as.numeric(pa101[num])
num=seq(19, length(pa101), 19)
vse201 = pa101[num]
vse201 = as.numeric(sub("(.*)(.{6})", "\\1", vse201))

#спарсиваем данные по институту соц коммуникаций и очищаем данные
с02 = readLines ("http://io.udsu.ru/iias/sait.fkt_info_region?var=17", encoding = "UTF-8")
сtree02 = htmlTreeParse(с02, useInternalNodes = T)
y02=xpathSApply(сtree02, "//td[@style='font-weight:bold; color:navy']", xmlValue)
y02
pa02=xpathSApply(сtree02, "//tr[@style='white-space: nowrap']/td", xmlValue)
pa02=gsub("NA", "0",pa02)
pa102 = strsplit (pa02, "\n")

num=seq(1, length(pa102), 19)
god02 = as.numeric(pa102[num])
num=seq(2, length(pa102), 19)
izh102 = as.numeric(pa102[num])
num=seq(3, length(pa102), 19)
izh202 = pa102[num]
izh202 =as.numeric(sub("(.*)(.{6})", "\\1", izh202))
num=seq(4, length(pa102), 19)
sar102 = as.numeric(pa102[num])
num=seq(5, length(pa102), 19)
sar202 = pa102[num]
sar202 = as.numeric(sub("(.*)(.{5})", "\\1", sar202))
num=seq(6, length(pa102), 19)
vot102 = as.numeric(pa102[num])
num=seq(7, length(pa102), 19)
vot202 = pa102[num]
vot202 = as.numeric(sub("(.*)(.{5})", "\\1", vot202))
num=seq(8, length(pa102), 19)
moz102 = as.numeric(pa102[num])
num=seq(9, length(pa102), 19)
moz202 = pa102[num]
moz202 = sub("100%", "100", moz202)
moz202 =as.numeric(sub("(.*)(.{5})", "\\1", moz202))
num=seq(10, length(pa102), 19)
gla102 = as.numeric(pa102[num])
num=seq(11, length(pa102), 19)
gla202 = pa102[num]
gla202 = as.numeric(sub("(.*)(.{5})", "\\1", gla202))
num=seq(12, length(pa102), 19)
kam102 = as.numeric(pa102[num])
num=seq(13, length(pa102), 19)
kam202 = pa102[num]
kam202 = sub("100%", "100", kam202)
kam202 = as.numeric(sub("(.*)(.{5})", "\\1", kam202))
num=seq(14, length(pa102), 19)
raj102 = as.numeric(pa102[num])
num=seq(15, length(pa102), 19)
raj202 = pa102[num]
raj202 = as.numeric(sub("(.*)(.{6})", "\\1", raj202))
num=seq(16, length(pa102), 19)
ost102 = as.numeric(pa102[num])
num=seq(17, length(pa102), 19)
ost202 = pa102[num]
ost202 = as.numeric(sub("(.*)(.{5})", "\\1", ost202))
num=seq(18, length(pa102), 19)
vse102 = as.numeric(pa102[num])
num=seq(19, length(pa102), 19)
vse202 = pa102[num]
vse202 = as.numeric(sub("(.*)(.{6})", "\\1", vse202))

#спарсиваем данные по институту удм фил, финно-угрев и журналистики и очищаем данные
с03 = readLines ("http://io.udsu.ru/iias/sait.fkt_info_region?var=7869", encoding = "UTF-8")
сtree03 = htmlTreeParse(с03, useInternalNodes = T)
y03=xpathSApply(сtree03, "//td[@style='font-weight:bold; color:navy']", xmlValue)
y03
pa03=xpathSApply(сtree03, "//tr[@style='white-space: nowrap']/td", xmlValue)
pa03 = sub("100%", "100", pa03)
pa103 = strsplit (pa03, "\n")

num=seq(1, length(pa103), 19)
god03 = as.numeric(pa103[num])
num=seq(2, length(pa103), 19)
izh103 = as.numeric(pa103[num])
num=seq(3, length(pa103), 19)
izh203 = pa103[num]
izh203 =as.numeric(sub("(.*)(.{5})", "\\1", izh203))
num=seq(4, length(pa103), 19)
sar103 = as.numeric(pa103[num])
num=seq(5, length(pa103), 19)
sar203 = pa103[num]
sar203 = as.numeric(sub("(.*)(.{5})", "\\1", sar203))
num=seq(6, length(pa103), 19)
vot103 = as.numeric(pa103[num])
num=seq(7, length(pa103), 19)
vot203 = pa103[num]
vot203 = as.numeric(sub("(.*)(.{5})", "\\1", vot203))
num=seq(8, length(pa103), 19)
moz103 = as.numeric(pa103[num])
num=seq(9, length(pa103), 19)
moz203 = pa103[num]
moz203 =as.numeric(sub("(.*)(.{5})", "\\1", moz203))
num=seq(10, length(pa103), 19)
gla103 = as.numeric(pa103[num])
num=seq(11, length(pa103), 19)
gla203 = pa103[num]
gla203 = as.numeric(sub("(.*)(.{5})", "\\1", gla203))
num=seq(12, length(pa103), 19)
kam103 = as.numeric(pa103[num])
num=seq(13, length(pa103), 19)
kam203 = pa103[num]
kam203 = as.numeric(sub("(.*)(.{5})", "\\1", kam203))
num=seq(14, length(pa103), 19)
raj103 = as.numeric(pa103[num])
num=seq(15, length(pa103), 19)
raj203 = pa103[num]
raj203 = as.numeric(sub("(.*)(.{5})", "\\1", raj203))
num=seq(16, length(pa103), 19)
ost103 = as.numeric(pa103[num])
num=seq(17, length(pa103), 19)
ost203 = pa103[num]
ost203 = as.numeric(sub("(.*)(.{5})", "\\1", ost203))
num=seq(18, length(pa103), 19)
vse103 = as.numeric(pa103[num])
num=seq(19, length(pa103), 19)
vse203 = pa103[num]
vse203 = as.numeric(sub("(.*)(.{5})", "\\1", vse203))


#спарсиваем данные по институту физ кул и спорта и очищаем данные
с04 = readLines ("http://io.udsu.ru/iias/sait.fkt_info_region?var=7874", encoding = "UTF-8")
сtree04 = htmlTreeParse(с04, useInternalNodes = T)
y04=xpathSApply(сtree04, "//td[@style='font-weight:bold; color:navy']", xmlValue)
y04
pa04=xpathSApply(сtree04, "//tr[@style='white-space: nowrap']/td", xmlValue)
pa04 = sub("100%", "100", pa04)
pa104 = strsplit (pa0, "\n")

num=seq(1, length(pa104), 19)
god04 = as.numeric(pa104[num])
num=seq(2, length(pa104), 19)
izh104 = as.numeric(pa104[num])
num=seq(3, length(pa104), 19)
izh204 = pa104[num]
izh204 =as.numeric(sub("(.*)(.{5})", "\\1", izh204))
num=seq(4, length(pa104), 19)
sar104 = as.numeric(pa104[num])
num=seq(5, length(pa104), 19)
sar204 = pa104[num]
sar204 = as.numeric(sub("(.*)(.{5})", "\\1", sar204))
num=seq(6, length(pa104), 19)
vot104 = as.numeric(pa104[num])
num=seq(7, length(pa104), 19)
vot204 = pa104[num]
vot204 = as.numeric(sub("(.*)(.{5})", "\\1", vot204))
num=seq(8, length(pa104), 19)
moz104 = as.numeric(pa104[num])
num=seq(9, length(pa104), 19)
moz204 = pa104[num]
moz204 =as.numeric(sub("(.*)(.{5})", "\\1", moz204))
num=seq(10, length(pa104), 19)
gla104 = as.numeric(pa104[num])
num=seq(11, length(pa104), 19)
gla204 = pa104[num]
gla204 = as.numeric(sub("(.*)(.{5})", "\\1", gla204))
num=seq(12, length(pa104), 19)
kam104 = as.numeric(pa104[num])
num=seq(13, length(pa104), 19)
kam204 = pa104[num]
kam204 = as.numeric(sub("(.*)(.{5})", "\\1", kam204))
num=seq(14, length(pa104), 19)
raj104 = as.numeric(pa104[num])
num=seq(15, length(pa104), 19)
raj204 = pa104[num]
raj204 = as.numeric(sub("(.*)(.{5})", "\\1", raj204))
num=seq(16, length(pa104), 19)
ost104 = as.numeric(pa104[num])
num=seq(17, length(pa104), 19)
ost204 = pa104[num]
ost204 = as.numeric(sub("(.*)(.{5})", "\\1", ost204))
num=seq(18, length(pa104), 19)
vse104 = as.numeric(pa104[num])
num=seq(19, length(pa104), 19)
vse204 = pa104[num]
vse204 = as.numeric(sub("(.*)(.{5})", "\\1", vse204))

#спарсиваем данные по институту экономики и упрвления и очищаем данные
с05 = readLines ("http://io.udsu.ru/iias/sait.fkt_info_region?var=11", encoding = "UTF-8")
сtree05 = htmlTreeParse(с05, useInternalNodes = T)
y05=xpathSApply(сtree05, "//td[@style='font-weight:bold; color:navy']", xmlValue)
y05
pa05=xpathSApply(сtree05, "//tr[@style='white-space: nowrap']/td", xmlValue)
pa05 = sub("100%", "100", pa05)
pa105 = strsplit (pa05, "\n")

num=seq(1, length(pa105), 19)
god05 = as.numeric(pa105[num])
num=seq(2, length(pa105), 19)
izh105 = as.numeric(pa105[num])
num=seq(3, length(pa105), 19)
izh205 = pa105[num]
izh205 =as.numeric(sub("(.*)(.{5})", "\\1", izh205))
num=seq(4, length(pa105), 19)
sar105 = as.numeric(pa105[num])
num=seq(5, length(pa105), 19)
sar205 = pa105[num]
sar205 = as.numeric(sub("(.*)(.{5})", "\\1", sar205))
num=seq(6, length(pa105), 19)
vot105 = as.numeric(pa105[num])
num=seq(7, length(pa105), 19)
vot205 = pa105[num]
vot205 = as.numeric(sub("(.*)(.{5})", "\\1", vot205))
num=seq(8, length(pa105), 19)
moz105 = as.numeric(pa105[num])
num=seq(9, length(pa105), 19)
moz205 = pa105[num]
moz205 =as.numeric(sub("(.*)(.{5})", "\\1", moz205))
num=seq(10, length(pa105), 19)
gla105 = as.numeric(pa105[num])
num=seq(11, length(pa105), 19)
gla205 = pa105[num]
gla205 = as.numeric(sub("(.*)(.{5})", "\\1", gla205))
num=seq(12, length(pa105), 19)
kam105 = as.numeric(pa105[num])
num=seq(13, length(pa105), 19)
kam205 = pa105[num]
kam205 = as.numeric(sub("(.*)(.{5})", "\\1", kam205))
num=seq(14, length(pa105), 19)
raj105 = as.numeric(pa105[num])
num=seq(15, length(pa105), 19)
raj205 = pa105[num]
raj205 = as.numeric(sub("(.*)(.{5})", "\\1", raj205))
num=seq(16, length(pa105), 19)
ost105 = as.numeric(pa105[num])
num=seq(17, length(pa105), 19)
ost205 = pa105[num]
ost205 = as.numeric(sub("(.*)(.{5})", "\\1", ost205))
num=seq(18, length(pa105), 19)
vse105 = as.numeric(pa105[num])
num=seq(19, length(pa105), 19)
vse205 = pa105[num]
vse205 = as.numeric(sub("(.*)(.{5})", "\\1", vse205))

#спарсиваем данные по институту языка и литературы и очищаем данные
с06 = readLines ("http://io.udsu.ru/iias/sait.fkt_info_region?var=7871", encoding = "UTF-8")
сtree06 = htmlTreeParse(с06, useInternalNodes = T)
y06=xpathSApply(сtree06, "//td[@style='font-weight:bold; color:navy']", xmlValue)
y06
pa06=xpathSApply(сtree06, "//tr[@style='white-space: nowrap']/td", xmlValue)
pa06 = sub("100%", "100", pa06)
pa106 = strsplit (pa06, "\n")

num=seq(1, length(pa106), 19)
god06 = as.numeric(pa106[num])
num=seq(2, length(pa106), 19)
izh106 = as.numeric(pa106[num])
num=seq(3, length(pa106), 19)
izh206 = pa106[num]
izh206 =as.numeric(sub("(.*)(.{5})", "\\1", izh206))
num=seq(4, length(pa106), 19)
sar106 = as.numeric(pa106[num])
num=seq(5, length(pa106), 19)
sar206 = pa106[num]
sar206 = as.numeric(sub("(.*)(.{5})", "\\1", sar206))
num=seq(6, length(pa106), 19)
vot106 = as.numeric(pa106[num])
num=seq(7, length(pa106), 19)
vot206 = pa106[num]
vot206 = as.numeric(sub("(.*)(.{5})", "\\1", vot206))
num=seq(8, length(pa106), 19)
moz106 = as.numeric(pa106[num])
num=seq(9, length(pa106), 19)
moz206 = pa106[num]
moz206 =as.numeric(sub("(.*)(.{5})", "\\1", moz206))
num=seq(10, length(pa106), 19)
gla106 = as.numeric(pa106[num])
num=seq(11, length(pa106), 19)
gla206 = pa106[num]
gla206 = as.numeric(sub("(.*)(.{5})", "\\1", gla206))
num=seq(12, length(pa106), 19)
kam106 = as.numeric(pa106[num])
num=seq(13, length(pa106), 19)
kam206 = pa106[num]
kam206 = as.numeric(sub("(.*)(.{5})", "\\1", kam206))
num=seq(14, length(pa106), 19)
raj106 = as.numeric(pa106[num])
num=seq(15, length(pa106), 19)
raj206 = pa106[num]
raj206 = as.numeric(sub("(.*)(.{5})", "\\1", raj206))
num=seq(16, length(pa106), 19)
ost106 = as.numeric(pa106[num])
num=seq(17, length(pa106), 19)
ost206 = pa106[num]
ost206 = as.numeric(sub("(.*)(.{5})", "\\1", ost206))
num=seq(18, length(pa106), 19)
vse106 = as.numeric(pa106[num])
num=seq(19, length(pa106), 19)
vse206 = pa106[num]
vse206 = as.numeric(sub("(.*)(.{5})", "\\1", vse206))

#спарсиваем данные по историческому факультету и очищаем данные
с07 = readLines ("http://io.udsu.ru/iias/sait.fkt_info_region?var=1", encoding = "UTF-8")
сtree07 = htmlTreeParse(с07, useInternalNodes = T)
y07=xpathSApply(сtree07, "//td[@style='font-weight:bold; color:navy']", xmlValue)
y07
pa07=xpathSApply(сtree07, "//tr[@style='white-space: nowrap']/td", xmlValue)
pa07 = sub("100%", "100", pa07)
pa107 = strsplit (pa07, "\n")

num=seq(1, length(pa107), 19)
god07 = as.numeric(pa107[num])
num=seq(2, length(pa107), 19)
izh107 = as.numeric(pa107[num])
num=seq(3, length(pa107), 19)
izh207 = pa107[num]
izh207 =as.numeric(sub("(.*)(.{5})", "\\1", izh207))
num=seq(4, length(pa107), 19)
sar107 = as.numeric(pa107[num])
num=seq(5, length(pa107), 19)
sar207 = pa107[num]
sar207 = as.numeric(sub("(.*)(.{5})", "\\1", sar207))
num=seq(6, length(pa107), 19)
vot107 = as.numeric(pa107[num])
num=seq(7, length(pa107), 19)
vot207 = pa107[num]
vot207 = as.numeric(sub("(.*)(.{5})", "\\1", vot207))
num=seq(8, length(pa107), 19)
moz107 = as.numeric(pa107[num])
num=seq(9, length(pa107), 19)
moz207 = pa107[num]
moz207 =as.numeric(sub("(.*)(.{5})", "\\1", moz207))
num=seq(10, length(pa107), 19)
gla107 = as.numeric(pa107[num])
num=seq(11, length(pa107), 19)
gla207 = pa107[num]
gla207 = as.numeric(sub("(.*)(.{5})", "\\1", gla207))
num=seq(12, length(pa107), 19)
kam107 = as.numeric(pa107[num])
num=seq(13, length(pa107), 19)
kam207 = pa107[num]
kam207 = as.numeric(sub("(.*)(.{5})", "\\1", kam207))
num=seq(14, length(pa107), 19)
raj107 = as.numeric(pa107[num])
num=seq(15, length(pa107), 19)
raj207 = pa107[num]
raj207 = as.numeric(sub("(.*)(.{5})", "\\1", raj207))
num=seq(16, length(pa107), 19)
ost107 = as.numeric(pa107[num])
num=seq(17, length(pa107), 19)
ost207 = pa107[num]
ost207 = as.numeric(sub("(.*)(.{5})", "\\1", ost207))
num=seq(18, length(pa107), 19)
vse107 = as.numeric(pa107[num])
num=seq(19, length(pa107), 19)
vse207 = pa107[num]
vse207 = as.numeric(sub("(.*)(.{5})", "\\1", vse207))

#спарсиваем данные по матем факультету и очищаем данные
с08 = readLines ("http://io.udsu.ru/iias/sait.fkt_info_region?var=9", encoding = "UTF-8")
сtree08 = htmlTreeParse(с08, useInternalNodes = T)
y08=xpathSApply(сtree08, "//td[@style='font-weight:bold; color:navy']", xmlValue)
y08
pa08=xpathSApply(сtree08, "//tr[@style='white-space: nowrap']/td", xmlValue)
pa08 = sub("100%", "100", pa08)
pa108 = strsplit (pa08, "\n")

num=seq(1, length(pa108), 19)
god08 = as.numeric(pa108[num])
num=seq(2, length(pa108), 19)
izh108 = as.numeric(pa108[num])
num=seq(3, length(pa108), 19)
izh208 = pa108[num]
izh208 =as.numeric(sub("(.*)(.{5})", "\\1", izh208))
num=seq(4, length(pa108), 19)
sar108 = as.numeric(pa108[num])
num=seq(5, length(pa108), 19)
sar208 = pa108[num]
sar208 = as.numeric(sub("(.*)(.{5})", "\\1", sar208))
num=seq(6, length(pa108), 19)
vot108 = as.numeric(pa108[num])
num=seq(7, length(pa108), 19)
vot208 = pa108[num]
vot208 = as.numeric(sub("(.*)(.{5})", "\\1", vot208))
num=seq(8, length(pa108), 19)
moz108 = as.numeric(pa108[num])
num=seq(9, length(pa108), 19)
moz208 = pa108[num]
moz208 =as.numeric(sub("(.*)(.{5})", "\\1", moz208))
num=seq(10, length(pa108), 19)
gla108 = as.numeric(pa108[num])
num=seq(11, length(pa108), 19)
gla208 = pa108[num]
gla208 = as.numeric(sub("(.*)(.{5})", "\\1", gla208))
num=seq(12, length(pa108), 19)
kam108 = as.numeric(pa108[num])
num=seq(13, length(pa108), 19)
kam208 = pa108[num]
kam208 = as.numeric(sub("(.*)(.{5})", "\\1", kam208))
num=seq(14, length(pa108), 19)
raj108 = as.numeric(pa108[num])
num=seq(15, length(pa108), 19)
raj208 = pa108[num]
raj208 = as.numeric(sub("(.*)(.{5})", "\\1", raj208))
num=seq(16, length(pa108), 19)
ost108 = as.numeric(pa108[num])
num=seq(17, length(pa108), 19)
ost208 = pa108[num]
ost208 = as.numeric(sub("(.*)(.{5})", "\\1", ost208))
num=seq(18, length(pa108), 19)
vse108 = as.numeric(pa108[num])
num=seq(19, length(pa108), 19)
vse208 = pa108[num]
vse208 = as.numeric(sub("(.*)(.{5})", "\\1", vse208))

#спарсиваем данные по нефтяному факультету и очищаем данные
с09 = readLines ("http://io.udsu.ru/iias/sait.fkt_info_region?var=15", encoding = "UTF-8")
сtree09 = htmlTreeParse(с09, useInternalNodes = T)
y09=xpathSApply(сtree09, "//td[@style='font-weight:bold; color:navy']", xmlValue)
y09
pa09=xpathSApply(сtree09, "//tr[@style='white-space: nowrap']/td", xmlValue)
pa09 = sub("100%", "100", pa09)
pa109 = strsplit (pa09, "\n")

num=seq(1, length(pa109), 19)
god09 = as.numeric(pa109[num])
num=seq(2, length(pa109), 19)
izh109 = as.numeric(pa109[num])
num=seq(3, length(pa109), 19)
izh209 = pa109[num]
izh209 =as.numeric(sub("(.*)(.{5})", "\\1", izh209))
num=seq(4, length(pa109), 19)
sar109 = as.numeric(pa109[num])
num=seq(5, length(pa109), 19)
sar209 = pa109[num]
sar209 = as.numeric(sub("(.*)(.{5})", "\\1", sar209))
num=seq(6, length(pa109), 19)
vot109 = as.numeric(pa109[num])
num=seq(7, length(pa109), 19)
vot209 = pa109[num]
vot209 = as.numeric(sub("(.*)(.{5})", "\\1", vot209))
num=seq(8, length(pa109), 19)
moz109 = as.numeric(pa109[num])
num=seq(9, length(pa109), 19)
moz209 = pa109[num]
moz209 =as.numeric(sub("(.*)(.{5})", "\\1", moz209))
num=seq(10, length(pa109), 19)
gla109 = as.numeric(pa109[num])
num=seq(11, length(pa109), 19)
gla209 = pa109[num]
gla209 = as.numeric(sub("(.*)(.{5})", "\\1", gla209))
num=seq(12, length(pa109), 19)
kam109 = as.numeric(pa109[num])
num=seq(13, length(pa109), 19)
kam209 = pa109[num]
kam209 = as.numeric(sub("(.*)(.{5})", "\\1", kam209))
num=seq(14, length(pa109), 19)
raj109 = as.numeric(pa109[num])
num=seq(15, length(pa109), 19)
raj209 = pa109[num]
raj209 = as.numeric(sub("(.*)(.{5})", "\\1", raj209))
num=seq(16, length(pa109), 19)
ost109 = as.numeric(pa109[num])
num=seq(17, length(pa109), 19)
ost209 = pa109[num]
ost209 = as.numeric(sub("(.*)(.{5})", "\\1", ost209))
num=seq(18, length(pa109), 19)
vse109 = as.numeric(pa109[num])
num=seq(19, length(pa109), 19)
vse209 = pa109[num]
vse209 = as.numeric(sub("(.*)(.{5})", "\\1", vse209))

#спарсиваем данные по факультету журналистики и очищаем данные
с00 = readLines ("http://io.udsu.ru/iias/sait.fkt_info_region?var=19", encoding = "UTF-8")
сtree00 = htmlTreeParse(с00, useInternalNodes = T)
y00=xpathSApply(сtree00, "//td[@style='font-weight:bold; color:navy']", xmlValue)
y00
pa00=xpathSApply(сtree00, "//tr[@style='white-space: nowrap']/td", xmlValue)
pa00 = sub("100%", "100", pa00)
pa100 = strsplit (pa00, "\n")

num=seq(1, length(pa100), 19)
god00 = as.numeric(pa100[num])
num=seq(2, length(pa100), 19)
izh100 = as.numeric(pa100[num])
num=seq(3, length(pa100), 19)
izh200 = pa100[num]
izh200 =as.numeric(sub("(.*)(.{5})", "\\1", izh200))
num=seq(4, length(pa100), 19)
sar100 = as.numeric(pa100[num])
num=seq(5, length(pa100), 19)
sar200 = pa100[num]
sar200 = as.numeric(sub("(.*)(.{5})", "\\1", sar200))
num=seq(6, length(pa100), 19)
vot100 = as.numeric(pa10[num])
num=seq(7, length(pa100), 19)
vot200 = pa100[num]
vot200 = as.numeric(sub("(.*)(.{5})", "\\1", vot200))
num=seq(8, length(pa100), 19)
moz100 = as.numeric(pa100[num])
num=seq(9, length(pa100), 19)
moz200 = pa100[num]
moz200 =as.numeric(sub("(.*)(.{5})", "\\1", moz200))
num=seq(10, length(pa100), 19)
gla100 = as.numeric(pa100[num])
num=seq(11, length(pa100), 19)
gla200 = pa100[num]
gla200 = as.numeric(sub("(.*)(.{5})", "\\1", gla200))
num=seq(12, length(pa100), 19)
kam100 = as.numeric(pa100[num])
num=seq(13, length(pa100), 19)
kam200 = pa10[num]
kam200 = as.numeric(sub("(.*)(.{5})", "\\1", kam200))
num=seq(14, length(pa100), 19)
raj100 = as.numeric(pa100[num])
num=seq(15, length(pa100), 19)
raj200 = pa100[num]
raj200 = as.numeric(sub("(.*)(.{5})", "\\1", raj200))
num=seq(16, length(pa100), 19)
ost100 = as.numeric(pa100[num])
num=seq(17, length(pa100), 19)
ost200 = pa100[num]
ost200 = as.numeric(sub("(.*)(.{5})", "\\1", ost200))
num=seq(18, length(pa100), 19)
vse100 = as.numeric(pa100[num])
num=seq(19, length(pa100), 19)
vse200 = pa100[num]
vse200 = as.numeric(sub("(.*)(.{5})", "\\1", vse200))

#спарсиваем данные по факультету инф тех и выч техники и очищаем данные
с10 = readLines ("http://io.udsu.ru/iias/sait.fkt_info_region?var=5554", encoding = "UTF-8")
сtree10 = htmlTreeParse(с10, useInternalNodes = T)
y10=xpathSApply(сtree10, "//td[@style='font-weight:bold; color:navy']", xmlValue)
y10
pa110=xpathSApply(сtree10, "//tr[@style='white-space: nowrap']/td", xmlValue)
pa110 = sub("100%", "100", pa110)
pa110 = strsplit (pa110, "\n")

num=seq(1, length(pa110), 19)
god10 = as.numeric(pa110[num])
num=seq(2, length(pa110), 19)
izh110 = as.numeric(pa110[num])
num=seq(3, length(pa110), 19)
izh210 = pa110[num]
izh210 =as.numeric(sub("(.*)(.{5})", "\\1", izh210))
num=seq(4, length(pa110), 19)
sar110 = as.numeric(pa110[num])
num=seq(5, length(pa110), 19)
sar210 = pa110[num]
sar210 = as.numeric(sub("(.*)(.{5})", "\\1", sar210))
num=seq(6, length(pa110), 19)
vot110 = as.numeric(pa110[num])
num=seq(7, length(pa110), 19)
vot210 = pa10[num]
vot210 = as.numeric(sub("(.*)(.{5})", "\\1", vot210))
num=seq(8, length(pa110), 19)
moz110 = as.numeric(pa110[num])
num=seq(9, length(pa110), 19)
moz210 = pa10[num]
moz210 =as.numeric(sub("(.*)(.{5})", "\\1", moz210))
num=seq(10, length(pa110), 19)
gla110 = as.numeric(pa110[num])
num=seq(11, length(pa110), 19)
gla210 = pa110[num]
gla210 = as.numeric(sub("(.*)(.{5})", "\\1", gla210))
num=seq(12, length(pa110), 19)
kam110 = as.numeric(pa110[num])
num=seq(13, length(pa110), 19)
kam210 = pa110[num]
kam210 = as.numeric(sub("(.*)(.{5})", "\\1", kam210))
num=seq(14, length(pa110), 19)
raj110 = as.numeric(pa110[num])
num=seq(15, length(pa110), 19)
raj210 = pa110[num]
raj210 = as.numeric(sub("(.*)(.{5})", "\\1", raj210))
num=seq(16, length(pa110), 19)
ost110 = as.numeric(pa110[num])
num=seq(17, length(pa110), 19)
ost210 = pa110[num]
ost210 = as.numeric(sub("(.*)(.{5})", "\\1", ost210))
num=seq(18, length(pa110), 19)
vse110 = as.numeric(pa110[num])
num=seq(19, length(pa110), 19)
vse210 = pa110[num]
vse210 = as.numeric(sub("(.*)(.{5})", "\\1", vse210))

#спарсиваем данные по факультету медицинской биотехнологии и очищаем данные
с11 = readLines ("http://io.udsu.ru/iias/sait.fkt_info_region?var=16", encoding = "UTF-8")
сtree11 = htmlTreeParse(с11, useInternalNodes = T)
y11=xpathSApply(сtree11, "//td[@style='font-weight:bold; color:navy']", xmlValue)
y11
pa11=xpathSApply(сtree11, "//tr[@style='white-space: nowrap']/td", xmlValue)
pa11 = sub("100%", "100", pa11)
pa111 = strsplit (pa11, "\n")

num=seq(1, length(pa111), 19)
god11 = as.numeric(pa111[num])
num=seq(2, length(pa111), 19)
izh111 = as.numeric(pa111[num])
num=seq(3, length(pa111), 19)
izh211 = pa111[num]
izh211 =as.numeric(sub("(.*)(.{5})", "\\1", izh211))
num=seq(4, length(pa111), 19)
sar111 = as.numeric(pa111[num])
num=seq(5, length(pa111), 19)
sar211 = pa111[num]
sar211 = as.numeric(sub("(.*)(.{5})", "\\1", sar211))
num=seq(6, length(pa111), 19)
vot111 = as.numeric(pa111[num])
num=seq(7, length(pa111), 19)
vot211 = pa10[num]
vot211 = as.numeric(sub("(.*)(.{5})", "\\1", vot211))
num=seq(8, length(pa111), 19)
moz111 = as.numeric(pa111[num])
num=seq(9, length(pa111), 19)
moz211 = pa10[num]
moz211 =as.numeric(sub("(.*)(.{5})", "\\1", moz211))
num=seq(10, length(pa111), 19)
gla111 = as.numeric(pa111[num])
num=seq(11, length(pa111), 19)
gla211 = pa10[num]
gla211 = as.numeric(sub("(.*)(.{5})", "\\1", gla211))
num=seq(12, length(pa111), 19)
kam111 = as.numeric(pa111[num])
num=seq(13, length(pa111), 19)
kam211 = pa111[num]
kam211 = as.numeric(sub("(.*)(.{5})", "\\1", kam211))
num=seq(14, length(pa111), 19)
raj111 = as.numeric(pa111[num])
num=seq(15, length(pa111), 19)
raj211 = pa10[num]
raj211 = as.numeric(sub("(.*)(.{5})", "\\1", raj211))
num=seq(16, length(pa10), 19)
ost111 = as.numeric(pa10[num])
num=seq(17, length(pa10), 19)
ost211 = pa111[num]
ost211 = as.numeric(sub("(.*)(.{5})", "\\1", ost211))
num=seq(18, length(pa111), 19)
vse111 = as.numeric(pa111[num])
num=seq(19, length(pa111), 19)
vse211 = pa111[num]
vse211 = as.numeric(sub("(.*)(.{5})", "\\1", vse211))

#спарсиваем данные по факультету проф иностранного языка и очищаем данные
с12 = readLines ("http://io.udsu.ru/iias/sait.fkt_info_region?var=22", encoding = "UTF-8")
сtree12 = htmlTreeParse(с12, useInternalNodes = T)
y12=xpathSApply(сtree12, "//td[@style='font-weight:bold; color:navy']", xmlValue)
y12
pa12=xpathSApply(сtree12, "//tr[@style='white-space: nowrap']/td", xmlValue)
pa12 = sub("100%", "100", pa12)
pa112 = strsplit (pa12, "\n")

num=seq(1, length(pa112), 19)
god12 = as.numeric(pa112[num])
num=seq(2, length(pa112), 19)
izh112 = as.numeric(pa112[num])
num=seq(3, length(pa112), 19)
izh212 = pa10[num]
izh212 =as.numeric(sub("(.*)(.{5})", "\\1", izh212))
num=seq(4, length(pa112), 19)
sar112 = as.numeric(pa112[num])
num=seq(5, length(pa112), 19)
sar212 = pa112[num]
sar212 = as.numeric(sub("(.*)(.{5})", "\\1", sar212))
num=seq(6, length(pa112), 19)
vot112 = as.numeric(pa112[num])
num=seq(7, length(pa112), 19)
vot212 = pa112[num]
vot212 = as.numeric(sub("(.*)(.{5})", "\\1", vot212))
num=seq(8, length(pa112), 19)
moz112 = as.numeric(pa112[num])
num=seq(9, length(pa112), 19)
moz212 = pa112[num]
moz212 =as.numeric(sub("(.*)(.{5})", "\\1", moz212))
num=seq(10, length(pa112), 19)
gla112 = as.numeric(pa112[num])
num=seq(11, length(pa112), 19)
gla212 = pa112[num]
gla212 = as.numeric(sub("(.*)(.{5})", "\\1", gla212))
num=seq(12, length(pa112), 19)
kam112 = as.numeric(pa112[num])
num=seq(13, length(pa112), 19)
kam212 = pa112[num]
kam212 = as.numeric(sub("(.*)(.{5})", "\\1", kam212))
num=seq(14, length(pa112), 19)
raj112 = as.numeric(pa112[num])
num=seq(15, length(pa112), 19)
raj212 = pa112[num]
raj212 = as.numeric(sub("(.*)(.{5})", "\\1", raj212))
num=seq(16, length(pa112), 19)
ost112 = as.numeric(pa112[num])
num=seq(17, length(pa112), 19)
ost212 = pa112[num]
ost212 = as.numeric(sub("(.*)(.{5})", "\\1", ost212))
num=seq(18, length(pa112), 19)
vse112 = as.numeric(pa112[num])
num=seq(19, length(pa112), 19)
vse212 = pa112[num]
vse212 = as.numeric(sub("(.*)(.{5})", "\\1", vse212))

#спарсиваем данные по факультету соц работы и очищаем данные
с13 = readLines ("http://io.udsu.ru/iias/sait.fkt_info_region?var=21", encoding = "UTF-8")
сtree13 = htmlTreeParse(с13, useInternalNodes = T)
y13=xpathSApply(сtree13, "//td[@style='font-weight:bold; color:navy']", xmlValue)
y13
pa13=xpathSApply(сtree13, "//tr[@style='white-space: nowrap']/td", xmlValue)
pa13 = sub("100%", "100", pa13)
pa113 = strsplit (pa13, "\n")

num=seq(1, length(pa113), 19)
god13 = as.numeric(pa113[num])
num=seq(2, length(pa113), 19)
izh113 = as.numeric(pa113[num])
num=seq(3, length(pa113), 19)
izh213 = pa113[num]
izh213 =as.numeric(sub("(.*)(.{5})", "\\1", izh213))
num=seq(4, length(pa113), 19)
sar113 = as.numeric(pa113[num])
num=seq(5, length(pa113), 19)
sar213 = pa113[num]
sar213 = as.numeric(sub("(.*)(.{5})", "\\1", sar213))
num=seq(6, length(pa113), 19)
vot113= as.numeric(pa113[num])
num=seq(7, length(pa113), 19)
vot213 = pa113[num]
vot213 = as.numeric(sub("(.*)(.{5})", "\\1", vot213))
num=seq(8, length(pa113), 19)
moz113 = as.numeric(pa113[num])
num=seq(9, length(pa113), 19)
moz213 = pa113[num]
moz213 =as.numeric(sub("(.*)(.{5})", "\\1", moz213))
num=seq(10, length(pa113), 19)
gla113 = as.numeric(pa113[num])
num=seq(11, length(pa113), 19)
gla213 = pa113[num]
gla213 = as.numeric(sub("(.*)(.{5})", "\\1", gla213))
num=seq(12, length(pa113), 19)
kam113 = as.numeric(pa113[num])
num=seq(13, length(pa113), 19)
kam213 = pa113[num]
kam213 = as.numeric(sub("(.*)(.{5})", "\\1", kam213))
num=seq(14, length(pa113), 19)
raj113 = as.numeric(pa113[num])
num=seq(15, length(pa113), 19)
raj213 = pa113[num]
raj213 = as.numeric(sub("(.*)(.{5})", "\\1", raj213))
num=seq(16, length(pa113), 19)
ost113 = as.numeric(pa113[num])
num=seq(17, length(pa113), 19)
ost213 = pa113[num]
ost213 = as.numeric(sub("(.*)(.{5})", "\\1", ost213))
num=seq(18, length(pa113), 19)
vse113 = as.numeric(pa113[num])
num=seq(19, length(pa113), 19)
vse213 = pa113[num]
vse213 = as.numeric(sub("(.*)(.{5})", "\\1", vse213))

#спарсиваем данные по факультету социологии и философии и очищаем данные
с14 = readLines ("http://io.udsu.ru/iias/sait.fkt_info_region?var=14", encoding = "UTF-8")
сtree14 = htmlTreeParse(с14, useInternalNodes = T)
y14=xpathSApply(сtree14, "//td[@style='font-weight:bold; color:navy']", xmlValue)
y14
pa14=xpathSApply(сtree14, "//tr[@style='white-space: nowrap']/td", xmlValue)
pa14 = sub("100%", "100", pa14)
pa114 = strsplit (pa14, "\n")

num=seq(1, length(pa114), 19)
god0 = as.numeric(pa114[num])
num=seq(2, length(pa114), 19)
izh114 = as.numeric(pa114[num])
num=seq(3, length(pa114), 19)
izh214 = pa114[num]
izh214 =as.numeric(sub("(.*)(.{5})", "\\1", izh214))
num=seq(4, length(pa114), 19)
sar114 = as.numeric(pa114[num])
num=seq(5, length(pa114), 19)
sar214 = pa114[num]
sar214 = as.numeric(sub("(.*)(.{5})", "\\1", sar214))
num=seq(6, length(pa10), 19)
vot114 = as.numeric(pa10[num])
num=seq(7, length(pa10), 19)
vot214 = pa114[num]
vot214 = as.numeric(sub("(.*)(.{5})", "\\1", vot214))
num=seq(8, length(pa114), 19)
moz114 = as.numeric(pa114[num])
num=seq(9, length(pa114), 19)
moz214 = pa114[num]
moz214 =as.numeric(sub("(.*)(.{5})", "\\1", moz214))
num=seq(10, length(pa114), 19)
gla114 = as.numeric(pa114[num])
num=seq(11, length(pa114), 19)
gla214 = pa114[num]
gla214 = as.numeric(sub("(.*)(.{5})", "\\1", gla214))
num=seq(12, length(pa114), 19)
kam114 = as.numeric(pa114[num])
num=seq(13, length(pa114), 19)
kam214 = pa114[num]
kam214 = as.numeric(sub("(.*)(.{5})", "\\1", kam214))
num=seq(14, length(pa114), 19)
raj114 = as.numeric(pa114[num])
num=seq(15, length(pa114), 19)
raj214 = pa114[num]
raj214 = as.numeric(sub("(.*)(.{5})", "\\1", raj214))
num=seq(16, length(pa114), 19)
ost114 = as.numeric(pa114[num])
num=seq(17, length(pa114), 19)
ost214 = pa114[num]
ost214 = as.numeric(sub("(.*)(.{5})", "\\1", ost214))
num=seq(18, length(pa114), 19)
vse114 = as.numeric(pa114[num])
num=seq(19, length(pa114), 19)
vse214 = pa114[num]
vse214 = as.numeric(sub("(.*)(.{5})", "\\1", vse214))

#спарсиваем данные по факультету удм филологии и очищаем данные
с15 = readLines ("http://io.udsu.ru/iias/sait.fkt_info_region?var=3", encoding = "UTF-8")
сtree15 = htmlTreeParse(с15, useInternalNodes = T)
y15=xpathSApply(сtree15, "//td[@style='font-weight:bold; color:navy']", xmlValue)
y15
pa15=xpathSApply(сtree15, "//tr[@style='white-space: nowrap']/td", xmlValue)
pa15 = sub("100%", "100", pa15)
pa115 = strsplit (pa15, "\n")

num=seq(1, length(pa115), 19)
god15 = as.numeric(pa115[num])
num=seq(2, length(pa115), 19)
izh115 = as.numeric(pa115[num])
num=seq(3, length(pa115), 19)
izh215 = pa115[num]
izh215 =as.numeric(sub("(.*)(.{5})", "\\1", izh215))
num=seq(4, length(pa115), 19)
sar115 = as.numeric(pa115[num])
num=seq(5, length(pa115), 19)
sar215 = pa115[num]
sar215 = as.numeric(sub("(.*)(.{5})", "\\1", sar215))
num=seq(6, length(pa115), 19)
vot115 = as.numeric(pa115[num])
num=seq(7, length(pa115), 19)
vot215 = pa115[num]
vot215 = as.numeric(sub("(.*)(.{5})", "\\1", vot215))
num=seq(8, length(pa115), 19)
moz115 = as.numeric(pa115[num])
num=seq(9, length(pa115), 19)
moz215 = pa115[num]
moz215 =as.numeric(sub("(.*)(.{5})", "\\1", moz215))
num=seq(10, length(pa115), 19)
gla115 = as.numeric(pa115[num])
num=seq(11, length(pa115), 19)
gla215 = pa115[num]
gla215 = as.numeric(sub("(.*)(.{5})", "\\1", gla215))
num=seq(12, length(pa115), 19)
kam115 = as.numeric(pa115[num])
num=seq(13, length(pa115), 19)
kam215 = pa115[num]
kam215 = as.numeric(sub("(.*)(.{5})", "\\1", kam215))
num=seq(14, length(pa115), 19)
raj115 = as.numeric(pa115[num])
num=seq(15, length(pa115), 19)
raj215 = pa115[num]
raj215 = as.numeric(sub("(.*)(.{5})", "\\1", raj215))
num=seq(16, length(pa115), 19)
ost115 = as.numeric(pa115[num])
num=seq(17, length(pa115), 19)
ost215 = pa115[num]
ost215 = as.numeric(sub("(.*)(.{5})", "\\1", ost215))
num=seq(18, length(pa10), 19)
vse115 = as.numeric(pa10[num])
num=seq(19, length(pa10), 19)
vse215 = pa115[num]
vse215 = as.numeric(sub("(.*)(.{5})", "\\1", vse215))

#спарсиваем данные по факультету физ кул и спорта и очищаем данные
с16 = readLines ("http://io.udsu.ru/iias/sait.fkt_info_region?var=12", encoding = "UTF-8")
сtree16 = htmlTreeParse(с16, useInternalNodes = T)
y16=xpathSApply(сtree16, "//td[@style='font-weight:bold; color:navy']", xmlValue)
y16
pa16=xpathSApply(сtree16, "//tr[@style='white-space: nowrap']/td", xmlValue)
pa16 = sub("100%", "100", pa16)
pa116 = strsplit (pa16, "\n")

num=seq(1, length(pa116), 19)
god16 = as.numeric(pa116[num])
num=seq(2, length(pa116), 19)
izh116 = as.numeric(pa116[num])
num=seq(3, length(pa116), 19)
izh216 = pa116[num]
izh216=as.numeric(sub("(.*)(.{5})", "\\1", izh216))
num=seq(4, length(pa116), 19)
sar116 = as.numeric(pa116[num])
num=seq(5, length(pa116), 19)
sar216 = pa116[num]
sar216 = as.numeric(sub("(.*)(.{5})", "\\1", sar216))
num=seq(6, length(pa116), 19)
vot116 = as.numeric(pa116[num])
num=seq(7, length(pa116), 19)
vot216 = pa116[num]
vot216 = as.numeric(sub("(.*)(.{5})", "\\1", vot216))
num=seq(8, length(pa116), 19)
moz116 = as.numeric(pa116[num])
num=seq(9, length(pa116), 19)
moz216 = pa116[num]
moz216 =as.numeric(sub("(.*)(.{5})", "\\1", moz216))
num=seq(10, length(pa116), 19)
gla116 = as.numeric(pa116[num])
num=seq(11, length(pa116), 19)
gla216 = pa116[num]
gla216 = as.numeric(sub("(.*)(.{5})", "\\1", gla216))
num=seq(12, length(pa116), 19)
kam116 = as.numeric(pa116[num])
num=seq(13, length(pa116), 19)
kam216 = pa116[num]
kam216 = as.numeric(sub("(.*)(.{5})", "\\1", kam216))
num=seq(14, length(pa116), 19)
raj116 = as.numeric(pa116[num])
num=seq(15, length(pa116), 19)
raj216 = pa116[num]
raj216 = as.numeric(sub("(.*)(.{5})", "\\1", raj216))
num=seq(16, length(pa116), 19)
ost116 = as.numeric(pa116[num])
num=seq(17, length(pa116), 19)
ost216 = pa116[num]
ost216 = as.numeric(sub("(.*)(.{5})", "\\1", ost216))
num=seq(18, length(pa116), 19)
vse116 = as.numeric(pa116[num])
num=seq(19, length(pa116), 19)
vse216 = pa116[num]
vse216 = as.numeric(sub("(.*)(.{5})", "\\1", vse216))

#спарсиваем данные по физ-энергет факультету и очищаем данные
с17 = readLines ("http://io.udsu.ru/iias/sait.fkt_info_region?var=5", encoding = "UTF-8")
сtree17 = htmlTreeParse(с17, useInternalNodes = T)
y17=xpathSApply(сtree17, "//td[@style='font-weight:bold; color:navy']", xmlValue)
y17
pa17=xpathSApply(сtree17, "//tr[@style='white-space: nowrap']/td", xmlValue)
pa17 = sub("100%", "100", pa17)
pa117 = strsplit (pa17, "\n")

num=seq(1, length(pa117), 19)
god17 = as.numeric(pa117[num])
num=seq(2, length(pa117), 19)
izh117 = as.numeric(pa117[num])
num=seq(3, length(pa117), 19)
izh217 = pa117[num]
izh217 =as.numeric(sub("(.*)(.{5})", "\\1", izh217))
num=seq(4, length(pa117), 19)
sar117 = as.numeric(pa117[num])
num=seq(5, length(pa117), 19)
sar217 = pa117[num]
sar217 = as.numeric(sub("(.*)(.{5})", "\\1", sar217))
num=seq(6, length(pa117), 19)
vot117 = as.numeric(pa117[num])
num=seq(7, length(pa117), 19)
vot217 = pa117[num]
vot217 = as.numeric(sub("(.*)(.{5})", "\\1", vot217))
num=seq(8, length(pa117), 19)
moz117 = as.numeric(pa117[num])
num=seq(9, length(pa117), 19)
moz217 = pa117[num]
moz217 =as.numeric(sub("(.*)(.{5})", "\\1", moz217))
num=seq(10, length(pa117), 19)
gla117 = as.numeric(pa117[num])
num=seq(11, length(pa117), 19)
gla217 = pa117[num]
gla217 = as.numeric(sub("(.*)(.{5})", "\\1", gla217))
num=seq(12, length(pa117), 19)
kam117 = as.numeric(pa117[num])
num=seq(13, length(pa117), 19)
kam217 = pa117[num]
kam217 = as.numeric(sub("(.*)(.{5})", "\\1", kam217))
num=seq(14, length(pa117), 19)
raj117 = as.numeric(pa117[num])
num=seq(15, length(pa117), 19)
raj217 = pa117[num]
raj217 = as.numeric(sub("(.*)(.{5})", "\\1", raj217))
num=seq(16, length(pa117), 19)
ost117 = as.numeric(pa117[num])
num=seq(17, length(pa117), 19)
ost217 = pa117[num]
ost217 = as.numeric(sub("(.*)(.{5})", "\\1", ost217))
num=seq(18, length(pa117), 19)
vse117 = as.numeric(pa117[num])
num=seq(19, length(pa117), 19)
vse217 = pa117[num]
vse217 = as.numeric(sub("(.*)(.{5})", "\\1", vse217))

#объединяем данные поступающих по факультетам из города Ижевск
izhevsk1=data.frame(Год=god, БХФ=izh1c, ВКСПН=izh11, ГФ=izh12, ИГЗ=izh13, ИЕН=izh14, 
                       ИИЯЛ=izh15, ИИД=izh16, ИИС=izh17, ИМИТиФ=izh18, ИНГ=izh19,ИППСТ=izh10, ИПСУБ=izh101,
                       ИСК=izh102, ИУФФУЖ=izh103, ИФКиС=izh104, ИЭиУ=izh105, ИЯЛ=izh106, ИФ=izh107, МФ=izh108, НФ=izh109,
                       ФЖ=izh100, ФИТВТ=izh110, ФМБ=izh111, ФПИЯ=izh112, ФСР=izh113, ФСФ=izh114, ФУФ=izh115,ФФКиС=izh116, ФЭФ=izh117)
izhevsk1=izhevsk1[order(izhevsk1$Год),]

#объединяем данные поступивших по факультетам из города Ижевск
izhevsk2=data.frame(Год=god, БХФ=izh2c, ВКСПН=izh21, ГФ=izh22, ИГЗ=izh23, ИЕН=izh24, 
                       ИИЯЛ=izh25, ИИД=izh26, ИИС=izh27, ИМИТиФ=izh28, ИНГ=izh29,ИППСТ=izh20, ИПСУБ=izh201,
                       ИСК=izh202, ИУФФУЖ=izh203, ИФКиС=izh204, ИЭиУ=izh205, ИЯЛ=izh206, ИФ=izh207, МФ=izh208, НФ=izh209,
                       ФЖ=izh200, ФИТВТ=izh210, ФМБ=izh211, ФПИЯ=izh212, ФСР=izh213, ФСФ=izh214,
                       ФУФ=izh215,ФФКиС=izh216, ФЭФ=izh217)
izhevsk2=izhevsk2[order(izhevsk2$Год),]

#объединяем данные поступающих по факультетам из города Сарапул
sarap1=data.frame(Год=god, БХФ=sar1c, ВКСПН=sar11, ГФ=sar12, ИГЗ=sar13, ИЕН=sar14, 
                     ИИЯЛ=sar15, ИИД=sar16, ИИС=sar17, ИМИТиФ=sar18, ИНГ=sar19,ИППСТ=sar10, ИПСУБ=sar101,
                     ИСК=sar102, ИУФФУЖ=sar103, ИФКиС=sar104, ИЭиУ=sar105, ИЯЛ=sar106, ИФ=sar107, МФ=sar108, НФ=sar109,
                     ФЖ=sar100, ФИТВТ=sar110, ФМБ=sar111, ФПИЯ=sar112, ФСР=sar113, ФСФ=sar114, ФУФ=sar115,ФФКиС=sar116, ФЭФ=sar117)
sarap1=sarap1[order(sarap1$Год),]

#объединяем данные поступивших по факультетам из города Сарапул
sarap2=data.frame(Год=god, БХФ=sar2c, ВКСПН=sar21, ГФ=sar22, ИГЗ=sar23, ИЕН=sar24, 
                     ИИЯЛ=sar25, ИИД=sar26, ИИС=sar27, ИМИТиФ=sar28, ИНГ=sar29,ИППСТ=sar20, ИПСУБ=sar201,
                     ИСК=sar202, ИУФФУЖ=sar203, ИФКиС=sar204, ИЭиУ=sar205, ИЯЛ=sar206, ИФ=sar207, МФ=sar208, НФ=sar209,
                     ФЖ=sar200, ФИТВТ=sar210, ФМБ=sar211, ФПИЯ=sar212, ФСР=sar213, ФСФ=sar214, ФУФ=sar215,ФФКиС=sar216, ФЭФ=sar217)
sarap2=sarap2[order(sarap2$Год),]

#объединяем данные поступающих по факультетам из города Воткинкс
votk1=data.frame(Год=god, БХФ=vot1c, ВКСПН=vot11, ГФ=vot12, ИГЗ=vot13, ИЕН=vot14, 
                    ИИЯЛ=vot15, ИИД=vot16, ИИС=vot17, ИМИТиФ=vot18, ИНГ=vot19,ИППСТ=vot10, ИПСУБ=vot101,
                    ИСК=vot102, ИУФФУЖ=vot103, ИФКиС=vot104, ИЭиУ=vot105, ИЯЛ=vot106, ИФ=vot107, МФ=vot108, НФ=vot109,
                    ФЖ=vot100, ФИТВТ=vot110, ФМБ=vot111, ФПИЯ=vot112, ФСР=vot113, ФСФ=vot114, ФУФ=vot115,ФФКиС=vot116, ФЭФ=vot117)
votk1=votk1[order(votk1$Год),]

#объединяем данные поступивших по факультетам из города Воткинкс
votk2=data.frame(Год=god, БХФ=vot2c, ВКСПН=vot21, ГФ=vot22, ИГЗ=vot23, ИЕН=vot24, 
                    ИИЯЛ=vot25, ИИД=vot26, ИИС=vot27, ИМИТиФ=vot28, ИНГ=vot29,ИППСТ=vot20, ИПСУБ=vot201,
                    ИСК=vot202, ИУФФУЖ=vot203, ИФКиС=vot204, ИЭиУ=vot205, ИЯЛ=vot206, ИФ=vot207, МФ=vot208, НФ=vot209,
                    ФЖ=vot200, ФИТВТ=vot210, ФМБ=vot211, ФПИЯ=vot212, ФСР=vot213, ФСФ=vot214, ФУФ=vot215,ФФКиС=vot216, ФЭФ=vot217)
votk2=votk2[order(votk2$Год),]

#объединяем данные поступающих по факультетам из города Можга
mozhga1=data.frame(Год=god, БХФ=moz1c, ВКСПН=moz11, ГФ=moz12, ИГЗ=moz13, ИЕН=moz14, 
                      ИИЯЛ=moz15, ИИД=moz16, ИИС=moz17, ИМИТиФ=moz18, ИНГ=moz19,ИППСТ=moz10, ИПСУБ=moz101,
                      ИСК=moz102, ИУФФУЖ=moz103, ИФКиС=moz104, ИЭиУ=moz105, ИЯЛ=moz106, ИФ=moz107, МФ=moz108, НФ=moz109,
                      ФЖ=moz100, ФИТВТ=moz110, ФМБ=moz111, ФПИЯ=moz112, ФСР=moz113, ФСФ=moz114, ФУФ=moz115,ФФКиС=moz116, ФЭФ=moz117)
mozhga1=mozhga1[order(mozhga1$Год),]

#объединяем данные поступивших по факультетам из города Можга
mozhga2=data.frame(Год=god, БХФ=moz2c, ВКСПН=moz21, ГФ=moz22, ИГЗ=moz23, ИЕН=moz24, 
                      ИИЯЛ=moz25, ИИД=moz26, ИИС=moz27, ИМИТиФ=moz28, ИНГ=moz29,ИППСТ=moz20, ИПСУБ=moz201,
                      ИСК=moz202, ИУФФУЖ=moz203, ИФКиС=moz204, ИЭиУ=moz205, ИЯЛ=moz206, ИФ=moz207, МФ=moz208, НФ=moz209,
                      ФЖ=moz200, ФИТВТ=moz210, ФМБ=moz211, ФПИЯ=moz212, ФСР=moz213, ФСФ=moz214, ФУФ=moz215,ФФКиС=moz216, ФЭФ=moz217)
mozhga2=mozhga2[order(mozhga2$Год),]

#объединяем данные поступающих по факультетам из города Глазов
glaz1=data.frame(Год=god, БХФ=gla1c, ВКСПН=gla11, ГФ=gla12, ИГЗ=gla13, ИЕН=gla14, 
                    ИИЯЛ=gla15, ИИД=gla16, ИИС=gla17, ИМИТиФ=gla18, ИНГ=gla19,ИППСТ=gla10, ИПСУБ=gla101,
                    ИСК=gla102, ИУФФУЖ=gla103, ИФКиС=gla104, ИЭиУ=gla105, ИЯЛ=gla106, ИФ=gla107, МФ=gla108, НФ=gla109,
                    ФЖ=gla100, ФИТВТ=gla110, ФМБ=gla111, ФПИЯ=gla112, ФСР=gla113, ФСФ=gla114, ФУФ=gla115,ФФКиС=gla116, ФЭФ=gla117)
glaz1=glaz1[order(glaz1$Год),]

#объединяем данные поступивших по факультетам из города Глазов
glaz2=data.frame(Год=god, БХФ=gla2c, ВКСПН=gla21, ГФ=gla22, ИГЗ=gla23, ИЕН=gla24, 
                    ИИЯЛ=gla25, ИИД=gla26, ИИС=gla27, ИМИТиФ=gla28, ИНГ=gla29,ИППСТ=gla20, ИПСУБ=gla201,
                    ИСК=gla202, ИУФФУЖ=gla203, ИФКиС=gla204, ИЭиУ=gla205, ИЯЛ=gla206, ИФ=gla207, МФ=gla208, НФ=gla209,
                    ФЖ=gla200, ФИТВТ=gla210, ФМБ=gla211, ФПИЯ=gla212, ФСР=gla213, ФСФ=gla214, ФУФ=gla215,ФФКиС=gla216, ФЭФ=gla217)
glaz2=glaz2[order(glaz2$Год),]

#объединяем данные поступающих по факультетам из города Камбарка
kamb1=data.frame(Год=god, БХФ=kam1c, ВКСПН=kam11, ГФ=kam12, ИГЗ=kam13, ИЕН=kam14, 
                    ИИЯЛ=kam15, ИИД=kam16, ИИС=kam17, ИМИТиФ=kam18, ИНГ=kam19,ИППСТ=kam10, ИПСУБ=kam101,
                    ИСК=kam102, ИУФФУЖ=kam103, ИФКиС=kam104, ИЭиУ=kam105, ИЯЛ=kam106, ИФ=kam107, МФ=kam108, НФ=kam109,
                    ФЖ=kam100, ФИТВТ=kam110, ФМБ=kam111, ФПИЯ=kam112, ФСР=kam113, ФСФ=kam114, ФУФ=kam115,ФФКиС=kam116, ФЭФ=kam117)
kamb1=kamb1[order(kamb1$Год),]

#объединяем данные поступивших по факультетам из города Камбарка
kamb2=data.frame(Год=god, БХФ=kam2c, ВКСПН=kam21, ГФ=kam22, ИГЗ=kam23, ИЕН=kam24, 
                    ИИЯЛ=kam25, ИИД=kam26, ИИС=kam27, ИМИТиФ=kam28, ИНГ=kam29,ИППСТ=kam20, ИПСУБ=kam201,
                    ИСК=kam202, ИУФФУЖ=kam203, ИФКиС=kam204, ИЭиУ=kam205, ИЯЛ=kam206, ИФ=kam207, МФ=kam208, НФ=kam209,
                    ФЖ=kam200, ФИТВТ=kam210, ФМБ=kam211, ФПИЯ=kam212, ФСР=kam213, ФСФ=kam214, ФУФ=kam215,ФФКиС=kam216, ФЭФ=kam217)
kamb2=kamb2[order(kamb2$Год),]

#объединяем данные поступающих по факультетам из районов УР
rajon1=data.frame(Год=god, БХФ=raj1c, ВКСПН=kam11, ГФ=raj12, ИГЗ=raj13, ИЕН=raj14, 
                     ИИЯЛ=raj15, ИИД=raj16, ИИС=raj17, ИМИТиФ=raj18, ИНГ=raj19,ИППСТ=raj10, ИПСУБ=raj101,
                     ИСК=raj102, ИУФФУЖ=raj103, ИФКиС=raj104, ИЭиУ=raj105, ИЯЛ=raj106, ИФ=raj107, МФ=raj108, НФ=raj109,
                     ФЖ=raj100, ФИТВТ=raj110, ФМБ=raj111, ФПИЯ=raj112, ФСР=raj113, ФСФ=raj114, ФУФ=raj115,ФФКиС=raj116, ФЭФ=raj117)
rajon1=rajon1[order(rajon1$Год),]

#объединяем данные поступивших по факультетам из районов УР
rajon2=data.frame(Год=god, БХФ=kam2c, ВКСПН=raj21, ГФ=raj22, ИГЗ=raj23, ИЕН=raj24, 
                     ИИЯЛ=raj25, ИИД=raj26, ИИС=raj27, ИМИТиФ=raj28, ИНГ=raj29,ИППСТ=raj20, ИПСУБ=raj201,
                     ИСК=raj202, ИУФФУЖ=raj203, ИФКиС=raj204, ИЭиУ=raj205, ИЯЛ=raj206, ИФ=raj207, МФ=raj208, НФ=raj209,
                     ФЖ=raj200, ФИТВТ=raj210, ФМБ=raj211, ФПИЯ=raj212, ФСР=raj213, ФСФ=raj214, ФУФ=raj215,ФФКиС=raj216, ФЭФ=raj217)
rajon2=rajon2[order(rajon2$Год),]

#объединяем данные поступающих по факультетам из остальных регионов и стран
ostal1=data.frame(Год=god, БХФ=ost1c, ВКСПН=ost11, ГФ=ost12, ИГЗ=ost13, ИЕН=ost14, 
                     ИИЯЛ=ost15, ИИД=ost16, ИИС=ost17, ИМИТиФ=ost18, ИНГ=ost19,ИППСТ=ost10, ИПСУБ=ost101,
                     ИСК=ost102, ИУФФУЖ=ost103, ИФКиС=ost104, ИЭиУ=ost105, ИЯЛ=ost106, ИФ=ost107, МФ=ost108, НФ=ost109,
                     ФЖ=ost100, ФИТВТ=ost110, ФМБ=ost111, ФПИЯ=ost112, ФСР=ost113, ФСФ=ost114, ФУФ=ost115,ФФКиС=ost116, ФЭФ=ost117)
ostal1=ostal1[order(ostal1$Год),]

#объединяем данные поступивших по факультетам из остальных регионов и стран
ostal2=data.frame(Год=god, БХФ=ost2c, ВКСПН=ost21, ГФ=ost22, ИГЗ=ost23, ИЕН=ost24, 
                     ИИЯЛ=ost25, ИИД=ost26, ИИС=ost27, ИМИТиФ=ost28, ИНГ=ost29,ИППСТ=ost20, ИПСУБ=ost201,
                     ИСК=ost202, ИУФФУЖ=ost203, ИФКиС=ost204, ИЭиУ=ost205, ИЯЛ=ost206, ИФ=ost207, МФ=ost208, НФ=ost209,
                     ФЖ=ost200, ФИТВТ=ost210, ФМБ=ost211, ФПИЯ=ost212, ФСР=ost213, ФСФ=ost214, ФУФ=ost215,ФФКиС=ost216, ФЭФ=ost217)
ostal2=ostal2[order(ostal2$Год),]

#объединяем данные поступающих по факультетам по УдГУ
vsee1=data.frame(Год=god, БХФ=vse1c, ВКСПН=kam11, ГФ=vse12, ИГЗ=vse13, ИЕН=vse14, 
                    ИИЯЛ=vse15, ИИД=vse16, ИИС=vse17, ИМИТиФ=vse18, ИНГ=vse19,ИППСТ=vse10, ИПСУБ=vse101,
                    ИСК=vse102, ИУФФУЖ=vse103, ИФКиС=vse104, ИЭиУ=vse105, ИЯЛ=vse106, ИФ=vse107, МФ=vse108, НФ=vse109,
                    ФЖ=vse100, ФИТВТ=vse110, ФМБ=vse111, ФПИЯ=vse112, ФСР=vse113, ФСФ=vse114, ФУФ=vse115,ФФКиС=vse116, ФЭФ=vse117)
vsee1=vsee1[order(vsee1$Год),]

#объединяем данные поступившх по факультетам по УдГУ
vsee2=data.frame(Год=god, БХФ=vse2c, ВКСПН=vse21, ГФ=vse22, ИГЗ=vse23, ИЕН=vse24, 
                    ИИЯЛ=vse25, ИИД=vse26, ИИС=vse27, ИМИТиФ=vse28, ИНГ=vse29,ИППСТ=vse20, ИПСУБ=vse201,
                    ИСК=vse202, ИУФФУЖ=vse203, ИФКиС=vse204, ИЭиУ=vse205, ИЯЛ=vse206, ИФ=vse207, МФ=vse208, НФ=vse209,
                    ФЖ=vse200, ФИТВТ=vse210, ФМБ=vse211, ФПИЯ=vse212, ФСР=vse213, ФСФ=vse214, ФУФ=vse215,ФФКиС=vse216, ФЭФ=vse217)
vsee2=vsee2[order(vsee2$Год),]
