---
title: "Speed date"
author: "Paulo Yun Cha"
date: "22 de junho de 2017"
output: html_document
---

library(corrplot)
library(plspm)
library(GGally)
library(FactoMineR)
library(plsdepot)
library(ggplot2)

setwd("/Users/Catarina/dropbox/Paulo - Data Science (R,Python, Netlogo, Tableau)/Rstudio/Pls_flexibility_rh")

data<-read.csv("data.csv",sep=";",na.strings=TRUE)

data[,37]<-as.factor(data[,37])

#Correlation map


data$type<-mapvalues(data[,37],from=c("1","2","3","4","5"),to = c("Não-privado","Não-privado","Não-privado","Privado","Não-privado"))

corrplot::corrplot(cor(data[,1:30]),method = "color" ,sig.level = 0.01,is.corr = TRUE,order = "AOE",type = "lower",hclust.method = "complete",title = "Matriz de correlação entre as variáveis")


corrplot::corrplot(cor(data[,1:6]),method = "color" ,sig.level = 0.01,is.corr = TRUE,order = "AOE",type = "lower",hclust.method = "complete",title = "Matriz de correlação - individual")


corrplot::corrplot(cor(data[,7:9]),method = "color" ,sig.level = 0.01,is.corr = TRUE,order = "AOE",type = "lower",hclust.method = "complete",title = "Matriz de correlação - grupo")

corrplot::corrplot(cor(data[,10:21]),method = "color" ,sig.level = 0.01,is.corr = TRUE,order = "AOE",type = "lower",hclust.method = "complete",title = "Matriz de correlação - organizacional")

corrplot::corrplot(cor(data[,22:30]),method = "color" ,sig.level = 0.01,is.corr = TRUE,order = "AOE",type = "lower",hclust.method = "complete",title = "Matriz de correlação - RH")


for (j in 1:30) {
	data[,j]<-as.factor(data[,j])
}

model_mca<-MCA(data[,1:30],ncp=2,graph=TRUE,na.method = "NA")

model_hcpc<-HCPC(model_mca)


for (j in 1:30) {
	data[,j]<-as.numeric(data[,j])
}

				Flexibilidade	grupo	individual	organizacional
Flexibilidade		0			
grupo				1			0		
individual			1			0	
organizacional		0			1			1			 0

flexibilidade=c(0,0,0,0)
grupo=c(1,0,0,0)
individual=c(1,0,0,0)
organizacional=c(0,1,1,0)


flex_path=rbind(flexibilidade,grupo,individual,organizacional)
colnames(flex_path)=rownames(flex_path)
innerplot(flex_path,box.size=0.1)

flex_block_1=list(22:30,7:9,1:6,10:21)

flex_mode=rep("A",4)

#Modelo 1 PLSPM

flex_pls_1<-plspm(Data = data,path_matrix = flex_path,blocks =  flex_block_1,modes = flex_mode)

plot(flex_pls_1,arr.pos = 0.35,main="Caminho de coeficiente - 1º modelo",arr.lwd=20*round(flex_pls_1$path_coefs,2))


flex_pls_1$unidim # primeira análise

Valores maiores que 0.7
Primeiro Eigen value maior que o Segundo Eigen value

               Mode MVs   C.alpha    DG.rho  eig.1st   eig.2nd
flexibilidade     A   9 0.8818570 0.9054804 4.659915 1.1628688
grupo             A   3 0.8226424 0.8950719 2.221908 0.5482759
individual        A   6 0.8817280 0.9104711 3.776085 0.6928688
organizacional    A  12 0.9266423 0.9376027 6.702203 0.9533224

plot(flex_pls_1,what="loadings")


flex_pls_1$outer_model # segunda análise

ggplot(data=flex_pls_1$outer_model, aes(x=name,y=loading,fill=block))+
geom_bar(stat="identity",position="dodge")+
geom_hline(yintercept=0.7,color="gray50")+
ggtitle("Barchart de loadings - 1º modelo")+
theme(axis.text.x=element_text(angle=90))

#Modelo 2 Plspm

São retirados as seguintes variáveis: v22,v25,v26,o10,o12,o11,o17

flex_block_2=list(c(23,24,27,28,29,30),7:9,1:6,c(13:16,18:21))

flex_mode=rep("A",4)

flex_pls_2<-plspm(Data = data,path_matrix = flex_path,blocks =  flex_block_2,modes = flex_mode)

plot(flex_pls_2,arr.pos = 0.35,main="Caminho de coeficiente - 2º modelo",arr.lwd=20*round(flex_pls_2$path_coefs,2))


flex_pls_2$unidim # primeira análise

Valores maiores que 0.7
Primeiro Eigen value maior que o Segundo Eigen value

               Mode MVs   C.alpha    DG.rho  eig.1st   eig.2nd
flexibilidade     A   6 0.8692891 0.9026423 3.656029 0.8124375
grupo             A   3 0.8226424 0.8950719 2.221908 0.5482759
individual        A   6 0.8817280 0.9104711 3.776085 0.6928688
organizacional    A   8 0.9229766 0.9369855 5.203813 0.8084633

plot(flex_pls_2,what="loadings")

flex_pls_2$outer_model # segunda análise

Valores maiores que 0.7

ggplot(data=flex_pls_2$outer_model, aes(x=name,y=loading,fill=block))+
geom_bar(stat="identity",position="dodge")+
geom_hline(yintercept=0.7,color="gray50")+
ggtitle("Barchart de loadings - 2º modelo")+
theme(axis.text.x=element_text(angle=90))


as.data.frame(flex_pls_2$crossloadings) # Terceira análise

       name          block 	flexibilidade  grupo   individual   organizacional
1  V..23.R.  flexibilidade     0.7082729 0.5880910  0.5338538      0.6086518
2  V..24.R.  flexibilidade     0.7583822 0.4247070  0.4826251      0.5578770
3  V..27.C.  flexibilidade     0.7747250 0.4085530  0.3813671      0.4792432
4  V..28.C.  flexibilidade     0.7474171 0.3333106  0.3230731      0.4369651
5  V..29.C.  flexibilidade     0.8463804 0.4788024  0.4311768      0.5351179
6  V..30.C.  flexibilidade     0.8185947 0.4742335  0.3957993      0.5173934
7   V..7.C.          grupo     0.4148095 0.7500262  0.4534821      0.4603935
8   V..8.C.          grupo     0.5085245 0.9160687  0.6331364      0.6391044
9   V..9.C.          grupo     0.5961990 0.9023308  0.7042231      0.7528188
10  V..1.I.     individual     0.3646039 0.5415736  0.8140016      0.5914589
11  V..2.I.     individual     0.3584361 0.4961574  0.7736554      0.5504552
12  V..3.I.     individual     0.4150052 0.5091026  0.7201015      0.5323440
13  V..4.I.     individual     0.5388927 0.5492336  0.8094815      0.6434862
14  V..5.I.     individual     0.4660378 0.6408950  0.8044320      0.6201881
15  V..6.I.     individual     0.4962073 0.6181866  0.8300175      0.6812867
16 V..13.O. organizacional     0.5504180 0.5893860  0.6182218      0.8171941
17 V..14.O. organizacional     0.5009607 0.5473420  0.5434660      0.7390321
18 V..15.O. organizacional     0.5968510 0.5836794  0.5610794      0.8380725
19 V..16.O. organizacional     0.5393582 0.5972374  0.6349135      0.8249263
20 V..18.O. organizacional     0.5652853 0.6317400  0.6061833      0.7885690
21 V..19.O. organizacional     0.6004153 0.5832643  0.6408498      0.8095515
22 V..20.O. organizacional     0.5262808 0.5798335  0.6643261      0.8208758
23 V..21.O. organizacional     0.5466408 0.6273981  0.6573460      0.8092401

flex_pls_2$inner_model	# Quarta análise

$grupo
                  Estimate Std. Error      t value     Pr(>|t|)
Intercept     1.069862e-16 0.05400626 1.980996e-15 1.000000e+00
flexibilidade 5.986077e-01 0.05400626 1.108404e+01 5.673654e-23

$individual
                  Estimate Std. Error      t value     Pr(>|t|)
Intercept     1.920371e-16 0.05575616 3.444231e-15 1.000000e+00
flexibilidade 5.622055e-01 0.05575616 1.008329e+01 6.716373e-20

$organizacional
                Estimate Std. Error       t value     Pr(>|t|)
Intercept  -2.742755e-17 0.03933917 -6.972072e-16 1.000000e+00
grupo       3.878284e-01 0.05577516  6.953425e+00 4.049521e-11
individual  4.907457e-01 0.05577516  8.798643e+00 4.195128e-16


flex_pls_2$effects	# Quinta análise

                    relationships    direct indirect     total
1          flexibilidade -> grupo 0.5986077 0.000000 0.5986077
2     flexibilidade -> individual 0.5622055 0.000000 0.5622055
3 flexibilidade -> organizacional 0.0000000 0.508057 0.5080570
4             grupo -> individual 0.0000000 0.000000 0.0000000
5         grupo -> organizacional 0.3878284 0.000000 0.3878284
6    individual -> organizacional 0.4907457 0.000000 0.4907457


flex_pls_2$inner_summary	# Sexta análise

AVE(average variance extracted > 0.50)
Block communality > 50 para cada bloco 


                     Type        R2 Block_Communality Mean_Redundancy       AVE
flexibilidade   Exogenous 0.0000000         0.6036804       0.0000000 0.6036804
grupo          Endogenous 0.3583311         0.7386407       0.2646780 0.7386407
individual     Endogenous 0.3160750         0.6284979       0.1986525 0.6284979
organizacional Endogenous 0.6610822         0.6503456       0.4299319 0.6503456


flex_pls_2$gof	# Sétima análise

abaixo da média dentro da comunidade PLS-Pm

0.5354252

 # Bootstrap

flex_pls_3<-plspm(Data = data,path_matrix = flex_path,blocks =  flex_block_2,modes = flex_mode,boot.val = TRUE,br=2000)


flex_pls_3$boot # Oitava análise

$weights
                         Original Mean.Boot   Std.Error  perc.025  perc.975
flexibilidade-V..23.R.  0.2758176 0.2759521 0.026901114 0.2286311 0.3363192
flexibilidade-V..24.R.  0.2230576 0.2226054 0.022522500 0.1799743 0.2677743
flexibilidade-V..27.C.  0.1941931 0.1937972 0.015401874 0.1629557 0.2228659
flexibilidade-V..28.C.  0.1613648 0.1607529 0.019985050 0.1175101 0.1949909
flexibilidade-V..29.C.  0.2237083 0.2235500 0.015342493 0.1944867 0.2547437
flexibilidade-V..30.C.  0.2138879 0.2136575 0.016540625 0.1817003 0.2477747
grupo-V..7.C.           0.2992156 0.2988012 0.021790571 0.2524948 0.3371570
grupo-V..8.C.           0.3923529 0.3924464 0.015836942 0.3629690 0.4251068
grupo-V..9.C.           0.4612037 0.4612324 0.022795196 0.4223646 0.5102733
individual-V..1.I.      0.1922956 0.1915936 0.012681311 0.1659237 0.2156912
individual-V..2.I.      0.1828078 0.1819856 0.014796708 0.1504605 0.2094787
individual-V..3.I.      0.1905429 0.1906952 0.016142245 0.1576146 0.2200064
individual-V..4.I.      0.2378149 0.2376957 0.015208561 0.2106388 0.2693380
individual-V..5.I.      0.2184756 0.2188987 0.014003426 0.1928441 0.2471236
individual-V..6.I.      0.2368325 0.2375494 0.014049173 0.2119257 0.2671884
organizacional-V..13.O. 0.1548820 0.1547958 0.006857266 0.1416817 0.1686374
organizacional-V..14.O. 0.1399018 0.1399642 0.007696664 0.1245956 0.1559409
organizacional-V..15.O. 0.1468213 0.1466037 0.006738585 0.1332458 0.1597229
organizacional-V..16.O. 0.1580297 0.1579468 0.007541962 0.1438129 0.1736931
organizacional-V..18.O. 0.1587701 0.1586704 0.007418859 0.1449009 0.1739073
organizacional-V..19.O. 0.1569990 0.1572572 0.010359088 0.1367332 0.1775690
organizacional-V..20.O. 0.1595700 0.1598081 0.007412760 0.1463262 0.1753200
organizacional-V..21.O. 0.1647751 0.1647287 0.007490744 0.1512028 0.1805935

$loadings
                         Original Mean.Boot  Std.Error  perc.025  perc.975
flexibilidade-V..23.R.  0.7082729 0.7100263 0.03544397 0.6378157 0.7729687
flexibilidade-V..24.R.  0.7583822 0.7572221 0.03837220 0.6670417 0.8179783
flexibilidade-V..27.C.  0.7747250 0.7743305 0.03304250 0.7026594 0.8324292
flexibilidade-V..28.C.  0.7474171 0.7445062 0.04713833 0.6397952 0.8242201
flexibilidade-V..29.C.  0.8463804 0.8458575 0.02304481 0.7968101 0.8861060
flexibilidade-V..30.C.  0.8185947 0.8186139 0.02857339 0.7564052 0.8678587
grupo-V..7.C.           0.7500262 0.7478639 0.04602909 0.6486834 0.8264153
grupo-V..8.C.           0.9160687 0.9157556 0.01211094 0.8891391 0.9371049
grupo-V..9.C.           0.9023308 0.9030405 0.01136211 0.8781560 0.9234296
individual-V..1.I.      0.8140016 0.8117707 0.02703412 0.7534554 0.8593629
individual-V..2.I.      0.7736554 0.7711403 0.03662507 0.6872403 0.8345370
individual-V..3.I.      0.7201015 0.7205206 0.04133981 0.6323639 0.7926260
individual-V..4.I.      0.8094815 0.8097931 0.02559034 0.7562319 0.8558779
individual-V..5.I.      0.8044320 0.8038263 0.02670771 0.7478895 0.8511898
individual-V..6.I.      0.8300175 0.8304081 0.02371968 0.7784736 0.8723141
organizacional-V..13.O. 0.8171941 0.8165118 0.02436401 0.7642332 0.8619042
organizacional-V..14.O. 0.7390321 0.7393128 0.03221631 0.6729354 0.7995783
organizacional-V..15.O. 0.8380725 0.8374369 0.02347706 0.7851395 0.8779356
organizacional-V..16.O. 0.8249263 0.8244096 0.02369402 0.7740352 0.8674894
organizacional-V..18.O. 0.7885690 0.7877147 0.02863497 0.7272226 0.8383469
organizacional-V..19.O. 0.8095515 0.8088913 0.03387392 0.7358460 0.8663630
organizacional-V..20.O. 0.8208758 0.8207401 0.02702705 0.7601552 0.8685807
organizacional-V..21.O. 0.8092401 0.8090572 0.03311305 0.7347941 0.8625629

$paths
                              Original Mean.Boot  Std.Error  perc.025  perc.975
flexibilidade -> grupo       0.5986077 0.6031995 0.04163003 0.5165504 0.6816382
flexibilidade -> individual  0.5622055 0.5673956 0.04339525 0.4790360 0.6517923
grupo -> organizacional      0.3878284 0.3888536 0.05832653 0.2713128 0.5021719
individual -> organizacional 0.4907457 0.4909766 0.05317194 0.3895792 0.5948178

$rsq
                Original Mean.Boot  Std.Error  perc.025  perc.975
grupo          0.3583311 0.3655819 0.05001699 0.2668244 0.4646306
individual     0.3160750 0.3238200 0.04905171 0.2294754 0.4248331
organizacional 0.6610822 0.6656523 0.03559836 0.5913595 0.7312865

$total.efs
                                 Original Mean.Boot  Std.Error  perc.025  perc.975
flexibilidade -> grupo          0.5986077 0.6031995 0.04163003 0.5165504 0.6816382
flexibilidade -> individual     0.5622055 0.5673956 0.04339525 0.4790360 0.6517923
flexibilidade -> organizacional 0.5080570 0.5142949 0.03995515 0.4349074 0.5927668
grupo -> individual             0.0000000 0.0000000 0.00000000 0.0000000 0.0000000
grupo -> organizacional         0.3878284 0.3888536 0.05832653 0.2713128 0.5021719
individual -> organizacional    0.4907457 0.4909766 0.05317194 0.3895792 0.5948178


flex_pls_3$boot$paths # Nona análise

                              Original Mean.Boot  Std.Error  perc.025  perc.975
flexibilidade -> grupo       0.5986077 0.6031995 0.04163003 0.5165504 0.6816382
flexibilidade -> individual  0.5622055 0.5673956 0.04339525 0.4790360 0.6517923
grupo -> organizacional      0.3878284 0.3888536 0.05832653 0.2713128 0.5021719
individual -> organizacional 0.4907457 0.4909766 0.05317194 0.3895792 0.5948178

#Model type Private and Non-private companies

data[,37]<-as.factor(data[,37])

data$type<-mapvalues(data[,37],from=c("1","2","3","4","5"),to = c("Não-privado","Não-privado","Não-privado","Privado","Não-privado"))

plspm.groups(flex_pls_3,group = data[,38],method="permutation",reps = 2000)

#Décima análise

GROUP COMPARISON IN PLS-PM FOR PATH COEFFICIENTS 

Scale of Data:       TRUE 
Weighting Scheme:    centroid 
Selected method:     permutation 
Num of replicates:   2000 

$test 
                            global  group.Não-privado  group.Privado  diff.abs
flexibilidade->grupo        0.5986             0.5561         0.6328    0.0767
flexibilidade->individual   0.5622             0.5207         0.6123    0.0916
grupo->organizacional       0.3878             0.3790         0.3808    0.0018
individual->organizacional  0.4907             0.5106         0.4873    0.0233
                            p.value  sig.05
flexibilidade->grupo         0.3703      no
flexibilidade->individual    0.3023      no
grupo->organizacional        0.9875      no
individual->organizacional   0.8426      no