# R-script to create Figure 4 for the following article: 
# Taillardat, Moore, Sasmito, Evans, Alfina, Lok, Bandla, Cahya, Deshmukh, Dubey, Kurnianto, Swarup, Tarigan, Taufik, Lupascu, Taylor
# (Submitted). Aquatic Carbon Greenhouse Gas Production and Emission Pathways in a Tropical Peatland Plantation Forest

# The dataset used in this script are available under the following DOI: PROVIDE
# Please cite the work accordingly when using this script.
#Please reach out to me if you spot any bug, missing or inacurrate information: pierre.taillardat@ntu.edu.sg

#Load packages, some may not be necessary to run the final version of the code.
library(ggplot2)
library(ggpmisc)
library(extrafont)
library(ggbreak) 
library(patchwork)
library(dplyr)
library(lubridate)
library(pastecs)
library(cowplot)
library(grid)
library(gridExtra)
library(chron)
library(hms)
library(scales)
library(ggthemes)
library(svglite)
library(tidyverse)
library(rstatix)
library(ggpubr)
library(coin)


setwd("C:/Users/aseuser/OneDrive - Nanyang Technological University/Postdoc2/Papers/APRIL_Aquatic/Version 10/Figure 4") #to be replaced by your own file location
data<- read.csv('Figure4.csv',header=TRUE, sep=",", dec=".",skip=2)

data$DOC_conc_mean_median_mg.L.1<-ifelse(is.na(data$DOC_conc_mean_mg.L.1),data$DOC_conc_median_mg.L.1,data$DOC_conc_mean_mg.L.1)
data$d13CDOC<-ifelse(is.na(data$X13C.DOC_mean_.),data$X13C.DOC_median_.,data$X13C.DOC_mean_.)
data$CO2_mean_median_umolL<-ifelse(is.na(data$Dissolved_CO2_mean_umolL),data$Dissolved_CO2_median_umolL,data$Dissolved_CO2_mean_umolL)
data$CO2_mean_median_mgCL<-data$CO2_mean_median_umolL*12.0107/1000
data$d13CO2<-ifelse(is.na(data$X13C.CO2_dissolved_mean_.),data$X13C.CO2_dissolved_median_.,data$X13C.CO2_dissolved_mean_.)
data$CH4_mean_median_ugCL<-ifelse(is.na(data$Dissolved_CH4_mean_ug.L.1),data$Dissolved_CH4_median_ug.L.1,data$Dissolved_CH4_mean_ug.L.1)
data$CH4_mean_median_mgCL<-data$CH4_mean_median_ugCL/1000
data$Co2CH4<-data$CO2_mean_median_mgCL/data$CH4_mean_median_mgCL
data$d13CH4<-ifelse(is.na(data$X13C.CH4_dissolved_mean_.),data$X13C.CH4_dissolved_median_.,data$X13C.CH4_dissolved_mean_.)
data$d14DOC<-ifelse(is.na(data$X14C.DOC_mean_.modern),data$X14C.DOC_median_.modern,data$X14C.DOC_mean_.modern)
data$d14CO2<-ifelse(is.na(data$X14C.CO2_mean_..modern),data$X14C.CO2_median_..modern,data$X14C.CO2_mean_..modern)
data$Land.use.type<-ifelse(data$Study.ID=="A12","THIS STUDY",data$Land.use.type)
data$Aquatic.component<-ifelse(data$Aquatic.component=="Canal","Ditches and\nCanals",data$Aquatic.component)
data$Aquatic.component <- factor(data$Aquatic.component , levels=c("Porewater", "Ditches and\nCanals"))
data$Land.use.type <- factor(data$Land.use.type , levels=c("Least Disturbed", "Managed", "Degraded", "THIS STUDY"))


# grouped boxplot
res.kruskal <- data %>% rstatix::kruskal_test(DOC_conc_mean_median_mg.L.1~Aquatic.component)
res.kruskal 
stat.test.dunn <- data %>% dunn_test(DOC_conc_mean_median_mg.L.1~Aquatic.component, p.adjust.method = "hochberg") 
stat.test.dunn


n_fun <- function(x){
  return(data.frame(y = -12,
                    label = paste0("n = ",length(x))))
}
a<-ggboxplot(data,x = "Aquatic.component", y = "DOC_conc_mean_median_mg.L.1", fill = "Aquatic.component", palette =c("#CC79A7", "#0072B2"), 
             outlier.shape = NA,color = "grey")+
  theme(legend.position = 'none',axis.title.x=element_blank(),
        axis.text.x = element_blank(),plot.title = element_text(face = "bold"))+
  ylab(expression(paste("DOC (mg C L"^"-1",")")))+ggtitle("a)")+
  stat_summary(fun.data = n_fun, geom = "text", hjust = 0.5,size=4)+
  geom_text(x=1, y=120, label="a",col="red",size=6)+
  geom_text(x=2, y=72, label="b",col="red",size=6)+
  ylim(-13,150)+
 geom_jitter(data = data[data$Land.use.type == "THIS STUDY",], 
           aes(x = Aquatic.component, y = DOC_conc_mean_median_mg.L.1), 
            color = "black",size=2)+
  geom_jitter(data = data[data$Land.use.type != "THIS STUDY",], 
             aes(x = Aquatic.component, y = DOC_conc_mean_median_mg.L.1),
             color = "grey")
a


plotres.kruskal <- data %>% rstatix::kruskal_test(d13CDOC~Aquatic.component)
res.kruskal 
stat.test.dunn <- data %>% dunn_test(d13CDOC~Aquatic.component, p.adjust.method = "hochberg") 
stat.test.dunn

n_fun <- function(x){
  return(data.frame(y = -30.9,
                    label = paste0("n = ",length(x))))
}
d<-ggboxplot(data ,x = "Aquatic.component" , y ="d13CDOC", fill = "Aquatic.component", palette =c("#CC79A7", "#0072B2"),outlier.shape = NA,color = "grey")+
  theme(legend.position = 'none',axis.title.x=element_blank(),
        axis.text.x = element_blank(),plot.title = element_text(face = "bold"))+
  ylab(expression(paste(delta^{13}, "C-DOC (\u2030)")))+ggtitle("d)")+
  stat_summary(fun.data = n_fun, geom = "text", hjust = 0.5,size=4)+
  geom_text(x=1, y=-29.2, label="a",col="red",size=6)+
  geom_text(x=2, y=-26.6, label="a",col="red",size=6)+
  scale_y_continuous(limits = c(-31,-26))+
  geom_jitter(data = data[data$Land.use.type != "THIS STUDY",], 
              aes(x = Aquatic.component, y = d13CDOC),
              color = "grey")+
  geom_jitter(data = data[data$Land.use.type == "THIS STUDY",], 
             aes(x = Aquatic.component, y = d13CDOC), 
             color = "black",size=2)

d


res.kruskal <- data %>% rstatix::kruskal_test(CO2_mean_median_mgCL~Aquatic.component)
res.kruskal 
stat.test.dunn <- data %>% dunn_test(CO2_mean_median_mgCL~Aquatic.component, p.adjust.method = "hochberg") 
stat.test.dunn

n_fun <- function(x){
  return(data.frame(y = -2,
                    label = paste0("n = ",length(x))))
}
b<-ggboxplot(data,x = "Aquatic.component" , y ="CO2_mean_median_mgCL",fill = "Aquatic.component", color="grey",outlier.shape = NA, palette =c("#CC79A7", "#0072B2"))+
  theme(legend.position = 'none',axis.title.x=element_blank(),
        axis.text.x = element_blank(),plot.title = element_text(face = "bold"))+
  ylab(expression(paste("CO"[2]," (mg C L"^"-1",")")))+ggtitle("b)")+
  stat_summary(fun.data = n_fun, geom = "text", hjust = 0.5,size=4)+
  geom_text(x=1, y=22, label="a",col="red",size=6)+
  geom_text(x=2, y=25, label="b",col="red",size=6)+
  geom_jitter(data = data[data$Land.use.type == "THIS STUDY",], 
             aes(x = Aquatic.component, y = CO2_mean_median_mgCL), 
             color = "black",size=2)+
  geom_jitter(data = data[data$Land.use.type != "THIS STUDY",], 
              aes(x = Aquatic.component, y = CO2_mean_median_mgCL),
              color = "grey")
b


res.kruskal <- data %>% rstatix::kruskal_test(d13CO2~Aquatic.component)
res.kruskal 
stat.test.dunn <- data %>% dunn_test(d13CO2~Aquatic.component, p.adjust.method = "hochberg") 
stat.test.dunn

n_fun <- function(x){
  return(data.frame(y = -27.8,
                    label = paste0("n = ",length(x))))
}
e<-ggboxplot(data,x = "Aquatic.component" , y ="d13CO2",outlier.shape = NA, color="grey",fill = "Aquatic.component", palette =c("#CC79A7", "#0072B2"))+
  theme(legend.position = 'none',axis.title.x=element_blank(),
        axis.text.x = element_blank(),plot.title = element_text(face = "bold"))+
  ylab(expression(paste(delta^{13}, "C-CO"[2]," (\u2030)")))+ggtitle("e)")+
  stat_summary(fun.data = n_fun, geom = "text", hjust = 0.5,size=4)+
  geom_text(x=1, y=-18.5, label="a",col="red",size=6)+
  geom_text(x=2, y=-12, label="a",col="red",size=6)+ylim(-28,-12)+
  geom_jitter(data = data[data$Land.use.type == "THIS STUDY",], 
             aes(x = Aquatic.component, y = d13CO2), 
             color = "black",size=2)+
  geom_jitter(data = data[data$Land.use.type != "THIS STUDY",], 
              aes(x = Aquatic.component, y = d13CO2),
              color = "grey")
print(e)


res.kruskal <- data %>% rstatix::kruskal_test(CH4_mean_median_mgCL~Aquatic.component)
res.kruskal 
stat.test.dunn <- data %>% dunn_test(CH4_mean_median_mgCL~Aquatic.component, p.adjust.method = "hochberg") 
stat.test.dunn

n_fun <- function(x){
  return(data.frame(y = -0.25,
                    label = paste0("n = ",length(x))))
}
c<-ggboxplot(data,x = "Aquatic.component" , y ="CH4_mean_median_mgCL",outlier.shape = NA, color="grey",fill = "Aquatic.component", palette =c("#CC79A7", "#0072B2"))+
  theme(legend.position = 'none',axis.title.x=element_blank(),
        axis.text.x = element_blank(),plot.title = element_text(face = "bold"))+
  ylab(expression(paste("CH"[4]," (mg C L"^"-1",")")))+ggtitle("c)")+
  stat_summary(fun.data = n_fun, geom = "text", hjust = 0.5,size=4)+
  geom_text(x=1, y=.4, label="a",col="red",size=6)+
  geom_text(x=2, y=.4, label="a",col="red",size=6)+
  geom_jitter(data = data[data$Land.use.type == "THIS STUDY",], 
             aes(x = Aquatic.component, y = CH4_mean_median_mgCL), 
             color = "black",size=2)+
  geom_jitter(data = data[data$Land.use.type != "THIS STUDY",], 
              aes(x = Aquatic.component, y = CH4_mean_median_mgCL),
              color = "grey")
print(c)

res.kruskal <- data %>% rstatix::kruskal_test(d13CH4~Aquatic.component)
res.kruskal 
stat.test.dunn <- data %>% dunn_test(d13CH4~Aquatic.component, p.adjust.method = "hochberg") 
stat.test.dunn

n_fun <- function(x){
  return(data.frame(y = -95,
                    label = paste0("n = ",length(x))))
}
f<-ggboxplot(data,x = "Aquatic.component" , y ="d13CH4",outlier.shape = NA, color="grey",fill = "Aquatic.component", palette =c("#CC79A7", "#0072B2"))+
  theme(legend.position = 'none',axis.title.x=element_blank(),axis.text.x = element_blank(),plot.title = element_text(face = "bold"))+
  ylab(expression(paste(delta^{13}, "C-CH"[4]," (\u2030)")))+ggtitle("f)")+
  stat_summary(fun.data = n_fun, geom = "text", hjust = 0.5,size=4)+
  geom_text(x=1, y=-54, label="a",col="red",size=6)+
  geom_text(x=2, y=-44, label="b",col="red",size=6)+
  ylim(-98,-30)+
  geom_jitter(data = data[data$Land.use.type == "THIS STUDY",], 
             aes(x = Aquatic.component, y = d13CH4), 
             color = "black",size=2)+
  geom_jitter(data = data[data$Land.use.type != "THIS STUDY",], 
              aes(x = Aquatic.component, y = d13CH4),
              color = "grey")
print(f)


res.kruskal <- data %>% rstatix::kruskal_test(d14DOC~Aquatic.component)
res.kruskal 
stat.test.dunn <- data %>% dunn_test(d14DOC~Aquatic.component, p.adjust.method = "hochberg") 
stat.test.dunn

n_fun <- function(x){
  return(data.frame(y = 55,
                    label = paste0("n = ",length(x))))
}
g<-ggboxplot(data,x = "Aquatic.component" , y ="d14DOC",outlier.shape = NA, color="grey",fill = "Aquatic.component", palette =c("#CC79A7", "#0072B2"))+
  theme(legend.position = 'none',axis.title.x=element_blank(),axis.text.x = element_text(colour = "black",size=12),
        plot.title = element_text(face = "bold"))+
  ylab(expression(paste('F'^14*'C DOC (fraction modern)')))+xlab("")+ggtitle("g)")+
  stat_summary(fun.data = n_fun, geom = "text", hjust = 0.5,size=4)+
  geom_text(x=1, y=105, label="a",col="red",size=6)+
  geom_text(x=2, y=116, label="a",col="red",size=6)+
  ylim(55,115)+
  geom_jitter(data = data[data$Land.use.type == "THIS STUDY",], 
             aes(x = Aquatic.component, y = d14DOC), 
             color = "black",size=2)+
  geom_jitter(data = data[data$Land.use.type != "THIS STUDY",], 
              aes(x = Aquatic.component, y = d14DOC),
              color = "grey")
print(g)


res.kruskal <- data %>% rstatix::kruskal_test(d14CO2~Aquatic.component)
res.kruskal 
stat.test.dunn <- data %>% dunn_test(d14CO2~Aquatic.component, p.adjust.method = "hochberg") 
stat.test.dunn

n_fun <- function(x){
  return(data.frame(y = 90,
                    label = paste0("n = ",length(x))))
}
h<-ggboxplot(data,x = "Aquatic.component" , y ="d14CO2",outlier.shape = NA, color="grey",fill = "Aquatic.component", palette =c("#CC79A7", "#0072B2"))+
  theme(legend.position = 'none',axis.title.x=element_blank(),axis.text.x = element_text(colour = "black",size=12),
        plot.title = element_text(face = "bold"))+
  ylab(expression(paste('F'^14*'C CO'[2],' (fraction modern)')))+xlab("")+ggtitle("h)")+
  stat_summary(fun.data = n_fun, geom = "text", hjust = 0.5,size=4)+
  geom_text(x=1, y=96, label="a",col="red",size=6)+
  geom_text(x=2, y=99, label="a",col="red",size=6)+
  geom_jitter(data = data[data$Land.use.type == "THIS STUDY",], 
             aes(x = Aquatic.component, y = d14CO2), 
             color = "black",size=2)+
  geom_jitter(data = data[data$Land.use.type != "THIS STUDY",], 
              aes(x = Aquatic.component, y = d14CO2),
              color = "grey")
print(h)


res.kruskal <- data %>% rstatix::kruskal_test(Co2CH4~Aquatic.component)
res.kruskal 
stat.test.dunn <- data %>% dunn_test(Co2CH4~Aquatic.component, p.adjust.method = "hochberg") 
stat.test.dunn

n_fun <- function(x){
  return(data.frame(y = 1,
                    label = paste0("n = ",length(x))))
}
i<-ggboxplot(data,x = "Aquatic.component" , y ="Co2CH4",outlier.shape = NA,fill = "Aquatic.component", color="grey", palette =c("#CC79A7", "#0072B2"))+
  theme(legend.position = 'none',
        axis.title.x = element_blank(),
        axis.text.x = element_text(colour = "black",size=12),
        plot.title = element_text(face = "bold"))+
  scale_y_log10(labels = scales::label_log()) +
  ylab(expression(paste("CO"[2]," / CH"[4],"")))+ggtitle("i)")+
  stat_summary(fun.data = n_fun, geom = "text", hjust = 0.5,size=4)+
  geom_text(x=1, y=6.9, label="a",col="red",size=6)+
  geom_text(x=2, y=2.7, label="a",col="red",size=6)+
  geom_jitter(data = data[data$Land.use.type == "THIS STUDY",], 
             aes(x = Aquatic.component, y = Co2CH4), 
             color = "black",size=2)+
  geom_jitter(data = data[data$Land.use.type != "THIS STUDY",], 
              aes(x = Aquatic.component, y = Co2CH4),
              color = "grey")
print(i)

# Arrange the plots with ggarrange
setwd("C:/Users/aseuser/OneDrive - Nanyang Technological University/Postdoc2/Papers/APRIL_Aquatic/Version 10/Figure 4")
tiff(("20241016_Figure4.tiff"), height = 25, width = 25, units = 'cm', compression = "lzw", res = 600)
ggarrange(a,b,c,d,e,f,g,h,i,ncol = 3, nrow = 3,heights = c(1, 1, 1, 1.2), widths = c(1, 1, 1.1),align="v",common.legend = F)
dev.off()
