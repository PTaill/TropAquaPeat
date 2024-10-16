# R-script to create Figure 3 for the following article: 
# Taillardat, Moore, Sasmito, Evans, Alfina, Lok, Bandla, Cahya, Deshmukh, Dubey, Kurnianto, Swarup, Tarigan, Taufik, Lupascu, Taylor
# (Submitted). Aquatic Carbon Greenhouse Gas Production and Emission Pathways in a Tropical Peatland Plantation Forest

# The dataset used in this script are available under the following DOI: PROVIDE
# Please cite the work accordingly when using this script.

#load the packages, some may not be necessary to run the final version of the code.
library(ggplot2)
library(gridExtra)
library(ggpmisc)
library(extrafont)
library(ggbreak) 
library(patchwork)
library(dplyr)
library(lubridate)
library(ggpubr)
library(cowplot)
library(grid)
library(chron)
library(hms)
library(scales)
library(ggthemes)
library(svglite)
library(tidyverse)
library(rstatix)
library(coin)


#load the data
setwd("...") #to be replaced by your own file location
data <- read.csv('Figure3_abcdefi_Figure5.csv',header=TRUE, sep=",", dec=".")

#Unit Conversion
data$water_temp_K <-data$temperature_water_degC  + 273.15
data$Sol_CO2 <- exp(-58.0931+(90.5069*(100/data$water_temp_K))+(22.294*(log(data$water_temp_K/100)))) # in [mol L-1 atm-1]; according to Weiss, 1974 (in Goldenfum, 2010)
data$Sol_CH4 <- exp(-115.6477+(155.5756/(data$water_temp_K/100))+65.2553*(log(data$water_temp_K/100))-(6.1698*(data$water_temp_K/100)))*(1000/18.0153) # in [mol L-1 atm-1]; according to Lide, 2007 (in Goldenfum, 2010)
data$air_pressure_atm <- (data$barometric_pressure_PSI)*0.068046 # https://www.chemteam.info/GasLaw/PressureConversions.html; PSI to Atm
data$water_pCO2_uatm <- data$pCO2_ppm*data$air_pressure_atm
data$water_pCH4_uatm <- data$pCH4_ppm*data$air_pressure_atm
data$water_CO2_umolL1 <-data$water_pCO2_uatm*data$Sol_CO2 # in [umol L-1]; according to Morel, 1982; Anderson, 2002 (in Goldenfum, 2010)
data$water_CH4_umolL1 <- data$water_pCH4_uatm*data$Sol_CH4 # in [umol L-1]; according to Morel, 1982; Anderson, 2002 (in Goldenfum, 2010)
data$CO2_mgCL<-data$water_CO2_umolL*12.0107/1000
data$CH4_mgCL<-data$water_CH4_umolL*12.0107/1000
data$CH4_ugCL<-data$CH4_mgCL*1000
data$CO2CH4<-data$CO2_mgCL/data$CH4_mgCL
data$DOC_mgCL<-data$DOC_mgCL*0.9 #DOC samples were analyzed without being acidified which leads to a DOC overestimation of approxametively 10% which is corrected using this line. You can refer to the manuscript methods section and Figure S2 for further information 
data$sampling_site_edit<-(ifelse(data$sampling_site=="CANIN","Canal",ifelse(data$sampling_site=="CAN80","Ditch",
                                                                            ifelse(data$sampling_site=="CANMID","Ditch",
                                                                                   ifelse(data$sampling_site=="CAN60","Ditch",
                                                                                          ifelse(data$sampling_site=="CAN40","Ditch",
                                                                                                 ifelse(data$sampling_site=="CANOUT","Canal",data$sampling_site)))))))




###DOC
res.kruskal <- data %>% rstatix::kruskal_test(DOC_mgCL~sampling_site_edit)
res.kruskal 
stat.test.dunn <- data %>% dunn_test(DOC_mgCL~sampling_site_edit, p.adjust.method = "hochberg") 
stat.test.dunn


n_fun <- function(x){
  return(data.frame(y = 75,
                    label = paste0("n = ",length(x))))
}
a<-ggboxplot(data,x = "sampling_site_edit" , y ="DOC_mgCL", add = "jitter",fill = "sampling_site_edit", color="grey",palette =c("#999999","#D55E00","#56B4E9", "#009E73"))+
  theme(legend.position = 'none',axis.title.x=element_blank(),
        axis.text.x = element_blank(),plot.title = element_text(face = "bold"))+
  ylab(expression(paste("DOC (mg C L"^"-1",")")))+ggtitle("a)")+
  stat_summary(fun.data = n_fun, geom = "text", hjust = 0.5,size=4)+
  scale_x_discrete(limits=c("-40","-80","Ditch","Canal"),labels=c( "-40" = "WTT_Shallower","-80" = "WTT_Deeper", "Ditch","Canal")) +
  geom_text(x=1, y=149, label="a",col="red",size=6)+
  geom_text(x=2, y=147, label="a",col="red",size=6)+
  geom_text(x=3, y=149, label="a",col="red",size=6)+
  geom_text(x=4, y=135, label="b",col="red",size=6)
print(a)

###13CDOC
res.kruskal <- data %>% rstatix::kruskal_test(d13CDOC_permil~sampling_site_edit)
res.kruskal 
stat.test.dunn <- data %>% dunn_test(d13CDOC_permil~sampling_site_edit, p.adjust.method = "hochberg") 
stat.test.dunn <- stat.test.dunn %>% add_xy_position(x = "sampling_site_edit")
stat.test.dunn

n_fun <- function(x){
  return(data.frame(y = -30.2,
                    label = paste0("n = ",length(x))))
}
d<-ggboxplot(data,x = "sampling_site_edit" , y ="d13CDOC_permil", add = "jitter",fill = "sampling_site_edit", color="grey", palette =c("#999999","#D55E00","#56B4E9", "#009E73"))+
  theme(legend.position = 'none',axis.title.x=element_blank(),
        axis.text.x = element_blank(),plot.title = element_text(face = "bold"))+
  ylab(expression(paste(delta^{13}, "C-DOC (\u2030)")))+ggtitle("d)")+
  stat_summary(fun.data = n_fun, geom = "text", hjust = 0.5,size=4)+
  scale_x_discrete(limits=c("-40","-80","Ditch","Canal"),labels=c( "-40" = "WTT_Shallower","-80" = "WTT_Deeper", "Ditch","Canal"))+
  geom_text(x=1, y=-29.95, label="a",col="red",size=6)+
  geom_text(x=2, y=-29.6, label="b,c",col="red",size=6)+
  geom_text(x=3, y=-29.65, label="a,b",col="red",size=6)+
  geom_text(x=4, y=-29.42, label="c",col="red",size=6)+
  scale_y_continuous(labels = scales::number_format(accuracy = 0.1), limits = c(-30.2,-29.4))
print(d)

###Co2
res.kruskal <- data %>% rstatix::kruskal_test(CO2_mgCL~sampling_site_edit)
res.kruskal 
stat.test.dunn <- data %>% dunn_test(CO2_mgCL~sampling_site_edit, p.adjust.method = "hochberg") 
stat.test.dunn

n_fun <- function(x){
  return(data.frame(y = 0,
                    label = paste0("n = ",length(x))))
}
b<-ggboxplot(data,x = "sampling_site_edit" , y ="CO2_mgCL", add = "jitter",fill = "sampling_site_edit", color="grey", palette =c("#999999","#D55E00","#56B4E9", "#009E73"))+
  theme(legend.position = 'none',axis.title.x=element_blank(),
        axis.text.x = element_blank(),plot.title = element_text(face = "bold"))+
  ylab(expression(paste("CO"[2]," (mg C L"^"-1",")")))+ggtitle("b)")+
  stat_summary(fun.data = n_fun, geom = "text", hjust = 0.5,size=4)+
  scale_x_discrete(limits=c("-40","-80","Ditch","Canal"),labels=c( "-40" = "WTT_Shallower","-80" = "WTT_Deeper", "Ditch","Canal"))+
  geom_text(x=1, y=62.5, label="a",col="red",size=6)+
  geom_text(x=2, y=62.5, label="a",col="red",size=6)+
  geom_text(x=3, y=45, label="b",col="red",size=6)+
  geom_text(x=4, y=15, label="b",col="red",size=6)
print(b)

###d13C-CO2
res.kruskal <- data %>% rstatix::kruskal_test(d13CCO2_permil~sampling_site_edit)
res.kruskal 
stat.test.dunn <- data %>% dunn_test(d13CCO2_permil~sampling_site_edit, p.adjust.method = "hochberg") 
stat.test.dunn

n_fun <- function(x){
  return(data.frame(y = -30,
                    label = paste0("n = ",length(x))))
}
e<-ggboxplot(data,x = "sampling_site_edit" , y ="d13CCO2_permil", add = "jitter", color="grey",fill = "sampling_site_edit", palette =c("#999999","#D55E00","#56B4E9", "#009E73"))+
  theme(legend.position = 'none',axis.title.x=element_blank(),
        axis.text.x = element_blank(),plot.title = element_text(face = "bold"))+
  ylab(expression(paste(delta^{13}, "C-CO"[2]," (\u2030)")))+ggtitle("e)")+
  stat_summary(fun.data = n_fun, geom = "text", hjust = 0.5,size=4)+
  scale_x_discrete(limits=c("-40","-80","Ditch","Canal"),labels=c( "-40" = "WTT_Shallower","-80" = "WTT_Deeper", "Ditch","Canal"))+
  geom_text(x=1, y=-16.5, label="a",col="red",size=6)+
  geom_text(x=2, y=-23, label="b",col="red",size=6)+
  geom_text(x=3, y=-22, label="b",col="red",size=6)+
  geom_text(x=4, y=-20, label="a",col="red",size=6)
print(e)

###CH4_mgCL
res.kruskal <- data %>% rstatix::kruskal_test(CH4_mgCL~sampling_site_edit)
res.kruskal 
stat.test.dunn <- data %>% dunn_test(CH4_mgCL~sampling_site_edit, p.adjust.method = "hochberg") 
stat.test.dunn

n_fun <- function(x){
  return(data.frame(y = -1,
                    label = paste0("n = ",length(x))))
}
c<-ggboxplot(data,x = "sampling_site_edit" , y ="CH4_mgCL", add = "jitter", color="grey",fill = "sampling_site_edit", palette =c("#999999","#D55E00","#56B4E9", "#009E73"))+
  theme(legend.position = 'none',axis.title.x=element_blank(),
        axis.text.x = element_blank(),plot.title = element_text(face = "bold"))+
  ylab(expression(paste("CH"[4]," (mg C L"^"-1",")")))+ggtitle("c)")+
  stat_summary(fun.data = n_fun, geom = "text", hjust = 0.5,size=4)+
  scale_x_discrete(limits=c("-40","-80","Ditch","Canal"),labels=c( "-40" = "WTT_Shallower","-80" = "WTT_Deeper", "Ditch","Canal"))+
  geom_text(x=1, y=7, label="a",col="red",size=6)+
  geom_text(x=2, y=3, label="a",col="red",size=6)+
  geom_text(x=3, y=1, label="b",col="red",size=6)+
  geom_text(x=4, y=1, label="c",col="red",size=6)+ylim(-1, 8.6)
print(c)

###d13C-CH4
res.kruskal <- data %>% rstatix::kruskal_test(d13CCH4_permil~sampling_site_edit)
res.kruskal 
stat.test.dunn <- data %>% dunn_test(d13CCH4_permil~sampling_site_edit, p.adjust.method = "hochberg") 
stat.test.dunn

n_fun <- function(x){
  return(data.frame(y = -95,
                    label = paste0("n = ",length(x))))
}
f<-ggboxplot(data,x = "sampling_site_edit" , y ="d13CCH4_permil", add = "jitter", color="grey",fill = "sampling_site_edit", palette =c("#999999","#D55E00","#56B4E9", "#009E73"))+
  theme(legend.position = 'none',axis.title.x=element_blank(),axis.text.x = element_blank(),plot.title = element_text(face = "bold"))+
  ylab(expression(paste(delta^{13}, "C-CH"[4]," (\u2030)")))+ggtitle("f)")+
  stat_summary(fun.data = n_fun, geom = "text", hjust = 0.5,size=4)+
  scale_x_discrete(limits=c("-40","-80","Ditch","Canal"),labels=c( "-40" = "WTT_Shallower","-80" = "WTT_Deeper", "Ditch","Canal"))+
  geom_text(x=1, y=-50, label="a",col="red",size=6)+
  geom_text(x=2, y=-60, label="a",col="red",size=6)+
  geom_text(x=3, y=-45, label="b",col="red",size=6)+
  geom_text(x=4, y=-17, label="b",col="red",size=6)
print(f)

###CO2/CH4
res.kruskal <- data %>% rstatix::kruskal_test(CO2CH4~sampling_site_edit)
res.kruskal 
stat.test.dunn <- data %>% dunn_test(CO2CH4~sampling_site_edit, p.adjust.method = "hochberg") 
stat.test.dunn

n_fun <- function(x){
  return(data.frame(y = 0,
                    label = paste0("n = ",length(x))))
}
i<-ggboxplot(data,x = "sampling_site_edit" , y ="CO2CH4", add = "jitter",fill = "sampling_site_edit", color="grey", palette =c("#999999","#D55E00","#56B4E9", "#009E73"))+
  theme(legend.position = 'none',
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 25, hjust = 1,colour = "black",size=12),
        plot.title = element_text(face = "bold"))+
  scale_y_log10() +
  ylab(expression(paste("CO"[2]," / CH"[4],"")))+ggtitle("i)")+
  stat_summary(fun.data = n_fun, geom = "text", hjust = 0.5,size=4)+
  scale_x_discrete(limits=c("-40","-80","Ditch","Canal"),labels=c( "-40" = "WTT_Shallower","-80" = "WTT_Deeper", "Ditch","Canal"))+
  geom_text(x=1, y=2, label="a",col="red",size=6)+
  geom_text(x=2, y=2.4, label="b",col="red",size=6)+
  geom_text(x=3, y=3.2, label="c",col="red",size=6)+
  geom_text(x=4, y=3.8, label="d",col="red",size=6)
print(i)

## Reading radiocarbon data
setwd("C:/Users/aseuser/OneDrive - Nanyang Technological University/Postdoc2/Papers/APRIL_Aquatic/Version 10/Figure 3") #to be replaced by your own file location
data<-read.table("Figure3_gh.csv",sep=",", header=T)
head(data)

data_DOC <- subset(data, C_species =="DOC")
data_CO2 <- subset(data, C_species =="CX")

# Calculate the mean F14C for each site
data_summary_DOC <- data_DOC %>%
  group_by(Site) %>%
  summarise(mean_F14C = mean(F14C),
            mean_Age = mean(Age),
            se_F14C = sd(F14C) / sqrt(n()),
            se_Age = sd(Age) / sqrt(n()))

data_summary_CO2 <- data_CO2 %>%
  group_by(Site) %>%
  summarise(mean_F14C = mean(F14C),
            mean_Age = mean(Age),
            se_F14C = sd(F14C) / sqrt(n()),
            se_Age = sd(Age) / sqrt(n()))

# Create a bar plot 
g<- ggplot(data_summary_DOC, aes(x = Site, y = mean_F14C, fill = Site)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.7) +
  geom_errorbar(aes(ymin = mean_F14C - se_F14C, ymax = mean_F14C + se_F14C),
                width = 0.25,  # Adjust the width of error bars as needed
                position = position_dodge(width = 0.7),colour="grey") +  # Match the bar width
  coord_cartesian(ylim = c(0.9, 1)) +  # Set y-axis limits from 0.9 to 1
  theme_classic() + theme(legend.position = 'none',
                          axis.title.x = element_blank(),
                          axis.text.x = element_text(angle = 25, hjust = 1,colour = "black",size=12),
                          axis.text.y = element_text(colour = "black",size=12),
                          plot.title = element_text(face = "bold"), text = element_text(size=12))+
  ylab(expression(paste("F"^14 * "C - DOC", "(fraction modern)"))) +
  ggtitle("g)") +
  scale_x_discrete(limits = c("WTT40", "WTT80", "Ditch", "Canal"),
                   labels = c("WTT40" = "WTT_Shallower", "WTT80" = "WTT_Deeper", "Ditch", "Canal"))+
  scale_fill_manual(values = c("#999999","#56B4E9","#009E73","#D55E00")) +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01))+
  geom_hline(yintercept = 1, linetype = "dotted", color = "grey")+
  geom_text(x=1, y=.92, label="291 ± 26 BP",size=4,angle=90)+
  geom_text(x=2, y=.92, label="410 ± 21 BP",size=4,angle=90)+
  geom_text(x=3, y=.92, label="420 ± 51 BP",size=4,angle=90)+
  geom_text(x=4, y=.92, label="331 ± 30 BP",size=4,angle=90)
print(g)

h<- ggplot(data_summary_CO2, aes(x = Site, y = mean_F14C, fill = Site)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.7) +
  geom_errorbar(aes(ymin = mean_F14C - se_F14C, ymax = mean_F14C + se_F14C),
                width = 0.25,  # Adjust the width of error bars as needed
                position = position_dodge(width = 0.7),colour="grey") +  # Match the bar width
  coord_cartesian(ylim = c(0.9, 1)) +  # Set y-axis limits from 0.9 to 1
  theme_classic() + theme(legend.position = 'none',
                          axis.title.x = element_blank(),
                          axis.text.x = element_text(angle = 25, hjust = 1, colour = "black",size=12),
                          axis.text.y = element_text(colour = "black",size=12),
                          plot.title = element_text(face = "bold"), text = element_text(size=12))+
  ylab(expression(paste("F"^14 * "C - CO"[2], " (fraction modern)"))) +
  ggtitle("h)") +
  scale_x_discrete(limits = c("WTT40", "WTT80", "Ditch", "Canal"),
                   labels = c("WTT40" = "WTT_Shallower", "WTT80" = "WTT_Deeper", "Ditch", "Canal"))+
  scale_fill_manual(values = c("#999999","#56B4E9","#009E73","#D55E00")) +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01))+
  geom_hline(yintercept = 1, linetype = "dotted", color = "grey")+
  geom_text(x=1, y=.95, label="614 ± 9 BP",size=4,angle=90)+
  geom_text(x=2, y=.93, label="842 ± 21 BP",size=4,angle=90)+
  geom_text(x=3, y=.945, label="688 ± NA BP",size=4,angle=90)+
  geom_text(x=4, y=.935, label="764 ± NA BP",size=4,angle=90)
print(h)

#Offload the figure
setwd("...")  #to be replaced by your own file location
tiff(("20241015_Figure3.tiff"), height = 25, width = 25, units = 'cm', compression = "lzw", res = 600)
ggarrange(a,b,c,d,e,f,g,h,i,ncol = 3, nrow = 3,heights = c(1, 1, 1, 1.2),align="v",common.legend = F)
dev.off()


