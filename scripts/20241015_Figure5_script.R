# R-script to create Figure 4 for the following article: 
# Taillardat, Moore, Sasmito, Evans, Alfina, Lok, Bandla, Cahya, Deshmukh, Dubey, Kurnianto, Swarup, Tarigan, Taufik, Lupascu, Taylor
# (Submitted). Aquatic Carbon Greenhouse Gas Production and Emission Pathways in a Tropical Peatland Plantation Forest

# The dataset used in this script are available under the following DOI: PROVIDE
# Please cite the work accordingly when using this script.
#Please reach out to me if you spot any bug, missing or inacurrate information: pierre.taillardat@ntu.edu.sg

#Load packages, some may not be necessary to run the final version of the code.
library(ggpmisc)
library(dplyr)
library(lubridate)
library(ggplot2)
library(ggpubr)
library(cowplot)
library(grid)
library(gridExtra)
library(pastecs)
library(chron)
library(hms)
library(scales)
library(ggthemes)
library(tidyr)

####
#load the data
setwd("...") #to be replaced by your own file sampling_plot
data <- read.csv('Figure3_abcdefi_Figure5.csv',header=TRUE, sep=",", dec=".")

#Unit Conversion
data$water_temp_K <-data$temperature_water_degC + 273.15
data$Sol_CO2 <- exp(-58.0931+(90.5069*(100/data$water_temp_K))+(22.294*(log(data$water_temp_K/100)))) # in [mol L-1 atm-1]; according to Weiss, 1974 (in Goldenfum, 2010)
data$Sol_CH4 <- exp(-115.6477+(155.5756/(data$water_temp_K/100))+65.2553*(log(data$water_temp_K/100))-(6.1698*(data$water_temp_K/100)))*(1000/18.0153) # in [mol L-1 atm-1]; according to Lide, 2007 (in Goldenfum, 2010)
data$air_pressure_atm <- ifelse(is.na(data$barometric_pressure_PSI),(14.61856)*0.068046,data$barometric_pressure_PSI*0.068046) # https://www.google.com/search?q=psi+to+atm&rlz=1C1GCEB_enSG1124SG1124&oq=psi+to+atm&gs_lcrp=EgZjaHJvbWUyCQgAEEUYORiABDIGCAEQBhhAMgcIAhAAGIAEMgcIAxAAGIAEMgcIBBAAGIAEMgcIBRAAGIAEMgcIBhAAGIAEMgcIBxAAGIAEMgcICBAAGIAEMgcICRAAGIAE0gEINDE4NGowajeoAgCwAgA&sourceid=chrome&ie=UTF-8; PSI to Atm
data$water_pCO2_uatm <- data$pCO2_ppm*data$air_pressure_atm
data$water_pCH4_uatm <- data$pCH4_ppm*data$air_pressure_atm
data$water_CO2_umolL1 <-data$water_pCO2_uatm*data$Sol_CO2 # in [umol L-1]; according to Morel, 1982; Anderson, 2002 (in Goldenfum, 2010)
data$water_CH4_umolL1 <- data$water_pCH4_uatm*data$Sol_CH4 # in [umol L-1]; according to Morel, 1982; Anderson, 2002 (in Goldenfum, 2010)
data$CO2_mgCL<-data$water_CO2_umolL*12.0107/1000
data$CH4_mgCL<-data$water_CH4_umolL*12.0107/1000
data$DOC_mgCL<-data$DOC_mgCL*0.9 #DOC samples were analyzed without being acidified which leads to a DOC overestimation of approxametively 10% which is corrected using this line. You can refer to the manuscript methods section and Figure S2 for further information 


#Calculate median values 
A <- aggregate(data$DOC_mgCL[data$month=="22_02"], by=list(data$sampling_plot[data$month=="22_02"]),FUN = median, na.rm=TRUE)
A$DOC_2202<-A$x
B <- aggregate(data$DOC_mgCL[data$month=="22_05"], by=list(data$sampling_plot[data$month=="22_05"]),FUN = median, na.rm=TRUE)
B$DOC_2205<-B$x
C <- aggregate(data$DOC_mgCL[data$month=="22_08"], by=list(data$sampling_plot[data$month=="22_08"]),FUN = median, na.rm=TRUE)
C$DOC_2208<-C$x
D <- aggregate(data$DOC_mgCL[data$month=="22_11"], by=list(data$sampling_plot[data$month=="22_11"]),FUN = median, na.rm=TRUE)
D$DOC_2211<-D$x

E<- aggregate(data$d13CDOC_permil[data$month=="22_02"], by=list(data$sampling_plot[data$month=="22_02"]),FUN = median, na.rm=TRUE)
E$d13DOC_2202<-E$x
G <- aggregate(data$d13CDOC_permil[data$month=="22_05"], by=list(data$sampling_plot[data$month=="22_05"]),FUN = median, na.rm=TRUE)
G$d13DOC_2205<-G$x
H <- aggregate(data$d13CDOC_permil[data$month=="22_08"], by=list(data$sampling_plot[data$month=="22_08"]),FUN = median, na.rm=TRUE)
H$d13DOC_2208<-H$x
I <- aggregate(data$d13CDOC_permil[data$month=="22_11"], by=list(data$sampling_plot[data$month=="22_11"]),FUN = median, na.rm=TRUE)
I$d13DOC_2211<-I$x


J <- aggregate(data$CO2_mgCL[data$month=="22_02"], by=list(data$sampling_plot[data$month=="22_02"]),FUN = median, na.rm=TRUE)
J$CO2_2202<-J$x
K <- aggregate(data$CO2_mgCL[data$month=="22_05"], by=list(data$sampling_plot[data$month=="22_05"]),FUN = median, na.rm=TRUE)
K$CO2_2205<-K$x
L <- aggregate(data$CO2_mgCL[data$month=="22_08"], by=list(data$sampling_plot[data$month=="22_08"]),FUN = median, na.rm=TRUE)
L$CO2_2208<-L$x
M <- aggregate(data$CO2_mgCL[data$month=="22_11"], by=list(data$sampling_plot[data$month=="22_11"]),FUN = median, na.rm=TRUE)
M$CO2_2211<-M$x

N <- aggregate(data$d13CCO2_permil[data$month=="22_02"], by=list(data$sampling_plot[data$month=="22_02"]),FUN = median, na.rm=TRUE)
N$d13CCO2_2202<-N$x
O <- aggregate(data$d13CCO2_permil[data$month=="22_05"], by=list(data$sampling_plot[data$month=="22_05"]),FUN = median, na.rm=TRUE)
O$d13CCO2_2205<-O$x
P <- aggregate(data$d13CCO2_permil[data$month=="22_08"], by=list(data$sampling_plot[data$month=="22_08"]),FUN = median, na.rm=TRUE)
P$d13CCO2_2208<-P$x
Q <- aggregate(data$d13CCO2_permil[data$month=="22_11"], by=list(data$sampling_plot[data$month=="22_11"]),FUN = median, na.rm=TRUE)
Q$d13CCO2_2211<-Q$x

R <- aggregate(data$CH4_mgCL[data$month=="22_02"], by=list(data$sampling_plot[data$month=="22_02"]),FUN = median, na.rm=TRUE)
R$CH4_2202<-R$x
S <- aggregate(data$CH4_mgCL[data$month=="22_05"], by=list(data$sampling_plot[data$month=="22_05"]),FUN = median, na.rm=TRUE)
S$CH4_2205<-S$x
U <- aggregate(data$CH4_mgCL[data$month=="22_08"], by=list(data$sampling_plot[data$month=="22_08"]),FUN = median, na.rm=TRUE)
U$CH4_2208<-U$x
V <- aggregate(data$CH4_mgCL[data$month=="22_11"], by=list(data$sampling_plot[data$month=="22_11"]),FUN = median, na.rm=TRUE)
V$CH4_2211<-V$x

W <- aggregate(data$d13CCH4_permil[data$month=="22_02"], by=list(data$sampling_plot[data$month=="22_02"]),FUN = median, na.rm=TRUE)
W$d13CCH4_2202<-W$x
X <- aggregate(data$d13CCH4_permil[data$month=="22_05"], by=list(data$sampling_plot[data$month=="22_05"]),FUN = median, na.rm=TRUE)
X$d13CCH4_2205<-X$x
Y <- aggregate(data$d13CCH4_permil[data$month=="22_08"], by=list(data$sampling_plot[data$month=="22_08"]),FUN = median, na.rm=TRUE)
Y$d13CCH4_2208<-Y$x
Z <- aggregate(data$d13CCH4_permil[data$month=="22_11"], by=list(data$sampling_plot[data$month=="22_11"]),FUN = median, na.rm=TRUE)
Z$d13CCH4_2211<-Z$x


BA<-merge(A,B, by="Group.1",all=T)
BB<-merge(C,D, by="Group.1",all=T)
BC<-merge(E,G, by="Group.1",all=T)
BD<-merge(H,I, by="Group.1",all=T)
BE<-merge(J,K, by="Group.1",all=T)
BF<-merge(L,M, by="Group.1",all=T)
BG<-merge(N,O, by="Group.1",all=T)
BH<-merge(P,Q, by="Group.1",all=T)
BI<-merge(R,S, by="Group.1",all=T)
BJ<-merge(U,V, by="Group.1",all=T)
BK<-merge(W,X, by="Group.1",all=T)
BL<-merge(Y,Z, by="Group.1",all=T)

CA<-merge(BA,BB, by="Group.1",all=T)
CB<-merge(BC,BD, by="Group.1",all=T)
CC<-merge(BE,BF, by="Group.1",all=T)
CD<-merge(BG,BH, by="Group.1",all=T)
CE<-merge(BI,BJ, by="Group.1",all=T)
CF<-merge(BK,BL, by="Group.1",all=T)

DA<-merge(CA,CB, by="Group.1",all=T)
DB<-merge(CC,CD, by="Group.1",all=T)
DC<-merge(CE,CF, by="Group.1",all=T)

EB<-merge(DA,DB, by="Group.1",all=T)
EC<-DC

FA<-merge(EB,EC, by="Group.1",all=T)

Total <- FA %>% select(Group.1,DOC_2202,DOC_2205,DOC_2208,DOC_2211,d13DOC_2202,d13DOC_2205,d13DOC_2208,d13DOC_2211,
                     CO2_2202,CO2_2205,CO2_2208,CO2_2211,d13CCO2_2202,d13CCO2_2205,d13CCO2_2208,d13CCO2_2211,
                       CH4_2202,CH4_2205,CH4_2208,CH4_2211,d13CCH4_2202,d13CCH4_2205,d13CCH4_2208,d13CCH4_2211)

Total$WaterBody_Type<-ifelse(Total$Group.1=="40.09","WTT40",ifelse(Total$Group.1=="40.26","WTT40",ifelse(Total$Group.1=="40.29","WTT40",
                             ifelse(Total$Group.1=="80.05","WTT80",ifelse(Total$Group.1=="80.14","WTT80",ifelse(Total$Group.1=="80.26","WTT80",
                             ifelse(Total$Group.1=="CAN40","Ditch",ifelse(Total$Group.1=="CAN40.09","Ditch",ifelse(Total$Group.1=="CAN60","Ditch",ifelse(Total$Group.1=="CAN80","Ditch",
                             ifelse(Total$Group.1=="CANIN","Canal",ifelse(Total$Group.1=="CANMID","Ditch",ifelse(Total$Group.1=="CANOUT","Canal",NA)))))))))))))

# Create a subdataset with only the specified values in 'Group.1'
subdataset <- Total %>%
  filter(Group.1 %in% c("40.09", "40.26", "40.29","80.05","80.14","80.26","CAN40","CAN40.09","CAN60","CAN80","CANIN","CANMID","CANOUT"))

subdataset_long <- subdataset %>%
  pivot_longer(cols = c(starts_with("DOC_"),      # Select all relevant columns
                        starts_with("d13DOC_"), 
                        starts_with("CO2_"), 
                        starts_with("d13CCO2_"), 
                        starts_with("CH4_"), 
                        starts_with("d13CCH4_")),
               names_to = "Variable_month",         # Create a new column for the variable name and month
               values_to = "Value") %>%            # Store the values in one column
  separate(Variable_month,                          # Split the combined "Variable_month" column
           into = c("Variable", "month"),           # Into "Variable" and "month"
           sep = "_") %>%                          # Separate on "_"
  pivot_wider(names_from = Variable,               # Reshape back to wide format for each variable
              values_from = Value)

# Display the result
head(subdataset_long)

#######
##Stable Isotope Mass balance Model
#######

data<-subdataset_long
data$Water_Type<-ifelse(data$WaterBody_Type=="WTT40"|data$WaterBody_Type=="WTT80","Porewater","Canal and Ditch")
data <- data %>%
  rename(DOC_mgCL = DOC,
         CO2_mgCL = CO2,
         CH4_mgCL = CH4)

###Stable isotope mass balance models are from doi:10.1007/s10533-012-9813-1 and doi:10.1002/2014GB005044
hist(data$d13CCH4[data$Water_Type=="Porewater"])

#εc
data$ec<-(data$d13CCO2-data$d13CCH4)
hist(data$ec[data$Water_Type=="Porewater"])
stat.desc(data$ec[data$Water_Type=="Porewater"])

#fractionation factors for CO2→CH4 (α)
data$Frac_meth<-(data$d13CCO2+1000)/(data$d13CCH4+1000)
hist(data$Frac_meth[data$Water_Type=="Porewater"])

#gap fill the d13C-DOC values for 22_02 when no samples were collected using the Median.Median d13C-DOC value from the dataset
stat.desc(data$d13DOC) #Median.Median=-2.992500e+01
data$d13DOC<-ifelse(is.na(data$d13DOC),-2.992500e+01,data$d13DOC)

#δ13C-CO2 resulting from methanogenesis
data$d13CCO2_meth<-(data$d13DOC-(0.5*data$d13CCH4))/0.5

#δ13C–CO2 = δ13DOC X fCO2_OMdecay + δ13CCO2_meth X fCO2_meth 
data$fCO2_OMdecay<-(data$d13CCO2-data$d13CCO2_meth)/(data$d13DOC-data$d13CCO2_meth)
data$fCO2_meth<-(1-data$fCO2_OMdecay)
data$d13CCO2_calc<-data$fCO2_OMdecay*data$d13DOC+data$fCO2_meth*data$d13CCO2_meth

#CH4 Transport Via Ebullition and Plant-Mediated Transport
data$CH4_Trans<-(data$fCO2_meth*data$CO2_mgCL)-data$CH4_mgCL

#ebullition+vascular<-methanogenesis-evasion
data$fCH4_Trans<-(data$CH4_Trans/(data$fCO2_meth*data$CO2_mgCL))
data$fCH4_Diff<-(data$CH4_mgCL/(data$fCO2_meth*data$CO2_mgCL))
data$fCH4_Trans+data$fCH4_Diff

#CH4 loss => from 10.1002/2014GB004951
data$CO2_meth<-data$fCO2_meth*data$CO2_mgCL
data$CH4loss_percent<-1-(data$CH4_mgCL/data$CO2_meth)

#First, determine the Median.Median d13CCH4 proewater value for each sampling period
stat.desc(data$d13CCH4[data$Water_Type=="Porewater"&data$month=="2202"]) #-73.62023407
stat.desc(data$d13CCH4[data$Water_Type=="Porewater"&data$month=="2205"]) #-79.28569180
stat.desc(data$d13CCH4[data$Water_Type=="Porewater"&data$month=="2208"]) #-76.86316891
stat.desc(data$d13CCH4[data$Water_Type=="Porewater"&data$month=="2211"]) #-73.94287576

data$d13CCH4_pw_month<-ifelse(data$month=="2202",-73.62023407,ifelse(data$month=="2205",-79.28569180,ifelse(data$month=="2208",-76.86316891,ifelse(data$month=="2211",-73.94287576,NA))))

#Second, calculate fox
data$fox<-(ifelse((data$Water_Type=="Ditch"|data$Water_Type=="Canal"),(data$d13CCH4-data$d13CCH4_pw_month)/(1000*(1.031-1.001)),NA)) # Throckmorton et al. (2015)
data$fdiff<-1-data$fox

summary(data$fCO2_OMdecay[data$WaterBody_Type=="WTT40"])
summary(data$fCO2_OMdecay[data$WaterBody_Type=="WTT80"])
summary(data$fCO2_meth[data$WaterBody_Type=="WTT40"])
summary(data$fCO2_meth[data$WaterBody_Type=="WTT80"])


# Create a summary table for fCO2_OMdecay, fCH4_Tran, and fCH4_Diff based on WaterBody_Type values

# Filter data for WTT40 and WTT80
wtt40_data <- data[data$WaterBody_Type == "WTT40", ]
wtt80_data <- data[data$WaterBody_Type == "WTT80", ]

# Function to calculate Q1, Median, and Q3 for a given variable
get_summary_stats <- function(x) {
  summary_stats <- summary(x)
  c(Q1 = summary_stats["1st Qu."], Median = summary_stats["Median"], Q3 = summary_stats["3rd Qu."])
}

# Calculate summary statistics for each variable
fCO2_OMdecay_wtt40 <- get_summary_stats(wtt40_data$fCO2_OMdecay)
fCO2_OMdecay_wtt80 <- get_summary_stats(wtt80_data$fCO2_OMdecay)

fCO2_meth_wtt40 <- get_summary_stats(wtt40_data$fCO2_meth)
fCO2_meth_wtt80 <- get_summary_stats(wtt80_data$fCO2_meth)

fCH4_Tran_wtt40 <- get_summary_stats(wtt40_data$fCH4_Trans)
fCH4_Tran_wtt80 <- get_summary_stats(wtt80_data$fCH4_Trans)

fCH4_Diff_wtt40 <- get_summary_stats(wtt40_data$fCH4_Diff)
fCH4_Diff_wtt80 <- get_summary_stats(wtt80_data$fCH4_Diff)

# Combine into a single summary table
summary_table <- data.frame(
  WaterBody_Type = c("WTT40", "WTT80"),
  fCO2_OMdecay_Q1 = c(fCO2_OMdecay_wtt40["Q1.1st Qu."], fCO2_OMdecay_wtt80["Q1.1st Qu."]),
  fCO2_OMdecay_Median = c(fCO2_OMdecay_wtt40["Median.Median"], fCO2_OMdecay_wtt80["Median.Median"]),
  fCO2_OMdecay_Q3 = c(fCO2_OMdecay_wtt40["Q3.3rd Qu."], fCO2_OMdecay_wtt80["Q3.3rd Qu."]),
  fCO2_meth_Q1 = c(fCO2_meth_wtt40["Q1.1st Qu."], fCO2_meth_wtt80["Q1.1st Qu."]),
  fCO2_meth_Median = c(fCO2_meth_wtt40["Median.Median"], fCO2_meth_wtt80["Median.Median"]),
  fCO2_meth_Q3 = c(fCO2_meth_wtt40["Q3.3rd Qu."], fCO2_meth_wtt80["Q3.3rd Qu."]),
  fCH4_Tran_Q1 = c(fCH4_Tran_wtt40["Q1.1st Qu."], fCH4_Tran_wtt80["Q1.1st Qu."]),
  fCH4_Tran_Median = c(fCH4_Tran_wtt40["Median.Median"], fCH4_Tran_wtt80["Median.Median"]),
  fCH4_Tran_Q3 = c(fCH4_Tran_wtt40["Q3.3rd Qu."], fCH4_Tran_wtt80["Q3.3rd Qu."]),
  fCH4_Diff_Q1 = c(fCH4_Diff_wtt40["Q1.1st Qu."], fCH4_Diff_wtt80["Q1.1st Qu."]),
  fCH4_Diff_Median = c(fCH4_Diff_wtt40["Median.Median"], fCH4_Diff_wtt80["Median.Median"]),
  fCH4_Diff_Q3 = c(fCH4_Diff_wtt40["Q3.3rd Qu."], fCH4_Diff_wtt80["Q3.3rd Qu."])
)

# Print summary table
print(summary_table)

# Reshape the data frame to have separate Q1, Median, and Q3 columns
reshaped_data <- summary_table %>%
  pivot_longer(
    cols = starts_with("f"),
    names_to = c("Variable", ".value"),
    names_pattern = "(.*)_(Q1|Median|Q3)"
  )

reshaped_data$Gas_type<-ifelse(reshaped_data$Variable=="fCO2_OMdecay"|reshaped_data$Variable=="fCO2_meth","CO2","CH4")

# Subset the data based on the "Gas_type" column
co2_subset <- reshaped_data %>%
  filter(Gas_type == "CO2")

ch4_subset <- reshaped_data %>%
  filter(Gas_type == "CH4")

# Create the new columns 'Q1_corr' and 'Q3_corr' by calculating the sum of the Median of fCO2_OMdecay and Q1/Q3 of fCO2_meth
co2_subset <- co2_subset %>%
  group_by(WaterBody_Type) %>%
  mutate(
    Q1_corr = ifelse(Variable == "fCO2_meth",
                     co2_subset$Median[co2_subset$Variable == "fCO2_OMdecay" & co2_subset$WaterBody_Type == WaterBody_Type] + Q1,
                     NA),
    Q3_corr = ifelse(Variable == "fCO2_meth",
                     co2_subset$Median[co2_subset$Variable == "fCO2_OMdecay" & co2_subset$WaterBody_Type == WaterBody_Type] + Q3,
                     NA)
  )

co2_subset$Q1_corr<-ifelse(is.na(co2_subset$Q1_corr),co2_subset$Q1,co2_subset$Q1_corr)
co2_subset$Q3_corr<-ifelse(is.na(co2_subset$Q3_corr),co2_subset$Q3,co2_subset$Q3_corr)

ch4_subset <- ch4_subset %>%
  group_by(WaterBody_Type) %>%
  mutate(
    Q1_corr = ifelse(Variable == "fCH4_Diff",
                     ch4_subset$Median[ch4_subset$Variable == "fCH4_Tran" & ch4_subset$WaterBody_Type == WaterBody_Type] + Q1,
                     NA),
    Q3_corr = ifelse(Variable == "fCH4_Diff",
                     ch4_subset$Median[ch4_subset$Variable == "fCH4_Tran" & ch4_subset$WaterBody_Type == WaterBody_Type] + Q3,
                     NA)
  )

ch4_subset$Q1_corr<-ifelse(is.na(ch4_subset$Q1_corr),ch4_subset$Q1,ch4_subset$Q1_corr)
ch4_subset$Q3_corr<-ifelse(is.na(ch4_subset$Q3_corr),ch4_subset$Q3,ch4_subset$Q3_corr)

# Display the updated data
print(co2_subset)
print(ch4_subset)

###Plot

breaks <- c(0, 0.25,0.50,0.75,1.00)
labels<-c("0","25","50","75","100")

a<- ggplot(co2_subset, aes(fill=Variable, y=Median, x=WaterBody_Type)) +
  geom_bar(position="stack", stat="identity")+
  theme(panel.background = element_rect(fill = "white", colour = "black"),legend.title=element_blank(),legend.position = "bottom",legend.direction = "vertical",legend.box = "vertical",
        text = element_text(size=14),axis.text.x = element_text(angle = 40, hjust = 1, colour = "black"),
        plot.title = element_text(face = "bold"))+
  scale_fill_manual(values=c("#E69F00", "#56B4E9"),
                    labels=c("Methanogenesis                            ","Respiration"))+labs(y=expression(paste("Fraction of porewater CO"[2]," production (%)      ")), x = " ")+
  geom_errorbar(aes(ymin= Q1_corr, 
                    ymax= Q3_corr),colour = "grey",
                width=.2)+ggtitle("a)")+scale_y_continuous(breaks = breaks,labels = labels)+
  scale_x_discrete(labels = c("WTT40" = "WTT_Shallower",    # Manually change the boxplot names here
                              "WTT80" = "WTT_Deeper")) 
a

b<- ggplot(ch4_subset, aes(fill=Variable, y=Median, x=WaterBody_Type)) +
  geom_bar(position="stack", stat="identity")+ 
  theme(panel.background = element_rect(fill = "white", colour = "black"),legend.title=element_blank(),legend.position = "bottom",legend.direction = "vertical",legend.box = "vertical",axis.text.x = element_text(angle = 40, hjust = 1, colour = "black"),
        text = element_text(size=14),plot.title = element_text(face = "bold"))+
  scale_fill_manual(values=c("#D55E00", "#009E73"),
                    labels=c("Available for Diffusion or Oxidation","Plant-mediated & Ebullition"))+labs(y = expression(paste("Fraction of porewater CH"[4], " loss (%)")), x = " ")+
  geom_errorbar(aes(ymin= Q1_corr, 
                    ymax= Q3_corr),colour = "grey",
                width=.2)+ggtitle("b)")+scale_y_continuous(breaks = breaks,labels = labels) +
  scale_x_discrete(labels = c("WTT40" = "WTT_Shallower",    # Manually change the boxplot names here
                              "WTT80" = "WTT_Deeper")) 
b

get_legend <- function(my_ggplot) {
  tmp <- ggplotGrob(my_ggplot)
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

# Extract legend from one of the plots
legend1 <- get_legend(a)
legend2 <- get_legend(b)

#Remove the legends from the original plots
a <- a + theme(legend.position = "none")
b <- b + theme(legend.position = "none")
# Combine the legends using gridExtra
library(gridExtra)
combined_legend <- arrangeGrob(legend1, legend2, nrow = 2)

#Combine the plots and the combined legend using cowplot
library(cowplot)
ab <- plot_grid(
  plot_grid(a, b, align = 'h', ncol = 2),
  combined_legend,
  ncol = 1,
  rel_heights = c(.9,.2)
)
ab

########
#Panel C
########

data$WaterBody_Type<-ifelse(data$WaterBody_Type=="WTT40","WTT_Shallower",data$WaterBody_Type)
data$WaterBody_Type<-ifelse(data$WaterBody_Type=="WTT80","WTT_Deeper",data$WaterBody_Type)

head(data)

my.fodatamy.formula <- y ~ x
c <- ggplot(data = data , aes(y = d13CCO2, x =d13CCH4,color=WaterBody_Type)) +
  geom_segment(x = -99.88, y = -10.83, xend =-80.51, yend =4.74, color = 3, size=1)+
  geom_segment(x = -80.51, y = 4.74, xend =-65.80, yend =15.28, color = 3, size=1)+
  #geom_segment(x = -65.80, y = 15.28, xend =-54.80, yend =18.58, color = 3, size=1)+
  #geom_segment(x = -54.80, y =18.58, xend =-51.45, yend =-16.07, color = 3, size=1)+
  #geom_segment(x = -51.45, y =-16.07, xend =-53.72, yend =0.49, color = 3, size=1)+
  #geom_segment(x = -53.72, y = 0.49, xend =-54.80, yend =-1.24, color = 3, size=1)+
  # geom_segment(x = -54.80, y = -1.24, xend =-58.98, yend =-4.07, color = 3, size=1)+
  # geom_segment(x = -58.98, y = -4.07, xend =-62.09, yend =-7.84, color = 3, size=1)+
  geom_segment(x = -62.09, y = -7.84, xend =-65.20, yend =-16.65, color = 3, size=1,alpha=0.4)+
  geom_segment(x = -65.20, y = -16.65, xend =-74.05, yend =-24.67, color = 3, size=1)+
  geom_segment(x = -74.05, y =-24.67, xend =-80.39, yend =-28.29, color = 3, size=1)+
  geom_segment(x =-80.39, y =-28.29, xend =-85.05, yend =-29.39, color = 3, size=1)+
  geom_segment(x =-85.05, y =-29.39, xend =-91.75, yend =-28.76, color = 3, size=1)+
  geom_segment(x =-85.05, y =-29.39, xend =-91.75, yend =-28.76, color = 3, size=1)+
  geom_segment(x =-91.75, y =-28.76, xend =-100, yend =-27.66, color = 3, size=1)+
  geom_segment(x =-66.41, y =-28.56, xend =-65.20, yend =-16.65, color = 4, size=1)+
  geom_segment(x =-65.20, y =-16.65, xend= -62.09, yend = -7.84, color = 4, size=1,linetype="dashed")+
  geom_segment(x = -62.09, y = -7.84, xend =-59.17, yend =-4.19, color = 4, size=1,linetype="dashed")+
  geom_segment(x =-59.17, y = -4.19, xend =-58.98, yend =-4.07, color = 4, size=1,linetype="dashed")+
  geom_segment(x = -58.98, y = -4.07, xend = -54.80, yend = -1.24, color = 4, size=1,linetype="dashed")+
  geom_segment(x =-51.99, y=-2.76, xend =-49.39, yend =-6.56, color = 4, size=1,linetype="dashed")+
  geom_segment(x =-49.39, y =-6.56, xend =-48.19, yend =-11.02, color = 1, size=1)+
  geom_segment(x =-48.19, y =-11.02, xend =-48.38, yend =-17.18, color = 4, size=1)+
  geom_segment(x =-48.38, y =-17.18, xend =-51.86, yend =-20.98, color = 4, size=1)+
  geom_segment(x =-51.86, y=-20.98, xend =-63.72, yend =-28.30, color = 4, size=1)+
  geom_segment(x =-63.72, y =-28.30, xend =-66.41, yend =-28.56, color = 4, size=1)+
  geom_segment(x = -86, y = -27.4885, xend = -30, yend = -29.32759, color = 2, size=1,
               arrow = arrow())+
  theme_tufte() +theme(axis.line = element_line(size = 0.5, colour = "Grey20"),text = element_text(family = "sans", size = 14, color = "black"),plot.title = element_text(face = "bold"),
                       legend.position = "bottom")+
  labs(x=expression(paste(delta^{13}, "C-"*~CH[4]*" (\u2030)")), y =expression(paste(delta^{13}, "C-"*~CO[2]*" (\u2030)")))+
  #geom_abline(intercept = (1.09*1000-1000), slope = 1.09, linetype="dashed", size=.4,col="grey")+
  geom_abline(intercept = (1.08*1000-1000), slope = 1.08, linetype="dashed", size=.4,col="grey")+
  geom_abline(intercept = (1.07*1000-1000), slope = 1.07, linetype="dashed", size=.4,col="grey")+
  geom_abline(intercept = (1.06*1000-1000), slope = 1.06, linetype="dashed", size=.4,col="grey")+
  geom_abline(intercept = (1.05*1000-1000), slope = 1.05, linetype="dashed", size=.4,col="grey")+
  geom_abline(intercept = (1.04*1000-1000), slope = 1.04, linetype="dashed", size=.4,col="grey")+
  geom_abline(intercept = (1.03*1000-1000), slope = 1.03, linetype="dashed", size=.4,col="grey")+
  geom_abline(intercept = (1.02*1000-1000), slope = 1.03, linetype="dashed", size=.4,col="grey")+
  geom_abline(intercept = (1.01*1000-1000), slope = 1.03, linetype="dashed", size=.4,col="grey")+
  geom_abline(intercept = (1.00*1000-1000), slope = 1.03, linetype="dashed", size=.4,col="grey")+
  geom_abline(intercept = (0.99*1000-1000), slope = 1.03, linetype="dashed", size=.4,col="grey")+
  geom_abline(intercept = (0.98*1000-1000), slope = 1.03, linetype="dashed", size=.4,col="grey")+
  geom_abline(intercept = (0.97*1000-1000), slope = 1.03, linetype="dashed", size=.4,col="grey")+
  geom_point(aes(size = CH4_mgCL)) +
  #annotate("text", x=-88.2, y=-25, label= "α=1.07",angle='65')+
  annotate("text", x=-84, y=-30, label= "α=1.06",angle='65')+
  annotate("text", x=-75.2, y=-30, label= "α=1.05",angle='65')+
  annotate("text", x=-66.5, y=-30, label= "α=1.04",angle='65')+
  annotate("text", x=-57, y=-30, label= "α=1.03",angle='65')+
  annotate("text", x=-47, y=-30, label= "α=1.02",angle='65')+
  annotate("text", x=-38, y=-30, label= "α=1.01",angle='65')+
  annotate("text", x=-28, y=-30, label= "α=1.00",angle='65')+
  annotate("text", x=-18, y=-30, label= "α=0.99",angle='65')+
  #annotate("text", x=-90.7, y=-8, label= "α=1.09",angle='45')+
  annotate("text", x=-78, y=-27.1, label= "Oxidation", size=6,angle='-3',color = 2)+
  annotate("text", x=-76, y=-15, label= "Hydrogenotrophic", size=6,color = 3)+
  annotate("text", x=-57, y=-17.5, label= "Acetoclastic", size=6,color = 4)+
  scale_colour_manual("Sampling Site",values=c("WTT_Shallower"="#440154FF", "WTT_Deeper"="#404788FF","Ditch"="#1F968BFF","Canal"="#B8DE29FF"))+
  labs(size=expression(paste(""*~CH[4]*" (mg C "~L^-1*")")))+guides(color = guide_legend(nrow = 2),
                                                                    size = guide_legend(nrow = 2))+ggtitle("c)")
c

###Oxidation line from https://doi.org/10.5194/bg-5-1457-2008
###Hydrogenotrophic and Acetoclastic boxes from 10.1371/journal.pone.0078204

d <- plot_grid(ab,c, ncol = 2, rel_widths = c(1,2))
d

#Offload the graph
setwd("...")  #to be replaced by your own file sampling_plot
tiff(("20241015_Figure5.tiff"), height = 18, width = 27, units = 'cm', compression = "lzw", res = 600)
d
dev.off()

