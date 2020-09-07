#Code for graphing figures fo OSMP project
#Load Master Soil Sheet
library(readxl)

#Import Master_Soil_Sheet
Soil_Fall2019 <- read_excel("~/Dropbox/Boulder_OSMP_TOG/Data/Soil_Fall2019.xlsx", 
                              +     sheet = "Master Sheet Soil")
Master_Soil_Sheet <- Soil_Fall2019
View(Master_Soil_Sheet)

#Import N cycling sheet
Soil_Fall2019 <- read_excel("~/Dropbox/Boulder_OSMP_TOG/Data/Soil_Fall2019.xlsx", 
                            +     sheet = "N cycling")
N_cycling <- Soil_Fall2019
View(N_cycling)


library("ggplot2", lib.loc="/Library/Frameworks/R.framework/Versions/3.5/Resources/library")

#Invaded vs. uninvaded for %Water
Master_Soil_Sheet<-Master_Soil_Sheet[!(Master_Soil_Sheet$Site=="Blank"),] #Remove Blanks
Master_Soil_Sheet$`%water_content` <-as.numeric(as.character(Master_Soil_Sheet$`%water_content`))
is.numeric(Master_Soil_Sheet$`%water_content`)
is.character(Master_Soil_Sheet$Invasion)

ggplot(data=Master_Soil_Sheet) + geom_col(mapping=aes(x=Invasion, y='%water_content'))

i#Plotting bar chart for dependent variable per site with invasion/uninvaded side-by-side
ggplot(data=N_cycling) + geom_col(mapping=aes(x=Site, y=Net_Mineralization, fill=Invasion), position = "dodge")

#Plotting all invaded versus uinvaded in bar graph
ggplot(data=N_cycling) + geom_col(mapping=aes(x=Invasion, y=Net_Mineralization))
is.character(N_cycling$Invasion)

#Testing for normality
library(nortest)
lillie.test((N_cycling$Net_Mineralization)) 
shapiro.test(N_cycling$Net_Mineralization)#Net_Mineralization is normally distributed
t.test(N_cycling$Net_Mineralization~N_cycling$Invasion)

#Biomass C:N between invaded and uninvaded
ggplot(data=Master_Veg_Sheet) + geom_col(mapping=aes(x=Invasion, y=C_N))
shapiro.test(Master_Veg_Sheet$C_N)#Net_Mineralization is normally distributed
t.test(Master_Veg_Sheet$C_N~Master_Veg_Sheet$Invasion)

#Biomass between all invaded and uninvaded
ggplot(data=Master_Veg_Sheet) + geom_col(mapping=aes(x=Invasion, y=Master_Veg_Sheet$`Biomass_(g)_per_m^2`))
shapiro.test(Master_Veg_Sheet$`Biomass_(g)_per_m^2`)#Net_Mineralization is normally distributed
t.test(Master_Veg_Sheet$`Biomass_(g)_per_m^2`~Master_Veg_Sheet$Invasion)


ggplot(data=N_cycling)+stat_count(mapping=aes(x=Net_Mineralization))
ggplot(data=Master_Sheet_Soil, aes(x="Site", y="C:N", fill="Invasion"))+geom_bar(stat="summary",fun.y = "mean", color="black",position=position_dodge())
ggplot(Master_Soil_Sheet)+geom_bar(aes(Site,"pH", fill = as.factor(Invasion)),position="dodge", stat="summary", fun.y="mean", na.rm = TRUE)
na.rm = TRUE
