library(tidyverse)
library(ggplot2)
Global_temp <- read_csv("C:/Users/gadge/Stats/Project/GlobalTemperatures.csv")
Temp_2013 <- read_csv("C:/Users/gadge/Stats/Project/temp_country_2013.csv")
Temp_2020 <- read_csv("C:/Users/gadge/Stats/Project/temp_country_2020.csv")

Global_temp$dt <- as.Date(Global_temp$dt, format = "%d-%m-%Y")
Global_temp$year <- format(Global_temp$dt, "%Y")

Global_temp_year <- Global_temp %>% group_by(year) %>% 
  summarise(LandAverageTemperature = mean(LandAverageTemperature),LandAndOceanAverageTemperature = mean(LandAndOceanAverageTemperature))

Temp_2013$year <- format(Temp_2013$Date, "%Y")

Temp_2013_year <- Temp_2013 %>% group_by(year,Country) %>% summarise(Temp = mean(Temp))


Temp_2020$year <- format(Temp_2020$Date, "%Y")

Temp_2020_year <- Temp_2020 %>% group_by(year,Country) %>% summarise(Temp = mean(Temp))

temp_1930 <- Temp_2013_year %>% filter(year == '1930')
temp_2013 <- Temp_2013_year %>% filter(year == '2013')

Freq_distribution <- data.frame(Temp = round(Global_temp_year$LandAverageTemperature,1))

Freq_distribution <- Freq_distribution %>% group_by(Temp) %>% summarize(Freq = n())
Freq_distribution$prob <- Freq_distribution$Freq/(sum(Freq_distribution$Freq))

s <- Global_temp_year$LandAverageTemperature
xi = seq(7.9,9.8,0.01)
f=dnorm(xi, mean(s), sd(s))
normal_density <- data.frame(xi, f)

exp_val <- mean(s)
standard_error <- sd(s)
label <- paste(paste("mean =",round(exp_val,2), sep = " "),paste("SD =",round(standard_error,2), sep = " "),sep = "\n")
labels <- data.frame(value = c(label), x = c(9.5), y = c(0.8))

ggplot()+
  geom_bar(data = Freq_distribution, aes(x = as.numeric(Temp),y = prob),stat = 'identity',fill = 'slategrey')+
  geom_density(data = Global_temp_year,aes(x = LandAverageTemperature,..scaled..),bw = 0.1,alpha = 0.4, fill = 'grey', col = 'grey')+
  geom_line(data = normal_density,aes(xi,f),size = 1.5, color = 'azure4')+xlim(7.7,10)+
  geom_label(data = labels, aes(x, y, label = value), size = 3)+
  xlab("LandAverageTemperature") + ylab("")+ ggtitle("Distribution of Land Average Temperature over the years")+
  theme(plot.title=element_text(hjust=0.5),legend.position="none")
