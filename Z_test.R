

Global_temp <- read_csv("C:/Users/gadge/Stats/Project/GlobalTemperatures.csv")

Global_temp$dt <- as.Date(Global_temp$dt, format = "%d-%m-%Y")
Global_temp$year <- format(Global_temp$dt, "%Y")

Global_temp_year <- Global_temp %>% group_by(year) %>% summarise(LandAverageTemperature = mean(LandAverageTemperature),LandAndOceanAverageTemperature = mean(LandAndOceanAverageTemperature))

pop <- Global_temp_year$LandAverageTemperature
sample <- Global_temp_year %>% filter(as.double(year) >= 1985) 
sample <- sample$LandAverageTemperature

mu <- mean(pop)
sd <- sd(pop)
x_bar <- mean(sample)
n <- length(sample)
z<- (x_bar - mu)/(sd/sqrt(n))

cri <- qnorm(0.95)

rejection_z_test <- function(x){
  y <- dnorm(x,0,1)
  y[x<cri] <- NA
  y
}

ggplot(data.frame(x = c(-8, 8)), aes(x)) +
  stat_function(fun = dnorm,geom = "line")+
  stat_function(fun = function(x){rejection_z_test(x)}, geom = "area", fill = "red")+
  geom_vline(xintercept = z, linetype = 'dashed') + xlab('Z')+ylab('F(x)')

