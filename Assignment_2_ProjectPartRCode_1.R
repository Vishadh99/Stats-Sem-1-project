# Reading datasets

fossil_fuel <- read_csv("C:/Users/gadge/Stats/Project/fossil-fuel-primary-energy.csv")
Global_temp <- read_csv("C:/Users/gadge/Stats/Project/GlobalTemperatures.csv")
Temp_2013 <- read_csv("C:/Users/gadge/Stats/Project/temp_country_2013.csv")
Temp_2020 <- read_csv("C:/Users/gadge/Stats/Project/temp_country_2020.csv")

#mean, medium, mode, range, variance, stddev, quartiles, and IQR for features

#mode function
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

round_num <- function(x){
  round(x, digits = 2)
}

#function to print all values
stats <- function(x){
  print(paste("Mean:", round_num(mean(x))))
  print(paste("Mode:", round_num(getmode(x))))
  print(paste("Range:", round_num(range(x)[2]-range(x)[1])))
  print(paste("Variance:", round_num(var(x))))
  print(paste("Standard Deviation:", round_num(sd(x))))
  print(paste("Q1:", round_num(quantile(x)[1])))
  print(paste("Q2:", round_num(quantile(x)[2])))
  print(paste("Q3:", round_num(quantile(x)[3])))
  print(paste("Q4:", round_num(quantile(x)[4])))
  print(paste("IQR:", round_num(IQR(x))))
  }


Global_temp$dt <- as.Date(Global_temp$dt, format = "%d-%m-%Y")
Global_temp$year <- format(Global_temp$dt, "%Y")

Global_temp_year <- Global_temp %>% group_by(year) %>% summarise(LandAverageTemperature = mean(LandAverageTemperature),LandAndOceanAverageTemperature = mean(LandAndOceanAverageTemperature))

stats(Global_temp_year$LandAverageTemperature)
stats(Global_temp_year$LandAndOceanAverageTemperature)

Temp_2013$year <- format(Temp_2013$Date, "%Y")

Temp_2013_year <- Temp_2013 %>% group_by(year,Country) %>% summarise(Temp = mean(Temp))


Temp_2020$year <- format(Temp_2020$Date, "%Y")

Temp_2020_year <- Temp_2020 %>% group_by(year,Country) %>% summarise(Temp = mean(Temp))

temp_1930 <- Temp_2013_year %>% filter(year == '1930')
temp_2013 <- Temp_2013_year %>% filter(year == '2013')

stats(temp_1930$Temp)
stats(temp_2013$Temp)

colnames(fossil_fuel) <- c('ID','Country','Code','Year','Amount_in_TWh')

fossil_fuel_1965 <- fossil_fuel %>% filter(Year == 1965)
fossil_fuel_2021 <- fossil_fuel %>% filter(Year == 2021)

stats(fossil_fuel_1965$Amount_in_TWh)
stats(fossil_fuel_2021$Amount_in_TWh)


# histogram function

histogram <- function(x, label){
  par(mfrow= c(2, 2))
  hist(x, freq= FALSE, col= "grey",breaks = 5,main = "Histogram with 5 breaks",xlab = label)
  hist(x, freq= FALSE, col= "light blue",breaks = 10,main = "Histogram with 10 breaks", xlab = label)
  hist(x, freq= FALSE, col= "coral",breaks = 20,main = "Histogram with 20 breaks", xlab = label)
  hist(x, freq= FALSE, col= "coral",breaks = 30,main = "Histogram with 30 breaks", xlab = label)
  par(mfrow = c(1,1))
}

histogram(Global_temp_year$LandAverageTemperature, "Land Average Temperature")
histogram(Global_temp_year$LandAndOceanAverageTemperature, "Land And Ocean Average Temperature")
histogram(temp_1930$Temp, "Temperature across countries")
histogram(temp_2013$Temp, "Temperature across countries")
histogram(fossil_fuel_1965$Amount_in_TWh, "Fuel consumption by countries")
histogram(fossil_fuel_2021$Amount_in_TWh, "Fuel consumption by countries")

library("vioplot")
box_violin <- function(x, title){
  valuelabels <- c(round_num(min(x)), round_num(quantile(x)[2]),round_num(median(x)), round_num(quantile(x)[4]), round_num(max(x)))
  print(valuelabels)
  par(mfrow= c(1, 2))
  boxplot(x, col = 'coral',main = title)
  text(x = c(1.2, 1.4, 1.4,1.4, 1.2), y = valuelabels, labels = valuelabels)
  vioplot(x,col = 'light blue', main = title)
  text(x = c(1.2, 1.4, 1.4,1.4, 1.2), y = valuelabels, labels = valuelabels)
  par(mfrow = c(1,1))
}

box_violin(Global_temp_year$LandAverageTemperature, "Land Temperature")
box_violin(Global_temp_year$LandAndOceanAverageTemperature, "Land and Ocean Temperature")
box_violin(temp_1930$Temp, "1930 Temperatures")
box_violin(temp_2013$Temp, "2013 Temperatures")
box_violin(fossil_fuel_1965$Amount_in_TWh, "Fuel consumption by countries")
box_violin(fossil_fuel_2021$Amount_in_TWh, "Fuel consumption by countries")

gaussKDF <- function(x){1/sqrt(2*pi)*exp(-(x^2)/2)}
QuarticKDF <- function(x){(abs(x) < 1)*((15*((1 - x^2)^2))/16)}

KDE <- function(x, label, inter){
  xi <- seq(from = min(x) - inter, to = max(x) + inter, length.out = 1000)
  h <- sqrt(var(x))
  n <- length(x)
  par(mfrow= c(1, 2))
  KDF <- gaussKDF
  Dist_around_xis <- sapply(x, function(x){(1/(h*n))*KDF((xi - x)/h)})
  plot(xi, apply(Dist_around_xis, 1, sum), type = "l",xlab = label, ylab = "", lwd = 2,main = "Guassian KDE")
  rug(x, lwd = 2)
  KDF <- QuarticKDF
  Dist_around_xis <- sapply(x, function(x){(1/(h*n))*KDF((xi - x)/h)})
  plot(xi, apply(Dist_around_xis, 1, sum), type = "l", xlab = label,ylab = "", lwd = 2, main = "Quartic KDE")
  rug(x, lwd = 2)
  par(mfrow= c(1, 1))
}

KDE(Global_temp_year$LandAverageTemperature, "Land Average Temperature", 1)
KDE(Global_temp_year$LandAndOceanAverageTemperature, "Land And Ocean Average Temperature", 1)
KDE(temp_1930$Temp, "Temperature across countries", 10)
KDE(temp_2013$Temp, "Temperature across countries", 10)
KDE(fossil_fuel_1965$Amount_in_TWh, "Fuel consumption by countries",1000)
KDE(fossil_fuel_2021$Amount_in_TWh, "Fuel consumption by countries",1000)

qqnorm(Global_temp_year$LandAverageTemperature)+qqline(Global_temp_year$LandAverageTemperature, col = "steelblue", lwd = 2)

qqplot(Global_temp_year$LandAverageTemperature,Global_temp_year$LandAndOceanAverageTemperature, xlab = 'Land Temperature', ylab = 'Land and Ocean Temperature')
qqplot(temp_1930$Temp,temp_2013$Temp, xlab = '1930', ylab = '2013')
qqplot(fossil_fuel_1965$Amount_in_TWh,fossil_fuel_2021$Amount_in_TWh, xlab = '1965', ylab = '2021')