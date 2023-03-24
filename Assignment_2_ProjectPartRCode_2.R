# Reading dataset


Data <- read_csv("~/Downloads/archive (2)/Air and Climate/Final_Dataset1.csv")


SO2_emissions_year <- Data %>% 
  group_by(Year)

SO2_by_year <- SO2_emissions_year %>% summarise(
  Total_SO2_Emissions = mean(na.omit(Total_SO2_Emissions))
)




CO2_emissions_year <- Data %>% 
  group_by(Year)

CO2_by_year <- CO2_emissions_year %>% summarise(
  Total_CO2_Emissions = mean(na.omit(Total_CO2_Emissions))
)

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





stats(na.omit(Data$Total_SO2_Emissions))

stats(na.omit(Data$Total_CO2_Emissions))

SO2_1990_year <- Data %>% 
  group_by(Year,Country)

SO2_1990_year <- SO2_1990_year %>% summarise(
  Total_SO2_Emissions = mean(na.omit(Total_SO2_Emissions))
)

CO2_1990_year <- Data %>% 
  group_by(Year,Country)

CO2_1990_year <- CO2_1990_year %>% summarise(
  Total_CO2_Emissions = mean(na.omit(Total_CO2_Emissions))
)



SO2_1990 <- SO2_1990_year %>% filter(Year == '1990' & Total_SO2_Emissions >1)
SO2_2018 <- SO2_1990_year %>% filter(Year == '2018' & Total_SO2_Emissions >1)


CO2_1990 <- CO2_1990_year %>% filter(Year == '1990' & Total_CO2_Emissions >1)
CO2_2018 <- CO2_1990_year %>% filter(Year == '2018' & Total_CO2_Emissions >1)


stats(na.omit(SO2_1990$Total_SO2_Emissions))
stats(na.omit(CO2_1990$Total_CO2_Emissions))

# histogram function

histogram <- function(x, label){
  par(mfrow= c(2, 2))
  hist(x, freq= FALSE, col= "grey",breaks = 15,main = "Histogram with 15 breaks",xlab = label)
  hist(x, freq= FALSE, col= "light blue",breaks = 20,main = "Histogram with 20 breaks", xlab = label)
  hist(x, freq= FALSE, col= "coral",breaks = 25,main = "Histogram with 25 breaks", xlab = label)
  hist(x, freq= FALSE, col= "coral",breaks = 30,main = "Histogram with 30 breaks", xlab = label)
  par(mfrow = c(1,1))
}



histogram(SO2_by_year$Total_SO2_Emissions, "Total SO2 Emissions")
histogram(CO2_by_year$Total_CO2_Emissions, "Total CO2 Emissions")
histogram(na.omit(SO2_1990$Total_SO2_Emissions), "SO2 Emissions in 1990")
histogram(na.omit(CO2_1990$Total_CO2_Emissions), "CO2 Emissions in 1990")


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

box_violin(SO2_by_year$Total_SO2_Emissions, "Total SO2 Emissions")
box_violin(CO2_by_year$Total_CO2_Emissions, "Total CO2 Emissions")
box_violin(na.omit(SO2_1990$Total_SO2_Emissions), "SO2 Emissions in 1990")
box_violin(na.omit(CO2_1990$Total_CO2_Emissions), "CO2 Emissions in 1990")



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



KDE(SO2_by_year$Total_SO2_Emissions, "Total SO2 Emissions", 1000)
KDE(CO2_by_year$Total_CO2_Emissions, "Total CO2 Emissions", 1000)
KDE(SO2_1990$Total_SO2_Emissions, "SO2 Emissions in 1990", 10000)
KDE(CO2_1990$Total_CO2_Emissions, "CO2 Emissions in 1990", 10000000)


qqnorm(SO2_by_year$Total_SO2_Emissions)+
  qqline(SO2_by_year$Total_SO2_Emissions, col = "steelblue", lwd = 2)

qqnorm(CO2_by_year$Total_CO2_Emissions)+
  qqline(CO2_by_year$Total_CO2_Emissions, col = "steelblue", lwd = 2)


qqplot(SO2_by_year$Total_SO2_Emissions,
       CO2_by_year$Total_CO2_Emissions, 
       xlab = 'SO2 Emissions', ylab = 'CO2 Emissions')

qqplot(SO2_1990$Total_SO2_Emissions,SO2_2018$Total_SO2_Emissions, xlab = 'SO2 Emissions in 1990', ylab = 'SO2 Emissions in 2018')

qqplot(CO2_1990$Total_CO2_Emissions,CO2_2018$Total_CO2_Emissions, xlab = 'CO2 Emissions in 1990', ylab = 'CO2 Emissions in 2018')


