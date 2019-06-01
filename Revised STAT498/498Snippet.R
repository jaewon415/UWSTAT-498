rm(list = ls())
##############################
#       Load Packages
##############################
library(reshape2)
library(ggplot2)
library(knitr)
library(dplyr)
library(grid)
library(gridExtra)
source("/Users/jaewon/Desktop/Revised STAT498/multiplot.R")
# Get the data for the analysis
Asia <- read.csv("/Users/jaewon/Desktop/Revised STAT498/Asia.csv")
Africa <- read.csv("/Users/jaewon/Desktop/Revised STAT498/Africa.csv")
Europe <- read.csv("/Users/jaewon/Desktop/Revised STAT498/Europe.csv")
NorthAmerica <- read.csv("/Users/jaewon/Desktop/Revised STAT498/NorthAmerica.csv")
SouthAmerica <- read.csv("/Users/jaewon/Desktop/Revised STAT498/SouthAmerica.csv")
CentralAmerica <- read.csv("/Users/jaewon/Desktop/Revised STAT498/CentralAmerica.csv")
Caribbean <- read.csv("/Users/jaewon/Desktop/Revised STAT498/Caribbean.csv")
Oceania <- read.csv("/Users/jaewon/Desktop/Revised STAT498/Oceania.csv")
World <- read.csv("/Users/jaewon/Desktop/Revised STAT498/World.csv")
Population <- read.csv("/Users/jaewon/Desktop/Revised STAT498/Population.csv")
ForestLand <- read.csv("/Users/jaewon/Desktop/Revised STAT498/ForestLand.csv")
AfricaForest <- read.csv("/Users/jaewon/Desktop/Revised STAT498/AfricaForest.csv")
# 5916 Export Quantity 5922 Export Value
# 5616 Import Quantity 5622 Import Value
Region.Criteria <- function(data = data, element = 5516, year = 2000) {
  if (element == 5516) {
    result <- subset(data, subset = data$Element.Code == 5516 & data$Year >= year)
  } else if (element == 5916 | element == 5616) {
    result <- subset(data, subset = data$Element.Code == 5916 | data$Element.Code == 5616 & data$Year >= year)
  } else {
    result <- subset(data, subset = data$Element.Code == 5922 | data$Element.Code == 5622 & data$Year >= year)
  }
  return(result)
} 




# Get the production dataset starting from 2000
Asia2 <- Region.Criteria(Asia, element = 5516, year = 2000)
TotalAsiaProduction <- aggregate(Asia2$Value, by = list(Asia2$Year), FUN = sum, na.rm = T)
colnames(TotalAsiaProduction) <- c("Year", "Asia")

Africa2 <- Region.Criteria(Africa, element = 5516, year = 2000)
TotalAfricaProduction <- aggregate(Africa2$Value, by = list(Africa2$Year), FUN = sum, na.rm = T)
colnames(TotalAfricaProduction) <- c("Year", "Africa")

Europe2 <- Region.Criteria(Europe, element = 5516, year = 2000)
TotalEuropeProduction <- aggregate(Europe2$Value, by = list(Europe2$Year), FUN = sum, na.rm = T)
colnames(TotalEuropeProduction) <- c("Year", "Europe")

NorthAmerica2 <- Region.Criteria(NorthAmerica, element = 5516, year = 2000)
TotalNorthAmericaProduction <- aggregate(NorthAmerica2$Value, by = list(NorthAmerica2$Year), FUN = sum, na.rm = T)
colnames(TotalNorthAmericaProduction) <- c("Year", "NorthAmerica")

SouthAmerica2 <- Region.Criteria(SouthAmerica, element = 5516, year = 2000)
TotalSouthAmericaProduction <- aggregate(SouthAmerica2$Value, by = list(SouthAmerica2$Year), FUN = sum, na.rm = T)
colnames(TotalSouthAmericaProduction) <- c("Year", "SouthAmerica")

CentralAmerica2 <- Region.Criteria(CentralAmerica, element = 5516, year = 2000)
TotalCentralAmericaProduction <- aggregate(CentralAmerica2$Value, by = list(CentralAmerica2$Year), FUN = sum, na.rm = T)
colnames(TotalCentralAmericaProduction) <- c("Year", "CentralAmerica")

Caribbean2 <- Region.Criteria(Caribbean, element = 5516, year = 2000)
TotalCaribbeanProduction <- aggregate(Caribbean2$Value, by = list(Caribbean2$Year), FUN = sum, na.rm = T)
colnames(TotalCaribbeanProduction) <- c("Year", "Carribean")

Oceania2 <- Region.Criteria(Oceania, element = 5516, year = 2000)
TotalOceaniaProduction <- aggregate(Oceania2$Value, by = list(Oceania2$Year), FUN = sum, na.rm = T)
colnames(TotalOceaniaProduction) <- c("Year", "Oceania")

# Get the total dataframe
TotalContinents <- Reduce(function(x, y) merge(x, y, by = "Year"), list(TotalAfricaProduction, TotalAsiaProduction, TotalOceaniaProduction, TotalEuropeProduction, TotalNorthAmericaProduction, TotalCentralAmericaProduction, TotalSouthAmericaProduction, TotalCaribbeanProduction))

# Melted Total Production Data
Melted.TotalContinents <- melt(TotalContinents, id.vars = "Year")

TotalContinents

# Graph of the combined continents
g <- ggplot(Melted.TotalContinents, aes(x = Year, y = value, col = variable)) + geom_line(size = 1.5)
g <- g + ylab("Production Value (M3)") + xlab("Year") + ggtitle("Woold Fuel Production [2000 ~ 2017]")
g <- g + labs(color = "Region")
g

# Graphs of the individual continents
par(mfrow = c(3, 3))
for (i in 2:ncol(TotalContinents)) {
  title <- paste("Wood Fuel Production in", colnames(TotalContinents)[i])
  plot(TotalContinents$Year, TotalContinents[, i], ylab = "Production (M3)", xlab = "Year", main = title, type = "l", col = "blue", lwd = 1.5)
  points(TotalContinents$Year, TotalContinents[, i], col = "red", cex = 1.1, pch = 18)
  if (colnames(TotalContinents)[i] %in% c("CentralAmerica", "Europe", "Africa", "NorthAmerica", "Carribean")) {
    box(col = "red", lwd = 2)
  }
}
plot.new()

# Let's look at the share of each region
World2 <- subset(World, subset = World$Year >= 2000 & (World$Item.Code == 1627 | World$Item.Code == 1628))
TotalWorldProduction <- aggregate(World2$Value, by = list(World2$Year), FUN = sum, na.rm = T)
colnames(TotalWorldProduction) <- c("Year", "Production")

# World population
par(mfrow = c(1,1))
WorldPop <- subset(Population, subset = Population$Area == "World" & Population$Element == "Total Population - Both sexes" & Population$Year >= 2000 & Population$Year <= 2017)
plot(y = WorldPop$Value, x = TotalWorldProduction$Production, xlab = "Production (M3)", ylab = "1000 persons", main = "World population vs wood fuel prod. [2000 ~ 2017]", type = "p")
points(y = WorldPop$Value, x = TotalWorldProduction$Production, col = "red", cex = 1.1, pch = 18)



g <- ggplot(World2, aes(x = World2$Value, y = World2$Year, fill = World2$Item))
g <- g + geom_bar(stat = "identity", position = "dodge") + ylab("Production Value (M3)") + xlab("Year")
g <- g + theme(axis.text.x = element_text(angle = 90)) + scale_fill_discrete(name = "Variable")
g <- g + ggtitle("Wood Fuel Production in the World [2000 ~ 2017]") 
g




Total.Share <- TotalContinents
rownames(Total.Share) <- Total.Share[, 1]
Total.Share <- Total.Share[, -1]
for (i in 1:nrow(TotalContinents)) {
  Total.Share[i, ] <- round(Total.Share[i, ] * 100 / TotalWorldProduction[i, 2], 2)
}
Total.Share <- as.data.frame(t(Total.Share))
Production.Share.Continents <- Total.Share[order(Total.Share$`2000`, decreasing = T), ]
knitr::kable(Production.Share.Continents[, seq(1, ncol(Production.Share.Continents), by = 4)])

# Let's look at the distribution of the histogram 
par(mfrow = c(3,3))
for (i in 2:ncol(TotalContinents)) {
  title <- paste("Wood Fuel Production in", colnames(TotalContinents)[i])
  hist(TotalContinents[, i], main = title, freq = F, col = "orange", xlab = "Production (M3)", ylab = "Density")
}
plot.new()


# Let's bootstrap on these regions and get 95% CI
BS_CI <- c()
for (i in 2:ncol(TotalContinents)) {
  BsValues <- c()
  for (j in 1:10000) {
    Bsample <- sample(TotalContinents[, i], size = length(TotalContinents), replace = TRUE)
    BsampleMean <- mean(Bsample)
    BsValues[j] <- BsampleMean
  }
  BS_CI <- rbind(BS_CI, quantile(BsValues, probs = c(0.025, 0.975)))
}
BS_CI <- as.data.frame(BS_CI)
rownames(BS_CI) <- colnames(TotalContinents)[-1]
knitr::kable(BS_CI)

# Population and its relation to production (Both Male & Female)
TotalAfricaPopulation <- subset(Population, subset = Population$Area == "Africa" & Population$Year >= 2000 & Population$Year <= 2017 & Population$Element == "Total Population - Both sexes")
TotalAsiaPopulation <- subset(Population, subset = Population$Area == "Asia" & Population$Year >= 2000 & Population$Year <= 2017 & Population$Element == "Total Population - Both sexes")
TotalOceaniaPopulation <- subset(Population, subset = Population$Area == "Oceania" & Population$Year >= 2000 & Population$Year <= 2017 & Population$Element == "Total Population - Both sexes")
TotalEuropePopulation <- subset(Population, subset = Population$Area == "Europe" & Population$Year >= 2000 & Population$Year <= 2017 & Population$Element == "Total Population - Both sexes")
TotalNorthAmericaPopulation <- subset(Population, subset = Population$Area == "Northern America" & Population$Year >= 2000 & Population$Year <= 2017 & Population$Element == "Total Population - Both sexes")
TotalCentralAmericaPopulation <- subset(Population, subset = Population$Area == "Central America" & Population$Year >= 2000 & Population$Year <= 2017 & Population$Element == "Total Population - Both sexes")
TotalSouthAmericaPopulation <- subset(Population, subset = Population$Area == "South America" & Population$Year >= 2000 & Population$Year <= 2017 & Population$Element == "Total Population - Both sexes")
TotalCaribbeanPopulation <- subset(Population, subset = Population$Area == "Caribbean" & Population$Year >= 2000 & Population$Year <= 2017 & Population$Element == "Total Population - Both sexes")

TotalPopulation <- cbind(TotalAfricaPopulation$Value, TotalAsiaPopulation$Value, TotalOceaniaPopulation$Value, TotalEuropePopulation$Value, TotalNorthAmericaPopulation$Value,
                         TotalCentralAmericaPopulation$Value, TotalSouthAmericaPopulation$Value, TotalCaribbeanPopulation$Value)
TotalPopulation <- as.data.frame(TotalPopulation)
colnames(TotalPopulation) <- colnames(TotalContinents)[-1]

MeltedPop <- melt(TotalPopulation)
MeltedCT <- melt(TotalContinents, id.vars = "Year")
MeltedCT[, 4] <- MeltedPop$value
colnames(MeltedCT) <- c("Year", "Area", "Prod", "Population")

# Graph of the combined continents
g <- ggplot(MeltedCT, aes(x = Population, y = Prod, col = Area)) + geom_point(size = 1)
g <- g + ylab("Production Value (M3)") + xlab("Population") + ggtitle("Woold Fuel Production vs Pop. [2000 ~ 2017]")
g <- g + labs(color = "Region")
g



par(mfrow = c(1, 1))
plot(TotalContinents$Africa, y = TotalPopulation$Africa, ylab = "Population (1000 persons)", xlab = "Production (M3)", ylim = c(0, 4600000), xlim = c(0, 1000000000))
for (i in 2:ncol(TotalPopulation)) {
  points(TotalContinents[, i + 1], y = TotalPopulation[, i], col = "red")
}

# Graphs of the individual continents by population
par(mfrow = c(3, 3))
for (i in 2:ncol(TotalContinents)) {
  title <- paste("WF Production vs Pop. in", colnames(TotalContinents)[i])
  plot(TotalContinents[, i], TotalPopulation[, i - 1], ylab = "Population (1000 persons)", xlab = "Production (M3)", main = title, type = "p", col = "blue", lwd = 1.5)
  if (colnames(TotalContinents)[i] %in% c("CentralAmerica", "Europe", "Africa", "NorthAmerica", "Carribean")) {
    box(col = "red", lwd = 2)
  }
}
plot.new()

# Use the permutation to check if there is a difference between small and large population
par(mfrow = c(3, 3))
Pvalues <- c()
for (i in 1:ncol(TotalPopulation)) {
  # Using the mean as the criteria for splitting the data
  meanPopulation <- mean(TotalPopulation[, i])
  BelowMean <- which(TotalPopulation[, i] <= meanPopulation)
  AboveMean <- which(TotalPopulation[, i] > meanPopulation)
  dataBelow <- TotalContinents[BelowMean, i + 1]
  dataAbove <- TotalContinents[AboveMean, i + 1]
  # Combine the data 
  ProductionInRegion <- c(dataBelow, dataAbove)
  Labels <- rep(c("Below", "Above"), c(length(dataBelow), length(dataAbove)))
  # Create the vector for Permutation Test
  nullDiffs <- numeric(10000)
  set.seed(123)
  for (j in 1:length(nullDiffs)) {
    LabelPerm <- sample(Labels, replace = FALSE)
    nullDiffs[j] <- mean(ProductionInRegion[LabelPerm == "Below"]) - mean(ProductionInRegion[LabelPerm == "Above"])
  }
  obsDiff <- mean(ProductionInRegion[Labels == "Below"]) - mean(ProductionInRegion[Labels == "Above"])
  hist(nullDiffs, col = "orange", main = paste("Permutation Test", colnames(TotalPopulation)[i]))
  abline(v = obsDiff, col = "red", lwd = 2)
  if (obsDiff < 0) { 
    Pvalues <- rbind(Pvalues, sum(nullDiffs <= obsDiff) / length(nullDiffs))
  } else {
    Pvalues <- rbind(Pvalues, sum(nullDiffs >= obsDiff) / length(nullDiffs))
  }
  nullDiffs <- numeric(10000)
}
Pvalues <- as.data.frame(Pvalues)
rownames(Pvalues) <- colnames(TotalPopulation)
colnames(Pvalues) <- "P-Values"
knitr::kable(Pvalues)
plot.new()


# Lets look at the africa continent closely
CountriesAfrica <- aggregate(Africa2$Value, by = list(Africa2$Area), FUN = sum, na.rm = T)
# set the seed for the reproducibility
set.seed(12345)
# Boostrap on whole production in africa from 2000
bsVals <- numeric(10000)
for (i in 1:length(bsVals)) {
  africaResamp <- sample(CountriesAfrica$x, size = nrow(CountriesAfrica), replace = TRUE)
  africaMean <- mean(africaResamp)
  bsVals[i] <- africaMean
}
bsCI <- quantile(bsVals, probs = c(0.025, 0.975))
bsCI

g <- ggplot(CountriesAfrica, aes(x = CountriesAfrica$Group.1, y = CountriesAfrica$x))
g <- g + geom_bar(stat = "identity", position = "dodge", fill = "aquamarine4") + ylab("Production Value (M3)") + xlab("Region")
g <- g + theme(axis.text.x = element_text(angle = 90)) + scale_fill_discrete(name = "Variable")
g <- g + ggtitle("Wood Fuel Production in Africa [2000 ~ 2017]") + geom_hline(yintercept = bsCI[2], col = "red", linetype = "dashed")
g <- g + geom_hline(yintercept = bsCI[1], col = "red", linetype = "dashed")
g

# Let's look at the share of each region in africa
CountriesAfricaYear <- aggregate(Africa2$Value, by = list(Africa2$Area, Africa2$Year), FUN = sum, na.rm = T)
DcastedCountriesAfrica <- dcast(CountriesAfricaYear, formula = Group.1~ Group.2 , fill = NA, value.var = "x")

Total.Africa.Share <- DcastedCountriesAfrica
rownames(Total.Africa.Share) <- Total.Africa.Share[, 1]
Total.Africa.Share <- Total.Africa.Share[, -1]

for (i in 1:ncol(Total.Africa.Share)) {
  Total.Africa.Share[, i] <- round(Total.Africa.Share[, i] * 100 / TotalAfricaProduction[i, 2], 2)
}
Total.Africa.Share <- Total.Africa.Share[order(Total.Africa.Share$`2016`, decreasing = T), ]
Total.Africa.Share <- Total.Africa.Share[!apply(Total.Africa.Share < 1, 1, any),]
Total.Africa.Share <- Total.Africa.Share[complete.cases(Total.Africa.Share),]

knitr::kable(Total.Africa.Share[, seq(0, ncol(Total.Share), by = 3)])

# Lets look at the africa continent closely
BothCountriesAfrica <- aggregate(Africa2$Value, by = list(Africa2$Area, Africa2$Item), FUN = sum, na.rm = T)
g <- ggplot(BothCountriesAfrica, aes(x = BothCountriesAfrica$Group.1, y = BothCountriesAfrica$x, fill = BothCountriesAfrica$Group.2))
g <- g + geom_bar(stat = "identity", position = "dodge") + ylab("Production Value (M3)") + xlab("Region")
g <- g + theme(axis.text.x = element_text(angle = 90)) + scale_fill_discrete(name = "Variable")
g <- g + ggtitle("Wood Fuel Production in Africa [2000 ~ 2017]")
g

# Just Coniferous and non coniferous
ConiferousAfrica <- subset(BothCountriesAfrica, subset = BothCountriesAfrica$Group.2 == "Wood fuel, coniferous")
NonConiferousAfrica <- subset(BothCountriesAfrica, subset = BothCountriesAfrica$Group.2 == "Wood fuel, non-coniferous")

ConiferousAfrica[ConiferousAfrica == 0] <- NA
ConiferousAfrica <- ConiferousAfrica[complete.cases(ConiferousAfrica), ]

g <- ggplot(ConiferousAfrica, aes(x = ConiferousAfrica$Group.1, y = ConiferousAfrica$x))
g <- g + geom_bar(stat = "identity", position = "dodge") + ylab("Production Value (M3)") + xlab("Region")
g <- g + theme(axis.text.x = element_text(angle = 90))
g <- g + ggtitle("Coniferous WoodFuel Production in Africa [2000 ~ 2017]")
g

g <- ggplot(NonConiferousAfrica, aes(x = NonConiferousAfrica$Group.1, y = NonConiferousAfrica$x))
g <- g + geom_bar(stat = "identity", position = "dodge") + ylab("Production Value (M3)") + xlab("Region")
g <- g + theme(axis.text.x = element_text(angle = 90))
g <- g + ggtitle("Non-Coniferous WoodFuel Production in Africa [2000 ~ 2017]")
g


# Look at the net import export
AfricaNX <- Region.Criteria(Africa, element = 5922, year = 2000)
TotalAfricaNX <- aggregate(AfricaNX$Value, by = list(AfricaNX$Area,AfricaNX$Element), FUN = sum, na.rm = T)
colnames(TotalAfricaNX) <- c("Location", "Type", "Value")
g <- ggplot(TotalAfricaNX, aes(x = TotalAfricaNX$Location, y = TotalAfricaNX$Value, fill = TotalAfricaNX$Type))
g <- g + geom_bar(stat = "identity", position = "dodge") + ylab("Export/Import Value ($1000)") + xlab("Region")
g <- g + theme(axis.text.x = element_text(angle = 90)) + scale_fill_discrete(name = "Variable")
g <- g + ggtitle("Wood Fuel Export/Import in Africa [2000 ~ 2017]")
g

# Overproducing countries
OverProduced <- as.character(CountriesAfrica[which(CountriesAfrica$x >= bsCI[2]), 1])
OverProduced
# Justproducing countries
RightProduced <- as.character(CountriesAfrica[which(CountriesAfrica$x <= bsCI[2] & CountriesAfrica$x >= bsCI[1]), 1])


# Let's Look at the overproduced countries closely
par(mfrow = c(3, 3))
OverProduceAfrica <- Africa2[which(Africa2$Area %in% OverProduced), ]
for (i in 1:length(OverProduced)) {
  dt <- subset(OverProduceAfrica, subset = OverProduceAfrica$Area == OverProduced[i])
  agg.dt <- aggregate(dt$Value, by = list(dt$Year), FUN = sum, na.rm = T)
  plot(x = agg.dt$Group.1, y = agg.dt$x, main = OverProduced[i], xlab = "Year", ylab = "Prod. M3")
}
par(mfrow = c(1, 1))

# Let's look at the forest land changes over time
par(mfrow = c(3, 3))
OverForestLandAfrica <- ForestLand[which(ForestLand$Area %in% OverProduced), ]
for (i in 1:length(OverProduced)) {
  dt <- subset(OverForestLandAfrica, subset = OverForestLandAfrica$Area == OverProduced[i])
  plot(x = dt$Year, y = dt$Value, main = OverProduced[i], xlab = "Year", ylab = "1000 ha")
}
par(mfrow = c(1, 1))

# Let's look at the share of the forest land changes over time (Overproduced)
DcastedForest <- dcast(OverForestLandAfrica, formula = Area ~ Year , fill = NA, value.var = "Value")
Forest.Africa.Share <- DcastedForest
rownames(Forest.Africa.Share) <- Forest.Africa.Share[, 1]
Forest.Africa.Share <- Forest.Africa.Share[, -1]

for (i in 1:ncol(Forest.Africa.Share)) {
  Forest.Africa.Share[, i] <- round(Forest.Africa.Share[, i] * 100 / AfricaForest[i, "Value"], 2)
}
Forest.Africa.Share <- Forest.Africa.Share[order(Forest.Africa.Share$`2016`, decreasing = T), ]
knitr::kable(Forest.Africa.Share[, seq(1, ncol(Forest.Africa.Share), by = 2)])

plot(dt2$Value, agg.dt$x[-length(agg.dt$x)])
agg.dt

########################################################
nullDiffs <- numeric(10000)
dataBelow <- subset(agg.dt, subset = agg.dt$Group.1 <= 2008)[, 2]
dataAbove <- subset(agg.dt, subset = agg.dt$Group.1 > 2008)[, 2]
# Combine the data 
ProductionInRegion <- c(dataBelow, dataAbove)
Labels <- rep(c("Below", "Above"), c(length(dataBelow), length(dataAbove)))

set.seed(123)
for (j in 1:length(nullDiffs)) {
  LabelPerm <- sample(Labels, replace = FALSE)
  nullDiffs[j] <- mean(ProductionInRegion[LabelPerm == "Above"]) - mean(ProductionInRegion[LabelPerm == "Below"])
}
obsDiff <- mean(ProductionInRegion[Labels == "Above"]) - mean(ProductionInRegion[Labels == "Below"])
DifferencePvalues <- rbind(DifferencePvalues,sum(nullDiffs >= obsDiff) / length(nullDiffs))





# Let's look at the overproduced and population
par(mfrow = c(3, 3))
DifferencePvalues <- NULL
OverProduceAfrica <- Africa2[which(Africa2$Area %in% OverProduced), ]
for (i in 1:length(OverProduced)) {
  dt <- subset(OverProduceAfrica, subset = OverProduceAfrica$Area == OverProduced[i])
  agg.dt <- aggregate(dt$Value, by = list(dt$Year), FUN = sum, na.rm = T)
  
  ########################################################
  nullDiffs <- numeric(10000)
  dataBelow <- subset(agg.dt, subset = agg.dt$Group.1 <= 2008)[, 2]
  dataAbove <- subset(agg.dt, subset = agg.dt$Group.1 > 2008)[, 2]
  # Combine the data 
  ProductionInRegion <- c(dataBelow, dataAbove)
  Labels <- rep(c("Below", "Above"), c(length(dataBelow), length(dataAbove)))
  
  set.seed(123)
  for (j in 1:length(nullDiffs)) {
    LabelPerm <- sample(Labels, replace = FALSE)
    nullDiffs[j] <- mean(ProductionInRegion[LabelPerm == "Above"]) - mean(ProductionInRegion[LabelPerm == "Below"])
  }
  obsDiff <- mean(ProductionInRegion[Labels == "Above"]) - mean(ProductionInRegion[Labels == "Below"])
  DifferencePvalues <- rbind(DifferencePvalues,sum(nullDiffs >= obsDiff) / length(nullDiffs))
  ############################################
  popRegion <- subset(Population, subset = Population$Area == OverProduced[i] & Population$Year >= 2000 & Population$Year <= 2017 & Population$Element == "Total Population - Both sexes")
  plot(y = popRegion$Value, x = agg.dt$x, main = paste("Pop. vs. production in", OverProduced[i]), xlab = "Production (M3)", ylab = "in 1000 persons")
}
rownames(DifferencePvalues) <- OverProduced
colnames(DifferencePvalues) <- "P-value"
knitr::kable(DifferencePvalues)
par(mfrow = c(1, 1))








# NX in overproduced: Not Kenya, Mozambique, Uganda
OverNEAfrica <- AfricaNX[which(AfricaNX$Area %in% OverProduced), ]
plots <- list()
for (i in 1:length(OverProduced)) {
  title <- paste("NX in", OverProduced[i])
  dt <- subset(OverNEAfrica, subset = OverNEAfrica$Area == OverProduced[i])
  g <- ggplot(dt, mapping = aes(x = Year, y = Value, group = Element))
  g <- g + geom_line(aes(col = Element), stat = "identity") + geom_point(aes(col = Element))
  g <- g + labs(x = "Year", y = "1000 USD") + ggtitle(title)
  plots[[i]] <- g
}
multiplot(plotlist = plots, cols = 3)

# NX in overproduced: Not Kenya, Mozambique, Uganda QTY
AfricaNXQuantity <- Region.Criteria(Africa, element = 5616, year = 2000)
OverNEAfricaQuantity <- AfricaNXQuantity[which(AfricaNXQuantity$Area %in% OverProduced), ]
plots <- list()
for (i in 1:length(OverProduced)) {
  title <- paste("NX Qty. in", OverProduced[i])
  dt <- subset(OverNEAfricaQuantity, subset = OverNEAfricaQuantity$Area == OverProduced[i])
  g <- ggplot(dt, mapping = aes(x = Year, y = Value, group = Element))
  g <- g + geom_line(aes(col = Element), stat = "identity") + geom_point(aes(col = Element))
  g <- g + labs(x = "Year", y = "Quantity (M3)") + ggtitle(title)
  plots[[i]] <- g
}
multiplot(plotlist = plots, cols = 3)
