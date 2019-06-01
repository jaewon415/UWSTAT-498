WF_AGGREGATE <- function(data = data, Year = Year, Prod = F, EINet = F, EIQuantity = F) {
  data <- subset(data, subset = data$Year >= Year)
  if (Prod == T) {
    data <- subset(data, subset = data$Element == "Production")
    result <- aggregate(x = data$Value, by = list(data$Area, data$Item), FUN = sum, na.rm = T)
  }
  if (EINet == T) {
    data <- subset(data, subset = (data$Element == "Import Value" | data$Element == "Export Value") & data$Item == "Wood fuel, all species (export/import)")
    result <- aggregate(x = data$Value, by = list(data$Area, data$Element), FUN = sum, na.rm = T)
  }
  if (EIQuantity == T) {
    data <- subset(data, subset = (data$Element == "Import Quantity" | data$Element == "Export Quantity") & data$Item == "Wood fuel, all species (export/import)")
    result <- aggregate(x = data$Value, by = list(data$Area, data$Element), FUN = sum, na.rm = T)
  }
  return(result)
}

# India Indonesia, China Mainland
AsiaCountriesProduction <- WF_AGGREGATE(data = AsiaRegion, Year = 2000, Prod = T)
g <- ggplot(AsiaCountriesProduction, aes(x = AsiaCountriesProduction$Group.1, y = AsiaCountriesProduction$x , fill = AsiaCountriesProduction$Group.2)) + geom_bar(stat = "identity", position = "dodge")
g <- g + ylab("Production Value (M3)") + xlab("Region") + ggtitle("Wood Fuel Production From 2000 to 2017")
g <- g + theme(axis.text.x = element_text(angle = 90)) + scale_fill_discrete(name = "Variable")
g

# Import value of turkey is very high
AsiaCountriesEIVal <- WF_AGGREGATE(data = AsiaRegion, Year = 2000, EINet = T)
g <- ggplot(AsiaCountriesEIVal, aes(x = AsiaCountriesEIVal$Group.1, y = AsiaCountriesEIVal$x , fill = AsiaCountriesEIVal$Group.2)) + geom_bar(stat = "identity", position = "dodge")
g <- g + ylab("In 1000 Dollars (USA)") + xlab("Region") + ggtitle("Wood Fuel Export Import Value From 2000 to 2017")
g <- g + theme(axis.text.x = element_text(angle = 90)) + scale_fill_discrete(name = "Variable")
g

# Malaysia high export and philipines high export and turkey had a high import
AsiaCountriesQuant <- WF_AGGREGATE(data = AsiaRegion, Year = 2000, EIQuantity = T)
g <- ggplot(AsiaCountriesQuant, aes(x = AsiaCountriesQuant$Group.1, y = AsiaCountriesQuant$x , fill = AsiaCountriesQuant$Group.2)) + geom_bar(stat = "identity", position = "dodge")
g <- g + ylab("Quantity (M3)") + xlab("Region") + ggtitle("Wood Fuel Export Import Quantity From 2000 to 2017")
g <- g + theme(axis.text.x = element_text(angle = 90)) + scale_fill_discrete(name = "Variable")
g

# Barplot for the wood production in the regions of Asia starting from 2000 
WF_Graph(data = AsiaRegion, Area = "Asia", YearAbove = 2000, ExportImport = F, EIQuantity = F)
WF_Graph(data = AsiaRegion, Area = "Asia", YearAbove = 2000, ExportImport = T, EIQuantity = F)
WF_Graph(data = AsiaRegion, Area = "Asia", YearAbove = 2000, ExportImport = T, EIQuantity = T)
# ====> India, China mainland, indonesia (Top 3)







#############################################
#############################################
#############################################
#############################################
#############################################

# Population and its relation to production (MALE)
# No data carribean, central america, south america, 
MaleAfricaPopulation <- subset(Population, subset = Population$Area == "Africa" & Population$Year >= 2000 & Population$Year <= 2017 & Population$Element == "Total Population - Male")
MaleAsiaPopulation <- subset(Population, subset = Population$Area == "Asia" & Population$Year >= 2000 & Population$Year <= 2017 & Population$Element == "Total Population - Male")
MaleOceaniaPopulation <- subset(Population, subset = Population$Area == "Oceania" & Population$Year >= 2000 & Population$Year <= 2017 & Population$Element == "Total Population - Male")
MaleEuropePopulation <- subset(Population, subset = Population$Area == "Europe" & Population$Year >= 2000 & Population$Year <= 2017 & Population$Element == "Total Population - Male")
MaleNorthAmericaPopulation <- subset(Population, subset = Population$Area == "Northern America" & Population$Year >= 2000 & Population$Year <= 2017 & Population$Element == "Total Population - Male")

MalePopulation <- cbind(MaleAfricaPopulation$Value, MaleAsiaPopulation$Value, MaleOceaniaPopulation$Value, MaleEuropePopulation$Value, MaleNorthAmericaPopulation$Value)
MalePopulation <- as.data.frame(MalePopulation)
colnames(MalePopulation) <- c("Africa", "Asia", "Oceania", "Europe", "NorthAmerica")

# Graphs of the individual continents by population
par(mfrow = c(3, 3))
for (i in 2:6) {
  title <- paste("Wood Fuel Production vs Male Pop. in", colnames(TotalContinents)[i])
  plot(TotalContinents[, i], MalePopulation[, i - 1], ylab = "Population (1000 persons)", xlab = "Production (M3)", main = title, type = "p", col = "blue", lwd = 1.5)
  
}
plot.new()

# Population and its relation to production (Female)
FemaleAfricaPopulation <- subset(Population, subset = Population$Area == "Africa" & Population$Year >= 2000 & Population$Year <= 2017 & Population$Element == "Total Population - Female")
FemaleAsiaPopulation <- subset(Population, subset = Population$Area == "Asia" & Population$Year >= 2000 & Population$Year <= 2017 & Population$Element == "Total Population - Female")
FemaleOceaniaPopulation <- subset(Population, subset = Population$Area == "Oceania" & Population$Year >= 2000 & Population$Year <= 2017 & Population$Element == "Total Population - Female")
FemaleEuropePopulation <- subset(Population, subset = Population$Area == "Europe" & Population$Year >= 2000 & Population$Year <= 2017 & Population$Element == "Total Population - Female")
FemaleNorthAmericaPopulation <- subset(Population, subset = Population$Area == "Northern America" & Population$Year >= 2000 & Population$Year <= 2017 & Population$Element == "Total Population - Female")
FemaleCentralAmericaPopulation <- subset(Population, subset = Population$Area == "Central America" & Population$Year >= 2000 & Population$Year <= 2017 & Population$Element == "Total Population - Female")
FemaleSouthAmericaPopulation <- subset(Population, subset = Population$Area == "South America" & Population$Year >= 2000 & Population$Year <= 2017 & Population$Element == "Total Population - Female")
FemaleCaribbeanPopulation <- subset(Population, subset = Population$Area == "Caribbean" & Population$Year >= 2000 & Population$Year <= 2017 & Population$Element == "Total Population - Female")

FemalePopulation <- cbind(FemaleAfricaPopulation$Value, FemaleAsiaPopulation$Value, FemaleOceaniaPopulation$Value, FemaleEuropePopulation$Value, FemaleNorthAmericaPopulation$Value,
                          FemaleCentralAmericaPopulation$Value, FemaleSouthAmericaPopulation$Value, FemaleCaribbeanPopulation$Value)
FemalePopulation <- as.data.frame(FemalePopulation)
colnames(FemalePopulation) <- colnames(TotalContinents)[-1]

# Graphs of the individual continents by population
par(mfrow = c(3, 3))
for (i in 2:ncol(TotalContinents)) {
  title <- paste("WF Production vs Female Pop. in", colnames(TotalContinents)[i])
  plot(TotalContinents[, i], FemalePopulation[, i - 1], ylab = "Population (1000 persons)", xlab = "Production (M3)", main = title, type = "p", col = "blue", lwd = 1.5)
  
}
plot.new()
