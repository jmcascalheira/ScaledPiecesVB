# Filter 2 damaged platforms

morpho_data_platA <- morpho_data %>%
  filter(DamagedPlatforms == "2" & RawMaterial != "Chalcedony") %>%
  select(4,5,7:18)

cnames <- colnames(morpho_data_platA)
cnames <- cnames %>%
  str_replace("_PlatformA", "")
names(morpho_data_platA) <- cnames


morpho_data_platB <- morpho_data %>%
  filter(DamagedPlatforms == "2" & RawMaterial != "Chalcedony") %>%
  select(4,5,19:30)

cnames <- colnames(morpho_data_platB)
cnames <- cnames %>%
  str_replace("_PlatformB", "")
names(morpho_data_platB) <- cnames


# Bind both tables into morpho_data_2plat

morpho_data_2plat <- bind_rows(morpho_data_platA, morpho_data_platB)
morpho_data_2plat <- select_if(morpho_data_2plat, is.character)


# Run condense() to combine all attributes with presence lower than 10%

var_list <- colnames(morpho_data_2plat)

morpho_data_2plat <- condense_to_other(morpho_data_2plat, "DamageDegree")
morpho_data_2plat <- condense_to_other(morpho_data_2plat, "ScarShape")
morpho_data_2plat <- condense_to_other(morpho_data_2plat, "ScarDistribution")
morpho_data_2plat <- condense_to_other(morpho_data_2plat, "ScarArrangement")
morpho_data_2plat <- condense_to_other(morpho_data_2plat, "ScarExtention")
morpho_data_2plat <- condense_to_other(morpho_data_2plat, "ScarFacialDistribution")
morpho_data_2plat <- condense_to_other(morpho_data_2plat, "ScarEdgeDelineation")
morpho_data_2plat <- condense_to_other(morpho_data_2plat, "Angle")

# Run condense() to combine all attributes with presence lower than 10%

morpho_data_2plat <- condense_to_NA(morpho_data_2plat, "DamageDegree")
morpho_data_2plat <- condense_to_NA(morpho_data_2plat, "ScarShape")
morpho_data_2plat <- condense_to_NA(morpho_data_2plat, "ScarDistribution")
morpho_data_2plat <- condense_to_NA(morpho_data_2plat, "ScarArrangement")
morpho_data_2plat <- condense_to_NA(morpho_data_2plat, "ScarExtention")
morpho_data_2plat <- condense_to_NA(morpho_data_2plat, "ScarFacialDistribution")
morpho_data_2plat <- condense_to_NA(morpho_data_2plat, "ScarEdgeDelineation")
morpho_data_2plat <- condense_to_NA(morpho_data_2plat, "Angle")


# Application of CHI-SQUARE to all variables across data.frame

morpho_data_2plat <- as.data.frame(morpho_data_2plat) # needed to use lapply()

morpho_data_2plat_RM <- split(morpho_data_2plat, morpho_data_2plat$RawMaterial)


# Identify all possible combination of variables in the data.frame
ind<-combn(NCOL(morpho_data_2plat),2)

# Run CHI() function
results <- lapply(1:NCOL(ind), function (i) CHI(morpho_data_2plat[,ind[1,i]],morpho_data_2plat[,ind[2,i]]))

results <- data.frame(sapply(results,c)) # convert to data.frame

# Join p-values with var names

ind2<-combn(colnames(morpho_data_2plat),2)
results <- cbind(ind2,results)

# Filter only small p-values
results <- results %>%
  filter(sapply.result..c. < 0.05)
