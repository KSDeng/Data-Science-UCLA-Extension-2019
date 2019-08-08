rm(list = ls())
# install.packages("readxl")
# install.packages("dplyr")
library(readxl)
library(dplyr)

# meta09 = data.frame(read.csv("ACS_DP02 data/ACS_09_5YR_DP02_metadata.csv", header = FALSE))

# Function: get data and calculate CHCI
getCHCI = function(dataSource) {
    VARIABLES_NEEDED = c(2, 3, 232, 238, 242, 246, 250, 254, 258, 262)
    acsData = data.frame(read.csv(dataSource, skip = 1))[, VARIABLES_NEEDED]
    colnames(acsData) = c("id", "County", "Population", "Less than 9th grade", "9th to 12th grade, no diploma",
         "High school graduate (includes equivalency)", "Some college, no degree", "Associate's degree",
         "Bachelor's degree", "Graduate or professional degree")
    # str(acsData)
    acsData$County = as.character(acsData$County)
    # Calculate CHCI
    acsData$CHCI = 0.5 * acsData$`Less than 9th grade` + 1 * acsData$`9th to 12th grade, no diploma` +
    1.2 * acsData$`High school graduate (includes equivalency)` + 1.3 * acsData$`Some college, no degree` +
    1.4 * acsData$`Associate's degree` + 1.9 * acsData$`Bachelor's degree` +
    2.3 * acsData$`Graduate or professional degree`
    returnMatrix = acsData %>% select(c("id","County","CHCI"))
}
# Function: get certain column(s) of data
getColumn = function(dataSource, columnId, columnName) {
    res = as.data.frame(data.frame(read.csv(dataSource, skip = 1))[, columnId])
    colnames(res) = columnName
    return(res)
}
# Calculate all CHCI
CHCI09 = getCHCI("ACS_DP02 data/ACS_09_5YR_DP02_with_ann.csv")
CHCI10 = getCHCI("ACS_DP02 data/ACS_10_5YR_DP02_with_ann.csv")
CHCI11 = getCHCI("ACS_DP02 data/ACS_11_5YR_DP02_with_ann.csv")
CHCI12 = getCHCI("ACS_DP02 data/ACS_12_5YR_DP02_with_ann.csv")
CHCI13 = getCHCI("ACS_DP02 data/ACS_13_5YR_DP02_with_ann.csv")
CHCI14 = getCHCI("ACS_DP02 data/ACS_14_5YR_DP02_with_ann.csv")
CHCI15 = getCHCI("ACS_DP02 data/ACS_15_5YR_DP02_with_ann.csv")
CHCI16 = getCHCI("ACS_DP02 data/ACS_16_5YR_DP02_with_ann.csv")
CHCI17 = getCHCI("ACS_DP02 data/ACS_17_5YR_DP02_with_ann.csv")
# Get all population data
POP09 = getColumn("ACS_DP02 data/ACS_09_5YR_DP02_with_ann.csv", columnId = c(2,232), columnName = c("id","pop09"))
POP10 = getColumn("ACS_DP02 data/ACS_10_5YR_DP02_with_ann.csv", columnId = c(2, 232), columnName = c("id", "pop10"))
POP11 = getColumn("ACS_DP02 data/ACS_11_5YR_DP02_with_ann.csv", columnId = c(2, 232), columnName = c("id", "pop11"))
POP12 = getColumn("ACS_DP02 data/ACS_12_5YR_DP02_with_ann.csv", columnId = c(2, 232), columnName = c("id", "pop12"))
POP13 = getColumn("ACS_DP02 data/ACS_13_5YR_DP02_with_ann.csv", columnId = c(2, 232), columnName = c("id", "pop13"))
POP14 = getColumn("ACS_DP02 data/ACS_14_5YR_DP02_with_ann.csv", columnId = c(2, 232), columnName = c("id", "pop14"))
POP15 = getColumn("ACS_DP02 data/ACS_15_5YR_DP02_with_ann.csv", columnId = c(2, 232), columnName = c("id", "pop15"))
POP16 = getColumn("ACS_DP02 data/ACS_16_5YR_DP02_with_ann.csv", columnId = c(2, 232), columnName = c("id", "pop16"))
POP17 = getColumn("ACS_DP02 data/ACS_17_5YR_DP02_with_ann.csv", columnId = c(2, 232), columnName = c("id", "pop17"))

# Function: merge a list of dataframes
mergeFunc = function(dataFrameList, mergeBy) {
    res = dataFrameList[[1]] %>% select(mergeBy)
    for (df in dataFrameList) {
        res = merge(res,df,by = mergeBy, all.x = TRUE)
    }
    return(res)
}
# Merge all the data frame
CHCI = mergeFunc(list(CHCI09, CHCI10, CHCI11, CHCI12, CHCI13, CHCI14, CHCI15, CHCI16, CHCI17),c("id","County"))
colnames(CHCI) = c("id","county","chci09","chci10","chci11","chci12","chci13","chci14","chci15","chci16","chci17")
CHCI_POP = mergeFunc(list(CHCI, POP09, POP10, POP11, POP12, POP13, POP14, POP15, POP16, POP17), mergeBy = c("id"))
# Calculate chci/population growth and growth rate
CHCI_POP$chcig = CHCI_POP$chci17 - CHCI_POP$chci09
CHCI_POP$chcigr = CHCI_POP$chcig / CHCI_POP$chci09
CHCI_POP$popg = CHCI_POP$pop17 - CHCI_POP$pop09
CHCI_POP$popgr = CHCI_POP$popg / CHCI_POP$pop09
# Save result to file.
write.csv(CHCI_POP, "CHCI_POP.csv")

# Order the data frame by chicgr and popgr respectively.
Highest_chcigr = CHCI_POP[order(-CHCI_POP$chcigr),]
Highest_popgr = CHCI_POP[order(-CHCI_POP$popgr),]
# Top 10 counties in CHCI growth rate
chcigr_top10 = Highest_chcigr[1:10,]
write.csv( (chcigr_top10 %>% select(id,county, chcigr)),"CHCIGR_top10.csv", row.names = FALSE)
chcigr_top10$county
# Top 10 counties in population growth rate
popgr_top10 = Highest_popgr[1:10,]
popgr_top10$county
write.csv((popgr_top10 %>% select(id,county,popgr)),"POPGR_top10.csv", row.names = FALSE)

# Plot chci16 on the map
library(maps)
library(ggplot2)
library(RColorBrewer)
# Load map data of counties and states.
county_map = map_data("county")
county2 = county_map %>% mutate(polyname = paste(region, subregion, sep = ",")) %>%
    left_join(county.fips, by = "polyname")
state_map = map_data("state")

colnames(CHCI_POP)[1] = "fips"
map_data = county2 %>% left_join(CHCI_POP, by = "fips")

# I don't know why using merge() doesn't work
# map_data = merge(CHCI_POP, county2, by = "fips", everything = FALSE, all.x = TRUE)
# write.csv(map_data, "map_data_merge.csv")

# Get state layer from state map data.
state_layer = geom_polygon(aes(long, lat, group = group), fill = NA, data = state_map, color = "black")
# Plot CHCI distribution, using color theme from colorbrewer2.org(PuBu)
ggplot(map_data, aes(long, lat, group = group)) + geom_polygon(aes(fill = chci17)) +
    coord_quickmap() + scale_fill_gradient(low = "#f7f7f0", high = "#084081") + state_layer

# Plot CHCI distribution discretely using color theme from colorbrewer2.org
library(RColorBrewer)
qt = quantile(map_data$chci17, probs = seq(0, 1, 0.1), na.rm = TRUE)
map_data$chci17_cut = cut(map_data$chci17, breaks = qt, labels = paste(qt[-1]))

ggplot(map_data, aes(long, lat, group = group)) + geom_polygon(aes(fill = chci17_cut)) +
    coord_quickmap() + scale_fill_brewer(palette = "RdYlBu") + state_layer

