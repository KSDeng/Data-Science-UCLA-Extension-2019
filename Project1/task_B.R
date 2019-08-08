
# Clear variables
rm(list = ls())

# Make sure the packages installed and loaded.
install.packages('readxl')
install.packages('dplyr')
library(readxl)
library(dplyr)

# Import the data that is going to be used.
laz2017 = data.frame(read_excel("P01_LA zipcode payroll.xlsx", sheet = "2017"))
# Explore the class and structure.
# class(laz2017)
# str(laz2017)

# Only keep the variables that are needed, which accelerates computation and saves cache.
VARIABLES_NEEDED = c("Zip.Code", "Industry", "Employment")
laz2017 = laz2017 %>% select(VARIABLES_NEEDED)
# The following line has the same effect, but is less readable and the magic numbers can be confusing.
# laz2017 = laz2017[, c(-3, -4, -6)]    


# Get the total employment by zip code
TOTAL_SUB = "Total"
laz2017$Zip.Code = gsub("[0-9]+\\sTotal", TOTAL_SUB, laz2017$Zip.Code)

laz2017.employment.total = filter(laz2017, Zip.Code == TOTAL_SUB)
VARIABLES_NEEDED = c("Employment")
laz2017.employment.total = laz2017.employment.total %>% select(VARIABLES_NEEDED)
laz2017.employment.total$Employment = as.numeric(laz2017.employment.total$Employment)
COLUMN_NAMES = c("Total Employment")
colnames(laz2017.employment.total) = COLUMN_NAMES

# laz2017.employment.total = as.numeric(laz2017$Employment[laz2017$Zip.Code == TOTAL_SUB])

# Now remove the rows of "total"
laz2017 = filter(laz2017, Zip.Code != TOTAL_SUB)

# Replace '*'s in the Employment column with "0"
laz2017$Employment = gsub("\\*+", "0", laz2017$Employment)

# Convert column 1, 5 and 6 from characters to numeric
# laz2017[,c(1,5,6)] = sapply(laz2017[c(1,5,6)],as.numeric)

# Convert "Zip.Code" and "Employment" from characters to numeric
VARIABLES_TRANSFORMED = c("Zip.Code", "Employment")
laz2017[VARIABLES_TRANSFORMED] = sapply(laz2017[VARIABLES_TRANSFORMED], as.numeric)

# Get all zip codes as data frame.
zipCodes = as.data.frame(unique(laz2017$Zip.Code))
colnames(zipCodes) = c("Zip.Code")
laz2017.employment.total = cbind(zipCodes, laz2017.employment.total)

# Get Information and Tech member data.
laz2017.employment.information = filter(laz2017, Industry == "Information")
colnames(laz2017.employment.information) = c("Zip.Code", "Industry", "Information")
laz2017.employment.tech = filter(laz2017, Industry == "Professional, Scientific, & Technical Skills")
colnames(laz2017.employment.tech) = c("Zip.Code", "Industry", "Professional, Scientific, & Technical Skills")

# Merge all the columns needed.
MERGE_BY_COLUMN = "Zip.Code"
laz2017.employment.IT =
    merge(laz2017.employment.tech, merge(zipCodes, laz2017.employment.information, by = MERGE_BY_COLUMN, all.x = TRUE),
          by = MERGE_BY_COLUMN, all.y = TRUE)

laz2017.employment.conclude = merge(laz2017.employment.IT,laz2017.employment.total,by = "Zip.Code")
VARIABLES_NEEDED = c("Zip.Code", "Information", "Professional, Scientific, & Technical Skills", "Total Employment")
laz2017.employment.conclude = laz2017.employment.conclude %>% select(VARIABLES_NEEDED)
# Replace the "NA"s with 0.
laz2017.employment.conclude$Information[is.na(laz2017.employment.conclude$Information)] = 0
laz2017.employment.conclude$`Professional, Scientific, & Technical Skills`[is.na(laz2017.employment.conclude$`Professional, Scientific, & Technical Skills`)] = 0


# Calculate the IT ratio and save it to file.
laz2017.employment.conclude$IT_Ratio =
    (laz2017.employment.conclude$Information + laz2017.employment.conclude$`Professional, Scientific, & Technical Skills`) / laz2017.employment.conclude$`Total Employment`
laz2017.employment.conclude$IT_Ratio[is.na(laz2017.employment.conclude$IT_Ratio)] = 0
write.csv(laz2017.employment.conclude, "CA2017_IT_Raio.csv")



