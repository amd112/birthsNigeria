
#importing relevant libraries
library(knitr)
library(data.table)
library(dplyr)
library(ggplot2)
library(ggmap)
library(scales)

#setting seed to ensure that randomized calculations are constant
set.seed(50)

#importing file, removing column of ids
data = as.data.frame(fread("MotherData.csv", stringsAsFactors = TRUE))

#splitting data to mother related data and baby related data
mothers = data[, c(1:78, 439:ncol(data))]
babies = data[, c(1, 79:438)]

#converting baby data to long format and renaming columns
babies = reshape(babies, varying = names(babies[, -1]), sep = "_", direction = "long")
babies = select(babies, caseid, bord, b0, b1, b2, b7, b9, b4)
names(babies) = c("caseid", "bord", "twin", "month", "year", "months_lived", "lives_with", "gender")

#removing lines for mothers that didn't have a baby
babies = babies[complete.cases(babies[, "bord"]), ]
#creating dummy variable for babies who die before reaching one year
babies$inf_death = as.numeric(babies$months_lived < 13)
#setting inf_death to 0 for babies that are still alive
babies[is.na(babies$inf_death), "inf_death"] = 0

#selecting relevant mother data
mothers = select(mothers, caseid, v005, v113, v116, v119, 
                 v120, v121, v122, v123, v124, v125, v127, v128, v129, v130, v131, v133, 
                 v135, v136, v137, v150, v151, v152, v153, v155, v161, v191, v201, v212)
#renaming to help interpret results
names(mothers) = c("caseid", "samp_weight", "water", "toilet", "electricity", "radio", "tv", "fridge",
                   "bike", "motorcycle", "car", "floor", "walls", "roof", "religion", "ethnicity", 
                   "education", "usual", "housemem", "housechild","reltohead", "sexhead", "agehead", 
                   "teleph", "literacy", "cookingfuel", "wealthindex", "childrenborn", "agefirstbirth")

full = merge(mothers, babies)

#removing 9 from rows that do not contain true 9's
rows = full[, -c(2, 4, 20, 22, 23, 31, 32, 33, 35, 37)]
rows[rows == "9"] = NA
full[, -c(2, 4, 20, 22, 23, 31, 32, 33, 35, 37)] = rows

#removing 99, 999 (NA codes)
full[full == "99"] = NA
full[full == "999"] = NA
full[full == ""] = NA
full = droplevels(full)

#in binary variables, removing "not a dejure resident", since there is a variable containing that information, 
#and it is redundant. Converting everything to binary where applicable
#converting factors to characters so they are editable
full[, c(5:11, 24)] <- lapply(full[, c(5:11, 24)], as.character)
full[full == "no"] = "0"
full[full == "yes"] = "1"
#converting characters to numeric
full[, c(5:11, 24)] <- lapply(full[, c(5:11, 24)], as.numeric)

#getting the group id from the combined caseid
full$location = sapply(strsplit(as.character(full$caseid), "\\s+"), `[`, 2)

#aggregate data by location (aka town), and find the mean wealth index and mortality rate.
means = full %>%
  select(location, wealthindex, inf_death) %>%
  group_by(location) %>%
  summarise(wealthindex = mean(wealthindex),inf_death = mean(inf_death))

#scatterplot of mortality rate by wealth where each point is a town.
ggplot(means) + geom_point(aes(x = wealthindex, y = inf_death)) + xlab("Wealth Index") + 
  ylab("Infant Mortality Rate") + ggtitle("            Wealth and Infant Mortality in Nigeria")

#import lat long data
loc = as.data.frame(fread("Locations.csv"))
#merge with aggregated town level data
loc = merge(loc, means, by.x = "v001", by.y = "location")

#get the base map to overlay data on
map = get_map(location = c(lon = 8.5, lat = 8.5), zoom = 6)

#create map, color dots, and remove axes
ggmap(map) + geom_point(aes(x = lon, y = lat, color = inf_death), size = 2, data = loc, alpha = .75) + 
  scale_colour_gradient(low = muted("green"), high = "red", guide = "colourbar") + 
  labs(color = "% Inf. Mortality") +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(), 
        axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank())

#model infant mortality using just wealth index
m_wealth = glm(inf_death~wealthindex, data = full, family = binomial(link='logit'))

#more comprehensive model on infant mortality. 
m_full = glm(inf_death~education+housemem+wealthindex+agefirstbirth+bord+gender+(gender*wealthindex), data = full, family = binomial(link='logit'))
summary(m_full)$coefficients