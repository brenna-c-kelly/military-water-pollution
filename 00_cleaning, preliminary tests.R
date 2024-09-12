
library(sf)
library(tmap)
library(INLA)
library(dplyr)
library(scales)
library(tidycensus)

aea <-  "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +ellps=GRS80 +datum=NAD83"

### tri, to get military facilities (included in fed)
tri <- read.csv("/Users/brenna/Downloads/2022_us-2.csv")
names(tri) <- tolower(names(tri))

tri_pfas <- tri #|>
  # filter(`x48..pfas` == "YES")

aggregate(tri$x53..5.3...water, by = list(tri$x21..federal.facility), FUN = sum)

p99 <- quantile(tri$x53..5.3...water, 0.9999)
tri[which(tri$x53..5.3...water >= p99), ]

aggregate(tri$x53..5.3...water, by = list(tri$x53..5.3...water >= p99, tri$x21..federal.facility),
          FUN = sum)

# defense <- tri |> filter(x15..parent.co.name == "US DEPARTMENT OF DEFENSE")

head(defense)

# tri_21 <- read.csv("/Users/brenna/Downloads/2021_us.csv")
# 
# tri <- rbind(tri, tri_21)

setdiff(rsei$tri.facility.id, tri$x2..trifd) # rsei not in tri


names(tri) <- tolower(names(tri))

tri_fed <- tri #|>
  #filter(`x21..federal.facility` == "YES")

### rsei, to get chemical, risk score, modeled hazard, modeled pounds
rsei_1 <- read.csv("data/rsei_12_22.csv")
rsei_2 <- read.csv("data/rsei_00_11.csv")
rsei_3 <- read.csv("data/rsei_88_99.csv")

rsei <- rbind(rsei_1, rsei_2, rsei_3)

names(rsei) <- tolower(names(rsei))

tri_fed$defense <- ifelse(tri_fed$x15..parent.co.name == "US DEPARTMENT OF DEFENSE",
                          1, 0)

tri_fed_cl <- tri_fed[, c("x2..trifd",
                          "x30..primary.naics",
                          "x21..federal.facility",
                          "defense",
                          "x12..latitude",
                          "x13..longitude")]

names(tri_fed_cl) <- c("trifd", "primary_naics", "federal_facility", 
                      "defense", "latitude", "longitude")

rsei_fed <- merge(rsei, tri_fed_cl, 
                  by.x = "tri.facility.id", 
                  by.y = "trifd", all.y = TRUE)

rsei_fed <- rsei_fed |>
  filter(!is.na(longitude))

rsei_fed <- st_as_sf(rsei_fed, coords = c("longitude", "latitude"), crs = 4326)
rsei_fed <- st_transform(rsei_fed, crs = aea)

rsei_fed$rsei.modeled.hazard <- as.numeric(gsub(",", "", rsei_fed$rsei.modeled.hazard))
rsei_fed$rsei.modeled.pounds <- as.numeric(gsub(",", "", rsei_fed$rsei.modeled.pounds))
rsei_fed$rsei.score <- as.numeric(gsub(",", "", rsei_fed$rsei.score))

### county shapefiles
us <- get_decennial(geography = "county",
                    variables = "DP1_0001C",
                    year = 2020,
                    sumfile = "dp",
                    geometry = TRUE)

us <- st_transform(us, crs = aea)
# st_crs(us) == st_crs(rsei_fed) # weird as fuck, but they only map together when crs is mismatched

## simple map
rsei_fed$rsei_log <- log(rsei_fed$rsei.score + 1)

tmap_mode(mode = "plot")
tm_shape(us) +
  tm_polygons(col = "white", alpha = 0.25, border.col = "black", lwd = 0.25) +
  tm_shape(rsei_fed) +
  tm_dots(col = "rsei_log", size = 0.15, style = "cont", palette = "viridis")


st_crs(rsei_fed) == st_crs(us)

names(rsei_fed)
### aggregate pounds, SUM
years_pounds <- aggregate(rsei_fed$rsei.modeled.pounds, 
                          by = list(rsei_fed$submission.year,
                                    rsei_fed$defense), 
                          FUN = sum) |>
  rename(year = Group.1,
         defense = Group.2,
         modeled_pounds = x)

years_pounds$label <- ifelse(years_pounds$defense == 1, "Department of Defense", "Other")

ggplot(years_pounds, aes(x = year, y = modeled_pounds)) +
  geom_bar(stat = "identity") +
  facet_wrap(~defense, ncol=1, scales = "free") +
  geom_bar(stat = "identity", fill = "orangered") +
  xlab("Reported Year") +
  ylab("Average Pounds") +
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6)) +
  facet_wrap(~label, ncol=1, scales = "free")
### aggregate pounds, MAX
years_pounds <- aggregate(rsei_fed$rsei.modeled.pounds, 
                          by = list(rsei_fed$submission.year,
                                    rsei_fed$defense), 
                          FUN = max) |>
  rename(year = Group.1,
         defense = Group.2,
         max_modeled_pounds = x)

years_pounds$label <- ifelse(years_pounds$defense == 1, "Department of Defense", "Other")

ggplot(years_pounds, aes(x = year, y = max_modeled_pounds)) +
  geom_bar(stat = "identity") +
  facet_wrap(~defense, ncol=1, scales = "free") +
  geom_bar(stat = "identity", fill = "orangered3") +
  xlab("Reported Year") +
  ylab("Average Hazard") +
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6)) +
  facet_wrap(~label, ncol=1, scales = "free")



### aggregate hazard, SUM
years_hazard <- aggregate(rsei_fed$rsei.modeled.hazard, 
                          by = list(rsei_fed$submission.year,
                                    rsei_fed$defense), 
                          FUN = sum) |>
  rename(year = Group.1,
         defense = Group.2,
         modeled_hazard = x)

years_hazard$label <- ifelse(years_hazard$defense == 1, "Department of Defense", "Other")

ggplot(years_hazard, aes(x = year, y = modeled_hazard)) +
  geom_bar(stat = "identity", fill = "olivedrab2") +
  xlab("Reported Year") +
  ylab("Average Hazard") +
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6)) +
  facet_wrap(~label, ncol=1, scales = "free")
### aggregate hazard, MAX
years_hazard <- aggregate(rsei_fed$rsei.modeled.hazard, 
                          by = list(rsei_fed$submission.year,
                                    rsei_fed$defense), 
                          FUN = max) |>
  rename(year = Group.1,
         defense = Group.2,
         max_modeled_hazard = x)

years_hazard$label <- ifelse(years_hazard$defense == 1, "Department of Defense", "Other")

ggplot(years_hazard, aes(x = year, y = max_modeled_hazard)) +
  geom_bar(stat = "identity", fill = "olivedrab4") +
  facet_wrap(~defense, ncol=1, scales = "free") +
  xlab("Reported Year") +
  ylab("Max Hazard") +
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6)) +
  facet_wrap(~label, ncol=1, scales = "free")


### aggregate risk, SUM
years_risk <- aggregate(rsei_fed$rsei.score, 
                        by = list(rsei_fed$submission.year,
                                  rsei_fed$defense), 
                        FUN = mean) |>
  rename(year = Group.1,
         defense = Group.2,
         risk_score = x)

years_risk$label <- ifelse(years_risk$defense == 1, "Department of Defense", "Other")

ggplot(years_risk, aes(x = year, y = risk_score)) +
  geom_bar(stat = "identity", fill = "magenta1") +
  xlab("Reported Year") +
  ylab("Average Risk Score") +
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6)) +
  facet_wrap(~label, ncol=1, scales = "free")
### aggregate risk, MAX
years_risk <- aggregate(rsei_fed$rsei.score, 
                        by = list(rsei_fed$submission.year,
                                  rsei_fed$defense), 
                        FUN = max) |>
  rename(year = Group.1,
         defense = Group.2,
         max_risk_score = x)

years_risk$label <- ifelse(years_risk$defense == 1, "Department of Defense", "Other")

ggplot(years_risk, aes(x = year, y = max_risk_score)) +
  geom_bar(stat = "identity", fill = "magenta3") +
  xlab("Reported Year") +
  ylab("Max Risk Score") +
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6)) +
  facet_wrap(~label, ncol=1, scales = "free")




### extreme aggregate risk
q95 <- quantile(rsei_fed$rsei.score, 0.95, na.rm = TRUE)

rsei_fed$extreme <- ifelse(rsei_fed$rsei.score >= q95, 1, 0)

years_risk <- aggregate(rsei_fed$rsei.score, 
                        by = list(rsei_fed$submission.year,
                                  rsei_fed$federal_facility,
                                  rsei_fed$extreme), 
                        FUN = max) |>
  rename(year = Group.1,
         federal = Group.2,
         extreme = Group.3,
         max_risk_score = x)

ggplot(years_risk, aes(x = year, y = max_risk_score)) +
  geom_bar(stat = "identity", fill = "chartreuse3") +
  facet_wrap(~extreme+federal, ncol = 1, scales = "free")


aggregate(rsei_fed$rsei.score, by = list(rsei_fed$extreme), FUN = mean)

rsei_fed[which(rsei_fed$rsei.score >= )]







aggregate(rsei_fed$rsei.modeled.pounds)

hist(rsei_fed$rsei.modeled.pounds)


### trend analysis
#load Kendall library and PrecipGL dataset
library(Kendall)

#Perform the Mann-Kendall Trend Test
MannKendall(rsei_fed$rsei.score)

#Plot the time series data
plot(rsei_fed$rsei.score)

#Add a smooth line to visualize the trend 
lines(lowess(time(rsei_fed$rsei.score), rsei_fed$rsei.score), col='blue')


summary(
  inla(rsei_bin ~ 1, data = rsei_fed)
)








### check for autocorrelation

pts_oneyear <- rsei_fed |>
  distinct(geometry)
head(pts_oneyear)

knea <- knearneigh(st_coordinates(rsei_fed), longlat = TRUE)
k_nb <- knn2nb(knea)

# ****
tri2nb(coords, row.names = NULL)

# coords <- st_coordinates(rsei_fed)
boston.listw = nb2listw(boston.nb)

moran.test(boston$logCMEDV, 
           listw = boston.listw, 
           alternative = "two.sided", 
           randomisation = TRUE)





