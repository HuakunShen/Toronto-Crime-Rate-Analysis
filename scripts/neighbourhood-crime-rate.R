# Setup: Install and Import R Librareis
# install.packages("opendatatoronto")
# install.packages("knitr")
# install.packages("janitor")
# install.packages("lubridate")
# install.packages("opendatatoronto")
# install.packages("tidyverse")
# install.packages("tidyr")
# install.packages("dplyr")
# install.packages("ggpubr")
library(knitr)
library(janitor)
library(opendatatoronto)
library(tidyverse)
library(tidyr)
library(dplyr)
library(ggpubr)

# Import Neighbourhood Crime Rates Dataset
package_id <- "fc4d95a6-591f-411f-af17-327e6c5d03c7"
package <- show_package(package_id)
# get all resources for this package
resources <- list_package_resources(package_id)
# identify datastore resources; by default, Toronto Open Data sets datastore resource format to CSV for non-geospatial and GeoJSON for geospatial resources
datastore_resources <- filter(resources, tolower(format) %in% c('csv', 'geojson'))
# load the first datastore resource as a sample
data <- filter(datastore_resources, row_number()==1) %>% get_resource()

# Preview a subset of the Original Dataset
cols_to_show <- c("Neighbourhood", "Assault_2014", "Assault_2015")
kable(head(data[cols_to_show]), "pipe", caption = "Preview of a subset of the original dataset")


# preprocess data, transform the original data frame
years <- c("2014", "2015", "2016", "2017", "2018", "2019", "2020")
categories <- c("Assault_","Assault_Rate","AutoTheft_","AutoTheft_Rate",
               "BreakAndEnter_","BreakAndEnter_Rate","Robbery_","Robbery_Rate",
               "TheftOver_","TheftOver_Rate","Homicide_","Homicide_Rate",
               "Shootings_","Shootings_Rate")
unique_neighbourhood <- unique(data$Neighbourhood)
unique_hood_id <- unique(data$Hood_ID)
categories <- c("Assault_","Assault_Rate","AutoTheft_","AutoTheft_Rate",
               "BreakAndEnter_","BreakAndEnter_Rate","Robbery_","Robbery_Rate",
               "TheftOver_","TheftOver_Rate","Homicide_","Homicide_Rate",
               "Shootings_","Shootings_Rate")
column.names <- c("hood_id", "year", "Neighbourhood", "F2020_Population_Projection", categories)
column.length <- length(column.names)
row.length <- length(years) * length(unique_hood_id)
mat <- matrix(ncol = column.length, nrow = 0)
df <- data.frame(matrix(ncol = column.length, nrow = 0))
colnames(df) <- column.names                    # set column names for data frame
## generate new data frame
for (hood_id in unique_hood_id) {
  hood_data <- data[data$Hood_ID == hood_id,]
  for (year in years) {
    v = c(hood_id, year, hood_data$Neighbourhood, hood_data$F2020_Population_Projection)
    for (category in categories) {
      col_name <- paste(category, year, sep="")
      val <- hood_data[[col_name]]
      if (is.null(val)) {
        v <- c(v, 0)
      } else {
        v <- c(v, val)
      }
    }
    df[nrow(df) + 1,] = v
  }
}

# rename columns, remove underscore from column names
for (category in categories) {
  df[[category]] = as.numeric(df[[category]])
}
df$F2020_Population_Projection = as.numeric(df$F2020_Population_Projection)
names(df)[names(df) == 'Assault_'] <- 'Assault'
names(df)[names(df) == 'AutoTheft_'] <- 'AutoTheft'
names(df)[names(df) == 'BreakAndEnter_'] <- 'BreakAndEnter'
names(df)[names(df) == 'Robbery_'] <- 'Robbery'
names(df)[names(df) == 'TheftOver_'] <- 'TheftOver'
names(df)[names(df) == 'Homicide_'] <- 'Homicide'
names(df)[names(df) == 'Shootings_'] <- 'Shootings'

# Preview a subset of Cleaned Dataset
cols_to_show <- c("Neighbourhood", "year", "Shootings_Rate", "Robbery_Rate", "Homicide_Rate")
knitr::kable(head(df[cols_to_show]), "pipe", caption="Cleaned Table (Subset)")

# group cleaned data by neighbourhood
df_gb <- df %>% group_by(Neighbourhood)
neighbourhood_mean_crime_rate <- df_gb %>% summarise(
  Assault_Rate=mean(Assault_Rate),
  AutoTheft_Rate=mean(AutoTheft_Rate),
  BreakAndEnter_Rate=mean(BreakAndEnter_Rate),
  Robbery_Rate=mean(Robbery_Rate),
  TheftOver_Rate=mean(TheftOver_Rate),
  Homicide_Rate=mean(Homicide_Rate),
  Shootings_Rate=mean(Shootings_Rate)
)

# Preview a subset of data after grouping by neighbourhood and averaging over all years on every crime type
cols_to_show <- c("Neighbourhood", "Shootings_Rate", "Robbery_Rate", "Homicide_Rate")
kable(head(neighbourhood_mean_crime_rate[cols_to_show]), "simple", caption = "Average Crime Rate By Neighbourhood (Subset)")

# Generate Shooting Rate By Neighbourhood
ggplot(neighbourhood_mean_crime_rate,
       aes(x = reorder(Neighbourhood, -Shootings_Rate), y = Shootings_Rate)) + 
  geom_bar(stat = "identity") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  xlab("Neightbourhood") + ylab("Shootings Rate") +
  ggtitle("Neighbourhood Shootings Rate (out of 100,000)")
ggsave("./outputs/images/neightbourhood-Shootings-Rate.png")

# group cleaned data by year
df_gb <- df %>% group_by(year)
mean_crime_rate_by_year <- df_gb %>% summarise(
  Assault_Rate=mean(Assault_Rate),
  AutoTheft_Rate=mean(AutoTheft_Rate),
  BreakAndEnter_Rate=mean(BreakAndEnter_Rate),
  Robbery_Rate=mean(Robbery_Rate),
  TheftOver_Rate=mean(TheftOver_Rate),
  Homicide_Rate=mean(Homicide_Rate),
  Shootings_Rate=mean(Shootings_Rate)
)
mean_crime_rate_by_year <- mean_crime_rate_by_year[order(mean_crime_rate_by_year$year),]

# preview dataset after groupping by year and averaging over all neighbourhood
cols_to_show <- c("year", "Shootings_Rate", "Robbery_Rate", "Homicide_Rate")
knitr::kable(head(mean_crime_rate_by_year[cols_to_show]), "pipe", caption="Average crime Rate By Year (Subset)")

# Generate crime rate by year histograms
assault_rate_plt <- ggplot(mean_crime_rate_by_year,
       aes(x = year, y = Assault_Rate)) + 
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  xlab("Year") + ylab("Assault Rate") +
  ggtitle("Assault Rate by year (out of 100,000)")
ggsave("./outputs/images/assault-rate-by-year.png")
auththeft_rate_plt <- ggplot(mean_crime_rate_by_year, 
       aes(x = year, y = AutoTheft_Rate)) + 
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  xlab("Year") + ylab("AutoTheft Rate") +
  ggtitle("AutoTheft Rate by year (out of 100,000)")
ggsave("./outputs/images/autotheft-rate-by-year.png")
breakAndEnter_plt <- ggplot(mean_crime_rate_by_year, 
       aes(x = year, y = BreakAndEnter_Rate)) + 
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  xlab("Year") + ylab("BreakAndEnter Rate") +
  ggtitle("BreakAndEnter Rate by year (out of 100,000)")
ggsave("./outputs/images/breakandenter-rate-by-year.png")
robbery_rate_plot <- ggplot(mean_crime_rate_by_year, 
       aes(x = year, y = Robbery_Rate)) + 
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  xlab("Year") + ylab("Robbery Rate") +
  ggtitle("Robbery Rate by year (out of 100,000)")
ggsave("./outputs/images/robbery-rate-by-year.png")
theftOver_rate_plt <- ggplot(mean_crime_rate_by_year, 
       aes(x = year, y = TheftOver_Rate)) + 
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  xlab("Year") + ylab("TheftOver Rate") +
  ggtitle("TheftOver Rate by year (out of 100,000)")
ggsave("./outputs/images/theftover-rate-by-year.png")
homicide_rate_plt <- ggplot(mean_crime_rate_by_year, 
       aes(x = year, y = Homicide_Rate)) + 
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  xlab("Year") + ylab("Homicide Rate") +
  ggtitle("Homicide Rate by year (out of 100,000)")
ggsave("./outputs/images/homicide-rate-by-year.png")
shootings_rate_plt <- ggplot(mean_crime_rate_by_year, 
       aes(x = year, y = Shootings_Rate)) + 
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  xlab("Year") + ylab("Shootings Rate") +
  ggtitle("Shootings Rate by year (out of 100,000)")
ggsave("./outputs/images/shootings-rate-by-year.png")

ggarrange(assault_rate_plt,
          auththeft_rate_plt,
          breakAndEnter_plt,
          robbery_rate_plot,
          theftOver_rate_plt,
          homicide_rate_plt,
          shootings_rate_plt,
          labels = c("A", "B", "C", "D", "E", "F", "G"),
          ncol = 2, nrow = 4)


toBibtex(citation("knitr"))
citation("janitor")
citation("opendatatoronto")
citation("tidyverse")
citation("tidyr")
citation("dplyr")
citation("ggpubr")








