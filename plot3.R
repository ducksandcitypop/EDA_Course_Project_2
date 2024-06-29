library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(stringr)

# Loading the Data

# Notes:
# NEI$fips: 5-digit codes of counties; some values are "NA's" (need to parse); (some counties codes missing)
# NEI$SCC: 8-code character indicating source of measurement; looks good
# NEI$Pollutant,Emissions, type, year: looks good

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# Notes:
# After examination, EI.Sector and SCC.Level.One look like good candidates for the description of sources
# Perhaps SCC.Level.One -- SCC.Level.Three can help us refine what type of descriptive data we are looking for

# Convert dataframes into tibbles 
NEI <- tibble(NEI)
NEI <- rename(NEI, Year = year)
SCC <- tibble(SCC)

# Summarize total emissions by Type and Year
# Create a bar plot, faceting by type, and according to year

NEIBaltimoreFacet <- filter(NEI, fips == "24510") |>
  group_by(type, Year) |>
  summarize(Emissions = sum(Emissions)/100) 

ggplot(data = NEIBaltimoreFacet, aes(factor(Year), Emissions, fill=type)) + 
  geom_bar(stat = "Identity") +
  geom_text(data = NEIBaltimoreFacet, 
            aes(label = paste("(", as.character(trunc(Emissions*100,2)/100), ")", sep="")),
            nudge_y = 1,
            size=4) +
  facet_grid(~ type) +
  labs(x="Years", y = "Total Emissions [PM 2.5] (Hundreds of Tons)", title="Total Emissions [PM 2.5] in Baltimore City from 1999-2008", subtitle="Arranged by Type", fill = "Type")
theme(plot.title = element_text(hjust = 0.5))

ggsave("plot3.png")