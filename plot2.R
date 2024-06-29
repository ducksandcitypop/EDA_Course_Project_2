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

# Summarize total emissions for Baltimore City by Year and plot

png("plot2.png")

NEIBaltimore <- filter(NEI, fips == "24510")

NEIBaltimore |>
  group_by(Year) |>
  summarize(Emissions = sum(Emissions)/10^2) |>
  with(barplot(Emissions ~ Year,
               col = Year,
               main = "Total Emissions [PM 2.5] in Baltimore City by Year",
               xlab = "Years",
               ylab = "Emissions [PM 2.5] (Hundreds of Tons)",
               ylim = c(0,max(Emissions))
  )
  )

dev.off()