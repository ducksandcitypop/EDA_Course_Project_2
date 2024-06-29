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

# Create another boolean mask for Motor-Vehicle sources
# Extract all valid SCC's; filter NEI by SCC's using boolean mask
# Summarize total emissions by year
# Plot total emissions by Year

MotorVehicleMask <- with(SCC, (grepl("(?=.*[Mm]obile)(?=.*[Vv]ehicle)", EI.Sector, perl=TRUE) 
)
)

SCCMask = SCC[MotorVehicleMask,]$SCC
NEIBaltMotorVehicle <- NEIBaltimore[NEIBaltimore$SCC %in% SCCMask,]

NEIBaltMotorVehicle |>
  group_by(Year) |>
  summarize(Emissions = sum(Emissions)) |>
  ggplot(aes(factor(Year), Emissions, fill=factor(Year))) +
  geom_bar(stat="identity") +
  geom_text(
    aes(label = paste("(", as.character(trunc(Emissions*100,2)/100), ")", sep="")),
    nudge_y = 18,
    size=6) +
  ylim(0,370) +
  labs(x="Years", y = "Emissions [PM 2.5] (in Tons)", title="Total Emissions [PM 2.5] from Motor-Vehicle Sources in Baltimore City", subtitle="Years 1999-2008", fill = "Year") +
  theme(plot.title = element_text(hjust = 0.5))

ggsave("plot5.png")