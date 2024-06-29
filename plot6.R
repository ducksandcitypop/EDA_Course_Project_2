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

NEIBaltLAMotorVehicle <- filter(NEI, fips== "24510" | fips == "06037") 
NEIBaltLAMotorVehicle <- NEIBaltLAMotorVehicle[NEIBaltLAMotorVehicle$SCC %in% SCCMask,]

# Compute total emissions by fips and Year and create summary tibble
# Plot data as a bar graph by year, grouped by fips

NEIBaltLAMotorVehicle |> group_by(fips, Year) |>
  summarize(Emissions = sum(Emissions)) |>
  ggplot(aes(factor(Year), Emissions, fill=fips)) +
  geom_bar(position="dodge", stat="identity") +
  geom_text(
    aes(x = factor(Year), y = Emissions,
        label = paste("(", as.character(trunc(Emissions*100,2)/100), ")", sep="")),
    position=position_dodge(width=0.9),
    vjust = -0.5,
    size=4) +
  labs(x = "Years", y="Emissions [PM 2.5] (in Tons)", 
       title="Total Emissions [PM 2.5], Baltimore City vs. Los Angeles", 
       subtitle="Years 1999-2008", fill="City") +
  scale_fill_discrete(labels=c("LA", "Baltimore\nCity"))

ggsave("plot6.png")