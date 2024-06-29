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

# Idea: Find all SCC's with Coal Combustion data; create a boolean mask for coal-combustion sources
# Extract all valid SCC's; filter NEI by SCC's using boolean mask
# Summarize total emissions by year
# Plot total emissions by Year

CombCoalMask <- with(SCC, (grepl("[Cc]omb", EI.Sector) == TRUE &
                             (
                               grepl("[Cc]oal", SCC.Level.One) |
                                 grepl("[Cc]oal", SCC.Level.Two) |
                                 grepl("[Cc]oal", SCC.Level.Three) |
                                 grepl("[Cc]oal", SCC.Level.Four)
                             )
)
)

NEICombCoal <- NEI[NEI$SCC %in% SCC[CombCoalMask,]$SCC,]

NEICombCoal |> 
  group_by(Year) |>
  summarize(Emissions = sum(Emissions)/100000) |>
  ggplot(aes(factor(Year), Emissions, fill=factor(Year))) +
  geom_bar(stat = "identity") +
  ylim(0, 8) +
  geom_text(
    aes(label = paste("(", as.character(trunc(Emissions*100,2)/100), ")", sep="")),
    nudge_y = .5,
    size=6) +
  labs(x="Years", y = "Emissions [PM 2.5] (in Hundred Thousand Tons)", title="Total Emissions [PM 2.5] from Coal-Combustion Sources in the US", subtitle="Years 1999-2008", fill = "Year")
theme(plot.title = element_text(hjust = 0.5))

ggsave("plot4.png")