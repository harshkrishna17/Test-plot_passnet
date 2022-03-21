# Packages

library(tidyverse)
library(StatsBombR)
library(ggsoccer)
library(ggshakeR)
library(viridis)
library(gridExtra)
library(glue)
library(useful)
library(scales)

# Opta 

optadata <- read.csv("bay.csv")

## Plugging in team and player names 

optadata <- optadata %>%
  mutate(teamId = case_when(teamId == 37 ~ "Bayern",
                            teamId == 361 ~ "Salzburg")) %>%
  mutate(playerId = case_when(playerId == "14295" ~ "Alexander Walke",
                              playerId == "17827" ~ "Zlatko Junuzovic",
                              playerId == "25032" ~ "Andreas Ulmer",
                              playerId == "29400" ~ "Robert Lewandowski",
                              playerId == "29809" ~ "Jean-Eric Maxim Choupo-Moting",
                              playerId == "35506" ~ "Sven Ulreich",
                              playerId == "37099" ~ "Thomas Müller",
                              playerId == "92051" ~ "Marcel Sabitzer",
                              playerId == "104747" ~ "Bouna Sarr",
                              playerId == "119501" ~ "Serge Gnabry",
                              playerId == "125883" ~ "Kingsley Coman",
                              playerId == "129983" ~ "Niklas Süle",
                              playerId == "134894" ~ "Corentin Tolisso",
                              playerId == "139186" ~ "Lucas Hernández",
                              playerId == "144711" ~ "Leroy Sané",
                              playerId == "259648" ~ "Benjamin Pavard",
                              playerId == "283323" ~ "Joshua Kimmich",
                              playerId == "301024" ~ "Maximilian Wöber",
                              playerId == "302812" ~ "Rasmus Kristensen",
                              playerId == "323148" ~ "Marc Roca",
                              playerId == "327721" ~ "Dayot Upamecano",
                              playerId == "342489" ~ "Omar Richards",
                              playerId == "342549" ~ "Christian Früchtl",
                              playerId == "346744" ~ "Antoine Bernede",
                              playerId == "354072" ~ "Oumar Solet",
                              playerId == "369480" ~ "Nicolás Capaldo",
                              playerId == "371012" ~ "Brenden Aaronson",
                              playerId == "376090" ~ "Noah Okafor",
                              playerId == "382492" ~ "Tanguy Nianzou",
                              playerId == "383689" ~ "Philipp Köhn",
                              playerId == "392645" ~ "Mohamed Camara",
                              playerId == "392646" ~ "Karim Adeyemi",
                              playerId == "397465" ~ "Malik Tillman",
                              playerId == "402849" ~ "Ignace van der Brempt",
                              playerId == "407080" ~ "Luka Sucic",
                              playerId == "410520" ~ "Nico Mantl",
                              playerId == "412348" ~ "Kamil Piatkowski",
                              playerId == "413272" ~ "Josip Stanisic",
                              playerId == "418587" ~ "Nicolas Seiwald",
                              playerId == "418590" ~ "Junior Chukwubuike Adamu",
                              playerId == "424367" ~ "Roko Simic",
                              playerId == "424368" ~ "Gabriel Vidovic",
                              playerId == "424805" ~ "Maurits Kjærgaard",
                              playerId == "425220" ~ "Daouda Guindo",
                              playerId == "430703" ~ "Paul Wanner",
                              playerId == "432889" ~ "Samson Tijani"))

optadata <- optadata %>%
  select(teamId, playerId, x, y, endX, endY, minute, type.displayName, outcomeType.displayName) %>%
  rename(type = type.displayName) %>%
  rename(outcome = outcomeType.displayName) %>%
  rename(finalX = endX,
         finalY = endY)

plot_passnet(eventData = optadata, dataType = "opta", team_name = "Bayern", theme = "dark")

# StatsBomb

dataframe <- FreeCompetitions() %>%
  filter(competition_id == 11 & season_name == "2018/2019")
df <- FreeMatches(dataframe)
StatsBombData <- StatsBombFreeEvents(MatchesDF = df, Parallel = T)
sbdata <- allclean(StatsBombData)

# Save

ggsave("test.png", bg = "#0d1117", width = 2400, height = 1900, units = "px")