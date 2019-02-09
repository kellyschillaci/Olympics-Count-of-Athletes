library(readr)
athlete_events <- read_csv("IST 707/athlete_events.csv")
noc_regions <- read_csv("IST 707/noc_regions.csv")

library("plotly")
library("tidyverse")
library("data.table")
library("gridExtra")
library("knitr")


counts <- athlete_events %>% 
  group_by(Year, Season) %>%
  summarize(
    Athletes = length(unique(ID)),
    Nations = length(unique(NOC)),
    Events = length(unique(Event))
  )

#Athletes by Season
library(ggplot2)
ggplot(counts, aes(x=Year, y=Athletes, group=Season, color=Season)) +
  geom_point(size=2) +
  geom_line() +
  scale_color_manual(values=c("purple","dodgerblue"))

#Nations by Season
ggplot(counts, aes(x=Year, y=Nations, group=Season, color=Season)) +
  geom_point(size=2) +
  geom_line() +
  scale_color_manual(values=c("purple","dodgerblue"))

#Events by Season
ggplot(counts, aes(x=Year, y=Events, group=Season, color=Season)) +
  geom_point(size=2) +
  geom_line() +
  scale_color_manual(values=c("purple","dodgerblue"))
