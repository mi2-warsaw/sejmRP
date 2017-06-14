library(dplyr)
library(lubridate)

read.csv("http://www.parlgov.org/static/stable/2015/view_party.csv") -> partie
read.csv("http://www.parlgov.org/static/stable/2015/view_election.csv") -> wybory

wybory %>%
  filter(country_id == 74 & election_type == "ep") %>%
  count(rok = year(election_date), partia = party_name_short) %>%
  count(rok)

