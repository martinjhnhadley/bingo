## This file is used to generate the data_chain_restaurant object

library(tidyverse)

data_chain_restaurants <- read_csv("data-raw/restaurant-info.csv")

save(data_chain_restaurants, file = "data/data_chain_restaurants.rdata")
