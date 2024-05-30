library(httr)
library(jsonlite)
library(progress)
library(here)

# Champion call urls Functions ------------------------------------------------------

base_url = "https://euw1.api.riotgames.com/"
info_url = "lol/platform/v3/champion-rotations"
full_url = base::paste0(base_url, info_url)
full_url
