library(rvest)
library(dplyr)
library(tidyverse)
library(chron)
library(lubridate)

# Champions ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
champions = read.csv("Champions.csv")

# Season change to numeric
champions$Season = gsub("S","",champions$Season)
champions$Season = as.numeric(champions$Season)

# Give Champions to numeric factors
champions$Champion_id = factor(champions$Champion)
champions$Champion_id = as.numeric(champions$Champion_id)

# Avg.BT as numerical
champions$Avg.BT = as.numeric(champions$Avg.BT)

# CSD.15 as numerical
champions$CSD.15 = as.numeric(champions$CSD.15)

# GD.15 as numerical
champions$GD.15 = as.numeric(champions$GD.15)

# XPD.15 as numerical
champions$XPD.15 = as.numeric(champions$XPD.15)


# NA replacement with 0
champions[is.na(champions)] = 0 


#Column reordering

champions = subset(champions, select = c(Season,Champion,Champion_id,Picks,Bans,Presence,Wins,Losses,Winrate,KDA,Avg.BT,
                     GT,CSM,DPM,GPM,CSD.15,GD.15,XPD.15))

# change of column name
colnames(champions)[2] = "Name"


str(champions)

write.csv(champions, "ChampionsClean.csv",row.names = FALSE)

ChampionsClean = read.csv("ChampionsClean.csv")





# Active Team data scraping~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
na_link = "https://lol.fandom.com/wiki/North_American_Teams"
na_page = read_html(na_link)
north_american_active_team = na_page %>% html_nodes("table") %>% .[2] %>% html_table() %>% .[[1]]
NorthAmericaTeam = dplyr::select(north_american_active_team, -2)
NorthAmericaTeam$Region = c("North America")


eu_link = "https://lol.fandom.com/wiki/European_Teams"
eu_page = read_html(eu_link)
europe_active_team = eu_page %>% html_nodes("table") %>% .[2] %>% html_table() %>% .[[1]]
EuropeanTeam = dplyr::select(europe_active_team, -2)
EuropeanTeam$Region = c("European Team")

k_link = "https://lol.fandom.com/wiki/Korean_Teams"
k_page = read_html(k_link)
korea_active_team = k_page %>% html_nodes("table") %>% .[2] %>% html_table() %>% .[[1]]
KoreanTeam = dplyr::select(korea_active_team, -2)
KoreanTeam$Region = c("Korean Team")


c_link = "https://lol.fandom.com/wiki/Chinese_Teams"
c_page = read_html(c_link)
chinese_active_team = c_page %>% html_nodes("table") %>% .[2] %>% html_table() %>% .[[1]]
ChineseTeam = dplyr::select(chinese_active_team, -2)
ChineseTeam$Region = c("Chinese Team")


b_link = "https://lol.fandom.com/wiki/Brazilian_Teams"
b_page = read_html(b_link)
brazil_active_team = b_page %>% html_nodes("table") %>% .[2] %>% html_table() %>% .[[1]]
Brazilian = dplyr::select(brazil_active_team, -2)
Brazilian$Region = c("Brazilian Team")


cis_link = "https://lol.fandom.com/wiki/CIS_Teams"
cis_page = read_html(cis_link)
cis_active_team = cis_page %>% html_nodes("table") %>% .[2] %>% html_table() %>% .[[1]]
CIS = dplyr::select(cis_active_team, -2)
CIS$Region = c("CIS Team")

j_link = "https://lol.fandom.com/wiki/Japanese_Teams"
j_page = read_html(j_link)
j_active_team = j_page %>% html_nodes("table") %>% .[2] %>% html_table() %>% .[[1]]
Japanese = dplyr::select(j_active_team, -2)
Japanese$Region = c("Japanese Team")


la_link = "https://lol.fandom.com/wiki/Latin_American_Teams"
la_page = read_html(la_link)
la_active_team = la_page %>% html_nodes("table") %>% .[2] %>% html_table() %>% .[[1]]
LatinAmerican = dplyr::select(la_active_team, -2)
LatinAmerican$Region = c("Latin American")



oce_link = "https://lol.fandom.com/wiki/Oceanic_Teams"
oce_page = read_html(la_link)
oce_active_team = oce_page %>% html_nodes("table") %>% .[2] %>% html_table() %>% .[[1]]
OceanicTeam = dplyr::select(oce_active_team, -2)
OceanicTeam$Region = c("Oceanic Team")


pcs_link = "https://lol.fandom.com/wiki/PCS_Teams"
pcs_page = read_html(la_link)
pcs_active_team = pcs_page %>% html_nodes("table") %>% .[2] %>% html_table() %>% .[[1]]
PCSTeam = dplyr::select(pcs_active_team, -2)
PCSTeam$Region = c("PCS Team")


t_link = "https://lol.fandom.com/wiki/Turkish_Teams"
t_page = read_html(la_link)
t_active_team = t_page %>% html_nodes("table") %>% .[2] %>% html_table() %>% .[[1]]
TurkishTeam = dplyr::select(t_active_team, -2)
TurkishTeam$Region = c("Turkish Team")


v_link = "https://lol.fandom.com/wiki/Vietnamese_Teams"
v_page = read_html(la_link)
v_active_team = v_page %>% html_nodes("table") %>% .[2] %>% html_table() %>% .[[1]]
VietnameseTeam = dplyr::select(v_active_team, -2)
VietnameseTeam$Region = c("Vietnamese Team")



ActiveTeam = rbind.data.frame(NorthAmericaTeam,EuropeanTeam,KoreanTeam,ChineseTeam,Brazilian,CIS,Japanese,LatinAmerican,OceanicTeam,PCSTeam,TurkishTeam,VietnameseTeam)


write.csv(ActiveTeam, "ActiveTeam.csv",row.names = FALSE)

# Active Team Data Cleaning~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

activeteam = read.csv("ActiveTeam.csv")


#removing unwanted html code
activeteam$Team = gsub("[[:punct:]]","",activeteam$Team)
activeteam$Team = gsub("U2060U2060","",activeteam$Team)


#Giving empty cell a value
activeteam[activeteam == ""] = "No Roster Avalable"


# Give active team to numeric factors
activeteam$Region_id = factor(activeteam$Region)
activeteam$Region_id = as.numeric(activeteam$Region_id)


# change of column name
colnames(activeteam)[2] = "Name"


str(activeteam)

write.csv(activeteam, "ActiveTeamClean.csv",row.names = FALSE)

ActiveTeamClean.csv = read.csv("ActiveTeamClean.csv")


# Tournament Data Cleaning ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
tournament = read.csv("Tournament.csv")

# Season change to numeric
tournament$Season = gsub("S","",tournament$Season)
tournament$Season = as.numeric(tournament$Season)

# NA replacement with N/A
tournament$Region[is.na(tournament$Region)] = "N/A"

# change of column name
colnames(tournament)[3] = "Name"

colnames(tournament)[2] = "Type"


str(tournament)

write.csv(tournament, "TournamentsClean.csv",row.names = FALSE)

TournamentsClean.csv = read.csv("TournamentsClean.csv")

# All Team Data Cleaning ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

allteams = read.csv("Teams.csv")

# Season change to numeric
allteams$Season = gsub("S","",allteams$Season)
allteams$Season = as.numeric(allteams$Season)

# win rate change to numeric
allteams$Win.rate = gsub("%","",allteams$Win.rate)
allteams$Win.rate = as.numeric(allteams$Win.rate)

# NA replacement with N/A
allteams$Region[is.na(allteams$Region)] = "N/A"


#changing multiple column to Numeric
cols.num = c("FB.","FT.","DRAPG","DRA.","HERPG","HER.","DRA.15","TD.15","GD.15","NASHPG","NASH.","DPM","WPM","VWPM","WCPM")
allteams[cols.num] = sapply(allteams[cols.num], as.numeric)

warnings()
#changing Game.duration format

allteams$Game.duration = chron(times = allteams$Game.duration)

allteams$Game.duration = hms(allteams$Game.duration)
allteams$Game.duration = hour(allteams$Game.duration)*60 + minute(allteams$Game.duration)


#NA replacement with 0

allteams[is.na(allteams)] = 0


# assigning region id to regions
allteams$Region_ID = factor(allteams$Region)
allteams$Region_ID = as.numeric(allteams$Region_ID)


str(allteams)

write.csv(allteams, "AllTeamsClean.csv",row.names = FALSE)

AllTeamsClean.csv = read.csv("AllTeamsClean.csv")
