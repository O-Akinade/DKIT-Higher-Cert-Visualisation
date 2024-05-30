#Data~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
tournaments = read.csv("Tournament.csv")
str(tournament)

teams = read.csv("Teams.csv")
str(teams)
teams$Rcode = factor(teams$Region)
as.numeric(teams$Rcode)

teams$Win.rate
as.numeric(teams$Win.rate)

activeteam = read.csv("ActiveTeam.csv")
str(activeteam)
activeteam$Rcode = factor(activeteam$Region)
activeteam$Rcode = as.numeric(activeteam$Rcode)


champions = read.csv("Champions.csv")
str(champions)
plot(champions , champions$KDA)

champions$Champion_code = factor(champions$Champion)
champions$Champion_code = as.numeric(champions$Champion_code)

arialplot = boxplot((champions$Wins[champions$Champion_code == 1]))








ui = dashboardPage(
  dashboardHeader(title = "League Of Legends Dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Tournaments",
               tabName = "tournaments_tab",
               icon = icon("dashboard")),
      menuItem("Teams",
               tabName = "team_tab",
               icon = icon("dashboard")),
      menuItem("Active Teams",
               tabName = "act_team_tab",
               icon = icon("dashboard")),
      menuItem("Champions",
               tabName = "cham_tab",
               icon = icon("dashboard"))
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "tournaments_tab",
              fluidRow(box(table_UI("tournament_table")))),  
      
      
      tabItem(tabName = "team_tab"),
      
      tabItem(tabName = "act_team_tab",
              box(plotOutput("act_histogram_plot"), width = 8)),
      
      tabItem(tabName = "cham_tab",
              box(plotOutput("cham_correlation_plot"),width = 9),
              box(selectInput("champ_pick_selction","Features:",
                              c("Wins","KDA","Bans","Winrate")), width = 5))
      
      
      
      colnames(champions)
      
      colnames(is.numeric(allteams))
      
      "Games", "Win.rate", "K.D", "Game.duration", "Kills...game",  "Deaths...game", "Towers.killed", "Towers.lost","Region_ID"    
      
      champpicks = champions$Picks[champions$Name=="Jinx"] 
      
      plot(champions$Season[champions$Name=="Jinx"],champpicks)
      
      # colums~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      colnames(champions)
      colnames(champions)
      colnames(champions)
      colnames(champions)
      #nested link attempt ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      
      #link = "https://lol.fandom.com/wiki/North_American_Teams"
      #page = read_html(link)
      
      #region_link = page %>% html_nodes(".titletabs-tab :nth-child(1)") %>% 
      # html_attr("href") %>% paste("https://lol.fandom.com", ., sep = "")
      
      #get_team = function(region_link) {
      #region_page = read_html(region_link)
      #region_team = region_page %>% html_nodes("table") %>% .[2] %>% 
      #html_table() %>% .[[1]]
      # return(team_member)
      #}
      
      #active_team = sapply(region_link, FUN = get_team)
      
      
      
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      #table2_UI = function(id){
      DT::dataTableOutput(NS(id,"db_table2"))
      }
#table2_server = function(id, df, type){
moduleServer(id, function(input, output, session){
  output$db_table2 = DT::renderDataTable({
    DT::datatable(
      df %>% 
        filter(Season == type) %>%
        rownames = TRUE
      
      
    )
  })
})

}

#table3_UI = function(id){
DT::dataTableOutput(NS(id,"db_table3"))
}
#table3_server = function(id, df, type){
moduleServer(id, function(input, output, session){
  output$db_table3 = DT::renderDataTable({
    DT::datatable(
      df %>% 
        filter(Region == type) %>%
        rownames = TRUE
      
      
    )
  })
})

}