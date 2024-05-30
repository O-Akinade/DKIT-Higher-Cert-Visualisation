library(shiny)
library(ggridges)
library(tidyverse)
library(shinydashboard)
library(recipes)

#Data~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
champions = read.csv("ChampionsClean.csv")
str(champions)

activeteam = read.csv("ActiveTeamClean.csv")
str(activeteam)

tournament = read.csv("TournamentsClean.csv")
str(tournament)

allteams = read.csv("AllTeamsClean.csv")
str(allteams)

#UI~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

table_UI = function(id){
  DT::dataTableOutput(NS(id,"db_table"))
}
table_server = function(id, df, type){
  moduleServer(id, function(input, output, session){
    output$db_table = DT::renderDataTable({
      DT::datatable(
        df %>% 
          filter(Name == type) %>%
          select_if(is.numeric) %>% 
          pivot_longer(everything()) %>% 
          group_by(name) %>% 
          summarise(Min = min(value),
                    Q1 = quantile(value, .25),
                    Median = median(value),
                    Q3 = quantile(value, .75),
                    Max = max(value),
                    sd = sd(value)) %>% 
          ungroup() %>% 
          mutate_if(is.numeric, .funs = ~round(.x, digits = 0)),
        rownames = FALSE
        
  
      )
    })
  })
  
}




ui = dashboardPage(
  dashboardHeader(title = "League Of Legends Dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Champions",
               tabName = "cham_tab",
               icon = icon("dashboard")),
      
      menuItem("Tournaments",
               tabName = "tournaments_tab",
               icon = icon("dashboard")),
      
     menuItem("Active Teams",
               tabName = "active_team_tab",
               icon = icon("dashboard")),
     
      menuItem("All Teams",
               tabName = "all_team_tab",
               icon = icon("dashboard"))
      

    )
  ),
  
  dashboardBody(
    tabItems(
      # champion Tab~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      tabItem(tabName = "cham_tab",
              sidebarPanel(
                HTML("<h3>Input Parameters</h3>"),
                selectInput("champ_selection", 
                            label =  "Tatget Variable",
                            choices = champions$Name,
                            selected = champions$Name["Jinx"]),
                
                selectInput("cham_performance_attriubute",
                            label = "Performance Attriubute",
                            choices = c("Picks","Bans","Presence","Wins",
                                        "Losses","Winrate","KDA"),
                            selected = "Picks")),
              
              fluidRow(box(DT::dataTableOutput("champions_table"))),
              
              
              fluidRow(box(plotOutput("cham_performance_plot"),width = 4)),
              
              fluidRow(box(plotOutput("cham_correlation_plot"),width = 4))),
              
      # tournaments Tab~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~       
      
      tabItem(tabName = "tournaments_tab",
              sidebarPanel(
                HTML("<h3>Input Parameters</h3>"),
                selectInput("tournaments_selection", 
                            label =  "Tatget Variable",
                            choices = tournament$Season,
                            selected = tournament$Season["12"])),
              
              fluidRow(box(DT::dataTableOutput("tourament_table")))),
     
      # active team~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
      tabItem(tabName = "active_team_tab",
              sidebarPanel(
                HTML("<h3>Input Parameters</h3>"),
                selectInput("active_team_region_selection", 
                            label =  "Tatget Variable",
                            choices = activeteam$Region,
                            selected = activeteam$Region["European Team"])),
              
              fluidRow(box(DT::dataTableOutput("acive_teams_table")))),
      
      # all  team~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      tabItem(tabName = "all_team_tab",
              sidebarPanel(
                HTML("<h3>Input Parameters</h3>"),
                selectInput("all_team_selection", 
                            label =  "Tatget Variable",
                            choices = allteams$Name,
                            selected = allteams$Name["100 Thieves"]),
                
                selectInput("all_team_performance_attriubute",
                            label = "Performance Attriubute",
                            choices = c("Games", "Win.rate", "K.D", "Game.duration", 
                                        "Kills...game",  "Deaths...game", "Towers.killed", 
                                        "Towers.lost","Region_ID"),
                            selected = "Games")),
              
              fluidRow(box(DT::dataTableOutput("all_team_table"))),
            
              
              fluidRow(box(plotOutput("all_team_performance_plot"),width = 4)),
              
              fluidRow(box(plotOutput("all_team_correlation_plot"),width = 4)))
    )
  )
) 
  


server = function(input,output) {

  
  # Champions summary table ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
  output$champions_table = DT::renderDataTable({
    
    name_select = input$champ_selection
    
    DT::datatable(
      champions %>% 
        filter(Name == name_select) %>%
        select_if(is.numeric) %>% 
        pivot_longer(everything()) %>% 
        group_by(name) %>% 
        summarise(Min = min(value),
                  Q1 = quantile(value, .25),
                  Median = median(value),
                  Q3 = quantile(value, .75),
                  Max = max(value),
                  sd = sd(value)) %>% 
        ungroup() %>% 
        mutate_if(is.numeric, .funs = ~round(.x, digits = 0)),
      rownames = FALSE
      
      
    )
  })
   
  #champion performance plot~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
    output$cham_performance_plot = renderPlot({
      
      
      attriubute_select = champions[[input$cham_performance_attriubute]]
      
      name_select = input$champ_selection
      
      attriubutepicks = attriubute_select[champions$Name==name_select] 
      
      plot(champions$Season[champions$Name==name_select],attriubutepicks,type = "l",
      
           xlab = "seasons",
           ylab = "Performance Attriubute")})
    
  #champion correlation plot~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    output$cham_correlation_plot = renderPlot({
      
      
      attriubute_select = champions[[input$cham_performance_attriubute]]
      
      name_select = input$champ_selection
      
      attriubutepicks = attriubute_select[champions$Name==name_select] 
      
      plot(champions$Wins[champions$Name==name_select],attriubutepicks,
           
           xlab = "Wins",
           ylab = "Performance Attriubute")})
    
    
    
  # Tournament ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    output$tourament_table = DT::renderDataTable({
      
      tournament_season_selection = input$tournaments_selection
      
      DT::datatable(
        tournament %>% 
          filter(Season == tournament_season_selection) %>%
          select_all  
      )
    })
  # Active Teams ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    output$acive_teams_table = DT::renderDataTable({
      
      active_team_region = input$active_team_region_selection
      
      DT::datatable(
        activeteam %>% 
          filter(Region == active_team_region) %>%
          select_all 
      )
    })
   
  # All Team summary table ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   output$all_team_table = DT::renderDataTable({
     
     teamname_select = input$all_team_selection
     
     DT::datatable(
       allteams %>% 
         filter(Name == teamname_select) %>%
         select_if(is.numeric) %>% 
         pivot_longer(everything()) %>% 
         group_by(name) %>% 
         summarise(Min = min(value),
                   Q1 = quantile(value, .25),
                   Median = median(value),
                   Q3 = quantile(value, .75),
                   Max = max(value),
                   sd = sd(value)) %>% 
         ungroup() %>% 
         mutate_if(is.numeric, .funs = ~round(.x, digits = 0)),
       rownames = FALSE
       
       
     )
   })
   
   #all team performance plot~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
   output$all_team_performance_plot = renderPlot({
     
     attriubute_select = allteams[[input$all_team_performance_attriubute]]
     
     teamname_select = input$all_team_selection
     
     teamattriubutepicks = attriubute_select[allteams$Name==teamname_select] 
     
     plot(allteams$Season[allteams$Name==teamname_select],teamattriubutepicks, type = "l",
          
          xlab = "seasons",
          ylab = "Performance Attriubute")})
   
   #all team correlation plot~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   
   output$all_team_correlation_plot = renderPlot({
     
     attriubute_select = allteams[[input$all_team_performance_attriubute]]
     
     teamname_select = input$all_team_selection
     
     teamattriubutepicks = attriubute_select[allteams$Name==teamname_select] 
     
     plot(allteams$Win.rate[allteams$Name==teamname_select],teamattriubutepicks,
          
          xlab = "Win.rate",
          ylab = "Performance Attriubute")})
   
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
}

shinyApp(ui = ui, server = server)