library(shiny)
library(shinythemes)
library(dplyr)
library(readr)
library(lubridate)
library(reshape2)
library(ggplot2)


wtaHistory <- read_csv("wta_shiny_timeline_2021-04-21.csv", col_names = T)
wtaRatings <- read_csv("wta_SR_ratings_2021-04-21.csv", col_names = T)
wtaMatches <- read_csv("wta_shiny_matches_2021-04-21.csv", col_names = TRUE)

wtaHistory
wtaRatings$Rating <- round(wtaRatings$Rating, 2)
wtaRatings$Rating

wtaMatches$tourney_date <- lubridate::ymd(wtaMatches$tourney_date)
wtaMatches$Year <- lubridate::year(wtaMatches$tourney_date)

wtaMatches <- wtaMatches %>% 
    dplyr::select(Year, tourney_date, tourney_name, surface, round, best_of, winner_name, loser_name, score) %>%
    dplyr::rename(`Event Date` = tourney_date,
                  `Event Name` = tourney_name,
                  Surface = surface,
                  Round = round,
                  `Best Of` = best_of,
                  Winner = winner_name,
                  Loser = loser_name,
                  `Match Score` = score) 

rndLevels <- c("R128","R64","R32","R16","RR","QF","SF","BR","F")

wtaMatches$Round <- factor(wtaMatches$Round, rndLevels, ordered = T)

wtaMatches <- wtaMatches %>% 
    dplyr::arrange(desc(`Event Date`), desc(Round))

wtaBaseDate <- min(wtaMatches$`Event Date`)
wtaEndDate <- Sys.Date()

wtaPlayerList <- unique(wtaHistory$Player)

# Elo Player Timeline for plotting

wtaEloTimeline <- melt(wtaHistory, id.vars = "Player",variable.name = "Time", 
                       value.name = "Rating")


# Elo Win Probability Function

probP1 <- function(p1_rtng, p2_rtng){
    dij <- p2_rtng - p1_rtng
    return(1 / (1 + 10 ^ (dij/400)))
}

# Build UI


ui <- navbarPage("WTA Head-to-Head",
                 theme = shinytheme("cerulean"),
                 tabPanel(title = "H2H Results",
                          sidebarLayout(
                              sidebarPanel(
                                  dateRangeInput("wtaDates", h4("Date Range Head-to-Head"),
                                                 start = wtaBaseDate, end = wtaEndDate),
                                  h4("Select Players"),
                                  selectInput(inputId = "wtaPlayer1", label = "Player 1", 
                                              choices = wtaPlayerList, selected = "Serena Williams"),
                                  selectInput(inputId = "wtaPlayer2", label = "Player 2", 
                                              choices = wtaPlayerList, selected = "Venus Williams"),
                                  actionButton("h2h", "Show H2H Results")),
                                  mainPanel(
                                  tableOutput("wtaWinLoss"),
                                  DT::dataTableOutput("wtaH2H", width = "100%")
                                  )
                              )
                          ),
                 tabPanel(title = "Elo",
                          h4("Elo Win Probabilities"),
                          tableOutput("wtaPreds"),
                          h4("Elo Timeline"),
                          plotOutput("wtaPlot")
                          )
    
)

server <- function(input, output) {
    
    wlData <- eventReactive(input$h2h, {
        wtaMatches %>% dplyr::filter((Winner == input$wtaPlayer1 & Loser == input$wtaPlayer2) |
                                         (Loser == input$wtaPlayer1 & Winner == input$wtaPlayer2)) %>% 
            dplyr::filter(dplyr::between(`Event Date`, input$wtaDates[1], input$wtaDates[2])) %>% 
            group_by(Winner) %>% 
            summarise(Wins = n()) %>% 
            rename(Player = Winner) %>% 
            arrange(desc(Wins))
    })
    
    output$wtaWinLoss <- renderTable({
        wlData()
    })
    
    h2hData <- eventReactive(input$h2h,{
        wtaMatches %>% dplyr::filter((Winner == input$wtaPlayer1 & Loser == input$wtaPlayer2) |
                                         (Loser == input$wtaPlayer1 & Winner == input$wtaPlayer2)) %>% 
            dplyr::filter(dplyr::between(`Event Date`, input$wtaDates[1], input$wtaDates[2]))
        
    })
    
    output$wtaH2H <- DT::renderDataTable({
        h2hData()
    })
    
    wtaData <- eventReactive(input$h2h,{
        matchup <- tibble(Time = 1,
                          Player1 = input$wtaPlayer1,
                          Player2 = input$wtaPlayer2)
        
        plyr1Elo <- wtaRatings$Rating[wtaRatings$Player == input$wtaPlayer1]
        plyr2Elo <- wtaRatings$Rating[wtaRatings$Player == input$wtaPlayer2]
        plyr1Prob <- probP1(plyr1Elo, plyr2Elo)
        
        matchPred <- tibble(Player = c(input$wtaPlayer1, input$wtaPlayer2),
                            Elo = c(plyr1Elo, plyr2Elo),
                            `Win Probability` = c(plyr1Prob, 1 - plyr1Prob))
    })
    
    output$wtaPreds <- renderTable({
        wtaData()
    })
    
    output$wtaPlot <- renderPlot({
        wtaEloTimeline %>% filter(Player %in% c(input$wtaPlayer1, input$wtaPlayer2)) %>% 
            ggplot(aes(x = Time,
                       y = Rating,
                       col = Player,
                       group = Player)) +
            labs(title = "Elo Timeline", 
                 x = "Time", y = "Rating") +
            scale_x_discrete(labels = NULL) +
            ylim(1500,2100) +
            geom_hline(yintercept = c(1600, 1700, 1800, 1900, 2000, 2100), col = "grey", lty = 2) +
            geom_line(lwd = 1)
    })

    
}

shinyApp(ui = ui, server = server)
