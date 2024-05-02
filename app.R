#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(readr)
library(ggplot2)
library(dplyr)
library(shinyWidgets)

cbb <- read.csv("https://raw.githubusercontent.com/sarahjennings2/stat436/main/cbb.csv")

cbb<-cbb|>
  drop_na()|>
  mutate(SEED=ifelse(SEED== "N/A",17,SEED),
         SEED=ifelse(is.na(SEED) ,17,SEED),
         POSTSEASON=ifelse(is.na(POSTSEASON),"NO",POSTSEASON),
         POSTSEASON=ifelse(POSTSEASON=="N/A", "NO", POSTSEASON),
         SEED=as.integer(SEED),
         POSTSEASON = case_when(
           POSTSEASON == "Champions" ~ "1st Place",
           POSTSEASON == "2ND" ~"2nd Place",
           POSTSEASON == "F4" ~"Final Four",
           POSTSEASON == "E8" ~ "Elite Eight",
           POSTSEASON == "S16" ~ "Sweet Sixteen",
           POSTSEASON == "R32" ~ "Round of 32",
           POSTSEASON == "R64" ~ "Round of 64",
           POSTSEASON == "R68" ~ "Round of 68",
           POSTSEASON == "NO" ~ "Not in Tournament"
         ))
size_range<- range(cbb$SEED, na.rm=TRUE)

seed_colors <- scale_color_gradient(low="darkorchid4", high="lightpink")

bubble_bball<- function(data){
 ggplot(data,aes(ADJOE,ADJDE, size=SEED, color=SEED,label=TEAM))+
    geom_point(alpha=.8)+
    scale_size_continuous(range=c(8,1), trans="sqrt", guide="legend")+
    seed_colors+
    theme_minimal()+
    labs(title= "Mens College Basketball Offensive and Defensive Efficiency",
         x="Offensive Efficiency",
         y="Defensive Efficiency")+
    guides(size= guide_legend(title="Seed", override.aes=list(color="black")),
           color=guide_legend(title=NULL, override.aes = list(size=3)))
}

ui <- fluidPage(
    titlePanel("Seeding and Finish in NCAA Tournament"),
    h5("This visualizations plot the offensive and defensive efficiencies for each NCAA Basketball Team from 2013 to 2023. The goal of this visualization is to see what is more important when it comes to success in the postseason, is it scoring more points or preventing the other team from scoring, and to see if that is different from success in the regular season. The higher the seed in the tournament is represented by a larger and darker colored bubble. Users can also look at how many games the team won in the regular season and what place they finished in the tournament. There is also a brush element, so if you highlight one or multiple points, the team name and year will appear in the data table below"),
    sidebarLayout(
        sidebarPanel(
            sliderInput("W",
                        "Number of Games Won in Regular Season:",
                        min = 2,
                        max = 38,
                        value = c(2,38)),
            pickerInput("POSTSEASON", "Tournament Finish", unique(cbb$POSTSEASON),options=list(`actions-box`=TRUE), selected="1st Place", multiple=TRUE)
        ),

        mainPanel(
           plotOutput("bubble_plot", brush="plot_brush"),
           verbatimTextOutput("brush_info")
        )
    )
)


server <- function(input, output) {
  filtered_data<-reactive({
    filter_cbb<-cbb|>
      filter(POSTSEASON %in% input$POSTSEASON,
             W >=input$W[1],
             W <=input$W[2])
    return(filter_cbb)
    })
  
  output$bubble_plot <- renderPlot({
    bubble_bball(filtered_data())
    })
  output$brush_info <-renderPrint ({
    brushed <-brushedPoints(filtered_data(), input$plot_brush)
    if(!is.null(brushed)){
      team_year_info <-brushed |>
        select(TEAM, YEAR, SEED, CONF)|>
        unique()
      return(team_year_info)
    }
  })
  
}

shinyApp(ui = ui, server = server)
