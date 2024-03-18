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

cbb <- read.csv("cbb.csv")

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
    titlePanel("Seeding and Finish in NCAA tournamenet"),
    sidebarLayout(
        sidebarPanel(
            sliderInput("W",
                        "Number of Games Won in Regular Season:",
                        min = 2,
                        max = 38,
                        value = c(2,38)),
            pickerInput("POSTSEASON", "Tournament Finish", unique(cbb$POSTSEASON),options=list(`actions-box`=TRUE), multiple=TRUE)
        ),

        mainPanel(
           plotOutput("bubble_plot")
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
}

shinyApp(ui = ui, server = server)
