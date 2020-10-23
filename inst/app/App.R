
library(coronavirus)
library(plotly)
library(tidyverse)
library(shiny)
library(shinythemes)
library(DT)
library(mypk)

ui <- fluidPage(theme = shinytheme("cerulean"),
                titlePanel("Confirmed Cases among States in Australia (from 2020-1-22 ~ 2020-8-31)"),
                br(),
                navbarPage("COVID-19 in Australia",
                           tabPanel("Graphs",fluid=TRUE,icon=icon("chart-bar" ),
                                    sidebarLayout(sidebarPanel(
                                      selectthem("state"),

                                      helpText("this bar chart shows the total confirmed cases in each state, click on each bar to inspect the daily confirmed cases in each state. ( maximize the window to get better visualization)"),
                                      plotlyOutput("col")),
                                      mainPanel(
                                        plotlyOutput("line"),
                                      ))
                           ),
                           tabPanel("Table",fluid=TRUE,icon=icon("table"),
                                    selectInput("typ","choose the type of COVID-19 cases",choices = auscov$type),
                                    DT::dataTableOutput("TData")),
                           tabPanel("About", fluid= TRUE, icon=icon("info"),
                                    h4(p("About the Project")),
                                    h5(p("This project is intended to give users an overview of COVID-19 in Australia during 2020-1-22 ~ 2020-8-31, and contains information about daily cases in each state")),
                                    br(),
                                    h4(p("About the Author")),
                                    h5(p("Junhao is a master student in Monash University")),

                           )
                ))


# Define server logic required
server <- function(input, output, session) {servfunc
  observeEvent(event_data("plotly_click"), {
    click_df <- event_data("plotly_click")
    statename <- filter(auscov, cases==click_df$x) %>%
      pull(state)
    updateSelectInput(session, "state", selected = statename)$state
  })

  output$col <- renderPlotly({aus_plot<-auscov%>%filter(type=="confirmed")%>%
    ggplot(aes(x=province,y=cases))+
    geom_col()+
    coord_flip()
  ggplotly(aus_plot)
  })


  output$line <- renderPlotly(
    {AUSCOV<-filter(data,type=="confirmed",state==input$state)%>%
      ggplot(aes(x=date,y=cases))+
      geom_col()+
      geom_smooth(span=0.1)
    ggplotly(AUSCOV,source="B")}
  )

  output$TData <- DT::renderDataTable({
    subset(auscov)
  })




}

# Run the application
shinyApp(ui = ui, server = server)
