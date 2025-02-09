
library(shiny)
library(tidyverse)






#' Title
#'
#' @return
#'
#' @examples
#' @export
launch_app <- function(){shinyApp(ui = ui, server = server)}


#' Title
#'
#' @return
#'
#' @examples
#' @export
uifunc <- function(){tabPanel("Table",fluid=TRUE,icon=icon("table"),
                              selectInput("typ","choose the type of COVID-19 cases",choices = auscov$type),
                              DT::dataTableOutput("TData"))}

#' Title
#'
#' @param x
#'
#' @return
#'
#' @examples
#' @export
selectthem<-function(x){selectInput(x, "choose one to inspect the daily confirmed cases in each state",
            choices = c("ACT", "NSW", "NT", "QLD", "SA", "TAS", "VIC", "WA"))}

#' Title
#'
#' @return
#'
#' @examples
#' @export
servfunc <- function(){data<-coronavirus%>%filter(country=="Australia")%>%mutate(
  state = case_when(
    province=="New South Wales" ~ "NSW",
    province=="Victoria" ~ "VIC",
    province=="Queensland" ~ "QLD",
    province=="South Australia" ~ "SA",
    province=="Western Australia" ~ "WA",
    province=="Tasmania" ~ "TAS",
    province=="Northern Territory" ~ "NT",
    province=="Australian Capital Territory" ~ "ACT",
    TRUE ~ "OT"
  )
)
auscov<-filter(data,country=="Australia")%>%
  group_by(province,type)%>%
  summarise(cases=sum(cases))%>%
  mutate(state = case_when(
    province=="New South Wales" ~ "NSW",
    province=="Victoria" ~ "VIC",
    province=="Queensland" ~ "QLD",
    province=="South Australia" ~ "SA",
    province=="Western Australia" ~ "WA",
    province=="Tasmania" ~ "TAS",
    province=="Northern Territory" ~ "NT",
    province=="Australian Capital Territory" ~ "ACT",
    TRUE ~ "OT"
  ))%>%relocate(state,province,type,cases)
}


