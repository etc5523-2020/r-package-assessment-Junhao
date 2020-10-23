library(shiny)
launch_app()
test_that("uifunc()", {
  expect_equal(uifunc(),tabPanel("Table",fluid=TRUE,icon=icon("table"),
                                                          selectInput("typ","choose the type of COVID-19 cases",choices = auscov$type),
                                                          DT::dataTableOutput("TData")))
})
