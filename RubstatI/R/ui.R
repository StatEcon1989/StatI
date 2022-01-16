#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
#library(shinydashboard)

fluidPage(
    fluidRow(
        column(8,
               rhandsontable::rHandsontableOutput('datenlage'),
               actionButton("refresh", "Aktualisieren")
        ),
        column(4,
              sliderInput("emp.Verteilung_an_stelle", label = "F(x)",
                          min=0, max=1, step=0.01, value=0),
              sliderInput("emp.Quantilfunktion", label = "F_inv(x)",
                          min=0, max=1, step=0.01, value=0)
        )
    ),
     fluidRow(
         column(6,
                plotOutput("F_hat")
         ),
         column(6,
                plotOutput("F_hat_inv")
         )
     )
)
