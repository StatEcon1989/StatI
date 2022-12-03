#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#'@import shiny
ui <- fluidPage(
  titlePanel("Datenlage C"),
  sidebarLayout(
    sidebarPanel = sidebarPanel(width = 4,
                                sliderInput("emp.Verteilung_an_stelle", label = "F(x)",
                                            min = 0, max = 4, step = 0.01, value = 3),
                                sliderInput("emp.Quantilfunktion", label = "F_inv(x)",
                                            min = 0, max = 1, step = 0.01, value = 0.5),
                                rhandsontable::rHandsontableOutput('datenlage'),
                                actionButton("reset", label = "Reset")
    ),
    mainPanel = mainPanel(width = 8,
                          plotOutput("F_hat"),
                          plotOutput("F_hat_inv")
    )
  )
)
