#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    dt <-
        data.frame(from = 0:5,
                   to = 1:6,
                   h = c(10, 20, 5, 30, 15, 20))
    x_inv = seq(from=0.01, to=1, by=0.01)

    values <- reactiveValues(data = complete_datenlage(dt))
    observe({
        if (!is.null(input$datenlage)) {
            values$data <- as.data.frame(hot_to_r(input$datenlage))

            isolate({
                values$data <- complete_datenlage(values$data)
                updateSliderInput(
                    inputId = "emp.Verteilung_an_stelle",
                    min = min(values$data$from),
                    max = max(values$data$to)
                )

            })
            if (!any(is.na(values$data[, c("from", "to", "h")]))) {
                functions <- datenlage_C(values$data)
                F_hat <- functions$F_hat
                F_hat_inv <- functions$F_hat_inv
                x <- functions$x
                x <-
                    c(min(-1, floor(0.8 * min(x))), x , ceiling(1.2 * max(x)))
                output$F_hat <-
                    renderPlot(plot(
                        x = x,
                        y = F_hat(x),
                        type = "l"
                    ))
                output$F_hat_inv <-
                    renderPlot(plot(
                        x = x_inv,
                        y = sapply(x_inv, F_hat_inv),
                        type = "l", ylab = "x", xlab ="Quantil"
                    ))
            }
            output$datenlage <- renderRHandsontable({
                hot <- rhandsontable::rhandsontable(values$data, readOnly = FALSE)
                rhandsontable::hot_context_menu(hot, allowColEdit = FALSE)
                rhandsontable::hot_col(hot,
                                       col = c("F", "f", "H", "x_mid"),
                                       readOnly = TRUE)
                hot

            })

        }
    })


    output$datenlage <- rhandsontable::renderRHandsontable({
        hot <- rhandsontable::rhandsontable(values$data, readOnly = FALSE)
        rhandsontable::hot_context_menu(hot, allowColEdit = FALSE)
        rhandsontable::hot_col(hot,
                               col = c("F", "f", "H", "x_mid"),
                               readOnly = TRUE)
        hot
    })

})
