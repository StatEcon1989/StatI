#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#'@import shiny
server <- function(input, output, session) {
  # helper functions
  complete_datenlage <- function(dt) {
    n_row <- nrow(dt)
    dt$H <- cumsum(dt$h)
    n <- dt$H[n_row]
    dt$f <- dt$h / n
    dt$F <- dt$H / n
    dt$Klassenmittelpunkt <- dt$Untergrenze + (dt$Obergrenze - dt$Untergrenze) / 2
    return(dt)
  }

  validata_data <- function(dt) {
    !any(is.na(dt[, c("Untergrenze", "Obergrenze", "h")])) &&
      all(dt$Untergrenze < dt$Obergrenze) &&
      all(dt$Untergrenze[-1] == dt$Obergrenze[-nrow(dt)])
  }

  datenlage_C <- function(dt) {
    n_row <- nrow(dt)
    x <- c(dt$Untergrenze, dt$Obergrenze[n_row])
    y <- c(0, dt$F)
    F_hat <-
      stats::approxfun(
        x = x,
        y = y,
        yleft = 0,
        yright = 1
      )

    F_hat_inv <- function(y) {
      stats::uniroot((function(x)
        F_hat(x) - y),
                     lower = min(dt$Untergrenze),
                     upper = dt$Obergrenze[n_row],
                     tol = 1e-6
      )$root[1]
    }

    return(list(F_hat = F_hat, F_hat_inv = F_hat_inv, x = x))
  }

  dt <- data.frame(Untergrenze = as.numeric(0:5), Obergrenze = as.numeric(1:6), h = c(10, 20, 5, 30, 15, 20))
  x_inv <- seq(from = 0.01, to = 1, by = 0.01)

  values <- reactiveValues(data = complete_datenlage(dt), f_plot = NULL, quantile_plot = NULL, x = NULL)

  observe({
    req(input$datenlage)
    values$data <- as.data.frame(rhandsontable::hot_to_r(input$datenlage))
    isolate({
      # complete table
      values$data <- complete_datenlage(values$data)
      # update Sliders
      updateSliderInput(inputId = "emp.Verteilung_an_stelle", min = min(values$data$Untergrenze), max = max(values$data$Obergrenze)
      )
    })
    output$datenlage <- rhandsontable::renderRHandsontable({
      hot <- rhandsontable::rhandsontable(values$data, readOnly = FALSE)
      hot <- rhandsontable::hot_context_menu(hot, allowColEdit = FALSE)
      hot <- rhandsontable::hot_col(hot, col = c("F", "f", "H", "Klassenmittelpunkt"), readOnly = TRUE)
      hot <- rhandsontable::hot_validate_numeric(hot, cols = c("F", "f", "h", "H"), min = 0)
      hot
    })
  })

  # update functions and plot objects conditional on data
  observeEvent(values$data, {
    if (validata_data(values$data)) {
      functions <- datenlage_C(values$data)
      # function definitions
      values$F_hat <- functions$F_hat
      values$F_hat_inv <- functions$F_hat_inv
      x <- functions$x
      values$x <- c(min(min(x) - 1, floor(0.8 * min(x))), x, ceiling(1.2 * max(x)))
      # plot definitions
      values$f_plot <- ggplot2::ggplot(data.frame(xvals = x, yvals = values$F_hat(x)), ggplot2::aes(xvals, yvals)) +
        ggplot2::geom_line(size = 1.1, color = "#17365c") +
        ggplot2::labs(x = "x", y = "", title = expression(widehat(F)(x))) +
        ggplot2::theme_light() +
        ggplot2::theme(text = ggplot2::element_text(size = 18))
      values$quantile_plot <- ggplot2::ggplot(data.frame(Quantil = c(0, x_inv), xvals = c(min(values$data$Untergrenze), sapply(x_inv, values$F_hat_inv))), ggplot2::aes(Quantil, xvals)) +
        ggplot2::geom_line(size = 1.1, color = "#17365c") +
        ggplot2::labs(x = "q", y = "", title = expression(widehat(F)^-1 * (q))) +
        ggplot2::theme_light() +
        ggplot2::theme(text = ggplot2::element_text(size = 18))
    } else {
      values$f_plot <- NULL
      values$quantile_plot <- NULL
    }
  })

  # update plots, if slider or plots have changed
  observeEvent(c(values$f_plot, input$emp.Verteilung_an_stelle), {
    req(values$f_plot)
    x <- input$emp.Verteilung_an_stelle
    y <- values$F_hat(x)
    # draw plot with
    output$F_hat <- renderPlot(values$f_plot +
                                 ggplot2::geom_segment(ggplot2::aes(x = x, y = -Inf, xend = x, yend = y), color = "darkorange") +
                                 ggplot2::geom_segment(ggplot2::aes(x = -Inf, y = y, xend = x, yend = y), color = "darkorange") +
                                 ggplot2::geom_label(ggplot2::aes(x = (x - abs(min(values$data$Untergrenze))) / 2, y = y, label = round(y, 2))))
  })
  observeEvent(c(values$quantile_plot, input$emp.Quantilfunktion), {
    req(values$quantile_plot)
    x <- input$emp.Quantilfunktion
    y <- values$F_hat_inv(x)
    output$F_hat_inv <- renderPlot(values$quantile_plot +
                                     ggplot2::geom_segment(ggplot2::aes(x = x, y = -Inf, xend = x, yend = y), color = "darkorange") +
                                     ggplot2::geom_segment(ggplot2::aes(x = -Inf, y = y, xend = x, yend = y), color = "darkorange") +
                                     ggplot2::geom_label(ggplot2::aes(x = x / 2, y = y, label = round(y, 2))))
  })

  # the input/output table and its properties
  output$datenlage <- rhandsontable::renderRHandsontable({
    hot <- rhandsontable::rhandsontable(values$data, readOnly = FALSE)
    rhandsontable::hot_context_menu(hot, allowColEdit = FALSE)
    rhandsontable::hot_col(hot, col = c("F", "f", "H", "Klassenmittelpunkt"), readOnly = TRUE)
    hot
  })
  observeEvent(input$reset, {
    values$data <- complete_datenlage(dt)
    values$f_plot <- NULL
    values$quantile_plot <- NULL
    values$x <- NULL
  })
}
