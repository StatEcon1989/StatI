# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

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
