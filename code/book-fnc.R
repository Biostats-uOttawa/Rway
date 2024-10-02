library(tidyverse)

d <- function(u1, u2, sd1, sd2) {
  abs(u1 - u2) / sqrt((sd1^2 + sd2^2) / 2)
}

plot_pwr <- function(
    n, d, sig_level, alternative
    ) {
  typ_num <- ifelse(alternative == "two.sided", 2, 1)
  qc <- qt(1 - sig_level / typ_num, (n - 1) * 2)
  if (typ_num == 2) {
    qcl <- c(-qc, qc)
  } else if (alternative == "one.pos"){
    qcl <- c(qc, qc)
  } else if (alternative == "one.neg"){
    qcl <- c(-qc, -qc)
  } 
  ncp <- d * sqrt(n*2) / 2
  dat <- data.frame(x = seq(-4, 4, length = 400)) %>%
  mutate(
    y0 = dt(x, (n - 1) * 2),
    y1 = dt(x, (n - 1) * 2, ncp = ncp)
  ) 

  if (alternative == "two.sided"){
        dat <- mutate(dat,
        alpha = ifelse(x < qcl[1] | x > qcl[2], y0, 0)
      )
      if (d > 0) {
        dat <- mutate(dat,
          beta = ifelse(x <= qcl[2], y1, 0)
        )
      } else {
        dat <- mutate(dat,
          beta = ifelse(x >= qcl[1], y1, 0)
        )
      }
  } else if (alternative == "one.pos")
{
    dat <- mutate(dat,
       beta = ifelse(x <= qcl[1], y1, 0),
        alpha = ifelse(x > qcl[1], y0, 0)
    )
}  else if (alternative == "one.neg") {
    dat <- mutate(dat,
      beta = ifelse(x >= qcl[1], y1, 0),
      alpha = ifelse(x < qcl[1], y0, 0)
    )
}
  ggplot(dat, aes(x = x)) +
    geom_line(aes(y = y0), color = "red") +
    geom_line(aes(y = y1), color = "blue") +
    geom_vline(xintercept = qcl, color = "green") +
    geom_area(
      aes(x = x, y = beta),
      fill = rgb(red = 0, green = 0.2, blue = 1, alpha = 0.5)
    ) +
    geom_area(
      aes(x = x, y = alpha),
      fill = rgb(red = 1, green = 0, blue = 0.2, alpha = 0.5)
    ) +
    theme_classic() +
    ylab("dt(x)") 
}
