#### pacotes ####

install.packages("expirest", dependencies = T)
library(expirest)

#### dados ####

meses <- c(0,1,2,3, 0,1,2,3,0,1,2,3)
teor <- c(99.88,95.59,92.87,89.9, 99.88,95.59,92.87,89.9, 99.88,95.59,92.87,89.9)
lote <- as.factor(c(1,1,1,1,2,2,2,2,3,3,3,3))

dados <- data.frame(meses, teor, lote)

mod <- expirest_osle(
  data=dados,
  response_vbl = "teor",
  time_vbl = "meses",
  batch_vbl = "lote",
  sl = c(90,110),
  sl_sf= c(2,2),
  srch_range = c(0,500),
  ivl_side = "lower"
)
 summary(mod)

 ggres1 <- plot_expirest_osle(
   model = mod,
   y_range = c(80,110)
   )
 