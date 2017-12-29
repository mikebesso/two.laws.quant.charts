library(two.laws.big.bang)
library(two.laws.quant.data)
library(two.laws.quant.indicators)
library(two.laws.quant.charts)

library(lubridate)

library(gridSVG)


DataSet <- LoadTestSymbol(amplitude = c(1, 2, 1, 2), slope = c(0.2, -0.5, -.1, -1.5)) %>%
  df_filter(Date > ymd("2017-01-01"))

StockChart <- StockChartClass$new(DataSet)

StockChart$ggplot()

DataSet %<>%
  AddMACD() %>%
  AddBBands()




StockChart <- StockChartClass$new(DataSet)

p <- StockChart$ggplot()


#ggsave("test.svg", p, "svg", width = 6, height = 6, units = "in")

SVGlist <- grid.export(name = "test.svg", htmlWrapper = TRUE)

# install.packages("gdtools")
# install.packages("svglite")

