

#' @export
StockChartClass <- R6Class(

  "StockChartClass",
  inherit = BaseClass,

  public = list(
    DataSet = NULL,

    StartDate = NA,
    EndDate = NA,

    CandlestickChart = NA,
    VolumeChart = NA,


    AddCandlesticks = function(){
      Geoms <- list(
        geom_linerange(mapping = aes(ymin = Low, ymax = High), size = 0.25, show.legend = FALSE, na.rm = TRUE),
        geom_linerange(mapping = aes(ymin = BodyMin, ymax = BodyMax, color = BodyColor), size = 1.75, show.legend = FALSE, na.rm = TRUE)
      )
      return(Geoms)
    },

    AddTrades = function(){
      Geoms <- NULL
      Trades <- attr(self$DataSet, "Trades")

      if (!is.null(Trades)){
        Geoms <- list(
          geom_segment(
            data = Trades,
            mapping = aes(
              x = TradeStartDate,
              xend = TradeEndDate,
              y = TradeStartPrice,
              yend = TradeEndPrice
            )
          )
        )
      }

      return(Geoms)
    },

    AddBollingerBands = function(){
      Geoms <- list()

      if ("BB.Mid" %in% names(self$DataSet)){
        Geoms %<>% rlist::list.append(geom_line(mapping = aes(y = BB.Mid), show.legend = FALSE, na.rm = TRUE))
      }

      if ("BB.Upper" %in% names(self$DataSet)){
        Geoms %<>% list.append(geom_ribbon(mapping = aes(ymin = BB.Lower, ymax = BB.Upper), fill = "gray20", alpha = 0.2, linetype = "dotted", color = "black", show.legend = FALSE, na.rm = TRUE))
      }

      return(Geoms)
    },


    ggplotCandlestick = function(){

      PriceRange <- c(
        min(self$DataSet$Low),
        max(self$DataSet$High)
      )

      PriceMargin <- (PriceRange[2] - PriceRange[1]) / 10.0

      ChartRange <- c(
        PriceRange[1] - PriceMargin,
        PriceRange[2] + PriceMargin
      )

      if (!("BodyMin" %in% names(self$DataSet))){
        self$DataSet %<>%
          df_mutate(
            BodyMin = pmin(Open, Close),
            BodyMax = pmax(Open, Close),
            BodyColor = if_else(Close > Open, "green", "red")
          )
      }

      p <- self$DataSet %>%
        ggplot(mapping = aes(x = Date)) +
        self$AddCandlesticks() +
        self$AddTrades() +
        # self$AddBollingerBands()
        # geom_linerange(mapping = aes(ymin = Low, ymax = High), size = 0.25, show.legend = FALSE, na.rm = TRUE) +
        # geom_linerange(mapping = aes(ymin = BodyMin, ymax = BodyMax, color = BodyColor), size = 1.75, show.legend = FALSE, na.rm = TRUE) +



        #geom_candlestick() +

        labs(title = "AAPL Line Chart", y = "Closing Price", x = "") +
        #geom_bbands(ma_fun = SMA, sd = 2, n = 20) +
        coord_x_date(
          xlim = c(
            self$StartDate,
            self$EndDate
          ),
          ylim = ChartRange
        )

      return(p)
    },

    ggplotVolume = function(){
      p <- self$DataSet %>%
        ggplot(aes(x = Date, y = Volume)) +
        geom_bar(stat = "identity", na.rm = FALSE) +
        ggtitle("Volume")

      return(p)
    },

    ggplotPosition = function(){

      p <- NULL

      if ("Position" %in% names(self$DataSet)){
        p <- self$DataSet %>%
          ggplot(aes(x = Date, y = Position)) +
          geom_bar(stat = "identity", na.rm = FALSE) +
          ggtitle("Position")
      }

      return(p)
    },


    ggplotCumlativeProfitLoss = function(){

      p <- NULL

      if ("CumPL" %in% names(self$DataSet)){
        p <- self$DataSet %>%
          ggplot(aes(x = Date, y = CumPL)) +
          geom_line(stat = "identity", na.rm = FALSE) +
          ggtitle("Cumlative Profit Loss")
      }

      return(p)
    },

    ggplotDrawdown = function(){

      p <- NULL

      if ("Drawdown" %in% names(self$DataSet)){
        p <- self$DataSet %>%
          ggplot(aes(x = Date, y = Drawdown)) +
          geom_line(stat = "identity", na.rm = FALSE) +
          ggtitle("Drawdown")
      }

      return(p)
    },

    ggplotMACD = function(){

      p <- NULL

      if ("MACD" %in% names(self$DataSet)) {

        ChartData <- self$DataSet %>%
          df_select(df_one_of("Date"), df_starts_with("MACD"))


        ChartData <- df_filter(ChartData, complete.cases(ChartData))


        ChartRange <- with(
          ChartData,
          c(pmin(min(MACD, na.rm = TRUE), min(MACD.Histogram, na.rm = TRUE)), pmax(max(MACD, na.rm = TRUE), max(MACD.Histogram, na.rm = TRUE)))
        )

        p <- ChartData %>%
          ggplot(mapping = aes(x = Date)) +
          geom_bar(mapping = aes(y = MACD.Histogram), stat = "identity", na.rm = TRUE, color = "grey10") +
          geom_line(mapping = aes(y = MACD), na.rm = TRUE, color = "blue") +
          geom_line(mapping = aes(y = MACD.Signal), na.rm = TRUE, color = "red") +
          coord_x_date(
            xlim = c(
              self$StartDate,
              self$EndDate
            ),
            ylim = ChartRange
          )
      }
      return(p)

    },

    ggplot = function(){

      Plots <- list(
        self$ggplotCandlestick(),
        self$ggplotVolume(),
        self$ggplotMACD(),
        self$ggplotPosition(),
        self$ggplotCumlativeProfitLoss(),
        self$ggplotDrawdown()
      )

      Plots %<>% list_remove_nulls()

      p <- egg::ggarrange(
        plots = Plots,
        ncol = 1,
        heights = c(3, rep(1, length(Plots) - 1))
      )

      return(p)
    },

    initialize = function(dataSet, startDate = NA, endDate = NA, verbose = FALSE) {

      self$DataSet <- dataSet

      if (is.na(startDate)){
        self$StartDate <- min(dataSet$Date)
      } else {
        self$StartDate <- startDate
      }

      if (is.na(endDate)){
        self$EndDate <- max(dataSet$Date)
      } else {
        self$EndDate <- endDate
      }

      super$initialize(verbose = verbose)


    }
  )

)



DataSet <- LoadTestSymbol(amplitude = c(1, 2, 1, 2), slope = c(0.2, -0.5, -.1, -1.5)) %>%
  df_filter(Date > ymd("2017-01-01"))

StockChart <- StockChartClass$new(DataSet)

StockChart$ggplot()