#
# CROSSING MOVING AVERAGES
#
# long / short only indicator
#

source("Indicators/IndicatorHelperFunctions.R")
source("Indicators/IndicatorPlottingFunctions.R")

# indicator calculation
XMA = function(v_values, i_short_window=10, i_long_window=20)
{
  checkWindow(v_values, i_long_window)
  checkWindows(i_short_window, i_long_window)
  
  v_short_ma = movingAverage(v_values, i_short_window)
  v_long_ma  = movingAverage(v_values, i_long_window)
  
  i_length = length(v_values)
  v_signals = rep(0, i_length)
  
  for (i in 1:i_length) {
    if (v_short_ma[i] < v_long_ma[i]) {
      v_signals[i] = -1
    } else {
      v_signals[i] = +1 
    }
  }
  
  df_xma = data.frame(v_signals, v_values, v_short_ma, v_long_ma)
  colnames(df_xma) = c("Signal", "Price", "sMA", "lMA")
  
  return(df_xma)
}

# indicator plotting
plotXMA = function(v_date, df_xma, s_path)
{
  isDataFrame(df_xma)
  dateMatch(v_date, df_xma)

  plotIndicator(v_date, df_xma$Signal, df_xma[, -1], c("black", "blue", "purple"), c("Price", "Short MA", "Long MA"), s_path)
}