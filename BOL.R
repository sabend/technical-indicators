#
# BOLLINGER BANDS
#
# long / short indicator
#

source("Indicators/IndicatorHelperFunctions.R")
source("Indicators/IndicatorPlottingFunctions.R")

# indicator calculation
BOL = function(v_values, i_window=20, d_leverage=2)
{
  checkWindow(v_values, i_window)
  
  i_length = length(v_values)
  
  v_ma  = movingAverage(v_values, i_window)
  v_sdv = standardDeviation(v_values, i_window)
  
  v_upper_band = v_ma + d_leverage * v_sdv
  v_lower_band = v_ma - d_leverage * v_sdv
  
  v_signals = rep(0, i_length)
  if (v_values[1] >= v_ma[2]) {
    v_signals[1] = 1
  } else {
    v_signals[1] = -1
  }
  
  for (i in 2 : i_length) {
    if (v_values[i] >= v_upper_band[i]) {
      v_signals[i] = 1
    } else if (v_values[i] <= v_lower_band[i]) {
      v_signals[i] = -1
    } else {
      v_signals[i] = v_signals[i-1]
    }
  }
  
  df_bol = data.frame(v_signals, v_values, v_ma, v_upper_band, v_lower_band)
  colnames(df_bol) = c("Signal", "Price", "MA", "uBand", "lBand")
  
  return(df_bol)
}

# indicator plotting
plotBOL = function(v_date, df_bol, s_path)
{
  isDataFrame(df_bol)
  dateMatch(v_date, df_bol)
  
  v_colors = c("black", "blue", "purple", "purple")
  v_legend = c("Price", "MA", "Upper Band", "Lower Band")
  
  plotIndicator(v_date, df_bol$Signal, df_bol[, -1], v_colors, v_legend, s_path)
}