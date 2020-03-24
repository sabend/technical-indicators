#
# AVERAGE TRUE RANGE
#
# measurement of trading range
#

source("Indicators/IndicatorHelperFunctions.R")
source("Indicators/IndicatorPlottingFunctions.R")

# indicator calculation
ATR = function(v_high, v_low, v_close, i_window=10)
{
  checkVectors(v_high, v_low, v_close)
  checkWindow(v_close, i_window)
  
  i_length = length(v_high)
  
  m_ranges = matrix(c(v_high - v_low, abs(c(0, v_high[2:i_length] - v_close[1:(i_length-1)])), 
                      abs(c(0, v_low[2:i_length] - v_close[1:(i_length-1)]))), nrow=3, byrow=TRUE)
  
  v_true_range = apply(m_ranges, 2, max)
  
  v_atr = rep(0, i_length)
  v_atr[1] =  mean(v_true_range[1:i_window])
  
  for (i in 2 : i_length) {
    v_atr[i] = (v_atr[i-1] * (i_window-1) + v_true_range[i]) / i_window
  }
  
  df_atr = data.frame(v_atr, v_high, v_low, v_close)
  colnames(df_atr) = c("ATR", "High", "Low", "Close")
  
  return(df_atr)
}

# indicator plotting
plotATR = function(v_date, df_atr, s_path)
{
  isDataFrame(df_atr)
  dateMatch(v_date, df_atr)
  
  v_colors = c("black", "blue", "purple", "green")
  v_legend = c("ATR", "High", "Low", "Close")
  
  plotIndicator2(v_date, df_atr$ATR, df_atr[, -1], v_colors, v_legend, s_path)
}