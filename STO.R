#
# STOCHASTIC OSCILLATOR
#

source("Indicators/IndicatorHelperFunctions.R")
source("Indicators/IndicatorPlottingFunctions.R")

# indicator calculation
STO = function(v_high, v_low, v_close, i_window_k=10, i_window_d=3)
{
  checkVectors(v_high, v_low, v_close)
  checkWindow(v_close, i_window_k)
  checkWindow(v_close, i_window_d)
  
  # %K indicator
  v_running_low  = runningMin(v_low, i_window_k)
  v_running_high = runningMax(v_high, i_window_k)
  v_percent_k = 100 * (v_close - v_running_low) / (v_running_high - v_running_low)
    
  # %D indicator
  v_percent_d = movingAverage(v_percent_k, i_window_d)
    
  df_sto = data.frame(v_percent_d, v_high, v_low, v_close)
  colnames(df_sto) = c("STO", "High", "Low", "Close")
    
  return(df_sto)
}

# indicator plotting
plotSTO = function(v_date, df_sto, s_path)
{
  isDataFrame(df_sto)
  dateMatch(v_date, df_sto)
  
  v_colors = c("black", "blue", "purple", "green")
  v_legend = c("STO", "High", "Low", "Close")
  
  plotIndicator2(v_date, df_sto$STO, df_sto[, -1], v_colors, v_legend, s_path)
}