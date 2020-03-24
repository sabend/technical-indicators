#
# (FOUR) WEEK RULE
#
# long / neutral / short indicator
#

source("Indicators/IndicatorHelperFunctions.R")
source("Indicators/IndicatorPlottingFunctions.R")

# indicator calculation
FWR = function(v_values, i_short_window=10, i_long_window=20, b_neutral=FALSE)
{
  checkWindow(v_values, i_long_window)
  checkWindows(i_short_window, i_long_window)
  
  v_short_min = c()
  v_short_max = c()
  if (b_neutral) {
    v_short_min = runningMin(v_values, i_short_window)
    v_short_max = runningMax(v_values, i_short_window)
  }
    
  v_long_min = runningMin(v_values, i_long_window)
  v_long_max = runningMax(v_values, i_long_window)
  
  i_length = length(v_values)
  v_signals = rep(0, i_length)
  
  if (b_neutral) {
    v_signals[1] = 0
  } else {
    v_signals[1] = 1
  }
  
  for (i in 2 : i_length) {
    d_value = v_values[i]
    
    if (d_value >= v_long_max[i-1]) {
      v_signals[i] = 1
    } else if (d_value <= v_long_min[i-1]) {
      v_signals[i] = -1
    } else if (b_neutral) {
      if (((v_signals[i-1] == 1)  && (d_value <= v_short_min[i-1])) 
       || ((v_signals[i-1] == -1) && (d_value >= v_short_max[i-1]))) {
        v_signals[i] = 0
      } else {
        v_signals[i] = v_signals[i-1]  
      }
    } else {
      v_signals[i] = v_signals[i-1]
    }
    
  }
  
  df_4wr = data.frame(v_signals, v_values, v_long_min, v_long_max)
  colnames(df_4wr) = c("Signal", "Price", "lMin", "lMax")
  
  if (b_neutral) {
    df_4wr$sMin = v_short_min
    df_4wr$sMax = v_short_max
  }
  
  return(df_4wr)
}

# indicator plotting
plotFWR = function(v_date, df_fwr, s_path)
{
  isDataFrame(df_fwr)
  dateMatch(v_date, df_fwr)
  
  v_legend = c("Price", "Long Min", "Long Max")
  v_color  = c("black", "blue", "blue")
  if (ncol(df_fwr) == 6) {
    v_legend = c(v_legend, "Short Min", "Short Max")
    v_color  = c(v_color, "purple", "purple")
  }
  
  plotIndicator(v_date, df_fwr$Signal, df_fwr[, -1], v_color, v_legend, s_path)
}