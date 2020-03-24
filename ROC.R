#
# RATE OF CHANGE
#

source("Indicators/IndicatorHelperFunctions.R")
source("Indicators/IndicatorPlottingFunctions.R")

# indicator calculation
ROC = function(v_values, i_lag)
{
  checkWindow(v_values, i_lag)
  
  i_length = length(v_values)
  
  v_roc = rep(0, i_length)
  v_roc[1] = 0
  
  for (i in 2 : i_length) {
    if (i <= i_lag) {
      v_roc[i] = v_values[i] / v_values[1] - 1
    } else {
      v_roc[i] = v_values[i] / v_values[i-i_lag] - 1
    }
  }
  
  df_roc = data.frame(v_roc, v_values)
  colnames(df_roc) = c("ROC", "Price")
  
  return(df_roc)
}

# indicator plotting
plotROC = function(v_date, df_roc, s_path)
{
  isDataFrame(df_roc)
  dateMatch(v_date, df_roc)
  
  v_colors = c("black", "blue")
  v_legend = c("ROC", "Price")
  
  plotIndicator2(v_date, df_roc$ROC, data.frame(df_roc[, -1]), v_colors, v_legend, s_path)
}