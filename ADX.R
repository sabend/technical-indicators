#
# AVERAGE DIRECTIONAL MOVEMENT INDEX
#

source("Indicators/IndicatorHelperFunctions.R")
source("Indicators/IndicatorPlottingFunctions.R")

source("Indicators/ATR.R")

# indicator calculation
ADX = function(v_high, v_low, v_close, i_window=10)
{
  checkVectors(v_high, v_low, v_close)
  checkWindow(v_close, i_window)
  
  i_length = length(v_close)
  
  # directional movement
  v_high_diff = c(0, (v_high[2:i_length] - v_high[1:(i_length-1)]))
  v_low_diff  = c(0, (v_low[1:(i_length-1)] - v_low[2:i_length]))
  v_mDM = rep(0, i_length)
  v_pDM = rep(0, i_length)
  v_mDM[(v_low_diff > v_high_diff)] = pmax(0, v_low_diff[(v_low_diff > v_high_diff)])
  v_pDM[(v_high_diff > v_low_diff)] = pmax(0, v_high_diff[(v_high_diff > v_low_diff)])

  # directional index
  v_mADM = movingAverage(v_mDM, i_window)
  v_pADM = movingAverage(v_pDM, i_window)
  v_atr = ATR(v_high, v_low, v_close, i_window)$ATR
  v_mDI = 100 * v_mADM / v_atr
  v_pDI = 100 * v_pADM / v_atr

  # ADX
  v_dx = abs((v_pDI - v_mDI) / (v_pDI + v_mDI))
  v_dx[is.na(v_dx)] = 0
  v_adx = 100 * movingAverage(v_dx, i_window)
  
  df_adx = data.frame(v_adx, v_mDI, v_pDI, v_close)
  colnames(df_adx) = c("ADX", "mDI", "pDI", "Close")
  
  return(df_adx)
}

# indicator plotting
plotADX = function(v_date, df_adx, s_path)
{
  isDataFrame(df_adx)
  dateMatch(v_date, df_adx)
  
  v_colors = c("black", "blue")
  v_legend = c("ADX", "Close")
  
  plotIndicator2(v_date, df_adx$ADX, as.data.frame(df_adx[, 4]), v_colors, v_legend, s_path)
}