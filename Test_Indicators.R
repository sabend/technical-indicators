#
# calculate and plot multiple technical indicators for sample data set
#

setwd("C:/Local/Programming/R/Trading")
rm(list=ls())

source("Indicators/TechnicalIndicators.R")

#################################################################################
# parameters
da_start        = as.Date("2017-01-01")
da_cur_date     = Sys.Date()
s_ticker        = "EckertZiegler"
s_output_dir    = "Output"

#################################################################################
# load data
df_data = read.csv(file=paste("Data/", s_ticker, ".csv", sep=""), header=TRUE, sep=",")
df_data = df_data[as.Date(df_data$Date) >= da_start, ]

head(df_data)

#################################################################################
# prepare input data
v_date  = df_data$Date
v_open  = df_data$Open
v_high  = df_data$High
v_low   = df_data$Low
v_close = df_data$Close

#################################################################################
# plot close
plotStock(v_date, v_close, paste(s_output_dir, "/", s_ticker, da_cur_date, ".jpg", sep=""))

#################################################################################
# hyper parameters
b_neutral           = FALSE
i_xma_window_short  = 20
i_xma_window_long   = 40
i_week_window_short = 10
i_week_window_long  = 20
i_bol_window        = 20
i_bol_leverage      = 2
i_atr_window        = 10
i_roc_lag           = 5
i_sto_window_k      = 10
i_sto_window_d      = 3
i_adx_window        = 20

#################################################################################
# evaluate indicators

# XMA
df_xma = XMA(v_close, i_xma_window_short, i_xma_window_long)
plotXMA(v_date, df_xma, paste(s_output_dir, "/XMA_", da_cur_date, ".jpg", sep=""))

# 4WR
df_4wr = FWR(v_close, i_week_window_short, i_week_window_long, b_neutral)
plotFWR(v_date, df_4wr, paste(s_output_dir, "/4WR_", da_cur_date, ".jpg", sep=""))

# BOL
df_bol = BOL(v_close, i_bol_window, i_bol_leverage)
plotBOL(v_date, df_bol, paste(s_output_dir, "/BOL_", da_cur_date, ".jpg", sep=""))

# ATR
df_atr = ATR(v_high, v_low, v_close, i_atr_window)
plotATR(v_date, df_atr, paste(s_output_dir, "/ATR_", da_cur_date, ".jpg", sep=""))

# STO
df_sto = STO(v_high, v_low, v_close, i_sto_window_k, i_sto_window_d)
plotSTO(v_date, df_sto, paste(s_output_dir, "/STO_", da_cur_date, ".jpg", sep=""))

# ROC
df_roc = ROC(v_close, i_roc_lag)
plotROC(v_date, df_roc, paste(s_output_dir, "/ROC_", da_cur_date, ".jpg", sep=""))

# ADX
df_adx = ADX(v_high, v_low, v_close, i_adx_window)
plotADX(v_date, df_adx, paste(s_output_dir, "/ADX_", da_cur_date, ".jpg", sep=""))
