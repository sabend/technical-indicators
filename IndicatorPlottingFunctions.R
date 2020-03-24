#
# helper functions in the context of plotting technical indicators
#

# define constants
COLOR_LONG_POSITION    = rgb(186/255, 255/255, 179/255, alpha=0.5)
COLOR_SHORT_POSITION   = rgb(255/255, 179/255, 186/255, alpha=0.3)
COLOR_NEUTRAL_POSITION = "white"

# plot indicator (trading strategy)
plotIndicator = function(v_date, v_signals, df_data, v_colors, v_legend, s_path)
{
  jpeg(s_path, width=1200, height=700)
  
  v_x = seq(1:nrow(df_data))
  
  # determine x axis date labels
  v_ticks      = c(v_x[1])
  s_prev_date  = format(as.Date(v_date[1]), "%m-%Y")
  v_tick_dates = c(s_prev_date)
  for (i in 2 : length(v_x)) {
    s_cur_date = format(as.Date(v_date[i]), "%m-%Y")
    if (s_cur_date != s_prev_date) {
      v_ticks      = c(v_ticks, v_x[i])
      v_tick_dates = c(v_tick_dates, s_cur_date)
      s_prev_date  = s_cur_date
    }
  }
  
  # determine start and end coordinates for signal boxes
  v_long_start    = c()
  v_long_end      = c()
  v_neutral_start = c()
  v_neutral_end   = c()
  v_short_start   = c()
  v_short_end     = c()
  i_start = 1
  i_end   = 1
  for (i in 2 : length(v_signals)) {
    i_signal = v_signals[i]
    if (i_signal == v_signals[i-1]) {
      i_end = i
    } else {
      if (v_signals[i-1] == 1) {
        v_long_start = c(v_long_start, i_start)
        v_long_end   = c(v_long_end, i_end+1)
      } else if (v_signals[i-1] == -1) {
        v_short_start = c(v_short_start, i_start)
        v_short_end   = c(v_short_end, i_end+1)
      } else if (v_signals[i-1] == 0) {
        v_neutral_start = c(v_neutral_start, i_start)
        v_neutral_end   = c(v_neutral_end, i_end+1)
      }
      i_start = i
      i_end   = i
    }
  }
  if (i_signal == 1) {
    v_long_start = c(v_long_start, i_start)
    v_long_end   = c(v_long_end, i)
  } else if (i_signal == -1) {
    v_short_start = c(v_short_start, i_start)
    v_short_end   = c(v_short_end, i)
  } else if (i_signal == 0) {
    v_neutral_start = c(v_neutral_start, i_start)
    v_neutral_end   = c(v_neutral_end, i)
  }
  
  # create plot
  d_max     = max(df_data)
  d_min     = min(df_data)
  v_y_ticks = pretty(seq(d_min, d_max))
  
  par(xpd=TRUE, mar=par("mar")+c(0, 0, 0, 7))
  
  plot(range(v_x), c(d_min, d_max), type="n", xaxt="n", yaxt="n", ylab="", xlab="", xaxs="i")
  if (length(v_long_start) > 0) {
    rect(xleft=v_long_start, xright=v_long_end, ybottom=par("usr")[3],
         ytop=par("usr")[4], border="transparent", col=COLOR_LONG_POSITION)
  }
  if (length(v_short_start) > 0) {
    rect(xleft=v_short_start, xright=v_short_end, ybottom=par("usr")[3],
         ytop=par("usr")[4], border="transparent", col=COLOR_SHORT_POSITION)
  }
  if (length(v_neutral_start) > 0) {
    rect(xleft=v_neutral_start, xright=v_neutral_end, ybottom=par("usr")[3],
         ytop=par("usr")[4], border="transparent", col=COLOR_NEUTRAL_POSITION)
  }
  legend(nrow(df_data)+3, d_max, legend=v_legend, col=v_colors, lty=rep(1, length(v_colors)), lwd=1.5)
  par(xpd=FALSE)
  axis(1, at=v_ticks, labels=v_tick_dates)
  axis(2, at=v_y_ticks, labels=v_y_ticks)
  abline(h=v_y_ticks, v=v_ticks, col="gray", lty=3)
  for (i in 1 : ncol(df_data)) {
    lines(df_data[, i], type="l", col=v_colors[i])
  }
  box()
  
  dev.off()
}

# plot indicator
plotIndicator2 = function(v_date, v_indicator, df_data, v_colors, v_legend, s_path)
{
  jpeg(s_path, width=1200, height=700)
  
  v_x = seq(1:nrow(df_data))
  
  # determine x axis date labels
  v_ticks      = c(v_x[1])
  s_prev_date  = format(as.Date(v_date[1]), "%m-%Y")
  v_tick_dates = c(s_prev_date)
  for (i in 2 : length(v_x)) {
    s_cur_date = format(as.Date(v_date[i]), "%m-%Y")
    if (s_cur_date != s_prev_date) {
      v_ticks      = c(v_ticks, v_x[i])
      v_tick_dates = c(v_tick_dates, s_cur_date)
      s_prev_date  = s_cur_date
    }
  }
  
  # create plot
  d_max     = max(df_data)
  d_min     = min(df_data)
  v_y_ticks = pretty(seq(d_min, d_max))
  
  par(xpd=TRUE, mar=par("mar")+c(0, 0, 0, 8))
  
  plot(range(v_x), c(d_min, d_max), type="n", xaxt="n", yaxt="n", ylab="", xlab="", xaxs="i")
  legend(nrow(df_data)+8, d_max, legend=v_legend, col=v_colors, lty=rep(1, length(v_colors)),
         pch=c(1, rep(NA, length(v_colors)-1)), lwd=1.5)
  par(xpd=FALSE)
  axis(1, at=v_ticks, labels=v_tick_dates)
  axis(2, at=v_y_ticks, labels=v_y_ticks)
  abline(h=v_y_ticks, v=v_ticks, col="gray", lty=3)
  for (i in 1 : ncol(df_data)) {
    lines(df_data[, i], type="l", col=v_colors[i+1])
  }
  par(new=TRUE)
  plot(v_x, v_indicator, type="o", col=v_colors[1], axes=FALSE, bty="n", xlab="", ylab="", xaxs="i")
  axis(4, at=pretty(range(v_indicator)))
  box()
  
  dev.off()
}

# plot indicator (trading strategy)
plotStock = function(v_date, v_price, s_path)
{
  jpeg(s_path, width=1200, height=700)
  
  v_x = seq(1:length(v_price))
  
  # determine x axis date labels
  v_ticks      = c(v_x[1])
  s_prev_date  = format(as.Date(v_date[1]), "%m-%Y")
  v_tick_dates = c(s_prev_date)
  for (i in 2 : length(v_x)) {
    s_cur_date = format(as.Date(v_date[i]), "%m-%Y")
    if (s_cur_date != s_prev_date) {
      v_ticks      = c(v_ticks, v_x[i])
      v_tick_dates = c(v_tick_dates, s_cur_date)
      s_prev_date  = s_cur_date
    }
  }
  
  # create plot
  d_max     = max(v_price)
  d_min     = min(v_price)
  v_y_ticks = pretty(seq(d_min, d_max))
  
  plot(v_x, v_price, type="l", xaxt="n", yaxt="n", ylab="", xlab="", xaxs="i")
  
  axis(1, at=v_ticks, labels=v_tick_dates)
  axis(2, at=v_y_ticks, labels=v_y_ticks)
  abline(h=v_y_ticks, v=v_ticks, col="gray", lty=3)

  box()
  
  dev.off()
}