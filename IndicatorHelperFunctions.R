#
# helper functions in the context of calculating technical indicators
#

# check function arguments for consistency
checkWindow = function(v_values, i_window)
{
  if (i_window > length(v_values)) {
    stop("window length > number of values!")
  }
}

checkWindows = function(i_window_short, i_window_long)
{
  if (i_window_short > i_window_long) {
    stop("short window length > long window length!")
  }
}

checkVectors = function(v_vec1, v_vec2, v_vec3=c(), v_vec4=c())
{
  b_is_error = FALSE
  
  if (length(v_vec1) != length(v_vec2)) {
    b_is_error = TRUE
  } else if (!is.null(v_vec3)) {
    if (length(v_vec1) != length(v_vec3)) {
      b_is_error = TRUE
    }
  } else if (!is.null(v_vec4)) {
    if (length(v_vec1) != length(v_vec4)) {
      b_is_error = TRUE
    }
  }
  
  if (b_is_error) {
    stop("vector size mismatch!")
  }
}

isDataFrame = function(df)
{
  if (!is.data.frame(df)) {
    stop("target is not of type 'data frame'!")
  }
}

dateMatch = function(v_date, df)
{
  isDataFrame(df)
  if (length(v_date) != nrow(df)) {
    stop("number of dates and rows of data frame do not match!")
  }
}

# moving average
movingAverage = function(v_values, i_window)
{
  i_length = length(v_values)
  
  v_ma = rep(0, i_length)
  
  d_cur_ma = v_values[1]
  v_ma[1] = d_cur_ma
  
  for (i in 2 : i_length) {
    if (i <= i_window) {
      d_cur_ma = d_cur_ma * (i - 1)
      d_cur_ma = d_cur_ma + v_values[i]
      d_cur_ma = d_cur_ma / i
    } else {
      d_cur_ma = d_cur_ma - v_values[i-i_window] / i_window
      d_cur_ma = d_cur_ma + v_values[i] / i_window
    }
    v_ma[i] = d_cur_ma
  }
  
  return(v_ma)
}

# running maximum
runningMax = function(v_values, i_window)
{
  i_length = length(v_values)
  
  v_rmax = rep(0, i_length)
  
  d_cur_max = v_values[1]
  v_rmax[1] = d_cur_max
  
  for (i in 2 : i_length) {
    if (i < i_window) {
      d_cur_max = max(d_cur_max, v_values[i]);
    } else {
      d_cur_max = max(v_values[(i-i_window+1):i]);
    }
    v_rmax[i] = d_cur_max;
  }
  
  return(v_rmax);
}

# running minimum
runningMin = function(v_values, i_window)
{
  i_length = length(v_values)
  
  v_rmin = rep(0, i_length)
  
  d_cur_min = v_values[1]
  v_rmin[1] = d_cur_min
  
  for (i in 2 : i_length) {
    if (i < i_window) {
      d_cur_min = min(d_cur_min, v_values[i]);
    } else {
      d_cur_min = min(v_values[(i-i_window+1):i]);
    }
    v_rmin[i] = d_cur_min;
  }
  
  return(v_rmin);
}

# lagged standard deviation
standardDeviation = function(v_values, i_window)
{
  i_length = length(v_values)
  
  v_sdv = rep(0, i_length)
  v_sdv[1] = 0
  
  for (i in 2 : i_length) {
    if (i < i_window) {
      v_sdv[i] = sd(v_values[1:i])
    } else {
      v_sdv[i] = sd(v_values[(i-i_window+1):i])
    }
  }
  
  return(v_sdv)
}