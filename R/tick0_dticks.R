
tick0_dtick <- function(data, x, tick0, dtick) {


  x_values <- pull(data, formula_to_sym(x))
  
  class_x <- class(x_values)
  
  dtick_null <- is.null(dtick)
  tick0_null <- is.null(tick0)
  
  if (!class_x %in% c("numeric", "Date") & any(!dtick_null, !tick0_null)) {
    
    message("dtick and tick0 have no effect for categorical data - setting to NULL")
    
    tick0 = NULL
    dtick = NULL
    
  } else if (class_x == "Date") {
    # for date x axes, set the default to monthly values starting at the minimum
    if (dtick_null) {
    
    dtick <- "M1"
    
    }
    
    if (tick0_null) {
      
      tick0 <- min(x_values)
      
    }
    
  }
  
  ticks <- list(
    tick0 = tick0,
    dtick = dtick
  )
  
  ticks
  

}