#####################################################################
#####################################################################
#' Get modern-day observational max wind speed uncertainty estimates from Torn & Snyder 2012. 
#'
#' \code{TornSnyder2012_vx_uncert} returns the estimates of intensity-dependent 
#' uncertainty in maximum TC windspeed from the study:
#' Torn, R.D., and C.M. Snyder, 2012: Uncertainty of tropical cyclone best-track 
#' information. Weather and Forecasting, 27, 715-729, DOI: 10.1175/WAF-D-11-00085.1.
#' 
#' @param vmax (Vector or scalar of) max wind speed(s) (default: NA).
#' @param units Character vector giving the units of the input windspeed 
#' (default: "kts". Can also be "m/s" or "mph").
#' @param SScat Alternately, can input the Saffir-Simpson category
#' (-1 for tropical depression; 0 for tropical storm, 1-5 for Cat 1-5 storms.)
#'  
#' @return sig Uncertainty estimate in knots; of same size as input vector.
#'
#' @export
TornSnyder2012_vx_uncert <- function(vmax = NA,units = "kts",SScat = NA){
  
  ts2012sd <- as.matrix(c(7.6, 9.7, 12.7, 9.8, 12.5, 12.1, 12.1))
  
  if(is.na(SScat)){SScat <- ss_category(vmax=vmax)}
  
  return(as.matrix(ts2012sd[SScat+2]))
  
}
#####################################################################
#' Get modern-day observational position uncertainty estimates from Torn & Snyder 2012. 
#'
#' \code{TornSnyder2012_pos_uncert} returns the estimates of intensity-dependent 
#' uncertainty in maximum TC windspeed from the study:
#' Torn, R.D., and C.M. Snyder, 2012: Uncertainty of tropical cyclone best-track 
#' information. Weather and Forecasting, 27, 715-729, DOI: 10.1175/WAF-D-11-00085.1.
#' 
#' @param vmax (Vector or scalar of) max wind speed(s) (default: NA).
#' @param units Character vector giving the units of the input windspeed 
#' (default: "kts". Can also be "m/s" or "mph").
#' @param SScat Alternately, can input the Saffir-Simpson category
#' (-1 for tropical depression; 0 for tropical storm, 1-5 for Cat 1-5 storms.)
#'  
#' @return sig Uncertainty estimate in nautical mi; of same size as input vector.
#'
#' @export

TornSnyder2012_pos_uncert <- function(vmax = NA,units = "kts",SScat = NA){
  
  ts2012sd <- as.matrix(c(43.6, 34.0, 28.8, 22.3, 20.3, 15.3, 15.3))
  
  if(is.na(SScat)){SScat <- ss_category(vmax=vmax)}
  
  return(as.matrix(ts2012sd[SScat+2]))
  
}

#####################################################################
#' Get modern-day central pressure uncertainty estimates from Torn & Snyder 2012. 
#'
#' \code{TornSnyder2012_pos_uncert} returns the estimates of intensity-dependent 
#' uncertainty in maximum TC windspeed from the study:
#' Torn, R.D., and C.M. Snyder, 2012: Uncertainty of tropical cyclone best-track 
#' information. Weather and Forecasting, 27, 715-729, DOI: 10.1175/WAF-D-11-00085.1.
#' 
#' @param pmin (Vector of) central pressure(s) of the storm(s), in hPa. 
#' @param SScat Alternately, can input the Saffir-Simpson category
#' (-1 for tropical depression; 0 for tropical storm, 1-5 for Cat 1-5 storms.)
#' @param vmax Alternatively, (vector of) max wind speed(s) of the TC(s) in knots.
#'  
#' @return sig Uncertainty estimate in hPa; of same size as input vector.
#'
#' @export

TornSnyder2012_pmin_uncert <- function(pmin = NA,SScat = NA,vmax = NA){
  
  ts2012sd <- as.matrix(c(4.900, 6.500, 8.400, 11.400, 10.600, 14.000, 14.000))
  
  if(is.na(SScat)){
    if(is.na(vmax)){SScat <- ss_category(pmin=pmin)}else{
      SScat <- ss_category(vmax=vmax)}
  }
  
  return(as.matrix(ts2012sd[SScat+2]))
  
}


####################################
#' Get Saffir Simpson category given max wind speed or central pressure.
#'
#' \code{ss_category} returns to Saffir-Simpson Category/ies of
#' a tropical cyclone given the (vector of) maximum wind speed(s) or
#' a (vector of) central pressure value(s).
#'
#' @param vx The max wind speed, in knots.
#' @param units Character vector, either "kt" (default),
#' "m/s", or "mph" for vmax input.
#' @param pmin Alternatively, the central pressure of the storm in hPa.
#' Conversion to SS Categories from pressure taken from table at
#' \url{https://www.unc.edu/~rowlett/units/scales/saffir.html}
#'
#' @return cat Category/ies. Values 1-5 denote category 1-5
#' hurricanes. For vmax inputs, 0 denotes tropical storm;
#' -1 denotes tropical depression. For pmin inputs, 0 denotes
#' tropical storm or tropical depression.
#'
#' @export
ss_category <- function(vx = NA, pmin = NA, units = "kt"){
  if(!is.na(vx)){
    # First convert units to knots if necessary:
    if(units == "m/s"){ vx <- 1.94384 * vx;}
    if(units == "mph"){ vx <- 0.868976 * vx;}
    
    # storage
    cat <- matrix(NA,nrow=length(vx),ncol=1)
    
    cat[which(vx >= 137)] <- 5
    cat[which(vx >= 113 & vx < 137)] <- 4
    cat[which(vx >= 96 & vx < 113)] <- 3
    cat[which(vx >= 83 & vx < 96)] <- 2
    cat[which(vx >= 64 & vx < 82)] <- 1
    cat[which(vx >= 34 & vx < 64)] <- 0
    cat[which(vx < 34)] <- -1
  
    }else{
      
      # storage
      cat <- matrix(NA,nrow=length(pmin),ncol=1)
      
      cat[which(pmin < 920)] <- 5
      cat[which(pmin >= 920 & pmin < 945)] <- 4
      cat[which(pmin >= 945 & pmin < 965)] <- 3
      cat[which(pmin >= 965 & pmin < 980)] <- 2
      cat[which(pmin >= 980 & pmin < 1000)] <- 1
      cat[which(pmin >= 1000)] <- 0
      
  }
  return(cat)
}

####################################
#' Convert wind speed units.
#'
#' \code{v_convert} converts windspeeds between knots, m/s, and mph.
#'
#' @param v The wind speed
#' @param from Units from which to convert. "kt", "m/s", or "mph".
#' @param to Units to which to convert. "kt", "m/s", or "mph".
#'
#' @return v Wind speed in converted units.
#'
#' @export
v_convert <- function(v,from,to){
  
  # first convert to knots:
  if(from != "kts"){
    if(from == "m/s"){ v <- 1.94384 * v;}
    if(from == "mph"){ v <- 0.868976 * v;}
  }
  if(to == "m/s"){ v <- v / 1.94384 ;}
  if(to == "mph"){ v <- v / 0.868976 ;}
  
  return(v) 
}






