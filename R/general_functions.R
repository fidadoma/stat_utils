#' Computes cohen's d
#' @description
#' Computes cohen's d for two vectors (pooled version)
#'
#' @param y vector of values
#' @param x vector of values
#'
#' @author Filip Dechterenko
#' @export
cohen.d <- function(x, y) {
  x <- na.omit(x)
  y <- na.omit(y)
  n1 <- length(x)
  n2 <- length(y)
  s1 <- sd(x)
  s2 <- sd(y)
  s <- sqrt(((n1 - 1) * s1^2 + (n2 - 1) * s2^2) / (n1 + n2 - 2))
  return((mean(x) - mean(y)) / s)
}

#' Analyzes performance of function
#' @description
#' Analyzes performance of specified function. Usefull for performance issues.
#'
#' @param f function which will be evaluated
#' @param file.name temporary file, where the output will be stored
#'
#' @author Filip Dechterenko
#' @export
analyze.preformance <- function(f, file.name = "tmp.out") {
  Rprof(filename = file.name, line.profiling = TRUE)
  f
  Rprof("")
  s <- summaryRprof(filename = file.name, lines = "show")
  file.remove(file.name)
  return(s)
}

#' Create time measure object
#' @description
#' Create and initialize time measure object. This object can be used for printing remaining time in some simulations
#'
#' @param n number of cases which have to evaluated
#'
#' @author Filip Dechterenko
#' @export
create.time.measure <- function(n) {
  tm <- list()
  class(tm) <- "tm"
  tm$complete <- 0
  tm$tstart <- proc.time()
  tm$n <- n
  tm$elapsed <- NA

  return(tm)
}

#' Update estimation of remaining time.
#' @description
#' Update estimation of remaining time. We should call this method after one item was computed.
#'
#' @param object initialized time measure object
#'
#' @author Filip Dechterenko
#' @export
update.tm <- function(object) {
  object$elapsed <- proc.time() - object$tstart
  object$elapsed <- object$elapsed[3]
  object$complete <- object$complete + 1
  return(object)
}

#' Prints remaining time of computation
#' @description
#' Prints remaining time of computation. First \code{update.tm} should be called
#'
#' @param x initialized time measure object
#'
#' @author Filip Dechterenko
#' @export
print.tm <- function(x) {
  eta <- with(x, elapsed / complete * (n - complete))
  cat(sprintf("   time=%8.3f ETA=%8.3f s (%.1f%%).\n", tm$elapsed, eta, x$complete / x$n * 100))
}

#' Convert cartesian coordinates to polar
#' @description
#' Convert cartesian coordinates to polar
#'
#' @param x coordinate x
#' @param y coordinate y
#' @param deg boolean, if output should be in degrees instead of radians
#'
#' @author Filip Dechterenko
#' @export
cart2pol <- function(x, y, deg = T) {
  r <- sqrt(x^2 + y^2)
  t <- atan(y / x)
  if (deg) {
    t <- t * 180 / pi
  }
  c("r" = r, "t" = t)
}

#' Convert polar coordinates to cartesian
#' @description
#' Convert polar coordinates to cartesian
#'
#' @param size distance from the origin
#' @param angle anlge parameter of the point
#' @param deg boolean, if output should be in degrees instead of radians
#'
#' @author Filip Dechterenko
#' @export
pol2cart <- function(size, angle, deg = T) {
  # convert degrees to radians (dividing by 360/2*pi, or multiplying by pi/180)
  if (deg == T) {
    angle <- angle * pi / 180
  }
  newx <- size * sin(angle) ## X #this is how you convert back to cartesian coordinates
  newy <- size * cos(angle) ## Y
  return(c("x" = newx, "y" = newy)) # output the new x and y coordinates
}


#' Pretty prints values for publication
#' @description
#' Pretty prints values for publication. Values are ounded to specified precision with trailing zeros.
#'
#' @param x vector or matrix with values
#' @param p how many decimal places should print precisely
#'
#' @author Filip Dechterenko
#' @export
prettyprint <- function(x, p = 2) {
  as.numeric(formatC(round(x, p), format = "f", digits = p))
}

#' Changes scale of the variable
#' @description
#' Changes scale of the variable from one range to another
#'
#' @param x vector or matrix with values
#' @param oldmin old minimum
#' @param oldmax old maximum
#' @param newmin new minimum
#' @param newmax new maximum
#'
#' @author Filip Dechterenko
#' @export
changescale <- function(x, oldmin, oldmax, newmin, newmax) {
  return((newmax - newmin) / (oldmax - oldmin) * (x - oldmin) + newmin)
}


#' Tests whether directory exist
#' 
#' this function can also create it, if it is specified
#'
#' @param pth path to directory
#' @param should_exist whether the directory should exist. 
#'
#' @return
#' @export
test_dir <- function(pth,should_exist = F) {
  if(!dir.exists(pth)) {
    if(should_exist) {
      stop(sprintf("Directory %s should exist!"))
    }
    dir.create(pth, recursive = T)
  }
}

#' Download files from OSF using osfr package
#'
#' We need to connect to OSF before using this function
#'
#' @param df data frame obtained using osf_ls_files() from osfr package
#' @param local_data_pth path, where should be the files downloaded  
#' @param should_overwrite boolean, whether we should overwrite files in our local directory
#'
#' @return
#' @export
#'
download_files <- function(df, local_data_pth, should_overwrite = T) {
  # we need to set correct class as the current version of osfr does not works with dplyr properly
  class(df) <- c("osf_tbl_file","osf_tbl", class(df)) 
  df %>% 
    rowwise() %>% 
    do(osf_retrieve_file(.$id) %>% 
         osf_download(path = file.path(local_data_pth, .$name), 
                      overwrite = should_overwrite))
}