#' Downsample a time series using LTTB
#' 
#' Downsample a time series using Steinarsson's
#' \emph{Largest-Triangle-Three-Buckets} algorithm
#' 
#' For a description of the algorithm, see
#' \url{https://github.com/sveinn-steinarsson/flot-downsample}
#' and
#' Sveinn Steinarsson. 2013.
#' \emph{Downsampling Time Series for Visual Representation.}
#' MSc thesis. University of Iceland.
#' 
#' @param data Numeric matrix with 2 columns, sorted by the first column
#' @param n_bins Number of bins used for downsampling
#' @return matrix with the same columns as \code{data} but at most
#'   \code{n_bins + 2} rows (fewer if \code{n_bins + 2 > nrow(data)})
#' @examples
#' data(timeseries)
#' downsampled_timeseries = LTTB(timeseries, n_bins=198)
#' with(as.data.frame(downsampled_timeseries),
#'      plot(X, Y, type='l'))
#' 
#' \dontrun{
#' library(ggplot2)
#' qplot(X, Y, data=as.data.frame(timeseries), geom='line') +
#'   geom_line(data=as.data.frame(downsampled_timeseries), colour=I('blue'))
#' }
#' @export
LTTB = function(data, n_bins) {
  
  area_of_triangle = function(A, B, C) {
    #' Area of a triangle from duples of vertex coordinates
    #' @param A,B,C numeric vectors of length 2
    #' @return numeric vector of length 1
    return(0.5 * abs((A[1] - C[1]) * (B[2] - A[2]) - (A[1] - B[1]) * (C[2] - A[2])))
  }
  
  if (ncol(data) != 2 | !identical(sort(data[, 1]), data[,1])) {
    stop('Input must be a 2-column matrix sorted by the first column')
  }

  if (class(data) == 'data.frame') {
    data = as.matrix(data)
  }

  N = nrow(data)
  bin_width = floor((N - 2) / n_bins)
  
  if (N <= n_bins + 2) {
    return(data)
  }

  ## set up output
  out = matrix(NA, nrow=(n_bins + 2), ncol=ncol(data))
  colnames(out) = colnames(data)
  out[1, ] = data[1, ]
  out[nrow(out), ] = data[nrow(data), ]
  
  ## In each bin (skipping the first and last data points)...
  for (i in 1 + 1:n_bins) {
    ## A = the saved point in the previous bin
    ## or data[1, ] if this is the first bin
    A = out[i - 1, ]
    if (i < n_bins) {
      ## C = the average of the points in the next bin
      C = apply(data[floor(i * bin_width + 1:bin_width), ], 2, mean)
      ## Bs = set of points in this bin
      Bs = data[floor(((i - 1) * bin_width + 1):(i * bin_width)), , drop=FALSE]
    } else {
      ## if this is the last bin:
      ## C = the last point in the input matrix
      C = out[nrow(out), ]
      ## the set of points in this bin might be less than bin_width,
      ## hence the bin explicitly stretches only to data[N-1, ]
      Bs = data[floor(((i - 1) * bin_width + 1):(N - 1)), , drop=FALSE]
    }
    ## save the point in this bin that makes the largest triangle
    ## with the saved point in the previous bin
    ## and the average of the points in the next bin
    triangle_areas = apply(Bs, 1, area_of_triangle, A=A, C=C)
    out[i, ] = Bs[which.max(triangle_areas), ]
  }
  
  return(out)
}

#' Time series data set for testing
#' 
#' This is one of the data sets used to illustrate the original
#' implementation of LTTB. It was downloaded from
#' \url{http://flot.base.is/},
#' converted from JSON to tab-delimited format,
#' and read into R with the command
#' \code{timeseries = as.matrix(read.delim(datafile, as.is=TRUE))}.
#' 
#' @format Matrix of two columns (X and Y) and 5000 rows
#' @source \url{http://flot.base.is/}
'timeseries'
