LTTB = function(data, n_bins) {
  ## Downsample a time series using Steinarsson's
  ## Largest-Triangle-Three-Buckets algorithm
  ## https://github.com/sveinn-steinarsson/flot-downsample
  ## Sveinn Steinarsson. 2013.
  ## Downsampling Time Series for Visual Representation.
  ## MSc thesis. University of Iceland.
  
  area_of_triangle = function(A, B, C) {
    ## Area of a triangle given duples of vertex coordinates
    return(0.5 * abs((A[1] - C[1]) * (B[2] - A[2]) - (A[1] - B[1]) * (C[2] - A[2])))
  }
  
  if (ncol(data) != 2 | !identical(sort(data[, 1]), data[,1])) {
    stop('Input must be a 2-column matrix sorted by the first column')
  }

  if (class(data) == 'data.frame') {
    data = as.matrix(data)
  }

  N = nrow(data)
  bin_width = (N - 2) / n_bins
  
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
