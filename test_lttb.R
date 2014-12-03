source('lttb.R')

datafile = 'demo_data.tab'
data = as.matrix(read.delim(datafile, as.is=TRUE))

downsampled_data = LTTB(data, n_bins=198)

library(ggplot2)
pl = qplot(X, Y, data=as.data.frame(data), geom='line')
pl = pl + geom_line(data=as.data.frame(downsampled_data), colour=I('blue'))
plot(pl)
