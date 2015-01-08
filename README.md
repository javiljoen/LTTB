LTTB
====

An R implementation of Steinarsson’s
*Largest-Triangle-Three-Buckets* algorithm
for downsampling time series–like data

Based on the original JavaScript code:

https://github.com/sveinn-steinarsson/flot-downsample

and:

Sveinn Steinarsson. 2013.
*Downsampling Time Series for Visual Representation.*
MSc thesis. University of Iceland.

The R package comes with a test data set downloaded from
<http://flot.base.is/>
and converted from JSON to tab-delimited format.
The resulting graphs look pretty similar to me.

Installation
------------

In R:

```{r}
library(devtools)
install_github('javiljoen/LTTB')
```

Usage
-----

```{r}
library(LTTB)
?LTTB
example(LTTB)
```
