## R for Archaeologists Function Library v0.08 alpha (Chapter 2)

R4ARCHVER <- data.frame(VER=0.12,ALPHA=TRUE,RC=0) #RC=FALSE reserved for release

## Copyright (c) 2013 Evan Sternberg
## Released under the ISC license:
##
## Permission to use, copy, modify, and/or distribute this software for any purpose with or without fee is hereby granted, provided that the above copyright notice and this permission notice appear in all copies.
##
## THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

require("aplpack")	# this lets us make nicer stem/leaf plots,
			# and back-to-back s/l plots.  Requires tcltk

r4arch.stemmode <- function(data,unit) { 
	# This function is for determining the mode of a specific
	# distribution of a batch, not the mode of the batch.
	# It is equivalent to finding the highest peak of the
	# stem and leaf plot.
	mode.slp <- stem.leaf(data,unit,m=1,trim.outliers=0,depths=0,printresult=0)
	mode.LEN <- nchar(mode.slp$leaves)-2
	mode.stem <- as.numeric(mode.slp$stem[which(mode.LEN == max(mode.LEN))])*unit*10
	cat("Mode Stem(s):\n",mode.stem,"'s\n")
}

r4arch.range <- function(...) {
	range(...)[2]-range(...)[1]
}

r4arch.quartiles <- function(x) {
	fivenum(x)[c(2,4)]
}

r4arch.iqr <- function(x) {
	r4arch.quartiles(x)[2]-r4arch.quartiles(x)[1]
}

r4arch.winsor <- function(x,trim) {
	winsor.x <- as.array(x[order(x)])
	winsor.trim <- round(length(winsor.x)*trim)
	winsor.replacewith.i <- c(1+winsor.trim,length(winsor.x)-winsor.trim)
	#winsor.replacewith <- winsor.x[winsor.replacewith.i]
	for (i in 0:winsor.trim) {
		winsor.x[c(1+i,length(winsor.x)-i)] <- winsor.x[winsor.replacewith.i]
	}
	print(winsor.x)
}

r4arch.winsor.sd <- function(x,trim) {
	winsor.trim <- round(length(x)*trim)
	winsor.var <- var(r4arch.winsor(x,trim))
	winsor.sd <- sqrt((length(x)-1)*winsor.var/((length(x)-(2*winsor.trim))-1))
	print(winsor.sd)
}

r4arch.indexpair <- function() {
	#code
}
