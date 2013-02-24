## R for Archaeologists Function Library v0.08 alpha (Chapter 2)

R4ARCHVER <- data.frame(VER=0.16,ALPHA=TRUE,RC=0) #RC=FALSE reserved for release

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
	winsor.x
}

r4arch.winsor.sd <- function(x,trim) {
	winsor.trim <- round(length(x)*trim)
	winsor.var <- var(r4arch.winsor(x,trim))
	winsor.sd <- sqrt((length(x)-1)*winsor.var/((length(x)-(2*winsor.trim))-1))
	winsor.sd
}

r4arch.indexpair <- function(x,trim,pair) {
	usage <- function() {
		cat("pair must be one of four options:\n\tmed-iqr\n\tmean-stddev\n\ttmean-tstddev\n\tall\nthese output respectively the median and IQR,\nthe mean and standard deviation, the trimmed\nmean and trimmed (winsorized) standard deviation\nor all of the above.\n")
	}
	#check for missing values...
	if(missing(pair)) {
		usage()
		pair <- 0
	}
	if(missing(trim)) { trim <- 0 ; cat("Warning:  Trim set to 0\n") }

	#define case subfunctions
	indexpair.mi <- function(x) {
		cat("\nMedian: ",median(x),"\nInterquartile Range: ",r4arch.iqr(x),"\n")
	}
	indexpair.ms <- function(x) {
		cat("\nMean: ",mean(x),"\nStandard Deviation: ",sd(x),"\n")
	}
	indexpair.tt <- function(x,trim) {
		cat("\nWarning: R does not round trim factors.\nTrimmed Mean: ",mean(x,trim=trim),"\nTrimmed Standard Deviation: ",r4arch.winsor.sd(x,trim),"\n")
	}
	indexpair.all <- function(x,trim) {
		indexpair.mi(x)
		indexpair.ms(x)
		indexpair.tt(x,trim)
	}

	#now, define cases
	if(pair=="med-iqr") { indexpair.mi(x) }
	else {
	if(pair=="mean-stddev") { indexpair.ms(x) }
	else {
	if(pair=="tmean-tstddev") { indexpair.tt(x,trim) }
	else {
	if(pair=="all") { indexpair.all(x,trim) }
	else {cat("Error:  Invalid pair designator\n")}
	}}}
}

r4arch.boxplot <- function(...) {
	boxplot(...,range=1.5)
	boxplot(...,range=3,add=1)
	cat("Secondary dashed-line and bar denote median come outliers\nif such exist.  Unaltered open dots are far outliers.\n")
}

r4arch.boxplot.std <- function(...,rem) {
	if(missing(rem)) { cat("rem must be either level or spread\nrem=level removes level from boxplot\nrem=spread removes level and spread\n") }
	remlevel <- function() {
		remdata <- ...-median(...)
	}
	remspread <- function() {}
	if(rem="level") { remlevel }
	else {
		if(rem="spread") { remspread }
	}
	r4arch.boxplot(remdata)
}
