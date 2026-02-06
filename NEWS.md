# ribiosPlot 1.3.0

* Initial CRAN submission
* Package prepared for CRAN submission with comprehensive documentation
* Depends on ribiosUtils (on CRAN)
* Optional Vennerable integration for users who install it separately

# ribiosPlot 1.1.30 (2015-08-04)

* Add `royalredgrayblue` and `royalbluegrayred` color palettes
* `plotPCA`: xlim/ylim parameters auto-adjust for text labels
* Add function `getLims` to get xlim/ylim values from vectors

# ribiosPlot 1.1.29 (2015-03-13)

* `compactPar` more flexible to accept and pass parameters to par

# ribiosPlot 1.1.28 (2015-02-03)

* Fix bug in heat function: return (n)

# ribiosPlot 1.1.26 (2015-01-06)

* Add `compactTrellis()` and `setCompactTrellis()` for compact publication figures
* Add function `p2asterisk` to map p values to asterisk symbols
* Use roxygen2 for documentation

# ribiosPlot 1.1.25 (2014-07-30)

* Add function `fcbrewer` for factor-matching colors
* Improved manual pages
* Rename `sqlayout` to `squareLayout`
* Rename `compPar` to `compactPar`

# ribiosPlot 1.1.24 (2014-07-01)

* `biosHeatmap`: cex main size better guessed

# ribiosPlot 1.1.23 (2014-04-28)

* Add `intRange` and `squareLayout`
* `pdf2png` works on UDIS machine

# ribiosPlot 1.1.22 (2014-04-22)

* Add `histMat` to visualize matrices with histograms
* Add `plotPCA`

# ribiosPlot 1.1.20 (2014-01-28)

* Add `qHist` and `xclipHist` histogram functions

# ribiosPlot 1.1.19 (2014-01-23)

* Add ribiosUtils to dependency list

# ribiosPlot 1.1.18 (2014-01-22)

* Add `pdf2png` to convert PDF to PNG

# ribiosPlot 1.1.17 (2013-08-19)

* Add `cyanblackyellow` and `yellowblackyan` color combinations
* Add `jitter.xyplot`

# ribiosPlot 1.1.16 (2013-07-22)

* `brewer.pal.factor` auto-adjusts color palette size
* PCA plotting functions refactored

# ribiosPlot 1.1.15 (2013-02-22)

* Update boxplot.Rscript with improved features

# ribiosPlot 1.1.14 (2012-10-20)

* Add `idev` and `ipdf` functions for plotting in Rscripts
* Add `blackyellowred` and `blackgoldred` color panels

# ribiosPlot 1.1.13 (2012-09-11)

* exprs-boxplot.Rscript: accepts tab-delimited and gct files

# ribiosPlot 1.1.12 (2012-09-10)

* exprs-boxplot.Rscript works with GCT files

# ribiosPlot 1.1.11 (2012-08-22)

* Bug fix: `boundNorm` works with NAs

# ribiosPlot 1.1.10 (2012-06-07)

* Fix typos in `brewer.pal.factor` and `brewer.pal.factorLevels`

# ribiosPlot 1.1.9 (2012-06-06)

* exprs-boxplot.Rscript: handle user-specified ylim values

# ribiosPlot 1.1.8 (2012-05-23)

* Distance function changed from dist to `robustDist`
* Add documentation for color functions
* Substantially updated `biosHeatmap` documentation

# ribiosPlot 1.1.7 (2012-04-30)

* coverage-plot.Rscript feature file improvements

# ribiosPlot 1.1.6 (2012-04-15)

* Add exprs-boxplot.Rscript for command line boxplots

# ribiosPlot 1.1.5 (2012-04-02)

* coverage-plot.Rscript supports sequence annotation

# ribiosPlot 1.1.4 (2012-03-19)

* Add -featurefile option to coverage-plot.Rscript
* Fix partial matching bug in `biosHeatmap`

# ribiosPlot 1.1.3 (2012-03-09)

* Add coverage-plot.Rscript

# ribiosPlot 1.1.2 (2012-02-14)

* biosHeatmap.Rscript uses Ward as default clustering method

# ribiosPlot 1.1.1 (2011-12-08)

* geneset_xyplot.Rscript improvements

# ribiosPlot 1.1.0 (2011-12-02)

* Add `whiteblueblackheat` to three-color palettes

# ribiosPlot 1.0.9 (2011-11-23)

* Add `whiteblack` and `blackwhite` to two-color palettes

# ribiosPlot 1.0.8 (2011-11-22)

* Add `guessWH` function for auto width/height ratio of heatmaps
* Margins automatically determined based on xlab/ylab

# ribiosPlot 1.0.7 (2011-11-17)

* biosHeatmap.Rscript: lwids and lheis options obsolete

# ribiosPlot 1.0.6 (2011-11-16)

* Data import taken over by ribiosIO::read_exprs_matrix
* Long titles wrapped in `biosHeatmap`

# ribiosPlot 1.0.5 (2011-11-15)

* Add Rscript subdirectory for plotting scripts

# ribiosPlot 1.0.4 (2011-11-10)

* Export `colorpanel` function

# ribiosPlot 1.0.3 (2011-10-10)

* Add zlim option to `biosHeatmap`

# ribiosPlot 1.0.2 (2011-10-05)

* Add `turyeb` color palette

# ribiosPlot 1.0.1 (2011-10-04)

* Margins automatically guessed
* Fine tuning of heatmap key plot parameters

# ribiosPlot 1.0.0 (2011-09-20)

* Initial release: refactor visualization functions from ribios package
