######### Code for the implementation of ######### 
######### the VoH methodology to calculate ######### 
######### prevalence of food insecurity ######### 
######### based on the food insecurity scales data ######### 
# Summary -----------------------------------------------------------------
## Section 0: Install packages
## Section 1: Data
## Section 2: Psychometric analysis
## Section 3: Save outputs
## Section 4: Descriptives
## Section 5: Probabilistic assignment
## Section 6: Prevalence comparison between two countries
## Section 7: automatic equating

# Section 0: Install packages -----------------------------------------
# The R package for the implementation of the VoH methodology
# is called "RM.weights" and it is available on CRAN (R packages' repository),
# starting from R version 3.2. 

install.packages("RM.weights")
library(RM.weights)

# Section 1: Data ------------------------------------------------------------
# Inside the package, four sample datasets are saved, named
# data.FAO_country1, data.FAO_country2, data.FAO_country3,
# data.FAO_country4
# Below you can find some instructions on how to handle these datasets

### Dataset description
?data.FAO_country1

### Attach the datasets
data(data.FAO_country1)
data(data.FAO_country2)
data(data.FAO_country3)
data(data.FAO_country4)

### Saving the FIES data and corresponding weights
XX.country1 = data.FAO_country1[,1:8]
wt.country1 = data.FAO_country1$wt

XX.country2 = data.FAO_country2[,1:8]
wt.country2 = data.FAO_country2$wt

XX.country3 = data.FAO_country3[,1:8]
wt.country3 = data.FAO_country3$wt

XX.country4 = data.FAO_country4[,1:8]
wt.country4 = data.FAO_country4$wt

### Calculate raw scores (number of yes for each individual to the 8 questions)
rv.country2=rowSums(XX.country2)
rv.country4=rowSums(XX.country4)
rv.country3=rowSums(XX.country3)
rv.country1=rowSums(XX.country1)

### Number of items (questions) of the FIES
k = ncol(XX.country1)

# Section 2: Psychometric analysis ----------------------------------------------------------------
rr.country1 = RM.w(XX.country1, wt.country1)
rr.country2 = RM.w(XX.country2, wt.country2)
rr.country3 = RM.w(XX.country3, wt.country3)
rr.country4 = RM.w(XX.country4, wt.country4)

# What's into the model...
str(rr.country4)
# Item severity
rr.country4$b
# Item standard error
rr.country4$se.b
# Person severity
rr.country4$a
# Person standard error
rr.country4$se.a
# Infit
rr.country4$infit
# Outfit
rr.country4$outfit
# Rasch reliability
rr.country4$reliab
# Rasch reliability (flat)
rr.country4$reliab.fl
# Person infit: observed and expected
quantile.seq = c(0,.01,.02,.05,.10,.25,.50,.75,.90,.95,.98,.99,1)
plot(quantile.seq, rr.country4$q.infit, type = "b", xlab = "Quantiles", 
     ylab = "Observed infit", ylim = c(0, 6))
lines(quantile.seq, rr.country4$q.infit.theor, type = "b", col = 2)
# Residual correlation
rr.country4$res.cor

# Section 3: Save outputs ------------------------------------------------------------------
# The function will save the output in the working directory.
# To check the working directory
getwd()
# To change the working directory
path = "C:\\Users\\Desktop"
setwd(path)

rr.country1 = RM.w(XX.country1, wt.country1, country = "Country1", write.file = T)
rr.country2 = RM.w(XX.country2, wt.country2, country = "Country2", write.file = T)
rr.country3 = RM.w(XX.country3, wt.country3, country = "Country3", write.file = T)
rr.country4 = RM.w(XX.country4, wt.country4, country = "Country4", write.file = T)

# Section 4: Descriptives ------------------------------------------------------------------
?tab.weight
## Raw score distribution
# Unweighted
table(rv.country1)
# Weighted
tab.weight(as.factor(rv.country1), wt.country1, XX.country1)$RS.abs.w
# Weighted percentage distribution
tab.weight(as.factor(rv.country1), wt.country1, XX.country1)$RS.rel.w*100

## Distribution by gender
gender1 = data.FAO_country4$gender
# Unweighted
table(gender1)
# Weighted
tab.weight(gender1, wt.country1)$tab.ext.w

## Distribution by urban/rural and gender
# Cross tabs - Unweighted
urb.rur1 = data.FAO_country4$urbanrural
table(gender1, urb.rur1)
# Cross tabs - Weighted
tab.weight(list(gender1, urb.rur1), wt.country1)$tab.ext.w

# Section 5: Probabilistic assignment -------------------------------------
# Pre-defined thresholds on the latent trait
# sthresh contains two possible thresholds on the latent trait
# to be used only within the same country 
?prob.assign
sthresh = c(-0.25, 1.81)
pp.country1 = prob.assign(rr.country1, sthres = sthresh)$sprob
# Probability of being beyond -0.25 on the latent trait in country 1 
pp.country1[1]*100
# Probability of being beyond 1.81 on the latent trait in country 1 
pp.country1[2]*100

#  Section 6: Prevalence comparison between two countries ----------------------------
## NOTE: Corresponding calculation can be found in the excel file
## Equating.xlsx
## Item severities for country 1 and 2
b1 = rr.country1$b
b2 = rr.country2$b
b1.std = b1/sd(b1)
b2.std = b2/sd(b2)

## Tolerance (maximum difference to consider items to be common)
tol = 0.5
## Defining common items based on item severity difference
diff.mat = abs(b1.std - b2.std)
comm.mat = rep(FALSE, length(diff.mat))
comm.mat[diff.mat < tol] = TRUE
names(comm.mat) = colnames(XX.country1)
comm.mat
# FALSE=unique, TRUE=common items

## Defining a metric based on mean and standard deviation of common items
## in both countries
mean.comm = c(mean(b1.std[comm.mat]), mean(b2.std[comm.mat]))
sd.comm = c(sd(b1.std[comm.mat]), sd(b2.std[comm.mat]))
# Cells F14 and G14 in Excel
mean.comm
# Cells F15 and G15 in Excel
sd.comm

## New standardized item severities
b.1.std.new = (b1.std * sd.comm[1]) + mean.comm[1]
b.2.std.new = (b2.std * sd.comm[2]) + mean.comm[2]
# Cells M3:M10 and N3:N10 in Excel
cbind(b.1.std.new, b.2.std.new)
# Graph
plot(b.1.std.new, b.2.std.new, pch = 5, col = "blue",xlab = "Country1", 
     ylab = "Country2", xlim = c(-3,3),ylim=c(-3,3))
abline(c(0,1))
text(b.1.std.new, b.2.std.new, colnames(XX.country1), cex = 0.6, pos=2)
points(b.1.std.new[!comm.mat], b.2.std.new[!comm.mat], col = 2, pch = 5)

## Calculate comparable prevalences
## Report thresholds to the metric of common items
int1=mean.comm[1]
slop1=sd.comm[1]/sd(b1)
int2=mean.comm[2]
slop2=sd.comm[2]/sd(b2)
# Cells B18-C18 in Excel
c(int1, int2)
# Cells B19-C19 in Excel
c(slop1, slop2)

sthresh = c(-0.25, 1.81)
sthesh.new1 = (sthresh - int1)/slop1
sthesh.new2 = (sthresh - int2)/slop2
## Prevalence calculated on equated thresholds
pp.country.new1 = prob.assign(rr.country1, sthres = sthesh.new1)$sprob
pp.country.new2 = prob.assign(rr.country2, sthres = sthesh.new2)$sprob
# Comparable prevalence of Moderate or severe and Severe FI in country1
pp.country.new1
# Comparable prevalence of Moderate or severe and Severe FI in country2
pp.country.new2

# Section 7: automatic equating to 2014-2015 global standard
# VoH 2014-2015 global standard
b.tot=c(-1.2590036, -0.8991436, -1.0876362,  0.4163556, -0.2506451,  0.4466926,  0.8065710, 1.8268093)
# Equating of country 1 to the global standard
ee=equating.fun(rr.country1, st=b.tot, tol=0.5)
# Equated prevalence rates
ee$prevs*100
# Correlation between common items
ee$cor.comm.items
# Producing a plot of the item severities
ee=equating.fun(rr.country1, st=b.tot, tol=0.5, plot=T)
# The plot will be saved as a pdf called "Equating_plot.pdf" file in the working directory


