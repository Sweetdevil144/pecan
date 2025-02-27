% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/allometry.LPJGUESS.R
\name{allometry}
\alias{allometry}
\title{LPJ-GUESS allometry}
\usage{
allometry(
  lifeform,
  cmass_leaf,
  cmass_sap,
  cmass_heart,
  densindiv,
  age,
  fpc,
  deltafpc,
  sla,
  k_latosa,
  k_rp,
  k_allom1,
  k_allom2,
  k_allom3,
  wooddens,
  crownarea_max,
  HEIGHT_MAX = 150
)
}
\arguments{
\item{lifeform}{An integer code for the lifeform of this individual (cohort): 1 = Tree, 2 = Grass}

\item{cmass_leaf}{The leaf C pool size (kgC/m^2)}

\item{cmass_sap}{The sapwood C pool size (kgC/m^2)}

\item{cmass_heart}{The heartwood C pool size (kgC/m^2)}

\item{densindiv}{The density of individuals in the cohort (indiv/m^2)}

\item{age}{The age of the coort}

\item{fpc}{The folar projective cover}

\item{deltafpc}{The change in foliar projective cover}

\item{sla}{The SLA (specific leaf area) (per PFT parameter)}

\item{k_latosa}{The leaf area to sapwood area ratio (per PFT parameter)}

\item{k_rp, k_allom1, k_allom2, k_allom3, }{Allometry coefficients (per PFT parameters)}

\item{wooddens}{Wood density (kgC/m^2) (per PFT parameter)}

\item{crownarea_max}{Maximum allowed crown area (m^2)  (per PFT parameter)}

\item{HEIGHT_MAX}{Maximum allowed height of an individual.  This is the maximum height that a tree
can have.  This is hard-coded in LPJ-GUESS to 150 m, but for SDA that might be unrealistically big, 
so this argument allows adjustment. 

This function was transcribed from LPJ-GUESS (v4.0) C++ to R for the purpose of nudging the LPJ-GUESS state offline.
The idea is of course to use the output from the analysis step from an SDA routine to provide the nudged values, although that isn't
relevant to the following code.

Since the original C++ code took as its only argument an LPJ-GUESS C++ class of type 'Individual' there was no way (to my knowledge)
of directly compiling using Rcpp (unlike for allocation.cpp/allocation.R. which was easy to compile from the native C++ using 
Rcpp with very few changes).

As noted in the original function header taken from the the C++ code (copied above), this function should be run after its biomass values 
have been updated.  In this case that means after the allocation() function has been applied to an individual.
 
This function can return following error codes:
 1.  "NegligibleLeafMass" - The individual has negligible leaf biomass.
 2.  "MaxHeightExceeded" - The indidual exceeds the maximum allowed height
 3.  "LowWoodDensity" - The individual's *actual* wood density drops below 90% of prescribed value.  This (slighty weird
 and unphysical) requirement is necessary because sometimes LPJ-GUESS can take carbon from the heartwood to
 ensure C-balance.  I think.  Or some other hockery-pockery.

 If all is well the code is simply "OK".}
}
\value{
A named list of updated state variables for the individual/cohort.  The first value in the list is the error code.
}
\description{
The LPJ-GUESS allometry function transcribed into R.
}
\author{
Matthew Forrest
}
\keyword{internal}
