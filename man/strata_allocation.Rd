% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/function_sampling_stratum_allocation.R
\name{strata_allocation}
\alias{strata_allocation}
\title{Porportional and disproportional allocations of sample sizes to explicit strata}
\usage{
strata_allocation(df, n, stratVar, enrVar, varVec)
}
\arguments{
\item{df}{Data frame containing primary sampling units (schools) with stratum and size (enrollment) variables}

\item{n}{Numeric representing the total number of elements to sample (students)}

\item{stratVar}{Character string containing name of stratification variable}

\item{enrVar}{Character string containing name of cluster/unit size variable (enrollment number)}

\item{varVec}{Numeric vector containing estimated variance in strata}
}
\value{
List with 3 vectors: proportional allocations, neyman allocations and the corresponding ratios of both methods
}
\description{
Porportional and disproportional allocations of sample sizes to explicit strata
}
