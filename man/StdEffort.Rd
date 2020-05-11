\name{StdEffort}
\alias{StdEffort}

\title{Standardisation of Fishing Effort (StdEffort)}

\description{This package provides a function named "StdEffort" for standardisation of fishing effort expended by various fishing gears in order to obtain the Catch Per Unit Effort (CPUE) for a particular fish species using the time series of total catch (landings) by each fishing gears, catch (landings) of a particular species (for which the CPUE is required) by each gears and total effort expended by each gears.}
\usage{
StdEffort(sp_catch,tot_catch,effort,meg)
}

\arguments{

\item{sp_catch,tot_catch,effort,meg}{
1) sp_catch = Time series of catch/landings of a particular species (for which the CPUE is required) by each gear

2) tot_catch = Time series of total catch/landings by each fishing gear

3) effort = Time series of total effort expended by each gear

4) meg = Most efficient gear (it takes value either FALSE (for least efficient gear) or TRUE (for most efficient gear))}
}

\details{Marine fisheries governance and management practices are very essential to ensure the sustainability of the marine resources. A widely accepted resource management strategy towards this is to derive sustainable fish harvest levels based on the status of marine fish stock. Various fish stock assessment models that describe the biomass dynamics using time series data on fish catch and fishing effort are generally used for this purpose. In the scenario of complex multi-species marine fishery in which different species are caught by a number of fishing gears and each gear harvests a number of species make it difficult to obtain the fishing effort corresponding to each fish species. Since the capacity of the gears varies, the effort made to catch a resource cannot be considered as the sum of efforts expended by different fishing gears. This necessitates standardisation of fishing effort in unit base. This package provides a function named "StdEffort" for standardisation of fishing effort expended by various gears and obtain Catch Per Unit Effort (CPUE) for a particular fish species using the time series data of each; total catch by each fishing gear, a particular species (for which the CPUE is required) and total effort expended by each gear.}

\value{Standardised fishing effort with a plot of the same along with time series of fish catch/landings. Also gives the unit of standardised effort.}

\references{Eldho Varghese, T. V. Sathianandan, J. Jayasankar, Somy Kuriakose, K.G. Mini and M. Muktha (2020).Bayesian State-space Implementation of Schaefer Production Model for Assessment of Stock Status for Multi-gear Fishery, Journal of the Indian Society of Agricultural Statistics, 74(1), 35–42.

Acknowledgement: Dr. A. Gopalakrishnan, Director, Central Marine Fisheries Research Institute, Kochi, India}

\author{1) Eldho Varghese, Scientist, Fishery Resources Assessment Division, CMFRI, Kochi.

2) Sathianandan T V, Pricipal Scientist and Head,Fishery Resources Assessment Division, CMFRI, Kochi

3) Jayasankar J, Pricipal Scientist,Fishery Resources Assessment Division, CMFRI, Kochi

4) Reshma Gills, Scientist,SEETT Division, CMFRI, Kochi}

\note{The stadardised effort can be obtained either in terms of the least efficient or most efficient gear unit (Say for example, in OBGN hours) for that species.
}

\examples{library (FESta)
data("Example")
StdEffort(sp_catch=Example$sp_catch,tot_catch=Example$tot_catch,effort=Example$effort,meg=FALSE)}

\keyword{Fishing effort}
\keyword{Standardisation}
\keyword{Fish stock assessment}
\keyword{Surplus production model}

