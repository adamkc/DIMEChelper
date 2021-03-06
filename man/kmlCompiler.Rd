% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/kmlCompiler.R
\name{kmlCompiler}
\alias{kmlCompiler}
\title{KML Compiler}
\usage{
kmlCompiler(homeDir = getwd(), flightName, modelObj, modelName,
  copyKMLs = TRUE, mergeKMLs = FALSE)
}
\arguments{
\item{homeDir}{Base directory. Should contain "Model Output" folder and
"Model Summary Output" folder}

\item{flightName}{Name of flight to summarize}

\item{modelObj}{A DIMEC model object.}

\item{copyKMLs}{Logical. Should function collect all kml outputs from flight
directory?}

\item{mergeKMLs}{Logical. Should function merge output kmls into one file for
easier sharing?}
}
\value{
copyKMLs. Creates multiple directories at the given export dir and
populates them with all the exported kmls from the given flight directory.
This option is useful if the flight has many positives and you'd rather check
results one search tile at a time.

mergeKMLs. Create a single kml for the flight by merging the exported
kml's at each tile.  This is useful if there are only a couple hundred
positives across the flight and you'd like to view them all at once.
}
\description{
KML Compiler
}
\examples{
\dontrun{

kmlCompiler(flightName = "ca_hayfork_20160529_rgb"
            modelObj = Mod17)
}
}
