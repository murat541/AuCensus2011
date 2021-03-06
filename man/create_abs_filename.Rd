% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/abs_files.R
\name{create_abs_filename}
\alias{create_abs_filename}
\title{Path to ABS DataPack CSV file}
\usage{
create_abs_filename(profile, table, level)
}
\arguments{
\item{profile}{The short code for the profile. e.g. 'BCP' for Basic Community Profile.}

\item{table}{Table number including and suffix. e.g. 'B01', 'B08A'}

\item{level}{The level of the statistcal area: [AUS, CED, GCCSA, IARE, ILOC, IREG, LGA, POA, RA, SA1, SA2, SA3, SA4, SED, SLA, SOS, SOSR, SSC, STE, SUA, UCL]}
}
\value{
Returns the full path to the CSV file containing the nominated data.
}
\description{
\code{create_abs_filename} creates the path to a given ABS datapack file.
}
\details{
MFC TODO: Describe exactly the file layout.
For now, root_dir = place where DataPack zip files were uncompressed to.
}
\examples{
create_abs_filename('BCP', 'B46', 'AUS')
}

