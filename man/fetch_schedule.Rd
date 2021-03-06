% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fetch_schedule.R
\name{fetch_schedule}
\alias{fetch_schedule}
\title{fetch_schedule}
\usage{
fetch_schedule(start_date = "", end_date = "", team_id = "")
}
\arguments{
\item{start_date}{a string written as mm/dd/yyyy that represents the start date of the desired schedule period, default to empty}

\item{end_date}{a string written as mm/dd/yyyy that represents the end date of the desired schedule period, default to empty}

\item{team_id}{a number or string that represent the id of an nhl team, if not feed return schedule for all teams}
}
\value{
data.table that contains all schedule info for the period between the given dates, return the schedule for the current day if no dates are passed as arguments
}
\description{
fetch schedule info between given dates from the nhl api
}
\examples{
fetch_schedule("03/16/2019", "03/18/2019")
fetch_schedule(team_id = 8)
fetch_schedule("01/16/2019", "03/18/2019", 8)
}
