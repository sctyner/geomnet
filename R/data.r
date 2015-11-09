#' Bike sharing use in the DC Metro Area 
#' 
#' A list of two datasets, vertices and edges, containing data on bike trips in the Washington DC metro area (see \url{https://secure.capitalbikeshare.com/}).
#' The variables are as follows:
#'
#' @format A list of two data frames:
#' \itemize{
#' \item the edges data set consists of six variables of length 53:
#' \itemize{
#'   \item Start.station: Station where bike trip starts
#'   \item End.station: Station where bike trip ends
#'   \item n: Number of trips between the two stations
#'   \item minlength: ## Duration of shortest trip between the two stations (units?)
#'   \item min5perc: ## Fifth percentile of trip durations (units?)
#'   \item village: I'm not sure ###
#' }
#' \item the vertices data set consists of 16 variables with information on 21 stations:
#' \itemize{
#'   \item id: Station ID number
#'   \item name: Station name
#'   \item terminalName: ??? ###
#'   \item lastCommWithServer: ??? ###  
#'   \item lat: Latitude of station location
#'   \item long: Longitude of station location
#'   \item installed: ??? ###
#'   \item locked: ??? ###
#'   \item installDate: ??? ###
#'   \item removalDate: ??? ###
#'   \item temporary: ??? ###
#'   \item public: ??? ###
#'   \item nbBikes: Number of bikes at the station
#'   \item nbEmptyDocks: Number of empty docks at the station
#'   \item latestUpdateTime: ??? ###
#'   \item village: ??? ###
#' }
#' }
"bikes"

#' Network of blood types
#'
#' A list of two datasets, vertices and edges, containing information on blood type (see \url{http://www.redcrossblood.org/learn-about-blood/blood-types}).
#' The variables are as follows:
#'
#' @format A list of two data frames:
#' \itemize{
#' \item the edges data set consists of three variables of length 27:
#' \itemize{
#'   \item from, to factor variables of blood types describing the relationship 'is compatible with'
#'   \item group_to factor variable with levels 'same' and 'diff' for same or different blood type group not regarding the rho-gam factor.
#' }
#' \item the vertices data set consists of five variables and 32 rows:
#' \itemize{
#'   \item label factor variable of blood types,
#'   \item type factor variable of blood type, not regarding the rhesus factor,
#'   \item rho factor variable: 'pos' and 'neg' describing the rhesus factor,
#'   \item Ethnicity factor variable of four variables: 'Caucasians', 'African.American', 'Hispanic', and 'Asian',
#'   \item Predominance numeric variable consisting of the percentage points of each blood type within each ethnicity.
#' }
#' }
"blood"

#' What it is
#' 
#' A list of two datasets, vertices and edges, containing data on
#' 
#' This data set is from .
#' The variables are as follows:
#'
#' @format A list of two data frames:
#' \itemize{
#' \item the edges data set consists of __ variables of length __:
#' \itemize{
#'   \item VarName: Var description
#'   \item 
#' }
#' \item the vertices data set consists of __ variables with information on __ actors:
#' \itemize{
#'   \item VarName: Var description
#'   \item 
#' }
#' }
"email"

#' What it is
#' 
#' A list of two datasets, vertices and edges, containing data on
#' 
#' This data set is from .
#' The variables are as follows:
#'
#' @format A list of two data frames:
#' \itemize{
#' \item the edges data set consists of __ variables of length __:
#' \itemize{
#'   \item VarName: Var description
#'   \item 
#' }
#' \item the vertices data set consists of __ variables with information on __ actors:
#' \itemize{
#'   \item VarName: Var description
#'   \item 
#' }
#' }
"football"

#' What it is
#' 
#' A list of two datasets, vertices and edges, containing data on
#' 
#' This data set is from .
#' The variables are as follows:
#'
#' @format A list of two data frames:
#' \itemize{
#' \item the edges data set consists of __ variables of length __:
#' \itemize{
#'   \item VarName: Var description
#'   \item 
#' }
#' \item the vertices data set consists of __ variables with information on __ actors:
#' \itemize{
#'   \item VarName: Var description
#'   \item 
#' }
#' }
"lesmis"

#' Network of romantic relationships in the TV show Mad Men
#'
#' A list of two datasets, vertices and edges, containing information on sexual relations in the TV show Mad Men.
#' This data set was first compiled by Winston Chang for the package gcookbook and was extended here to include
#' the gender of each of the characters.
#' The variables are as follows:
#'
#' @format A list of two data frames:
#' \itemize{
#' \item the edges data set consists of two variables of length 39:
#' \itemize{
#'   \item Name1: Name of one of the sexual partners.
#'   \item Name2: Name of the other sexual partner.
#' }
#' \item the vertices data set consists of three variables with information on 45 characters of the show:
#' \itemize{
#'   \item label Name of the character,
#'   \item Gender factor variable of the gender of the character.
#' }
#' }
"madmen"

#' What it is
#' 
#' A list of two datasets, vertices and edges, containing data on
#' 
#' This data set is from .
#' The variables are as follows:
#'
#' @format A list of two data frames:
#' \itemize{
#' \item the edges data set consists of __ variables of length __:
#' \itemize{
#'   \item VarName: Var description
#'   \item 
#' }
#' \item the vertices data set consists of __ variables with information on __ actors:
#' \itemize{
#'   \item VarName: Var description
#'   \item 
#' }
#' }
"metro_map"

#' What it is
#' 
#' A list of two datasets, vertices and edges, containing data on
#' 
#' This data set is from .
#' The variables are as follows:
#'
#' @format A list of two data frames:
#' \itemize{
#' \item the edges data set consists of __ variables of length __:
#' \itemize{
#'   \item VarName: Var description
#'   \item 
#' }
#' \item the vertices data set consists of __ variables with information on __ actors:
#' \itemize{
#'   \item VarName: Var description
#'   \item 
#' }
#' }
"mm_directed"

#' What it is
#' 
#' A list of two datasets, vertices and edges, containing data on
#' 
#' This data set is from .
#' The variables are as follows:
#'
#' @format A list of two data frames:
#' \itemize{
#' \item the edges data set consists of __ variables of length __:
#' \itemize{
#'   \item VarName: Var description
#'   \item 
#' }
#' \item the vertices data set consists of __ variables with information on __ actors:
#' \itemize{
#'   \item VarName: Var description
#'   \item 
#' }
#' }
"protein"

#' What it is
#' 
#' A list of two datasets, vertices and edges, containing data on
#' 
#' This data set is from .
#' The variables are as follows:
#'
#' @format A list of two data frames:
#' \itemize{
#' \item the edges data set consists of __ variables of length __:
#' \itemize{
#'   \item VarName: Var description
#'   \item 
#' }
#' \item the vertices data set consists of __ variables with information on __ actors:
#' \itemize{
#'   \item VarName: Var description
#'   \item 
#' }
#' }
"soccer"

#' What it is
#' 
#' A list of two datasets, vertices and edges, containing data on
#' 
#' This data set is from .
#' The variables are as follows:
#'
#' @format A list of two data frames:
#' \itemize{
#' \item the edges data set consists of __ variables of length __:
#' \itemize{
#'   \item VarName: Var description
#'   \item 
#' }
#' \item the vertices data set consists of __ variables with information on __ actors:
#' \itemize{
#'   \item VarName: Var description
#'   \item 
#' }
#' }
"theme_elements"