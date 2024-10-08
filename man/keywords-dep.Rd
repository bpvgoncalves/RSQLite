% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SQLKeywords_SQLiteConnection.R,
%   R/isSQLKeyword_SQLiteConnection_character.R,
%   R/make.db.names_SQLiteConnection_character.R
\name{SQLKeywords_SQLiteConnection}
\alias{SQLKeywords_SQLiteConnection}
\alias{SQLKeywords,SQLiteConnection-method}
\alias{isSQLKeyword_SQLiteConnection_character}
\alias{isSQLKeyword,SQLiteConnection,character-method}
\alias{make.db.names_SQLiteConnection_character}
\alias{make.db.names,SQLiteConnection,character-method}
\title{Make R/S-Plus identifiers into legal SQL identifiers}
\usage{
\S4method{SQLKeywords}{SQLiteConnection}(dbObj, ...)

\S4method{isSQLKeyword}{SQLiteConnection,character}(
  dbObj,
  name,
  keywords = .SQL92Keywords,
  case = c("lower", "upper", "any")[3],
  ...
)

\S4method{make.db.names}{SQLiteConnection,character}(
  dbObj,
  snames,
  keywords = .SQL92Keywords,
  unique = TRUE,
  allow.keywords = TRUE,
  ...
)
}
\description{
Deprecated. Please use \code{\link[DBI:dbQuoteIdentifier]{DBI::dbQuoteIdentifier()}} instead.
}
\keyword{internal}
