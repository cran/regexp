\name{regexp}
\title{Regular Expressions}
\usage{
regexp(pattern, string, ignore.case=FALSE, extended=TRUE, index.only=TRUE)
regsub(pattern, string, sub, ignore.case=FALSE, extended=TRUE, all=FALSE)
regcomp(pattern, flags=REG.EXTENDED)
regexec(rex, string, flags=0)
url.parts(url)
url.decode(url)
REG.EXTENDED
REG.NEWLINE
REG.NOSUB
REG.ICASE
REG.NOTBOL
REG.NOTEOL
}
\alias{regsub}
\alias{regcomp}
\alias{regexec}
\alias{url.parts}
\alias{url.decode}
\alias{REG.EXTENDED}
\alias{REG.NEWLINE}
\alias{REG.NOSUB}
\alias{REG.ICASE}
\alias{REG.NOTBOL}
\alias{REG.NOTEOL}
\arguments{
\item{pattern}{character string containing a regular expression
  to be matched.}
\item{string}{character string where matches are sought.}
\item{ignore.case}{if \code{FALSE}, the pattern matching is
  \emph{case sensitive} and if \code{TRUE}, case is ignored during matching.}
\item{extended}{if \code{TRUE}, extended regular expression matching
  is used, and if \code{FALSE} basic regular expressions are used.}
\item{index.only}{if \code{FALSE}, a vector of the mathed substrings
  is returned; if \code{TRUE}, a matrix of the start and end
  positions, in C indexing, is returned.}
\item{sub}{a value to substitute. Can be a string or a function of one
  argument, the string vector of matches, returning a string to use.}
\item{all}{replace all occurences if \code{TRUE}, only first if \code{FALSE}}
\item{rex}{a compiled regular expression}
\item{flags}{in integer containing flags}
\item{url}{a url string}
}
\description{
  \code{regexp} Finds match and any substring matches.

  \code{regsub}{replaces the first or all matches with the value specified
  by \code{sub}}

  \code{regcomp} and \code{regmatch} are interfaces to the lower level POSIX
  regular expression routines by the same name.

  \code{url.parts} takes a url string and decomposes it into the
  canonical parts.  \code{url.decode} reverses the standard url
  encoding of spaces and non-alpha-numeric characters.

}
\value{
  \code{regexp} returns a string vector of the matches found, or a
  matrix of the match posotions if \code{index.only} is \code{TRUE}}

  \code{regsub} and \code{url.decode} return the modified string.

  \code{url.parts} returns a string vector of the parts.

  \code{regcomp} returns a compiled regular expression object
  and \code{regexec} returns either \code{NULL}, if there is no match,
  or a matrix of the match indices.
}
\examples{
regexp("^(([^:]+)://)?([^:/]+)(:([0-9]+))?(/.*)",
       "http://stat.umn.edu:80/xyz")

url.parts("http://stat.umn.edu:80/xyz")

regsub("\\\\+", "abc+def+ghi", " ", all=TRUE)

rex <- regcomp("^(([^:]+)://)?([^:/]+)(:([0-9]+))?(/.*)")
regexec(rex, "http://stat.umn.edu:80/xyz")
}
\keyword{character}
\keyword{utilities}
