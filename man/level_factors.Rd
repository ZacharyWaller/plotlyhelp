% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{level_factors}
\alias{level_factors}
\title{Specify factor level orders}
\usage{
level_factors(data, category_type, category, factor_data)
}
\arguments{
\item{data}{data.frame with columns to be turned into factors and ordered}

\item{category_type}{column name (unquoted) in data giving the name of
the overall category group to be joined on the corresponding
\code{category_type} column in \code{factor_data}}

\item{category}{column name (unquoted) to be turned into a factor}

\item{factor_data}{data.frame with 3 columns: \code{category_type} and
\code{category} to join to the columns specified by \code{category_type}
and \code{category} and an \code{order} column specifying the levels of
each factor.}
}
\description{
Specify factor level orders
}
\details{
Use a data.frame to order the levels of factors in another. This
means you can make a csv with all your factor orders specified and not
worry about having factor orders all over your code.

Returns data untouched if the values in  \code{category_type} and \code{category}
in \code{factor_data} don't align with the values in the data.
}
\examples{
# Make the sub_group column into a factor, ordered in reverse alphabetical
# order
data <- data.frame(
  sub_group = sample(letters[10:15], 20, replace = TRUE),
  values = rnorm(20)
)

factor_data <- data.frame(
  category_type = "A",
  category = c(letters[10:15]),
  order = 6:1
)

arrange(data, sub_group)

new_data <- level_factors(data, "A", sub_group, factor_data)

arrange(new_data, sub_group)

# You can store the orders of many factors in factor_data and apply
# to different columnns
factor_data <- data.frame(
  category_type = rep(c("A", "B", "C"), times = c(6, 2, 4)),
  category = c(letters[10:15], "B 1", "B 2", LETTERS[23:26]),
  order = c(6:1, 1:2, c(2, 1, 4, 3))
)

data <- data.frame(
  A_group = sample(letters[10:15], 20, replace = TRUE),
  B_group = sample(c("B 1", "B 2"), 20, replace = TRUE),
  C_group = sample(LETTERS[23:26], 20, replace = TRUE),
  values = rnorm(20)
)

new_data <- data \%>\%
  level_factors("A", A_group, factor_data) \%>\%
  level_factors("B", B_group, factor_data) \%>\%
  level_factors("C", C_group, factor_data)

levels(new_data$A_group)
levels(new_data$B_group)
levels(new_data$C_group)

}
