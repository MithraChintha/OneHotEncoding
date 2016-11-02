\name{one hot encoding}
\alias{one hot encoding}
\title{
One Hot Encoding
}

\usage{
oneHotEncoding(data)
}
\arguments{
  \item{data}{- a dataframe which can contain any type of variables but only variables of type factor are converted into multiple columns
}

}
\value{
returns a data frame with One hot encoded columns}
\description{

One hot encoding technique is used to encode categorical integer features using a one-hot aka one-of-K scheme

One hot encoding function is used to convert the factor variables into multiple columns based on the unique values.Here assuming the categorical integer variables are factors

}
\examples{

cars <-oneHotEncoding(cars)

}
