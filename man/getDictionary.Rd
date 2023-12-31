% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/main.r
\name{getDictionary}
\alias{getDictionary}
\title{Create a dictionary}
\usage{
getDictionary(filename)
}
\arguments{
\item{filename}{The file path to the document template. Supports .doc and .docx}
}
\value{
A data frame where each row represents a flag in the template document and its replacement value
}
\description{
Scans the input file for strings enclosed by flag wings: « », then creates an empty dictionary with corresponding
replacement values for each flag.
}
\examples{

# Path to the sample template file included in the package
template_path <- system.file("template_document", "Template.docx", package="docket")

# Create a dictionary by using the getDictionary function on the sample template file
result <- getDictionary(template_path)
print(result)

}
