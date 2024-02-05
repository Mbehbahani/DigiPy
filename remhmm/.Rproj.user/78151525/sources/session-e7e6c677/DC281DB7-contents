# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

matrices_to_vector <- function(gamma, mean0, sd0, delta0) {
  m <- nrow(gamma)
  vc <- c(t(gamma[,1:m-1]), t(mean0), t(sd0), delta0[1:m-1])
  return(vc)
}

# Function to convert vector to matrices
# number of variables
