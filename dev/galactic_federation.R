library(readxl)
library(Matrix)
library(dplyr)
library(tidyr)

galactic_federation <- read_excel("dev/galactic_federation.xlsx", range = "A2:M11")

galactic_federation <- galactic_federation %>%
  gather(product, export_value, -planet) %>%
  mutate_if(is.character, as.factor)

galactic_federation <- with(
  galactic_federation,
  sparseMatrix(
    i = as.numeric(planet),
    j = as.numeric(product),
    x = export_value,
    dimnames = list(levels(planet), levels(product))
  )
)
