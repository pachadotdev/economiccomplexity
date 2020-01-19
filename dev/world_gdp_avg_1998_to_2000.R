library(wbstats)
library(rlang)
library(Matrix)
library(dplyr)
library(purrr)

reference_year <- 2000

# inflation <- purrr::map_df(
#   1998:2000,
#   function(year) {
#     tradestatistics::ots_inflation %>%
#       dplyr::filter(
#         !!sym("to") <= 2010,
#         !!sym("to") > year
#       ) %>%
#       dplyr::summarise(
#         conversion_factor = dplyr::last(cumprod(!!sym("conversion_factor")))
#       ) %>%
#       dplyr::mutate(
#         year = year,
#         conversion_year = 2010
#       ) %>%
#       dplyr::select(!!!rlang::syms(c("year", "conversion_year", "conversion_factor")))
#   }
# )

inflation2 <- purrr::map_df(
  2000,
  function(year) {
    tradestatistics::ots_inflation %>%
      dplyr::filter(
        !!sym("to") <= 2010,
        !!sym("to") > year
      ) %>%
      dplyr::summarise(
        conversion_factor = dplyr::last(cumprod(!!sym("conversion_factor")))
      ) %>%
      dplyr::mutate(
        year = year,
        conversion_year = 2010
      ) %>%
      dplyr::select(!!!rlang::syms(c("year", "conversion_year", "conversion_factor")))
  }
)

gdp <- wb(indicator = "NY.GDP.PCAP.KD", startdate = 1998, enddate = 2000)

gdp2 <- gdp %>%
  select(year = date, iso3c, gdppc = value) %>%
  mutate(year = as.integer(year), iso3c = tolower(iso3c)) %>%
  mutate(gdppc = gdppc / inflation2$conversion_factor)

gdp2 <- gdp2 %>%
  group_by(iso3c) %>%
  summarise(gdppc = mean(gdppc, na.rm = T)) %>%
  mutate(gdppc = round(gdppc, 0))

world_gdp_avg_1998_to_2000 <- setNames(gdp2$gdppc, gdp2$iso3c)

save(world_gdp_avg_1998_to_2000, file = 'data/world_gdp_avg_1998_to_2000.rda', compress = "xz")
