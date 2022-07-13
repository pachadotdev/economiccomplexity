# library(Matrix)
library(rlang)
library(dplyr)
library(tidyr)
library(purrr)

finp <- list.files(path = "dev/sitc-data", full.names = T)

reference_year <- 2000

inflation <- purrr::map_df(
  1998:2000,
  function(year) {
    tradestatistics::ots_inflation %>%
      dplyr::filter(
        !!sym("to") <= reference_year,
        !!sym("to") > year
      ) %>%
      dplyr::summarise(
        conversion_factor = dplyr::last(cumprod(!!sym("conversion_factor")))
      ) %>%
      dplyr::mutate(
        year = year,
        conversion_year = reference_year
      ) %>%
      dplyr::select(!!!rlang::syms(c("year", "conversion_year", "conversion_factor")))
  }
)

inflation <- inflation %>% mutate(conversion_factor = ifelse(is.na(conversion_factor), 1, conversion_factor))

trade <- map_df(finp, readRDS)

trade <- trade %>%
  filter(product_code_length == 4)

trade <- trade %>%
  left_join(inflation) %>%
  mutate(trade_value_usd = trade_value_usd * conversion_factor)

trade <- trade %>%
  group_by(reporter_iso, product_code) %>%
  summarise(trade_value_usd = mean(trade_value_usd, na.rm = T)) %>%
  mutate(trade_value_usd = round(trade_value_usd, 0))

# world_trade_avg_1998_to_2000 <- trade %>%
#   ungroup() %>%
#   mutate_if(is.character, as.factor)
#
# world_trade_avg_1998_to_2000 <- with(
#   world_trade_avg_1998_to_2000,
#   sparseMatrix(
#     i = as.numeric(reporter_iso),
#     j = as.numeric(product_code),
#     x = trade_value_usd,
#     dimnames = list(levels(reporter_iso), levels(product_code))
#   )
# )

world_trade_avg_1998_to_2000 <- trade
names(world_trade_avg_1998_to_2000) <- c("country", "product", "value")

world_trade_avg_1998_to_2000 <- world_trade_avg_1998_to_2000 %>%
  ungroup()

save(world_trade_avg_1998_to_2000, file = 'data/world_trade_avg_1998_to_2000.rda', compress = "xz")
