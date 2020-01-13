library(dplyr)
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
  summarise(trade_value_usd = sum(trade_value_usd, na.rm = T)) %>%
  mutate(trade_value_usd = round(trade_value_usd, 0))

saveRDS(trade, file = 'dev/world_trade_avg_1998_to_2000.rds', compress = "xz")
