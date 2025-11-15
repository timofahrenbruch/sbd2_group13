# -------------------------------------------------------------------------
# Load libraries
# -------------------------------------------------------------------------
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(purrr)
library(plotly)
library(stringr)
library(furrr)
library(cli)

# -------------------------------------------------------------------------
# Load & merge data
# -------------------------------------------------------------------------
setw_wd("data")
digitec_data <- read.csv("DigitecLive_Cleaned.csv")
gemeinde_data <- read_xlsx("Gemeindestand.xlsx")

merged_data <- digitec_data |>
  left_join(
    gemeinde_data |> select(Gemeindename, Bezirksname),
    by = c("cityName_clean" = "Gemeindename")
  )

canton_lang_lookup <- tibble::tibble(
  canton = c(
    "AG","AR","AI","BS","BL","BE","FR","GE","GL",
    "GR","JU","LU","NE","NW","OW","SH","SG","SZ",
    "SO","TI","TG","UR","VS","VD","ZG","ZH"
  ),
  language_region = c(
    "German",          # AG
    "German",          # AR
    "German",          # AI
    "German",          # BS
    "German",          # BL
    "German/French",   # BE
    "French/German",   # FR
    "French",          # GE
    "German",          # GL
    "German/Italian",  # GR
    "French",          # JU
    "German",          # LU
    "French",          # NE
    "German",          # NW
    "German",          # OW
    "German",          # SH
    "German",          # SG
    "German",          # SZ
    "German",          # SO
    "Italian",         # TI
    "German",          # TG
    "German",          # UR
    "French/German",   # VS
    "French",          # VD
    "German",          # ZG
    "German"           # ZH
  )
)

gemeinde_with_language <- gemeinde_data |>
  left_join(canton_lang_lookup, by = c("Kanton" = "canton"))

merged_data <- digitec_data |>
  left_join(
    gemeinde_with_language |> select(Gemeindename, Bezirksname, language_region),
    by = c("cityName_clean" = "Gemeindename")
  )

# -------------------------------------------------------------------------
# Weighted average prices
# -------------------------------------------------------------------------
merged_with_weights <- merged_data |>
  group_by(cityName_clean, infos.Category) |> mutate(purchases_municipality = n()) |> ungroup() |>
  group_by(Bezirksname, infos.Category) |> mutate(purchases_district = n()) |> ungroup() |>
  group_by(canton, infos.Category) |> mutate(purchases_canton = n()) |> ungroup() |>
  group_by(language_region, infos.Category) |> mutate(purchases_language = n()) |> ungroup()

avg_price_municipality <- merged_with_weights |>
  group_by(cityName_clean, infos.Category) |>
  summarise(avg_price = weighted.mean(salesPrice.amountIncl, w = purchases_municipality), .groups = "drop")

avg_price_district <- merged_with_weights |>
  group_by(Bezirksname, infos.Category) |>
  summarise(avg_price = weighted.mean(salesPrice.amountIncl, w = purchases_district), .groups = "drop")

avg_price_canton <- merged_with_weights |>
  group_by(canton, infos.Category) |>
  summarise(avg_price = weighted.mean(salesPrice.amountIncl, w = purchases_canton), .groups = "drop")

avg_price_language <- merged_with_weights |>
  group_by(language_region, infos.Category) |>
  summarise(avg_price = weighted.mean(salesPrice.amountIncl, w = purchases_language), .groups = "drop")

avg_price_overall <- merged_with_weights |>
  group_by(infos.Category) |>
  summarise(overall_avg_price = mean(salesPrice.amountIncl), .groups = "drop")

price_stats <- list(
  municipality = avg_price_municipality,
  district     = avg_price_district,
  canton       = avg_price_canton,
  language     = avg_price_language,
  overall      = avg_price_overall
)

# -------------------------------------------------------------------------
# FAST VECTORISED BOOTSTRAP
# -------------------------------------------------------------------------
bootstrap_mean_diff_fast <- function(
    data,
    group_var,
    n_boot = 500,
    min_obs_per_region = 5
) {
  region_levels <- unique(data[[group_var]])
  if (length(region_levels) < 2) return(NULL)
  
  region_pairs <- combn(region_levels, 2, simplify = FALSE)
  
  map_dfr(region_pairs, function(pair) {
    r1 <- pair[1]
    r2 <- pair[2]
    
    prices1 <- data$salesPrice.amountIncl[data[[group_var]] == r1]
    prices2 <- data$salesPrice.amountIncl[data[[group_var]] == r2]
    
    if (length(prices1) < min_obs_per_region || length(prices2) < min_obs_per_region) {
      return(NULL)
    }
    
    obs <- mean(prices1) - mean(prices2)
    
    n1 <- length(prices1)
    n2 <- length(prices2)
    
    boot_mat1 <- matrix(sample(prices1, n1 * n_boot, replace = TRUE), n1, n_boot)
    boot_mat2 <- matrix(sample(prices2, n2 * n_boot, replace = TRUE), n2, n_boot)
    
    boot_means1 <- colMeans(boot_mat1)
    boot_means2 <- colMeans(boot_mat2)
    boot_dist <- boot_means1 - boot_means2
    
    tibble(
      region1 = r1,
      region2 = r2,
      observed_difference = obs,
      lower_ci = quantile(boot_dist, 0.025),
      upper_ci = quantile(boot_dist, 0.975),
      p_value = mean(sign(boot_dist) != sign(obs)) * 2
    )
  })
}

# -------------------------------------------------------------------------
# PARALLEL BOOTSTRAP WRAPPER
# -------------------------------------------------------------------------
run_region_bootstrap_parallel <- function(
    data,
    region_var,
    n_boot = 500,
    min_total_per_cat = 30,
    max_regions_per_cat = 30,
    min_obs_per_region = 5
) {
  region_summary <- data |>
    filter(!is.na(.data[[region_var]])) |>
    group_by(infos.Category) |>
    summarise(
      n_total   = n(),
      n_regions = n_distinct(.data[[region_var]]),
      .groups = "drop"
    )
  
  valid_categories <- region_summary |>
    filter(
      n_total >= min_total_per_cat,
      n_regions >= 2,
      n_regions <= max_regions_per_cat
    ) |> pull(infos.Category)
  
  category_list <- data |>
    filter(
      infos.Category %in% valid_categories,
      !is.na(.data[[region_var]])
    ) |>
    group_by(infos.Category) |> group_split()
  
  cli::cli_inform(c("i" = paste0(
    "Bootstrapping ", length(category_list),
    " categories for region variable '", region_var, "'."
  )))
  
  future_map_dfr(
    category_list,
    .progress = TRUE,
    .options = furrr_options(seed = TRUE),   # FIXED RNG
    function(df_cat) {
      cat_name <- unique(df_cat$infos.Category)
      
      boot_res <- bootstrap_mean_diff_fast(
        df_cat,
        region_var,
        n_boot = n_boot,
        min_obs_per_region = min_obs_per_region
      )
      
      if (is.null(boot_res) || nrow(boot_res) == 0) return(NULL)
      
      boot_res |>
        mutate(category = cat_name) |>
        relocate(category)
    }
  )
}

future::plan(future::multisession, workers = max(1, parallel::detectCores() - 1))

# -------------------------------------------------------------------------
# LANGUAGE REGIONS
# -------------------------------------------------------------------------
all_language_boot <- run_region_bootstrap_parallel(
  merged_data, "language_region", n_boot = 500, max_regions_per_cat = 10
)

significant_language_diffs <- all_language_boot |>
  filter((lower_ci > 0 | upper_ci < 0) & p_value < 0.05) |>
  mutate(direction = if_else(observed_difference > 0,
                             "region1_more_expensive", "region2_more_expensive"))

top10_language_diffs <- significant_language_diffs |>
  mutate(abs_diff = abs(observed_difference)) |>
  arrange(desc(abs_diff)) |> slice_head(n = 10) |> select(-abs_diff)

# -------------------------------------------------------------------------
# CANTONS
# -------------------------------------------------------------------------
all_canton_boot <- run_region_bootstrap_parallel(
  merged_data, "canton", n_boot = 500, max_regions_per_cat = 26
)

significant_canton_diffs <- all_canton_boot |>
  filter((lower_ci > 0 | upper_ci < 0) & p_value < 0.05) |>
  mutate(direction = if_else(observed_difference > 0,
                             "region1_more_expensive", "region2_more_expensive"))

top10_canton_diffs <- significant_canton_diffs |>
  mutate(abs_diff = abs(observed_difference)) |>
  arrange(desc(abs_diff)) |> slice_head(n = 10) |> select(-abs_diff)

# -------------------------------------------------------------------------
# DISTRICTS
# -------------------------------------------------------------------------
all_district_boot <- run_region_bootstrap_parallel(
  merged_data, "Bezirksname", n_boot = 500, max_regions_per_cat = 30
)

significant_district_diffs <- all_district_boot |>
  filter((lower_ci > 0 | upper_ci < 0) & p_value < 0.05) |>
  mutate(direction = if_else(observed_difference > 0,
                             "region1_more_expensive", "region2_more_expensive"))

top10_district_diffs <- significant_district_diffs |>
  mutate(abs_diff = abs(observed_difference)) |>
  arrange(desc(abs_diff)) |> slice_head(n = 10) |> select(-abs_diff)

# -------------------------------------------------------------------------
# MUNICIPALITIES
# -------------------------------------------------------------------------
all_municipality_boot <- run_region_bootstrap_parallel(
  merged_data, "cityName_clean", n_boot = 500, max_regions_per_cat = 30
)

significant_municipality_diffs <- all_municipality_boot |>
  filter((lower_ci > 0 | upper_ci < 0) & p_value < 0.05) |>
  mutate(direction = if_else(observed_difference > 0,
                             "region1_more_expensive", "region2_more_expensive"))

top10_municipality_diffs <- significant_municipality_diffs |>
  mutate(abs_diff = abs(observed_difference)) |>
  arrange(desc(abs_diff)) |> slice_head(n = 10) |> select(-abs_diff)