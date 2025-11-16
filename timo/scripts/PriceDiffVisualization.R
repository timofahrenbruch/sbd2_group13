# ======================================================================
# VISUALISATION & STORYTELLING (FINAL CLEAN SINGLE-FILE VERSION)
# Run AFTER your analysis / bootstrapping code.
# ======================================================================

library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(sf)
library(geodata)
library(leaflet)
library(viridis)
library(htmltools)

# ----------------------------------------------------------------------
# 0) NORMALISE LANGUAGE REGION LABELS + RECOMPUTE exp_lang
# ----------------------------------------------------------------------

normalize_lang <- function(x) {
  case_when(
    x %in% c("German", "French", "Italian") ~ x,
    x %in% c("German/French", "French/German") ~ "German-French",
    x == "German/Italian" ~ "German-Italian",
    TRUE ~ x
  )
}

significant_language_diffs_norm <- significant_language_diffs |>
  mutate(
    region1 = normalize_lang(region1),
    region2 = normalize_lang(region2)
  )

# Compute Expensiveness Index for language regions
compute_expensiveness_index <- function(sig_tbl, region_type_label) {
  sig_tbl |>
    mutate(
      more_expensive = if_else(observed_difference > 0, region1, region2),
      less_expensive = if_else(observed_difference > 0, region2, region1)
    ) |>
    select(category, more_expensive, less_expensive) |>
    tidyr::pivot_longer(
      cols = c(more_expensive, less_expensive),
      names_to = "role",
      values_to = "region"
    ) |>
    mutate(score = if_else(role == "more_expensive", 1, -1)) |>
    group_by(region) |>
    summarise(
      expensiveness_index = sum(score),
      n_comparisons = n(),
      .groups = "drop"
    ) |>
    mutate(region_type = region_type_label)
}


exp_lang <- compute_expensiveness_index(
  significant_language_diffs_norm,
  "language_region"
) |>
  rename(language_region = region) |>
  mutate(
    expensiveness_index = replace_na(expensiveness_index, 0),
    n_comparisons = replace_na(n_comparisons, 0)
  )

exp_cant <- compute_expensiveness_index(
  significant_canton_diffs,
  "canton"
) |>
  rename(region = region) |>   # Keep column name 'region' for canton codes
  mutate(
    expensiveness_index = replace_na(expensiveness_index, 0),
    n_comparisons = replace_na(n_comparisons, 0)
  )


# ----------------------------------------------------------------------
# 1) PREP SIGNIFICANT DIFF TABLES → structured list
# ----------------------------------------------------------------------

sig_list <- list(
  language_region = significant_language_diffs_norm |>
    mutate(region_type = "language_region"),
  canton          = significant_canton_diffs   |>
    mutate(region_type = "canton")
)

sig_list <- lapply(
  sig_list,
  function(df) df |>
    mutate(
      abs_diff = abs(observed_difference),
      region_pair = paste(region1, "vs", region2)
    )
)

# ----------------------------------------------------------------------
# 2) RANKINGS PER REGION LEVEL (language + canton only)
# ----------------------------------------------------------------------

make_topN_plot <- function(df, region_label, N = 20) {
  
  topdf <- df |>
    arrange(desc(abs_diff)) |>
    slice_head(n = N)
  
  p <- ggplot(
    topdf,
    aes(
      x = reorder(paste(category, region_pair, sep = " | "), abs_diff),
      y = abs_diff,
      fill = abs_diff,
      text = paste(
        "Category:", category,
        "<br>Region pair:", region_pair,
        "<br>Observed Δ:", round(observed_difference, 2),
        "<br>CI:", paste0("[", round(lower_ci, 2), ", ", round(upper_ci, 2), "]"),
        "<br>p:", signif(p_value, 3)
      )
    )
  ) +
    geom_col() +
    coord_flip() +
    scale_fill_viridis(option = "C", guide = "none") +
    labs(
      x = NULL,
      y = "Absolute price difference",
      title = paste("Top", N, "price differences —", region_label)
    ) +
    theme_minimal(base_size = 11)
  
  ggplotly(p, tooltip = "text")
}

top20_lang <- make_topN_plot(sig_list$language_region, "Language regions")
top20_cant <- make_topN_plot(sig_list$canton,          "Cantons")

# ----------------------------------------------------------------------
# 3) CATEGORY STORYTELLING: distribution + bootstrap CI
# ----------------------------------------------------------------------

get_boot_table <- function(region_type) {
  switch(
    region_type,
    "language_region" = all_language_boot,
    "canton"          = all_canton_boot
  )
}

plot_category_distribution <- function(category_name, region_type, region_var) {
  
  df <- merged_data |>
    filter(
      infos.Category == category_name,
      !is.na(.data[[region_var]])
    )
  
  p <- ggplot(
    df,
    aes(
      x = .data[[region_var]],
      y = salesPrice.amountIncl,
      fill = .data[[region_var]],
      text = paste("Region:", .data[[region_var]], "<br>Price:", salesPrice.amountIncl)
    )
  ) +
    geom_violin(alpha = 0.4, color = NA) +
    geom_boxplot(width = 0.2, outlier.alpha = 0.2, color = "black") +
    coord_flip() +
    scale_fill_viridis_d(option = "C", guide = "none") +
    labs(
      x = region_type,
      y = "Price (CHF)",
      title = paste("Price distribution by", region_type, "for category:", category_name)
    ) +
    theme_minimal()
  
  ggplotly(p, tooltip = "text")
}

plot_category_bootstrap_ci <- function(category_name, region_type, region_var) {
  
  boot_tbl <- get_boot_table(region_type) |>
    filter(category == category_name)
  
  if (nrow(boot_tbl) == 0) return(NULL)
  
  region_scores <- boot_tbl |>
    mutate(
      more_expensive = if_else(observed_difference > 0, region1, region2),
      less_expensive = if_else(observed_difference > 0, region2, region1)
    ) |>
    select(more_expensive, less_expensive) |>
    pivot_longer(cols = everything(), names_to = "role", values_to = "region") |>
    mutate(score = if_else(role == "more_expensive", 1, -1)) |>
    group_by(region) |>
    summarise(net_score = sum(score), .groups = "drop")
  
  p <- ggplot(
    region_scores,
    aes(
      x = reorder(region, net_score),
      y = net_score,
      fill = net_score,
      text = paste("Region:", region, "<br>Score:", net_score)
    )
  ) +
    geom_col() +
    coord_flip() +
    scale_fill_viridis(option = "C", guide = "none") +
    labs(
      x = region_var,
      y = "Bootstrap expensiveness score",
      title = paste("Relative expensiveness for:", category_name)
    ) +
    theme_minimal()
  
  ggplotly(p, tooltip = "text")
}

# EXAMPLE category story (based on canton differences)
example_category    <- "sportuhr-smartwatch-2446" # replace with your chosen category
example_dist_plot   <- plot_category_distribution(example_category, "canton", "canton")
example_ci_plot     <- plot_category_bootstrap_ci(example_category, "canton", "canton")

# ----------------------------------------------------------------------
# 4) EXPENSIVENESS INDEX PLOTS (only lang + canton)
# ----------------------------------------------------------------------

plot_exp_index <- function(df, label, region_col) {
  
  df <- df |> rename(region_name = {{ region_col }})
  
  p <- ggplot(
    df,
    aes(
      x = reorder(region_name, expensiveness_index),
      y = expensiveness_index,
      fill = expensiveness_index,
      text = paste(
        "Region:", region_name,
        "<br>Index:", expensiveness_index,
        "<br>Comparisons:", n_comparisons
      )
    )
  ) +
    geom_col() +
    coord_flip() +
    scale_fill_viridis(option = "C", guide = "none") +
    labs(
      title = paste("Expensiveness index —", label),
      x = NULL,
      y = "Expensiveness index"
    ) +
    theme_minimal()
  
  ggplotly(p, tooltip = "text")
}

exp_lang_plot <- plot_exp_index(exp_lang, "Language regions", language_region)
exp_cant_plot <- plot_exp_index(exp_cant, "Cantons", region)

# ----------------------------------------------------------------------
# 5) CANTON MAP (expensiveness)
# ----------------------------------------------------------------------

exp_cant_coded <- exp_cant |>
  rename(canton = region) |>
  mutate(
    expensiveness_index = replace_na(expensiveness_index, 0),
    n_comparisons = replace_na(n_comparisons, 0)
  )

che_cantons <- geodata::gadm("CHE", level = 1, path = tempdir()) |>
  st_as_sf()

canton_name_lookup <- tibble::tribble(
  ~canton, ~NAME_1,
  "AG","Aargau","AI","Appenzell Innerrhoden","AR","Appenzell Ausserrhoden",
  "BE","Bern","BL","Basel-Landschaft","BS","Basel-Stadt","FR","Fribourg",
  "GE","Genève","GL","Glarus","GR","Graubünden","JU","Jura","LU","Lucerne",
  "NE","Neuchâtel","NW","Nidwalden","OW","Obwalden","SG","Sankt Gallen",
  "SH","Schaffhausen","SO","Solothurn","SZ","Schwyz","TG","Thurgau",
  "TI","Ticino","UR","Uri","VD","Vaud","VS","Valais","ZG","Zug","ZH","Zürich"
)

exp_cant_fixed <- canton_name_lookup |>
  left_join(exp_cant_coded, by = "canton")

che_cantons_joined <- che_cantons |>
  left_join(canton_name_lookup, by = "NAME_1") |>
  left_join(exp_cant_fixed,  by = "canton")

pal_canton <- colorNumeric(
  palette = viridis(256),
  domain  = che_cantons_joined$expensiveness_index
)

leaflet_canton_exp <- leaflet(che_cantons_joined) |>
  addProviderTiles(providers$CartoDB.Positron) |>
  addPolygons(
    fillColor = ~pal_canton(expensiveness_index),
    fillOpacity = 0.8,
    weight = 1,
    color = "#555555",
    label = ~lapply(
      paste0(
        "<b>", canton, "</b><br>",
        "Expensiveness index: ", expensiveness_index, "<br>",
        "Comparisons: ", n_comparisons
      ),
      htmltools::HTML
    ),
    highlightOptions = highlightOptions(weight = 3, color = "black")
  ) |>
  addLegend(
    pal = pal_canton,
    values = ~expensiveness_index,
    title = "Canton Expensiveness Index"
  )

# ----------------------------------------------------------------------
# 6) LANGUAGE REGION MAP
# ----------------------------------------------------------------------

canton_lang_clean <- canton_lang_lookup |>
  mutate(language_region = normalize_lang(language_region)) |>
  select(canton, language_region)

che_lang_base <- che_cantons_joined |>
  select(canton, geometry) |>
  distinct(canton, .keep_all = TRUE)

che_lang <- che_lang_base |>
  left_join(canton_lang_clean, by = "canton")

che_lang_joined <- che_lang |>
  left_join(exp_lang, by = "language_region")

lang_regions <- che_lang_joined |>
  group_by(language_region, expensiveness_index, n_comparisons) |>
  summarise(geometry = st_union(geometry), .groups = "drop") |>
  st_as_sf()

pal_lang <- colorNumeric(
  palette = viridis(256),
  domain  = lang_regions$expensiveness_index
)

leaflet_language_exp <- leaflet(lang_regions) |>
  addProviderTiles(providers$CartoDB.Positron) |>
  addPolygons(
    fillColor = ~pal_lang(expensiveness_index),
    fillOpacity = 0.8,
    weight = 1,
    color = "#333333",
    label = ~lapply(
      paste0(
        "<b>", language_region, "</b><br>",
        "Expensiveness index: ", expensiveness_index, "<br>",
        "Comparisons: ", n_comparisons
      ),
      htmltools::HTML
    ),
    highlightOptions = highlightOptions(weight = 3, color = "black")
  ) |>
  addLegend(
    pal = pal_lang,
    values = ~expensiveness_index,
    title = "Language Region Expensiveness Index"
  )

# ----------------------------------------------------------------------
# 7) TRIGGER OUTPUT
# ----------------------------------------------------------------------

top20_lang
top20_cant
example_dist_plot
example_ci_plot
exp_lang_plot
exp_cant_plot
leaflet_language_exp
leaflet_canton_exp

# ======================================================================
# END OF STORYTELLING SCRIPT
# ======================================================================
