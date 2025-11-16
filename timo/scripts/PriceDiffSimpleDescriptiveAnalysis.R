# ======================================================================
# ADDITIONAL SIMPLE DESCRIPTIVE ANALYSIS  (FINAL VERSION — NO DASHBOARD)
# ======================================================================
# This script assumes that your full analysis + storytelling script
# has already run in the same R session.
# Objects used here: merged_data, normalize_lang(), canton_name_lookup,
#                    che_cantons
# ======================================================================

library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(sf)
library(leaflet)
library(viridis)
library(htmltools)


# ======================================================================
# 1) OVERALL SWISS MEAN vs REGION MEANS  (LANGUAGE + CANTON)
# ======================================================================

overall_mean_price <- mean(merged_data$salesPrice.amountIncl, na.rm = TRUE)

# --- 1a) Language regions -------------------------------------------------------
avg_lang_simple <- merged_data %>%
  mutate(language_region = normalize_lang(language_region)) %>%
  filter(!is.na(language_region)) %>%
  group_by(language_region) %>%
  summarise(
    mean_price = mean(salesPrice.amountIncl, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  mutate(diff_from_overall = mean_price - overall_mean_price)

p_lang_overall_simple <- ggplot(
  avg_lang_simple,
  aes(
    x = reorder(language_region, mean_price),
    y = mean_price,
    fill = diff_from_overall,
    text = paste(
      "Language region:", language_region,
      "<br>Mean price:", round(mean_price, 2),
      "<br>vs Swiss mean:", round(diff_from_overall, 2),
      "<br>n:", n
    )
  )
) +
  geom_col() +
  coord_flip() +
  scale_fill_viridis(option = "C", name = "Δ vs Swiss mean") +
  labs(
    title = "Average price by language region vs Swiss mean",
    x = NULL, y = "Mean price (CHF)"
  ) +
  theme_minimal(12)

p_lang_overall_simple <- ggplotly(p_lang_overall_simple, tooltip = "text")


# --- 1b) Cantons -------------------------------------------------------
avg_canton_simple <- merged_data %>%
  filter(!is.na(canton)) %>%
  group_by(canton) %>%
  summarise(
    mean_price = mean(salesPrice.amountIncl, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  mutate(diff_from_overall = mean_price - overall_mean_price)

p_canton_overall_simple <- ggplot(
  avg_canton_simple,
  aes(
    x = reorder(canton, mean_price),
    y = mean_price,
    fill = diff_from_overall,
    text = paste(
      "Canton:", canton,
      "<br>Mean price:", round(mean_price, 2),
      "<br>vs Swiss mean:", round(diff_from_overall, 2),
      "<br>n:", n
    )
  )
) +
  geom_col() +
  coord_flip() +
  scale_fill_viridis(option = "C", name = "Δ vs Swiss mean") +
  labs(
    title = "Average price by canton vs Swiss mean",
    x = NULL, y = "Mean price (CHF)"
  ) +
  theme_minimal(12)

p_canton_overall_simple <- ggplotly(p_canton_overall_simple, tooltip = "text")



# ======================================================================
# 2) TOP-10 CATEGORY SYSTEM (TWO DIFFERENT RANKINGS)
# ======================================================================

# A) Top 10 by transaction volume
top_categories_by_volume <- merged_data %>%
  count(infos.Category, name = "n") %>%
  arrange(desc(n)) %>%
  slice_head(n = 10) %>%
  pull(infos.Category)

# B) Top 10 by revenue
top_categories_by_revenue <- merged_data %>%
  mutate(revenue = salesPrice.amountIncl) %>%
  group_by(infos.Category) %>%
  summarise(
    total_revenue = sum(revenue, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(total_revenue)) %>%
  slice_head(n = 10) %>%
  pull(infos.Category)

# ======================================================================
# 3c) NEW — SEPARATED “TOP 10 BY VOLUME” vs “TOP 10 BY REVENUE”
# ======================================================================

# ------------------------ LANGUAGE REGION — VOLUME ---------------------
lang_cat_volume <- merged_data %>%
  mutate(language_region = normalize_lang(language_region)) %>%
  filter(
    !is.na(language_region),
    infos.Category %in% top_categories_by_volume
  ) %>%
  group_by(infos.Category, language_region) %>%
  summarise(
    mean_price = mean(salesPrice.amountIncl, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

p_lang_cat_volume <- ggplot(
  lang_cat_volume,
  aes(
    x = language_region,
    y = mean_price,
    fill = language_region,
    text = paste(
      "Category:", infos.Category,
      "<br>Language region:", language_region,
      "<br>Mean price:", round(mean_price, 2),
      "<br>n:", n
    )
  )
) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  facet_wrap(~ infos.Category, scales = "free_x") +
  scale_fill_viridis_d(option = "C") +
  labs(
    title = "Mean price by Language Region — Top 10 by Volume",
    subtitle = "Categories ranked by transaction count",
    x = "Language region", y = "Mean price (CHF)"
  ) +
  theme_minimal(11)

p_lang_cat_volume <- ggplotly(p_lang_cat_volume, tooltip = "text")


# ------------------------ LANGUAGE REGION — REVENUE ---------------------
lang_cat_revenue <- merged_data %>%
  mutate(language_region = normalize_lang(language_region)) %>%
  filter(
    !is.na(language_region),
    infos.Category %in% top_categories_by_revenue
  ) %>%
  group_by(infos.Category, language_region) %>%
  summarise(
    mean_price = mean(salesPrice.amountIncl, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

p_lang_cat_revenue <- ggplot(
  lang_cat_revenue,
  aes(
    x = language_region,
    y = mean_price,
    fill = language_region,
    text = paste(
      "Category:", infos.Category,
      "<br>Language region:", language_region,
      "<br>Mean price:", round(mean_price, 2),
      "<br>n:", n
    )
  )
) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  facet_wrap(~ infos.Category, scales = "free_x") +
  scale_fill_viridis_d(option = "C") +
  labs(
    title = "Mean price by Language Region — Top 10 by Revenue",
    subtitle = "Categories ranked by total CHF revenue",
    x = "Language region", y = "Mean price (CHF)"
  ) +
  theme_minimal(11)

p_lang_cat_revenue <- ggplotly(p_lang_cat_revenue, tooltip = "text")


# ------------------------------ CANTON — VOLUME -------------------------
canton_cat_volume <- merged_data %>%
  filter(
    !is.na(canton),
    infos.Category %in% top_categories_by_volume
  ) %>%
  group_by(infos.Category, canton) %>%
  summarise(
    mean_price = mean(salesPrice.amountIncl, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

p_canton_cat_volume <- ggplot(
  canton_cat_volume,
  aes(
    x = canton,
    y = mean_price,
    fill = canton,
    text = paste(
      "Category:", infos.Category,
      "<br>Canton:", canton,
      "<br>Mean price:", round(mean_price, 2),
      "<br>n:", n
    )
  )
) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  facet_wrap(~ infos.Category, scales = "free_x") +
  scale_fill_viridis_d(option = "C") +
  labs(
    title = "Mean price by Canton — Top 10 by Volume",
    subtitle = "Categories ranked by transaction count",
    x = "Canton", y = "Mean price (CHF)"
  ) +
  theme_minimal(11)

p_canton_cat_volume <- ggplotly(p_canton_cat_volume, tooltip = "text")


# ------------------------------ CANTON — REVENUE -------------------------
canton_cat_revenue <- merged_data %>%
  filter(
    !is.na(canton),
    infos.Category %in% top_categories_by_revenue
  ) %>%
  group_by(infos.Category, canton) %>%
  summarise(
    mean_price = mean(salesPrice.amountIncl, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

p_canton_cat_revenue <- ggplot(
  canton_cat_revenue,
  aes(
    x = canton,
    y = mean_price,
    fill = canton,
    text = paste(
      "Category:", infos.Category,
      "<br>Canton:", canton,
      "<br>Mean price:", round(mean_price, 2),
      "<br>n:", n
    )
  )
) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  facet_wrap(~ infos.Category, scales = "free_x") +
  scale_fill_viridis_d(option = "C") +
  labs(
    title = "Mean price by Canton — Top 10 by Revenue",
    subtitle = "Categories ranked by total CHF revenue",
    x = "Canton", y = "Mean price (CHF)"
  ) +
  theme_minimal(11)

p_canton_cat_revenue <- ggplotly(p_canton_cat_revenue, tooltip = "text")



# ======================================================================
# 4) MAPS — MEAN PRICE (CANTON + LANGUAGE REGION)
# ======================================================================

# --- 4a) Mean price per canton ----------------------------------------

che_cantons_mean <- che_cantons %>%
  left_join(canton_name_lookup, by = "NAME_1") %>%
  left_join(avg_canton_simple, by = "canton")

pal_canton_mean <- colorNumeric(
  palette = viridis(256),
  domain  = che_cantons_mean$mean_price
)

leaflet_canton_mean_price <- leaflet(che_cantons_mean) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(
    fillColor = ~pal_canton_mean(mean_price),
    fillOpacity = 0.8,
    weight = 1,
    color = "#555555",
    label = ~lapply(
      paste0(
        "<b>", canton, "</b><br>",
        "Mean price: ", round(mean_price, 2), " CHF<br>",
        "Transactions: ", n
      ),
      htmltools::HTML
    ),
    highlightOptions = highlightOptions(
      weight = 3,
      color = "black",
      bringToFront = TRUE
    )
  ) %>%
  addLegend(
    pal = pal_canton_mean,
    values = ~mean_price,
    title = "Mean price per canton (CHF)"
  )


# --- 4b) Mean price per language region -------------------------------

avg_lang_simple_map <- merged_data %>%
  mutate(language_region = normalize_lang(language_region)) %>%
  filter(!is.na(language_region)) %>%
  group_by(language_region) %>%
  summarise(
    mean_price = mean(salesPrice.amountIncl, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

canton_lang_clean_simple <- canton_lang_lookup %>%
  mutate(language_region = normalize_lang(language_region)) %>%
  select(canton, language_region)

che_lang_mean_base <- che_cantons %>%
  left_join(canton_name_lookup, by = "NAME_1") %>%
  select(canton, geometry) %>%
  distinct(canton, .keep_all = TRUE) %>%
  left_join(canton_lang_clean_simple, by = "canton")

che_lang_mean_joined <- che_lang_mean_base %>%
  left_join(avg_lang_simple_map, by = "language_region")

lang_mean_regions <- che_lang_mean_joined %>%
  group_by(language_region, mean_price, n) %>%
  summarise(geometry = st_union(geometry), .groups = "drop") %>%
  st_as_sf()

pal_lang_mean <- colorNumeric(
  palette = viridis(256),
  domain  = lang_mean_regions$mean_price
)

leaflet_language_mean_price <- leaflet(lang_mean_regions) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(
    fillColor = ~pal_lang_mean(mean_price),
    fillOpacity = 0.8,
    weight = 1,
    color = "#333333",
    label = ~lapply(
      paste0(
        "<b>", language_region, "</b><br>",
        "Mean price: ", round(mean_price, 2), " CHF<br>",
        "Transactions: ", n
      ),
      htmltools::HTML
    ),
    highlightOptions = highlightOptions(
      weight = 3,
      color = "black",
      bringToFront = TRUE
    )
  ) %>%
  addLegend(
    pal = pal_lang_mean,
    values = ~mean_price,
    title = "Mean price per language region (CHF)"
  )


# ======================================================================
# 5) DISPLAY ALL SIMPLE PLOTS/MAPS
# ======================================================================

# Overall
p_lang_overall_simple
p_canton_overall_simple

# NEW separated plots
p_lang_cat_volume
p_lang_cat_revenue
p_canton_cat_volume
p_canton_cat_revenue

# Maps
leaflet_language_mean_price
leaflet_canton_mean_price

# ======================================================================
# END OF SIMPLE DESCRIPTIVE ANALYSIS (NO DASHBOARD)
# ======================================================================
