# Here's a corrected version that shows how to properly use Observable plots in spacetimeview

library(tidyverse)
library(sf)
# library(spacetimeview)
devtools::load_all('../spacetimeview')

# Read in data
spatial_predictions <- readRDS("spatial_predictions.rds")
spatial_occurrences <- readRDS("spatial_occurrences.rds") 

# This is the 11-class RdBu colorbrewer diverging colour scheme. I'm happy to
# look at alternative schemes.
prediction_colours <-  c(
  "Predicted_present_1" = "#67001f",
  "Predicted_present_0.9" = "#b2182b",
  "Predicted_present_0.8" = "#d6604d",
  "Predicted_present_0.7" = "#f4a582",
  "Predicted_present_0.6" = "#fddbc7",
  "Equivocal" = "#f7f7f7",
  "Predicted_absent_0.6" = "#d1e5f0",
  "Predicted_absent_0.7" = "#92c5de",
  "Predicted_absent_0.8" = "#4393c3",
  "Predicted_absent_0.9" = "#2166ac",
  "Predicted_absent_1" = "#053061"
) 

# This vector helps with plotting
plot_limits_breaks <-  c(
  "Predicted_present_1",
  "Predicted_present_0.9",
  "Predicted_present_0.8",
  "Predicted_present_0.7",
  "Predicted_present_0.6",
  "Equivocal",
  "Predicted_absent_0.6",
  "Predicted_absent_0.7",
  "Predicted_absent_0.8",
  "Predicted_absent_0.9",
  "Predicted_absent_1"
) 

d <- readRDS("presence_model_predictions_points.rds")
d <- d %>% pivot_wider(names_from=scientificName, values_from=c(pred_class, pred_prob_median))
d <- d %>% select(matches('pred_class|decimal'))
d <- d %>%
  rename_with(~ str_remove(.x, "pred_class_"), contains("pred_class_"))

names(d)

spatial_predictions_centroids <- st_centroid(spatial_predictions)

species_columns <- names(d)[!names(d) %in% c("decimalLatitude", "decimalLongitude")]

factor_levels_list <- list()
for(species in species_columns) {
  factor_levels_list[[species]] <- plot_limits_breaks
}

# Simple Observable code - shows average prediction value for each species
histogram_code <- "
Plot.plot({
  marks: [
    Plot.barX(
      Object.keys(data[0])
        .filter(key => key !== 'lat' && key !== 'lng' && key !== 'value')
        .map(species => ({
          species: species.replace(/[_]/g, ' '),
          average: data.reduce((sum, d) => sum + (d[species] || 0), 0) / data.length
        }))
        .sort((a, b) => b.average - a.average),
      {y: 'species', x: 'average', fill: 'steelblue'}
    )
  ],
  y: {
    label: 'Species',
    domain: Object.keys(data[0])
      .filter(key => key !== 'lat' && key !== 'lng' && key !== 'value')
      .map(species => ({
        species: species.replace(/[_]/g, ' '),
        average: data.reduce((sum, d) => sum + (d[species] || 0), 0) / data.length
      }))
      .sort((a, b) => b.average - a.average)
      .map(d => d.species)
  },
  x: {
    label: 'Average Prediction Value',
    grid: true
  },
  title: 'Average Prediction Values by Species',
  width: 800,
  height: 600,
  marginLeft: 200
})
"

predictions_tab <- spacetimeview(
  data = d, 
  style = 'Summary',
  summary_radius = 7000,
  summary_height = 10,
  visible_controls = c('column_to_plot', 'enable_clicked_tooltips'),
  control_names = c(
    column_to_plot = 'Select a beetle species',
    enable_clicked_tooltips = 'Enable Clickable Charts'
  ),
  observable = histogram_code,
  factor_levels = factor_levels_list,
  factor_icons = list(
    # "All" = "public/beetle_images/Bubas_bison.jpg",
    "Bubas bison" = "public/beetle_images/Bubas_bison.jpg",
    "Bubas bubalus" = "public/beetle_images/Bubas_bubalus.jpg",
    "Copris elphenor" = "public/beetle_images/Copris_elphenor.jpg",
    "Copris hispanus" = "public/beetle_images/Copris_hispanus.jpg",
    "Digitonthophagus gazella" = "public/beetle_images/Digitonthophagus_gazella.jpg",
    "Euoniticellus africanus" = "public/beetle_images/Euoniticellus_africanus.jpg",
    "Euoniticellus fulvus" = "public/beetle_images/Euoniticellus_fulvus.jpg",
    "Euoniticellus intermedius" = "public/beetle_images/Euoniticellus_intermedius.jpg",
    "Euoniticellus pallipes" = "public/beetle_images/Euoniticellus_pallipes.jpg",
    "Geotrupes spiniger" = "public/beetle_images/Geotrupes_spiniger.jpg",
    "Liatongus militaris" = "public/beetle_images/Liatongus_militaris.jpg",
    "Onitis alexis" = "public/beetle_images/Onitis_alexis.jpg",
    "Onitis aygulus" = "public/beetle_images/Onitis_aygulus.jpg",
    "Onitis caffer" = "public/beetle_images/Onitis_caffer.jpg",
    "Onitis pecuarius" = "public/beetle_images/Onitis_pecuarius.jpg",
    "Onitis vanderkelleni" = "public/beetle_images/Onitis_vanderkelleni.jpg",
    "Onitis viridulus" = "public/beetle_images/Onitis_viridulus.jpg",
    "Onthophagus binodis" = "public/beetle_images/Onthophagus_binodis.jpg",
    "Onthophagus nigriventris" = "public/beetle_images/Onthophagus_nigriventris.jpg",
    "Onthophagus obliquus" = "public/beetle_images/Onthophagus_obliquus.jpg",
    "Onthophagus sagittarius" = "public/beetle_images/Onthophagus_sagittarius.jpg",
    "Onthophagus taurus" = "public/beetle_images/Onthophagus_taurus.jpg",
    "Onthophagus vacca" = "public/beetle_images/Onthophagus_vacca.jpg",
    "Sisyphus rubrus" = "public/beetle_images/Sisyphus_rubrus.jpg",
    "Sisyphus spinipes" = "public/beetle_images/Sisyphus_spinipes.jpg"
  ),
  factor_colors = list(
    "Bubas bison" = prediction_colours,
    "Bubas bubalus" = prediction_colours,
    "Copris elphenor" = prediction_colours,
    "Copris hispanus" = prediction_colours,
    "Digitonthophagus gazella" = prediction_colours,
    "Euoniticellus africanus" = prediction_colours,
    "Euoniticellus fulvus" = prediction_colours,
    "Euoniticellus intermedius" = prediction_colours,
    "Euoniticellus pallipes" = prediction_colours,
    "Geotrupes spiniger" = prediction_colours,
    "Liatongus militaris" = prediction_colours,
    "Onitis alexis" = prediction_colours,
    "Onitis aygulus" = prediction_colours,
    "Onitis caffer" = prediction_colours,
    "Onitis pecuarius" = prediction_colours,
    "Onitis vanderkelleni" = prediction_colours,
    "Onitis viridulus" = prediction_colours,
    "Onthophagus binodis" = prediction_colours,
    "Onthophagus nigriventris" = prediction_colours,
    "Onthophagus obliquus" = prediction_colours,
    "Onthophagus sagittarius" = prediction_colours,
    "Onthophagus taurus" = prediction_colours,
    "Onthophagus vacca" = prediction_colours,
    "Sisyphus rubrus" = prediction_colours,
    "Sisyphus spinipes" = prediction_colours
  ),
  # CRITICAL: Enable clicked tooltips to show Observable plots
  enableClickedTooltips = TRUE
)

occurrence_tab <- spacetimeview(
  data = spatial_occurrences, 
  style = 'Scatter',
)

dashboard <- predictions_tab + occurrence_tab + occurrence_tab

names(dashboard) <- c('Predicted Occurrence', 'Available resources', 'Occurrences')

plot(dashboard)

save(dashboard, "my_dashboard.html")

# INSTRUCTIONS FOR USING OBSERVABLE PLOTS:
# 1. Observable plots only appear in TOOLTIPS, not in the main interface
# 2. You must set enableClickedTooltips = TRUE 
# 3. You must add 'enable_clicked_tooltips' to visible_controls to show the toggle
# 4. Click on hexagons/grid cells in the Summary view to see the Observable plot
# 5. The Observable code should be a simple expression that returns Plot.plot(...)
# 6. The 'data' variable contains the filtered data for the clicked area