library(tidyverse)
library(sf)
library(dungfaunaR)

# library(spacetimeview)
devtools::load_all('../spacetimeview')

data('dungfauna_occurrence')
# select the columns we need
dungfauna_occurrence <- dungfauna_occurrence[
  ,
  c(
    'decimalLatitude', 
    'decimalLongitude',
    'eventDate_collect',
    'scientificName', 
    'individualCount', 
    'occurrenceStatus', 
    'locationID_site', 
    'county'
  )
]

# rename the columns
colnames(dungfauna_occurrence) <- c(
  'Latitude', 
  'Longitude',
  'Date',
  'Species',
  'Abundance',
  'Status',
  'Site',
  'County'
)

# load polygon data
# Optional but recommended for better GeoJSON conversion
if (!requireNamespace("geojsonsf", quietly = TRUE)) {
  install.packages("geojsonsf")
  library(geojsonsf)
} else {
  library(geojsonsf)
}

# Download Australian states polygons
# For this example, we'll use the rnaturalearth package to get the Australian states
if (!requireNamespace("rnaturalearth", quietly = TRUE)) {
  install.packages("rnaturalearth")
}
if (!requireNamespace("rnaturalearthdata", quietly = TRUE)) {
  install.packages("rnaturalearthdata")
}

# Get Australia states
aus_states <- rnaturalearth::ne_states(country = "australia", returnclass = "sf")

# now make your dashboard with one function call
occurrence_p <- spacetimeview(
  dungfauna_occurrence,
  time_column_name = 'Date',
  column_to_plot = 'Abundance',
  color_scheme = 'Reds',
  summary_radius = 20000,
  animation_speed = 10,
  summary_height = 100,
  header_title='Dung Beetles of Australia',
  social_links=c('github'='https://github.com/jakemanger/spacetimeview_dungbeetles'),
  filter_column='Species',
  factor_icons=list(
    Species = list(
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
    )
  ),
  # polygons = aus_states,
  # visible_controls = c('aggregate', 'filter'),
  # control_names = c('aggregate' = 'Metric'),
  menu_text = 'Click on the map to see measurements from traps, or select a species to view where we have found them 👇',
  initial_latitude = -25.007754997248703, 
  initial_longitude = 134.35406022625756,
  initial_zoom = 3
)




spatial_predictions <- readRDS("spatial_predictions.rds")

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

# reverse the factor levels so that 1 = Predicted_absent_1 and 11 = Predicted_present_1
reversed_plot_limits_breaks <- rev(plot_limits_breaks)

species_cols <- names(d)[!names(d) %in% c("decimalLatitude", "decimalLongitude")]
for(col in species_cols) {
  d[[col]] <- factor(d[[col]], levels = reversed_plot_limits_breaks)
}

names(d)

spatial_predictions_centroids <- st_centroid(spatial_predictions)

species_columns <- names(d)[!names(d) %in% c("decimalLatitude", "decimalLongitude")]

factor_levels_list <- list()
for(species in species_columns) {
  factor_levels_list[[species]] <- plot_limits_breaks
}

# observable code with beetle icons - fixed to avoid const declarations
histogram_code <- "
Plot.plot({
  marks: [
    // Lollipop sticks (vertical lines from axis to dot)
    Plot.link(
      Object.keys(data[0])
        .filter(key => key !== 'lat' && key !== 'lng' && key !== 'value')
        .map(species => {
          var cleanName = species.replace(/[_]/g, ' ');
          var average = data.reduce((sum, d) => sum + (d[species] || 0), 0) / data.length;
          var iconUrl = factorIcons && factorIcons[species] ? factorIcons[species] : null;
          var colorKey = 'Predicted_absent_1';
          if (average >= 9) colorKey = 'Predicted_present_1';
          else if (average >= 8) colorKey = 'Predicted_present_0.9';
          else if (average >= 7) colorKey = 'Predicted_present_0.8';
          else if (average >= 6) colorKey = 'Predicted_present_0.7';
          else if (average >= 5.5) colorKey = 'Predicted_present_0.6';
          else if (average >= 4.5) colorKey = 'Equivocal';
          else if (average >= 3.5) colorKey = 'Predicted_absent_0.6';
          else if (average >= 2.5) colorKey = 'Predicted_absent_0.7';
          else if (average >= 1.5) colorKey = 'Predicted_absent_0.8';
          else if (average >= 0.5) colorKey = 'Predicted_absent_0.9';
          return { 
            species: cleanName,
            originalSpecies: species,
            average: average,
            iconUrl: iconUrl,
            color: colorKey
          };
        })
        .sort((a, b) => b.average - a.average),
      {
        y1: 'species',
        y2: 'species',
        x1: 0,
        x2: 'average',
        stroke: 'color',
        strokeWidth: 2
      }
    ),
    // Lollipop dots
    Plot.dot(
      Object.keys(data[0])
        .filter(key => key !== 'lat' && key !== 'lng' && key !== 'value')
        .map(species => {
          var cleanName = species.replace(/[_]/g, ' ');
          var average = data.reduce((sum, d) => sum + (d[species] || 0), 0) / data.length;
          var iconUrl = factorIcons && factorIcons[species] ? factorIcons[species] : null;
          var colorKey = 'Predicted_absent_1';
          if (average >= 9) colorKey = 'Predicted_present_1';
          else if (average >= 8) colorKey = 'Predicted_present_0.9';
          else if (average >= 7) colorKey = 'Predicted_present_0.8';
          else if (average >= 6) colorKey = 'Predicted_present_0.7';
          else if (average >= 5.5) colorKey = 'Predicted_present_0.6';
          else if (average >= 4.5) colorKey = 'Equivocal';
          else if (average >= 3.5) colorKey = 'Predicted_absent_0.6';
          else if (average >= 2.5) colorKey = 'Predicted_absent_0.7';
          else if (average >= 1.5) colorKey = 'Predicted_absent_0.8';
          else if (average >= 0.5) colorKey = 'Predicted_absent_0.9';
          return { 
            species: cleanName,
            originalSpecies: species,
            average: average,
            iconUrl: iconUrl,
            color: colorKey
          };
        })
        .sort((a, b) => b.average - a.average),
      {
        y: 'species', 
        x: 'average', 
        fill: 'color',
        r: 4,
        title: d => d.species + ': ' + d.average.toFixed(3)
      }
    ),
         // Icons positioned to the left of the y-axis
     Plot.image(
       Object.keys(data[0])
         .filter(key => key !== 'lat' && key !== 'lng' && key !== 'value')
         .map(species => {
           var cleanName = species.replace(/[_]/g, ' ');
           var average = data.reduce((sum, d) => sum + (d[species] || 0), 0) / data.length;
           var iconUrl = factorIcons && factorIcons[species] ? factorIcons[species] : null;
           return { 
             species: cleanName,
             originalSpecies: species,
             average: average,
             iconUrl: iconUrl
           };
         })
         .filter(d => d.iconUrl)
         .sort((a, b) => b.average - a.average),
       {
         y: 'species',
         x: -1.0,
         src: 'iconUrl',
         width: 20,
         height: 20,
         title: d => d.species + ' icon'
       }
     ),
         // Beetle emoji for species without icons
     Plot.text(
       Object.keys(data[0])
         .filter(key => key !== 'lat' && key !== 'lng' && key !== 'value')
         .map(species => {
           var cleanName = species.replace(/[_]/g, ' ');
           var average = data.reduce((sum, d) => sum + (d[species] || 0), 0) / data.length;
           var iconUrl = factorIcons && factorIcons[species] ? factorIcons[species] : null;
           return { 
             species: cleanName,
             originalSpecies: species,
             average: average,
             iconUrl: iconUrl
           };
         })
        .filter(d => !d.iconUrl)
        .sort((a, b) => b.average - a.average),
      {
        y: 'species',
        x: -2.0,
        text: '🪲',
                 fontSize: 7,
        fill: '#666',
        title: d => d.species + ' (no icon available)'
      }
    )
  ],
  y: {
    label: '',
    fontSize: 19,
    tickPadding: 20,
    domain: Object.keys(data[0])
      .filter(key => key !== 'lat' && key !== 'lng' && key !== 'value')
      .map(species => {
        var cleanName = species.replace(/[_]/g, ' ');
        return {
          species: cleanName,
          average: data.reduce((sum, d) => sum + (d[species] || 0), 0) / data.length
        };
      })
      .sort((a, b) => b.average - a.average)
      .map(d => d.species),
    tickFormat: function(d) {
      var parts = d.split(' ');
      return parts.length >= 2 ? parts[0].charAt(0) + '. ' + parts[1] : d;
    }
  },
  x: {
    label: 'Likelihood',
    fontSize: 12,
    grid: true,
    domain: [-0.8, Math.max(...Object.keys(data[0])
      .filter(key => key !== 'lat' && key !== 'lng' && key !== 'value')
      .map(species => data.reduce((sum, d) => sum + (d[species] || 0), 0) / data.length)) * 1.1],
    ticks: [0, 3, 5, 7, 10],
    tickFormat: function(d) {
      if (d === 0) return 'Absent';
      if (d === 3) return '';
      if (d === 5) return 'Unsure';
      if (d === 7) return '';
      if (d === 10) return 'Present';
      return d;
    }
  },
  color: {
    range: ['#053061', '#2166ac', '#4393c3', '#92c5de', '#d1e5f0', '#f7f7f7', '#fddbc7', '#f4a582', '#d6604d', '#b2182b', '#67001f'],
    domain: ['Predicted_absent_1', 'Predicted_absent_0.9', 'Predicted_absent_0.8', 'Predicted_absent_0.7', 'Predicted_absent_0.6', 'Equivocal', 'Predicted_present_0.6', 'Predicted_present_0.7', 'Predicted_present_0.8', 'Predicted_present_0.9', 'Predicted_present_1']
  },
  style: {
    fontSize: '12px'
  },
  title: 'Beetles found at ' + data[0].lat.toFixed(2) + ', ' + data[0].lng.toFixed(2),
  width: 300,
  height: 390,
  marginLeft: 110,
  marginBottom: 40
})
"


predictions_tab <- spacetimeview(
  data = d, 
  style = 'Summary',
  summary_radius = 7000,
  summary_height = 1,
  visible_controls = c('column_to_plot'),
  control_names = c(
    column_to_plot = 'Select a beetle species'
  ),
  observable = histogram_code,
  factor_levels = factor_levels_list,
  legend_order = c(10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0),
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
  country_codes = 'AU',
  header_title = "Dung Beetles of Australia",
  social_links = c('github'='https://github.com/jakemanger/spacetimeview_dungbeetles'),
  menu_text = 'Click on the map to see beetles found there, or select a species to view its range 👇',
  initial_latitude = -25.007754997248703, 
  initial_longitude = 134.35406022625756,
  initial_zoom = 3
)


dashboard <- predictions_tab + occurrence_p

names(dashboard) <- c('Predictions', 'Occurrences')

plot(dashboard)

plt <- dashboard