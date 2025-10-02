library(tidyverse)
library(sf)
library(dungfaunaR)

devtools::load_all('../spacetimeview')

# configuration
json_digits <- 2

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

d <- readRDS("presence_model_predictions_points_new.rds")
d <- d %>%
  select(contains("pred_class_") | contains("pred_prob_0.95_lower_") | contains("pred_prob_0.95_upper_") | pred_richness_median | decimalLatitude | decimalLongitude) %>%
  rename_with(~ str_remove(.x, "pred_class_"), contains("pred_class_")) %>%
  rename_with(~ paste0(str_remove(.x, "pred_prob_0.95_lower_"), "_lower"), contains("pred_prob_0.95_lower_")) %>%
  rename_with(~ paste0(str_remove(.x, "pred_prob_0.95_upper_"), "_upper"), contains("pred_prob_0.95_upper_")) %>%
  rename("Number of species" = pred_richness_median)

reversed_plot_limits_breaks <- rev(plot_limits_breaks)

species_cols <- names(d)[!names(d) %in% c("decimalLatitude", "decimalLongitude", "Number of species") & !str_detect(names(d), "_lower|_upper")]
for(col in species_cols) {
  d[[col]] <- factor(d[[col]], levels = reversed_plot_limits_breaks)
}

names(d)

species_columns <- names(d)[!names(d) %in% c("decimalLatitude", "decimalLongitude", "Number of species") & !str_detect(names(d), "_lower|_upper")]

factor_levels_list <- list()
for(species in species_columns) {
  factor_levels_list[[species]] <- plot_limits_breaks
}



histogram_code <- "
Plot.plot({
  marks: [
    Plot.link(
      Object.keys(data[0])
        .filter(key => key !== 'lat' && key !== 'lng' && key !== 'value' && key !== 'Number of species' && !key.includes('No. of') && !key.includes('species') && !key.includes('_lower') && !key.includes('_upper'))
        .map(species => {
          var cleanName = species.replace(/[_]/g, ' ');
          var average = data.reduce((sum, d) => sum + (d[species] || 0), 0) / data.length;

          var lowerKey = species + '_lower';
          var upperKey = species + '_upper';
          var lowerAverage = data.reduce((sum, d) => sum + (d[lowerKey] || 0), 0) / data.length * 10;
          var upperAverage = data.reduce((sum, d) => sum + (d[upperKey] || 0), 0) / data.length * 10;
          
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
            lowerBound: lowerAverage,
            upperBound: upperAverage,
            iconUrl: iconUrl,
            color: colorKey
          };
        })
        .sort((a, b) => b.average - a.average),
      {
        y1: 'species',
        y2: 'species',
        x1: 'lowerBound',
        x2: 'upperBound',
        stroke: 'color',
        strokeWidth: 2
      }
    ),
    Plot.dot(
      Object.keys(data[0])
        .filter(key => key !== 'lat' && key !== 'lng' && key !== 'value' && key !== 'Number of species' && !key.includes('richness') && !key.includes('_lower') && !key.includes('_upper'))
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
     Plot.image(
       Object.keys(data[0])
         .filter(key => key !== 'lat' && key !== 'lng' && key !== 'value' && key !== 'Number of species' && !key.includes('_lower') && !key.includes('_upper'))
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
         width: 18,
         height: 18,
         title: d => d.species + ' icon'
       }
     ),
     Plot.text(
       Object.keys(data[0])
         .filter(key => key !== 'lat' && key !== 'lng' && key !== 'value' && key !== 'Number of species' && !key.includes('_lower') && !key.includes('_upper'))
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
    tickPadding: 15,
    domain: Object.keys(data[0])
      .filter(key => key !== 'lat' && key !== 'lng' && key !== 'value' && key !== 'Number of species' && !key.includes('_lower') && !key.includes('_upper'))
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
      .filter(key => key !== 'lat' && key !== 'lng' && key !== 'value' && key !== 'Number of species' && !key.includes('_lower') && !key.includes('_upper'))
      .map(species => data.reduce((sum, d) => sum + (d[species] || 0), 0) / data.length)) * 1.1],
    ticks: [0, 5, 10],
    tickFormat: function(d) {
      if (d === 0) return 'Absent';
      if (d === 5) return 'Unsure';
      if (d === 10) return 'Present';
      return d;
    }
  },
  color: {
    range: ['#053061', '#2166ac', '#4393c3', '#92c5de', '#d1e5f0', '#e3e3e3', '#fddbc7', '#f4a582', '#d6604d', '#b2182b', '#67001f'],
    domain: ['Predicted_absent_1', 'Predicted_absent_0.9', 'Predicted_absent_0.8', 'Predicted_absent_0.7', 'Predicted_absent_0.6', 'Equivocal', 'Predicted_present_0.6', 'Predicted_present_0.7', 'Predicted_present_0.8', 'Predicted_present_0.9', 'Predicted_present_1']
  },
  style: {
    fontSize: '12px'
  },
  title: 'Beetles predicted to be found at ' + data[0].lat.toFixed(2) + ', ' + data[0].lng.toFixed(2),
  width: 300,
  height: 380,
  marginBottom: 40,
  marginLeft: 120,
  marginRight: 40
})
"

legend_remapping <- c(
  "Predicted_present_1" = "Present", "Predicted_present_0.9" = "", 
  "Predicted_present_0.8" = "", "Predicted_present_0.7" = "", 
  "Predicted_present_0.6" = "", "Equivocal" = "Unsure", 
  "Predicted_absent_0.6" = "", "Predicted_absent_0.7" = "", 
  "Predicted_absent_0.8" = "", "Predicted_absent_0.9" = "", 
  "Predicted_absent_1" = "Absent"
)

d <- d %>% 
  select(`Number of species`, everything())

legend_order <- c(10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0)


about_text = "
  <h2>About This Dataset</h2>
  <p>The data maps show where dung beetles are known to be found in Australia over the 12 months of the year, based on field sampling. The predictions show the likelihood of finding a dung beetle at a given location across Australia, based on modelling from sampling records.</p>
  <p>These data were collected and analysed by the University of Western Australia on behalf of Meat and Livestock Australia.</p>
  <p>Data curation and modelling was led by <a href='https://research-repository.uwa.edu.au/en/persons/jacob-berson'>Dr Jacob Berson</a> and <a href='https://research-repository.uwa.edu.au/en/persons/theo-evans'>Associate Professor Theo Evans</a>. The website was created by <a href='https://github.com/jakemanger'>Jake Manger</a>.</p>
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
  json_digits = json_digits,
  observable = histogram_code,
  factor_levels = factor_levels_list,
  legend_order = list(
    "Bubas bison" = legend_order,
    "Bubas bubalus" = legend_order,
    "Copris elphenor" = legend_order,
    "Copris hispanus" = legend_order,
    "Digitonthophagus gazella" = legend_order,
    "Euoniticellus africanus" = legend_order,
    "Euoniticellus fulvus" = legend_order,
    "Euoniticellus intermedius" = legend_order,
    "Euoniticellus pallipes" = legend_order,
    "Geotrupes spiniger" = legend_order,
    "Liatongus militaris" = legend_order,
    "Onitis alexis" = legend_order,
    "Onitis aygulus" = legend_order,
    "Onitis caffer" = legend_order,
    "Onitis pecuarius" = legend_order,
    "Onitis vanderkelleni" = legend_order,
    "Onitis viridulus" = legend_order,
    "Onthophagus binodis" = legend_order,
    "Onthophagus nigriventris" = legend_order,
    "Onthophagus obliquus" = legend_order,
    "Onthophagus sagittarius" = legend_order,
    "Onthophagus taurus" = legend_order,
    "Onthophagus vacca" = legend_order,
    "Sisyphus rubrus" = legend_order,
    "Sisyphus spinipes" = legend_order
  ),
  legend_labels = list(
    "Bubas bison" = legend_remapping,
    "Bubas bubalus" = legend_remapping,
    "Copris elphenor" = legend_remapping,
    "Copris hispanus" = legend_remapping,
    "Digitonthophagus gazella" = legend_remapping,
    "Euoniticellus africanus" = legend_remapping,
    "Euoniticellus fulvus" = legend_remapping,
    "Euoniticellus intermedius" = legend_remapping,
    "Euoniticellus pallipes" = legend_remapping,
    "Geotrupes spiniger" = legend_remapping,
    "Liatongus militaris" = legend_remapping,
    "Onitis alexis" = legend_remapping,
    "Onitis aygulus" = legend_remapping,
    "Onitis caffer" = legend_remapping,
    "Onitis pecuarius" = legend_remapping,
    "Onitis vanderkelleni" = legend_remapping,
    "Onitis viridulus" = legend_remapping,
    "Onthophagus binodis" = legend_remapping,
    "Onthophagus nigriventris" = legend_remapping,
    "Onthophagus obliquus" = legend_remapping,
    "Onthophagus sagittarius" = legend_remapping,
    "Onthophagus taurus" = legend_remapping,
    "Onthophagus vacca" = legend_remapping,
    "Sisyphus rubrus" = legend_remapping,
    "Sisyphus spinipes" = legend_remapping
  ),
  factor_icons = list(
    "Number of species" = "public/beetle_images/Species_richness.jpg",
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
  selectable_columns = c(
   "Number of species",
   "Bubas bison",
   "Copris elphenor",
   "Copris hispanus",
   "Digitonthophagus gazella",
   "Euoniticellus africanus",
   "Euoniticellus fulvus",
   "Euoniticellus intermedius",
   "Euoniticellus pallipes",
   "Geotrupes spiniger",
   "Liatongus militaris",
   "Onitis alexis",
   "Onitis aygulus",
   "Onitis caffer",
   "Onitis pecuarius",
   "Onitis vanderkelleni",
   "Onitis viridulus",
   "Onthophagus binodis",
   "Onthophagus nigriventris",
   "Onthophagus obliquus",
   "Onthophagus sagittarius",
   "Onthophagus taurus",
   "Sisyphus rubrus",
   "Sisyphus spinipes"
  ),
  country_codes = 'AU',
  header_title = "Dung Beetles of Australia",
  social_links = c('github'='https://github.com/jakemanger/spacetimeview_dungbeetles'),
  menu_text = 'Click on the coloured portions of the map to see what beetles are predicted to be found there\n\nOr select a species from the dropdown menu to view its range 👇',
  initial_latitude = -27.007754997248703, 
  initial_longitude = 134.35406022625756,
  initial_zoom = 4,
  legend_direction_text = c("Bubas bison" = "Likelihood", "Bubas bubalus" = "Likelihood", "Copris elphenor" = "Likelihood", "Copris hispanus" = "Likelihood", "Digitonthophagus gazella" = "Likelihood", "Euoniticellus africanus" = "Likelihood", "Euoniticellus fulvus" = "Likelihood", "Euoniticellus intermedius" = "Likelihood", "Euoniticellus pallipes" = "Likelihood", "Geotrupes spiniger" = "Likelihood", "Liatongus militaris" = "Likelihood", "Onitis alexis" = "Likelihood", "Onitis aygulus" = "Likelihood", "Onitis caffer" = "Likelihood", "Onitis pecuarius" = "Likelihood", "Onitis vanderkelleni" = "Likelihood", "Onitis viridulus" = "Likelihood", "Onthophagus binodis" = "Likelihood", "Onthophagus nigriventris" = "Likelihood", "Onthophagus obliquus" = "Likelihood", "Onthophagus sagittarius" = "Likelihood", "Onthophagus taurus" = "Likelihood", "Onthophagus vacca" = "Likelihood", "Sisyphus rubrus" = "Likelihood", "Sisyphus spinipes" = "Likelihood"),
  about_text = about_text
)

occurrence_d_raw <- readRDS("presence_model_predictions_points_new.rds") %>%
  select(decimalLatitude, decimalLongitude, starts_with("occurrenceStatus_")) %>%
  select(-contains("richness"), -contains("pred_richness")) %>%
  rename_with(~ str_remove(.x, "occurrenceStatus_"), starts_with("occurrenceStatus_")) %>%
  mutate(across(-c(decimalLatitude, decimalLongitude), ~ case_when(
    .x == "Found" ~ factor("Found", levels = c("Found", "Not found")),
    .x == "Not found" ~ factor("Not found", levels = c("Found", "Not found")),
    TRUE ~ NA_character_
  ))) %>%
  mutate(across(-c(decimalLatitude, decimalLongitude), ~ factor(.x, levels = c("Found", "Not found"))))

# filter out rows where all species columns are NA (98% of rows)
occurrence_d <- occurrence_d_raw %>%
  filter(rowSums(!is.na(select(., -decimalLatitude, -decimalLongitude))) > 0)

cat(sprintf("Occurrence data: filtered from %s to %s rows (removed %s%% empty rows)\n",
            format(nrow(occurrence_d_raw), big.mark=","),
            format(nrow(occurrence_d), big.mark=","),
            round(100 * (1 - nrow(occurrence_d) / nrow(occurrence_d_raw)), 1)))

occurrence_colours <- c(
  "Found" = "#e74c3c",
  "Not found" = "#fbf8f3"
)

occurrence_factor_levels_list <- list()
species_columns_occ <- names(occurrence_d)[!names(occurrence_d) %in% c("decimalLatitude", "decimalLongitude")]
for(species in species_columns_occ) {
  occurrence_factor_levels_list[[species]] <- c("Found", "Not found")
}

occurrence_histogram_code <- "
Plot.plot({
  marks: [
    Plot.barX(
      Object.keys(data[0])
        .filter(key => key !== 'lat' && key !== 'lng' && key !== 'value' && key !== 'Number of species' && !key.includes('richness'))
        .map(species => {
          var cleanName = species.replace(/[_]/g, ' ');
          
          var foundCount = data.filter(d => {
            var val = d[species];
            return val == 0
          }).length;
          var notFoundCount = data.filter(d => {
            var val = d[species];
            return val == 1
          }).length;
          
          var totalSurveyed = foundCount + notFoundCount;
          var status = foundCount > 0 ? 'Present' : (totalSurveyed > 0 ? 'Absent' : 'No data');
          var iconUrl = factorIcons && factorIcons[species] ? factorIcons[species] : null;
          
          return { 
            species: cleanName,
            originalSpecies: species,
            status: status,
            foundCount: foundCount,
            notFoundCount: notFoundCount,
            totalSurveyed: totalSurveyed,
            iconUrl: iconUrl,
            color: status === 'Present' ? '#e74c3c' : (status === 'Absent' ? '#3498db' : '#cccccc'),
            value: status === 'Present' ? 1 : (status === 'Absent' ? 0.5 : 0.05)
          };
        })
        .sort((a, b) => {
          if (a.status === 'Present' && b.status !== 'Present') return -1;
          if (b.status === 'Present' && a.status !== 'Present') return 1;
          if (a.status === 'Absent' && b.status === 'No data') return -1;
          if (b.status === 'Absent' && a.status === 'No data') return 1;
          return 0;
        }),
      {
        y: 'species',
        x: 'value',
        fill: 'color',
        title: d => d.species + ': ' + d.status + 
                   (d.totalSurveyed > 0 ? ' (' + d.foundCount + ' found, ' + d.notFoundCount + ' not found)' : ' (no survey data)')
      }
    ),
    Plot.image(
      Object.keys(data[0])
        .filter(key => key !== 'lat' && key !== 'lng' && key !== 'value' && key !== 'Number of species' && !key.includes('richness'))
        .map(species => {
          var cleanName = species.replace(/[_]/g, ' ');
          
          var foundCount = data.filter(d => {
            var val = d[species];
            return val == 0
          }).length;
          var notFoundCount = data.filter(d => {
            var val = d[species];
            return val == 1
          }).length;
          
          var totalSurveyed = foundCount + notFoundCount;
          var status = foundCount > 0 ? 'Present' : (totalSurveyed > 0 ? 'Absent' : 'No data');
          var iconUrl = factorIcons && factorIcons[species] ? factorIcons[species] : null;
          return { 
            species: cleanName,
            status: status,
            iconUrl: iconUrl
          };
        })
        .filter(d => d.iconUrl)
        .sort((a, b) => {
          if (a.status === 'Present' && b.status !== 'Present') return -1;
          if (b.status === 'Present' && a.status !== 'Present') return 1;
          if (a.status === 'Absent' && b.status === 'No data') return -1;
          if (b.status === 'Absent' && a.status === 'No data') return 1;
          return 0;
        }),
      {
        y: 'species',
        x: -0.05,
        src: 'iconUrl',
        width: 18,
        height: 18,
        title: d => d.species + ' icon'
      }
    ),
    Plot.text(
      Object.keys(data[0])
        .filter(key => key !== 'lat' && key !== 'lng' && key !== 'value' && key !== 'Number of species' && !key.includes('richness'))
        .map(species => {
          var cleanName = species.replace(/[_]/g, ' ');
          
          var foundCount = data.filter(d => {
            var val = d[species];
            return val == 0
          }).length;
          var notFoundCount = data.filter(d => {
            var val = d[species];
            return val == 1
          }).length;
          
          var totalSurveyed = foundCount + notFoundCount;
          var status = foundCount > 0 ? 'Present' : (totalSurveyed > 0 ? 'Absent' : 'No data');
          var iconUrl = factorIcons && factorIcons[species] ? factorIcons[species] : null;
          return { 
            species: cleanName,
            status: status,
            iconUrl: iconUrl
          };
        })
        .filter(d => !d.iconUrl)
        .sort((a, b) => {
          if (a.status === 'Present' && b.status !== 'Present') return -1;
          if (b.status === 'Present' && a.status !== 'Present') return 1;
          if (a.status === 'Absent' && b.status === 'No data') return -1;
          if (b.status === 'Absent' && a.status === 'No data') return 1;
          return 0;
        }),
      {
        y: 'species',
        x: -0.1,
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
      .filter(key => key !== 'lat' && key !== 'lng' && key !== 'value' && key !== 'Number of species')
      .map(species => {
        var cleanName = species.replace(/[_]/g, ' ');
        
        var foundCount = data.filter(d => {
          var val = d[species];
          return val == 0
        }).length;
        var notFoundCount = data.filter(d => {
          var val = d[species];
          return val == 1
        }).length;
        
        var totalSurveyed = foundCount + notFoundCount;
        var status = foundCount > 0 ? 'Present' : (totalSurveyed > 0 ? 'Absent' : 'No data');
        return {
          species: cleanName,
          status: status
        };
      })
      .sort((a, b) => {
        if (a.status === 'Present' && b.status !== 'Present') return -1;
        if (b.status === 'Present' && a.status !== 'Present') return 1;
        if (a.status === 'Absent' && b.status === 'No data') return -1;
        if (b.status === 'Absent' && a.status === 'No data') return 1;
        return 0;
      })
      .map(d => d.species),
    tickFormat: function(d) {
      var parts = d.split(' ');
      return parts.length >= 2 ? parts[0].charAt(0) + '. ' + parts[1] : d;
    }
  },
  x: {
    label: '',
    fontSize: 12,
    grid: false,
    domain: [0, 1],
    ticks: [0.05, 1],
    tickFormat: function(d) {
      if (d === 0.05) return 'Not Found';
      if (d === 1) return 'Found';
      return '';
    }
  },
  style: {
    fontSize: '12px'
  },
  title: 'Beetles found at ' + data[0].lat.toFixed(2) + ', ' + data[0].lng.toFixed(2),
  width: 300,
  height: 390,
  marginLeft: 120,
  marginBottom: 40,
})
"


occurrence_tab <- spacetimeview(
  data = occurrence_d,
  style = 'Summary',
  summary_radius = 7000,
  summary_height = 1,
  visible_controls = c('column_to_plot'),
  control_names = c(
    column_to_plot = 'Select a beetle species'
  ),
  json_digits = json_digits,
  observable = occurrence_histogram_code,
  factor_levels = occurrence_factor_levels_list,
  factor_icons = list(
    "Bubas bison" = "public/beetle_images/Bubas_bison.jpg",
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
    "Sisyphus rubrus" = "public/beetle_images/Sisyphus_rubrus.jpg",
    "Sisyphus spinipes" = "public/beetle_images/Sisyphus_spinipes.jpg"
  ),
  factor_colors = list(
    "Bubas bison" = occurrence_colours,
    "Digitonthophagus gazella" = occurrence_colours,
    "Euoniticellus africanus" = occurrence_colours,
    "Euoniticellus intermedius" = occurrence_colours,
    "Euoniticellus pallipes" = occurrence_colours,
    "Geotrupes spiniger" = occurrence_colours,
    "Liatongus militaris" = occurrence_colours,
    "Onitis alexis" = occurrence_colours,
    "Onitis aygulus" = occurrence_colours,
    "Onitis caffer" = occurrence_colours,
    "Onitis pecuarius" = occurrence_colours,
    "Onitis viridulus" = occurrence_colours,
    "Onthophagus nigriventris" = occurrence_colours,
    "Onthophagus sagittarius" = occurrence_colours,
    "Onthophagus taurus" = occurrence_colours,
    "Sisyphus rubrus" = occurrence_colours,
    "Sisyphus spinipes" = occurrence_colours,
    "Onthophagus binodis" = occurrence_colours,
    "Euoniticellus fulvus" = occurrence_colours
  ),
  selectable_columns = c(
   "Bubas bison",
   "Copris elphenor",
   "Copris hispanus",
   "Digitonthophagus gazella",
   "Euoniticellus africanus",
   "Euoniticellus fulvus",
   "Euoniticellus intermedius",
   "Euoniticellus pallipes",
   "Geotrupes spiniger",
   "Liatongus militaris",
   "Onitis alexis",
   "Onitis aygulus",
   "Onitis caffer",
   "Onitis pecuarius",
   "Onitis vanderkelleni",
   "Onitis viridulus",
   "Onthophagus binodis",
   "Onthophagus nigriventris",
   "Onthophagus obliquus",
   "Onthophagus sagittarius",
   "Onthophagus taurus",
   "Sisyphus rubrus",
   "Sisyphus spinipes"
  ),
  country_codes = 'AU',
  header_title = "Dung Beetles of Australia",
  social_links = c('github'='https://github.com/jakemanger/spacetimeview_dungbeetles'),
  menu_text = 'Click on the coloured portions of the map to see what beetles were found there\n\nOr select a species from the dropdown menu to view its observed range 👇',
  initial_latitude = -27.007754997248703, 
  initial_longitude = 134.35406022625756,
  initial_zoom = 4,
  about_text = about_text
)


model_validation_data <- readRDS('temporal_model_predictions_points.rds')

model_validation_data |>
  filter(id == 13145) |>
  ggplot(aes(x = eventDate, y = `pred_abun_median_Onthophagus taurus`,
             ymin = `pred_abun_0.95_lower_Onthophagus taurus`, ymax = `pred_abun_0.95_upper_Onthophagus taurus`)) +
  geom_line(colour = "blue", lwd = 3) +
  geom_ribbon(fill = "blue", alpha = 0.5) +
  geom_point(inherit.aes = FALSE,
             aes(x = eventDate, y = `individualCount_Onthophagus taurus`)) + 
  scale_y_continuous(transform = "log") +
  theme_bw()

model_validation_data |>
  filter(id == 13145) |>
  ggplot(aes(x = eventDate, y = `pred_prob_median_Onthophagus taurus`,
             ymin = `pred_prob_0.95_lower_Onthophagus taurus`, ymax = `pred_prob_0.95_upper_Onthophagus taurus`)) +
  geom_line(colour = "blue", lwd = 3) +
  geom_ribbon(fill = "blue", alpha = 0.5) +
  geom_point(inherit.aes = FALSE,
             aes(x = eventDate, y = `occurrenceStatus_Onthophagus taurus`))


model_validation_data <- model_validation_data %>%
  select(
    contains("individualCount_")
    | contains("pred_abun_median_")
    | contains("pred_abun_0.95_lower_")
    | contains("pred_abun_0.95_upper_")
    | eventDate
    | decimalLatitude
    | decimalLongitude
  ) %>%
  rename_with(~ paste0(str_remove(.x, "individualCount_"), "_observed"), contains("individualCount_")) %>%
  rename_with(~ str_remove(.x, "pred_abun_median_"), contains("pred_abun_median_")) %>%
  rename_with(~ paste0(str_remove(.x, "pred_abun_0.95_lower_"), "_pred_lower"), contains("pred_abun_0.95_lower_")) %>%
  rename_with(~ paste0(str_remove(.x, "pred_abun_0.95_upper_"), "_pred_upper"), contains("pred_abun_0.95_upper_")) %>%
  mutate(eventDate = as.POSIXct(eventDate, tz = "UTC"))


model_validation_observable_code <- "
Plot.plot({
  marks: [
    ...(() => {
      var speciesColumns = Object.keys(data[0])
        .filter(key => key !== 'lat' && key !== 'lng' && key !== 'value' && key !== 'timestamp' && key !== 'originalTimestamp'
          && key !== 'decimalLatitude' && key !== 'decimalLongitude' && key !== 'eventDate'
          && !key.includes('_pred') && !key.includes('_lower') && !key.includes('_upper') && !key.includes('_observed'));

      if (!data || data.length === 0 || speciesColumns.length === 0) {
        return [Plot.text(['No data available'], {x: 0.5, y: 0.5, text: d => d})];
      }

      var selectedSpecies = columnName
      var observedKey = selectedSpecies + '_observed';
      var lowerKey = selectedSpecies + '_pred_lower';
      var upperKey = selectedSpecies + '_pred_upper';

      var marks = [];

      var timeField = data[0].timestamp ? 'timestamp' : (data[0].eventDate ? 'eventDate' : null);

      if (!timeField) {
        return [Plot.text(['No time data available'], {x: 0.5, y: 0.5, text: d => d})];
      }

      var sortedData = data.slice().sort((a, b) => new Date(a[timeField]) - new Date(b[timeField]));

      var dataWithPred = sortedData.filter(d => d[timeField] && d[selectedSpecies] !== undefined);
      if (dataWithPred.length > 0) {
        marks.push(
          Plot.areaY(
            dataWithPred,
            {
              x: d => new Date(d[timeField]),
              y1: d => Math.max(0.001, d[lowerKey] || 0.001),
              y2: d => Math.max(0.001, d[upperKey] || 0.001),
              fill: '#3498db',
              fillOpacity: 0.2,
              z: 'Confidence interval'
            }
          )
        );

        marks.push(
          Plot.line(
            dataWithPred,
            {
              x: d => new Date(d[timeField]),
              y: d => Math.max(0.001, d[selectedSpecies] || 0.001),
              stroke: '#3498db',
              strokeWidth: 2,
              z: 'Predicted abundance'
            }
          )
        );
      }

      var dataWithObserved = sortedData.filter(d => d[timeField] && d[observedKey] !== undefined && d[observedKey] !== null && d[observedKey] > 0);
      if (dataWithObserved.length > 0) {
        marks.push(
          Plot.dot(
            dataWithObserved,
            {
              x: d => new Date(d[timeField]),
              y: d => Math.max(0.001, d[observedKey]),
              fill: '#e74c3c',
              r: 4,
              z: 'Observed counts',
              title: d => {
                var date = new Date(d[timeField]).toLocaleDateString();
                var observed = d[observedKey] || 0;
                var predicted = d[selectedSpecies] || 0;
                return date + '\\nObserved: ' + observed + '\\nPredicted: ' + predicted.toFixed(2);
              }
            }
          )
        );
      }

      if (marks.length === 0) {
        return [Plot.text(['No time series data available for this location'], {x: 0.5, y: 0.5, text: d => d})];
      }

      return marks;
    })()
  ],
  color: {
    legend: true,
    domain: ['Predicted abundance', 'Prediction interval', 'Observed counts'],
    range: ['#3498db', '#3498db', '#e74c3c']
  },
  x: {
    type: 'time',
    label: 'Date',
    grid: true
  },
  y: {
    //type: 'log',
    grid: true,
    tickFormat: d => d < 1 ? d.toFixed(2) : Math.round(d).toString(),
  },
  style: {
    fontSize: '12px'
  },
  title: 'Predicted and observed abundance over time at ' + (data[0] ? (data[0].lat || data[0].decimalLatitude || 0).toFixed(2) : '0') + ', ' + (data[0] ? (data[0].lng || data[0].decimalLongitude || 0).toFixed(2) : '0'),
  width: 450,
  height: 300,
  marginBottom: 40,
  marginLeft: 60,
  marginRight: 20
})
"

model_validation_tab <- spacetimeview(
  model_validation_data,
  style = 'Summary',
  summary_radius = 5000,
  summary_height = 1,
  visible_controls = c('column_to_plot'),
  control_names = c(
   column_to_plot = 'Select a beetle species'
  ),
  json_digits = json_digits,
  observable = model_validation_observable_code,
  factor_levels = occurrence_factor_levels_list,
  selectable_columns = c(
   "Bubas bison",
   "Copris elphenor",
   "Copris hispanus",
   "Digitonthophagus gazella",
   "Euoniticellus africanus",
   "Euoniticellus fulvus",
   "Euoniticellus intermedius",
   "Euoniticellus pallipes",
   "Geotrupes spiniger",
   "Liatongus militaris",
   "Onitis alexis",
   "Onitis aygulus",
   "Onitis caffer",
   "Onitis pecuarius",
   "Onitis vanderkelleni",
   "Onitis viridulus",
   "Onthophagus binodis",
   "Onthophagus nigriventris",
   "Onthophagus obliquus",
   "Onthophagus sagittarius",
   "Onthophagus taurus",
   "Sisyphus rubrus",
   "Sisyphus spinipes"
  ),
  factor_icons = list(
   "Bubas bison" = "public/beetle_images/Bubas_bison.jpg",
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
   "Sisyphus rubrus" = "public/beetle_images/Sisyphus_rubrus.jpg",
   "Sisyphus spinipes" = "public/beetle_images/Sisyphus_spinipes.jpg"
  ),
  factor_colors = list(
   "Bubas bison" = occurrence_colours,
   "Digitonthophagus gazella" = occurrence_colours,
   "Euoniticellus africanus" = occurrence_colours,
   "Euoniticellus intermedius" = occurrence_colours,
   "Euoniticellus pallipes" = occurrence_colours,
   "Geotrupes spiniger" = occurrence_colours,
   "Liatongus militaris" = occurrence_colours,
   "Onitis alexis" = occurrence_colours,
   "Onitis aygulus" = occurrence_colours,
   "Onitis caffer" = occurrence_colours,
   "Onitis pecuarius" = occurrence_colours,
   "Onitis viridulus" = occurrence_colours,
   "Onthophagus nigriventris" = occurrence_colours,
   "Onthophagus sagittarius" = occurrence_colours,
   "Onthophagus taurus" = occurrence_colours,
   "Sisyphus rubrus" = occurrence_colours,
   "Sisyphus spinipes" = occurrence_colours,
   "Onthophagus binodis" = occurrence_colours,
   "Euoniticellus fulvus" = occurrence_colours
  ),
  country_codes = 'AU',
  header_title = "Dung Beetles of Australia",
  social_links = c('github'='https://github.com/jakemanger/spacetimeview_dungbeetles'),
  menu_text = 'Click on a location to see predicted vs observed abundance over time for model validation\n\nUse the dropdown menu to view a different species 👇',
  initial_latitude = -27.007754997248703,
  initial_longitude = 134.35406022625756,
  initial_zoom = 4,
  about_text = about_text,
  animation_speed = 6,
  initial_time_mode = 'seasonal',
  sticky_range = TRUE
)


seasonal_predictions_raw <- readRDS('temporal_model_predictions_points_2010_11.rds')

seasonal_predictions <- seasonal_predictions_raw %>%
  select(
    contains("pred_abun_median_")
    | contains("pred_abun_0.95_lower_")
    | contains("pred_abun_0.95_upper_")
    | eventDate
    | decimalLatitude
    | decimalLongitude
  ) %>%
  select(-contains("pred_abun_median_NA"), -contains("pred_abun_0.95_lower_NA"), -contains("pred_abun_0.95_upper_NA")) %>%
  rename_with(~ str_remove(.x, "pred_abun_median_"), contains("pred_abun_median_")) %>%
  rename_with(~ paste0(str_remove(.x, "pred_abun_0.95_lower_"), "_pred_lower"), contains("pred_abun_0.95_lower_")) %>%
  rename_with(~ paste0(str_remove(.x, "pred_abun_0.95_upper_"), "_pred_upper"), contains("pred_abun_0.95_upper_")) %>%
  mutate(eventDate = as.POSIXct(eventDate, tz = "UTC"))

# filter out rows where all prediction columns are NA (78% of rows)
seasonal_predictions <- seasonal_predictions %>%
  filter(rowSums(!is.na(select(., -eventDate, -decimalLatitude, -decimalLongitude))) > 0)

cat(sprintf("Seasonal predictions: filtered from %s to %s rows (removed %s%% empty rows)\n",
            format(nrow(seasonal_predictions_raw), big.mark=","),
            format(nrow(seasonal_predictions), big.mark=","),
            round(100 * (1 - nrow(seasonal_predictions) / nrow(seasonal_predictions_raw)), 1)))

seasonal_predictions_observable_code <- "
Plot.plot({
  marks: [
    ...(() => {
      var speciesColumns = Object.keys(data[0])
        .filter(key => key !== 'lat' && key !== 'lng' && key !== 'value' && key !== 'timestamp' && key !== 'originalTimestamp'
          && key !== 'decimalLatitude' && key !== 'decimalLongitude' && key !== 'eventDate'
          && !key.includes('_pred') && !key.includes('_lower') && !key.includes('_upper'));

      if (!data || data.length === 0 || speciesColumns.length === 0) {
        return [Plot.text(['No data available'], {x: 0.5, y: 0.5, text: d => d})];
      }

      var selectedSpecies = columnName;
      var lowerKey = selectedSpecies + '_pred_lower';
      var upperKey = selectedSpecies + '_pred_upper';

      var marks = [];

      var timeField = data[0].timestamp ? 'timestamp' : (data[0].eventDate ? 'eventDate' : null);

      if (!timeField) {
        return [Plot.text(['No time data available'], {x: 0.5, y: 0.5, text: d => d})];
      }

      var sortedData = data.slice().sort((a, b) => new Date(a[timeField]) - new Date(b[timeField]));

      var dataWithPred = sortedData.filter(d => d[timeField] && d[selectedSpecies] !== undefined);
      if (dataWithPred.length > 0) {
        marks.push(
          Plot.areaY(
            dataWithPred,
            {
              x: d => new Date(d[timeField]),
              y1: d => Math.max(0.001, d[lowerKey] || 0.001),
              y2: d => Math.max(0.001, d[upperKey] || 0.001),
              fill: '#3498db',
              fillOpacity: 0.2,
              z: 'Confidence interval'
            }
          )
        );

        marks.push(
          Plot.line(
            dataWithPred,
            {
              x: d => new Date(d[timeField]),
              y: d => Math.max(0.001, d[selectedSpecies] || 0.001),
              stroke: '#3498db',
              strokeWidth: 2,
              z: 'Predicted abundance'
            }
          )
        );
      }

      if (marks.length === 0) {
        return [Plot.text(['No seasonal prediction data available for this location'], {x: 0.5, y: 0.5, text: d => d})];
      }

      return marks;
    })()
  ],
  color: {
    legend: true,
    domain: ['Predicted abundance', 'Confidence interval'],
    range: ['#3498db', '#3498db']
  },
  x: {
    type: 'time',
    label: 'Date',
    grid: true
  },
  y: {
    label: 'Abundance',
    grid: true,
    tickFormat: d => d < 1 ? d.toFixed(2) : Math.round(d).toString()
  },
  style: {
    fontSize: '12px'
  },
  title: 'Predicted abundance over the year at ' + (data[0] ? (data[0].lat || data[0].decimalLatitude || 0).toFixed(2) : '0') + ', ' + (data[0] ? (data[0].lng || data[0].decimalLongitude || 0).toFixed(2) : '0'),
  width: 450,
  height: 300,
  marginBottom: 40,
  marginLeft: 60,
  marginRight: 20
})
"

seasonal_predictions_tab <- spacetimeview(
  seasonal_predictions,
  style = 'Summary',
  summary_radius = 45000,
  summary_height = 1,
  visible_controls = c('column_to_plot'),
  control_names = c(
   column_to_plot = 'Select a beetle species'
  ),
  json_digits = json_digits,
  observable = seasonal_predictions_observable_code,
  selectable_columns = c(
   "Euoniticellus fulvus",
   "Euoniticellus africanus",
   "Copris hispanus"
  ),
  factor_icons = list(
   "Euoniticellus fulvus" = "public/beetle_images/Euoniticellus_fulvus.jpg",
   "Euoniticellus africanus" = "public/beetle_images/Euoniticellus_africanus.jpg",
   "Copris hispanus" = "public/beetle_images/Copris_hispanus.jpg"
  ),
  country_codes = 'AU',
  header_title = "Dung Beetles of Australia",
  social_links = c('github'='https://github.com/jakemanger/spacetimeview_dungbeetles'),
  menu_text = 'Click on a location to see predicted abundance of each beetle species throughout the year\n\nUse the dropdown menu to view a different species 👇',
  initial_latitude = -27.007754997248703,
  initial_longitude = 134.35406022625756,
  initial_zoom = 4,
  about_text = about_text,
  animation_speed = 6,
  initial_time_mode = 'seasonal',
  sticky_range = TRUE
)


plt <- predictions_tab + seasonal_predictions_tab + occurrence_tab + model_validation_tab

names(plt) <- c('Predictions', 'Seasonal Predictions', 'Occurrences', 'Model Validation')

plot(plt)
