library(tidyverse)
library(sf)
library(dungfaunaR)

devtools::load_all('../spacetimeview')

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
d <- d %>%
  # keep only prediction columns, richness, and coordinates (using class predictions for main viz)
  select(contains("pred_class_") | contains("pred_prob_0.95_lower_") | contains("pred_prob_0.95_upper_") | pred_richness_median | decimalLatitude | decimalLongitude) %>%
  # clean up column names
  rename_with(~ str_remove(.x, "pred_class_"), contains("pred_class_")) %>%
  rename_with(~ paste0(str_remove(.x, "pred_prob_0.95_lower_"), "_lower"), contains("pred_prob_0.95_lower_")) %>%
  rename_with(~ paste0(str_remove(.x, "pred_prob_0.95_upper_"), "_upper"), contains("pred_prob_0.95_upper_")) %>%
  # rename richness column
  rename("Number of species" = pred_richness_median)

# reverse factor levels so 1 = absent and 11 = present
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
    // Lollipop sticks (confidence interval lines)
    Plot.link(
      Object.keys(data[0])
        .filter(key => key !== 'lat' && key !== 'lng' && key !== 'value' && key !== 'Number of species' && !key.includes('No. of') && !key.includes('species') && !key.includes('_lower') && !key.includes('_upper'))
        .map(species => {
          var cleanName = species.replace(/[_]/g, ' ');
          var average = data.reduce((sum, d) => sum + (d[species] || 0), 0) / data.length;
          
          // Get confidence bounds
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
    // Lollipop dots
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
         // Icons positioned to the left of the y-axis
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
         // Beetle emoji for species without icons
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
  country_codes = 'AU',
  header_title = "Dung Beetles of Australia",
  social_links = c('github'='https://github.com/jakemanger/spacetimeview_dungbeetles'),
  menu_text = 'Click on the map to see what beetles are predicted to be found there, or select a species to view its range 👇',
  initial_latitude = -27.007754997248703, 
  initial_longitude = 134.35406022625756,
  initial_zoom = 4,
  legend_direction_text = c("Bubas bison" = "Likelihood", "Bubas bubalus" = "Likelihood", "Copris elphenor" = "Likelihood", "Copris hispanus" = "Likelihood", "Digitonthophagus gazella" = "Likelihood", "Euoniticellus africanus" = "Likelihood", "Euoniticellus fulvus" = "Likelihood", "Euoniticellus intermedius" = "Likelihood", "Euoniticellus pallipes" = "Likelihood", "Geotrupes spiniger" = "Likelihood", "Liatongus militaris" = "Likelihood", "Onitis alexis" = "Likelihood", "Onitis aygulus" = "Likelihood", "Onitis caffer" = "Likelihood", "Onitis pecuarius" = "Likelihood", "Onitis vanderkelleni" = "Likelihood", "Onitis viridulus" = "Likelihood", "Onthophagus binodis" = "Likelihood", "Onthophagus nigriventris" = "Likelihood", "Onthophagus obliquus" = "Likelihood", "Onthophagus sagittarius" = "Likelihood", "Onthophagus taurus" = "Likelihood", "Onthophagus vacca" = "Likelihood", "Sisyphus rubrus" = "Likelihood", "Sisyphus spinipes" = "Likelihood"),
  about_text = about_text
)

occurrence_d <- readRDS("presence_model_predictions_points.rds") %>%
  # get coordinates and occurrence status data
  select(decimalLatitude, decimalLongitude, starts_with("occurrenceStatus_")) %>%
  # explicitly exclude any richness columns that might have leaked through
  select(-contains("richness"), -contains("pred_richness")) %>%
  # clean up column names
  rename_with(~ str_remove(.x, "occurrenceStatus_"), starts_with("occurrenceStatus_")) %>%
  # convert to Found/Not found factors
  mutate(across(-c(decimalLatitude, decimalLongitude), ~ case_when(
    .x == "Found" ~ factor("Found", levels = c("Found", "Not found")),
    .x == "Not found" ~ factor("Not found", levels = c("Found", "Not found")),
    TRUE ~ NA_character_
  ))) %>%
  # ensure proper factor levels
  mutate(across(-c(decimalLatitude, decimalLongitude), ~ factor(.x, levels = c("Found", "Not found"))))

# simple color scheme for found/not found
occurrence_colours <- c(
  "Found" = "#e74c3c",
  "Not found" = "#3498db"
)

# factor levels for occurrence data
occurrence_factor_levels_list <- list()
species_columns_occ <- names(occurrence_d)[!names(occurrence_d) %in% c("decimalLatitude", "decimalLongitude")]
for(species in species_columns_occ) {
  occurrence_factor_levels_list[[species]] <- c("Found", "Not found")
}

occurrence_histogram_code <- "
Plot.plot({
  marks: [
    // Bar chart showing presence/absence status
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
    // Icons positioned to the left of the y-axis
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
    // Beetle emoji for species without icons
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
    ticks: [0.05, 0.5, 1],
    tickFormat: function(d) {
      if (d === 0.05) return 'Unknown';
      if (d === 0.5) return 'Not Found';
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
  country_codes = 'AU',
  header_title = "Dung Beetles of Australia",
  social_links = c('github'='https://github.com/jakemanger/spacetimeview_dungbeetles'),
  menu_text = 'Click on the map to see what beetles are found there, or select a species to view its observed range 👇',
  initial_latitude = -27.007754997248703, 
  initial_longitude = 134.35406022625756,
  initial_zoom = 4,
  about_text = about_text
)

library(tidyverse)
library(ggplot2)

time_series_data <- readRDS('temporal_model_predictions_points.rds')

# diagnostic ggplots
time_series_data |>
  filter(id == 13145) |>
  ggplot(aes(x = eventDate, y = `pred_abun_median_Onthophagus taurus`,
             ymin = `pred_abun_0.95_lower_Onthophagus taurus`, ymax = `pred_abun_0.95_upper_Onthophagus taurus`)) +
  geom_line(colour = "blue", lwd = 3) +
  geom_ribbon(fill = "blue", alpha = 0.5) +
  geom_point(inherit.aes = FALSE,
             aes(x = eventDate, y = `individualCount_Onthophagus taurus`)) + 
  scale_y_continuous(transform = "log") +
  theme_bw()

time_series_data |>
  filter(id == 13145) |>
  ggplot(aes(x = eventDate, y = `pred_prob_median_Onthophagus taurus`,
             ymin = `pred_prob_0.95_lower_Onthophagus taurus`, ymax = `pred_prob_0.95_upper_Onthophagus taurus`)) +
  geom_line(colour = "blue", lwd = 3) +
  geom_ribbon(fill = "blue", alpha = 0.5) +
  geom_point(inherit.aes = FALSE,
             aes(x = eventDate, y = `occurrenceStatus_Onthophagus taurus`))


time_series_data <- time_series_data %>%
  # keep only occurrence status, time and coordinates
  select(
    contains("pred_prob_median_") 
    | contains("pred_prob_0.95_lower_") 
    | contains("pred_prob_0.95_upper_") 
    | eventDate 
    | decimalLatitude 
    | decimalLongitude
  ) %>%
  # clean up column names
  rename_with(~ str_remove(.x, "pred_prob_median_"), contains("pred_prob_median_")) %>%
  rename_with(~ paste0(str_remove(.x, "pred_prob_0.95_lower_"), "_lower"), contains("pred_prob_0.95_lower_")) %>%
  rename_with(~ paste0(str_remove(.x, "pred_prob_0.95_upper_"), "_upper"), contains("pred_prob_0.95_upper_"))


time_series_tab <- spacetimeview(
  time_series_data,
  style = 'Summary',
  summary_radius = 7000,
  summary_height = 1,
  visible_controls = c('column_to_plot'),
  control_names = c(
   column_to_plot = 'Select a beetle species'
  ),
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
  country_codes = 'AU',
  header_title = "Dung Beetles of Australia",
  social_links = c('github'='https://github.com/jakemanger/spacetimeview_dungbeetles'),
  menu_text = 'Click on the map to see what beetles are found there, or select a species to view its observed range 👇',
  initial_latitude = -27.007754997248703, 
  initial_longitude = 134.35406022625756,
  initial_zoom = 4,
  about_text = about_text,
  animation_speed = 6,
  initial_time_mode = 'seasonal',
  sticky_range = TRUE
)
time_series_tab

plt <- time_series_tab + predictions_tab + occurrence_tab

names(plt) <- c('Predictions over time', 'Predictions', 'Occurrences')

plot(plt)
