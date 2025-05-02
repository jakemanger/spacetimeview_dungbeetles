# devtools::install_github("jakemanger/spacetimeview")
# devtools::install_github("jdberson/dungfaunaR")
library(spacetimeview)
library(dungfaunaR)

# load your data
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

# now make your dashboard in one line of code
p <- spacetimeview(
  dungfauna_occurrence,
  column_to_plot = 'individualCount',
  color_scheme = 'Reds',
  summary_radius = 20000,
  animation_speed = 10,
  summary_height = 100,
  header_title='Dung Beetles of Australia',
  social_links=c('github'='https://github.com/jakemanger/spacetimeview_dungbeetles'),
  filter_column='scientificName',
  factor_icons=list(

    scientificName = list(
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
  )
)

print(p)
