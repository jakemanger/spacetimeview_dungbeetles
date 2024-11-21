# devtools::install_github("jakemanger/spacetimeview")
library(spacetimeview)

# load your data
d <- read.csv('https://raw.githubusercontent.com/uber-web/kepler.gl-data/master/earthquakes/data.csv')
d <- d[,c('Latitude', 'Longitude', 'DateTime', 'Magnitude')]

# now make your dashboard in one line of code
spacetimeview(d, summary_radius = 10000, summary_height = 100, header_title='spacetimeview example', social_links=c('github'='https://github.com/jakemanger/spacetimeview'))
