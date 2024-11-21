# devtools::install_github("jakemanger/spacetimeview")
library(spacetimeview)

# load your data
d <- read.csv('https://raw.githubusercontent.com/uber-web/kepler.gl-data/master/earthquakes/data.csv')
d <- d[,c('Latitude', 'Longitude', 'DateTime', 'Magnitude')]

# now make your dashboard in one line of code
plt <- spacetimeview(d, summary_radius = 10000, summary_height = 100, header_title='spacetimeview example', social_links=c('github'='https://github.com/jakemanger/spacetimeview'))

# plot it
print(plt)

# save it
htmlwidgets::saveWidget(plt, "my_plot.html")

# automate GitHub Pages setup to deploy html files in the `docs` folder
usethis::use_github_pages(branch='main', path='/docs')
# move the my_plot.html we just generated to the docs folder and push it to github
system("mkdir -p docs/")

# move the HTML file into the subdirectory and rename it as index.html for direct access
system("mv ./my_plot.html ./docs/index.html")

# commit and push the changes to GitHub
system("git add docs/index.html")
system("git commit -m 'Deploy spacetimeview widget to GitHub Pages'")
system("git push")
