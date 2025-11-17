# devtools::install_github("jakemanger/spacetimeview")
# library(spacetimeview)
devtools::load_all('../spacetimeview')

source('plot.R')

# save it
htmlwidgets::saveWidget(spacetimetabs(plt), "my_plot.html")

# automate GitHub Pages setup to deploy html files in the `docs` folder
usethis::use_github_pages(branch='main', path='/docs')
# move the my_plot.html we just generated to the docs folder and push it to github
system("mkdir -p docs/")

# move the HTML file into the subdirectory and rename it as index.html for direct access
system("mv ./my_plot.html ./docs/index.html")

# copy the data directory to docs if it exists
if(dir.exists("data")) {
  system("cp -r data/ docs/data/")
}

# commit and push the changes to GitHub
system("git add docs/")
system("git commit -m 'Deploy spacetimeview widget to GitHub Pages'")
system("git push")
