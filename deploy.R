# devtools::install_github("jakemanger/spacetimeview")
# library(spacetimeview)
devtools::load_all('../spacetimeview')

source('plot.R')

# save it straight into the GitHub Pages folder
dir.create("docs", showWarnings = FALSE, recursive = TRUE)
htmlwidgets::saveWidget(spacetimetabs(plt), "docs/index.html", selfcontained = FALSE)

# copy the data directory to docs if it exists
if(dir.exists("data")) {
  if (dir.exists("docs/data")) {
    unlink("docs/data", recursive = TRUE)
  }
  dir.create("docs/data", showWarnings = FALSE, recursive = TRUE)
  file.copy(list.files("data", full.names = TRUE), "docs/data", overwrite = TRUE)
}

# commit and push the changes to GitHub
system("git add docs/")
if (system("git diff --cached --quiet") != 0) {
  system("git commit -m 'Deploy spacetimeview widget to GitHub Pages'")
  system("git push")
} else {
  message("No docs changes to deploy.")
}
