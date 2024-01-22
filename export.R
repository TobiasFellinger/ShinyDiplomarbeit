
tryCatch(fs::dir_delete("docs"), error=\(e){})

source("prepare.R")

shinylive::export("app", "docs")

fs::file_copy("datasets.Rds", "docs")

rmarkdown::render(
  "landingpage.md",
  rmarkdown::html_document(
    template = "pandoc_template_about.html"
  ),
  output_file="docs/about.html"
) 
