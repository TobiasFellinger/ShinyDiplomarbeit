
tryCatch(fs::dir_delete("docs"), error=\(e){})

source("prepare.R")

shinylive::export("app", "docs")

fs::file_copy("datasets.Rds", "docs")