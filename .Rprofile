.libPaths(c(".packages", .libPaths()))
options(menu.graphics=FALSE)

local({
  r <- getOption("repos")
  r["CRAN"] <- "https://cloud.r-project.org/"
  options(repos = r)
  cat("Repo set to ", r["CRAN"], "\n")
})

.First <- function(){

  cat("Make sure you have '.packages' folder.")
  cat("Lib path loaded:\n")
  for (i in .libPaths()) {
    cat("  ", i, "\n")

  }
}