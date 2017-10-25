.libPaths(c(".packages", .libPaths()))

options(menu.graphics=FALSE)

local({
  r <- getOption("repos")
  r["CRAN"] <- "https://cloud.r-project.org/"
  options(repos = r)
  cat("repo set to ", r["CRAN"], "\n")
})

.First <- function(){
    cat("make sure you have '.packages' folder for local packages installation\n")
    cat("lib path loaded:\n")
        for (i in .libPaths()) {
        cat(i, "\n")
 }
}