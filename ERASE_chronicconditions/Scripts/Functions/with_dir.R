# Change directory to do something and then go back to the previous one
with_dir <- function(dir, expr) {
    old_wd <- getwd()
    on.exit(setwd(old_wd))
    setwd(dir)
    evalq(expr)
}