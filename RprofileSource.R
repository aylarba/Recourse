# SOURCED BY DEFAULT FROM SYSTEM Rprofile.site
# system('open /Library/Frameworks/R.framework/Versions/Current/Resources/etc')
.env <- new.env()
.env$unrowname <- function(x) { # removes rownames from df
  rownames(x) <- NULL
  x
}
.env$unfactor <- function(df){ # removes erroneous as.factor
  id <- sapply(df, is.factor)
  df[id] <- lapply(df[id], as.character)
  df
}
.env$vvv <- function(){ # pastes from text into code
  L <- readLines(pipe("pbpaste"))
  rx <- "^[[:blank:]]*[^>+[:blank:]]*[>+]"
  is.cmd <- grepl(rx, L)
  L[is.cmd] <- gsub(paste(rx, "?"), "", L[is.cmd])
  L[!is.cmd] <- paste("#", L[!is.cmd])
  for(el in L) cat(el, "\n")
  invisible(L)
}

attach(.env)
# options(max.print=100)

# options(scipen=10)

# options(editor="vim")

# options(show.signif.stars=FALSE)

# options(menu.graphics=FALSE)

# options(prompt="> ")
# options(continue="... ")

# options(width = 80)

#q <- function (save="no", ...) {
#  quit(save=save, ...)
#}

# utils::rc.settings(ipck=TRUE)

.First <- function(){
  system("cp ~/Dropbox/R/DefaultDir/snippets/r.snippets ~/.R/snippets/r.snippets")
#  if(interactive()){
#    library(utils)
#    timestamp(,prefix=paste("##------ [",getwd(),"] ",sep=""))
# 
#  }
}
# 
.Last <- function(){
  # system("cp ~/.R/snippets/r.snippets ~/Dropbox/R/DefaultDir/snippets/r.snippets")
#  if(interactive()){
#    hist_file <- Sys.getenv("R_HISTFILE")
# if(hist_file=="") hist_file <- "~/.RHistory"
# savehistory(hist_file)
# }
}

# if(Sys.getenv("TERM") == "xterm-256color")
#   library("colorout")


# silent autoloads ---------------------------------------------------------------
# move this to .env
# loads a package silently
slibrary <- function(a.package){
  suppressWarnings(suppressPackageStartupMessages(
    library(a.package, character.only=TRUE)))
}

auto.loads <-c("data.table",
               # 'janitor',
               'pipeR',
               'rlist',
               'datapasta',
               'tibble')
if(interactive()){
  invisible(sapply(auto.loads, slibrary))
  rm(auto.loads,slibrary)
}
# selected Kmisc functions
read.cb <- function (sep = "\t", header = TRUE, ...) {
  sn <- Sys.info()["sysname"]
  if (sn == "Darwin") {
    read.table(pipe("pbpaste"), header = header, sep = sep, 
               ...)
  }
  else if (sn == "Windows") {
    read.table("clipboard", header = header, sep = sep, ...)
  }
  else {
    stop("Reading from the clipboard is not implemented for your system (", 
         sn, ") in this package.")
  }
}
write.cb <- function (dat, row.names = FALSE, col.names = TRUE, sep = "\t", 
                      quote = FALSE) {
  sn <- Sys.info()["sysname"]
  if (sn == "Darwin") {
    file <- pipe("pbcopy")
    write.table(dat, file = file, row.names = row.names, 
                col.names = col.names, sep = sep, quote = quote)
    close(file)
  }
  else if (sn == "Windows") {
    write.table(dat, file = "clipboard", row.names = row.names, 
                col.names = col.names, sep = sep, quote = quote)
  }
  else {
    stop("Writing to the clipboard is not implemented for your system (", 
         sn, ") in thfgrep
         is package.")
  }
}



# rm(tempdir1)
message("\n*** Successfully loaded RprofileSource ***\n")

