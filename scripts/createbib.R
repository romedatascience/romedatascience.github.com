## This file writes the files in content/publication/
## It must be run once the file publication.bib is updated
## R -e "source('createbib.R')"

library(RefManageR)
library(stringr)

bib <- ReadBib("~/Dropbox/Documents/CV/publications.bib", check = FALSE)

fnames <- names(bib)

bibu <- unclass(bib)
qr <- "&rdquo;"
ql <- "&ldquo;"


path = "~/scratch/WWW/content/publications/"

authorize <- function(x) {
  lx <- length(x)
  if (lx == 1)  {
    return(x[1])
  }
  au <- x[1]

  if (lx > 2) {
    for (j in 2:(lx-1)) {
      au <- paste0(au, ", ", x[j])
    }
    au <- paste0(au, ", and ", x[lx])
  } else {
    au <- paste0(au, " and ", x[2])
  }
  au
}

pages <- function(x){
  p <- x$pages
  sub("--", "-",p)
}

abstract <- function(x) {

  abst <- be$abstract
  abst <- gsub(pattern = "\\{\"\\}", "\"", abst)
  abst <- gsub(pattern = "\\\\", "", abst)
  abst <- gsub(pattern = "“", q, abst)
  abst <- gsub(pattern = "”", q, abst)
  abst
}

for (j in 1:length(bibu)) {
be <- bibu[[j]]

citation = paste('"', ql, be$title, ".", qr, sep ="")
if (!is.null(be$booktitle)) {
  citation <- paste(citation, " In ", htmltools::em(be$booktitle), ", ", pages(be), ", edited by ", authorize(be$editor),". ", be$publisher, ".", qr, '"',sep = "" )
} else {
  if (!is.null(be$kind) && be$kind == "forthcoming") {
    citation <- paste(citation, " Forthcoming in ", be$journal, '"', sep ="")
  } else {
  if (be$status == "published")
    citation <- paste(citation, " ", be$journal, ", ", be$volume,"(",be$number,"):",pages(be), '"', sep ="")
    #citation <- paste(citation, " ", be$journal, ", ", htmltools::strong(be$year), " ", be$volume,"(",be$number,"):",pages(be), q, '"', sep ="")

  if (be$status == "publishedwp")
    citation <- paste(citation, " ", be$journal, '"', sep ="")

  if (be$status == "retired")
    citation <- paste('"', ql, be$title, ".", qr, "\"", sep ="")
  }
}

mdfile = "---\n"
mdfile <- paste(mdfile, "title: ", "\"",be$title,"\"", "\n\n", sep = "")
mdfile <- paste(mdfile, "author: ", authorize(be$author), "\n", sep ="")
mdfile <- paste(mdfile, "status: ", str_to_title(be$status), "\n", sep="")
mdfile <- paste(mdfile, "type: ", be$status, "\n", sep="")
mdfile <- paste(mdfile, "citation: ", citation, "\n", sep="")
mdfile <- paste(mdfile, "tag:\nsubjects:\ncomments: no\n", sep="")

if (!is.null(be$preprinturl) | length(be$printurl) > 0)
  mdfile <- paste(mdfile, "file: ", be$preprinturl, "\n", sep="")
mdfile <- paste(mdfile, "date: ", strptime(paste0("1-",be$month,"-",be$year, "\n"), "%d-%b-%Y"), "\n", sep="")
mdfile <- paste(mdfile, "publishdate: ", strptime(paste0("1-",be$month,"-",be$year, "\n"), "%d-%b-%Y"), "\n", sep="")
mdfile <- paste(mdfile, "doi: ", be$doi, "\n", sep="")
mdfile <- paste(mdfile, "supplemental: ", be$supplemental, "\n", sep="")


#mdfile <- paste(mdfile, "filter:\n - erb\n - markdown\n- rubypants\n", sep="")

mdfile <- paste(mdfile, "---\n\n", sep="")
mdfile <- paste(mdfile, abstract(be), sep="")

#fname <- paste0(gsub(" ", "_", str_trim(str_trunc(be$title, 22, ellipsis = ""))), ".md")

cat(mdfile, file = paste0(path, fnames[j], ".md"))


}
