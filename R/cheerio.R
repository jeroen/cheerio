#' Parse HTML/XML
#'
#' This function parses a HTML or XML document in a private V8 context.
#'
#' @export
#' @importFrom V8 new_context
#' @importFrom curl curl
#' @param html Character vector html/xml
cheerio <- function(html, ...){
  # Shorthand for files and URLs
  if(is.character(html) && length(html) == 1 && nchar(html) < 200){
    if(grepl("^https?://", html)){
      html <- readLines(con <- curl(html, "r"))
      close(con)
    } else if(file.exists(html)){
      html <- readLines(html)
    }
  }

  # Collapse HTML doc into single string
  html <- paste(html, collapse="\n")
  opts <- list(...)

  # Setup Engine
  ct <- new_context();
  ct$source(system.file("js/cheerio.min.js", package = "cheerio"))
  ct$source(system.file("js/bindings.js", package = "cheerio"))
  ct$call("new_doc", html, opts)
  cheerio_doc(ct)
}

cheerio_doc <- function(ct){
  function(selector){
    cheerio_node(ct, ct$call("select_node", selector))
  }
}

cheerio_node <- function(ct, varname){
  this <- local({
    text <- function(){
      ct$call(paste_dot(varname, "text"))
    }
    html <- function(){
      ct$call(paste_dot(varname, "html"))
    }
    attr <- function(name, value){
      fun <- paste_dot(varname, "attr")
      if(missing(value)){
        ct$call(fun, name)
      } else {
        statement <- paste0(fun, "('", name, "','", value, "')")
        newvar <- ct$call("create_object", I(statement));
        cheerio_node(ct, newvar)
      }
    }
    environment();
  })
}

paste_dot <- function(...){
  paste(..., sep = ".")
}
