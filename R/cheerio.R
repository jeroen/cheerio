#' Parse HTML/XML
#'
#' This function parses a HTML or XML document in a private V8 context.
#'
#' @export
#' @importFrom V8 new_context
#' @importFrom curl curl
#' @importFrom jsonlite toJSON
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
  call_and_return <- function(fun, ...){
    FUN <- paste_dot(varname, fun)
    ct$call(FUN, ...)
  }
  call_and_save <- function(fun, ...){
    FUN <- paste_dot(varname, fun)
    args <- paste(vapply(list(...), toJSON, character(1), auto_unbox = TRUE), collapse=",")
    statement <- paste0(FUN, "(", args, ")")
    newvar <- ct$call("create_object", I(statement));
    cheerio_node(ct, newvar)
  }
  el <- local({
    length <- function(){
      ct$get(paste_dot(varname, "length"))
    }
    text <- function(){
      call_and_return("text")
    }
    html <- function(){
      call_and_return("html")
    }
    attr <- function(name, value){
      if(missing(value)){
        call_and_return("attr", name)
      } else {
        call_and_save("attr", name, value)
      }
    }
    data <- function(name, value){
      if(missing(value)){
        call_and_return("data", name)
      } else {
        call_and_save("data", name, value)
      }
    }
    val <- function(value){
      if(missing(value)){
        call_and_return("val")
      } else {
        call_and_save("val", value)
      }
    }
    removeAttr <- function(name){
      call_and_save("removeAttr", name)
    }
    hasClass <- function(className){
      call_and_return("hasClass", className)
    }
    addClass <- function(className){
      call_and_save("addClass", className)
    }
    removeClass <- function(className){
      if(missing(className)){
        call_and_save("removeClass")
      } else {
        call_and_save("removeClass", className)
      }
    }
    toggleClass <- function(className){
      call_and_save("toggleClass", className)
    }
    is <- function(selector){
      call_and_return("is", selector)
    }
    find <- function(selector){
      call_and_save("is", selector)
    }
    parent <- function(selector){
      if(missing(selector)){
        call_and_save("parent")
      } else {
        call_and_save("parent", selector)
      }
    }
    parents <- function(selector){
      if(missing(selector)){
        call_and_save("parents")
      } else {
        call_and_save("parents", selector)
      }
    }
    parentsUntil <- function(selector, filter){
      if(missing(selector)){
        call_and_save("parents")
      } else if(missing(filter)) {
        call_and_save("parents", selector)
      } else {
        call_and_save("parents", selector, filter)
      }
    }
    closest <- function(selector){
      call_and_save("closest", selector)
    }
    next <- function(selector){
      if(missing(selector)){
        call_and_save("next")
      } else {
        call_and_save("next", selector)
      }
    }
    nextAll <- function(){
      call_and_save("nextAll")
    }
    nextUntil <- function(selector){
      call_and_save("nextUntil", selector)
    }
    prev <- function(selector){
      if(missing(selector)){
        call_and_save("prev")
      } else {
        call_and_save("prev", selector)
      }
    }
    prevUntil <- function(selector){
      call_and_save("prevUntil", selector)
    }




    environment();
  })
}

paste_dot <- function(...){
  paste(..., sep = ".")
}


