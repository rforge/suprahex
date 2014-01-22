# Replay a list of evaluated results, just like you'd run them in a R
# terminal, but rendered as html

replay_html <- function(x, ...) UseMethod("replay_html", x)

#' @importFrom evaluate is.source
#' @S3method replay_html list
replay_html.list <- function(x, ...) {
  # Stitch adjacent source blocks back together
  src <- vapply(x, is.source, logical(1))
  # New group whenever not source, or when src after not-src
  group <- cumsum(!src | c(FALSE, src[-1] != src[-length(src)]))
  
  parts <- split(x, group)
  parts <- lapply(parts, function(x) {
    if (length(x) == 1) return(x[[1]])
    src <- str_c(vapply(x, "[[", "src", FUN.VALUE = character(1)), 
      collapse = "")
    structure(list(src = src), class = "source")
  })
  
  pieces <- mapply(replay_html, parts, obj_id = seq_along(parts), 
    MoreArgs = list(...))
  str_c(pieces, collapse = "\n")
}

#' @S3method replay_html character
replay_html.character <- function(x, ...) {
  str_c("<div class='output'>", str_c(x, collapse = ""), "</div>")
}

#' @S3method replay_html value
replay_html.value <- function(x, ...) {
  if (!x$visible) return()
  
  printed <- str_c(capture.output(print(x$value)), collapse = "\n")
  str_c("<div class='output'>", printed, "</div>")
}

#' @S3method replay_html source
replay_html.source <- function(x, ..., package) {
  str_c("<div class='input'>", src_highlight(x$src, package$rd_index),
    "</div>")
}

#' @S3method replay_html warning
replay_html.warning <- function(x, ...) {
  str_c("<strong class='warning'>Warning message:\n", x$message, "</strong>")
}

#' @S3method replay_html message
replay_html.message <- function(x, ...) {
  str_c("<strong class='message'>", str_replace(x$message, "\n$", ""),
   "</strong>")
}

#' @S3method replay_html error
replay_html.error <- function(x, ...) {
  if (is.null(x$call)) {
    str_c("<strong class='error'>Error: ", x$message, "</strong>")
  } else {
    call <- deparse(x$call)
    str_c("<strong class='error'>Error in ", call, ": ", x$message,
     "</strong>")
  }
}

#' @S3method replay_html recordedplot
replay_html.recordedplot <- function(x, package, name_prefix, obj_id, ...) {  
  name <- str_c(name_prefix, obj_id, ".png")
  path <- file.path(package$base_path, name)
  
  ### always generate new png
  #if (!file.exists(path)) { 
    wh <- dev.size(units="px")
    wth <- 1200
    hgt <- wth*wh[2]/wh[1]
    res <- min(c(wth,hgt))*72/480
    png(path, width=wth, height=hgt, res=res)
    on.exit(dev.off())
    print(x)
  #}

  #str_c("<p><img src='", name, "' alt='' width='400' height='400' /></p>")
  #str_c("<p><img src='", name, "' alt='' width='400' /></p>")
  #str_c("<p><a href='", name,"' target='", name, "'><IMG src='", name, "' alt='' width='400' /></a></p>")
  
  str_c("<a href=\"javascript:newWin('", name,"', '", name,"', '",wth,"', '",hgt,"')\" title=\"Click to enlarge\"><img style=\"max-width:95%;border:3px solid #FFFFFF;\" src='", name,"', width=\"400\" /></a>")
}

