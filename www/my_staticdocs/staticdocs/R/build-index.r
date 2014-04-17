#' Build the index page.
#'
build_index <- function(package) {

  index <- package$index
  topic_index <- package$topics[package$topics$in_index, , drop = FALSE]
  package$topic_index <- rows_list(topic_index)

  # Cross-reference complete list of topics vs. topics found in index page
  topics <- unlist(lapply(index, "[[", "topics"))
  missing <- !(topics %in% topic_index$name)
  if (any(missing)) {
    warning("Can't find index topics: ", paste(topics[missing], 
      collapse = ", "), call. = FALSE)
    topics <- topics[!missing]
  }
  
  other <- !(topic_index$name %in% topics)
  if (any(other)) {
  title <- if(length(topics)) 'Other' else ''
  index <- 
    c(index, list(sd_section(title, NULL, sort(topic_index$name[other]))))
  }
  
  # Render each section
  sections <- lapply(index, build_section, package = package)
  package$sections <- sections
  package$rd <- NULL
  
  render_icons(package)
  
  return(package)
  
}

build_section <- function(section, package) {
  find_info <- function(item) {
    match <- package$topics$name == item$name
    if (!any(match)) return(NULL)
    
    row <- package$topics[match, , drop = FALSE]
    item$file_out <- row$file_out

    aliases <- setdiff(row$alias[[1]], row$name)
    if (length(aliases) > 0) {
      item$aliases <- str_c("(", str_c(aliases, collapse = ", "), ")")
    }
    
    if (is.null(item$title)) {
      rd <- package$rd[[row$file_in]]
      item$title <- extract_title(rd)
    }
    
    item$icon <- icon_path(package, item$name)
    item
  }
  
  desc <- section$description
  
  ###
    markdown <- function(x = NULL, path = NULL) {
      if (is.null(path)) {
        if (is.null(x) || x == ""){
            return("")
        }else{
            markdownToHTML(text = x, fragment.only = TRUE, options = c("safelink", "use_xhtml", "smartypants"))
        }
      }else{
        markdownToHTML(text = x, file = path, fragment.only = TRUE, options = c("safelink", "use_xhtml", "smartypants"))
      }
    }
  ###
  
  list(
    title = section$name %||% "Missing section title",
    description = markdown(desc),
    items = compact(lapply(section$elements, find_info))
  )
}

extract_title <- function(x) {
  alias <- Find(function(x) attr(x, "Rd_tag") == "\\title", x)
  alias[[1]][[1]]
}

compact <- function (x) Filter(Negate(is.null), x)

