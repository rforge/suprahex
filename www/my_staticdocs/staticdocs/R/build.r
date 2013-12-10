#' Build complete static documentation for a package.
#'
#' Currently, knitr builds documentation for:
#'
#' \itemize{
#'   \item Rd files.  Files
#'   \item Demos. Must be listed in \file{demos/00index}.
#'   \item Vignettes.
#' }
#'
#' @param package path to source version of package.  See
#'   \code{\link[devtools]{as.package}} for details on how paths and package
#'   names are resolved.
#' @param base_path root directory in which to create documentation
#' @param examples include examples or not?  Examples are particularly
#'   slow to render because all code must be run, so turning them off makes
#'   it easier to tweak templates etc.
#' @param flag_demos include demos or not? Disable this if no update 
#' @export
#' @import stringr
#' @importFrom devtools load_all
#' @aliases staticdocs-package
build_package <- function(package, base_path = NULL, examples=T, flag_demos=T) {
  load_all(package)
  
  package <- package_info(package, base_path, examples=examples)
  if (!file.exists(package$base_path)) dir.create(package$base_path)
  copy_bootstrap(base_path)
    
    # if there is a directory 'images', copy to the destinaton
    images_local_path <- file.path(pkg_sd_path(package), "images")
    if(file.exists(images_local_path)){
        file.copy(dir(images_local_path, full.names = TRUE), base_path, recursive = TRUE)
    }

  package$vignettes <- build_vignettes(package)
  package$demos <- build_demos(package, flag_demos=flag_demos)
  package$readme <- readme(package)
  
  package$installation <- installation(package)
  package$citation <- citation(package)
  package$demostations <- citation(package)
  
  package$topics <- build_topics(package)
  
  package <- build_index(package)

  ###############
  # for index html
  message("Generating index.html")
  out <- file.path(package$base_path, "index.html")
  package$pagetitle <- "Home"
  render_page(package, "index", package, out)

  ###############
  # for docs html
  message("Generating docs.html")
  out <- file.path(package$base_path, "docs.html")
  package$pagetitle <- "Docs"
  render_page(package, "docs", package, out)
  
  ###############
  # for install html
  message("Generating install.html")
  out <- file.path(package$base_path, "install.html")
  package$pagetitle <- "Install"
  render_page(package, "install", package, out)
  
  ###############
  # for cite html
  message("Generating cite.html")
  out <- file.path(package$base_path, "cite.html")
  package$pagetitle <- "Cite"
  render_page(package, "cite", package, out)
  
  ###############
  # for demos html
  message("Generating demos.html")
  out <- file.path(package$base_path, "demos.html")
  package$pagetitle <- "Demos"
  render_page(package, "demos", package, out)
  
  
  ###############
  
  if (interactive()) {
    browseURL(normalizePath(file.path(base_path, "index.html")))
  }
  
  invisible(TRUE)
}


#' Generate all topic pages for a package.
#'
#' @export
#' @inheritParams build_package
#' @param package_info A list containing information about the package,
#'   as generated by \code{\link{package_info}}
#' @keywords internal
build_topics <- function(package) {

  # for each file, find name of one topic
  index <- package$rd_index
  paths <- file.path(package$base_path, index$file_out)

  # create columns for extra topic info
  index$title <- ""
  index$in_index <- TRUE
  
  for (i in seq_along(index$name)) {
    message("Generating ", basename(paths[[i]]))
    
    rd <- package$rd[[i]]
    html <- to_html(rd, 
      env = new.env(parent = globalenv()), 
      topic = str_replace(basename(paths[[i]]), "\\.html$", ""),
      package = package)
    html$pagetitle <- html$name

    html$package <- package[c("package", "version")]
    
    # inherit several items from package
    if(1){
        package_items <- names(package)
        html_items <- names(html)
        for(item in package_items){
            if(!(item %in% html_items)){
                html[[item]] <- package[[item]]
            }
        }
    }
    
    render_page(package, "topic", html, paths[[i]])
    graphics.off()

    if ("internal" %in% html$keywords) {
      index$in_index[i] <- FALSE
    }
    index$title[i] <- html$title
  }

  index
}

readme <- function(package) {
    #if (package$readme!="") return(markdown(package$readme))
    
    top_path <- file.path(package$path, "README.md")
    local_path <- file.path(pkg_sd_path(package), "README.md")
  
    if(file.exists(top_path)){
        markdown(path = top_path)
    }else if(file.exists(local_path)){
        markdown(path = local_path)
    }else{
        # use description if no README.md is available
        #return( package$description )
        return( paste("<h2>",package$title,"</h2>\n\n", package$description, sep="") )
    }
}

installation <- function(package) {
    
    local_path <- file.path(pkg_sd_path(package), "INSTALLATIONs.md")
  
    if(file.exists(local_path)){
        markdown(path = local_path)
    }else{
        #return( paste("<h2>Installation</h2>\n\n", sep="") )
        return()
    }
}

citation <- function(package) {
    
    local_path <- file.path(pkg_sd_path(package), "CITATIONs.md")
  
    if(file.exists(local_path)){
        markdown(path = local_path)
    }else{
        #return( paste("<h2>Citation</h2>\n\n", sep="") )
        return()
    }
}

copy_bootstrap <- function(base_path) {
  bootstrap <- file.path(inst_path(), "bootstrap")
  file.copy(dir(bootstrap, full.names = TRUE), base_path, recursive = TRUE)
}


#' List all package vignettes.
#'
#' Copies all vignettes and returns data structure suitable for use with
#' whisker templates.
#'
#' @keywords internal
#' @inheritParams build_package
#' @importFrom tools buildVignettes
#' @return a list, with one element for each vignette containing the vignette
#'   title and file name.
build_vignettes <- function(package) {  
  # Locate source and built versions of vignettes
  path <- dir(file.path(package$path, c("inst/doc", "vignettes")), ".Rnw", 
    full.names = TRUE)
  if (length(path) == 0) return()
  
  message("Building vignettes")
  buildVignettes(dir = package$path)
  
  message("Copying vignettes")
  src <- str_replace(path, "\\.Rnw$", ".pdf")
  filename <- basename(src)
  dest <- file.path(package$base_path, "vignettes")

  if (!file.exists(dest)) dir.create(dest)
  file.copy(src, file.path(dest, filename))  

  # Extract titles
  title <- vapply(path, FUN.VALUE = character(1), function(x) {
    contents <- str_c(readLines(x), collapse = "\n")
    str_match(contents, "\\\\VignetteIndexEntry\\{(.*?)\\}")[2]
  })  
  
  list(vignette = unname(apply(cbind(filename, title), 1, as.list)))
}


build_demos <- function(package, flag_demos) {
  demo_dir <- file.path(pkg_sd_path(package), "demo")

  if (!file.exists(demo_dir)) return()
  
  message("Rendering demos")
  demos <- readLines(file.path(demo_dir, "List_demos.txt"))
  
  pieces <- str_split_fixed(demos, "\\s+", 2)
  in_path <- str_c(pieces[, 1], ".r")
  filename <- str_c("demo-", pieces[,1], ".html")
  title <- pieces[, 2]
  
  if(flag_demos){
      for(i in seq_along(title)) {
        demo_code <- readLines(file.path(demo_dir, in_path[i]))
        demo_expr <- evaluate(demo_code, new.env(parent = globalenv()))

        package$demo <- replay_html(demo_expr, package = package, name = str_c(pieces[i], "-"))
        package$pagetitle <- title[i]
        render_page(package, "demo", package, file.path(package$base_path, filename[i]))
        graphics.off()
      }
  }
  
  list(demo = unname(apply(cbind(filename, title), 1, as.list)))
}

