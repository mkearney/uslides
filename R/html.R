

#' @export
uslides_html <- function(katex = TRUE,
                         incremental = FALSE,
                         fig_width = 8,
                         fig_height = 4.9,
                         fig_retina = 2,
                         fig_caption = FALSE,
                         smart = TRUE,
                         self_contained = FALSE,
                         highlight = "default",
                         template = "default",
                         css = NULL,
                         includes = NULL,
                         keep_md = FALSE,
                         lib_dir = NULL,
                         pandoc_args = NULL) {

  theme <- "ribbon"
  ratio <- "16x10"

  #template_path <- system.file(
  #  "rmarkdown/templates/uslides_html/resources", package = "uslides"
  #)
  ## set locations of doc pre/suf fixes
  #doc_css <- file.path(
  #  template_path, "resources", "uslides_html_style.css"
  #)
  #doc_prefix <- add_logos(doc_prefix)

  theme_url <- find_theme(theme)

  #ratio <- match.arg(ratio)

  ## put common pandoc options here
  args <- c()

  if (identical(template, "default")) {
    default_template <- system.file(
      "rmarkdown/templates/uslides_html/resources/default.html",
      package = "uslides"
    )
    args <- c(args, "--template",
              rmarkdown::pandoc_path_arg(default_template))

  } else if (!is.null(template)) {
    args <- c(args, "--template",
              rmarkdown::pandoc_path_arg(template))
  }

  if (incremental)
    args <- c(args, "--incremental")

  # slide level
  args <- c(args, "--slide-level", "2")

  # aspect ratio
  args <- c(args, paste0("--variable=ratio:", ratio))

  # KaTeX?
  args <- c(args, if (katex) paste0("--variable=katex:yes"))

  # content includes
  args <- c(args, rmarkdown::includes_to_pandoc_args(includes))

  # additional css
  for (css_file in css) {
    args <- c(args, "--css", rmarkdown::pandoc_path_arg(css_file))
  }

  pre_processor <- function(metadata, input_file, runtime, knit_meta, files_dir,
                            output_dir) {

    ## we don't work with runtime shiny
    if (identical(runtime, "shiny")) {
      stop("shower is not compatible with runtime 'shiny'",
           call. = FALSE)
    }

    ## use files_dir as lib_dir if not explicitly specified
    if (is.null(lib_dir)) lib_dir <- files_dir

    ## extra args
    args <- c()

    shower_path <- system.file("rmarkdown/templates/uslides_html/resources", package = "uslides")
    if (!self_contained || identical(.Platform$OS.type, "windows")) {
      shower_path <- rmarkdown::relative_to(
        output_dir, rmarkdown::render_supporting_files(shower_path, lib_dir)
      )
    }
    args <- c(
      args,
      paste0("--variable=shower-url:",
             rmarkdown::pandoc_path_arg(shower_path))
    )

    ## theme
    args <- c(args, paste0("--variable=theme:", theme))
    if (!self_contained || identical(.Platform$OS.type, "windows")) {
      theme_url <- rmarkdown::relative_to(
        output_dir, rmarkdown::render_supporting_files(theme_url, lib_dir)
      )
    }
    args <- c(args, paste0("--variable=shower-theme-url:", theme_url))

    ## highlight
    args <- c(args,
              rmarkdown::pandoc_highlight_args(highlight, default = "pygments"))

    ## return additional args
    args
  }

  post_processor <- function(metadata, input_file, output_file, clean,
                             verbose) {

    ## Get lines from output file
    con <- file(output_file)
    lines <- readLines(con)
    close(con)

    ## Change <li class="fragment"> elements, add a "next" class.
    ## Shower needs this for incremental lists
    lines <- sub(
      "<li class=\"fragment\"",
      "<li class=\"fragment next\"",
      lines,
      fixed = TRUE
    )

    ## Everything should be H2 for shower
    lines <- sub(
      "^<h1>(.*)</h1>$",
      "<h2>\\1</h2>",
      lines,
      perl = TRUE
    )

    ## Title slides are H2, too, but have a special class
    lines <- sub(
      "(class=\"titleslide slide level1\">)<h1>(.*)</h1>",
      "\\1<h2 class=\"shout\">\\2</h2>",
      lines,
      perl = TRUE
    )

    ## No embedded sections, please
    lines <- sub("^<section><section", "<section", lines)
    lines <- sub("^</section></section>", "</section>", lines)

    ## Write it out
    con <- file(output_file)
    writeLines(lines, con)
    close(con)

    output_file
  }

  knitr::opts_chunk$set(collapse = TRUE)

  rmarkdown::output_format(
    knitr = rmarkdown::knitr_options_html(
      fig_width, fig_height, fig_retina, keep_md),
    pandoc = rmarkdown::pandoc_options(
      to = "revealjs",
      from = rmarkdown::rmarkdown_format(
        if (fig_caption) "" else "-implicit_figures"),
      args = args
    ),
    keep_md = keep_md,
    clean_supporting = self_contained,
    pre_processor = pre_processor,
    post_processor = post_processor,
    base_format = rmarkdown::html_document_base(
      smart = smart,
      lib_dir = lib_dir,
      self_contained = self_contained,
      mathjax = if (katex) "default" else NULL,
      pandoc_args = pandoc_args
    )
  )

}



find_theme <- function(theme) {

  ## builtin-themes
  if (theme %in% c("ribbon", "material", "earl2016")) {
    find_builtin_theme(theme)

  } else {
    find_theme_package(theme)
  }
}


find_builtin_theme <- function(theme) {
  system.file("rmarkdown/templates/uslides_html/resources", package = "uslides")
}

#' @importFrom utils installed.packages
find_theme_package <- function(theme) {
  theme_pkg <- paste0("rmdshower.", theme)
  if (!theme_pkg %in% rownames(installed.packages())) {
    stop("Cannot find theme package ", theme_pkg)
  }

  system.file(package = theme_pkg)
}

