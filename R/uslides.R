#' uslides_document
#'
#' Document template for uslide beamer presentation.
#' @inheritParams rmarkdown::beamer_presentation
#' @export
uslides_document <- function(toc = FALSE,
                             incremental = FALSE,
                             fig_width = 9,
                             fig_height = 6,
                             fig_crop = TRUE,
                             fig_caption = TRUE,
                             keep_tex = FALSE,
                             pandoc_args = NULL,
                             highlight = "tango",
                             latex_engine = "xelatex") {
  template_path <- system.file(
    "rmarkdown", "templates", "uslides_document", package = "uslides"
  )
  ## set locations of doc pre/suf fixes
  doc_prefix <- file.path(
    template_path, "resources", "uslides_document_prefix.tex"
  )
  doc_prefix <- add_logos(doc_prefix)
  doc_afterbody <- file.path(
    template_path, "resources", "uslides_document_afterbody.tex"
  )
  doc_prebody <- file.path(
    template_path, "resources", "uslides_document_beforebody.tex"
  )
  knitr::knit_hooks$set(mysize = function(before, options, envir) {
    if (before) return(options$size)
  })
  knitr::opts_chunk$set(collapse = TRUE, mysize = TRUE, size = "\\scriptsize")
  knit_print.character = function(x, ...) {
    sQuote(noquote(x))
  }
  ## call the base html_document function
  rmarkdown::beamer_presentation(
    toc = toc,
    incremental = incremental,
    theme = "boxes",
    colortheme = "structure",
    latex_engine = latex_engine,
    df_print = "tibble",
    fig_crop = fig_crop,
    fig_width = fig_width,
    fig_height = fig_height,
    fig_caption = fig_caption,
    highlight = highlight,
    keep_tex = keep_tex,
    includes = rmarkdown::includes(
      in_header = doc_prefix,
      before_body = doc_prebody,
      after_body = doc_afterbody
    ),
    pandoc_args = pandoc_args
  )
}

#' add_logos
#'
#' Set logos as environment variables for uslides design
#'
#' @param doc_prefix Path to uslides_document_prefix. Default (NULL) will
#'   look up where package is located for default document.
#' @export
add_logos <- function(doc_prefix = NULL) {
  if (is.null(doc_prefix)) {
    template_path <- system.file(
      "rmarkdown", "templates", "uslides_document", package = "uslides"
    )
    ## set locations of css and doc pre/suf fixes
    doc_prefix <- file.path(
      template_path, "resources", "uslides_document_prefix.tex"
    )
  }
  con <- file(doc_prefix)
  x <- readLines(con, warn = FALSE)
  close(con)

  ## BOTTOM LEFT LOGO
  bl <- Sys.getenv("USLIDES_BOTTOMLEFT")
  ## if not set then ask and set
  if (identical(bl, "")) {
    if (interactive()) {
      ## if env var not set, then ask user and set it for future sessions
      bl <- readline_("What is the full path for BOTTOMLEFT image?")
      if (nchar(bl) > 0L) {
        cat(
          paste0("USLIDES_BOTTOMLEFT=", bl),
          file = .Renviron(),
          append = TRUE, fill = TRUE
        )
      }
    } else {
      ## if not interactive then guess the path
      bl <- path.expand("~/Dropbox/uslidelogos/bl.jpg")
    }
  }

  ## BOTTOM RIGHT LOGO
  br <- Sys.getenv("USLIDES_BOTTOMRIGHT")
  if (identical(br, "")) {
    if (interactive()) {
      br <- readline_("What is the full path for BOTTOMRIGHT image?")
      if (nchar(br) > 0L) {
        cat(
          paste0("USLIDES_BOTTOMRIGHT=", br),
          file = .Renviron(),
          append = TRUE, fill = TRUE
        )
      }
    } else {
      ## if not interactive then guess the path
      br <- path.expand("~/Dropbox/uslidelogos/br.jpg")
    }
  }
  if (file.exists(bl) || file.exists(br)) {
    x <- gsub("BOTTOMLEFT", bl, x)
    x <- gsub("BOTTOMRIGHT", br, x)
  } else {
    x <- grep("BOTTOMLEFT", x, invert = TRUE, value = TRUE)
    x <- grep("BOTTOMRIGHT", x, invert = TRUE, value = TRUE)
  }

  ## BOTTOM TITLE BANNER
  bn <- Sys.getenv("USLIDES_BANNER")
  if (identical(bn, "")) {
    if (interactive()) {
      bn <- readline_("What is the full path for BANNER image?")
      if (nchar(bn) > 0L) {
        cat(
          paste0("USLIDES_BANNER=", bn),
          file = .Renviron(),
          append = TRUE, fill = TRUE
        )
      }
    } else {
      ## if not interactive then guess the path
      bn <- path.expand("~/Dropbox/uslidelogos/bn.jpg")
    }
  }
  if (file.exists(bn)) {
    x <- gsub("BANNER", bn, x)
  } else {
    x <- grep("BANNER", x, invert = TRUE, value = TRUE)
  }

  ## BACKGROUND WATERMARK
  bg <- Sys.getenv("USLIDES_BACKGROUND")
  if (identical(bg, "")) {
    if (interactive()) {
      ## if env var not set, then ask user and set it for future sessions
      bg <- readline_("What is the full path for BACKGROUND image?")
      if (nchar(bg) > 0L) {
        cat(
          paste0("USLIDES_BACKGROUND=", bg),
          file = .Renviron(),
          append = TRUE, fill = TRUE
        )
      }
    } else {
      ## if not interactive then guess
      bg <- path.expand("~/Dropbox/uslidelogos/bg.jpg")
    }
  }
  if (file.exists(bg)) {
    x <- gsub("BACKGROUND", bg, x)
  } else {
    x <- grep("BACKGROUND", x, invert = TRUE, value = TRUE)
  }

  ## save as temp file
  tmp <- tempfile(fileext = ".tex")
  writeLines(x, tmp)
  invisible(tmp)
}


readline_ <- function(...) {
  input <- readline(paste(unlist(c(...)), collapse = ""))
  gsub("^\"|^'|\"$|'$", "", input)
}


.Renviron <- function() {
  if (file.exists(".Renviron")) {
    ".Renviron"
  } else if (!identical(Sys.getenv("HOME"), "")) {
    file.path(Sys.getenv("HOME"), ".Renviron")
  } else {
    file.path(normalizePath("~"), ".Renviron")
  }
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
  writeLines(lines, output_file)

  output_file
}

#' @export
uslides_html <- function() {
  fig_caption <- TRUE
  highlight <- "pygments"
  fig_width <- 8
  fig_height <- 4.9
  fig_retina <- 2
  fig_caption <- FALSE
  keep_md <- FALSE
  default_template <- system.file(
    "rmarkdown/templates/uslides_document/resources/default.html",
    package = "uslides"
  )
  template <- rmarkdown::pandoc_path_arg(default_template)
  css <- system.file(
    "rmarkdown/templates/uslides_document/resources/screen-16x10.css",
    package = "uslides"
  )
  shower_path <- system.file(package = "uslides")
  shower_path <- paste0("--variable=shower-url:",
                        rmarkdown::pandoc_path_arg(shower_path))
  css <- rmarkdown::pandoc_path_arg(css)
  highlight <- rmarkdown::pandoc_highlight_args(highlight)
  theme1 <- "--variable=theme:ribbon"
  theme2 <- system.file("node_modules/shower-ribbon", package = "uslides")
  #system.file("node_modules/shower-", package = "uslides")
  #list.files(system.file("node_modules", package = "uslides"))
  theme2 <- paste0("--variable=shower-theme-url:", theme2)
  args <- c("--template", template,
            "--slide-level", "2",
            "--variable=ratio:16x10",
            "--css", css,
            theme1, theme2,
            highlight,
            shower_path)
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
    clean_supporting = TRUE,
    pre_processor = NULL,
    post_processor = post_processor,
    base_format = rmarkdown::html_document_base(
      smart = TRUE,
      lib_dir = NULL,
      self_contained = TRUE,
      mathjax = NULL,
      pandoc_args = NULL
    )
  )
}
