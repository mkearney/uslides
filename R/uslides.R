#' uslides_document
#'
#' Document template for uslide beamer presentation.
#'
#' @export
uslides_document <- function() {
  template_path <- system.file(
    "rmarkdown", "templates", "uslides_document", package = "uslides"
  )
  ## set locations of css and doc pre/suf fixes
  doc_prefix <- file.path(
    template_path, "resources", "uslides_document_prefix.tex"
  )
  doc_prefix <- add_logos(doc_prefix)
  doc_prebody <- file.path(
    template_path, "resources", "uslides_document_prebody.tex"
  )
  ## create css file
  ## call the base html_document function
  rmarkdown::beamer_presentation(
    latex_engine = "xelatex",
    toc = FALSE,
    fig_width = 10,
    fig_height = 7,
    theme = "default",
    highlight = "haddock",
    includes = rmarkdown::includes(
      in_header = doc_prefix,
      before_body = doc_prebody
    )
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
  tmp
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