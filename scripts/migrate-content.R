# One-time migration: Hugo TOML/YAML content -> Quarto-ready YAML content.
# Reads content/{publication,project,package,poster,talk}/*.md (TOML front matter)
# and content/post/*.Rmd (YAML front matter), writes new content/<type>/*.qmd files
# in place (overwriting the old .md, and replacing .Rmd with .qmd).
# Safe to delete after migration is verified.

library(RcppTOML)
library(yaml)
library(stringr)
library(purrr)

`%||%` <- function(a, b) if (is.null(a) || (is.character(a) && !nzchar(a))) b else a

# Run this script with the project root as the working directory.
root <- getwd()
stopifnot("run from the project root (content/ not found)" = dir.exists(file.path(root, "content")))

read_delimited <- function(path, delim) {
  txt <- readLines(path, warn = FALSE, encoding = "UTF-8")
  idx <- which(str_trim(txt) == delim)
  fm <- txt[(idx[1] + 1):(idx[2] - 1)]
  body <- if (length(txt) > idx[2]) txt[(idx[2] + 1):length(txt)] else character(0)
  list(fm = paste(fm, collapse = "\n"), body = str_trim(paste(body, collapse = "\n")))
}

read_toml_file <- function(path) {
  parts <- read_delimited(path, "+++")
  list(meta = RcppTOML::parseTOML(input = parts$fm, fromFile = FALSE), body = parts$body)
}

read_yaml_frontmatter_only <- function(path) {
  parts <- read_delimited(path, "---")
  list(meta = yaml::yaml.load(parts$fm), body_raw = parts$body)
}

fmt_date <- function(x) {
  if (is.null(x)) return(NULL)
  if (inherits(x, "Date") || inherits(x, "POSIXct")) return(format(as.Date(x), "%Y-%m-%d"))
  as.character(x)
}

write_qmd <- function(path, meta, body) {
  meta <- purrr::compact(meta)
  yml <- yaml::as.yaml(meta, indent.mapping.sequence = TRUE)
  writeLines(c("---", str_trim(yml, side = "right"), "---", "", body), path, useBytes = TRUE)
}

format_links <- function(pairs) {
  # pairs: named character vector, e.g. c(PDF = "url", Code = "url2")
  pairs <- pairs[!is.na(pairs) & nzchar(pairs)]
  if (length(pairs) == 0) return("")
  items <- glue::glue("[{names(pairs)}]({pairs})")
  paste0("\n\n**Links:** ", paste(items, collapse = " &middot; "))
}

pub_type_legend <- c(
  "0" = "Uncategorized", "1" = "Conference paper", "2" = "Journal article",
  "3" = "Manuscript", "4" = "Report", "5" = "Book", "6" = "Book section"
)

## ---- publication -----------------------------------------------------

migrate_publication <- function(path) {
  x <- read_toml_file(path)
  m <- x$meta
  type_code <- as.character(m$publication_types[[1]] %||% "0")
  links <- c(
    PDF = m$url_pdf %||% "", Preprint = m$url_preprint %||% "",
    Code = m$url_code %||% "", Dataset = m$url_dataset %||% "",
    Project = m$url_project %||% "", Slides = m$url_slides %||% "",
    Video = m$url_video %||% "", Poster = m$url_poster %||% "",
    Source = m$url_source %||% ""
  )
  meta <- list(
    title = m$title,
    date = fmt_date(m$date),
    author = as.list(m$authors),
    subtitle = m$publication %||% NULL,
    type = unname(pub_type_legend[type_code]),
    categories = if (length(m$tags)) as.list(m$tags) else NULL,
    # NB: old `selected` field dropped — it only ever fed the
    # publications_selected/talks_selected widgets, both already inactive
    # (active=false) on the live Hugo site, so the field is dead weight.
    image = if (nzchar(m$image_preview %||% "")) paste0("/static/img/", m$image_preview) else NULL
  )
  body <- paste0(m$abstract %||% "", format_links(links))
  list(meta = meta, body = body)
}

## ---- project / package / poster (shared schema) -----------------------

migrate_project_like <- function(path) {
  x <- read_toml_file(path)
  m <- x$meta
  links <- c(Link = m$external_link %||% "")
  meta <- list(
    title = m$title,
    date = fmt_date(m$date),
    description = m$summary %||% NULL,
    categories = if (length(m$tags)) as.list(m$tags) else NULL,
    image = if (nzchar(m$image_preview %||% "")) paste0("/static/img/", m$image_preview) else NULL
  )
  body <- paste0(m$summary %||% "", format_links(links))
  list(meta = meta, body = body)
}

## ---- talk --------------------------------------------------------------

migrate_talk <- function(path) {
  x <- read_toml_file(path)
  m <- x$meta
  links <- c(
    PDF = m$url_pdf %||% "", Slides = m$url_slides %||% "",
    Video = m$url_video %||% "", Code = m$url_code %||% ""
  )
  subtitle <- paste(str_trim(c(m$event, m$location)), collapse = " — ")
  meta <- list(
    title = m$title,
    date = fmt_date(m$time_start %||% m$date),
    subtitle = subtitle,
    categories = if (length(m$tags)) as.list(m$tags) else NULL,
    image = if (nzchar(m$header$image %||% "")) paste0("/static/img/", m$header$image) else NULL
  )
  event_name <- str_trim(m$event %||% "")
  event_line <- if (nzchar(m$event_url %||% "")) {
    glue::glue("**Event:** [{event_name}]({m$event_url})")
  } else {
    glue::glue("**Event:** {event_name}")
  }
  body <- paste0(
    m$abstract %||% "", "\n\n",
    event_line, "\n\n",
    "**Location:** ", m$location %||% "",
    format_links(links)
  )
  list(meta = meta, body = body)
}

## ---- post (YAML already; normalize + convert .Rmd -> .qmd) -------------

migrate_post <- function(path) {
  x <- read_yaml_frontmatter_only(path)
  m <- x$meta
  author <- m$author %||% "Robert W Schlegel"
  meta <- list(
    title = m$title,
    author = author,
    date = fmt_date(m$date),
    categories = if (length(m$tags)) as.list(m$tags) else NULL,
    bibliography = m$bibliography %||% NULL
  )
  list(meta = meta, body = x$body_raw)
}

## ---- driver --------------------------------------------------------------

migrate_dir <- function(type, migrate_fn) {
  dir <- file.path(root, "content", type)
  files <- list.files(dir, pattern = "\\.md$", full.names = TRUE)
  files <- files[basename(files) != "_index.md"]
  for (f in files) {
    res <- migrate_fn(f)
    out <- sub("\\.md$", ".qmd", f)
    write_qmd(out, res$meta, res$body)
    file.remove(f)
    message("migrated: ", out)
  }
}

migrate_dir("publication", migrate_publication)
migrate_dir("project", migrate_project_like)
migrate_dir("package", migrate_project_like)
migrate_dir("poster", migrate_project_like)
migrate_dir("talk", migrate_talk)

# Posts: source of truth is the .Rmd (has live code); .md is Hugo's knitted
# output and is discarded. Plain .md posts with no .Rmd (none currently exist,
# but handled for robustness) are treated the same way.
post_dir <- file.path(root, "content", "post")
rmd_files <- list.files(post_dir, pattern = "\\.Rmd$", full.names = TRUE)
for (f in rmd_files) {
  res <- migrate_post(f)
  out <- sub("\\.Rmd$", ".qmd", f)
  write_qmd(out, res$meta, res$body)
  file.remove(f)
  md_sibling <- sub("\\.Rmd$", ".md", f)
  if (file.exists(md_sibling)) file.remove(md_sibling)
  message("migrated: ", out)
}
# Any remaining plain .md posts (no .Rmd source) that _index.md isn't
md_only <- list.files(post_dir, pattern = "\\.md$", full.names = TRUE)
md_only <- md_only[basename(md_only) != "_index.md"]
for (f in md_only) {
  res <- migrate_post(f)
  out <- sub("\\.md$", ".qmd", f)
  write_qmd(out, res$meta, res$body)
  file.remove(f)
  message("migrated: ", out)
}

message("Migration complete.")
