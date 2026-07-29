# REMEMBER to restart R after you modify and save this file!

# First, execute the global .Rprofile if it exists.
if (file.exists("~/.Rprofile")) {
  base::sys.source("~/.Rprofile", envir = environment())
}

# The site is built with Quarto directly -- no blogdown. blogdown was tried
# first (method = 'custom' + R/build.R calling quarto::quarto_render()), but
# once all content became native .qmd, blogdown was just an extra layer of
# indirection around a single quarto::quarto_render() call, and it added a
# real trap: blogdown::serve_site() has no 'custom' method support at all
# (unlike build_site()) and always assumes a Hugo/Jekyll/Hexo site, so it
# fails looking for hugo.toml/config.toml that no longer exist. Simpler and
# safer to call the quarto package directly:
#   quarto::quarto_render()   -- rebuild the whole site (in R or the console)
#   quarto::quarto_preview()  -- live-reloading local preview while editing
# RStudio also has native Quarto project support (Render/Serve in the Build
# pane) once it detects _quarto.yml, with no site-generator marker file
# needed -- that's what index.Rmd used to be for under blogdown.

# Make sure the Quarto CLI is on PATH even when R wasn't launched from a
# shell that sourced ~/.zprofile (e.g. RStudio.app opened from Finder/Dock).
quarto_bin <- path.expand("~/opt/quarto/bin")
if (dir.exists(quarto_bin) && !grepl(quarto_bin, Sys.getenv("PATH"), fixed = TRUE)) {
  Sys.setenv(PATH = paste(quarto_bin, Sys.getenv("PATH"), sep = .Platform$path.sep))
}
