# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this is

A personal academic portfolio site ("The Ocean Code") for Robert William Schlegel, built with [Quarto](https://quarto.org)'s website project type. Previously ran on Hugo + a vendored `hugo-academic` theme + `blogdown`; both were fully removed and replaced (see PRODUCT.md for audience/purpose, and the git history around the migration for the full rationale). See `PRODUCT.md` for audience, purpose, and content inventory. `DESIGN.md` is **stale** — it documents the old Hugo "Ocean" indigo/serif visual system, which no longer exists; the site currently ships on Quarto's plain `cosmo` theme with no bespoke visual identity. Treat DESIGN.md as historical only, not as a spec for the current UI, until it's regenerated.

## Build & serve — direct Quarto, no blogdown

Quarto CLI is installed at `~/opt/quarto/bin` (not via Homebrew cask — that needs `sudo`; this was a manual tarball install), on `PATH` via `~/.zprofile` and re-asserted in `.Rprofile` for R sessions that don't inherit shell PATH (e.g. RStudio.app launched from Finder).

- **Rebuild the whole site**: `quarto render` (terminal) or `quarto::quarto_render()` (R/RStudio console).
- **Live preview while editing**: `quarto preview` (terminal) or `quarto::quarto_preview()` (R/RStudio console) — file-watching, live-reloading dev server.
- RStudio/Positron also have native Quarto project support (Render/Serve controls) once they detect `_quarto.yml` — no site-generator marker file needed.

**blogdown was deliberately removed, not just left unconfigured.** It was tried first (`.Rprofile`: `blogdown.method = 'custom'`, plus `R/build.R` calling `quarto::quarto_render()`), since blogdown can be configured to hand off to an arbitrary build script. But once all content became native `.qmd`, blogdown added nothing beyond an extra indirection layer around a single `quarto::quarto_render()` call — and it left a real trap: `blogdown::serve_site()` has **no** `method = 'custom'` support at all (unlike `build_site()`), so it always assumes a Hugo/Jekyll/Hexo project and fails looking for `hugo.toml`/`config.toml`, which no longer exist. Confirmed directly by reading blogdown's installed source (`blogdown:::serve_it`, `blogdown:::site_root`) and reproducing the failure. If you ever see an error like `Could not find 'hugo.toml' / ... / 'config.toml'` from a `site_root()` call, something is invoking blogdown again (`serve_site()`, an old RStudio "Serve Site" addin binding, muscle memory) — don't chase a Hugo config fix; use the direct Quarto commands above instead.

Netlify (`netlify.toml`) builds via the official `@quarto/netlify-plugin-quarto` plugin (declared as a dependency in `package.json`), publishing `_site`. Netlify's build servers cannot execute R/Python/Julia code, so `_quarto.yml` sets `execute: freeze: auto` — any `.qmd` with real code chunks must be rendered locally first so results get cached under `_freeze/`, which **must be committed** (unlike `_site/`, which is gitignored build output, same discipline as the old `public/` mistake — never commit it).

## Content authoring pipeline

- Content lives under `content/<type>/*.qmd` (`publication`, `project`, `package`, `poster`, `talk`, `post`), plain YAML front matter. Drop a new `.qmd` in the right directory and it's picked up automatically by that type's listing page (`publication.qmd`, `package.qmd`, `project.qmd`, `poster.qmd`, `talk.qmd`, `blog.qmd` at the project root) via Quarto's native `listing:` feature — no registration step needed.
- Per-type front-matter pattern (not enforced by schema, just convention — match the sibling files in each directory): `title`, `date`, `categories` (drives Quarto's tag filter UI), plus type-specific fields — `author`/`subtitle` for publications, `description` for package/project/poster, `subtitle` (event — location) for talks. Optional links (PDF/code/dataset/slides/video/etc.) are written as a plain markdown line in the body (`**Links:** [PDF](url) &middot; [Code](url)`), not a custom shortcode.
- `content/post/*.qmd` may contain real R code chunks — Quarto's own knitr engine executes them (no separate knitting step). `scripts/migrate-content.R` documents the exact field mapping used to migrate off Hugo, kept for reference.
- One post, `content/post/mapping_with_ggplot2.qmd`, has `execute: enabled: false` — it depends on `maptools`, archived from CRAN in 2023 and no longer compilable against current R. Re-enabling it needs the mapping code rewritten against `sf` or equivalent.
