# Product

<!-- impeccable:product-schema 1 -->

## Platform

web

## Users

Three confirmed audiences, all visiting to evaluate Robert William Schlegel and his work:
- Academic peers and collaborators — checking publications, methods, and research fit for potential collaboration.
- Prospective students/postdocs — evaluating him as a supervisor or team member, and research fit.
- Employers/grant committees — assessing CV, track record, and role for hiring or funding decisions.

## Product Purpose

A personal academic portfolio site ("The Ocean Code") for Robert William Schlegel, Data Scientist on the OMTAB team at the Laboratoire d'Océanographie de Villefranche, working on the RiOMar and HYPERNETS projects. It is a general reference hub — publications, projects, R packages, talks, posts, CV — that visitors self-serve from. Confirmed: there is no single call-to-action the site should drive toward; it stays a broad profile/CV rather than a funnel.

## Positioning

Research focus: marine heatwaves, climate change, biodiversity, machine learning, data science. Distinct output includes original R packages (heatwaveR, coastR, FjordLight, heatwave3) — the site is as much a home for reusable research software as for publications.

## Operating Context

Content types the site serves, each with existing content: home (about/bio + contact, on `index.qmd`/`contact.qmd`), publications (18 entries), packages (4: heatwaveR, coastR, FjordLight, heatwave3), projects (3: FjordLight, MHWtracker, demoMHW), posters (1), talks (2), blog posts (14, one with code execution disabled — see Capabilities and Constraints). Each content type has its own listing page (`publication.qmd`, `package.qmd`, `project.qmd`, `poster.qmd`, `talk.qmd`, `blog.qmd`) with built-in tag filtering, search, and RSS. The old Hugo-era "teaching"/"workshops"/"skills" home widgets were inactive placeholder content and were dropped during the Quarto migration, not carried forward.

## Capabilities and Constraints

- Deployed via Netlify (netlify.toml), building through the official `@quarto/netlify-plugin-quarto` plugin — deploy process must keep working. Confirmed as the one binding constraint by the user.
- Built with [Quarto](https://quarto.org)'s website project type (`_quarto.yml`), replacing the former Hugo/hugo-academic/blogdown stack entirely (see CLAUDE.md for the full rationale). Content is native `.qmd` with YAML front matter; posts with R code execute via Quarto's own knitr engine, with `_freeze/` caching results since Netlify's build servers can't execute R.
- No bespoke visual theme currently — the site ships on Quarto's stock `cosmo` theme. This is the open item the current design pass addresses.
- All 14 blog posts execute their R code normally, including `mapping_with_ggplot2.qmd` — its one dependency on the archived `maptools` package (a single great-circle-destination function used by a sourced scale-bar helper) was replaced with a small self-contained implementation, not routed around.

## Brand Commitments

- Site title: "The Ocean Code". Owner: Robert William Schlegel, Data Scientist, LOV (Laboratoire d'Océanographie de Villefranche).
- Confirmed identity/contact channels: email (robwschlegel@gmail.com), Google Scholar, GitHub (robwschlegel), CV (files/cv.pdf). Twitter was dropped by explicit request — not to be promoted anywhere on the site.
- Real profile photos: `static/img/lov-member-robert-schlegel.jpg` (current homepage photo), plus alternates `static/img/Schlegel_profile_small.JPG` and `static/img/Robert_profile_1.JPG`. `static/img/portrait.jpg` is **not** a real photo — it's an unused generic-avatar placeholder that shipped with the original hugo-academic theme scaffold (present since the repo's first commit); do not use it as an avatar source.

## Evidence on Hand

- Real, populated content for all major sections: 31 posts, 18 publications, 3 projects, 4 packages, talks, posters. No placeholder/lorem content to avoid — treat existing entries as real evidence.
- Research imagery on hand in `static/img/` (marine heatwave tracker screenshots, package figures, field photos) — usable as real assets rather than requiring stock imagery.
- No formal accessibility standard or additional audience was raised; none should be assumed beyond baseline good practice.

## Product Principles

1. The site is a reference hub, not a conversion funnel — every visitor type (peers, students, employers) should be able to self-serve the specific proof they came for (papers, code, credentials) without being pushed toward one CTA.
2. Research software (R packages) is a first-class output alongside papers — surface it with the same seriousness as publications, not as a secondary afterthought.
3. Credibility comes from real, verifiable evidence already in the repo (publications, CV, ORCID/Scholar, GitHub) — never invent metrics, testimonials, or affiliations.
4. Netlify deployability is the one hard operational constraint; the underlying theme/tooling is otherwise open to change if a future request calls for it.

## Accessibility & Inclusion

No product-specific requirement was established during this interview.
