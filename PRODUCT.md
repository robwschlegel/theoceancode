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

Content types the site serves, each with existing content: home (about/bio, hero, contact, skills, tags, search), posts (31 entries), publications (18 entries), projects (3: FjordLight, MHWtracker, demoMHW), packages (4: heatwaveR, coastR, FjordLight, heatwave3), talks (2 + index), posters, teaching, workshops.

## Capabilities and Constraints

- Deployed via Netlify (netlify.toml) — deploy process must keep working. Confirmed as the one binding constraint by the user.
- Currently built with Hugo static site generator on the `hugo-academic` theme (config.toml `theme = "hugo-academic"`), with content partly authored in R Markdown and knit to Markdown (`ignoreFiles` in config.toml excludes `.Rmd`/`.knit.md`). These are recorded as current implementation facts, not user-confirmed must-preserve constraints — the user did not flag the theme or the R/Rmd pipeline as binding, so future design work is not restricted to them unless stated otherwise.
- Current theme params: color_theme = "ocean", font = "classic".

## Brand Commitments

- Site title: "The Ocean Code". Owner: Robert William Schlegel, Data Scientist, LOV (Laboratoire d'Océanographie de Villefranche).
- Confirmed identity/contact channels: email (robwschlegel@gmail.com), Google Scholar, GitHub (robwschlegel), CV (files/cv.pdf). Twitter was dropped by explicit request — not to be promoted anywhere on the site.
- Existing avatar/profile images: `static/img/Schlegel_profile_small.JPG`, `static/img/Robert_profile_1.JPG`, `static/img/portrait.jpg`.

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
