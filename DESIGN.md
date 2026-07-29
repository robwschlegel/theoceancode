<!-- SEED: established with the user before implementation; re-run /impeccable document once there's code to capture the actual tokens and components. -->

---
name: The Ocean Code
description: Personal academic portfolio for a marine heatwave researcher and data scientist
---

# Design System: The Ocean Code

## Overview

**Creative North Star: "The Research Vessel Expedition Log"**

Chosen via the impeccable new-work direction process (concept-seed.mjs, mode `read`, seed key `739d1f52`): 7 grounded directions drawn from the audience's own working world (nautical charts, oceanographic instrument panels, field-guide specimen plates, expedition logs, journal typesetting, R package documentation, chart-plotter UI) were ordered by resonance, and the seed assigned candidate 4 of 7 — the expedition log — not the top-ranked pick, refusing the model's own ranking rut. Six catalog challengers (an endurance-racing livery, a children's cutaway illustration, a risograph print system, broadcast teletext, a wax-print market wall, hand-cranked paper automata) were weighed against it on audience identification and product clarity; none had a legitimate tie to marine science strong enough to beat it. The user confirmed the assigned direction directly (no re-roll, no standing "play it straight" exit taken).

The thesis: every publication, package, project, poster, and talk is logged like a voyage entry — numbered, dated, stamped by type, a structured metadata header before the narrative body — refusing the generic "academic Bootstrap card grid" every templated researcher site (including this one's own Hugo predecessor) ships by default. This is confirmed **reference-first**: scanability and fast cross-referencing lead throughout, including the homepage, matching the confirmed audience behavior (peers, prospective students, employers cross-checking specific facts against Scholar/GitHub/a CV, not browsing for pleasure).

**Key Characteristics:**
- Cool grey-white "log paper" ground (not warm cream/parchment — a working document, not a keepsake), deep navy fountain-pen-ink text
- One accent only: a weathered verdigris-copper, reserved for verified/active states, links, and entry stamps — never a large fill (Restrained color strategy, confirmed default for a Read-mode surface)
- Three coordinated type registers from one family (IBM Plex): Mono for structured fields (dates, entry numbers, position-style metadata), Serif for reading body copy, Sans Condensed for stamped/stenciled labels
- Sharp corners, ruled-line hierarchy — no drop-shadow elevation; a logbook page has ink weight and rule lines, not card shadows
- Explicit anti-goal, confirmed by the user: avoid generic SaaS/startup gloss, sterile corporate flatness, *and* overly-academic dryness — the log framing must stay disciplined and instrument-grade (typographic and structural materiality: stamps, coordinates, numbered entries) and never tip into costume-nautical decoration (no rope borders, anchor clipart, or "ahoy" language)

## Colors

A single-accent system on a cool neutral "paper" scale — no warm cream, no generic SaaS blue.

### Primary
- **Verdigris** (`oklch` mid-tone weathered copper-green, ~`#4F7C6E`): links, active/verified entry stamps, active nav state. Never a large fill.
- **Verdigris Deep** (~`#35594F`): hover/pressed state.
- **Verdigris Tint** (~`#A8C4B9`): subtle borders, unselected/inactive stamp outlines.

### Neutral
- **Log Paper** (~`#F3F2ED`): default page background — cool, slightly grey, not warm-cream.
- **Log Paper Alt** (~`#EAEAE6`): secondary surface for banding/cards.
- **Ink** (~`#1A2332`): primary text — near-black navy, not pure black.
- **Ink Muted** (~`#5B6472`): metadata, dates, secondary text.

### Reserved
- **Oxide Red** (~`#A33B2E`): sparing use only — an alert/flag color for the rare "important" marker (e.g. a disabled-execution note), styled like a chart-annotation flag, not a UI-red CTA.

### Named Rules
**The One-Accent Rule.** Verdigris is the only brand color used with intent — links, active state, entry stamps. It never fills a large surface. (Carried forward from the prior direction's own rule — it held up under a fresh workshop, not reused by default.)

## Typography

**Structured/Data Font:** IBM Plex Mono — dates, entry numbers, position-style metadata, tags-as-signal-flags.
**Body Font:** IBM Plex Serif — reading copy (abstracts, bio, post text).
**Label Font:** IBM Plex Sans Condensed — stamped/stenciled headers and labels.

**Character:** One coordinated superfamily carrying three real registers this system actually needs — structure, reading, and labeling — not three unrelated faces stitched together, and not a single reflexive display face doing everything.

### Hierarchy
- **Title** (700, browser default h1 scale, Plex Sans Condensed): page/entry titles, inside the stamped title-block.
- **Subtitle** (400 italic, Plex Serif): venue/event line under a title, muted ink.
- **Label** (700, 0.72rem, uppercase, letter-spacing 0.08em, Plex Sans Condensed, verdigris-deep): field headings inside the entry header ("Authors", "Published").
- **Data field** (500, ~0.82–0.9rem, Plex Mono): dates, listing metadata, category tags.
- **Body** (400, Plex Serif, Bootstrap base scale): running text — abstracts, bio, post copy.

## Layout

Implemented via CSS against Quarto's own generated structures, not custom templating:
- **Entry header** (`.quarto-title-block`): every detail page's existing Quarto title-block (title/subtitle/authors/date/categories) is bordered (1px rule, 4px verdigris left rule) and given a `log-paper-alt` tint — reads as a stamped log entry using real, semantic Quarto markup, no per-file editing or custom Lua filter needed.
- **Listings** (`.quarto-listing-container-default .quarto-post`): ruled dividers between entries (bottom-border rows, a heavier top rule on the first entry) instead of card shadows; dates in Plex Mono.
- **Grid listings** (`.quarto-grid-item.card`, used by package/project/poster): sharp-cornered bordered cards, border brightens to verdigris on hover, no shadow-lift.
- **Homepage** (`.quarto-about-solana`): unchanged Quarto `about: template: solana` structure, restyled — see Shapes for the portrait treatment.

## Elevation & Depth

Flat, ruled-line hierarchy — no drop shadows anywhere (`* { box-shadow: none !important; }`, deliberately blunt). A logbook page conveys structure through ink weight, rule lines, and stamps, not card elevation. Hover states move via border-color shift (to verdigris) rather than any lift/shadow.

## Shapes

Sharp corners throughout UI chrome (buttons: 2px, inputs: 2px, cards: 0) — logbooks and ledgers are ruled and rectilinear, not soft/pill-shaped (the confirmed anti-goal: no SaaS gloss). The one deliberate exception is the homepage portrait: circular, with a double-ring verdigris border (`border: 3px double`) — a genuine seal/stamp device, not a site-wide rounding language. Category/tag markers use a small clipped notch (`clip-path`) evoking a signal flag rather than a rounded pill.

## Components

### Navbar
Log-paper background, 2px solid ink bottom rule (not a shadow seam). Condensed sans nav text; active/hover state turns verdigris with an underline, not a background fill.

### Entry type stamp
A small bordered `◆ PUBLICATION` / `◆ PACKAGE` / `◆ PROJECT` / `◆ POSTER` / `◆ TALK` / `◆ POST` kicker line above the title, on every detail page. Driven by a per-directory `content/<type>/_metadata.yml` (`entry-type: ...`) plus one shared `_partials/type-stamp.html` include (`{{< meta entry-type >}}`) — no per-file front matter needed. Deliberately **not** repeated on listing pages (every row on e.g. `publication.qmd` is already a publication; the stamp is redundant there and only earns its keep where a visitor might land without that surrounding context — search results, RSS, a direct link).

### Entry header (title-block)
Bordered box, verdigris left rule, log-paper-alt tint. Field labels ("Authors", "Published") in small uppercase Plex Sans Condensed; field values in Plex Mono. Categories render as small bordered, notch-clipped flags in verdigris-deep.

### Homepage structured fields
Position / Interests / Education render as `.log-fields` &rarr; `.log-field` blocks (ruled top/bottom, condensed-sans uppercase field labels, Plex Mono values) — the "position-style metadata block" the direction contract promised, not a generic bulleted bio.

### Listings
Default (publication/talk/blog): ruled rows, Plex Mono dates, serif-italic subtitle (venue/event), muted serif author/description line.
Grid (package/project/poster): sharp bordered cards, border-only hover state (no lift), sharp-cornered thumbnail images.

### Buttons / links (about-block)
Bordered rectangles (not pills), condensed sans uppercase label, ink border at rest, solid verdigris fill with log-paper text on hover. Icons suppressed (text-only labels) to avoid Bootstrap's generic icon glyphs breaking the instrument-label register.

### Disabled-execution notice
The one place Oxide Red appears: a left-ruled blockquote (verdigris-rule pattern reused in red), Plex Mono, styled like a chart-annotation flag rather than a generic Bootstrap alert box. Used once, on `mapping_with_ggplot2.qmd`'s maptools note.

## Do's and Don'ts

### Do:
- **Do** keep verdigris as the only accent color, reserved for links/active-state/stamps (**The One-Accent Rule**).
- **Do** build every content type's detail page and listing on the same log-entry structure (numbered, dated, stamped) — one system, not per-type ad hoc styling.
- **Do** keep the nautical/log materiality typographic and structural (stamps, coordinates, rule lines), never illustrative (no anchor/rope/wave clipart).

### Don't:
- **Don't** use a warm cream/parchment ground — the confirmed palette is cool grey-white "log paper," not a keepsake-journal aesthetic.
- **Don't** add drop-shadow card elevation — hierarchy comes from ink weight and rule lines.
- **Don't** let the log framing become twee or costume-nautical ("ahoy" copy, decorative rope/anchor icons) — instrument-grade discipline is the confirmed anti-goal boundary.
- **Don't** treat this as the final token set — spacing, exact type scale, and component specifics are placeholders pending the first real implementation pass.
