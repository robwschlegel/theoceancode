---
name: The Ocean Code
description: Personal academic portfolio for a marine heatwave researcher and data scientist
colors:
  primary: "#3f51b5"
  primary-light: "#757de8"
  primary-dark: "#002984"
  menu-text-active: "#8c9eff"
  neutral-surface: "#ffffff"
  neutral-surface-alt: "#f7f7f7"
  neutral-heading: "#313131"
  neutral-title: "#151515"
  neutral-body: "rgba(0,0,0,0.8)"
  neutral-muted: "#888888"
  neutral-border: "rgba(0,0,0,0.09)"
  neutral-divider: "rgba(0,0,0,0.05)"
typography:
  display:
    fontFamily: "Lato, sans-serif"
    fontSize: "2.7rem"
    fontWeight: 400
    lineHeight: 1
    letterSpacing: "normal"
  headline:
    fontFamily: "Lato, sans-serif"
    fontSize: "2.25rem"
    fontWeight: 400
    lineHeight: 1.25
    letterSpacing: "normal"
  title:
    fontFamily: "Lato, sans-serif"
    fontSize: "1.75rem"
    fontWeight: 400
    lineHeight: 1.25
    letterSpacing: "normal"
  body:
    fontFamily: "Merriweather, serif"
    fontSize: "1rem"
    fontWeight: 400
    lineHeight: 1.65
    letterSpacing: "normal"
  label:
    fontFamily: "Lato, sans-serif"
    fontSize: "0.9rem"
    fontWeight: 700
    lineHeight: 1.5
    letterSpacing: "0.03em"
  mono:
    fontFamily: "Roboto Mono, monospace"
    fontSize: "0.875em"
    fontWeight: 400
    lineHeight: 1.4
    letterSpacing: "normal"
rounded:
  sm: "3px"
  lg: "10px"
  pill: "1rem"
  full: "50%"
spacing:
  xs: "8px"
  sm: "16px"
  md: "20px"
  lg: "60px"
  xl: "110px"
components:
  button-primary:
    backgroundColor: "{colors.primary}"
    textColor: "#ffffff"
    rounded: "4px"
    padding: "6px 12px"
  button-light:
    backgroundColor: "#ffffff"
    textColor: "{colors.primary}"
    rounded: "4px"
    padding: "6px 12px"
  card:
    backgroundColor: "{colors.neutral-surface}"
    rounded: "{rounded.sm}"
    padding: "0.75rem 1rem 0.75rem"
  card-simple:
    backgroundColor: "{colors.neutral-surface}"
    rounded: "{rounded.sm}"
    padding: "15px 20px"
---

# Design System: The Ocean Code

## Overview

**Creative North Star: "The Tide Chart"**

The site behaves like a navigational reference, not a brochure: dense, real information (papers, code, credentials) laid out to be scanned and cross-referenced quickly by people who already know what they're looking for. Confirmed direction is oceanic, but the current implementation hasn't fully arrived there yet — the "Ocean" theme's primary color is a royal indigo (`#3f51b5`) inherited from Hugo Academic's built-in preset, not a literal ocean hue. It's the working primary for now and stays structurally load-bearing (links, buttons, active nav), but it's provisional: a future pass may shift it toward an oceanic teal/cyan register without touching layout, type, or the rest of the system.

The voice is restrained and credible without being sterile. Body copy sits in a serif (Merriweather) at an unusually generous size — 20px on desktop, 16px on mobile, around a quarter larger than a typical web default — which gives long-form writing (posts, abstracts, the bio) the unhurried, page-like register of a printed research notebook rather than a typical web app. Headings, navigation, and labels switch to a brisk sans-serif (Lato) for contrast and wayfinding.

**Key Characteristics:**
- Serif body text at a generous base size; sans-serif for headings, nav, and labels
- One accent color (indigo, provisional) used sparingly — links, primary buttons, active nav state, icon accents — never as a large fill
- Alternating white / pale-gray banded sections stack down the one-page homepage instead of container borders
- Two uncoordinated radius languages: a tight 3px on cards and tags, full circles on avatars and icon links, plus two one-off exceptions (pill search field, 10px alerts)
- Flat surfaces at rest; an inherited Bootstrap shadow-lift on grid-card hover that is documented here, not treated as a deliberate rule

## Colors

A single-accent system: one working indigo, a wide neutral scale for text and surfaces, and a handful of Bootstrap-inherited semantic colors for buttons and alerts.

### Primary
- **Ocean Indigo** (#3f51b5): links, primary buttons, navbar background, featurette icons. Provisional per the confirmed "Tide Chart" direction — treat as the current, not final, brand color.
- **Ocean Indigo Light** (#757de8): active/pressed state on toolbar buttons.
- **Ocean Indigo Dark** (#002984): hover state on toolbar buttons.

### Neutral
- **Active Indigo** (#8c9eff): nav link hover/active text against the solid-indigo navbar — the one place a lighter tint of the accent carries text instead of the base indigo.
- **Paper White** (#ffffff): default page background, odd-numbered home sections, card backgrounds.
- **Pale Gray** (#f7f7f7): even-numbered home sections — sections alternate every block down the homepage.
- **Heading Charcoal** (#313131): default h1–h6 color.
- **Title Ink** (#151515): article and post titles specifically, darker than generic headings.
- **Body** (rgba(0,0,0,0.8)): base running text.
- **Muted** (#888888): metadata, dates, bylines.
- **Border** (rgba(0,0,0,0.09)): `.card-simple` borders.
- **Divider** (rgba(0,0,0,0.05)): `hr-light` rules.

### Named Rules
**The One-Accent Rule.** Indigo is the only brand color used with intent, and only for interactive/wayfinding purposes — links, primary buttons, active nav, icon accents. It never fills a large surface.

### Inherited, not canonical
A few colors appear outside this palette: publication-author bylines in blue (#3170A5), publication-venue names in green (#090), and search-match highlighting in amber (#FFE0B2 background / #E65100 text). These predate the deliberate system and are not confirmed as intentional signal colors — document them as legacy, don't extend the pattern into new work.

## Typography

**Display Font:** Lato (with sans-serif fallback)
**Body Font:** Merriweather (with serif fallback)
**Label/Mono Font:** Roboto Mono for code, Lato for nav and labels

**Character:** A sans/serif split doing real work: Lato carries headings, navigation, and labels with brisk, upright authority, while Merriweather sets body copy at a generous size, giving longer writing the unhurried register of a printed page rather than a typical web app.

### Hierarchy
- **Display** (400, 2.7rem, line-height 1): hero title only.
- **Headline** (400, 2.25rem, line-height 1.25): h1.
- **Title** (400, 1.75rem, line-height 1.25): article and post titles.
- **Body** (400, 1rem — 20px desktop / 16px mobile base, line-height 1.65): running text; noticeably larger than a typical 16px web default.
- **Label** (700, 0.9rem, uppercase, letter-spacing 0.03em): card titles, nav brand wordmark.

### Named Rules
**The Large-Print Rule.** Base body font-size is 20px on screens ≥58em (16px below that). It's roughly 25% larger than a typical web default and is load-bearing for the "printed page" register the serif body font is chosen for — don't shrink it back toward 16px.

## Layout

Container: `.universal-wrapper` caps at 1000px, centered, with 15px side padding below its 1001px breakpoint.
Reading measure: `.article-container` narrows further to 760px for post/article copy.
Homepage: a vertical stack of full-width `.home-section` blocks, alternating white/pale-gray backgrounds, each with heavy vertical padding — 110px top/bottom on desktop, 60px on mobile (the first section is 50px/40px). This banding is how the one-page site segments Publications / Packages / Projects / Blog / Talks / Posters / Workshops / Contact without any container borders.
Navbar: fixed-height bar (70px desktop, 50px mobile below a separate 1200px breakpoint) that offsets body content via a top margin.
Grid content (projects, publications): a filtered/masonry card grid (isotope.js).

## Elevation & Depth

Mostly flat: surfaces sit directly on the white/pale-gray section backgrounds with no shadow at rest. Two exceptions carry real shadow: the fixed navbar (a faint seam) and grid cards, which jump from a light rest shadow to a noticeably heavier one on hover. That hover-lift is an inherited Bootstrap-card default, not a deliberately designed interaction — document it as current behavior, don't extend it as a philosophy into new components.

### Shadow Vocabulary
- **Navbar seam** (`box-shadow: 0 0.125rem 0.25rem 0 rgba(0,0,0,.11)`): separates the fixed nav from scrolled content.
- **Card rest** (`box-shadow: 0 2px 4px 0 rgba(0,0,0,0.2)`): default grid-card state.
- **Card hover** (`box-shadow: 0 8px 16px 0 rgba(0,0,0,0.2)`): inherited default, not a confirmed rule.

## Shapes

Two radius languages coexist without a clear bridge: a tight 3px on content surfaces (cards, tags/labels) that reads almost square, versus full circles (50%) on avatars and icon-link buttons. Two one-off exceptions sit outside both — the search field is pill-shaped (1rem radius) and alert boxes use a wider 10px radius. This is the system as observed, not a resolved rule; a future pass should converge on fewer radius values rather than carry all four forward.

## Components

### Buttons
- **Shape:** 4px radius — a Bootstrap 3.3.7 default, not overridden by theme CSS.
- **Primary:** solid indigo fill (`background`/`border-color: #3f51b5`), white text.
- **Light:** white fill, used on dark hero overlays.
- **Hover/Focus:** inconsistent across contexts. Toolbar buttons darken to Ocean Indigo Dark on hover and lighten to Ocean Indigo Light when active, but the standalone `.btn-primary` hover rule reassigns the same base indigo — outside the toolbar, hovering a primary button currently produces no visible color change. Documented as a real gap, not a pattern to replicate.
- **Outline variants:** transparent fill, colored border and text (primary indigo plus inherited Bootstrap semantic colors for success/info/warning/danger), filling to solid color on hover.

### Cards
Two variants serve different jobs:
- **`.card-simple`** — bordered and minimal: 1px `rgba(0,0,0,.09)` border, 3px radius, faint shadow, white background, 15px/20px padding. Used for simple content blocks.
- **`.card`** (grid/image cards) — image-forward, with an uppercase bold 0.9rem title and muted 0.75rem body copy; hover reveals an overlay icon on the image and lifts the shadow (see Elevation).

### Tags / Labels
- **Style:** translucent black background/text (`rgba(0,0,0,.05)` / `rgba(0,0,0,.68)`), 3px radius, 5px/10px padding. Used for post tags and categories.

### Search Input
- **Style:** pill-shaped (1rem radius), 1px `#dedede` border, left-padded for an inline search icon.
- **Match highlight:** amber background/text on matched terms (a legacy accent — see Colors).

### Navigation
Fixed top bar filled solid indigo, white nav text that shifts to Active Indigo on hover/active, bold uppercase brand wordmark. Collapses to a hamburger menu below the 1200px breakpoint.

### Alerts
- **Style:** solid color fill (info: #03A9F4, warning: #f44336), white text, 10px radius, left-inset icon.

### Avatar
- **Shape:** a perfect circle (50% radius), single portrait image, centered — the one place circular form appears outside icon buttons.

## Do's and Don'ts

### Do:
- **Do** keep the accent color singular and sparing — indigo appears only on links, primary buttons, active nav state, and icon accents, never as a large fill (**The One-Accent Rule**).
- **Do** set body copy in the serif body font at the current oversized base, 20px desktop / 16px mobile — it's what gives long-form writing its notebook-like register (**The Large-Print Rule**).
- **Do** use the alternating white/pale-gray section banding to segment homepage content, instead of adding borders or container backgrounds.

### Don't:
- **Don't** extend the legacy accent colors (publication-author blue, publication-venue green, search-highlight amber) into new components; they predate the deliberate palette.
- **Don't** treat the card hover shadow-lift as a confirmed interaction pattern to replicate elsewhere — it's an inherited Bootstrap default, not a chosen rule.
- **Don't** add a fifth radius value. The system already carries four uncoordinated ones (3px / 1rem / 50% / 10px); resolve toward fewer, don't add another.
- **Don't** treat the current indigo as permanent. Per the confirmed "Tide Chart" direction, a future pass may shift the primary toward an oceanic teal/cyan — don't build new components that assume indigo is final.
