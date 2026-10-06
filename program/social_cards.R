# Social media (Open Graph) cards for every session, written to img/social/ as
# <code>.svg and <code>.png (1200x630).
#
# The SVG is self-contained (fonts, photos and logo embedded as data URIs) and
# is the source of truth; the PNG is a screenshot of it from headless Chrome.
# Run from the project root, after update_sessions.R:
#
#   Rscript program/social_cards.R
#
# Card details that aren't in pretalx (short speaker roles, card titles,
# hosts) live in program/social_cards.yml. The fonts in program/fonts/ are
# Latin subsets of Cabin and PT Sans from Google Fonts (OFL); text outside
# Latin-1/Latin Extended-A would fall back to a system font.

library(purrr)

out_dir <- "img/social"
cfg <- yaml::read_yaml("program/social_cards.yml")
chrome <- Sys.which(c("google-chrome", "chromium", "chromium-browser"))
chrome <- Sys.getenv("CHROME", unname(chrome[nzchar(chrome)][1]))

W <- 1200
H <- 630
pad <- c(top = 40, side = 64, bottom = 36)
navy <- "#232c78"
orange <- "#fbac27"
muted <- "#c3c9f2"

font <- list(
  cabin_600 = "program/fonts/Cabin-SemiBold.woff2",
  cabin_700 = "program/fonts/Cabin-Bold.woff2",
  pt_400 = "program/fonts/PTSans-Regular.woff2",
  pt_700 = "program/fonts/PTSans-Bold.woff2"
)

# Text metrics --------------------------------------------------------------

text_width <- function(x, f, size) {
  systemfonts::string_width(x, path = f, size = size, res = 72)
}

# Baseline of a single line of text whose line box (height `lh`) starts at
# `top`, matching how browsers centre the font's content area in a line box.
baseline <- function(top, f, size, lh = NULL) {
  info <- systemfonts::font_info(path = f, size = size, res = 72)
  asc <- info$max_ascend
  desc <- -info$max_descend
  lh <- lh %||% (asc + desc)
  top + (lh - (asc + desc)) / 2 + asc
}

wrap_text <- function(text, width, f, size) {
  words <- strsplit(text, " ", fixed = TRUE)[[1]]
  lines <- character()
  current <- ""
  for (word in words) {
    candidate <- if (nzchar(current)) paste(current, word) else word
    if (nzchar(current) && text_width(candidate, f, size) > width) {
      lines <- c(lines, current)
      current <- word
    } else {
      current <- candidate
    }
  }
  c(lines, current)
}

# SVG helpers ---------------------------------------------------------------

xml_escape <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  gsub(">", "&gt;", x, fixed = TRUE)
}

# Titles and names from the .qmd front matter are HTML-escaped.
html_unescape <- function(x) {
  x <- gsub("&lt;", "<", x, fixed = TRUE)
  x <- gsub("&gt;", ">", x, fixed = TRUE)
  x <- gsub("&quot;", "\"", x, fixed = TRUE)
  x <- gsub("&#39;", "'", x, fixed = TRUE)
  gsub("&amp;", "&", x, fixed = TRUE)
}

data_uri <- function(path, type) {
  paste0("data:", type, ";base64,", base64enc::base64encode(path))
}

# Pointy-top hexagon filling the box (x, y, w, h).
hex_points <- function(x, y, w, h) {
  px <- c(x + w / 2, x + w, x + w, x + w / 2, x, x)
  py <- c(y, y + h / 4, y + 3 * h / 4, y + h, y + 3 * h / 4, y + h / 4)
  paste(sprintf("%.1f,%.1f", px, py), collapse = " ")
}

svg_text <- function(x, y, label, f, size, fill = "#ffffff", anchor = "start", spacing = 0) {
  family <- switch(f,
    cabin_600 = , cabin_700 = "'Cabin'",
    pt_400 = , pt_700 = "'PT Sans'"
  )
  weight <- switch(f, cabin_600 = 600, cabin_700 = 700, pt_400 = 400, pt_700 = 700)
  sprintf(
    '<text x="%.1f" y="%.1f" font-family="%s, sans-serif" font-weight="%d" font-size="%s" fill="%s" text-anchor="%s"%s>%s</text>',
    x, y, family, weight, size, fill, anchor,
    if (spacing) sprintf(' letter-spacing="%.2f"', spacing) else "",
    label
  )
}

font_faces <- local({
  face <- function(family, weight, path) {
    sprintf(
      "@font-face{font-family:'%s';font-weight:%d;src:url(%s) format('woff2')}",
      family, weight, data_uri(path, "font/woff2")
    )
  }
  paste0(
    face("Cabin", 600, font$cabin_600),
    face("Cabin", 700, font$cabin_700),
    face("PT Sans", 400, font$pt_400),
    face("PT Sans", 700, font$pt_700)
  )
})

logo_uri <- data_uri("img/wombat/hex-orange.svg", "image/svg+xml")
logo_ratio <- 1650 / 1905

# Crop a photo to the given box around a focus point (like CSS object-fit:
# cover with object-position), at 2x for sharpness, and return a data URI.
photo_uri <- function(path, w, h, position) {
  img <- magick::image_read(path)
  info <- magick::image_info(img)
  focus <- as.numeric(sub("%", "", strsplit(position, " ")[[1]])) / 100
  scale <- max(w / info$width, h / info$height)
  sw <- round(w / scale)
  sh <- round(h / scale)
  ox <- round(focus[1] * (info$width - sw))
  oy <- round(focus[2] * (info$height - sh))
  img <- magick::image_crop(img, magick::geometry_area(sw, sh, ox, oy))
  img <- magick::image_resize(img, magick::geometry_size_pixels(round(2 * w), round(2 * h), preserve_aspect = FALSE))
  img <- magick::image_background(img, "#1a2160")
  tmp <- tempfile(fileext = ".jpg")
  magick::image_write(img, tmp, format = "jpeg", quality = 85)
  data_uri(tmp, "image/jpeg")
}

# Session data --------------------------------------------------------------

read_session <- function(path) {
  front <- rmarkdown::yaml_front_matter(path)
  code <- tools::file_path_sans_ext(basename(path))
  day <- basename(dirname(path))
  opts <- cfg$sessions[[code]] %||% list()

  # A single speaker with no code is pretalx's "no speakers" placeholder.
  speakers <- front$speaker
  if (!is.null(names(speakers))) speakers <- list(speakers)
  speakers <- keep(speakers, \(s) nzchar(s$code %||% ""))
  if (!is.null(opts$order)) {
    speakers <- speakers[order(match(map_chr(speakers, "code"), opts$order))]
  }
  speakers <- map(speakers, \(s) {
    info <- cfg$speakers[[s$code]] %||% list()
    list(
      name = html_unescape(s$name),
      role = info$role %||% "",
      photo = sub("^/", "", s$avatar_url),
      position = info$position %||% "50% 35%",
      host = identical(s$code, opts$host)
    )
  })
  # The host goes last, in the bottom hexagon.
  speakers <- c(discard(speakers, "host"), keep(speakers, "host"))

  start <- as.POSIXct(front$date, tz = "Australia/Melbourne")
  when <- paste(
    trimws(gsub("\\s+", " ", format(start, "%a %e %b"))),
    "·",
    trimws(format(start, "%l:%M %p"))
  )

  list(
    code = code,
    kind = opts$kind %||% cfg$kinds[[day]],
    when = when,
    title = html_unescape(opts$title %||% front$title),
    speakers = speakers
  )
}

# Layout --------------------------------------------------------------------

# Photo hexagons as (x, y) offsets within a box of width box_w; height h.
photo_layout <- function(n) {
  g <- 10
  if (n == 0 || n == 1) {
    h <- 420
    box_w <- 430
    w <- round(h * 0.866)
    spots <- list(c((box_w - w) / 2, 0))
  } else if (n == 2) {
    h <- 250
    box_w <- 430
    w <- round(h * 0.866)
    x0 <- (box_w - 1.5 * w - g / 2) / 2
    spots <- list(c(x0, 0), c(x0 + w / 2 + g / 2, 0.75 * h + g))
  } else if (n == 3) {
    h <- 260
    box_w <- 500
    w <- round(h * 0.866)
    x0 <- (box_w - 2 * w - g) / 2
    spots <- list(c(x0, 0), c(x0 + w + g, 0), c((box_w - w) / 2, 0.75 * h + g))
  } else {
    stop("No card layout for ", n, " speakers")
  }
  list(h = h, w = w, box_w = box_w, spots = spots, box_h = max(map_dbl(spots, 2)) + h)
}

background_hexes <- function() {
  # x, y, w, h, fill: larger and smaller hexagons that don't overlap.
  hexes <- list(
    c(790, -260, 560, 646, "#2e3994"),
    c(-190, 445, 420, 485, "#2e3994"),
    c(590, -70, 140, 162, "#2e3994"),
    c(700, 104, 80, 92, "#2b3590"),
    c(-50, 150, 120, 139, "#2b3590"),
    c(340, 485, 100, 115, "#2b3590"),
    c(650, 541, 190, 219, "#2e3994"),
    c(960, 430, 64, 74, "#2e3994"),
    c(1110, 497, 150, 173, "#2b3590")
  )
  map_chr(hexes, \(v) {
    n <- as.numeric(v[1:4])
    sprintf('<polygon points="%s" fill="%s"/>', hex_points(n[1], n[2], n[3], n[4]), v[5])
  })
}

card_svg <- function(s) {
  n <- length(s$speakers)
  out <- c(
    sprintf('<svg xmlns="http://www.w3.org/2000/svg" width="%d" height="%d" viewBox="0 0 %d %d">', W, H, W, H),
    sprintf("<style>%s</style>", font_faces),
    sprintf('<rect width="%d" height="%d" fill="%s"/>', W, H, navy),
    background_hexes()
  )

  # Footer: tagline left, website right, under a faint rule.
  footer_lh <- 18 * 1.3
  footer_top <- H - pad[["bottom"]] - footer_lh
  rule_y <- footer_top - 14
  footer_base <- baseline(footer_top, font$pt_400, 18, footer_lh)
  out <- c(
    out,
    sprintf('<line x1="%d" x2="%d" y1="%.1f" y2="%.1f" stroke="%s" stroke-opacity="0.3"/>', pad[["side"]], W - pad[["side"]], rule_y, rule_y, muted),
    svg_text(pad[["side"]], footer_base, "Learn and discuss open-source tools for business analytics and data science", "pt_400", 18, muted),
    svg_text(W - pad[["side"]], footer_base, "wombat2026.numbat.space", "pt_700", 18, anchor = "end")
  )
  main_top <- pad[["top"]]
  main_bottom <- rule_y - 22

  # Photos (or the logo, for sessions without speakers), centred vertically.
  lay <- photo_layout(n)
  box_x <- W - pad[["side"]] - lay$box_w
  box_y <- main_top + (main_bottom - main_top - lay$box_h) / 2
  if (n == 0) {
    sp <- lay$spots[[1]]
    out <- c(out, sprintf(
      '<image x="%.1f" y="%.1f" width="%.1f" height="%d" href="%s"/>',
      box_x + sp[1], box_y + sp[2], lay$w, lay$h, logo_uri
    ))
  }
  for (i in seq_len(n)) {
    p <- s$speakers[[i]]
    x <- box_x + lay$spots[[i]][1]
    y <- box_y + lay$spots[[i]][2]
    clip <- sprintf("photo%d", i)
    iw <- lay$w - 14
    ih <- lay$h - 16
    out <- c(
      out,
      sprintf('<polygon points="%s" fill="%s"/>', hex_points(x, y, lay$w, lay$h), orange),
      sprintf('<clipPath id="%s"><polygon points="%s"/></clipPath>', clip, hex_points(x + 7, y + 8, iw, ih)),
      sprintf(
        '<image x="%.1f" y="%.1f" width="%d" height="%d" clip-path="url(#%s)" preserveAspectRatio="none" href="%s"/>',
        x + 7, y + 8, iw, ih, clip, photo_uri(p$photo, iw, ih, p$position)
      )
    )
    if (p$host) {
      spacing <- 15 * 0.14
      tag_w <- text_width("HOST", font$pt_700, 15) + 4 * spacing + 28
      tag_h <- 15 * 1.3 + 10
      tag_y <- y + lay$h - 14
      out <- c(
        out,
        sprintf(
          '<rect x="%.1f" y="%.1f" width="%.1f" height="%.1f" rx="%.1f" fill="%s"/>',
          x + lay$w / 2 - tag_w / 2, tag_y, tag_w, tag_h, tag_h / 2, orange
        ),
        # Trailing letter-spacing after the last letter is offset by half a gap.
        svg_text(x + lay$w / 2 + spacing / 2, baseline(tag_y + 5, font$pt_700, 15, 15 * 1.3), "HOST", "pt_700", 15, navy, "middle", spacing)
      )
    }
  }

  # Header: logo with "WOMBAT 2026" and the organiser line.
  col_x <- pad[["side"]]
  col_w <- box_x - 48 - col_x
  logo_h <- 96
  logo_w <- logo_h * logo_ratio
  head_x <- col_x + logo_w + 16
  head_h <- 61 + 4 + 17 * 1.25
  head_top <- main_top + (logo_h - head_h) / 2
  out <- c(
    out,
    sprintf('<image x="%d" y="%d" width="%.1f" height="%d" href="%s"/>', col_x, main_top, logo_w, logo_h, logo_uri),
    sprintf(
      '<text x="%.1f" y="%.1f" font-family="\'Cabin\', sans-serif" font-weight="700" font-size="61" fill="#ffffff">WOMBAT <tspan fill="%s">2026</tspan></text>',
      head_x, baseline(head_top, font$cabin_700, 61, 61), orange
    ),
    svg_text(head_x, baseline(head_top + 65, font$pt_400, 17, 17 * 1.25), "Workshop Organised by Monash Business Analytics Team", "pt_400", 17, muted)
  )

  # Speakers, stacked up from the bottom of the column.
  name_lh <- 27 * 1.2
  role_lh <- 19 * 1.3
  y <- main_bottom
  speaker_text <- character()
  for (p in rev(s$speakers)) {
    if (nzchar(p$role)) {
      y <- y - role_lh
      speaker_text <- c(speaker_text, svg_text(col_x, baseline(y, font$pt_400, 19, role_lh), xml_escape(p$role), "pt_400", 19, muted))
    }
    y <- y - name_lh
    speaker_text <- c(speaker_text, svg_text(col_x, baseline(y, font$pt_700, 27, name_lh), xml_escape(p$name), "pt_700", 27))
    y <- y - 8
  }
  speakers_top <- if (n) y + 8 else main_bottom

  # Kicker and title, below the header. The title starts at 64px and shrinks
  # only until it clears the speakers by 24px.
  kicker_top <- main_top + logo_h + 24
  kicker_lh <- 19 * 1.3
  kicker <- toupper(paste(s$kind, "·", s$when))
  title_top <- kicker_top + kicker_lh + 12
  for (size in seq(64, 36, by = -2)) {
    lines <- wrap_text(s$title, col_w, font$cabin_600, size)
    if (title_top + length(lines) * size * 1.08 + 24 <= speakers_top) break
  }
  if (title_top + length(lines) * size * 1.08 + 24 > speakers_top) {
    warning(s$code, ": title still overlaps the speakers at ", size, "px")
  }
  title_text <- imap_chr(lines, \(line, i) {
    svg_text(col_x, baseline(title_top + (i - 1) * size * 1.08, font$cabin_600, size, size * 1.08), xml_escape(line), "cabin_600", size)
  })

  c(
    out,
    svg_text(col_x, baseline(kicker_top, font$pt_700, 19, kicker_lh), xml_escape(kicker), "pt_700", 19, orange, spacing = 19 * 0.14),
    title_text,
    speaker_text,
    "</svg>"
  )
}

# Write the cards -----------------------------------------------------------

dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
sessions <- list.files(c("program/tutorials", "program/workshops"), pattern = "\\.qmd$", full.names = TRUE)

walk(sessions, \(path) {
  s <- read_session(path)
  svg <- file.path(out_dir, paste0(s$code, ".svg"))
  png <- file.path(out_dir, paste0(s$code, ".png"))
  xfun::write_utf8(card_svg(s), svg)
  system2(chrome, c(
    "--headless", "--disable-gpu", "--hide-scrollbars", "--force-device-scale-factor=1",
    sprintf("--window-size=%d,%d", W, H),
    paste0("--screenshot=", normalizePath(png, mustWork = FALSE)),
    paste0("file://", normalizePath(svg))
  ), stdout = FALSE, stderr = FALSE)
  message("Wrote ", svg, " and ", png)
})
