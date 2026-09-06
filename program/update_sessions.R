library(httr2)
library(purrr)
library(tibble)
library(dplyr)
library(tidyr)

req <- request("https://conf.nectric.com.au/api/events/wombat-2026/")

sessions <- req |>
  req_url_path_append("schedules/latest/") |>
  req_url_query(expand = "slots.submission,slots.room") |>
  req_perform() |>
  resp_body_json()

speakers <- req |>
  req_url_path_append("speakers/") |>
  req_perform() |>
  resp_body_json()

rooms <- req |>
  req_url_path_append("rooms/") |>
  req_perform() |>
  resp_body_json()

recurse_tibble <- function(x) {
  is_list_col <- lengths(x) > 1
  is_df_col <- map_lgl(x, \(y) !is.null(names(y)))
  tbl_cols <- names(x[is_list_col & is_df_col])
  x[tbl_cols] <- lapply(x[tbl_cols], recurse_tibble)
  lst_cols <- names(x[is_list_col & !is_df_col])
  x[lst_cols] <- lapply(x[lst_cols], \(x) list(unlist(x)))
  x[lengths(x) == 0] <- NA
  as_tibble(x)
}

escape_html <- function(x) {
  x |>
    gsub("&amp;", "&", x = _) |>
    gsub("&gt;", ">", x = _) |>
    gsub("&lt;", "<", x = _) |>
    gsub("&quot;", '"', x = _) |>
    gsub("&#39;", "'", x = _) |>
    gsub("<[^>]*>", "", x = _) # Remove HTML tags
}

# Speaker photos are hosted on the conference platform. Mirror them into the
# repo so pages don't depend on a third-party host staying up (and so
# og:image keeps working if it ever changes/blocks hotlinking). Downloads are
# skipped once a copy exists locally, so re-running this script is cheap.
avatar_dir <- "img/speakers"
if (!dir.exists(avatar_dir)) dir.create(avatar_dir, recursive = TRUE)

localise_avatar <- function(url, code) {
  if (is.null(url) || is.na(url) || !nzchar(url)) {
    return("/img/user.png")
  }
  ext <- tools::file_ext(sub("\\?.*$", "", url))
  if (!nzchar(ext)) ext <- "jpg"
  dest <- file.path(avatar_dir, paste0(code, ".", ext))
  if (!file.exists(dest)) {
    ok <- tryCatch({
      download.file(url, dest, mode = "wb", quiet = TRUE)
      TRUE
    }, error = function(e) {
      warning("Failed to download avatar for ", code, ": ", conditionMessage(e))
      FALSE
    })
    if (!ok) return(url) # fall back to hotlinking rather than a dead image
  }
  paste0("/", dest)
}

speakers_tidy <- map_dfr(speakers$results, recurse_tibble) |>
  mutate(biography = escape_html(biography) |> gsub("\r\n", "\n", x = _) |> map_chr(commonmark::markdown_html)) |>
  unnest(submissions) |>
  nest(.key = "speakers", .by = submissions)

sessions_tidy <- map_dfr(sessions$slots, recurse_tibble) |>
  filter(!is.na(submission$code)) |>
  transmute(
    start = as.POSIXct(start, format = "%Y-%m-%dT%H:%M:%S+11:00"),
    duration,
    time = format(start, "%B %d, %I:%M %p"),
    title = submission$title |> escape_html(),
    abstract = submission$abstract |> escape_html() |> gsub("\r\n", "\n", x = _) |> map_chr(commonmark::markdown_html),
    submissions = submission$code,
    room = room$name |> unlist()
  ) |>
  left_join(speakers_tidy, by = "submissions")

write_session_qmd <- function(x, ...) {
  is_tutorial <- x$start < as.POSIXct("2026-12-01")
  if (is_tutorial) {
    dir <- "program/tutorials"
    x$register <- "[Register for this tutorial](https://events.humanitix.com/wombat-2026-day-1-tutorials)"
  } else {
    dir <- "program/workshops"
    x$register <- "[Register for the day 2 workshop](https://events.humanitix.com/wombat-2026-day-2-workshop)"
  }
  path <- xfun::with_ext(file.path(dir, x$submissions), "qmd")

  x <- as.list(x)

  # Preserve hand-edited fields across re-runs. These were already run through
  # HTML-escaping (via the whisker template) the last time this script wrote
  # them, so undo that before it happens again -- otherwise re-running the
  # script repeatedly double-escapes entities (e.g. "&amp;" -> "&amp;amp;").
  x$pagetitle <- NULL
  # `online` (hybrid vs in-person only) isn't part of the pretalx data --
  # it's curated by hand per session, so preserve it like the fields above.
  x$online <- NULL
  if (file.exists(path)) {
    front <- rmarkdown::yaml_front_matter(path)
    if (!is.null(front$description)) x$description <- escape_html(front$description)
    x$slides_url <- front$slides_url
    if (!is.null(front$pagetitle)) x$pagetitle <- escape_html(front$pagetitle)
    x$online <- front$online
  }
  x$has_location <- !is.null(x$online)
  x$room_label <- paste("Room", x$room)
  if (is.null(x$description)) {
    # chat <- ellmer::chat_google_gemini(
    #   system_prompt = "Briefly summarise the key session topics in a plain text from the following abstract. The summary should start with a background details sentence, followed a sentence detailing the key topics of the session in passive voice."
    # )
    # x$description <- chat$chat(x$abstract)
    x$description <- "Generated summary"
  }
  x$description <- gsub("\r\n", " ", x$description)
  x$description <- gsub("’", "'", x$description)

  speaker_tbl <- x$speakers[[1]]
  speaker <- NULL
  speaker_list <- NULL
  x$multi_speaker <- !is.null(speaker_tbl) && nrow(speaker_tbl) > 1
  if (is.null(speaker_tbl)) {
    speaker <- list(
      code = "",
      name = "",
      avatar_url = "/img/user.png"
    )
    speaker_list <- c(" ")
    x$speakers <- speaker
  } else {
    speaker_tbl$avatar_url <- map2_chr(speaker_tbl$avatar_url, speaker_tbl$code, localise_avatar)
    speaker <- transpose(speaker_tbl[c("code", "name", "avatar_url")])
    speaker_list <- paste(speaker_tbl[["name"]], collapse = ", ")
    x$speakers <- transpose(speaker_tbl)
  }
  image <- if (is.null(speaker_tbl)) {
    "/img/user.png"
  } else {
    speaker_tbl[["avatar_url"]][[1]]
  }
  x$yml <- yaml::as.yaml(
    list(
      pagetitle = x$pagetitle %||% x$title,
      date = format(x$start),
      time = format(x$start, "%I:%M %p"),
      title = x$title,
      description = x$description,
      # abstract = x$abstract,
      speaker = speaker,
      speakerlist = speaker_list,
      room = paste("Room ", x$room, collapse = " "),
      online = x$online,
      image = image,
      format = list(html = list(css = "../../css/talks.css")),
      slides_url = x$slides_url
    ),
    # Render logical online as bare true/false (matching the existing
    # hand-edited convention) instead of yaml::as.yaml's default yes/no.
    handlers = list(logical = function(x) {
      v <- ifelse(x, "true", "false")
      class(v) <- "verbatim"
      v
    })
  )
  x$is_tutorial <- is_tutorial
  x$is_workshop <- !is_tutorial

  xfun::write_utf8(
    whisker::whisker.render(
      xfun::read_utf8("program/session_template.qmd"),
      x
    ),
    path
  )
}

# Workshops
sessions_tidy |>
  rowwise() |>
  group_walk(write_session_qmd)
