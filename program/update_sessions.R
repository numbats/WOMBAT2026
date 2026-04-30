library(httr2)
req <- request("https://conf.nectric.com.au/api/events/wombat-2026/")

sessions <- req |> 
  req_url_path_append("schedules/latest/") |> 
  req_url_query(expand="slots.submission,slots.room") |> 
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

library(purrr)
library(tibble)
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
    gsub("&amp;", "&", x=_) |>
    gsub("&gt;", ">", x=_) |>
    gsub("&lt;", "<", x=_) |>
    gsub("&quot;", '"', x=_) |>
    gsub("&#39;", "'", x=_) |>
    gsub("<[^>]*>", "", x=_)  # Remove HTML tags
}

library(dplyr)
library(tidyr)
speakers_tidy <- map_dfr(speakers$results,recurse_tibble) |> 
  mutate(biography = escape_html(biography)) |>
  unnest(submissions) |> 
  nest(.key = "speakers", .by = submissions)
sessions_tidy <- map_dfr(sessions$slots,recurse_tibble) |> 
  filter(!is.na(submission$code)) |> 
  transmute(
    start = as.POSIXct(start, format = "%Y-%m-%dT%H:%M:%S+10:00"),
    duration,
    time = format(start, "%B %d, %I:%M %p"),
    title = submission$title |> escape_html(),
    abstract = submission$abstract |> escape_html(),
    submissions = submission$code,
    room = room$name |> unlist()
  ) |> 
  left_join(speakers_tidy, by = "submissions")

write_session_qmd <- function(x, ...) {
  is_tutorial <- x$start < as.POSIXct("2026-09-30")
  if(is_tutorial) {
    dir <- "program/tutorials"
    x$register <- "[Register for this tutorial](https://events.humanitix.com/wombat-2026-day-1-tutorials)"
  } else {
    dir <- "program/workshops"
    x$register <- "[Register for the day 2 workshop](https://events.humanitix.com/wombat-2026-day-2-workshop)"
  }
  path <- xfun::with_ext(file.path(dir, x$submissions), "qmd")

  x <- as.list(x)
  
  # Generate short summary
  if(file.exists(path)) {
    x$description <- rmarkdown::yaml_front_matter(path)$description
  }
  if(is.null(x$description)) {
    chat <- ellmer::chat_google_gemini(
      system_prompt = "Briefly summarise the key session topics in a plain text from the following abstract. The summary should start with a background details sentence, followed a sentence detailing the key topics of the session in passive voice."
    )
    x$description <- chat$chat(x$abstract)
    # x$description <- ""
  }
  x$description <- gsub("\r\n", " ", x$description)
  x$description <- gsub("’", "'", x$description)
  
  x$yml <- yaml::as.yaml(
    list(
      pagetitle = paste("WOMBAT 2026:", x$title),
      date = format(x$start),
      time = format(x$start, "%I:%M %p"),
      title = x$title,
      description = x$description,
      # abstract = x$abstract,
      speaker = transpose(x$speakers[[1]][c("code", "name", "avatar_url")]),
      speakerlist = paste(x$speakers[[1]][["name"]], collapse = ", "),
      room = paste("Room ", x$room, collapse = ' ')
    )
  )
  x$is_tutorial <- is_tutorial
  x$is_workshop <- !is_tutorial
  
  x$speakers <- transpose(x$speakers[[1]])
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
