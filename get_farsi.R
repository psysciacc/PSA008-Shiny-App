library(qualtRics)
survey_id <- "SV_8k10IEYESlBLf5I"
survey_def <- fetch_description(surveyID = survey_id,
                                elements = "questions")

questions <- survey_def$questions

# ---- helpers ----
extract_language_tbl <- function(q) {
  idx <- which(tolower(names(q)) == "language")
  if (!length(idx)) return(NULL)   # no $language field
  
  lang <- q[[idx[1]]]
  if (length(lang) == 0) return(NULL)   # empty $language list
  
  if (is.null(names(lang))) {
    names(lang) <- paste0("lang_", seq_along(lang))
  }
  
  tibble(language = names(lang), node = unname(lang))
}
extract_options_tbl <- function(lang_node) {
  # try common keys where options live
  candidates <- c("options", "choices", "Choices", "Answers")
  key <- intersect(candidates, names(lang_node %||% list()))
  opts <- if (length(key)) lang_node[[key[1]]] else lang_node
  
  # normalize to a two-column tibble: option_id, option_text
  if (is.null(opts)) return(tibble(option_id = character(), option_text = character()))
  
  # character vector
  if (is.character(opts)) {
    return(tibble(option_id = as.character(seq_along(opts)), option_text = opts))
  }
  
  # atomic vector
  if (is.atomic(opts)) {
    return(tibble(option_id = as.character(seq_along(opts)), option_text = as.character(opts)))
  }
  
  # list without names
  if (is.list(opts) && is.null(names(opts))) {
    return(tibble(option_id = as.character(seq_along(opts)),
                  option_text = map_chr(opts, ~ as.character(.x))))
  }
  
  # named list (e.g., codes → objects/strings)
  if (is.list(opts) && !is.null(names(opts))) {
    pull_text <- function(x) {
      if (is.list(x)) {
        # try common text fields, then fall back to first element as string
        val <- x[intersect(c("text", "label", "Description", "Text"), names(x))]
        val <- if (length(val)) val[[1]] else x[[1]]
        return(as.character(val))
      }
      as.character(x)
    }
    return(tibble(option_id = names(opts),
                  option_text = map_chr(opts, pull_text)))
  }
  
  tibble(option_id = character(), option_text = character())
}

`%||%` <- function(a, b) if (!is.null(a)) a else b

# ---- main extraction ----
languages_options <- map_dfr(seq_along(questions), function(i) {
  q <- questions[[i]]
  langs <- extract_language_tbl(q)
  if (is.null(langs) || !nrow(langs)) return(tibble())
  
  map_dfr(seq_len(nrow(langs)), function(r) {
    opts <- extract_options_tbl(langs$node[[r]])
    mutate(opts,
           question_index = i,
           language = langs$language[[r]],
           .before = 1)
  })
})

# Result: one row per (question × language × option)
languages_options
