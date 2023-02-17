#' ==== Principles ====
#'
#' -- Dependencies --
#' Anything I'd write normally is a dependency for synsyn. This includes and is
#' not limited to dplyr, tidyr, rlang, dddr, and rbids.
#'
#' -- Move fast, write shit --
#' At this stage, I'm writing this for my own use. I can't spend more than
#' 30 seconds thinking through a design decision, especially if I predict how a
#' "future user" would do it.
#'
#' -- Sizing --
#' It is fair to assume the bids metadata will fit in memory, and that any
#' session will fit in memory. This assumption may be challenged, and can go
#' either way (perhaps down to pair)
#'
#' -- Tagging / annotation / semantics of columns
#' at teh top of each synsyn analyusis, check if all columns are tagged.
#'
#'
#' ==== Open questions ====
#' - What would a user do with intermediate computation?
#'   - interactively program a data transformation step?
#'
#' - Is this appropriate to do mostly in SQL?
#'   - I mean, to some extent, yes, the idea is to do "as much on small as possible"
#'   - but I'd be wanting to add in new columns, etc, pretty often, e.g. computing velocity, etc
#'   - and I don't want people thinking about that??? Or do I?
#'   - Some stuff would count as "additions" to the sample-record files, like speed
#'   - but it also could be varying, etc.
#'   - really the idea would be to specify what to save / append and what not to?
#'   - rea
#'


#' @keywords Internal
compute_pairs_up_to <- function(dta, n_todo, relate, preprocess, group_summary, ordered, unique, pb) {

  pairs_completed <- 0

  fps <- dta$file_path
  rows <- list()
  #browser()
  for (fpx_ind in seq_along(fps)) {
    px_data <- preprocess(
      fps[[fpx_ind]],
      session_id = dta$session_id[[fpx_ind]],
      participant_id = dta$participant_id[[fpx_ind]],
      task_label = dta$task_label[[fpx_ind]]
    )
    for (fpy_ind in seq_along(fps)) {

      py_data <- NULL
      if (fpx_ind == fpy_ind) {
        if (unique) {
          # save time, don't recompute
          py_data <- px_data
        } else {
          next
        }
      } else if (ordered || (fpx_ind < fpy_ind)) {
        py_data <- preprocess(
          fps[[fpy_ind]],
          session_id = dta$session_id[[fpy_ind]],
          participant_id = dta$participant_id[[fpy_ind]],
          task_label = dta$task_label[[fpy_ind]]
        )
      } else {
        # if unordered only, and fpx_ind > fpy_ind
        next # aka skip
      }
      # apply function
      result <- relate(px_data, py_data)
      #rm(py_data)
      # perhaps have addl args if necessary for participant ids, etc.
      if (!is.null(pb)) {
        pb$tick()
      }
      pairs_completed <- pairs_completed + 1

      # what if the result is a df? does it complain?
      rows <- c(rows, list(list(
        participant_id_x = dta$participant_id[[fpx_ind]],
        participant_id_y= dta$participant_id[[fpy_ind]],
        result = result
      )))

      if (pairs_completed >= n_todo) break
    }
    #rm(px_data)
    if (pairs_completed >= n_todo) break
  }
  group_summary(rows)
}

#' Summarize pairs of motion data
#'
#' Given a bids dataset, apply a function relating pairs of participants in the same session.
#'
#' @param bd_motion A BIDS motion files table, of the format created by `bids::bids_motion`
#' @param relate A function with two arguments that relate two participant's motion together into a data frame of values.
#' @param preprocess A function with arguments `f(file_path, session_id, participant_id, task_label)` that returns a data frame on which `relate` operates. At the very least, this function needs to read the file at `file_path` into a data frame.
#' @param ordered Whether the pairs are ordered pairs, i.e, whether to compute both `relate(A, B)` and `relate(B, A)` for participants A and B.
#' @param unique Deprecated, use `self_relate` instead
#' @param self_relate Whether to compute relationships within the same person, i.e., whether to calculate `relate(A, A)` for a participant A.
#' @param head_n How many pairs to compute before returning.
#' @param progress Whether to show a progress bar
#'
#' @export
#' @importFrom dplyr group_by group_nest mutate bind_rows
#' @importFrom magrittr %>%
#' @importFrom lifecycle deprecate_soft
#'
summarize_motion_pairs <- function(bd_motion, relate, preprocess, group_summary = bind_rows, ordered = F, unique = NA, self_relate = F, head_n = Inf, progress = T) {
  #browser()
  # todo: does match.arg go here?
  if (!isTRUE(is.na(unique))) {
    deprecate_soft("0.1.1", "summarize_motion_pairs(unique)", with = "summarize_motion_pairs(self_relate)")
    self_relate <- unique
  }

  npairs <- function(n) {
    triangle = (n^2-n)/2
    triangle + triangle*ordered + n*unique
  }

  nested_pairs <- bd_motion %>%
    group_by(session_id, task_label) %>%
    group_nest(.key = ".synsyn.map_motion_pairs.nest") %>%
    mutate(
      .synsyn.nesting.nrow = map_int(.synsyn.map_motion_pairs.nest, nrow),
      .synsyn.pairs_in_nesting = npairs(.synsyn.nesting.nrow),
      .synsyn.pairs_todo = head_n - lag(cumsum(.synsyn.pairs_in_nesting), default = 0)
    )

  # TODO: use pb if there are enough (>10) entries
  pb <- NULL
  if (progress) {
    pb <- progress::progress_bar$new(
      format = "[:bar] :percent eta: :eta",
      total = min(sum(nested_pairs$.synsyn.pairs_in_nesting), head_n)
    )
    pb$tick(0)
  }


  nested_pairs %>%
    filter(.synsyn.pairs_todo > 0) %>%
    mutate(
      .synsyn.map_motion_pairs.nest = pmap(
        list(.synsyn.map_motion_pairs.nest, session_id, task_label),
        function(dta, c1, c2) {
          dta %>%
            mutate(
              "session_id" = c1,
              "task_label" = c2
            )
        }
      ),
      .synsyn.map_motion_pairs.result = map2( #furrr::future_map2(
        .synsyn.map_motion_pairs.nest, .synsyn.pairs_todo,
        compute_pairs_up_to,
        relate = relate, preprocess = preprocess, group_summary = group_summary,
        ordered = ordered, unique = unique, pb = pb
      )
    ) %>%
    select(-c(
      .synsyn.map_motion_pairs.nest,
      .synsyn.nesting.nrow,
      .synsyn.pairs_in_nesting,
      .synsyn.pairs_todo,
    )) %>%
    unnest(.synsyn.map_motion_pairs.result)
}

#' Summarize individuals' motion data
#'
#' Given a bids dataset, apply a function to each individual's sessions of motion, one at a time.
#'
#' @param bd_motion A BIDS motion files table, of the format created by `bids::bids_motion`
#' @param fn A function with arguments `f(file_path, session_id, participant_id, task_label)` that returns a summarized data frame
#' @param head_n How many pairs to compute before returning
#'
#' @export
summarize_motion <- function(bd_motion, fn, head_n = Inf) {
  # todo: does match.arg go here?
  motion_files <- bd_motion %>%
    head(n = head_n)

  # TODO: use pb if there are enough (>10) entries
  pb <- progress::progress_bar$new(
    format = "[:bar] :percent eta: :eta",
    total = nrow(motion_files)
  )
  pb$tick(0)

  motion_files %>%
    mutate(
      .synsyn.map_motion.result = pmap( # furrr::future_pmap(
        list(file_path, participant_id, session_id, task_label),
        function(file_path, participant_id, session_id, task_label) {
          result <- fn(file_path, participant_id, session_id, task_label)
          pb$tick()
          result
        }
      )
    ) %>%
    unnest(.synsyn.map_motion.result)
}


#' Summarize individual motion data iteratively
#'
#' Summarize individuals' motion data into a single frame with a custom function using the functional programming approach called reduce, fold, etc.
#'
#' @param bd_motion A BIDS motion files table, of the format created by `bids::bids_motion`
#' @param fn A function that takes arguments `f(file_path, session_id, participant_id, task_label)` and returns a data frame
#' @param reduce_fn A function that takes arguments `f(result, step_result)`, the first is the accumulated value, and the second is the result of `fn` for the current element in `bids_motion`.
#' @param head_n How many pairs to compute before returning
#'
#' @export
reduce_motion <- function(bd_motion, fn, reduce_fn, head_n = Inf) {

  motion_files <- bd_motion %>%
    head(n = head_n)

  # TODO: use pb if there are enough (>10) entries
  pb <- progress::progress_bar$new(
    format = "[:bar] :percent eta: :eta",
    total = nrow(motion_files)
  )
  pb$tick(0)

  result <- NULL

  for (i in 1:nrow(motion_files)) {

    step_result <- with(motion_files, fn(file_path[i], participant_id[i], session_id[i], task_label[i]))

    if (i == 1) {
      result <- step_result
    } else {
      result <- reduce_fn(result, step_result)
    }

    pb$tick()
  }
  result
}










