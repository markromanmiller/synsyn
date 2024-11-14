# `synsyn` - to synchronize synchrony measurement

Now it's more about dealing with motion data nicely.

Here's an example counting the number of rows per motion file:
```r
# devtools::install_github("markromanmiller/rbids")
# devtools::install_github("markromanmiller/synsyn")
library(tidyverse)
library(rbids)
library(synsyn)

# load data with BIDS
bd_fall <- bids("path/to/bids/dataset")

# Define the function to apply to each function.
count_rows <- function(file_path, session_id, participant_id, ...) {
  # exactly one file per person
  read_tsv(file_path, col_types = list(Timestamp = col_time("%H:%M:%OS")), progress = F) %>%
    nrow()
}

results <- bd_fall %>%
  bids_motion() %>%
  #filter(session_id %>% endsWith("section13")) %>%
  #filter(session_id == "week3section13") %>%
  summarize_motion(
    fn = count_rows
  )
```
