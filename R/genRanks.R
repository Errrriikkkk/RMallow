#' Generate a list of objects of class ranking.
#' @param ranks a list of objects of class 'ranking'.  May be length 1.
#' @param orig the original ranking object, for context.
#' @param modes a matrix of modal sequences
#' @param updates which items in the list do we need to update?
#' @return a list of objects of class 'ranking', one for each mode
genRanks <- function(ranks, old.modes, new.modes, orig) {#
  # If input is a list, update them for the items in update
  if (!is.list(ranks)) {
    ranks <- list(ranks)
  }
  if (is.list(new.modes)) {
    new.modes <- do.call('rbind', new.modes)
  }
  G <- nrow(new.modes)
  for (i in 1:G) {
    if (!identical(old.modes[i, ], new.modes[i, ])) {
      ranks[[i]] <- ranking$new(orig[, new.modes[i, ]])
    }
  }
  ranks
}