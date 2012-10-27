#' Generate a list of objects of class ranking.
#' @param ranks a list of objects of class 'ranking'.  May be length 1.
#' @param orig the original ranking object, for context.
#' @param modes a matrix of modal sequences
#' @param updates which items in the list do we need to update?
#' @return a list of objects of class 'ranking', one for each mode
genRanks <- function(ranks, modes, updates , orig) {#
  # If input is a list, update them for the items in update
  for(G in updates) {
    ranks[[G]] <- new('ranking', orig[, order(modes[G, ])]) # add order
  }
  return(ranks)
}