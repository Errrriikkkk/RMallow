ranking <- setRefClass("ranking", fields = list(info = "matrix",
                                     ranks = "data.frame",
                                     items = "integer",
                                     rankers = "integer",
                                     type = "character",
                                     ties = "matrix",
                                     geqties = "matrix",
                                     relative = "matrix",
                                     V = "matrix"),
                       methods = list(
                      show = function() {
                        cat(cat(paste("Ranking objects on", type, "rankings with", 
                                      rankers, "rankers and",
                                      items, "items ranked.")))
                      },
                      initialize = function(input, meaning = NULL) {
                        if(!missing(input)) {
                          if (class(input) == "ranking") { # Duplicate existing object if a ranking
                            .self <<- input
                          }
                          else {
                            items <<- ncol(input)
                            rankers <<- nrow(input)
                            cat('Simplifying Rankings\n')
                            temp <- simplifySequences(as.matrix(input))
                            ranks <<- data.frame(temp)
                            if(all(diff(rowSums(ranks)) == 0)) {
                              type <<- "full"
                              if(is.null(meaning)) {
                                cat("Meaning of rankings is ambiguous, defaulting to direct\n")
                              }
                              else if (meaning == "inverse") {
                                cat("Converted rankings from inverse form to direct form\n")
                                ranks <<- data.frame(t(apply(temp, 1, order)))
                              }
                            }
                            else {
                              type <<- "partial"
                            }
                            cat('Calculating tie information\n')
                            ties <<- tieInfo(ranks) 
                            cat('Calculating more tie information\n')
                            geqties <<- geqInfo(ranks, ties)
                            cat('Calculating kendall info\n')
                            info <<- kendallInfo(ranks)
                            cat('Calculating relative ranks of items within their tie groups\n')
                            relative <<- relativeRank(ranks)
                            cat('Calculating the V matrix')
                            V <<- findV(info, ncol(ranks))
                          }
                        }
                      }))
mallow <- setRefClass("mallow", fields = list(modes = "matrix",
                                              props = "numeric",
                                              theta = "matrix",
                                              normalize = "numeric",
                                              log.like = "numeric",
                                              clust = "numeric",
                                              dists = "matrix",
                                              pvals = "matrix"),
                      contains = "ranking",
                      methods = list(
                        initialize = function(ranks, modes, props,
                                              normalize, log.like,
                                              clust, dists, pvals) {
                            callSuper(ranks)
                            .self$initFields()
                        }))