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
                        cat(paste("Ranking objects on", type, "rankings with", 
                                      rankers, "rankers and",
                                      items, "items ranked.\n"))
                      },
                      initialize = function(input, meaning = NULL) {
                        if(!missing(input)) {
                          if (class(input) == "ranking") { # Duplicate existing object if a ranking
                            .self <<- input
                          }
                          else {
                            items <<- ncol(input)
                            rankers <<- nrow(input)
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
                            ties <<- tieInfo(ranks) 
                            geqties <<- geqInfo(ranks, ties)
                            info <<- kendallInfo(ranks)
                            relative <<- relativeRank(ranks)
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
                                              clust, dists, pvals, theta) {
                            callSuper(ranks)
                            modes <<- modes
                            props <<- props
                            pvals <<- pvals
                            theta <<- theta
                            log.like <<- log.like
                            .self$initFields()
                        },
                        show = function() {
                          cat('Mallows model with modes: \n')
                          namez <- names(ranks)
                          m <- apply(t(apply(modes, 1, function(i) namez[i])), 1, paste, collapse = ", ")
                          cat('Modes:\n')
                          print(m)
                          cat('\nThetas:\n')
                          print(theta)
                          cat('\nProportions:\n')
                          cat(props)
                        }))
