#' convenience functions for working with leaflet maps
#' 
#' @param m a leaflet map
#' 
#' @author
#' Tim Appelhans
#' 

getLayerControlEntriesFromMap <- function(m) {
  
  seq_along(m$x$calls)[sapply(m$x$calls, 
                              FUN = function(X) "addLayersControl" %in% X)]

}


getLayerNamesFromMap <- function(m) {
  
  len <- Rsenal:::getLayerControlEntriesFromMap(m)
  len <- len[length(len)]
  if (length(len) != 0) m$x$calls[[len]]$args[[2]] else NULL
  
}


layers2bHidden <- function(m) {
  
  nms <- Rsenal:::getLayerNamesFromMap(m)
  nms[2:length(nms)]
  
}


# extractObjectName <- function(x) {
#   pipe_splt <- strsplit(x, "%>%")[[1]][-1]
#   
#   grp <- vector("character", length(pipe_splt))
#   for (i in seq(grp)) {
#     x <- pipe_splt[i]
#     tmp <- strsplit(strsplit(x, 
#                              "\\(")[[1]][2], ",")[[1]][1]
#     grp[i] <- gsub("\\)", "", tmp)
#   }
#   return(grp)
# }
  