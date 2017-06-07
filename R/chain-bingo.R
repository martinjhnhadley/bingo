#' Generate a restaurant chain bingo card
#'
#' @param restaurant.info Data.frame with restaurant details, must contain following columns:
#' \itemize{
#'  \item{"name.of.chain"}{ : start longitude}
#'  \item{"img.file"}{ : start latitude}
#'  }
#'
#' @param path location to store the bingo card
#'
#' @return a PDF containing bingo cards!
#' @export
#'
chain_bingo_card <- function(restaurant.info, path = "./chain-bingo-card.pdf"){
  if(all(c("name.of.chain", "img.file") %in% colnames(restaurant.info)) &
     is.data.frame(restaurant.info)) {
    message("warming ovens...")
  } else {
    warning("restaurant.info must be a data.frame containing the columns; name.of.chain and img.file")
  }

  img_paths <- restaurant.info %>%
    select(img.file) %>%
    .[[1]]

  tryCatch(logo_images <- map(img_paths, function(x)readPNG(x)),
           error = function(e) simpleError(e))

  grid_of_n_squares <- generate_grid_of_n_squares(length(logo_images))

  bingo_images <-
    fill_with_free_squares(logo_images, grid_of_n_squares)

  gl = lapply(bingo_images, grid::rasterGrob)
  gridExtra::grid.arrange(grobs=gl)
  make_grid(sqrt(grid_of_n_squares))

}

chain_bingo_card(data_chain_restaurants %>%
                   slice(1:9))
