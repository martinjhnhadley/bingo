generate_grid_of_n_squares <- function(number.of.images){

  ## HARD CODED SQUARE NUMBERS BECAUSE OF REASONS
  acceptable_perfect_squares <- c(9, 25, 49, 81, 121, 169,255)

  if(number.of.images < 9){
    9 # hard code 3x3 grid
  } else {
    acceptable_perfect_squares[number.of.images < acceptable_perfect_squares][[1]]
  }
}


fill_with_free_squares <- function(logo.imgs, grid_of_n_squares){

  free_square_img <- readPNG("inst/free-square.png")
  centre_square_img <- readPNG("inst/centre-square.png")

  if(length(logo.imgs) == grid_of_n_squares - 1){
    append(logo.imgs,
            list(centre_square_img),
            after = {ceiling(grid_of_n_squares / 2)} - 1)
  } else {

    all_squares <- 1:grid_of_n_squares
    except_centre <- all_squares[-{{ceiling(grid_of_n_squares / 2)}}]
    print(except_centre)
    logo_positions <- sample(except_centre, 9)
    free_square_positions <- setdiff(except_centre, logo_positions)

    new_list <- list(1:grid_of_n_squares)

    new_list[free_square_positions] <- list(free_square_img)

    new_list[logo_positions] <- logo.imgs

    new_list[{ceiling(grid_of_n_squares / 2)}] <- list(centre_square_img)

    new_list
  }

}


# fill_with_free_squares(1:9, 25)

make_grid <- function(n) {
  gridlines <- grid::unit( (0:n) / n, "npc")
  grid::grid.grill(h = gridlines,  v = gridlines) # gp = gpar(col="grey"))
  gridlines
}

