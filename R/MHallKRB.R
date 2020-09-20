#' @title
#'   Create a new Monty Hall Problem game.
#'
#' @description
#'   `create_game()` generates a new game that consists of two doors 
#'   with goats behind them, and one with a car.
#'
#' @details
#'   The game setup replicates the game on the TV show "Let's
#'   Make a Deal" where there are three doors for a contestant
#'   to choose from, one of which has a car behind it and two 
#'   have goats. The contestant selects a door, then the host
#'   opens a door to reveal a goat, and then the contestant is
#'   given an opportunity to stay with their original selection
#'   or switch to the other unopened door. There was a famous 
#'   debate about whether it was optimal to stay or switch when
#'   given the option to switch, so this simulation was created
#'   to test both strategies. 
#'
#' @param ... no arguments are used by the function.
#' 
#' @return The function returns a length 3 character vector
#'   indicating the positions of goats and the car.
#'
#' @examples
#'   create_game()
#'
#' @export
create_game <- function()
{
  a.game <- sample( x=c("goat","goat","car"), size=3, replace=F )
  return( a.game )
} 



#' @title
#' Randomly select one of three doors, corresponding to the return create_game().
#' @description
#' `select_door ()`` randomly selects one of three doors.  
#' @details
#' Doors correspond to the a.game output. Behind one of the doors is a goat,
#'  behind one is a car, behind another is a goat, etc. The door selected represents
#'  the guest's choice without knowing what is behind that door. 
#' @param 
#' No arguments are used in this function
#' @return 
#' Returns a length one numeric vector, 1,2,or 3, determining the guest pick of doors. 
#' @examples
select_door ()
#' @export
select_door <- function( )
{
  doors <- c(1,2,3) 
  a.pick <- sample( doors, size=1 )
  return( a.pick )  # number between 1 and 3
}

#' @title
#' Open a door that is not a.pick or corresponds to the value of 'car' in a.game.
#' @description
#' Opens one of three doors that is neither the guest's first choice (a.pick, return of select_door) 
#' nor has a car behind it. 
#' @details
#' Because it cannot be a door with a car behind it, it must be a door with a 
#' goat behind it. This represents the host opening another door, thus increasing the odds that the 
#' remaining door will have the car behind it.
#' @param 
#' a.pick, a.game
#' @return 
#' returns a length one numeric vector, 1,2, or 3, which corresponds to the host-opened door
#' that is neither the car nor the guest's first choice. 
#' @examples
#' opened.door
#' @export
open_goat_door <- function( game, a.pick )
{
  doors <- c(1,2,3)
  # if contestant selected car,
  # randomly select one of two goats 
  if( game[ a.pick ] == "car" )
  { 
    goat.doors <- doors[ game != "car" ] 
    opened.door <- sample( goat.doors, size=1 )
  }
  if( game[ a.pick ] == "goat" )
  { 
    opened.door <- doors[ game != "car" & doors != a.pick ] 
  }
  return( opened.door ) # number between 1 and 3
}



#' @title
#' Returns final pick of the guest, door 1, 2, or 3.
#' @description
#' If guest chooses to stay with their first pick of door, then a.pick is equal to their final.pick. 
#' If the guest chooses to change doors, then their final pick is neither a.pick (guest's first pick) 
#' nor the opened.goat.door (or the door opened by the host)
#' @details
#' Because the host opened a goat door, the final.pic of the guest cannot be this door. The guest's 
#' final pick of door can either be their first pick (a.pick) or the remaining door (not opened.door 
#' or a.pick)
#' @param 
#' stay = T or F, opened.door, a.pick
#' @return 
#' Returns a vector of length one and class numeric, which represents the guest's final choice of door.
#' @examples
#' final.pick
#' @export
change_door <- function( stay=T, opened.door, a.pick )
{
  doors <- c(1,2,3) 
  
  if( stay )
  {
    final.pick <- a.pick
  }
  if( ! stay )
  {
    final.pick <- doors[ doors != opened.door & doors != a.pick ] 
  }
  
  return( final.pick )  # number between 1 and 3
}



#' @title
#' Determine game outcome based on guest's choice of doors (to stay or not).
#' @description
#' Determines if guest wins or loses evaluating the game value (goat or car) final pick of door, whether they change 
#' doors or not.
#' @details
#' If the guest's choice is to stay with the first choice of doors and that door corresponds to a 
#' value of "car" in a.game, the guest 'wins'. If the guest decides to change doors and the new door 
#' of their choice is a 'car' door, the guest wins. If the guest stays with the first pick and that 
#' door is a 'goat' door, the gues loses. If the guest changes doors and the new chosen door is a 
#' 'goat' door, the guest loses.
#' @param 
#' final.pick, a.game
#' @return 
#' returns a character vector of length 3 or 4, which represents whether the guest won or lost the game
#' by choosing a door with or without a car.
#' @examples
#' "WIN" or "LOSE"
#' @export
determine_winner <- function( final.pick, game )
{
  if( game[ final.pick ] == "car" )
  {
    return( "WIN" )
  }
  if( game[ final.pick ] == "goat" )
  {
    return( "LOSE" )
  }
}





#' @title
#' Determine one of two game outcomes utilising the Monty Hall Problem.
#' @description
#' Determine if a contestant wins a game by choosing from one of three doors, 
#' which conceal two goats and one car. Winning is achieved when the contestant chooses a door 
#' concealing a car. Losing occurs when the conestant chooses a door concealing a goat.
#' @details
#' Three doors exist concealing one goat, one car, and another goat, randomly assigned (a.game).
#' When the contestant randomly chooses a door (a.pick), that door is eliminated from the doors available for
#' the host to open. The host opens a door that is not the guest's first pick and is not a door with
#' a car behind it (opened.door). The contestant can now choose to change their door pick to the remaining door (stay = F) or 
#' stay (stay = T) with their original door pick (a.pick) as their final pick. If final.pick is equal
#' to a door with a car behind it, that contestant wins. If final.pick is equal to a door with a goat 
#' behind it, the contestant loses.
#' @return 
#' returns a character vector of length 3 or 4, which represents whether the guest won or lost the game
#' by choosing a door with or without a car.
#' @examples
#' game.results
#' @export
play_game <- function( )
{
  new.game <- create_game()
  first.pick <- select_door()
  opened.door <- open_goat_door( new.game, first.pick )
  
  final.pick.stay <- change_door( stay=T, opened.door, first.pick )
  final.pick.switch <- change_door( stay=F, opened.door, first.pick )
  
  outcome.stay <- determine_winner( final.pick.stay, new.game  )
  outcome.switch <- determine_winner( final.pick.switch, new.game )
  
  strategy <- c("stay","switch")
  outcome <- c(outcome.stay,outcome.switch)
  game.results <- data.frame( strategy, outcome,
                              stringsAsFactors=F )
  return( game.results )
}






#' @title
#' Determine the likelihood of winning a car utilising the Monty Hall Problem.
#' @description
#' A loop generates the game results ("Win" or "Lose") for 100 iterations of the Monty Hall Problem. 
#' Results are assigned to a collector vector list and displayed as a table of proportions.
#' @details
#' The two game outcomes of play_game, win or lose, are played out 100 times using the conditions of 
#' play_game, or the Monty Hall Problem. The proportion of wins and losses, by stay or change choice, 
#' is displayed in the table.
#' @param 
#' game.outcome
#' @return 
#' Returned is a proportion table of wins and losses by guest door choice, of class dataframe.  
#' @examples
#' results.df
#' @export
play_n_games <- function( n=100 )
{
  
  library( dplyr )
  results.list <- list()   # collector
  loop.count <- 1
  
  for( i in 1:n )  # iterator
  {
    game.outcome <- play_game()
    results.list[[ loop.count ]] <- game.outcome 
    loop.count <- loop.count + 1
  }
  
  results.df <- dplyr::bind_rows( results.list )
  
  table( results.df ) %>% 
    prop.table( margin=1 ) %>%  # row proportions
    round( 2 ) %>% 
    print()
  
  return( results.df )
  
}
