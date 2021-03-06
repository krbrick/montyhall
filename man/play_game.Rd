% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MHallKRB.R
\name{play_game}
\alias{play_game}
\title{Determine one of two game outcomes utilising the Monty Hall Problem.}
\usage{
play_game()
}
\value{
returns a character vector of length 3 or 4, which represents whether the guest won or lost the game
by choosing a door with or without a car.
}
\description{
Determine if a contestant wins a game by choosing from one of three doors,
which conceal two goats and one car. Winning is achieved when the contestant chooses a door
concealing a car. Losing occurs when the conestant chooses a door concealing a goat.
}
\details{
Three doors exist concealing one goat, one car, and another goat, randomly assigned (a.game).
When the contestant randomly chooses a door (a.pick), that door is eliminated from the doors available for
the host to open. The host opens a door that is not the guest's first pick and is not a door with
a car behind it (opened.door). The contestant can now choose to change their door pick to the remaining door (stay = F) or
stay (stay = T) with their original door pick (a.pick) as their final pick. If final.pick is equal
to a door with a car behind it, that contestant wins. If final.pick is equal to a door with a goat
behind it, the contestant loses.
}
\examples{
game.results
}
