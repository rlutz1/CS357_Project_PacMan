# CS357_Project_PacMan
pacman game in haskell!

+ move all data decl to a data folder

## todo 4/25 - 4/27
+ ghosts with dummy ai's to get them on the board and moving.
  (something dumb like one picks up/l/r/d, one d/l/u/r...)
+ implement a collision detection system.
+ on collide, decrease lives
+ on all lives lost (lives == 0) game over and reset the board
+ implement the cherries--keep collision detection turn off and onable so that i can keep it on for a counter.
+ move how to draw each element of board into the element itself. each gets a draw method--itself -> Picture
+ encap update functions within each element--update main just turns into each update function being called on board, player, ghosts, etc as needed. can legit be id but move it.

+ move players and ghosts to within board? or is it overkill; it'll likely be helpful for the ghost ai

## base requirements

+ ~~Tile map/grid-based movement dictated by arrow keys (user input).~~
+ ~~Oranges: plain 10 point grabs, no effect~~
+ Cherries: 10 points, timed collision detection disabling (can get past the
ghosts)
+ Have 4 ghosts with at least 2 unique/more complex AI path generation
algorithms.
+ get collision detection up and running

+ Keep track of lives (PacMan has 3, loses 1 each ghost collision)
+ Keep track of score (each collectible may have its own unique score as
determined during creation, consider extra score points for lives left at end of
board clear.)
+ Implement game over logic:
  + Board has been cleared of all collectibles and PacMan has not run out of
  lives. Success! Prompt user to play again or go back to main menu.
  + PacMan has not cleared the board and has run out of lives. FAILURE!
Prompt user to retry or go back to main menu.
+ Include at least 3 different levels to load up with unique configurations.
+ Implement a simple main menu with buttons to load up different levels to enable a
nice game flow.
