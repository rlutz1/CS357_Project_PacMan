# CS357_Project_PacMan
pacman game in haskell!

+ ~~overhaul the tracking system~~
+ remove destination from ghosts
+ remove direction from ghosts
+ ~~remove arg from valid neighbors thing, all is shuffled now~~
+ ~~get neighbor -> gen neighbor~~
+ ~~get tracks -> get neighbor~~
+ holy shit turn coll detection back on on start ha


## todo 4/28 - 5/2
+ change the damn collectibles thing to just remove a collectible from the list if eaten. no more eaten crap, it'll make everything far more efficient there.

+ implement the cherries--keep collision detection turn off and onable so that i can keep it on for a counter.
+ move how to draw each element of board into the element itself. each gets a draw method--itself -> Picture
+ encap update functions within each element--update main just turns into each update function being called on board, player, ghosts, etc as needed. can legit be id but move it.
+ giving the ghosts like 3 frames before they start moving would be nice
+ consider extra score points for lives left at end of board clear

+ once fully done, separate out player/ghost functions to different folders for cleanliness

### lower priority stuff (clean up tasks)
+ separate out the radius or width of everything so collision detection is malleable to size changes
+ note i could technically just give the board a function to test game over rather than a state flag. unclear if benefit



## base requirements

+ Cherries: 10 points, timed collision detection disabling (can get past the
ghosts)
+ Include at least 3 different levels to load up with unique configurations.
+ Implement a simple main menu with buttons to load up different levels to enable a
nice game flow.

## notes && thots
+ two versions of dfs are almost the same despite the order change and it depends on the order of directional neighbors.
+ could just speed up the simulation steps to make it less obvious that the movement is highly controlled. it'll be good enough for this project at least.