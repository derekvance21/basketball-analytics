# Steps
- [ ] Have datatype to represent a game
- [ ] Parse game file into datatype
- [ ] Define operations on that datatype

# Goals
- **TO%**
- Shots at rim %
- Shots from 3pt %
- **eFG%**
- **Points per possession (PPP)** 
- And then keep track of players individually, you can aggregate later
- **OReb%**
- **FTRate**
- Can track these team metrics with specific lineups, opposing teams, etc.
![New Mexico State Basketball Analytics Dashboard](images/nms-bball-dashboard.jpg)

# Ideas
- Four factors of basketball success (Dean Oliver):
  - **Shooting** *(eFG%)*
  - **Turnovers** *(TO%)*
  - **Rebounding** *(OReb%)*
  - **Free Throws** *(FTM/FGA)*
- Could say there are eight, because you multiply those by two for offense/defense
- These inherently cover the entire game of basketball, which is what my logging does. They are the *possession enders*
  - Another possession ender might be the clock running out
- Eventually, game logs will have player information, and each play in the data structure will have the list of players in the game

# Other
- I ran into an interesting situation: a player shot a three and the ball got wedged between the rim and backboard. That's a jumpball, so is it a OR or DR? The possession changed so a DR made more sense but if the possession had stayed it would've been quite weird to mark it as an OR