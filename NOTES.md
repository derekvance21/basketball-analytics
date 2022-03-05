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
