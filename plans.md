## A functional approach

### Using StateT World (ST s)

Using `StateT World (ST s) ()` to do some nice stateful computation:
```
applyActions :: World -> World
applyActions world = runST $ flip execStateT world $ do
  moveActions    <- lift $ newSTRef []
  harvestActions <- lift $ newSTRef []
  ...

  forM (antsIn world) $ \ant ->
    case getActionOf ant world of
      AMove moveData -> lift $ modifySTRef moveActions (moveData :)
      AHarvest harvestData -> lift $ modifySTRef harvestActions (harvestData :)
      ...

  modify $ applyHarvestActions harvestActions
  modify $ applyMoveActions moveActions
  ...

-- Or just ST s and then State World inside:
applyActions world = runST $ do
  -- compute actions as before; no chance of modifying world; no need to lift!
  return $ flip execState world $ do
    modify $ applyHarvestActions harvestActions
    modify $ applyMoveActions moveActions
```

The above example doesn't quite work for actions that can generate more actions.
I could follow Ben's approach where actions are the output of action resolution.

```
data ActionData = AMove MoveData
                | AHarvest HarvestData
                | APing PingData
                ...

-- An action has an associated position in the world.
type Action data = (GridPosition, data)
type SomeAction = Action ActionData

type PingAction = (GridPosition, PingData)
applyPingActions :: (MonadState World m)
                 => [Action PingData]
                 -> m [PingSuccessAction]


class IsActionData d where
  asActionData :: d -> ActionData

asSomeAction :: (IsActionData d)
             => Action d
             -> SomeAction

applyActions world = runST $ flip execStateT world $ do
  pingActions    <- lift $ newSTRef []
  moveActions    <- lift $ newSTRef []
  harvestActions <- lift $ newSTRef []

  let addActions []     = void
      addActions (a:as) = do
        let (pos, actionData) = asSomeAction a
        case actionData of
          AMove d    -> lift $ modifySTRef moveActions ((pos, d):)
          AHarvest d -> lift $ modifySTRef harvestActions ((pos, d):)
          APing d    -> lift $ modifySTRef pingActions ((pos, d):)
        addActions as

  addActions $ getAntAction <$> antsIn world

  applyPingActions pingActions >>= addActions
  applyMoveActions moveActions
  applyHarvestActions harvestActions
  ...
```

This is awesome, but every time I add an action, I have to modify applyActions.
The problem is that I have to modify it in three spots which get farther and
farther away.

My first attempt at solving this:
```
type ActionConfig d = ActionConfig
  { resolver    :: ActionResolver d
  , _ActionData :: Prism' ActionData d
  }

-- using HList (as :: [*])
-- replicateH :: (forall a. (Element a as) => a)
              -> HList as
-- sequenceH :: HList (Map m as)
             -> m (HList as)
-- mapH :: (forall a. (Element a as) => a -> b)
        -> HList as
        -> HList (Replicate (Size as) b)
-- zipH :: HList as -> HList bs -> HList (Zip as bs)

applyActions world = runST $ flip execStateT world $ do
  actions <- sequenceH $ replicateH $ lift $ newSTRef []

  let addActions []     = void
      addActions (a:as) = do
        let (pos, actionData) = asSomeAction a
        flip mapH (zipH actions actionConfigs) $ \(config) ->
          if (matches (_ActionData config) actionData)
            then lift $ modifySTRef ((pos, ...))

  addActions $ getAntAction <$> antsIn world

  for each action x:
    applyX xActions >>= addActions
```

I think a cleaner approach will be to use TemplateHaskell. In fact, using
TemplateHaskell, I could also enforce at compile-time that all actions produced
during an action resolution stage are processed in a later stage.


### Naming

The suffix `Data` represents "local" information about something, whereas
removing the suffix signifies knowing the location of the thing in the world.
```


data ActionData = ... -- The description of an action
type Action = WithPosition ActionData

data CellData = ...
type Cell = WorldObject CellData

data EntityData = EntityAnt AntData
                | EntityFood FoodData
type Entity = WorldObject EntityData

data AntData = ...
type Ant = WorldObject AntData
```

I can use the `Is...Data` pattern to define subsets of a sum type.

```
class IsEntityData d where
  asEntityData :: d -> EntityData

asEntity = fmap asEntityData

instance IsEntityData AntData where
  asEntityData = EntityAnt
```

### Organization

I have defined the world, world transitions, world data, and actions. I have
defined that action types are processed in order, and the resolution of an
action type can produce more actions of a later type.

In my new approach, I will separate actions from the world definition. This way,
the action system can be defined completely separately from the world system.

`World` will define the concept of ant knowledge and state.

```
-- Ordered in topological order by dependencies.
BattleAnts/
  GUI.hs  -- Displays the whole game in a GUI.

  Graphics/
    Gloss.hs  -- Definitions needed to render the game in the gloss package.
    Gloss/
      WorldGrid.hs  -- Draws the entire grid of the world.
      WorldCell.hs  -- Draws a cell of the world.
                    -- Called 'WorldCell' because it doesn't just draw CellData.
                    -- Instead, it depends on Ant.hs and Food.hs.
      Cell.hs       -- Draws the background for a cell.
      Ant.hs        -- Draws an ant; intended to be overlaid on a cell.
      Food.hs       -- Draws a food; intended to be overlaid on a cell.

  Simulator.hs  -- Runs a `GameState`.

  GameState.hs  -- The entire state of the game, including the World and the
                -- player functions.

  Player.hs   -- Exports all definitions useful to the player.
  Player/
    AntComputation.hs -- Monad: ReaderT AntInput (State StdGen)
                      -- Defines API for defining ants.
                      --   Makes it easier to modify ant state.
                      --   Enables randomness.
                      -- An AntBrain is an AntComputation AntOutput.

    AntInput.hs   -- Input to an ant. Does not include a random seed.
    AntOutput.hs  -- An ant's output.

    WorldView/  -- Player versions of World data types.
      CellView.hs
      EntityView.hs
      AntView.hs
      FoodView.hs

      ViewedByAnt.hs

  Actions.hs  -- Exports public Actions/ definitions.
  Actions/
    Action.hs         -- defines Action, ActionData and the general resolver

    -- Specific actions:
    MoveAction.hs     -- defines MoveAction, MoveData and their resolver
    HarvestAction.hs  -- defines HarvestAction, HarvestData and their resolver
    ...


  World.hs  -- Exports public World/ definitions.
  World/
    Transitions/  -- common world transitions; these define the only public API
                  -- to modify World after construction!
      KillAnts.hs       -- canonical way to murder ants

      -- Functions for dealing with entities and the ID system.
      MoveEntities.hs   -- canonical way to move entities; returns conflicts
      PutEntity.hs      -- places and IDs a new entity in the world
      RemoveEntity.hs   -- removes an entity from the world

      ModifyCell.hs     -- modifies a cell in the world
      ModifyEntity.hs   -- modifies an entity in the world (changing its data)

    World.hs        -- defines the World type and its accessors
    Cell.hs         -- defines Cell (no ID, just position) and CellData

    Entity.hs       -- defines Entity and EntityData
    Ant.hs          -- defines Ant, AntData and AntState (part of AntData).
    Food.hs         -- defines Food and FoodData

    WorldObject.hs  -- Something plus a GridPosition and a WorldId.
    WorldId.hs      -- An ID used in the world.

    Positioned.hs   -- Class for stuff that is positioned. E.g. actions (which
                    -- do not have IDs).
  Fields/ -- Defines 'HasFieldName' classes to break ambiguities.
Control/  -- Utilities for control structures.
Data/     -- Defines Grid, Grid3x3, GridDirection, SparseGrid.
```

#### ID system

Entities in the world will have IDs associated to them. The World module will
guarantee that all entities in the world have different IDs.

World.hs will not export a constructor for WorldId or the ID lens. External
code will not be able to create Entity/Ant/Food or change their IDs.

#### Transitions

All transition functions will be of the form `Input -> State World Result` with
their own Input and Result types.

```
data Result = Result { collisions :: [GridPosition] }

newtype Origin = Origin { _originPos :: GridPosition }
newtype Destination = Destination { _destinationPos :: GridPosition }
makeFields ''Origin
makeFields ''Destination

-- | Moves ants from origins to destinations.
--
-- If multiple ants try to move into the same destination, they are not moved
-- and the collision is recorded in the result.
moveAnts :: [(Origin, Destination)] -> State World Result
```

#### World.hs

Don't use the `In` suffix, since I expect `World` to be used in `State World`.

```
entities :: World -> [Entity]
ants :: World -> [Ant]
food :: World -> [Food]
```

Users will write

```
as <- gets ants
```

#### Positioned.hs

I decided not to worry about the concept of "object identity". I'll write my
code carefully, making sure not to re-use a `Positioned` value after modifying
`World`.

```
type Positioned d = (GridPosition, d)
```

##### Consider: type safety vs ergonomicity

I could return `GridPosition` instead of `Positioned d`. This would force the
consuming code to check whether the position actually contains the value that
I claim it does. This is clunky: my APIs will usually promise that the value
exists. This suffers the same modification problem: the consuming code can
query the position, store the result, and then modify the world.

##### Consider: knowledge about world transitions

```
as <- gets ants
moveAnts $ catMaybes $ getMovement <$> as

-- 'as' is now invalid, so must get it again!
as <- gets ants
```

Maybe better:

```
antRefs <- gets ants
runOO $ do
  moveResult <- moveAnts <$> catMaybes <$> fmap getMovement <$> antRefs
```

Or just write in a different style:

```
result <- moveAnts =<< catMaybes <$> fmap getMovement <$> gets ants
```
