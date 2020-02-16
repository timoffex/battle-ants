## Design

### Data Constructors

#### Most types

In general, do not export data constructors. Instead, for every type, export a
function named `mkMyType` that can be used to set any parameters without
reasonable defaults:

```
data AntData = AntData { _antDataFood :: Int
                       , _antDataAntId :: AntId
                       , _antDataPlayerId :: PlayerId
                       , _antDataAction :: AntAction }
makeFields ''AntData
mkAntData :: AntId -> PlayerId -> AntData
mkAntData aid pid = AntData 0 aid pid ANothing
```

Reasoning: Makes it easier to add new parameters to types when those parameters
have default values. The naming `mkMyType` is useful because `myType` is better
as the name of a lens.

#### Exceptions

It may be useful to export constructors when code may need to pattern-match on
all cases of a sum type. The same concerns about future changes apply here, so
when constructors are exported, they should all either not have any data or
have a single associated datum. For example,
```
-- Exporting OK: one datum per constructor.
data Entity = EntityAnt AntData
            | EntityFood FoodData

-- Exporting OK: this is just an enum.
data FileType = PNG | JPEG | TXT | EXE

-- Exporting BAD: difficult to change the data for a food type.
data Food = Poison Int    -- Poison strength.
          | Sugar Int Int -- Sugar amount, sweetness.
          | Protein Int ProteinType -- Protein amount, type.

-- Better alternative.
data Food = Poison PoisonInfo
          | Sugar SugarInfo
          | Protein ProteinInfo
```

### Lenses and Fields

Always use `makeFields` from `Control.Lens.TH` for non-sum type data. Export
the generated type classes.

Reasoning: Exporting the `Has...` type classes lets you reap the benefits of
overloaded names.

## Aesthetics

### Records

Put the constructor name on the same line as the type name, then define
the fields starting on the following line, indented two spaces:

```
data AntData = AntData
  { _antDataFoodSupply :: FoodData
  , _antDataKnowledge  :: AntKnowledge
  }
```

- Align the `::` for each field in the record, if possible.
- End with a brace on its own line.

### Lenses

Lenses should be nouns so that they can be prefixed by 'set' and 'view'.

*Good*: `antData`

*Bad*: `getAntData`

### Traversals

A traversal should be the pluralized name of the target prefixed with
`traverse`.

*Good*: `traverseAntDatas`

*Bad*: `antDatas`

Reasoning: Makes it clear that it's a traversal and not a lens to a list.

```
game & traverseEntities .~ duplicatedEntity
  =
game & cells . traverse . entities .~ duplicatedEntity

game^.entities :: [Entity]
```

### Getter functions

Simple getter functions should be suffixed with `Of` to signal that they are
not lenses.

*Good*: `positionOf :: a -> GridPosition`

*Bad*: `position :: a -> GridPosition`

For getter functions that will be used with `gets` in a `MonadState`
computation, give them a lens name and a `Getter` (or `Getting`) type:

```
position :: Getter a GridPosition

gets $ view position :: MonadState s m => m GridPosition
```
