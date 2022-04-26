module Postgrest.Constraint exposing
    ( Constraint(..)
    , ForeignKeyParams
    , foreignKey
    , none
    , primaryKey
    )


type Constraint
    = None
    | PrimaryKey
    | ForeignKey ForeignKeyParams


type alias ForeignKeyParams =
    { table : String
    , primaryKeyName : String
    , labelColumnName : Maybe String
    , label : Maybe String
    }


none : Constraint
none =
    None


primaryKey : Constraint
primaryKey =
    PrimaryKey


foreignKey : ForeignKeyParams -> Constraint
foreignKey =
    ForeignKey
