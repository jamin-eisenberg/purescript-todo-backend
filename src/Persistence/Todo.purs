module Persistence.Todo
  ( createTodoTableIfNotExists
  , deleteAllTodos
  , deleteSingleTodo
  , getAllTodos
  , getSingleTodo
  , mkTodo
  , updateTodo
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.String (joinWith, toUpper)
import Data.Tuple (uncurry)
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import Foreign (Foreign)
import Model.Todo (Todo)
import SQLite3 (DBConnection, queryDB)
import Simple.JSON as SimpleJson

type DBTodo =
  { id :: Int
  , title :: String
  , ord :: Maybe.Maybe Int
  , completed :: Int
  }

createTodoTableIfNotExists :: DBConnection -> Aff Unit
createTodoTableIfNotExists conn = do
  results <- SimpleJson.read <$> queryDB conn sql []
  case results of
    Right (_ :: Foreign) -> pure unit
    Left e -> liftEffect $ throw $ "Table creation failed: " <> show e
  where
  sql = "CREATE TABLE IF NOT EXISTS todo (id INTEGER PRIMARY KEY AUTOINCREMENT, title TEXT NOT NULL, ord INTEGER, completed INTEGER NOT NULL)"

dbToTodo :: DBTodo -> Todo
dbToTodo todo = todo { completed = todo.completed /= 0 }

queryTodos :: String -> DBConnection -> Aff (Array Todo)
queryTodos sql conn = do
  results <- SimpleJson.read <$> (queryDB conn sql [])
  case results of
    Right (todo :: Array DBTodo) -> pure $ dbToTodo <$> todo
    Left e -> liftEffect $ throw $ "SQL `" <> sql <> "` failed with error: " <> show e

queryTodo :: String -> DBConnection -> Aff (Maybe Todo)
queryTodo sql conn = do
  todos <- queryTodos sql conn
  case todos of
    [] -> pure Nothing
    [ todo ] -> pure $ Just todo
    _ -> liftEffect $ throw $ "Should be impossible, more than one record returned by SQL `" <> sql <> "`: " <> show todos

getAllTodos :: DBConnection -> Aff (Array Todo)
getAllTodos = queryTodos "SELECT * FROM todo"

mkTodo :: { title :: String, ord :: Maybe.Maybe Int, completed :: Maybe.Maybe Boolean } -> DBConnection -> Aff Todo
mkTodo { title, ord, completed } conn = do
  result <- queryTodo sql conn
  case result of
    Nothing -> liftEffect $ throw $ "Should be impossible, zero records returned by INSERT"
    Just todo -> pure todo
  where
  completedBool = Maybe.fromMaybe false completed
  sql = "INSERT INTO todo (title, ord, completed) VALUES ("
    <> csv
      [ sqlStringValue title
      , sqlMaybeValue show ord
      , sqlBoolValue completedBool
      ]
    <> ") RETURNING *"

deleteAllTodos ∷ DBConnection → Aff Unit
deleteAllTodos conn = void $ queryTodos "DELETE FROM todo RETURNING *" conn

getSingleTodo :: Int -> DBConnection -> Aff (Maybe Todo)
getSingleTodo id = queryTodo ("SELECT * FROM todo WHERE id = " <> show id)

updateTodo :: { id :: Int, title :: Maybe.Maybe String, ord :: Maybe.Maybe Int, completed :: Maybe.Maybe Boolean } -> DBConnection -> Aff (Maybe Todo)
updateTodo { id, title, ord, completed } conn = do
  queryTodo sql conn
  where
  setExpr columnName value = columnName <> " = " <> value

  sql =
    "UPDATE todo SET "
      <>
        ( csv $ uncurry setExpr <$> Map.toUnfoldable
            ( Map.catMaybes
                ( Map.fromFoldable
                    [ "title" /\ (sqlStringValue <$> title)
                    , "ord" /\ (show <$> ord)
                    , "completed" /\ (sqlBoolValue <$> completed)
                    ]
                )
            )
        ) -- TODO cleanup
      <> " WHERE id = "
      <> show id
      <> " RETURNING *"

deleteSingleTodo :: Int -> DBConnection -> Aff Unit
deleteSingleTodo id conn = void $ queryTodos ("DELETE FROM todo WHERE id = " <> show id) conn

csv :: Array String -> String
csv = joinWith ", "

sqlStringValue :: String -> String
sqlStringValue s = "'" <> s <> "'"

sqlBoolValue :: Boolean -> String
sqlBoolValue = toUpper <<< show

sqlMaybeValue :: forall a. (a -> String) -> Maybe a -> String
sqlMaybeValue show val =
  case val of
    Nothing -> "NULL"
    Just present -> show present