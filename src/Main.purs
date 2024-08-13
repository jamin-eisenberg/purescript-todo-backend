module Main (main) where

import Prelude

import Control.Monad.Trans.Class (lift)
import Data.Argonaut (class DecodeJson, class EncodeJson, JsonDecodeError, decodeJson, encodeJson, parseJson, stringify)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Effect (Effect)
import Effect.Aff (Aff, Error, runAff_)
import Effect.Exception (throw)
import HTTPurple (class Generic, JsonDecoder(..), JsonEncoder(..), Method(..), Request, ResponseM, RouteDuplex', badRequest, headers, int, jsonHeaders, lookup, methodNotAllowed, mkRoute, noArgs, notFound, ok', segment, serve, toJson, usingCont)
import HTTPurple.Json (fromJsonE)
import Model.Todo (Todo)
import Persistence.Todo (createTodoTableIfNotExists, deleteAllTodos, deleteSingleTodo, getAllTodos, getSingleTodo, mkTodo, updateTodo)
import SQLite3 (DBConnection, newDB, queryDB)

data Route = AllTodos | SingleTodo Int

derive instance Generic Route _

type ResponseTodo =
  { url :: String
  , title :: String
  , order :: Maybe.Maybe Int
  , completed :: Boolean
  }

fromTodo :: String -> Todo -> ResponseTodo
fromTodo baseUrl { id, title, ord, completed } = { url: baseUrl <> "/" <> show id, title, order: ord, completed }

route :: RouteDuplex' Route
route = mkRoute
  { "AllTodos": noArgs
  , "SingleTodo": int segment
  }

corsMiddleware
  :: forall route
   . (Request route -> ResponseM)
  -> Request route
  -> ResponseM
corsMiddleware realRouter request = do
  case lookup request.headers "Origin" of
    Nothing -> badRequest "Not a valid CORS request"
    Just origin ->
      case request.method, lookup request.headers "Access-Control-Request-Method" of
        Options, Just _ -> do
          ok'
            ( headers
                { "Access-Control-Allow-Origin": origin
                , "Access-Control-Allow-Methods": "*"
                , "Access-Control-Allow-Headers": "*"
                }
            )
            ""
        _, _ -> do
          response <- realRouter request
          pure $ response
            { headers = response.headers <>
                headers
                  { "Access-Control-Allow-Origin": origin
                  , "Access-Control-Expose-Headers": "*"
                  }
            }

jsonDecoder :: forall json. DecodeJson json => JsonDecoder JsonDecodeError json
jsonDecoder = JsonDecoder fromJsonString
  where
  fromJsonString :: String -> Either JsonDecodeError json
  fromJsonString = parseJson >=> decodeJson

jsonEncoder :: forall json. EncodeJson json => JsonEncoder json
jsonEncoder = JsonEncoder toJsonString
  where
  toJsonString :: json -> String
  toJsonString = encodeJson >>> stringify

initDB :: String -> Aff DBConnection
initDB filename = do
  conn <- newDB filename
  void $ queryDB conn "DROP TABLE todo" []
  createTodoTableIfNotExists conn
  pure conn

main :: Effect Unit
main = do
  (flip runAff_)
    (initDB "./db.sqlite")
    serveWithDB

serveWithDB :: Either Error DBConnection -> Effect Unit
serveWithDB =
  case _ of
    Left e -> throw $ show e
    Right conn -> void $ serve
      { port: 8080 }
      { route, router: corsMiddleware (router conn "http://localhost:8080") }

router :: DBConnection -> String -> Request Route -> ResponseM
router conn baseUrl { route: AllTodos, method: Get } = do
  allTodos <- getAllTodos conn
  ok' jsonHeaders $ toJson jsonEncoder (fromTodo baseUrl <$> allTodos)

router conn baseUrl { route: AllTodos, method: Post, body } = usingCont do
  { title, order: ord } :: { title :: String, order :: Maybe.Maybe Int } <- fromJsonE jsonDecoder (badRequest <<< show) body
  createdTodo <- lift $ mkTodo { title, ord, completed: Nothing } conn
  ok' jsonHeaders $ toJson jsonEncoder (fromTodo baseUrl createdTodo)

router conn _ { route: AllTodos, method: Delete } = do
  void $ deleteAllTodos conn
  ok' jsonHeaders $ toJson jsonEncoder ([] :: Array Todo)

router _ _ { route: AllTodos } = methodNotAllowed

router conn baseUrl { route: SingleTodo id, method: Get } = do
  todo <- getSingleTodo id conn
  let todoResponse = fromTodo baseUrl <$> todo
  case todoResponse of
    Nothing -> notFound
    Just presentTodoResponse -> ok' jsonHeaders $ toJson jsonEncoder presentTodoResponse

router conn baseUrl { route: SingleTodo id, method: Patch, body } = usingCont do
  { title, order: ord, completed } :: { title :: Maybe.Maybe String, order :: Maybe.Maybe Int, completed :: Maybe.Maybe Boolean } <-
    fromJsonE jsonDecoder (badRequest <<< show) body
  updated <- lift $ updateTodo { id, title, ord, completed } conn
  let updatedResponse = fromTodo baseUrl <$> updated
  case updatedResponse of
    Nothing -> notFound
    Just presentUpdatedResponse -> ok' jsonHeaders $ toJson jsonEncoder presentUpdatedResponse

router conn _ { route: SingleTodo id, method: Delete } = do
  void $ deleteSingleTodo id conn
  ok' jsonHeaders $ toJson jsonEncoder {}

router _ _ { route: SingleTodo _ } = methodNotAllowed