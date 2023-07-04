module Salas where


import Database.SQLite.Simple
import Database.SQLite.Simple.Types
import Data.String(fromString)

import Database.SQLite.Simple

import Database.SQLite.Simple

import Database.SQLite.Simple
import qualified Data.Text as T

inserirSalas :: Connection -> Int -> String -> String -> IO ()
inserirSalas conn salaID salaNome salaCapacidade = do
  execute conn (Query $ T.pack "INSERT INTO Salas (salaID, salaNome, salaCapacidade) VALUES (?, ?, ?)") (salaID, salaNome, salaCapacidade)
  putStrLn "Salas inseridas com sucesso!"

addSalas :: String -> IO ()
addSalas "db/dados.sqlite" = do
  conn <- open "db/dados.sqlite"

  putStrLn "Inserção de Salas"
  putStrLn "salaID:"
  salaID <- readLn :: IO Int
  putStrLn "salaNome:"
  salaNome <- getLine
  putStrLn "salaCapacidade:"
  salaCapacidade <- getLine

  inserirSalas conn salaID salaNome salaCapacidade

  close conn


data Sala = Sala
  { salaID :: Int
  , salaNome :: String
  , salaCapacidade :: Int
  }

instance FromRow Sala where
  fromRow = Sala <$> field <*> field <*> field

lerSalas :: IO ()
lerSalas = do
  conn <- open "db/dados.sqlite"
  let query = fromString "SELECT salaID, salaNome, salaCapacidade FROM Salas"
  salas <- query_ conn query :: IO [Sala]
  putStrLn "=========== Salas: ==========="
  mapM_ (\(Sala salaID salaNome salaCapacidade) -> do
    putStrLn $ "ID: " ++ show (salaID :: Int)
    putStrLn $ "Nome: " ++ salaNome
    putStrLn $ "Capacidade: " ++ show salaCapacidade
    putStrLn "\n"
    ) salas
  putStrLn "Aperte ENTER para continuar..."
  getLine
  close conn

menuSalas :: IO()
menuSalas = do 
    
    putStrLn("=========== Menu Salas ===========")
    putStrLn "1 - Adicionar Sala"
    putStrLn "2 - Exibir Salas" 
    putStrLn("Opção: ")
    opCliente <- getChar
    getChar
    switchOpCliente opCliente

switchOpCliente :: Char -> IO ()
switchOpCliente '1' = do
    addSalas "db/dados.sqlite"
    return()
switchOpCliente '2' = do
    lerSalas 
    return()
switchOpCliente _ = do
    putStrLn "Opção inválida"