module Evento where


import Database.SQLite.Simple
import Database.SQLite.Simple.Types
import Data.String(fromString)

import Database.SQLite.Simple

import Database.SQLite.Simple

import Database.SQLite.Simple
import qualified Data.Text as T

inserirEvento :: Connection -> Int -> String -> String -> String -> IO ()
inserirEvento conn eventoID nomeEvento dataEvento localEvento = do
  execute conn (Query $ T.pack "INSERT INTO Eventos (eventoID, nomeEvento, dataEvento, localEvento) VALUES (?, ?, ?, ?)") (eventoID, nomeEvento, dataEvento, localEvento)
  putStrLn "Evento inserido com sucesso!"

addEvento :: String -> IO ()
addEvento "db/dados.sqlite" = do
  conn <- open "db/dados.sqlite"

  putStrLn "Inserção de Evento"
  putStrLn "eventoID:"
  eventoID <- readLn :: IO Int
  putStrLn "Nome do evento:"
  nomeEvento <- getLine
  putStrLn "Data do evento:"
  dataEvento <- getLine
  putStrLn "Local do evento:"
  localEvento <- getLine

  inserirEvento conn eventoID nomeEvento dataEvento localEvento

  close conn


   
   
data Evento = Evento
  { eventoID :: Int
  , nomeEvento :: String
  , dataEvento :: String
  , localEvento :: String
  }

instance FromRow Evento where
  fromRow = Evento <$> field <*> field <*> field <*> field

lerEventos :: IO ()
lerEventos = do
  conn <- open "db/dados.sqlite"
  let query = fromString "SELECT eventoID, nomeEvento, dataEvento, localEvento FROM Eventos"
  eventos <- query_ conn query :: IO [Evento]
  putStrLn "=========== Eventos: ==========="
  mapM_ (\(Evento eventoID nomeEvento dataEvento localEvento) -> do
    putStrLn $ "ID: " ++ show (eventoID :: Int)
    putStrLn $ "Nome: " ++ nomeEvento
    putStrLn $ "Data: " ++ dataEvento
    putStrLn $ "Local: " ++ localEvento
    putStrLn "\n") eventos
  putStrLn "Aperte ENTER para continuar..."
  getLine
  close conn


menuEventos :: IO()
menuEventos = do 
    
    putStrLn("=========== Menu Eventos ===========")
    putStrLn "1 - Adicionar Evento"
    putStrLn "2 - Exibir Evento" 
    putStrLn("Opção: ")
    opCliente <- getChar
    getChar
    switchOpCliente opCliente

switchOpCliente :: Char -> IO ()
switchOpCliente '1' = do
  addEvento "db/dados.sqlite"
  return ()
switchOpCliente '2' = do
    lerEventos
    return()
switchOpCliente _ = do
    putStrLn "Opção inválida"




