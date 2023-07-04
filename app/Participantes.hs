module Participantes where


import Database.SQLite.Simple
import Database.SQLite.Simple.Types
import Data.String(fromString)

import Database.SQLite.Simple

import Database.SQLite.Simple

import Database.SQLite.Simple
import qualified Data.Text as T

inserirParticipantes :: Connection -> Int -> String -> String -> String -> IO ()
inserirParticipantes conn participanteID nomeParticipante emailParticipante empresaParticipante = do
  execute conn (Query $ T.pack "INSERT INTO Participantes (participanteID, nomeParticipante, emailParticipante, empresaParticipante) VALUES (?, ?, ?, ?)") (participanteID, nomeParticipante, emailParticipante, empresaParticipante)
  putStrLn "Participantes inseridos com sucesso!"

addParticipantes :: String -> IO ()
addParticipantes "db/dados.sqlite" = do
  conn <- open "db/dados.sqlite"

  putStrLn "Inserção de Participantes"
  putStrLn "participanteID:"
  participanteID <- readLn :: IO Int
  putStrLn "nomeParticipante:"
  nomeParticipante <- getLine
  putStrLn "emailParticipante:"
  emailParticipante <- getLine
  putStrLn "empresaParticipante:"
  empresaParticipante <- getLine

  inserirParticipantes conn participanteID nomeParticipante emailParticipante empresaParticipante

  close conn

  

data Participante = Participante
  { participanteID :: Int
  , nomeParticipante :: String
  , emailParticipante :: String
  , empresaParticipante :: String
  }

instance FromRow Participante where
  fromRow = Participante <$> field <*> field <*> field <*> field

lerParticipantes :: IO ()
lerParticipantes = do
  conn <- open "db/dados.sqlite"
  let query = fromString "SELECT participanteID, nomeParticipante, emailParticipante, empresaParticipante FROM Participantes"
  participantes <- query_ conn query :: IO [Participante]
  putStrLn "=========== Participantes: ==========="
  mapM_ (\(Participante participanteID nomeParticipante emailParticipante empresaParticipante) -> do
    putStrLn $ "ID: " ++ show (participanteID :: Int)
    putStrLn $ "Nome: " ++ nomeParticipante
    putStrLn $ "Email: " ++ emailParticipante
    putStrLn $ "Empresa: " ++ empresaParticipante
    putStrLn "") participantes
  putStrLn "Aperte ENTER para continuar..."
  getLine
  close conn

menuParticipantes :: IO()
menuParticipantes = do 
    
    putStrLn("=========== Menu Participantes ===========")
    putStrLn "1 - Adicionar Participante"
    putStrLn "2 - Exibir Participante" 
    putStrLn("Opção: ")
    opCliente <- getChar
    getChar
    switchOpCliente opCliente

switchOpCliente :: Char -> IO ()
switchOpCliente '1' = do
    addParticipantes "db/dados.sqlite"
    return()
switchOpCliente '2' = do
    lerParticipantes 
    return()
switchOpCliente _ = do
    putStrLn "Opção inválida"

