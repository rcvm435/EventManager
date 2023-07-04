module Palestras where


import Database.SQLite.Simple
import Database.SQLite.Simple.Types
import Data.String(fromString)

import Database.SQLite.Simple

import Database.SQLite.Simple

import Database.SQLite.Simple
import qualified Data.Text as T

inserirPalestras :: Connection -> Int -> Int -> String -> String -> String -> IO ()
inserirPalestras conn palestraID eventoID palestraTitulo palestraPalestrante palestraHorario = do
  execute conn (Query $ T.pack "INSERT INTO Palestras (palestraID, eventoID, palestraTitulo, palestraPalestrante, palestraHorario) VALUES (?, ?, ?, ?, ?)") (palestraID, eventoID, palestraTitulo, palestraPalestrante, palestraHorario)
  putStrLn "Palestras inseridas com sucesso!"

addPalestras :: String -> IO ()
addPalestras "db/dados.sqlite" = do
  conn <- open "db/dados.sqlite"

  putStrLn "Inserção de Palestras"
  putStrLn "palestraID:"
  palestraID <- readLn :: IO Int
  putStrLn "eventoID:"
  eventoID <- readLn :: IO Int
  putStrLn "palestraTitulo:"
  palestraTitulo <- getLine
  putStrLn "palestraPalestrante:"
  palestraPalestrante <- getLine
  putStrLn "palestraHorario:"
  palestraHorario <- getLine

  inserirPalestras conn palestraID eventoID palestraTitulo palestraPalestrante palestraHorario

  close conn


data Palestra = Palestra
  { palestraID :: Int
  , eventoID :: Int
  , palestraTitulo :: String
  , palestraPalestrante :: String
  , palestraHorario :: String
  }

instance FromRow Palestra where
  fromRow = Palestra <$> field <*> field <*> field <*> field <*> field

lerPalestras :: IO ()
lerPalestras = do
  conn <- open "db/dados.sqlite"
  let query = fromString "SELECT palestraID, eventoID, palestraTitulo, palestraPalestrante, palestraHorario FROM Palestras"
  palestras <- query_ conn query :: IO [Palestra]
  putStrLn "=========== Palestras: ==========="
  mapM_ (\(Palestra palestraID eventoID palestraTitulo palestraPalestrante palestraHorario) -> do
    putStrLn $ "ID: " ++ show (palestraID :: Int)
    putStrLn $ "Evento ID: " ++ show (eventoID :: Int)
    putStrLn $ "Título: " ++ palestraTitulo
    putStrLn $ "Palestrante: " ++ palestraPalestrante
    putStrLn $ "Horário: " ++ palestraHorario
    putStrLn "") palestras
  putStrLn "Aperte ENTER para continuar..."
  getLine
  close conn

  
menuPalestras :: IO()
menuPalestras = do 
    
    putStrLn("=========== Menu Palestras ===========")
    putStrLn "1 - Adicionar Palestras"
    putStrLn "2 - Exibir Palestras" 
    putStrLn("Opção: ")
    opCliente <- getChar
    getChar
    switchOpCliente opCliente

switchOpCliente :: Char -> IO ()
switchOpCliente '1' = do
    addPalestras "db/dados.sqlite"
    return()
switchOpCliente '2' = do
    lerPalestras 
    return()
switchOpCliente _ = do
    putStrLn "Opção inválida"

