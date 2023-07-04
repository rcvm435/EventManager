{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}

module Main where
import Evento
import Palestras
import Participantes
import Salas

import Control.Applicative
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Data.Time
import Database.SQLite.Simple
import Database.SQLite.Simple.Types
import Data.String(fromString)

import Database.SQLite.Simple

import Database.SQLite.Simple

import Database.SQLite.Simple
import qualified Data.Text as T


main :: IO ()
main = do
  
  putStrLn "=========== Gerenciador de Eventos ==========="
  putStrLn "Digite '1' para gerenciar eventos"
  putStrLn "Digite '2' para gerenciar palestras"
  putStrLn "Digite '3' para gerenciar participantes"
  putStrLn "Digite '4' para gerenciar salas"
  putStrLn "Digite '0' para finalizar o programa"
  putStrLn "Opção: "
  op <- getChar
  getChar
  switchOp op
  if op == '0'
    then return ()
    else main

switchOp :: Char -> IO ()
switchOp '1' = do
  menuEventos
switchOp '2' = do
  menuPalestras
switchOp '3' = do
  menuParticipantes
switchOp '4' = do
  menuSalas
switchOp _ = putStrLn "Opção inválida"

