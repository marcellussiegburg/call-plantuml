{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-|
Module      : Language.PlantUML.Call
Description : A simple library to call PlantUML given a diagram specification
Copyright   : (c) Marcellus Siegburg, 2022
License     : MIT
Maintainer  : marcellus.siegburg@uni-due.de

This module provides the basic functionality to call PlantUML.
-}
module Language.PlantUML.Call (
  DiagramType (..),
  drawPlantUMLDiagram,
  ) where

import Paths_call_plantuml (getDataDir)

import qualified Data.ByteString.Char8            as BS (
  dropWhile,
  head,
  null,
  putStrLn,
  tail,
  )

import Control.Concurrent (
  forkIO, killThread, newEmptyMVar, putMVar, takeMVar,
  )
import Control.Monad                    (unless, when)
import Data.ByteString                  (ByteString, hGetContents, hPutStr)
import Data.ByteString.Char8            (unpack)
import System.Exit                      (ExitCode (..))
import System.FilePath
  ((</>), (<.>))
import System.IO
  (BufferMode (..), hClose, hFlush, hSetBuffering)
import System.Process (
  CreateProcess (..), StdStream (..),
  createProcess, proc, waitForProcess,
  )

{-|
An output format for PlantUML.
-}
data DiagramType =
  ASCIIArt |
  ASCIIArtUnicode |
  EPS |
  LaTeX |
  LaTeXFull |
  PNG |
  SVG |
  VDX
  deriving (Bounded, Enum, Read, Show)

typeShortName :: DiagramType -> String
typeShortName x = case x of
  ASCIIArt          -> "txt"
  ASCIIArtUnicode   -> "utxt"
  EPS               -> "eps"
  LaTeX             -> "latex"
  LaTeXFull         -> "latex:nopreamble"
  PNG               -> "png"
  SVG               -> "svg"
  VDX               -> "vdx"

{-|
This function may be used to draw a PlantUML diagram given a valid
specification and a return type.
It calls PlantUML via Java.
-}
drawPlantUMLDiagram
  :: DiagramType
  -- ^ The return type of diagram to return
  -> ByteString
  -- ^ The PlantUML diagram specification which should be loaded
  -> IO ByteString
drawPlantUMLDiagram what content = do
  dataDir <- getDataDir
  let callPlantUML = proc "java" [
        "-jar", dataDir </> "plantuml" <.> "jar",
        "-p", "-t" ++ typeShortName what, "-nometadata", "-noerror"
        ]
  (Just hin, Just hout, Just herr, ph) <-
    createProcess callPlantUML {
        std_out = CreatePipe,
        std_in  = CreatePipe,
        std_err = CreatePipe
      }
  pout <- listenForOutput hout
  perr <- listenForOutput herr
#ifndef mingw32_HOST_OS
  hSetBuffering hin NoBuffering
#endif
  hPutStr hin content
  hFlush hin
  hClose hin
  out <- getOutput pout
  err <- getOutput perr
  printContentOnError ph out
  unless (BS.null err) $ fail $ unpack err
  return out
  where
    printContentOnError ph out = do
      code <- waitForProcess ph
      when (code == ExitFailure 1 || isError out)
        $ BS.putStrLn $ "Error on calling PlantUML with:\n" <> content
    listenForOutput h = do
      mvar <- newEmptyMVar
      pid <- forkIO $ hGetContents h >>= putMVar mvar
      return (pid, mvar)
    getOutput (pid, mvar) = do
      output <- takeMVar mvar
      killThread pid
      return output

isError :: ByteString -> Bool
isError xs =
  let ys = BS.dropWhile (== ' ') xs
      zs = BS.dropWhile (== ' ') $ BS.tail ys
  in not (BS.null ys)
  && BS.head ys == '\n'
  && not (BS.null zs)
  && BS.head zs == '\n'
