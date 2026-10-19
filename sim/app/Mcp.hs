module Main (main) where

import Sim.Mcp.Server (serve)
import Sim.Mcp.Tools (tools)

main :: IO ()
main = serve tools
