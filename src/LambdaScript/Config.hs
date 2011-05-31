-- | Stores and manipulates configuration data for lsc.
module LambdaScript.Config (Cfg (..), defCfg) where

data Cfg = Cfg {
    forceModName :: String, -- ^ If the module name of the base module should
                            --   be forcefully set to something other than the
                            --   module's file name sans extension, set it to
                            --   this value. Empty string means don't force.
                            --   Default: ""
    libDir :: String,       -- ^ The directory where libraries and runtime
                            --   are kept.
                            --   Default: "./lib"
    extraLibDirs::[String], -- ^ Additional directories to search for library
                            --   files when compiling.
                            --   Default: []
    output :: String,       -- ^ The file to write the comiled bundle to, sans
                            --   file extension which will always be .js.
                            --   Default: "a.out"
    input :: String,        -- ^ The file to compile.
                            --   Default: ""
    tailcalls :: Bool       -- ^ Whether to optimize tail calls or not.
                            --   Default: True
  } deriving Show

-- | The default configuration.
defCfg :: Cfg
defCfg = Cfg {
    forceModName = "",
    libDir = "./lib",
    extraLibDirs = [],
    output = "a.out",
    input = "",
    tailcalls = True
  }