module CodeSketch.Types
  ( Visibility(..)
  , DefType(..)
  , DefInfo(..)
  , Definition(..)
  , PathInfo(..)
  , Root
  , emptyDefInfo
  ) where

-- | Visibility of a definition
data Visibility
  = Public    -- ^ Public visibility, represented as "*" in output
  | Protected -- ^ Protected visibility, represented as "+" in output
  | Private   -- ^ Private visibility, represented as "-" in output
  deriving (Eq, Show)

-- | Type of code definition
data DefType
  = Module    -- ^ Module or namespace
  | Struct    -- ^ Struct definition
  | Enum      -- ^ Enum definition
  | Function  -- ^ Function or method definition
  | Trait     -- ^ Trait/interface/protocol
  | Impl      -- ^ Implementation block
  | Other String -- ^ Other definition types
  deriving (Eq, Show)

-- | Type to store extra information about a definition
data DefInfo = DefInfo
  { signature :: Maybe String  -- ^ Function or method signature if available
  } deriving (Eq, Show)

-- | Default empty definition info
emptyDefInfo :: DefInfo
emptyDefInfo = DefInfo Nothing

-- | A single code definition
data Definition = Definition
  { iden       :: String     -- ^ Identifier name
  , defType    :: DefType    -- ^ Type of definition
  , visibility :: Visibility -- ^ Visibility of definition
  , defInfo    :: DefInfo    -- ^ Additional definition information
  } deriving (Eq, Show)

-- | A file path with its definitions
data PathInfo = PathInfo
  { path :: String           -- ^ Path to file
  , defs :: [Definition]     -- ^ List of definitions in the file
  } deriving (Eq, Show)

-- | Root structure of the output
type Root = [PathInfo]