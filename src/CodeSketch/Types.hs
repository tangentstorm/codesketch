module CodeSketch.Types
  ( Visibility(..)
  , DefType(..)
  , Definition(..)
  , PathInfo(..)
  , Root
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
  | Class     -- ^ Class definition
  | Function  -- ^ Function or method definition
  | Trait     -- ^ Trait/interface/protocol
  | Other String -- ^ Other definition types
  deriving (Eq, Show)

-- | A single code definition
data Definition = Definition
  { iden       :: String     -- ^ Identifier name
  , defType    :: DefType    -- ^ Type of definition
  , visibility :: Visibility -- ^ Visibility of definition
  } deriving (Eq, Show)

-- | A file path with its definitions
data PathInfo = PathInfo
  { path :: String           -- ^ Path to file
  , defs :: [Definition]     -- ^ List of definitions in the file
  } deriving (Eq, Show)

-- | Root structure of the output
type Root = [PathInfo]