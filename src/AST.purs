module AST where

-- import Data.Tuple
import Prelude

-- import Control.Monad.Except (ExceptT, Except)
-- import Data.Generic.Rep (class Generic)
-- import Data.List.Types (NonEmptyList)
-- import Data.Map (Map)
-- import Data.Maybe (Maybe(..))
-- import Data.Newtype (traverse)
-- import Data.Show.Generic (genericShow)
-- import Effect (Effect)
-- import Foreign (Foreign, ForeignError, readArray, readInt, readNullOrUndefined, readString)
-- import Foreign.Index ((!))


-- readTuple :: Foreign -> Except (NonEmptyList ForeignError) (Tuple String String)
-- readTuple value = do 
  -- a <- value ! 0 >>= readString 
  -- b <- value ! 1 >>= readString 
  -- pure $ Tuple a b

-- readSourceLoc :: Foreign -> Except (NonEmptyList ForeignError) SourceLoc
-- readSourceLoc value = do 
  -- line <- value ! "line" >>= readInt 
  -- col <- value ! "col" >>= readInt 
  -- offset <- value ! "offset" >>= readInt 
  -- pure $ SourceLoc { line, col, offset }

-- readChild :: Foreign -> Except (NonEmptyList ForeignError) Child
-- readChild value = do 
  -- tag <- value ! "tag" >>= readString 
  -- children <- value ! "children" >>= readArray >>= traverse readChild
  -- -- let children = Nothing
  -- attributes <- value ! "attributes" >>= readNullOrUndefined >>= readArray >>= traverse readTuple
  -- pos <- value ! "pos" >>= readNullOrUndefined >>= readSourceLoc
  -- pure $ Child { tag, children, attributes, pos }

-- readDoc :: Foreign -> Except (NonEmptyList ForeignError) Doc
-- readDoc value = do 
  -- tag <- value ! "tag" >>= readString 
  -- references <- value ! "references" >>= traverse readString
  -- footnotes <- value ! "footnotes" >>= traverse readString
  -- children <- value ! "children" >>= readArray >>= traverse readChild
  -- pure $ Doc { tag, references, footnotes, children }

-- derive instance genericChild :: Generic Child _

-- instance showChild :: Show Child where
  -- show child = genericShow child

-- derive instance genericDoc :: Generic Doc _