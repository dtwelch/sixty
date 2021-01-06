module Environment where

import Protolude hiding (IntMap)

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Index
import qualified Index.Map
import qualified Index.Map as Index
import Monad
import qualified Scope
import Var (Var)
import qualified Var

data Environment value v = Environment
  { scopeKey :: !Scope.KeyedName
  , indices :: Index.Map v Var
  , values :: IntMap Var value
  , glueableBefore :: !(Index (Succ v))
  } deriving Show

empty :: Scope.KeyedName -> Environment value Void
empty key =
  Environment
    { scopeKey = key
    , indices = Index.Map.Empty
    , values = mempty
    , glueableBefore = Index.zero
    }

emptyFrom :: Environment value' v -> Environment value Void
emptyFrom env =
  Environment
    { scopeKey = scopeKey env
    , indices = Index.Map.Empty
    , values = mempty
    , glueableBefore = Index.zero
    }

extend
  :: Environment value v
  -> M (Environment value (Succ v), Var)
extend env = do
  var <- freshVar
  pure (extendVar env var, var)

extendVar
  :: Environment value v
  -> Var
  -> Environment value (Succ v)
extendVar env v =
  env
    { indices = indices env Index.Map.:> v
    , glueableBefore = Index.succ $ glueableBefore env
    }

extendValue
  :: Environment value v
  -> value
  -> M (Environment value (Succ v), Var)
extendValue env value = do
  var <- freshVar
  pure
    ( env
      { indices = indices env Index.Map.:> var
      , values = IntMap.insert var value (values env)
      , glueableBefore = Index.succ $ glueableBefore env
      }
    , var
    )

lookupVarIndex :: Var -> Environment value v -> Maybe (Index v)
lookupVarIndex var context =
  Index.Map.elemIndex var (indices context)

lookupIndexVar :: Index v -> Environment value v -> Var
lookupIndexVar index env =
  Index.Map.index (indices env) index

lookupIndexValue :: Index v -> Environment value v -> Maybe value
lookupIndexValue index env =
  lookupVarValue (lookupIndexVar index env) env

lookupVarValue :: Var -> Environment value v -> Maybe value
lookupVarValue v env =
  IntMap.lookup v $ values env
