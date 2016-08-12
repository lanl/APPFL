module ADT where

import AST (PrimType)

data Monotype = MVar String
              | MFun Monotype Monotype
              | MCon (Maybe Bool) String [MonoType]
              | MPVar String
              | MPrim PrimType
              | MPhony