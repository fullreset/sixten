{-# LANGUAGE OverloadedStrings #-}
-- | Desugaring of programs
module Syntax.Desugar where

import Bound
import Control.Applicative
import Control.Monad.Except
import Data.Foldable
import Data.Hashable
import Data.HashMap.Lazy(HashMap)
import qualified Data.HashMap.Lazy as HM
import Data.Monoid
import Data.Text(Text)
import qualified Data.Text as Text

import Syntax
import Syntax.Concrete
import Syntax.Parse
import Util

program :: [TopLevelParsed Name] -> Either Text (Program Plicitness Expr Name)
program xs = snd <$> foldlM resolveName (Nothing, mempty) xs >>= matchTypes
  where
    resolveName :: (Hashable v, Eq v, Show v)
                => (Maybe v, (HashMap v (Expr v), HashMap v (Type v), HashMap v (DataDef Plicitness Type v)))
                -> TopLevelParsed v
                -> Either Text (Maybe v, (HashMap v (Expr v), HashMap v (Type v), HashMap v (DataDef Plicitness Type v)))
    resolveName (prevName, (defs, types, datas)) (ParsedDefLine mn e) = case mn <|> prevName of
      Nothing -> throwError "not sure what a wildcard definition refers to"
      Just n  -> do
        defs' <- insertNoDup (throwError $ "duplicate definition: " <> shower n) n e defs
        return (Just n, (defs', types, datas))
    resolveName (_, (defs, types, datas)) (ParsedTypeDecl n e) = do
      types' <- insertNoDup (throwError $ "duplicate type declaration: " <> shower n) n e types
      return (Just n, (defs, types', datas))
    resolveName (_, (defs, types, datas)) (ParsedData n dataDefi) = do
      datas' <- insertNoDup (throwError $ "duplicate datatype: " <> shower n) n dataDefi datas
      return (Just n, (defs, types, datas'))
    insertNoDup err k v m = case (HM.lookup k m, HM.insert k v m) of
      (Just _, _)   -> err
      (Nothing, m') -> return m'
    matchTypes :: (HashMap Name (Expr Name), HashMap Name (Type Name), HashMap Name (DataDef Plicitness Type Name))
               -> Either Text (Program Plicitness Expr Name)
    matchTypes (defs, types, datas) = case HM.keys $ HM.difference types defs of
      [] -> do
        let defs' = HM.unionWith (\(d, _) (t, _) -> (d, t)) (flip (,) Wildcard <$> defs)
                                                            (flip (,) Wildcard <$> types)
            ldefs = (\(e, t) -> (Definition e, t, Explicit)) <$> defs'
            rdatas = (\x -> (DataDefinition x, dataType x Pi $ Scope Type, Explicit)) <$> datas
        case HM.keys $ HM.intersection ldefs rdatas of
          [] -> return $ ldefs <> rdatas
          vs -> throwError $ "duplicate definition: " <> Text.intercalate ", " (map shower vs)
      vs -> throwError $ "type without a definition: " <> Text.intercalate ", " (map shower vs)
