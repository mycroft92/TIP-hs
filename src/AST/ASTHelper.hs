module AST.ASTHelper where

import AST.AST

noDup :: (Eq a) => [a] -> [a] -> [a]
noDup x1 x2 = foldr (\e acc -> if elem e acc then acc else e : acc) x1' x2
  where
    x1' = foldr (\e acc -> if elem e acc then acc else e : acc) [] x1

appearingFieldsInExp :: AExpr -> [String]
appearingFieldsInExp (Record rs _) = foldr getFieldName [] rs
  where
    getFieldName (RecField n _ _) acc = if elem n acc then acc else n : acc
appearingFieldsInExp (FieldAccess _ n _) = [n]
appearingFieldsInExp (Binop e1 _ e2 _) = noDup (appearingFieldsInExp e1) (appearingFieldsInExp e2)
appearingFieldsInExp (Unop _ e _) = appearingFieldsInExp e
appearingFieldsInExp (Alloc e _) = appearingFieldsInExp e
appearingFieldsInExp (CallExpr f1 es _) = foldr (\e acc -> noDup (appearingFieldsInExp e) acc) (appearingFieldsInExp f1) es
appearingFieldsInExp _ = []

appearingFieldsInLExp :: LExp -> [String]
appearingFieldsInLExp (DirectWrite _ n _) = [n]
appearingFieldsInLExp (IndirectWrite e n _) = n : (appearingFieldsInExp e)
appearingFieldsInLExp _ = []

appearingFieldsInStmt :: AStmt -> [String]
appearingFieldsInStmt (SimpleAssign le e _) = noDup (appearingFieldsInLExp le) (appearingFieldsInExp e)
appearingFieldsInStmt (FieldAssign le e _) = noDup (appearingFieldsInLExp le) (appearingFieldsInExp e)
appearingFieldsInStmt (Output e _) = appearingFieldsInExp e
appearingFieldsInStmt (Seq s1 s2 _) = noDup (appearingFieldsInStmt s1) (appearingFieldsInStmt s2)
appearingFieldsInStmt (IfStmt e s1 (Just s2) _) = noDup (noDup (appearingFieldsInStmt s1) (appearingFieldsInStmt s2)) (appearingFieldsInExp e)
appearingFieldsInStmt (IfStmt e s1 Nothing _) = noDup (appearingFieldsInStmt s1) (appearingFieldsInExp e)
appearingFieldsInStmt (WhileStmt e s _) = noDup (appearingFieldsInExp e) (appearingFieldsInStmt s)
appearingFieldsInStmt (NullStmt _) = []

appearingFieldsInFunction :: AFuncDec -> [String]
appearingFieldsInFunction (Fun _ _ _ body ret _) = noDup (appearingFieldsInStmt body) (appearingFieldsInExp ret)

appearingFields :: [AFuncDec] -> [String]
appearingFields fs = foldr (\x acc -> noDup (appearingFieldsInFunction x) acc) [] fs
