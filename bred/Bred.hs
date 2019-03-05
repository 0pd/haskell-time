module Bred where

data Term = Var Int | App Term Term | Abs Int Term deriving Show

data Strategy = Normal | Applicative

getNormalForm :: Strategy -> Term -> Term
getNormalForm s t =
    case reduce s t of
        Just term -> getNormalForm s term
        Nothing -> t

reduce :: Strategy -> Term -> Maybe Term
reduce _ (Var _) = Nothing
reduce s (Abs a f) = reduce s f >>= Just . Abs a
reduce s (App f a) =
    case s of
        Normal -> case reduce s f of
            Just t -> Just $ App t a
            Nothing -> case f of
                Abs _ fun -> Just $ substitute 0 fun a
                _ -> reduce s a >>= Just . App f
        Applicative -> case reduce s a of
            Just t -> Just $ App f t
            Nothing -> case f of
                Abs _ fun -> Just $ substitute 0 fun a
                _ -> reduce s f >>= Just . (flip App) a

substitute :: Int -> Term -> Term -> Term
substitute v f a =
    case f of 
        App fun arg -> App (substitute v fun a) (substitute v arg a)
        Abs arg fun -> Abs arg (substitute (v + 1) fun (shift 1 0 a))
        Var var | var == v -> a
        Var var -> Var var

shift :: Int -> Int -> Term -> Term
shift d c (App f a) = App (shift d c f) (shift d c a)
shift d c (Abs a f) = Abs a (shift d (c + 1) f)
shift d c (Var v)
    | v < c = Var v
    | otherwise = Var $ v + d

id' = Abs 0 (Var 0)
f = Abs 1 (Abs 0 (Var 1))
omega = Abs 0 (App (Var 0) (Var 0))
bigOmega = App omega omega
testTerm = App (App f id') bigOmega