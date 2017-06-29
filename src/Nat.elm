module Nat exposing
    ( Nat (..)
    , isZero
    , isSucc
    , plus
    , mult
    , minus
    , power
    , natToInt
    , intToNat
    , lte
    , gte
    , lt
    , gt
    , compare
    )

type Nat = Z | S Nat

isZero : Nat -> Bool
isZero nat =
    case nat of
        Z -> True
        S n -> False

isSucc : Nat -> Bool
isSucc = not << isZero


plus : Nat -> Nat -> Nat
plus left right =
    case left of
        Z -> right
        S n -> S <| plus n right


minus : Nat -> Nat -> Maybe Nat
minus left right =
    if gte left right
        then case left of
            Z -> Just Z
            S l -> case right of
                Z -> Just left
                S r -> minus l r
        else Nothing

power : Nat -> Nat -> Nat
power base exp =
    case exp of
        Z -> S Z
        S n -> mult base <| power base n


mult : Nat -> Nat -> Nat
mult left right =
    case left of
        Z -> Z
        S n -> plus right <| mult n right


intToNat : Int -> Maybe Nat
intToNat int =
    if int >= 0
        then Just <| intToNatZero int
    else
        Nothing


intToNatZero : Int -> Nat
intToNatZero int =
    if (int > 0)
        then S <| intToNatZero (int - 1)
        else Z


natToInt : Nat -> Int
natToInt nat =
    case nat of
        Z -> 0
        S n -> 1 + natToInt n


lte : Nat -> Nat -> Bool
lte left right =
    case left of
        Z -> True
        S l ->
            case right of
            Z -> False
            S r -> lte l r


gte : Nat -> Nat -> Bool
gte left right = lte right left


lt : Nat -> Nat -> Bool
lt left right = lte (S left) right


gt : Nat -> Nat -> Bool
gt left right = lt right left


compare : Nat -> Nat -> Order
compare left right =
    if lt left right
        then LT
    else if gt left right
        then GT
    else EQ

