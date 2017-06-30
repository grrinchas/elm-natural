module Nat exposing
    ( Nat (Z, S)
    , isZero
    , isSucc
    , succ
    , pred
    , plus
    , minus
    , mult
    , pow
    , toInt
    , fromInt
    , lte
    , gte
    , lt
    , gt
    , compare
    , min
    , max
    )

{-| A library for natural numbers. Every natural number is either a zero or a positive number. The numbers are
represented as [Peano](https://en.wikipedia.org/wiki/Peano_axioms) numbers using only zero value and a successor
function.

# Constructors
@docs Nat

# Operations
@docs isZero, isSucc, succ, pred, plus, minus, mult, pow

# Comparisons
@docs compare, lt, lte, gt, gte, min, max

# Conversions
@docs toInt, fromInt
-}


{-| A `Nat` is either zero `Z` or one more than some other natural number `S Nat`.

    Z -- constructs zero
    S Z -- constructs one
    S (S Z) -- constructs two
-}
type Nat = Z | S Nat


{-| Checks if `Nat` is zero. Returns `True` if it is or `False` if it isn't.

    isZero Z == True
    isZero (S Z) == False

-}
isZero : Nat -> Bool
isZero nat =
    case nat of
        Z -> True
        S n -> False


{-| Checks if `Nat` is not a zero. Returns `True` if it is not or `False` if it is.

    isSucc Z == False
    isSucc (S Z) == True

-}
isSucc : Nat -> Bool
isSucc = not << isZero

{-| Increments `Nat` by one.

    -- incrementing zero yields one
    succ Z == S Z

    -- incrementing one yields two
    succ (S Z) == S (S Z)

-}
succ : Nat -> Nat
succ nat = S nat


{-| Decrements `Nat` by one. If input `Nat` is zero, then resulting `Nat` will be zero as well.

    -- decrementing two yields one
    pred (S (S Z)) == S Z

    -- decrementing one yields zero
    pred (S Z) == Z

    -- decrementing zero yields zero
    pred Z == Z

-}
pred : Nat -> Nat
pred nat =
    case nat of
        Z -> Z
        S n -> n


{-| Adds two `Nat` together.

    -- adding zero and zero yields zero
    plus Z Z == Z

    -- adding zero and one yields one
    plus Z (S Z) == S Z

    -- adding one and one yields two
    plus (S Z) (S Z) == S (S Z)

-}
plus : Nat -> Nat -> Nat
plus left right =
    case left of
        Z -> right
        S n -> S <| plus n right


{-| Subtracts second `Nat` from the first one. If the second `Nat` is
larger than the first one, then resulting `Nat` will be zero.

    -- subtracting zero from zero yields zero
    minus Z Z == Z

    -- subtracting one from zero yields zero
    minus Z (S Z) == Z

    -- subtracting one from two yields one
    minus (S (S Z)) (S Z) == (S Z)

-}
minus : Nat -> Nat -> Nat
minus left right =
    case left of
        Z -> Z
        S l -> case right of
            Z -> left
            S r -> minus l r


{-| Multiplies two `Nat` together.

    -- multiplying zero by one yields zero
    mult Z (S Z) == Z

    -- multiplying two by two yields four
    mult (S (S Z)) (S (S Z)) == S (S (S (S Z)))

-}
mult : Nat -> Nat -> Nat
mult left right =
    case left of
        Z -> Z
        S n -> plus right <| mult n right


{-| Rises the first `Nat` by the second `Nat`.

    -- rising zero by zero yields one
    pow Z Z == S Z

    -- rising one by two yields one
    pow (S Z) (S (S Z)) == S Z

    -- rising two by two yields four
    pow (S (S Z)) (S (S Z)) == S (S (S (S Z)))

-}
pow : Nat -> Nat -> Nat
pow base exp =
    case exp of
        Z -> S Z
        S n -> mult base <| pow base n


{-| Checks if the first `Nat` is less than or equal to the second `Nat`.
Returns `True` if this is the case or `False` otherwise.

    -- comparing zero with zero yields True
    lte Z Z == True

    -- comparing one with zero yields False
    lte (S Z) Z == False

    -- comparing zero with one yields True
    lte Z (S Z) == True

-}
lte : Nat -> Nat -> Bool
lte left right =
    case left of
        Z -> True
        S l ->
            case right of
            Z -> False
            S r -> lte l r


{-| Checks if the first `Nat` is greater than or equal to the second `Nat`.
Returns `True` if this is the case or `False` otherwise.

    -- comparing zero with zero yields True
    gte Z Z == True

    -- comparing one with zero yields True
    gte (S Z) Z == True

    -- comparing zero with one yields False
    gte Z (S Z) == False

-}
gte : Nat -> Nat -> Bool
gte left right = lte right left


{-| Checks if the first `Nat` is less than the second `Nat`.
Returns `True` if this is the case or `False` otherwise.

    -- comparing zero with zero yields False
    lt Z Z == False

    -- comparing one with zero yields False
    lt (S Z) Z == False

    -- comparing zero with one yields True
    lt Z (S Z) == True

-}
lt : Nat -> Nat -> Bool
lt left right = lte (S left) right



{-| Checks if the first `Nat` is greater than the second `Nat`.
Returns `True` if this is the case or `False` otherwise.

    -- comparing zero with zero yields False
    gt Z Z == False

    -- comparing one with zero yields True
    gt (S Z) Z == True

    -- comparing zero with one yields False
    gt Z (S Z) == False

-}
gt : Nat -> Nat -> Bool
gt left right = lt right left


{-| Compares two `Nat` together and returns value of `Order`.

    -- comparing zero with zero yields EQ
    compare Z Z == EQ

    -- comparing one with zero yields GT
    compare (S Z) Z == GT

    -- comparing zero with one yields LT
    compare Z (S Z) == LT

-}
compare : Nat -> Nat -> Order
compare left right =
    if lt left right
        then LT
    else if gt left right
        then GT
    else EQ


{-| Compares two `Nat` together and returns smaller one.

    -- minimum of zero and zero is zero
    min Z Z == Z

    -- minimum of one and zero is zero
    min (S Z) Z == Z

-}
min : Nat -> Nat -> Nat
min left right =
    case compare left right of
        GT -> right
        _ -> left


{-| Compares two `Nat` together and returns larger one.

    -- maximum of zero and zero is zero
    max Z Z == Z

    -- maximum of one and zero is one
    max (S Z) Z == S Z

-}
max : Nat -> Nat -> Nat
max left right =
    case compare left right of
        GT -> left
        _ -> right


{-| Converts `Nat` to `Int`.

    toInt Z == 0
    toInt (S Z) == 1
-}
toInt : Nat -> Int
toInt nat =
    case nat of
        Z -> 0
        S n -> 1 + toInt n


{-| Converts `Int` to `Nat`. If `Int` is negative, then resulting `Nat` will be zero.

    fromInt 0 == Z
    fromInt 3 == S (S (S Z))

    -- converting negative `Int` yields zero
    fromInt -1 == Z
-}
fromInt : Int -> Nat
fromInt int =
    if (int > 0)
        then S <| fromInt (int - 1)
        else Z



