module Calculus where

{- NOTES ABOUT HOW IT WORKS
I've changed so much I thought it would be worth a little paragraph here

Simplification rules:
~ factorisation
~ evaluation
~ coefficients out front
~ constants out back
~ alphabetically ordered letters
~ fraction simplification

Most of these are so the data structures are predictable

Roughly how the simplification system does factorisation
x*2 + x
= x*(2 + 1)    -- recognises common factor
= 3*x          -- evaluates in brackets

Because one of these steps is evalutation, my eval function is just a
find and replace

Products of things are thought of semantically as linked lists, the left
operand of a multiply sign can only ever be a constant or another *
It also sorts expressions alphabetically
Moves constants 
-}

import Data.Maybe
import Data.List

data UnOp = Neg | Sin | Cos | Log
          deriving (Eq, Ord, Show)

data BinOp = Add | Mul | Div
           deriving (Eq, Ord, Show)

data Exp = Val Double | Id String | UnApp UnOp Exp | BinApp BinOp Exp Exp
         deriving (Eq)

type Env = [(String, Double)]

---------------------------------------------------------------------------
-- Type classes and class instances

instance Show Exp where
  show = showExp

instance Ord Exp where
  compare (Val a) (Val b) = compare a b
  compare (Val _)  _      = LT
  compare _       (Val _) = GT
  compare (Id a)  (Id b)  = compare a b
  compare _       (Id _)  = GT
  compare _        _      = LT

-- Got rid of this class so x,y,z would reliably be Exp
x, y, z :: Exp
x = Id "x"
y = Id "y"
z = Id "z"

instance Num Exp where
  fromInteger = Val . fromInteger
  -- Negation
  negate 0             = 0           --   -0 = 0
  negate (UnApp Neg a) = a           --  --a = a
  negate (Val a)       = Val (-a)    -- evaluation
  negate a             = UnApp Neg a -- construction
  -- Addition
  0              +     b = b
  a              +     0 = a
  Val a          + Val b = Val (a + b) -- evaluation
  Val a          +     b = b + Val a   -- puts constants at back and
  BinApp Add a b +     c = a + (b + c) -- forces left associativity (so they end up together)
  a              +     b
    | h /= 1             = h * (a/h + b/h) -- factorisation
    | otherwise          = BinApp Add a b  -- construction
      where
        h = product (factors a ∩ factors b)
  -- Multiplication
  0              * _              = 0
  _              * 0              = 0
  (Val a)        * (Val b)        = Val (a * b)    -- evaluation
  a              * BinApp Mul b c = (a * b) * c    -- forces right associativity (so they end up together)
  a              * BinApp Div b c = (a * b) / c    -- Makes fractions 'swallow' products
  BinApp Div a b * c              = (a * c) / b    -- so fractions can be simplified 
  BinApp Mul a b * c
    | swap = (a * c) * b
      where
        swap = case (b, c) of
          (_    , Val _) -> True
          (Id bi, Id ci) -> bi > ci
          (_    , Id _ ) -> True
          _              -> False
  Val a          * b              = BinApp Mul (Val a) b -- construction
  BinApp Mul a b * c              = BinApp Mul (a * b) c -- construction
  a              * b              = (1 * a) * b          -- makes products into linked lists where constant is the empty list
-- Leave the following two undefined...
  signum      = undefined
  abs         = undefined

-- training function: simplify ("abc", "b") = ("ac", "")
simplify :: (String, String) -> (String, String)
simplify ((n : ns), (d : ds))
  | n < d  = (n : nsLt, dsLt)
  | n == d = simplify (ns, ds)
  | n > d  = (nsGt, d : dsGt)
      where
        (nsLt, dsLt) = simplify (ns, d : ds)
        (nsGt, dsGt) = simplify (n : ns, ds)
simplify a
  = a

-- simplifies fractions 
simplifyFrac :: Exp -> Exp
simplifyFrac (BinApp Div a (Val b))
  = BinApp Div (a * (Val (1/b))) 1
simplifyFrac (BinApp Div (BinApp Mul ns n) (BinApp Mul ds d))
  | n > d  = BinApp Div (nsLt * n) dsLt
  | n == d = simplifyFrac (BinApp Div ns ds)
  | n < d  = BinApp Div nsGt (dsGt * d)
    where
      BinApp Div nsLt dsLt = simplifyFrac (BinApp Div ns (ds * d))
      BinApp Div nsGt dsGt = simplifyFrac (BinApp Div (ns * n) ds)
simplifyFrac a = a

instance Fractional Exp where
  fromRational = Val . fromRational
  -- Division
  0 / _                    = 0
  _ / 0                    = Val (1/0) -- Infinity
  a / 1                    = a
  a / b           | a == b = 1
  a / BinApp Div b c       = (a * c) / b   -- 1/(1/x) = x
  a / b                    = case simplifyFrac (BinApp Div (1*a) (1*b)) of
    BinApp Div a 1 -> a
    x              -> x
-- Leave the following one undefined...
  recip        = undefined

instance Floating Exp where
  sin (Val a) = Val (sin a)
  sin      a  = UnApp Sin a
  cos (Val a) = Val (cos a)
  cos      a  = UnApp Cos a
  log (Val a) = Val (log a)
  log      a  = UnApp Log a
-- Leave the following fifteen undefined...
  tan     = undefined
  asin    = undefined
  acos    = undefined
  atan    = undefined
  pi      = undefined
  exp     = undefined
  sqrt    = undefined
  (**)    = undefined
  logBase = undefined
  sinh    = undefined
  cosh    = undefined
  tanh    = undefined
  asinh   = undefined
  acosh   = undefined
  atanh   = undefined

---------------------------------------------------------------------------
-- Helper Functions

-- "Blackbird" Operator
-- f x y = g (h x y)
-- f     = g ... h
(...) = (.) . (.)

-- [1,1] ∩ [1] == [1]
(∩) :: Eq a => [a] -> [a] -> [a]
(∩) _    []   = []
(∩) (as) (b : bs)
  | elem b as = b : as ∩ bs
  | otherwise =     as ∩ bs

-- splits list into two lists based on a condition
-- splitBy even [1..] = ([2, 4..], [1, 3..])
splitBy :: Eq a => (a -> Bool) -> [a] -> ([a], [a])
splitBy _ [] = ([], [])
splitBy cond (l:ls)
  | cond l    = (l:ts, fs)
  | otherwise = (ts, l:fs)
    where
      (ts, fs) = splitBy cond ls

-- overly complicated algorithm for the performance
-- requirements but I was taught this method during
-- my imperial interview :)
-- primes = [2, 3, 5, 7, 11..]
primes :: [Int]
primes = sieve (2 : [3, 5..])
  where
    sieve (p:xs) = p : sieve [ x | x <- xs, x `mod` p > 0 ]

-- primeFactors 63 = [3, 3, 7]
primeFactors :: Int -> [Int]
primeFactors 1 = []
primeFactors n = prime : primeFactors (n `div` prime)
  where
    prime = head (filter ((0 ==) . (n `mod`)) primes)

-- Gets factors of exp
-- factors (x*x)   = [x,x]
-- factors (x*x)/x = [x]
-- factors (x + x) = [x]
factors :: Exp -> [Exp]
factors (Id         id ) = [Id id]
factors (UnApp  Neg a  ) = factors a
factors (BinApp Add a b) = factors a ∩ factors b
factors (BinApp Mul a b) = factors a ++ factors b
factors (BinApp Div a b) = factors a \\ factors b
factors (Val a)
  | a < 0                = factors (Val (-a)) -- this fixes the infinite loop
  | isInt                = map (Val . fromIntegral) (primeFactors (round a))
  where
    isInt = fromIntegral (round a) == a
factors _                = []

-- removes factors from exp
rmFactors :: [Exp] -> Exp -> Exp
rmFactors [] exp              = exp
rmFactors _  (Id         _  ) = 1
rmFactors fs (UnApp  Neg a  ) = - rmFactors fs a
rmFactors fs (BinApp Add a b) = rmFactors fs a + rmFactors fs b
rmFactors fs (BinApp Mul a b) = rmFactors fsa a * rmFactors fsb b
  where (fsa, fsb) = splitBy (`elem` factors a) fs
rmFactors fs (BinApp Div a b) = rmFactors fs a / b

---------------------------------------------------------------------------

-- showExp ((x * y) + z) = "x * y + z"
showExp :: Exp -> String
showExp exp = showBracketLess (wrap exp) exp
  where
    -- takes an expression and 
    wrap :: Exp -> Exp -> String
    wrap outer inner
      | bracketed = "(" ++ innerText ++ ")"
      | spaced    = ' '  : innerText
      | otherwise =        innerText
        where
          innerText = showBracketLess (wrap inner) inner
          -- whether or not brackets are needed
          bracketed = bidmasLvl outer > bidmasLvl inner
            where
              bidmasLvl (BinApp Add _ _) = 1
              bidmasLvl (BinApp Div _ _) = 2
              bidmasLvl (BinApp Mul _ _) = 3
              bidmasLvl (UnApp _ _)      = 4
              bidmasLvl _                = 5
          -- whether or not a space is needed
          spaced = case outer of
            UnApp Neg _ -> False
            UnApp _   _ -> True
            _           -> False

    showBracketLess :: (Exp -> String) -> Exp -> String
    showBracketLess w exp = case exp of
      UnApp  Neg e     ->          "-"  ++ w e
      UnApp  Sin e     ->         "sin" ++ w e
      UnApp  Cos e     ->         "cos" ++ w e
      UnApp  Log e     ->         "log" ++ w e
      BinApp Add e1 e2 -> case e2 of
        UnApp Neg e2'  -> w e1 ++ " - " ++ w e2'
        Val v | v < 0  -> w e1 ++ " - " ++ w (Val (-v))
        _              -> w e1 ++ " + " ++ w e2
      BinApp Mul e1 e2 -> case e1 of
        (Val 1)        ->                  w e2
        _              -> w e1 ++  "*"  ++ w e2
      BinApp Div e1 e2 -> w e1 ++  "/"  ++ w e2
      Id         k     ->                    k
      Val        v
        | isInt        -> show $ round v -- 1.0 -> "1" instead of "1.0"
        | otherwise    -> show v
          where isInt = fromIntegral (round v) == v

lookUp :: Eq a => a -> [(a, b)] -> b
lookUp
  = fromJust ... lookup

-- fromExp (5 :: Exp) = (5 :: Double)
fromExp :: Exp -> Double
fromExp (Val x) = x
fromExp      x  = error (show x ++ " | must be a value")

-- find & replace for expressions
-- replace (sin x) (cos x) (2*sin x + 5) = (2*cos x + 5)
replace :: Exp -> Exp -> Exp -> Exp
replace n n' haystack -- needle, new needle, haystack
  | n == haystack = n'
  | otherwise     = case haystack of
    Val        a   ->     Val a
    Id         a   ->      Id a
    UnApp  Neg a   ->   0 - r a  -- I know this looks messy
    UnApp  Sin a   ->  sin (r a) -- but using a lookup table
    UnApp  Cos a   ->  cos (r a) -- would be even messier and
    UnApp  Log a   ->  log (r a) -- I can't create expressions
    BinApp Add a b -> r a + r b  -- normally because simplification
    BinApp Mul a b -> r a * r b  -- happens in the overloaded
    BinApp Div a b -> r a / r b  -- function expressions
    where r = replace n n'

-- evaluates an expression
-- the simplifications is so aggressive there's nothing
-- for eval to do really other than replace the identifiers
-- and change the types round for the tests
eval :: Exp -> Env -> Double
eval = fromExp ... foldr (\(k, v) -> replace (Id k) (Val v))

-- x is the x in d/dx but it can be any var
diff :: Exp -> Exp -> Exp
diff (Val        _  ) _             = 0
diff e x                  | e == x  = 1
diff (Id i)       (Id x)  | i /= x  = 0                                     -- rules:
diff (BinApp Add a b) x             =      diff a x + diff b x                -- addition
diff (BinApp Mul a b) x             =  b * diff a x + diff b x * a            -- product
diff (BinApp Div a b) x             = (b * diff a x - diff b x * a) / (b * b) -- quotient
diff (UnApp  Neg a  ) x             =    - diff a x                           -- negation
diff (UnApp  f   a  ) x   | a /= x  =      diff a x * diff (UnApp f a) (a)    -- chain rule
diff (UnApp  Sin a  ) _             = cos a
diff (UnApp  Cos a  ) _             = - sin a
diff (UnApp  Log a  ) _             = 1 / a

-- expands a function into its infinite power series
-- differentiates expression with respect to x (as passed)
-- expand (sin x) x = [0, x, 0, x^3/3!, 0, x^5/5!, ...]
expand :: Exp -> Exp -> [Exp]
expand f x
  = zipWith3 ((/) ... (*)) derivatives numers denoms         -- = [f(0)*x^0/0!, f'(0)*x^1/1!, f''(0)*x^2/2!, f'''(0)*x^3/3!, ...]
    where
      derivatives = map (replace x 0) (iterate (`diff` x) f) -- = [f(0), f'(0), f''(0), f'''(0), ...]
      numers = iterate (* x) 1                               -- = [x^0, x^1, x^2, x^3, ...]
      denoms = Val 1 : zipWith (*) denoms (map Val [1..])    -- = [0!, 1!, 2!, 3!, ...]

-- sin x -> pi/2 -> # of terms -> 1
maclaurin :: Exp -> Double -> Int -> Double
maclaurin f value n
  = fromExp $ replace x (Val value) $ sum $ take n (expand f x)

---------------------------------------------------------------------------
-- Test cases...

e1, e2, e3, e4, e5, e6 :: Exp

-- 5*x
e1 = BinApp Mul (Val 5.0) (Id "x")

-- x*x+y-7
e2 = BinApp Add (BinApp Add (BinApp Mul (Id "x") (Id "x")) (Id "y"))
                (UnApp Neg (Val 7.0))

-- x-y^2/(4*x*y-y^2)::Exp
e3 = BinApp Add (Id "x")
            (UnApp Neg (BinApp Div (BinApp Mul (Id "y") (Id "y"))
            (BinApp Add (BinApp Mul (BinApp Mul (Val 4.0) (Id "x")) (Id "y"))
                        (UnApp Neg (BinApp Mul (Id "y") (Id "y"))))))

-- -cos x::Exp
e4 = UnApp Neg (UnApp Cos (Id "x"))

-- sin (1+log(2*x))::Exp
e5 = sin(1.0 + log(2.0*x))

-- log(3*x^2+2)::Exp
e6 = log(3.0*(x*x) + 2.0)

