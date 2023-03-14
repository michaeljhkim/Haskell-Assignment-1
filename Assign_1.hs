{-
Module      : 1JC3-Assign1.Assign_1.hs
Copyright   :  (c) Curtis D'Alves 2022
License     :  GPL (see the LICENSE file)
Maintainer  :  none
Stability   :  experimental
Portability :  portable

Description:
  Assignment 1 - McMaster CS 1JC3 2022
-}
module Assign_1 where

-----------------------------------------------------------------------------------------------------------
-- INSTRUCTIONS              README!!!
-----------------------------------------------------------------------------------------------------------
-- 1) DO NOT DELETE/ALTER ANY CODE ABOVE THESE INSTRUCTIONS AND DO NOT ADD ANY IMPORTS
-- 2) DO NOT REMOVE / ALTER TYPE DECLERATIONS (I.E THE LINE WITH THE :: ABOUT THE FUNCTION DECLERATION)
--    IF YOU ARE UNABLE TO COMPLETE A FUNCTION, LEAVE IT'S ORIGINAL IMPLEMENTATION (I.E. THROW AN ERROR)
-- 3) MAKE SURE THE PROJECT COMPILES (I.E. RUN STACK BUILD AND MAKE SURE THERE ARE NO ERRORS) BEFORE
--    SUBMITTING, FAILURE TO DO SO WILL RESULT IN A MARK OF 0
-- 4) REPLACE macid = "TODO" WITH YOUR ACTUAL MACID (EX. IF YOUR MACID IS jim THEN macid = "jim")
-----------------------------------------------------------------------------------------------------------

-- Name: Michael Kim
-- Date: September 25 2022
macid :: String
macid = "kim370"

{- -----------------------------------------------------------------
 - ***
 - -----------------------------------------------------------------
 - Description:
 -   This function takes in two doubles and makes sure that for odd numbered rooting, the root is a negative
 - -----------------------------------------------------------------
 - |   Input    |                                              |
 - |     x      | Double input                                 |
 - |     y      | Double input                                 |
 - -----------------------------------------------------------------
 - |   Output   |                                              |
 - |     root   | Double Output                                |
 - -----------------------------------------------------------------
 -}
(***) :: Double -> Double -> Double
x *** y = if x >= 0 then x ** y else -( (-x) ** y)


 {- -----------------------------------------------------------------
 - cubicQ
 - -----------------------------------------------------------------
 - Description:
 -   This function calculates the Q value which is used for the
 -   discriminant of the cubic equation.
 - -----------------------------------------------------------------
 - |   Input    |                                              |
 - |     a      | Double input                                 |
 - |     b      | Double input                                 |
 - |     c      | Double input                                 |
 - -----------------------------------------------------------------
 - |   Output   |                                              |
 - |     Q      | Double Output                                |
 - -----------------------------------------------------------------
 -}
cubicQ :: Double -> Double -> Double -> Double
cubicQ a b c = ((3*a*c)-(b**2)) / (9*(a**2))

{- -----------------------------------------------------------------
 - cubicR
 - -----------------------------------------------------------------
 - Description:
 -   This function calculates the R value which is used for the
 -   discriminant of the cubic equation.
 - -----------------------------------------------------------------
 - |   Input    |                                              |
 - |     a      | Double input                                 |
 - |     b      | Double input                                 |
 - |     c      | Double input                                 |
 - |     d      | Double input                                 |
 - -----------------------------------------------------------------
 - |   Output   |                                              |
 - |     R      | Double Output                                |
 - -----------------------------------------------------------------
 -}
cubicR :: Double -> Double -> Double -> Double -> Double
cubicR a b c d = ( (9*a*b*c)-(27*d*(a**2))-(2*(b**3)) ) / (54*(a**3))


 {- -----------------------------------------------------------------
 - cubicDisc
 - -----------------------------------------------------------------
 - Description:
 -   This function calculates the discriminant of a cubic equation using the values calculated above (Q and R)
 - -----------------------------------------------------------------
 - |   Input    |                                              |
 - |     Q      | Double input                                 |
 - |     R      | Double input                                 |
 - -----------------------------------------------------------------
 - |   Output   |                                              |
 - |     Disc   | Double Output                                |
 - -----------------------------------------------------------------
 -}
cubicDisc :: Double -> Double -> Double
cubicDisc q r = (q**3) + (r**2)





{- -----------------------------------------------------------------
 - cubicRoot
 - -----------------------------------------------------------------
 - Description:
 -   Finds the cube root of a number using the *** function used above
 - -----------------------------------------------------------------
 - |   Input    |                                              |
 - |     X      | Double input                                 |
 - -----------------------------------------------------------------
 - |   Output   |                                              |
 - |     root   | Double Output                                |
 - -----------------------------------------------------------------
 -}
cubeRoot :: Double -> Double
cubeRoot x = x *** (1/3)



{- -----------------------------------------------------------------
 - cubicS
 - -----------------------------------------------------------------
 - Description:
 -   This function calculates the S value for solving a cubic equation using the Q and R values found above
 - -----------------------------------------------------------------
 - |   Input    |                                              |
 - |     Q      | Double input                                 |
 - |     R      | Double input                                 |
 - -----------------------------------------------------------------
 - |   Output   |                                              |
 - |     S      | Double Output                                |
 - -----------------------------------------------------------------
 -}
cubicS :: Double -> Double -> Double
cubicS q r = cubeRoot (r+(sqrt(cubicDisc q r)))

{- -----------------------------------------------------------------
 - cubicT
 - -----------------------------------------------------------------
 - Description:
 -   This function calculates the T value for solving a cubic equation using the Q and R values found above
 - -----------------------------------------------------------------
 - |   Input    |                                              |
 - |     Q      | Double input                                 |
 - |     R      | Double input                                 |
 - -----------------------------------------------------------------
 - |   Output   |                                              |
 - |     T      | Double Output                                |
 - -----------------------------------------------------------------
 -}
cubicT :: Double -> Double -> Double
cubicT q r = cubeRoot (r-(sqrt(cubicDisc q r))) 


{- -----------------------------------------------------------------
 - cubicRealSolutions
 - -----------------------------------------------------------------
 - Description:
 -   Using all the functions and values found above, this function determines the root of a cubic equation
 - -----------------------------------------------------------------
 - |   Input    |                                              |
 - |     a      | Double input                                 |
 - |     b      | Double input                                 |
 - |     c      | Double input                                 |
 - |     d      | Double input                                 |
 - -----------------------------------------------------------------
 - |   Output   |                                              |
 - |     []     | List Output                                  |
 - -----------------------------------------------------------------
 -}
cubicRealSolutions :: Double -> Double -> Double -> Double -> [Double]
cubicRealSolutions a b c d
  | isNaN disc  = []
  | disc === 0  = [x1,x2,x2]
  | disc > 0    = [x1]
  | otherwise   = []
  where
    disc = cubicDisc q r
    s    = cubicS q r
    t    = cubicT q r
    q    = cubicQ a b c
    r    = cubicR a b c d

    x1   = s + t - (b/(3*a))

    --x2 is the same as x3
    x2   = -((s+t)/2) - (b/(3*a))


(===) :: Double -> Double -> Bool
x === y = let
  tol = 1e-3
  in abs (x-y) <= tol

{- -----------------------------------------------------------------
 - Test Cases
 - -----------------------------------------------------------------
 -}

-- TODO Add Test Cases for each of your functions below here
