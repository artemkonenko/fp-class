module TestTaylor (tests) where

import Distribution.TestSuite
import Taylor

eps :: Double
eps = 1e-4

sin'' :: Double -> Double
sin'' x = fst $ sin' x

cos'' :: Double -> Double
cos'' x = fst $ cos' x

tests :: IO [Test]
tests = return [Test sin_0, Test sin_pi4, Test sin_pi2, Test sin_pi
              , Test cos_0, Test cos_pi4, Test cos_pi2, Test cos_pi]
    where
        sin_0 = TestInstance
            { name = "sin 0"
            , run = return $ Finished $ if (abs( sin 0 - sin'' 0) < eps) then Pass else Fail "sin 0 "
            , tags = []
            , options = []
            , setOption = \_ _ -> Right sin_0
            }
        sin_pi4 = TestInstance
            { name = "sin pi/4" 
            , run = return $ Finished $ if (abs( sin pi/4 - sin'' pi/4) < eps) then Pass else Fail "sin pi/4 "
            , tags = []
            , options = []
            , setOption = \_ _ -> Right sin_pi4
            }
        sin_pi2 = TestInstance
            { name = "sin pi/2"
            , run = return $ Finished $ if (abs( sin pi/2 - sin'' pi/2) < eps) then Pass else Fail "sin pi/2 "
            , tags = []
            , options = []
            , setOption = \_ _ -> Right sin_pi2
            }
        sin_pi = TestInstance
            { name = "sin pi"
            , run = return $ Finished $ if (abs( sin pi - sin'' pi) < eps) then Pass else Fail "sin pi "
            , tags = []
            , options = []
            , setOption = \_ _ -> Right sin_pi
            }
        cos_0 = TestInstance
            { name = "cos 0"
            , run = return $ Finished $ if (abs( cos 0 - cos'' 0) < eps) then Pass else Fail "cos 0 "
            , tags = []
            , options = []
            , setOption = \_ _ -> Right cos_0
            }
        cos_pi4 = TestInstance
            { name = "cos pi/4" 
            , run = return $ Finished $ if (abs( cos pi/4 - cos'' pi/4) < eps) then Pass else Fail "cos pi/4 "
            , tags = []
            , options = []
            , setOption = \_ _ -> Right cos_pi4
            }
        cos_pi2 = TestInstance
            { name = "cos pi/2"
            , run = return $ Finished $ if (abs( cos pi/2 - cos'' pi/2) < eps) then Pass else Fail "cos pi/2 "
            , tags = []
            , options = []
            , setOption = \_ _ -> Right cos_pi2
            }
        cos_pi = TestInstance
            { name = "cos pi"
            , run = return $ Finished $ if (abs( cos pi - cos'' pi) < eps) then Pass else Fail "cos pi "
            , tags = []
            , options = []
            , setOption = \_ _ -> Right cos_pi
            }
