import Prelude



isLeapYr :: (Integral a,Eq a)=> a -> Bool
isLeapYr yr = if ((yr `mod` 4==0) && (yr `mod` 100 /=0)) || (yr `mod` 400==0)  then True else False;

main  = do
    year <- getLine
    month <- getLine
    putStrLn "input end"


yr = 1970
mt = 7
yrs = yr-1

leapYrs = (yrs `div` 4) - (yrs `div` 100) + (yrs `div` 400)
commYrs = yrs - leapYrs

day = (commYrs*1 + leapYrs*2) `mod` 7 

