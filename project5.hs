import Prelude



isLeapYr :: (Integral a,Eq a)=> a -> Bool
isLeapYr yr = if ((yr `mod` 4==0) && (yr `mod` 100 /=0)) || (yr `mod` 400==0)  then True else False

getLeapYear :: (Integral a) => a -> a
getLeapYear yrs = (yrs `div` 4) - (yrs `div` 100) + (yrs `div` 400)

getCommYear :: (Integral a) => a -> a -> a
getCommYear yrs leapYrs = yrs - leapYrs 

getYearDay :: (Integral a) => a ->a -> a
getYearDay  commYrs leapYrs = (commYrs*1 + leapYrs*2) `mod` 7

tbl = [0, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

getSum :: (Eq a,Integral a) => a -> [a] -> a
getSum 0 (x:xs) = x
getSum n (x:xs) = x + getSum (n-1) xs



main  = do
    year <- getLine
    month <- getLine
    let yr = read year::Int
    let mt = read month::Int
    let yrs = yr - 1;
    let leapYrs = getLeapYear yrs 
    let commYrs = getCommYear yrs leapYrs
    let yrDay = getYearDay commYrs leapYrs
    let mtDay = (yrDay+((getSum (mt-1) tbl) + (\x->if x then 1 else 0)(isLeapYr yr))) `mod` 7
    putStrLn (show yrDay)
    putStrLn (show mtDay)
  

