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

getDaysOfMonth :: Int-> Int -> Int
getDaysOfMonth yr 2  = (tbl !! 2) + (\x->if x then 1 else 0)(isLeapYr yr)
getDaysOfMonth yr mt = (tbl !! mt)

getMonthTbl :: Int -> Int -> [Int]
getMonthTbl day days= tail ([0..day] ++ [1..days])

printMonth :: [Int] -> [Char]
printMonth monthTbl = [ x | a<-[0..(length monthTbl - 1)],x<-if(a `mod` 7 == 6) then show (monthTbl !! a) ++ "\n"  else show (monthTbl !! a) ++ "\t" ]


main  = do
    putStrLn "Year"
    year <- getLine
    putStrLn "Month"
    month <- getLine
    let yr = read year::Int
    let mt = read month::Int
    let yrs = yr - 1;
    let leapYrs = getLeapYear yrs 
    let commYrs = getCommYear yrs leapYrs
    let yrDay = getYearDay commYrs leapYrs
    let mtDay = (yrDay+((getSum (mt-1) tbl) + (\x->if x && (mt > 2) then 1 else 0)(isLeapYr yr))) `mod` 7
    let mtDays = getDaysOfMonth yr mt
    --putStrLn (show yrDay)
    --putStrLn (show mtDay)
    --putStrLn (show mtDays)
    putStrLn "MON\tTUE\tWEN\tTHR\tFRI\tSAT\tSUN"
    let mtTbl = getMonthTbl mtDay mtDays
    --putStrLn (show mtTbl)
    let strMt = printMonth mtTbl
    putStrLn strMt