data Month = Jan | Feb | Mar | Apr | May | Jun | Jul | Aug | Sep | Oct | Nov | Dec
isWinter :: Month -> Bool
isWinter x = case x of
    Nov -> True
    Dec -> True
    Jan -> True
    _ -> False

data Time = T Int Int Int
showTime :: Time -> String
showTime = undefined