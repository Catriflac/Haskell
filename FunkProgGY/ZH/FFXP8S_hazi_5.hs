sort' :: Ord t => [t] -> [t]
sort' [] = []  
sort' (x:xs) =   
    let smallerSorted = sort' [a | a <- xs, a <= x]  
        biggerSorted = sort' [a | a <- xs, a > x]  
    in  smallerSorted ++ [x] ++ biggerSorted 

mergeSort :: Ord t => [t] -> [t]
mergeSort [] = []  
mergeSort (x:xs) =   
    let smallerSorted = mergeSort [a | a <- xs, a <= x]  
        biggerSorted = mergeSort [a | a <- xs, a > x]  
    in  smallerSorted ++ [x] ++ biggerSorted 
