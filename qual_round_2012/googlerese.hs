--http://code.google.com/codejam/contest/1460488/dashboard#s=p0
--Problem A. Speaking in Tongues

import Data.List
import System.IO

examples = nub "y qee ejp mysljylc kd kxveddknmc re jsicpdrysi \
    \rbcpc ypc rtcsra dkh wyfrepkym veddknkmkrkcd de kr kd eoya \
    \kw aej tysr re ujdr lkgc jv"
results = nub "a zoo our language is impossible to understand \
    \there are twenty six factorial possibilities so it is okay \
    \if you want to just give up"

missing = [(x, y) 
    | x <- ['a'..'z'], y <- ['a'..'z'],
    (x `elem` examples) == False,
    (y `elem` results) == False]

--only (z, q) mapping is missing

mapping = sort (zip examples results ++ missing)

main = do  
    handle_in <- openFile "A-small.in" ReadMode
    handle_out <- openFile "A-small.out" WriteMode 
    contents <- hGetContents handle_in
    let input_lines = zip (tail (lines contents)) [1..]
        output_lines = [([y
                        | z <- line, (x, y) <- mapping, z == x], i)
                        | (line, i) <- input_lines]
        result_lines = ["Case #" ++ show x ++ ": " ++ line
                        | (line, x) <- output_lines]
    mapM (hPutStrLn handle_out) result_lines
    hClose handle_in
    hClose handle_out

