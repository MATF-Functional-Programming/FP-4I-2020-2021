module Main where

main :: IO ()
main = main1

main0 = getLine >>= putStrLn

main0' = getLine >>= (\line -> putStrLn line)

main0do = do { line <- getLine
             ; putStrLn line
             }

main1 = putStr "Hello" >> putStr " " >> putStr "World" >> putStrLn ""

main1do = do { putStr "Hello"
             ; putStr " "
             ; putStr "World"
             ; putStrLn ""
             }

main2 = getLine >>= putStrLn >> putStrLn "End"


input message = putStrLn message >> getLine

main3 = input "Enter question" >>= \x ->
            input x >>= \y ->
                putStrLn y


main3' = do { x <- input "Enter question: "
            ; y <- input x
            ; putStrLn y
            }

main3'' = do { x <- input "Enter question: "
             ; do { y <- input x
                  ; putStrLn y
                  }
             }

main3''' = do { y <- do { x <- input "Enter question: "
                        ; input x
                        }
              ; putStrLn y
              }





