module Config where

windowSize = (600, 600) :: (Int, Int)

boardData = [ "##############"
            , "#     #  ##  #"
            , "#   #    #   #"
            , "#A######E### #"
            , "#  #         #"
            , "#     #  ##  #"
            , "# ######   # #"
            , "# #   #  ##  #"
            , "#   T    #   #"
            , "##############"
            ]

blockSize   = 32 :: Float

boardHeight = length boardData
boardWidth  = length $ boardData !! 0

boardOffsetVertical   = - blockSize / 2.0 * (fromIntegral boardHeight)
boardOffsetHorizontal = - blockSize / 2.0 * (fromIntegral boardWidth)


