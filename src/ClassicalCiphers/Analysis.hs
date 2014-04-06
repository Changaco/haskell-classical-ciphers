module ClassicalCiphers.Analysis where

import BasicPrelude

import qualified Data.ByteString.Char8 as C
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV


type Freq = Vector Double


freqEnglish :: Freq
freqEnglish = V.fromList
    [ 8.167, 1.492, 2.782, 4.253, 12.702, 2.228, 2.015, 6.094, 6.966, 0.153
    , 0.772, 4.025, 2.406, 6.749, 7.507, 1.929, 0.095, 5.987, 6.327, 9.056
    , 2.758, 0.978, 2.360, 0.150, 1.974, 0.074
    ]


compareFreqs :: Freq -> Freq -> Double
compareFreqs f1 f2 = foldr f 0 $ zip (V.toList f1) (V.toList f2)
    where f (a, b) acc = acc + abs (a - b)


computeFreq :: ByteString -> Freq
computeFreq bs = runST $ do
    mv <- MV.new 26
    MV.set mv 0
    forM_ (filterNonAlpha $ C.unpack bs) $ \c -> do
        let i = ord c - 65
        count <- MV.read mv i
        MV.write mv i (count+1)
    v <- freeze mv
    return $ V.map (\x -> x / (fromIntegral $ C.length bs) * 100) v
