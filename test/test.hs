import qualified Binaryen

main :: IO ()
main = print =<< Binaryen.getOptimizeLevel
