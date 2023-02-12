import Test.HUnit
import SafeSync.Database.Types.Test.Test

main :: IO ()
main = print =<< runTestTT testTypes
