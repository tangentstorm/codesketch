import Test.Hspec
import CodeSketch.Types
import CodeSketch.Json
import Data.Aeson (encode, decode)
import qualified Data.ByteString.Lazy.Char8 as BL
import System.Directory (getCurrentDirectory)

main :: IO ()
main = hspec $ do
  describe "CodeSketch.Types" $ do
    it "creates definitions correctly" $ do
      let def = Definition "test_function" Function Public
      iden def `shouldBe` "test_function"
      defType def `shouldBe` Function
      visibility def `shouldBe` Public
    
    it "creates path info correctly" $ do
      let def = Definition "test_function" Function Public
          pathInfo = PathInfo "/path/to/test.rs" [def]
      path pathInfo `shouldBe` "/path/to/test.rs"
      defs pathInfo `shouldBe` [def]
  
  describe "CodeSketch.Json" $ do
    it "serializes definitions to JSON correctly" $ do
      let def = Definition "test_function" Function Public
          json = definitionToJSON def
          encoded = BL.unpack $ encode json
      encoded `shouldContain` "\"iden\":\"test_function\""
      encoded `shouldContain` "\"type\":\"fn\""
      encoded `shouldContain` "\"vis\":\"*\""
    
    it "serializes path info to JSON correctly" $ do
      let def = Definition "test_function" Function Public
          pathInfo = PathInfo "/path/to/test.rs" [def]
          json = pathInfoToJSON pathInfo
          encoded = BL.unpack $ encode json
      encoded `shouldContain` "\"path\":\"/path/to/test.rs\""
      encoded `shouldContain` "\"defs\":"
      
    it "serializes root to JSON correctly" $ do
      let def = Definition "test_function" Function Public
          pathInfo = PathInfo "/path/to/test.rs" [def]
          root = [pathInfo]
          encoded = rootToJSONString root
      encoded `shouldContain` "\"path\":\"/path/to/test.rs\""
      encoded `shouldContain` "\"defs\":"