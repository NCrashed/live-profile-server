module Profile.Live.Server.Application.Upload.ModelSpec(
    spec 
  ) where 

import Profile.Live.Server.Application.Upload.Model
import Test.Hspec
import Test.QuickCheck

spec :: Spec 
spec = describe "Application.Upload.Model" $ do 
  describe "parseChunkFileName and encodeChunkFileName" $ do 
    it "can encode common chunk name" $ do 
      encodeChunkFileName 42 5 `shouldBe` "42_5.chunk"

    it "can decode common chunk name" $ do 
      parseChunkFileName "1_54.chunk" `shouldBe` Just (1, 54)

    it "can decode what encodes" $ 
      property $ \i n -> let
        encoded = encodeChunkFileName i n 
        decoded = parseChunkFileName encoded 
        in decoded == Just (i, n)
