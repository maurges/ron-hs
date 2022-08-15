module Escape (benchmark) where

import Criterion.Main (bgroup, bench, nf, Benchmark)
import Data.Ron.Deserialize (loads)
import Data.Text.Encoding (encodeUtf16BE)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as Text

benchmark = bgroup "Escape"
    [ example "ascii" $ BS8.pack . take 500 $ cycle ['a'..'z']
    , example "cyrillic" $ BS8.unwords
      [ "Стандарт состоит из двух основных частей: универсального набора "
      , "символов (англ. Universal character set, UCS) и семейства кодировок"
      , "(англ. Unicode transformation format, UTF). Универсальный набор"
      , "символов перечисляет допустимые по стандарту Юникод символы и"
      , "присваивает каждому символу код в виде неотрицательного целого"
      , "числа, записываемого обычно в шестнадцатеричной форме с префиксом"
      , "U+, например, U+040F. Семейство кодировок определяет способы"
      , "преобразования кодов символов для передачи в потоке или в файле."
      ]
    , example "hexEscapes" $
        let chars = ['0'..'9'] ++ ['a'..'z'] ++ ['A'..'Z'] ++ ['\x10000' .. '\x100ff']
            formatChar :: Char -> Builder.Builder
            formatChar c = case BS.length bs of
                4 -> "\\u" <> Builder.byteString bs
                8 -> "\\u" <> Builder.byteString (BS.take 4 bs) <> "\\u" <> Builder.byteString (BS.drop 4 bs)
                _ -> error "formatChar: ???"
              where
                bs = Base16.encode $ encodeUtf16BE $ Text.pack [c]

        in BSL.toStrict $ Builder.toLazyByteString $ foldMap formatChar chars
    , example "charEscapes" $
        BS.concat $ replicate 10000 $ BS8.pack "\\\""
    ]
  where
    example :: String -> BS.ByteString -> Benchmark
    example name input =
        let input' = "r##\"" <> input <> "\"##"
        in bench name $ nf loads input

