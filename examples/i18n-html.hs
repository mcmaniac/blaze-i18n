import Text.I18n
import Text.I18n.Po hiding (putStrLn)
import Text.Blaze.I18n
import Text.Blaze.Renderer.Pretty

import qualified Blaze.Html5 as H

import System.Environment

main :: IO ()
main = do

  -- get locale
  args <- getArgs
  env  <- getEnv "LANG"
  let locale = Locale $ head (args ++ [takeWhile ('_' /=) env, "en"])

  -- load L10n from PO file
  po <- getL10n "."

  putStrLn $ case po of
    (ln, []) -> renderMarkup $ localizeMarkup ln locale html
    (_, err) -> "Errors loading po file: " ++ show err

html :: H.Html
html = H.docTypeHtml $ H.body $ do

  i18nContext "title" $ do

    H.h1 $ i18n "Hello, %s!" "Joe"

  i18nContext "body" $ do

    H.p  $ i18n "This is a test."
