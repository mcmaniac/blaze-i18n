{-# LANGUAGE OverloadedStrings #-}

-- for PrintfType instance
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS -fno-warn-orphans #-}

module Text.Blaze.I18n
  ( i18n, localizeMarkup
  ) where

import Data.Foldable as F
import Data.Text (Text)

-- blaze package
import Text.Blaze
import Text.Blaze.Internal hiding (null)

-- i18n package
import Text.I18n
import Text.I18n.Printf

-- | Create a HTML tag marked for localization
i18n
  :: PrintfType r
  => String   -- ^ Message ID
  -> r
i18n msgid = spr msgid []

instance PrintfType (MarkupM ()) where
  spr msgid args = customParent "blaze-html-i18n-tag" $ do
    customParent "msg-id" $ string msgid
    forM_ args $ \arg ->
      case arg of
        UChar c        -> customParent "char" (string [c])
        UString s      -> customParent "string" (string s)
        UInteger i1 i2 -> customParent "integer" $ do string (show i1); string (show i2)
        UFloat f       -> customParent "float" (string (show f))
        UDouble d      -> customParent "double" (string (show d))

------------------------------------------------------------------------------
-- MarkupM modification

isCustomParent :: Text -> MarkupM a -> Bool
isCustomParent t (CustomParent (Static (StaticString _ _ c)) _) = c == t
isCustomParent _ _ = False

isParent, isMsgId :: MarkupM a -> Bool

isParent = isCustomParent "blaze-html-i18n-tag"
isMsgId  = isCustomParent "msg-id"

isChar, isString, isInteger, isFloat, isDouble :: MarkupM a -> Bool

isChar    = isCustomParent "char"
isString  = isCustomParent "string"
isInteger = isCustomParent "integer"
isFloat   = isCustomParent "float"
isDouble  = isCustomParent "double"

localizeParent :: L10n -> Locale -> MarkupM a -> MarkupM a
localizeParent ln lc m
  | isParent m
  , CustomParent _ (Append c1 c2) <- m
  , isMsgId c1
  , CustomParent _ (Content (String msgid)) <- c1
  = Content . String $ localize ln lc $ gettext' msgid (uprintfsOf c2)
localizeParent _ _ m = m

uprintfsOf :: MarkupM a -> [UPrintf]
uprintfsOf m = case m of
  CustomParent _ c
    | Just u <- toUPrintf m -> [u]
    | otherwise             -> uprintfsOf c
  Content (String s)        -> [UString s]
  Append c1 c2              -> uprintfsOf c1 ++ uprintfsOf c2
  Parent _ _ _ c            -> uprintfsOf c
  AddAttribute _ _ _ c      -> uprintfsOf c
  AddCustomAttribute _ _ c  -> uprintfsOf c
  _                         -> []

toUPrintf :: MarkupM a -> Maybe UPrintf
toUPrintf m@(CustomParent _ c)

  | isChar m
  , Content (String [ch]) <- c
  = Just $ UChar ch

  | isString m
  , Content (String s) <- c
  = Just $ UString s

  | isInteger m
  , Append (Content (String i1)) (Content (String i2)) <- c
  = Just $ UInteger (read i1) (read i2)

  | isFloat m
  , Content (String f) <- c
  = Just $ UFloat (read f)

  | isDouble m
  , Content (String d) <- c
  = Just $ UDouble (read d)

toUPrintf _ = Nothing

localizeMarkup :: L10n -> Locale -> Markup -> Markup
localizeMarkup ln lc mu = go mu
 where
  go :: MarkupM a -> MarkupM a
  go m
    | isParent m = localizeParent ln lc m
    | otherwise = case m of
      Append             a b      -> Append (go a) (go b)
      Parent             a b c d  -> Parent a b c (go d)
      CustomParent       a b      -> CustomParent a (go b)
      AddAttribute       a b c d  -> AddAttribute a b c (go d)
      AddCustomAttribute a b c    -> AddCustomAttribute a b (go c)
      _                           -> m
