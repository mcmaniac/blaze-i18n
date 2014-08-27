{-# LANGUAGE OverloadedStrings #-}

-- for PrintfType instance
{-# LANGUAGE FlexibleInstances #-}

module Text.Blaze.I18n
  ( i18n, I18nType
  , i18nContext
  , localizeMarkup
  ) where

import GHC.Prim
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
  :: I18nType r
  => String   -- ^ Message ID
  -> r
i18n msgid = spr_i18n msgid []

i18nContext :: String -> Markup -> Markup
i18nContext ctxt =
  customParent "blaze-html-i18n-context" ! customAttribute "value" (toValue ctxt)

--------------------------------------------------------------------------------
-- "printf" type for i18n

class I18nType a where
  spr_i18n :: String -> [UPrintf] -> a

instance (PrintfArg a, I18nType r) => I18nType (a -> r) where
  spr_i18n fmt args = \a -> spr_i18n fmt (args ++ [toUPrintf a])

instance I18nType (MarkupM a) where
  spr_i18n msgid args = coerce $ customParent "blaze-html-i18n-tag" $ do
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

isParent, isContext, isMsgId :: MarkupM a -> Bool

isParent  = isCustomParent "blaze-html-i18n-tag"
isContext = isCustomParent "blaze-html-i18n-context"
isMsgId   = isCustomParent "msg-id"

isChar, isString, isInteger, isFloat, isDouble :: MarkupM a -> Bool

isChar    = isCustomParent "char"
isString  = isCustomParent "string"
isInteger = isCustomParent "integer"
isFloat   = isCustomParent "float"
isDouble  = isCustomParent "double"

localizeParent :: L10n -> Locale -> Maybe Context -> MarkupM a -> MarkupM a
localizeParent ln lc ctxt m

  | isParent m
  , CustomParent _ (Append c1 c2) <- m
  , isMsgId c1
  , CustomParent _ (Content (String msgid)) <- c1
  = Content . String $ localize ln lc $
    withContext ctxt $ gettext' msgid (uprintfsOf c2)

localizeParent _ _ _ m = m

uprintfsOf :: MarkupM a -> [UPrintf]
uprintfsOf m = case m of
  CustomParent _ c
    | Just u <- tagToUPrintf m  -> [u]
    | otherwise                 -> uprintfsOf c
  Content (String s)            -> [UString s]
  Append c1 c2                  -> uprintfsOf c1 ++ uprintfsOf c2
  Parent _ _ _ c                -> uprintfsOf c
  AddAttribute _ _ _ c          -> uprintfsOf c
  AddCustomAttribute _ _ c      -> uprintfsOf c
  _                             -> []

tagToUPrintf :: MarkupM a -> Maybe UPrintf
tagToUPrintf m@(CustomParent _ c)

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

tagToUPrintf _ = Nothing

localizeMarkup :: L10n -> Locale -> Markup -> Markup
localizeMarkup ln lc mu = go Nothing mu
 where
  go :: Maybe Context -> MarkupM a -> MarkupM a
  go ctxt m

    | isParent m = localizeParent ln lc ctxt m

    | AddCustomAttribute (Static (StaticString _ _ _)) (String val) c <- m
    , isContext c
    , CustomParent _ m' <- c
    = go (Just val) (mv m')

    | otherwise = case m of
      Append             a b      -> Append (go ctxt a) (go ctxt b)
      Parent             a b c d  -> Parent a b c (go ctxt d)
      CustomParent       a b      -> CustomParent a (go ctxt b)
      AddAttribute       a b c d  -> AddAttribute a b c (go ctxt d)
      AddCustomAttribute a b c    -> AddCustomAttribute a b (go ctxt c)
      _                           -> m

  -- just move the types around
  mv :: MarkupM a -> MarkupM b
  mv m = case m of
    Append a b                -> Append a b
    Parent a b c d            -> Parent a b c d
    CustomParent a b          -> CustomParent a b
    Leaf a b c                -> Leaf a b c
    CustomLeaf a b            -> CustomLeaf a b
    Empty                     -> Empty
    Content a                 -> Content a
    AddAttribute a b c d      -> AddAttribute a b c (mv d)
    AddCustomAttribute a b c  -> AddCustomAttribute a b (mv c)
