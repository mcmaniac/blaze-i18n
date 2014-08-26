{-# LANGUAGE OverloadedStrings #-}

module Text.Blaze.I18n
  ( i18n, localizeMarkup
  ) where

import Data.Foldable as F

-- blaze package
import Text.Blaze
import Text.Blaze.Internal hiding (null)

-- i18n package
import Text.I18n

-- | Create a HTML tag marked for localization
i18n
  :: String   -- ^ Message ID
  -> [String] -- ^ printf arguments
  -> Markup
i18n msgid args = customParent "blaze-html-i18n-tag" $ do
  customParent "msg-id" $ string msgid
  forM_ args $ string

isParent :: MarkupM a -> Bool
isParent (CustomParent (Static (StaticString _ _ c)) _) = c == "blaze-html-i18n-tag"
isParent _ = False

isMsgId :: MarkupM a -> Bool
isMsgId (CustomParent (Static (StaticString _ _ c)) _) = c == "msg-id"
isMsgId _ = False

localizeParent :: L10n -> Locale -> MarkupM a -> MarkupM a
localizeParent ln lc m
  | isParent m
  , CustomParent _ (Append c1 c2) <- m
  , isMsgId c1
  , CustomParent _ (Content (String msgid)) <- c1
  = Content . String $ localize ln lc $ gettext' msgid (stringsOf c2)
localizeParent _ _ m = m

stringsOf :: MarkupM a -> [String]
stringsOf m = case m of
  Append c1 c2              -> stringsOf c1 ++ stringsOf c2
  Content (String s)        -> [s]
  Parent _ _ _ c            -> stringsOf c
  CustomParent _ c          -> stringsOf c
  AddAttribute _ _ _ c      -> stringsOf c
  AddCustomAttribute _ _ c  -> stringsOf c
  _                         -> []

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
