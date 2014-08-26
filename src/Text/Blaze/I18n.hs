{-# LANGUAGE OverloadedStrings #-}

module Text.Blaze.I18n where

import Data.List
import Data.Foldable

-- blaze package
import Text.Blaze
import Text.Blaze.Internal
import Text.Blaze.Renderer.String

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

msgId :: MarkupM a -> Maybe String
msgId m@(CustomParent _ (Content (String s))) = Just s
msgId _ = Nothing

localizeParent :: L10n -> Locale -> MarkupM a -> MarkupM a
localizeParent l10 loc m
  | isParent m
  , CustomParent _ (Append c1 c2) <- m
  , isMsgId c1
  , CustomParent _ (Content (String s)) <- c1
  = Content . String $ s ++ "(" ++ intercalate ", " (stringsOf c2) ++ ")"
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
localizeMarkup _ _ _ = undefined

l10n :: L10n
l10n = undefined

loc :: Locale
loc = Locale "de"

























-- | Render some 'Markup' to an appending 'String'.
renderString' :: MarkupM a  -- ^ Markup to render
              -> String     -- ^ String to append
              -> String     -- ^ Resulting String
renderString' = go 0 id
  where
    go :: Int -> (String -> String) -> MarkupM b -> String -> String
    go i attrs (Parent _ open close content) =
        ind i . getString open . attrs . (">\n" ++) . go (inc i) id content
              . ind i . getString close .  ('\n' :)
    go i attrs (CustomParent tag content) =
        ind i . ('<' :) . fromChoiceString tag . attrs . (">\n" ++) .
        go (inc i) id content . ind i . ("</" ++) . fromChoiceString tag .
        (">\n" ++)
    go i attrs (Leaf _ begin end) =
        ind i . getString begin . attrs . getString end . ('\n' :)
    go i attrs (CustomLeaf tag close) =
        ind i . ('<' :) . fromChoiceString tag . attrs .
        ((if close then " />\n" else ">\n") ++)
    go i attrs (AddAttribute _ key value h) = flip (go i) h $
        getString key . fromChoiceString value . ('"' :) . attrs
    go i attrs (AddCustomAttribute key value h) = flip (go i) h $
        (' ' : ) . fromChoiceString key . ("=\"" ++) . fromChoiceString value .
        ('"' :) .  attrs
    go i _ (Content content) = ind i . fromChoiceString content . ('\n' :)
    go i attrs (Append h1 h2) = go i attrs h1 . go i attrs h2
    go _ _ Empty = id
    {-# NOINLINE go #-}

    -- Increase the indentation
    inc = (+) 4

    -- Produce appending indentation
    ind i = (replicate i ' ' ++)
{-# INLINE renderString' #-}
