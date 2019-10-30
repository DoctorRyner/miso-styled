{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections     #-}

module Miso.Styled
    ( module Miso.Styled
    , module Clay
    ) where

import           Clay                hiding (map)
import           Data.Coerce         (coerce)
import qualified Data.HashMap.Strict as HMap
import           Data.List           (nub)
import qualified Data.Text           as T
import qualified Data.Text.Lazy      as TL
import qualified Miso
import           Miso.String         (MisoString, ms)
import qualified Miso.String         as MS
import           System.IO.Unsafe    (unsafePerformIO)
import           System.Random

instance Eq Css where
    s1 == s2 = render s1 == render s2
    s1 /= s2 = render s1 /= render s2

type View a = VTree a

newtype Attribute a = Attribute (Miso.Attribute a)

data VTree a
    = VNode MisoString (Maybe T.Text) (Maybe Css) [Attribute a] [VTree a]
    | VText MisoString

el :: MisoString -> [Miso.Attribute a] -> [View a] -> View a
el tag attrs = VNode tag Nothing Nothing (coerce attrs)

generateHtml :: HMap.HashMap TL.Text Int -> MisoString -> View a -> Miso.View a
generateHtml _ _ (VText str) = Miso.text str
generateHtml cssHash uniqId (VNode tag mbClasses (Just css) attrs childs) = Miso.nodeHtml
    tag
    (coerce attrs ++ case HMap.lookup (render css) cssHash of
        Just className ->
            [ Miso.class_ $ mconcat
                [ "_" <> uniqId <> Miso.String.ms className
                , maybe "" ((" " <>) . Miso.String.ms) mbClasses
                ]
            ]
        Nothing        -> []
    )
    $ map (generateHtml cssHash uniqId) childs
generateHtml cssHash uniqId (VNode tag _ Nothing attrs childs) = Miso.nodeHtml
    tag
    (coerce attrs)
    $ map (generateHtml cssHash uniqId) childs

collectCss :: View a -> [Css]
collectCss (VText _                    ) = mempty
collectCss (VNode _ _ (Just css) _ childs) = css : mconcat (map collectCss childs)
collectCss (VNode _ _ Nothing    _ childs) = mconcat (map collectCss childs)

{-# NOINLINE rnd #-}
rnd :: () -> Int
rnd _ = unsafePerformIO $ randomRIO (0, 2000000000)

toUnstyled :: View a -> Miso.View a
toUnstyled tree = Miso.div_ []
    [ Miso.nodeHtml "style" [] [ Miso.text $ Miso.String.ms $ mconcat $ map render renderCss ]
    , generateHtml cssHash (Miso.String.ms uniqId) tree
    ]
  where
    uniqId    = T.pack $ show $ rnd ()
    renderCss = map (\(css, id') -> element ("._" <> uniqId <> T.pack (show id')) ? css) cssKeyed
    cssHash   = HMap.fromList (map (\(css, id') -> (render css, id')) cssKeyed)
    cssKeyed  = map (, rnd ()) css
    css       = nub $ collectCss tree

toUnstyled' :: View a -> [Miso.View a]
toUnstyled' tree = [ Miso.nodeHtml "style" [] [ Miso.text $ Miso.String.ms $ mconcat $ map render renderCss ]
                   , generateHtml cssHash (Miso.String.ms uniqId) tree
                   ]
  where
    uniqId    = T.pack $ show $ rnd ()
    renderCss = map (\(css, id') -> element ("._" <> uniqId <> T.pack (show id')) ? css) cssKeyed
    cssHash   = HMap.fromList (map (\(css, id') -> (render css, id')) cssKeyed)
    cssKeyed  = map (, rnd ()) css
    css       = nub $ collectCss tree

text :: MisoString -> VTree a
text = VText

styled :: MisoString -> Css -> [Miso.Attribute a] -> [View a] -> View a
styled tag css attrs = VNode tag Nothing (Just css) (coerce attrs)

styled' :: MisoString -> Css -> MisoString -> [Miso.Attribute a] -> [View a] -> View a
styled' tag css classes attrs = VNode tag (Just $ T.pack $ MS.unpack classes) (Just css) (coerce attrs)

linkCss :: MisoString -> View a
linkCss path = el "link" [ Miso.rel_ "stylesheet", Miso.href_ path ] []
