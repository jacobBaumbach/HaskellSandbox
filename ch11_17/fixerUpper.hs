module FixerUpper where

import Control.Applicative

const <$> Just "Hello" <*> (pure "World")

(,,,) <$> Just 90 <*> Just 10 <*> Just "Tierness" <*> (pure [1,2,3])