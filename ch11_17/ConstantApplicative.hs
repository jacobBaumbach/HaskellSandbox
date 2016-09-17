module ConstantApplicative where
import Control.Applicative

newtype Constant a b = Constant {getConstant :: a} deriving (Eq, Show)

instance Functor (Constant a) where
	fmap f (Constant b) = Constant b

instance (Monoid a) => Applicative (Constant a) where
	pure a = Constant (mempty a)
	(<*>) (Constant f) (Constant a) = Constant (mappend f a)