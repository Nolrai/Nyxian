{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE InstanceSigs #-}
module Nyxian.Types where
import Data.List
import Data.Word
import Data.Maybe
import Data.Ord
import Data.Eq
import Prelude (Show, Enum, Bounded, (+), (.))
import GHC.Generics (Generic)
import Nyxian.Util
data Voicing = Voiced | Unvoiced
    deriving (Show, Eq, Ord, Enum, Bounded, Generic)
data Place = Bilabial | Labiodental | Dental | Alveolar | Postalveolar | Velar
    deriving (Show, Eq, Ord, Enum, Bounded, Generic)
data Liquid = ClearL | DarkL | Rhotic
    deriving (Show, Eq, Ord, Enum, Bounded, Generic)
data Plosive = Plosive Place Voicing
    deriving (Show, Eq, Ord, Bounded, Generic)
data Fricative = Fricative Place Voicing
    deriving (Show, Eq, Ord, Bounded, Generic)
data MidSlot = MidPlosive Plosive | MidArchNasal
    deriving (Show, Eq, Ord, Generic)

data OnsetLike = OnsetLike
  { olFricative :: Maybe Fricative
  , olMidSlot :: Maybe MidSlot
  , olLiquid :: Maybe Liquid
  } deriving (Show, Eq, Ord, Generic)

data Onset = OnsetOnsetLike OnsetLike | OnsetNasal Place
    deriving (Show, Eq, Generic)

data ComplexLiquid = Single Liquid | Double Liquid Liquid
    deriving (Show, Eq, Generic)

data IsNasal = IsNasal | NotNasal
    deriving (Show, Eq, Enum, Bounded, Generic)

data Coda = Coda
  { cStop   :: Maybe Plosive
  , cNasal  :: IsNasal
  , cLiquids :: Maybe ComplexLiquid
  , cFric   :: Maybe Fricative
  } deriving (Show, Eq, Generic)

data Laryngeal = GlottalStop | GlottalFricative
    deriving (Show, Eq, Enum, Bounded, Generic)

data CodaLikeCore = CLCPlosive MidSlot | CLCLiquid Liquid
    deriving (Show, Eq, Generic)

data CodaLike = CodaLike CodaLikeCore (Maybe Fricative)
    deriving (Show, Eq, Generic)

data Intervocalic = IvLaryngeal Laryngeal | IvOnset OnsetLike | IvCoda CodaLike
    deriving (Show, Eq, Generic)

data Stress = Primary | Secondary | Unstressed
  deriving (Show, Eq, Enum, Bounded, Generic)

data VowelCore = Front | Back
  deriving (Show, Eq, Enum, Bounded, Generic)

data Heading = Left | Right
  deriving (Show, Eq, Enum, Bounded, Generic)

data Diph = Diph { heading :: Heading, core :: VowelCore, glide :: VowelCore }
  deriving (Show, Eq, Bounded, Generic)

data MonoStrength = Tense | Centered
  deriving (Show, Eq, Enum, Bounded, Generic)

data Vowel
  = VPrimary Diph           -- always diphthong
  | VSecondaryDiph Diph     -- sometimes diphthong
  | VSecondaryMono MonoStrength VowelCore     -- sometimes uncentered
  | VUnstressed (Maybe VowelCore)   -- never diphthong, always centered
  deriving (Show, Eq, Generic)

data PhonWord = PhonWord
  { tOnset :: Maybe Onset
  , tVowel :: Vowel
  , tIntervocalic :: [(Intervocalic, Vowel)]
  , tCoda :: Maybe Coda
  } deriving (Show, Eq, Generic)

type Time = Word
class Timeable a where
  toTime :: a -> Time
instance Timeable a => Timeable (Maybe a) where
  toTime :: Timeable a => Maybe a -> Time
  toTime Nothing = 0
  toTime (Just x) = toTime x
instance Timeable a => Timeable [a] where
  toTime :: Timeable a => [a] -> Time
  toTime = sum . map toTime
instance (Timeable a, Timeable b) => Timeable (a, b) where
  toTime :: (Timeable a, Timeable b) => (a, b) -> Time
  toTime (a, b) = toTime a + toTime b
instance Timeable PhonWord where
  toTime :: PhonWord -> Time
  toTime PhonWord{..} =
    toTime tOnset + toTime tVowel + toTime tIntervocalic + toTime tCoda
instance Timeable Onset where
  toTime :: Onset -> Time
  toTime (OnsetOnsetLike ol) = toTime ol
  toTime (OnsetNasal _) = 2
instance Timeable OnsetLike where
  toTime :: OnsetLike -> Time
  toTime OnsetLike{..} =
    toTime olFricative + toTime olMidSlot + toTime olLiquid
instance Timeable Fricative where
  toTime :: Fricative -> Time
  toTime _ = 2
instance Timeable MidSlot where
  toTime :: MidSlot -> Time
  toTime (MidPlosive p) = toTime p
  toTime MidArchNasal = 1
instance Timeable Plosive where
  toTime :: Plosive -> Time
  toTime _ = 2
instance Timeable Liquid where
  toTime :: Liquid -> Time
  toTime _ = 2
instance Timeable Coda where
  toTime :: Coda -> Time
  toTime Coda{..} = toTime cStop + toTime cNasal + toTime cLiquids + toTime cFric
instance Timeable IsNasal where
  toTime :: IsNasal -> Time
  toTime IsNasal = 1
  toTime NotNasal = 0
instance Timeable ComplexLiquid where
  toTime :: ComplexLiquid -> Time
  toTime (Single _) = 2
  toTime (Double _ _) = 4
instance Timeable CodaLike where
  toTime (CodaLike core mFric) = toTime core + toTime mFric
instance Timeable CodaLikeCore where
  toTime (CLCPlosive p) = toTime p
  toTime (CLCLiquid l) = toTime l
instance Timeable Intervocalic where
  toTime (IvLaryngeal l) = 0
  toTime (IvOnset ol) = toTime ol
  toTime (IvCoda cl) = toTime cl
instance Timeable Laryngeal where
  toTime _ = 0
instance Timeable Vowel where
  toTime (VPrimary _) = 3
  toTime (VSecondaryDiph _) = 3
  toTime (VSecondaryMono Tense _) = 2
  toTime (VSecondaryMono Centered _) = 2
  toTime (VUnstressed _) = 1
