{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Nyxian.Lib where

import Data.List

data Voicing = Voiced | Voiceless
    deriving (Show, Eq)

data Place = Bilabial | Labiodental | Dental | Alveolar | Postalveolar | Velar
    deriving (Show, Eq)

data Liquid = ClearL | DarkL | Rhotic
    deriving (Show, Eq)

data Plosive = Plosive Place Voicing
    deriving (Show, Eq)

data Fricative = Fricative Place Voicing
    deriving (Show, Eq)

data MidSlot = MidPlosive Plosive | MidArchNasal
    deriving (Show, Eq)

data OnsetLike = OnsetLike
  { olFricative :: Maybe Fricative
  , olMidSlot :: Maybe MidSlot
  , olLiquid :: Maybe Liquid
  } deriving (Show, Eq)

data Onset = OnsetOnsetLike OnsetLike | OnsetNasal Place
    deriving (Show, Eq)

data ComplexLiquid = Single Liquid | Double Liquid Liquid
    deriving (Show, Eq)

data IsNasal = IsNasal | NotNasal
    deriving (Show, Eq)

data Coda = Coda
  { cStop   :: Maybe Plosive
  , cNasal  :: IsNasal
  , cLiquids :: Maybe ComplexLiquid
  , cFric   :: Maybe Fricative
  } deriving (Show, Eq)

data Laryngeal = GlottalStop | GlottalFricative
    deriving (Show, Eq)

data CodaLikeCore = CLCPlosive MidLike | CLCLiquid Liquid
    deriving (Show, Eq)

data CodaLike = CodaLike CodaLikeCore (Maybe Fricative)
    deriving (Show, Eq)

data Intervocalic = IvLaryngeal Laryngeal | IvOnset OnsetLike | IvCoda CodaLike
    deriving (Show, Eq)

data Stress = Primary | Secondary | Unstressed
  deriving (Show, Eq)

data VowelCore = Front | Back
  deriving (Show, Eq)

data Heading = Left | Right
  deriving (Show, Eq)

data Diph = Diph { heading :: Heading, core :: VowelCore, glide :: VowelCore }
  deriving (Show, Eq)

data MonoStrength = Tense | Centered
  deriving (Show, Eq)

data Vowel
  = VPrimary Diph           -- always diphthong
  | VSecondaryDiph Diph     -- sometimes diphthong
  | VSecondaryMono MonoStrength VowelCore     -- sometimes uncentered
  | VUnstressed (Maybe VowelCore)   -- never diphthong, always centered
  deriving (Show, Eq)

data PhonWord = PhonWord
  { tOnset :: Maybe Onset
  , tVowel :: Vowel
  , tIntervocalic :: [(Intervocalic, Vowel)]
  , tCoda :: Maybe Coda
  } deriving (Show, Eq)
