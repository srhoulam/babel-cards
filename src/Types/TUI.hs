{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE TemplateHaskell        #-}
module Types.TUI where

import           Brick.Forms            (Form)
import           Brick.Widgets.List     (GenericList)
import           Database.Persist.Types (Entity)
import           Lens.Micro.TH
import           Model
import           RIO                    hiding (view)
import           RIO.Time               (UTCTime)
import           Types

data BabelTUI = BabelTUI
  { babelTUIBabel                  :: !Babel
  , babelTUIView                   :: !BabelView
  , babelTUIPreviousView           :: !(Maybe BabelView)

  -- Review state
  , babelTUIMatchMode              :: !(Maybe BabelMode)
  , babelTUIActiveCard             :: !(Maybe (Entity Card))
  , babelTUIUserAnswer             :: !(Maybe Text)
  , babelTUIAnswerDistance         :: !(Maybe Int)
  , babelTUIReviewStartTimestamp   :: !(Maybe UTCTime)
  , babelTUIReviewEndTimestamp     :: !(Maybe UTCTime)

  , babelTUIFocusX                 :: !Int
  -- ^ Left-to-right focus.
  -- Each view will set and interpret this as it will.
  -- Setting min/max bounds during view transitions is
  -- advised.
  , babelTUIFocusY                 :: !Int
  -- ^ Top-to-bottom focus.
  -- Each view will set and interpret this as it will.
  -- Setting min/max bounds during view transitions is
  -- advised.

  , babelTUICardMapEnabled         :: !(IntMap (Entity Card))
  , babelTUICardMapDisabled        :: !(IntMap (Entity Card))
  , babelTUIDeckMap                :: !(IntMap DeckMetadata)
  , babelTUITagMap                 :: !(IntMap (Entity Tag))

  , babelTUIAnswerForm             :: !(Form Text () String)
  , babelTUICardForm               :: !(Form CardFormState () String)
  , babelTUIDeckForm               :: !(Form Deck () String)

  -- Display lists
  , babelTUIActiveCardDecks        :: !(GenericList String Seq DeckId)
  , babelTUIActiveCardTags         :: !(GenericList String Seq TagId)

  -- Interactive lists
  , babelTUIAvailableCardsEnabled  :: !(GenericList String Seq CardId)
  , babelTUIAvailableCardsDisabled :: !(GenericList String Seq CardId)
  , babelTUIAvailableDecks         :: !(GenericList String Seq DeckId)
  , babelTUIAvailableModes         :: !(GenericList String Seq BabelMode)
  , babelTUIAvailableTags          :: !(GenericList String Seq TagId)
  , babelTUIStartOptions           :: !(GenericList String Seq (BabelView, String))

  -- Interactive state
  , babelTUICardFilter             :: !(Maybe Text)
  }


data BabelMode =
  Standard
  | Reverse
  deriving Show

data BabelView =
  Start
  | DeckSelect
  | ModeSelect
  | Playing
  | GameOver

  | AddNewCard
  | CardsOverview
  | CardsOverviewDisabled
  | CardManagement
  | SetCardFilter

  | AddNewDeck
  | DecksOverview
  | DeckManagement
  | DeleteDeckConfirm

  | AddNewTag
  | TagsOverview
  | TagManagement
  | Credits

data CardFormState = CardFormState
  { cardFormStateObverse :: Text
  , cardFormStateReverse :: Text
  , cardFormStateTags    :: Text
  }

data UserInput =
  TextInput Text
  | CardChoice CardId
  | TagChoice TagId

makeFields ''CardFormState
makeFields ''CardMetadata
makeFields ''DeckMetadata
makeFields ''BabelTUI
