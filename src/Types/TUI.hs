{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE TemplateHaskell        #-}
module Types.TUI where

import           Brick.BChan            (BChan)
import           Brick.Forms            (Form)
import           Brick.Widgets.List     (GenericList)
import           Data.IntMap.Strict     (IntMap)
import           Data.Sequence          (Seq)
import           Data.Text              (Text)
import           Database.Persist.Types (Entity)
import           Lens.Micro.TH
import           Model
import           RIO                    hiding (view)
import           Types

data BabelTUI = BabelTUI
  { _babel                  :: !Babel
  , _view                   :: !BabelView
  , _chan                   :: !(BChan BabelEvent)

  , _focusX                 :: !Int
  -- ^ Left-to-right focus.
  -- Each view will set and interpret this as it will.
  -- Setting min/max bounds during view transitions is
  -- advised.
  , _focusY                 :: !Int
  -- ^ Top-to-bottom focus.
  -- Each view will set and interpret this as it will.
  -- Setting min/max bounds during view transitions is
  -- advised.

  , _activeCard             :: !(Maybe (Entity Card))

  , _cardMapEnabled         :: !(IntMap (Entity Card))
  , _cardMapDisabled        :: !(IntMap (Entity Card))
  , _deckMap                :: !(IntMap DeckMetadata)
  , _tagMap                 :: !(IntMap (Entity Tag))

  , _answerForm             :: !(Form Text BabelEvent String)
  , _cardForm               :: !(Form NewCard BabelEvent String)
  , _deckForm               :: !(Form Deck BabelEvent String)

  -- Display lists
  , _activeCardDecks        :: !(GenericList String Seq DeckId)
  , _activeCardTags         :: !(GenericList String Seq TagId)

  -- Interactive lists
  , _availableCardsEnabled  :: !(GenericList String Seq CardId)
  , _availableCardsDisabled :: !(GenericList String Seq CardId)
  , _availableDecks         :: !(GenericList String Seq DeckId)
  , _availableModes         :: !(GenericList String Seq BabelMode)
  , _availableTags          :: !(GenericList String Seq TagId)
  , _startOptions           :: !(GenericList String Seq (BabelView, String))
  }


data BabelEvent =
  AssignCardDeck DeckId CardId
  | UnassignCardDeck DeckId CardId
  | AssignCardTag TagId CardId
  | UnassignCardTag TagId CardId
  | CreateCard NewCard
  | DisableCard CardId
  | EnableCard CardId
  | LoadCard CardId

  | CreateDeck Deck
  | DeleteDeck DeckId

  | SetCursorBounds
    (Maybe Int)
    -- ^ Minimum _focusX
    (Maybe Int)
    -- ^ Maximum _focusX
    (Maybe Int)
    -- ^ Minimum _focusY
    (Maybe Int)
    -- ^ Maximum _focusY
  | ReloadCards
  | ReloadTags
  -- | ReloadEverything

-- TODO: Care will have to
-- be taken in designing this type, as it will be
-- necessary later to make it user-definable via
-- Lua scripting.
data BabelMode =
  Standard
  -- | UserDefined <FreeMonadChain>

data BabelView =
  Start
  | DeckSelect
  | ModeSelect
  | Playing

  | AddNewCard
  | CardsOverview
  | CardsOverviewDisabled
  | CardManagement

  | AddNewDeck
  | DecksOverview
  | DeckManagement
  | DeleteDeckConfirm

  | AddNewTag
  | Credits

data NewCard = NewCard
  { newCardObverse :: Text
  , newCardReverse :: Text
  , newCardTags    :: Text
  }

makeFields ''NewCard
makeFields ''CardMetadata
makeFields ''DeckMetadata
makeLenses ''BabelTUI
