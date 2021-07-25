{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE TemplateHaskell        #-}
module Types.TUI where

import           Brick.BChan            (BChan)
import           Brick.Forms            (Form)
import           Brick.Widgets.List     (GenericList)
import           Data.IntMap.Strict     (IntMap)
import           Data.Map.Strict        (Map)
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

  , _gameState              :: !GameState

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

  | CreateDeck Deck
  | DeleteDeck DeckId

-- TODO: Care will have to
-- be taken in designing this type, as it will be
-- necessary later to make it user-definable via
-- Lua scripting.
-- IDEA: a state machine architecture:
--       - specify how to create the initial state
--         - inevitably, get a number of cards from
--           the scheduler!
--       - specify a state transition function
--         - e.g., if cards match according to metric,
--           they are removed from the board
--       - specify how to determine an end state
--         - e.g., all cards exhausted
--       - use a stack for user input
--         - stack type: string or card id or tag id
--         - thus we can create games with a variety
--           of equivalence metrics
--           - e.g., memory game where you match cards
--             by common tag; the tag may indicate part
--             of speech, tense, mood, etc.
--       - everything takes place within a free monad
--         representing babel fundamental operations
data BabelMode =
  Standard
  -- ^ Basic flash cards game.
  -- Will be superseded by type that can be defined via
  -- the scripting interface. This basic mode will be
  -- shipped as an example script.

data BabelView =
  Start
  | DeckSelect
  | ModeSelect
  | Playing

  | AddNewCard
  | CardsOverview
  | CardsOverviewDisabled

  | AddNewDeck
  | DecksOverview
  | DeckManagement
  | DeleteDeckConfirm

  | AddNewTag
  | Credits

-- | TODO
data GameState = GameState
  { gameStateCards :: ![Maybe CardId]
    -- ^ Cards in play. A @Nothing@ indicates an empty
    -- slot for purposes where many cards are shown at
    -- once.
  , gameStateScore :: !Int
    -- ^ Fun! :D
  , gameStateDict  :: !(Map Text Text)
    -- ^ A dictionary for more complex scripts to store
    -- and retrieve data.
  }

data NewCard = NewCard
  { newCardObverse :: Text
  , newCardReverse :: Text
  , newCardTags    :: Text
  }

makeFields ''NewCard
makeFields ''CardMetadata
makeFields ''DeckMetadata
makeFields ''GameState
makeLenses ''BabelTUI

emptyGameState = GameState
  { gameStateCards = mempty
  , gameStateScore = 0
  , gameStateDict  = mempty
  }
