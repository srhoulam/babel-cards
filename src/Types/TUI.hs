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

  , _gameState              :: !(Maybe GameState)

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


-- TODO: we can't delegate quite as much responsibility to Lua
--       as we thought.
--       - init will have to get a few constants: number of cards
--         to draw, what display mode (once implemented)
--       - state transition will have to be managed by haskell. Lua
--         will provide hooks into it, such as how to compare user
--         input to the card to ascertain correctness
--       - I don't know how to make many types of game possible! thus
--         LUA is on hold until this is hammered out through the
--         built-in modes!
data BabelMode =
  Standard
  | Reverse
  deriving Show
  -- NOTE: Lua scripting is on hold until I learn the requirements
  --       of modes of operation in order to better architect the
  --       interface
  -- | BabelMode
  --   { babelModeInitGame        :: !(Lua ())
  --   , babelModeStateTransition :: !(Lua ())
  --   , babelModeDetermineEnd    :: !(Lua Bool)
  --   }

data BabelView =
  Start
  | DeckSelect
  | ModeSelect
  | Playing
  | GameOver

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
  { gameStateMode           :: !BabelMode
  , gameStateCards          :: ![Maybe (Entity Card)]
    -- ^ Cards in play. A @Nothing@ indicates an empty
    -- slot for purposes where many cards are shown at
    -- once.
  , gameStateScore          :: !Int
    -- ^ Fun! :D
  , gameStateDict           :: !(Map Text Text)
    -- ^ A dictionary for more complex scripts to store
    -- and retrieve data.
  , gameStateUserInputStack :: ![UserInput]
    -- ^ User input. This will be consulted to determine
    -- whether the user answered correctly.
  , gameStateMessagesStack :: ![String]
  }

data NewCard = NewCard
  { newCardObverse :: Text
  , newCardReverse :: Text
  , newCardTags    :: Text
  }

data UserInput =
  TextInput Text
  | CardChoice CardId
  | TagChoice TagId

makeFields ''NewCard
makeFields ''CardMetadata
makeFields ''DeckMetadata
makeFields ''GameState
makeLenses ''BabelTUI

-- emptyGameState :: GameState
-- emptyGameState = GameState
--   { gameStateMode = Standard
--   , gameStateCards = mempty
--   , gameStateScore = 0
--   , gameStateDict  = mempty
--   , gameStateUserInputStack = mempty
--   , gameStateMessagesStack = mempty
--   }
