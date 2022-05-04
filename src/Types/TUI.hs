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
  { babelTUIBabel                  :: !Babel
  , babelTUIView                   :: !BabelView
  , babelTUIChan                   :: !(BChan BabelEvent)

  , babelTUIGameState              :: !(Maybe GameState)

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

  , babelTUIAnswerForm             :: !(Form Text BabelEvent String)
  , babelTUICardForm               :: !(Form CardFormState BabelEvent String)
  , babelTUIDeckForm               :: !(Form Deck BabelEvent String)

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
  }


data BabelEvent =
  AssignCardDeck DeckId CardId
  | UnassignCardDeck DeckId CardId
  | AssignCardTag TagId CardId
  | UnassignCardTag TagId CardId
  | CreateCard CardFormState
  | EditCard CardId CardFormState
  | DisableCard CardId
  | EnableCard CardId

  | CreateDeck Deck
  | DeleteDeck DeckId

  | CreateTag Tag

  | LoadCards
  | LoadCurrCardMd
  | LoadDecks
  | LoadDeckCards
  | LoadTags
  | LoadTagCards


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
  | CardManagement

  | AddNewDeck
  | DecksOverview
  | DeckManagement
  | DeleteDeckConfirm

  | AddNewTag
  | TagsOverview
  | TagManagement
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
makeFields ''GameState
makeFields ''BabelTUI

-- emptyGameState :: GameState
-- emptyGameState = GameState
--   { gameStateMode = Standard
--   , gameStateCards = mempty
--   , gameStateScore = 0
--   , gameStateDict  = mempty
--   , gameStateUserInputStack = mempty
--   , gameStateMessagesStack = mempty
--   }
