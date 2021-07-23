{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE TemplateHaskell        #-}
module Types.TUI where

import           Brick.BChan            (BChan)
import           Brick.Forms            (Form)
import           Brick.Widgets.List     (GenericList)
import           Data.Sequence          (Seq)
import           Data.Text              (Text)
import           Database.Persist.Types (Entity)
import           Lens.Micro.TH
import           Model
import           RIO
import           Types

data BabelTUI = BabelTUI
  { _babel          :: !Babel
  , _view           :: !BabelView
  , _chan           :: !(BChan BabelEvent)

  , _activeCard     :: !(Maybe (Entity Card))
  , _activeDeck     :: !(Maybe (Entity Deck))

  , _answerForm     :: !(Form Text BabelEvent String)
  , _cardForm       :: !(Form Card BabelEvent String)
  , _deckForm       :: !(Form Deck BabelEvent String)

  --, _availableCards
  , _availableDecks :: !(GenericList String Seq DeckMetadata)
  , _availableModes :: !(GenericList String Seq BabelMode)
  , _startOptions   :: !(GenericList String Seq (BabelView, String))
  }


data BabelEvent =
  CreateDeck Deck
  | DeleteDeck DeckId

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
  | CardManagement

  | AddNewDeck
  | DecksOverview
    Bool -- ^ Deleting the active deck?
  | DeckManagement

  | Credits

makeLenses ''BabelTUI
