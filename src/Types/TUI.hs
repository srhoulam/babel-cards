{-# LANGUAGE NoImplicitPrelude #-}
module Types.TUI where

import           Brick.BChan            (BChan)
import           Brick.Forms            (Form)
import           Brick.Widgets.List     (GenericList)
import           Data.Sequence          (Seq)
import           Data.Text              (Text)
import           Database.Persist.Types (Entity)
import           Model
import           RIO
import           Types

data BabelTUI = BabelTUI
  { btBabel          :: !Babel
  , btView           :: !BabelView
  , btChan           :: !(BChan BabelEvent)

  , btActiveCard     :: !(Maybe (Entity Card))
  , btActiveDeck     :: !(Maybe (Entity Deck))

  , btAnswerForm     :: !(Form Text BabelEvent String)
  , btCardForm       :: !(Form Card BabelEvent String)
  , btDeckForm       :: !(Form Deck BabelEvent String)

  --, btAvailableCards
  , btAvailableDecks :: !(GenericList String Seq DeckMetadata)
  , btAvailableModes :: !(GenericList String Seq BabelMode)
  , btStartOptions   :: !(GenericList String Seq (BabelView, String))
  }

data BabelEvent =
  CreateDeck Deck

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
  | DeckManagement

  | Credits
