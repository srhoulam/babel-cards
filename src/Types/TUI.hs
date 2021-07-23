{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE TemplateHaskell        #-}
module Types.TUI where

import           Brick.BChan            (BChan)
import           Brick.Forms            (Form)
import           Brick.Widgets.List     (GenericList)
import           Data.IntMap.Strict        (IntMap)
import           Data.Sequence          (Seq)
import           Data.Text              (Text)
import           Database.Persist.Types (Entity)
import           Lens.Micro.TH
import           Model
import           RIO hiding (view)
import           Types

data BabelTUI = BabelTUI
  { _babel          :: !Babel
  , _view           :: !BabelView
  , _chan           :: !(BChan BabelEvent)

  , _activeCard     :: !(Maybe (Entity Card))
  , _activeDeck     :: !(Maybe (Entity Deck))

  , _cardMap        :: !(IntMap (Entity Card))
  , _deckMap        :: !(IntMap DeckMetadata)
  , _tagMap         :: !(IntMap (Entity Tag))

  , _answerForm     :: !(Form Text BabelEvent String)
  , _cardForm       :: !(Form NewCard BabelEvent String)
  , _deckForm       :: !(Form Deck BabelEvent String)

  , _availableCards :: !(GenericList String Seq CardId)
  , _availableDecks :: !(GenericList String Seq DeckId)
  , _availableModes :: !(GenericList String Seq BabelMode)
  , _availableTags :: !(GenericList String Seq TagId)
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
    Bool -- ^ Deleting the active card?
  | CardManagement

  | AddNewDeck
  | DecksOverview
    Bool -- ^ Deleting the active deck?
  | DeckManagement

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
