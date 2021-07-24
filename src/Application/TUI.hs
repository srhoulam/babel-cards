{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
module Application.TUI where

import           Application.Database
import           Brick
import           Brick.BChan
import           Brick.Forms                      (checkboxField, editTextField,
                                                   formState, handleFormEvent,
                                                   newForm, renderForm, (@@=))
import           Brick.Main
import           Brick.Widgets.Border             (border, borderWithLabel,
                                                   hBorder, hBorderWithLabel)
import           Brick.Widgets.Border.Style       (unicode)
import           Brick.Widgets.Center             (hCenter, vCenter)
import           Brick.Widgets.Dialog             (dialog)
import           Brick.Widgets.Edit               (Editor, editor,
                                                   handleEditorEvent,
                                                   renderEditor)
import           Brick.Widgets.List               hiding (reverse)
import qualified Data.IntMap.Strict as IntMap
import           Data.Maybe                       (fromJust)
import qualified Data.Sequence                    as Seq (filter, fromList,
                                                          singleton, (|>))
import           Data.String.Interpolate.IsString
import qualified Data.Text                        as Text (unpack)
import qualified Graphics.Vty                     (defaultConfig, mkVty)
import           Graphics.Vty.Attributes
import           Graphics.Vty.Input.Events
import           Lens.Micro.Platform hiding (view)
-- import           Model
import           RIO                              hiding (reverse, view)
-- import           RIO.List                         (headMaybe)
import           RIO.Time
import           Text.Printf
import           Types
import           Types.TUI

lifecycle :: RIO Babel ()
lifecycle = do
  env <- ask
  eventChan <- liftIO $ newBChan 10
  let buildVty = Graphics.Vty.mkVty Graphics.Vty.defaultConfig
  initialVty <- liftIO $ buildVty
  _ <- liftIO
    $ customMain initialVty buildVty (Just eventChan) lifecycleApp
    $ initialState env eventChan
  return ()
  where lifecycleApp = App {..} :: App BabelTUI BabelEvent String
        appAttrMap _ = attrMap defAttr
          [ (listAttr, withStyle currentAttr dim)
          , (listSelectedAttr, withStyle currentAttr bold)
          , (listSelectedFocusedAttr, withStyle currentAttr $ bold + standout)
          ]
        appChooseCursor = showFirstCursor
        appStartEvent st = do
          (availCards, availDecks, availTags) <- runRIO (st ^. babel) $ runDB $ do
            availCards <- retrieveCards
            availDecks <- retrieveDeckSummaries
            availTags  <- retrieveTags
            return (availCards, availDecks, availTags)
          -- TODO: load modes, when lua scripting is implemented
          let dmap = IntMap.fromList
                $ (\dm -> (keyToInt $ dm ^. deckEntity . key, dm))
                <$> availDecks
              deckIds = (^. deckEntity . key) <$> availDecks
              cmap = IntMap.fromList
                $ (\ce -> (keyToInt $ ce ^. key, ce))
                <$> availCards
              cardIds = (^. key) <$> availCards
              tmap = IntMap.fromList
                $ (\te -> (keyToInt $ te ^. key, te))
                <$> availTags
              tagIds = (^. key) <$> availTags
          return
            $ st
            & cardMap .~ cmap
            & deckMap .~ dmap
            & tagMap  .~ tmap
            & availableCards .~ list "availableCards" (Seq.fromList cardIds) 1
            & availableDecks .~ list "availableDecks" (Seq.fromList deckIds) 1
            & availableModes .~ list "availableModes" (Seq.singleton Standard) 1
            & availableTags  .~ list "availableTags"  (Seq.fromList tagIds) 1

        appHandleEvent st evt = do
          case evt of
            AppEvent appEvent -> case appEvent of
              AssignCardDeck   deckId cardId -> error "assign card deck"
              UnassignCardDeck deckId cardId -> error "unassign card deck"
              AssignCardTag    tagId  cardId -> error "assign card tag"
              UnassignCardTag  tagId  cardId -> error "unassign card tag"

              CreateCard NewCard {..} -> do
                -- refresh availableCards from db
                -- differentially update cardMap
                error "create card"
              DisableCard cardId -> error "disable card"
              EnableCard cardId -> error "enable card"
              LoadCard cardId -> error "load card"
                -- TODO NOW MARK FIXME
                -- SKETCH
                -- - load the currently selected card's (from availCards)
                --   associated decks and tags
                -- - assign the decks to activeCardDecks
                -- - assign the tags to activeCardTags

              CreateDeck deck -> do
                deckId <- liftIO $ runRIO (st ^. babel) $ runDB $ insert deck
                continue $ st
                  & deckMap %~ IntMap.insert (keyToInt deckId) (newDeckMetadata $ Entity deckId deck)
                  & availableDecks . listElementsL
                  %~ (Seq.|> deckId)
              DeleteDeck deckId -> do
                liftIO $ runRIO (st ^. babel) $ runDB $ delete deckId
                continue $ st
                  & deckMap %~ IntMap.delete (keyToInt deckId)
                  & availableDecks . listElementsL
                  %~ Seq.filter (deckId /=)

            VtyEvent event -> case st ^. view of
              Start -> do
                updatedList <- handleListEvent event $ st ^. startOptions
                let newState = st & startOptions .~ updatedList
                case event of
                  EvKey (KChar 'q') [] -> halt newState
                  EvKey KEnter [] -> continue $ newState
                    & view
                    .~ maybe (st ^. view) (fst . snd) (listSelectedElement updatedList)
                  _ -> continue newState

              DeckSelect -> continue st
              ModeSelect -> continue st

              AddNewCard -> error "add card"
              CardsOverview -> do
                -- SKETCH:
                -- - card relations to tags and decks are loaded once
                --   they become the selected card in the list; this
                --   i/o will be mitigated by layers of caches
                --   (libsqlite, os disk cache, etc.)
                updatedAvailCards <- handleListEvent event $ st ^. availableCards
                updatedAvailDecks <- handleListEvent event $ st ^. availableDecks
                updatedAvailTags <- handleListEvent event $ st ^. availableTags

                let newState = st
                      & availableCards .~ updatedAvailCards
                      & availableDecks .~ updatedAvailDecks
                      & availableTags .~ updatedAvailTags
                    selectedCardId = fromJust $ snd
                      <$> listSelectedElement (st ^. availableCards)
                    selectedDeckId = fromJust $ snd
                      <$> listSelectedElement (st ^. availableDecks)
                    selectedTagId = fromJust $ snd
                      <$> listSelectedElement (st ^. availableTags)
                    activeCardHasChanged =
                      st ^? activeCard . _Just . key
                      /= Just selectedCardId

                when activeCardHasChanged
                  $ liftIO $ writeBChan (st ^. chan)
                  $ LoadCard selectedCardId

                case event of
                  EvKey KEsc [] -> continue $ newState & view .~ Start
                  EvKey KIns [] -> do
                    liftIO $ writeBChan (st ^. chan)
                      $ EnableCard selectedCardId
                    continue newState
                  EvKey KDel [] -> do
                    liftIO $ writeBChan (st ^. chan)
                      $ DisableCard selectedCardId
                    continue newState
                  EvKey (KChar 'd') [MCtrl] -> do
                    liftIO $ writeBChan (st ^. chan)
                      $ AssignCardDeck selectedDeckId selectedCardId
                    continue newState
                  EvKey (KChar 'd') [MCtrl, MShift] -> do
                    liftIO $ writeBChan (st ^. chan)
                      $ UnassignCardDeck selectedDeckId selectedCardId
                    continue newState
                  EvKey (KChar 't') [MCtrl] -> do
                    liftIO $ writeBChan (st ^. chan)
                      $ AssignCardTag selectedTagId selectedCardId
                    continue newState
                  EvKey (KChar 't') [MCtrl, MShift] -> do
                    -- TODO: unassign card from tag
                    liftIO $ writeBChan (st ^. chan)
                      $ UnassignCardTag selectedTagId selectedCardId
                    continue newState
                  _ -> continue newState

              AddNewDeck -> do
                updatedForm <- handleFormEvent evt $ st ^. deckForm
                let newState = st & deckForm .~ updatedForm
                case event of
                  EvKey KEsc [] ->
                    continue $ newState & view .~ DecksOverview
                  EvKey (KChar 'd') [MCtrl] -> do
                    liftIO $ writeBChan (st ^. chan) $ CreateDeck $ formState updatedForm
                    continue $ newState & view .~ DecksOverview
                  _ -> continue newState

              DecksOverview -> do
                updatedList <- handleListEvent event $ st ^. availableDecks
                let newState = st & availableDecks .~ updatedList

                case event of
                  EvKey KEsc [] -> continue $ newState & view .~ Start
                  EvKey KEnter [] -> continue $ newState
                    & view .~ DeckManagement

                  EvKey (KChar 'a') [] -> continue $ newState
                    & view .~ AddNewDeck
                    & deckForm .~ deckForm'
                  EvKey KDel [] -> continue $ newState
                    & view .~ DeleteDeckConfirm
                    & answerForm .~ answerForm'
                  _ -> continue newState

              DeckManagement -> do
                error "deck management"

              DeleteDeckConfirm -> do
                updatedForm <- handleFormEvent evt $ st ^. answerForm

                let newState = st & answerForm .~ updatedForm
                    userInput = formState updatedForm
                    selectedDeckId = fromJust $ snd
                      <$> listSelectedElement (st ^. availableDecks)
                    selectedDeck = st ^?! deckMap . at (keyToInt selectedDeckId) . _Just
                    selectedDeckName = selectedDeck ^. deckEntity . val . name
                    userInputMatchesName = Just userInput == Just selectedDeckName

                case event of
                  EvKey KEsc [] -> continue $ newState & view .~ DecksOverview
                  EvKey KEnter [] -> do
                    case (userInputMatchesName, selectedDeckId) of
                      (True, deckId) -> do
                        liftIO $ writeBChan (st ^. chan)
                          $ DeleteDeck deckId
                        continue $ newState
                          & view .~ DecksOverview
                      _ -> continue newState
                  _ -> continue newState

            _ -> continue st

        appDraw st = catMaybes
          [ Just $ case st ^. view of
              Start -> applicationTitle
                $ vBox
                [ hCenter $ strWrap "A flash-cards memorization tool."
                , hBorder
                , vCenter
                  $ vBox
                  [ renderList renderStartOption True $ st ^. startOptions
                  , hCenter (strWrap "Press ENTER to make a selection.")
                  , hCenter (strWrap "Press Q to quit.")
                  ]
                , hBorder
                , copyrightNotice
                ]
              DeckSelect ->
                error "deck select"
              ModeSelect ->
                error "mode select"
              Playing ->
                error "playing"

              AddNewCard -> applicationTitle
                $ vBox
                [ hBorderWithLabel (str "Add New Card")
                , vCenter $ renderForm $ st ^. cardForm
                , hCenter $ strWrap "Press TAB to proceed to the next field."
                , hCenter $ strWrap "Press Shift+TAB to return to a previous field."
                , hCenter $ strWrap "Press Ctrl+D when finished."
                ]

              CardsOverview -> applicationTitle
                $ vBox
                [ hBorderWithLabel (str "Cards")
                , hBox
                  [ vBox
                    [ borderWithLabel (str "All Decks")
                      $ renderList
                      (renderDeckOption $ st ^. deckMap)
                      (st ^. focusX == 0)
                      $ st ^. availableDecks
                    , borderWithLabel (str "Card Decks")
                      $ renderList
                      (renderDeckOption $ st ^. deckMap)
                      (st ^. focusX == 0)
                      $ st ^. activeCardDecks
                    ]
                  , vBox
                    [ borderWithLabel (str "All Tags")
                      $ renderList
                      (renderTagOption  $ st ^. tagMap)
                      (st ^. focusX == 1)
                      $ st ^. availableTags
                    , borderWithLabel (str "Card Tags")
                      $ renderList
                      (renderTagOption  $ st ^. tagMap)
                      (st ^. focusX == 1)
                      $ st ^. activeCardTags
                    ]
                  , borderWithLabel (str "Cards")
                    $ renderList
                    (renderCardOption $ st ^. cardMap)
                    (st ^. focusX == 2)
                    $ st ^. availableCards
                  ]
                , hCenter $ strWrap "Switch lists with left/right keys."
                , hCenter $ strWrap "Select list options with up/down keys."
                , hCenter $ strWrap "Press Ctrl+T to assign the selected tag to a card."
                , hCenter $ strWrap "Press Ctrl+D to assign the selected deck to a card."
                , hCenter $ strWrap "Press Ctrl+Shift+T to unassign the selected tag from a card."
                , hCenter $ strWrap "Press Ctrl+Shift+D to unassign the selected deck from a card."
                , hCenter $ strWrap "Press INS to enable the selected card."
                , hCenter $ strWrap "Press DEL to disable the selected card."
                ]

              CardManagement ->
                error "CardManagement"

              AddNewDeck -> applicationTitle
                $ vBox
                [ hBorderWithLabel (str "Add New Deck")
                , vCenter $ renderForm $ st ^. deckForm
                , hCenter $ strWrap "Press TAB to proceed to the next field."
                , hCenter $ strWrap "Press Shift+TAB to return to a previous field."
                , hCenter $ strWrap "Press Ctrl+D when finished."
                ]
              DecksOverview -> applicationTitle
                $ vBox
                [ hBorderWithLabel (str "Decks")
                , vCenter
                  $ vBox
                  [ hCenter
                    $ renderList (renderDeckOverviewOption $ st ^. deckMap) True
                    $ st ^. availableDecks
                  , hCenter (strWrap "Press ENTER to make a selection.")
                  , hCenter (strWrap "Press A to add a new deck.")
                  , hCenter (strWrap "Press DEL to delete the selected deck.")
                  , hCenter (strWrap "Press ESC to return.")
                  ]
                ]
              DeckManagement ->
                let selectedDeckId = fromJust $ snd
                      <$> listSelectedElement (st ^. availableDecks)
                    selectedDeck = st ^?! deckMap . at (keyToInt selectedDeckId) . _Just
                    selectedDeckName = selectedDeck ^. deckEntity . val . name
                in applicationTitle
                   $ vBox
                   [ hBorderWithLabel (str $ Text.unpack selectedDeckName)
                   -- TODO: list cards in this deck
                   -- TODO: enable removing cards from deck
                   -- TODO: deck form for editing the active deck's
                   --       name and desc
                   ]
              DeleteDeckConfirm ->
                let selectedDeckId = fromJust $ snd
                      <$> listSelectedElement (st ^. availableDecks)
                    selectedDeck = st ^?! deckMap . at (keyToInt selectedDeckId) . _Just
                    selectedDeckName = selectedDeck ^. deckEntity . val . name
                in applicationTitle
                   $ vCenter
                   $ borderWithLabel (str "Delete Deck")
                   $ vBox
                   [ hCenter $ strWrap
                     ([i|To confirm deletion, write '#{selectedDeckName}' in the box below and press ENTER. This cannot be undone. To cancel, press ESC.|])
                   , hCenter $ border $ renderForm $ st ^. answerForm
                   ]

              AddNewTag -> error "add new tag"
              Credits -> error "Credits"
          ]

        renderCardOption :: IntMap (Entity Card) -> Bool -> CardId -> Widget a
        renderCardOption cmap _ cardId =
          let cardObverse = card ^. obverse
              cardReverse = card ^. reverse
              card = cmap ^?! at (keyToInt cardId) . _Just . val
              cardLabel = ([i|#{cardObverse} / #{cardReverse}|])
          in padRight Max
             $ str cardLabel

        renderDeckOption :: IntMap DeckMetadata -> Bool -> DeckId -> Widget a
        renderDeckOption dmap _ deckId = padRight Max
          $ str $ Text.unpack
          $ dmap ^?! at (keyToInt deckId) . _Just . deckEntity . val . name

        renderTagOption :: IntMap (Entity Tag) -> Bool -> TagId -> Widget a
        renderTagOption tmap _ tagId = padRight Max
          $ str $ Text.unpack
          $ tmap ^?! at (keyToInt tagId) . _Just . val . name

        renderDeckOverviewOption :: IntMap DeckMetadata -> Bool -> DeckId -> Widget a
        renderDeckOverviewOption dmap _ deckId = hBox
          [ padRight Max $ str $ Text.unpack
            $ dmap ^?! at (keyToInt deckId) . _Just . deckEntity . val . name
          , str $ printf "%7d"
            $ dmap ^?! at (keyToInt deckId) . _Just . cardCount
          , padLeft Max $ str
            $ maybe "Never" (formatTime defaultTimeLocale "%_Y-%m-%d %T")
            $ dmap ^? at (keyToInt deckId) . _Just . lastStudied . _Just
          ]
          <=> strWrap (Text.unpack $ dmap ^. at (keyToInt deckId) . _Just . deckEntity . val . description)

        renderStartOption _ (_, label) =
          hCenter $ strWrap label

        applicationTitle = borderWithLabel (str "BabelCards")

        initialState env chan = BabelTUI
          { _babel = env
          , _view = Start
          , _chan = chan

          , _focusX = 0
          , _focusY = 0

          , _activeCard = Nothing

          , _cardMap = mempty
          , _deckMap = mempty
          , _tagMap  = mempty

          , _answerForm = answerForm'
          , _cardForm = cardForm'
          , _deckForm = deckForm'

          , _activeCardDecks = list "activeCardDecks" mempty 1
          , _activeCardTags = list "activeCardTags" mempty 1

          , _availableCards = list "availableCards" mempty 1
          , _availableDecks = list "availableDecks" mempty 1
          , _availableModes = list "availableModes" mempty 1
          , _availableTags  = list "availableTags"  mempty 1
          , _startOptions = list "startOptions"
              (Seq.fromList
               [ (DeckSelect, "Study a Deck")
               , (DecksOverview, "Decks")
               , (CardsOverview, "Cards")
               , (Credits, "About")
               ])
              1
          }

        answerForm' = newForm
          [ editTextField id "userEntry" (Just 1) ]
          mempty

        cardForm' = newForm
          [ (padRight (Pad 1) (str "Obverse:") <+>)
            @@= editTextField obverse "cardObverse" (Just 1)
          , (padRight (Pad 1) (str "Reverse:") <+>)
            @@= editTextField reverse "cardReverse" (Just 1)
          , (padRight (Pad 4) (str "Tags:") <+>)
            @@= editTextField tags "cardTagList" Nothing
          ]
          (NewCard "" "" "")

        deckForm' = newForm
          [ (padRight (Pad 8) (str "Name:") <+>)
            @@= editTextField name "deckName" (Just 1)
          , (padRight (Pad 1) (str "Description:") <+>)
            @@= editTextField description "deckDescription" Nothing
          ]
          (Deck "" "")

        newDeckMetadata de = DeckMetadata
          { deckMetadataDeckEntity = de
          , deckMetadataCardCount = 0
          , deckMetadataLastStudied = Nothing
          }

        copyrightNotice = vBox
          $ hCenter <$>
          [ strWrap "BabelCards v0.1.0"
          , strWrap "Copyright (c) 2021 Saad Rhoulam"
          , strWrap "BabelCards comes with ABSOLUTELY NO WARRANTY."
          , strWrap "BabelCards is distributed under the GPLv2 license."
          ]
