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

              AddNewDeck -> do
                updatedForm <- handleFormEvent evt $ st ^. deckForm
                let newState = st & deckForm .~ updatedForm
                case event of
                  EvKey KEsc [] ->
                    continue $ newState & view .~ DecksOverview False
                  EvKey (KChar 'd') [MCtrl] -> do
                    liftIO $ writeBChan (st ^. chan) $ CreateDeck $ formState updatedForm
                    continue $ newState & view .~ DecksOverview False
                  _ -> continue newState

              DecksOverview True -> do
                updatedForm <- handleFormEvent evt $ st ^. answerForm

                let newState = st & answerForm .~ updatedForm
                    userInput = formState updatedForm

                case event of
                  EvKey KEsc [] -> continue $ newState & view .~ DecksOverview False
                  EvKey KEnter [] -> do
                    let mayDeckName = st ^? activeDeck . _Just . val . name
                        mayDeckId = st ^? activeDeck . _Just . key

                    case (Just userInput == mayDeckName, mayDeckId) of
                      (True, Just deckId) -> do
                        liftIO $ writeBChan (st ^. chan)
                          $ DeleteDeck deckId
                        continue $ newState
                          & activeDeck .~ Nothing
                          & view .~ DecksOverview False
                      _ -> continue newState
                  _ -> continue newState

              DecksOverview False -> do
                updatedList <- handleListEvent event $ st ^. availableDecks
                let newState = st & availableDecks .~ updatedList
                    selectedDeckId = fromJust $ keyToInt . snd <$> listSelectedElement updatedList
                    selectedDeck = fromJust $ st ^. deckMap . at selectedDeckId

                case event of
                  EvKey KEsc [] -> continue $ newState & view .~ Start
                  EvKey KEnter [] -> continue $ newState
                    & view .~ DeckManagement
                    & activeDeck .~ selectedDeck ^? deckEntity

                  EvKey (KChar 'a') [] -> continue $ newState
                    & view .~ AddNewDeck
                    & deckForm .~ deckForm'
                  EvKey KDel [] -> continue $ newState
                    & activeDeck .~ selectedDeck ^? deckEntity
                    & view .~ DecksOverview True
                    & answerForm .~ answerForm'
                  _ -> continue newState
            _ -> continue st

        appDraw st = catMaybes
          [ deleteDeckConfirm st
          , Just $ case st ^. view of
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

              CardsOverview _ -> -- TODO NOW
                -- SKETCH:
                -- - a list of tags to apply to the active card
                -- - a list of decks to assign the active card to
                -- - one can then assign the active card to the selected
                --   deck or to the selected tag while scrolling through
                --   the card list
                -- - card relations to tags and decks are loaded once
                --   they become the selected card in the list; this
                --   i/o will be mitigated by layers of caches
                --   (libsqlite, os disk cache, etc.)
                error "CardsOverview"
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
              DecksOverview _ -> applicationTitle
                $ vBox
                [ hBorderWithLabel (str "Decks")
                , vCenter
                  $ vBox
                  [ hCenter
                    $ renderList (renderDeckMenuOption $ st ^. deckMap) True
                    $ st ^. availableDecks
                  , hCenter (strWrap "Press ENTER to make a selection.")
                  , hCenter (strWrap "Press A to add a new deck.")
                  , hCenter (strWrap "Press DEL to delete the selected deck.")
                  , hCenter (strWrap "Press ESC to return.")
                  ]
                ]
              DeckManagement -> applicationTitle
                $ vBox
                [ hBorderWithLabel (str $ Text.unpack $ st ^?! activeDeck . _Just . val . name)
                -- TODO: list cards in this deck
                -- TODO: enable removing cards from deck
                ]
                -- error "DeckManagement"
                -- TODO: deck form for editing the active deck's
                --       name and desc

              Credits -> error "Credits"
          ]

        renderDeckMenuOption :: IntMap DeckMetadata -> Bool -> DeckId -> Widget a
        renderDeckMenuOption dmap _ deckId = hBox
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

          , _activeCard = Nothing
          , _activeDeck = Nothing

          , _cardMap = mempty
          , _deckMap = mempty
          , _tagMap  = mempty

          , _answerForm = answerForm'
          , _cardForm = cardForm'
          , _deckForm = deckForm'

          , _availableCards = list "availableCards" mempty 1
          , _availableDecks = list "availableDecks" mempty 1
          , _availableModes = list "availableModes" mempty 1
          , _availableTags  = list "availableTags"  mempty 1
          , _startOptions = list "startOptions"
              (Seq.fromList
               [ (DeckSelect, "Study a Deck")
               , (DecksOverview False, "Decks")
               , (CardsOverview False, "Cards")
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

        deleteDeckConfirm st = case st ^. view of
          DecksOverview deleting -> do
            guard deleting
            Entity _ deck <- st ^. activeDeck
            return $ vCenter
              $ borderWithLabel (str "Delete Deck")
              $ vBox
              [ hCenter $ strWrap
                ([i|To confirm deletion, write '#{deck ^. name}' in the box below and press ENTER. This cannot be undone. To cancel, press ESC.|])
              , hCenter $ border $ renderForm $ st ^. answerForm
              ]
          _ -> Nothing

        copyrightNotice = vBox
          $ hCenter <$>
          [ strWrap "BabelCards v0.1.0"
          , strWrap "Copyright (c) 2021 Saad Rhoulam"
          , strWrap "BabelCards comes with ABSOLUTELY NO WARRANTY."
          , strWrap "BabelCards is distributed under the GPLv2 license."
          ]
