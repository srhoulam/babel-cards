{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
module Application.TUI where

import           Application.Database             as DB
import           Application.Scheduler
import           Brick
import           Brick.BChan
import           Brick.Forms                      (editTextField, formState,
                                                   handleFormEvent, newForm,
                                                   renderForm, (@@=))
import           Brick.Widgets.Border             (border, borderWithLabel,
                                                   hBorder, hBorderWithLabel)
import           Brick.Widgets.Center             (hCenter, vCenter)
import           Brick.Widgets.List               hiding (reverse)
import           Control.Monad.Trans.Maybe
import qualified Data.IntMap.Strict               as IntMap
import           Data.Maybe                       (fromJust)
import qualified Data.Sequence                    as Seq (fromList)
import           Data.String.Interpolate.IsString
import qualified Data.Text                        as Text (pack, unpack)
import qualified Graphics.Vty                     (defaultConfig, mkVty)
import           Graphics.Vty.Attributes
import           Graphics.Vty.Input.Events
import           Lens.Micro.Platform              hiding (view)
import           RIO                              hiding (reverse, view)
import           RIO.Partial                      (toEnum)
import           RIO.Time
import           Text.EditDistance                (defaultEditCosts, restrictedDamerauLevenshteinDistance)
import           Text.Printf
import           Types
import           Types.TUI

lifecycle :: RIO Babel ()
lifecycle = do
  env <- ask
  eventChan <- liftIO $ newBChan 100
  let buildVty = Graphics.Vty.mkVty Graphics.Vty.defaultConfig
  initialVty <- liftIO buildVty
  _ <- liftIO
    $ customMain initialVty buildVty (Just eventChan) lifecycleApp
    $ initialState env
  return ()
  where lifecycleApp = App {..} :: App BabelTUI () String
        appAttrMap _ = attrMap defAttr
          [ (listAttr, withStyle currentAttr dim)
          , (listSelectedAttr, withStyle currentAttr bold)
          , (listSelectedFocusedAttr, withStyle currentAttr $ bold + standout)
          ]
        appChooseCursor = showFirstCursor

        appStartEvent = loadAll

        appHandleEvent st evt = case evt of
          VtyEvent event -> case st ^. view of
            Playing -> case st ^?! matchMode . _Just of
              Standard -> playStandardGame st evt event
              Reverse  -> playReverseGame st evt event

            CardsOverview -> do
              st1 <- setCursorBounds st (Just 0) (Just 2) Nothing Nothing

              updatedAvailDecks <- runMaybeT $ do
                guard $ st1 ^. focusX == 0
                lift $ handleListEvent event $ st1 ^. availableDecks
              updatedAvailTags <- runMaybeT $ do
                guard $ st1 ^. focusX == 1
                lift $ handleListEvent event $ st1 ^. availableTags
              updatedAvailCardsEnabled <- runMaybeT $ do
                guard $ st1 ^. focusX == 2
                lift $ handleListEvent event $ st1 ^. availableCardsEnabled

              let st2 = st1
                    & availableCardsEnabled .~ fromMaybe (st1 ^. availableCardsEnabled) updatedAvailCardsEnabled
                    & availableDecks .~ fromMaybe (st1 ^. availableDecks) updatedAvailDecks
                    & availableTags .~ fromMaybe (st1 ^. availableTags) updatedAvailTags

              let selectedCardId = fmap snd
                    $ listSelectedElement
                    $ st2 ^. availableCardsEnabled
                  selectedDeckId = fmap snd
                    $ listSelectedElement
                    $ st2 ^. availableDecks
                  selectedTagId = fmap snd
                    $ listSelectedElement
                    $ st2 ^. availableTags

              newState <- loadCurrCardMd st2

              case event of
                EvKey KEsc [] -> returnToStart newState
                EvKey KLeft [] -> continue $ newState
                  & focusX %~ max 0 . (\x -> x - 1)
                EvKey KRight [] -> continue $ newState
                  & focusX %~ min 2 . (1+)
                EvKey KIns [] ->
                  continue $ newState & view .~ CardsOverviewDisabled
                EvKey KDel [] -> do
                  _ <- liftIO $ runMaybeT $ do
                    scid <- MaybeT $ return selectedCardId
                    lift $ runRIO (st2 ^. babel) $ runDB
                      $ update scid [ CardEnabled =. False ]

                  loadCards newState
                    >>= loadCurrCardMd
                    >>= continue

                EvKey (KChar 'a') [] -> continue $ newState
                  & view .~ AddNewCard
                  & cardForm .~ newCardForm'
                EvKey (KChar 'e') [] -> do
                  let selectedCardMay' = selectedCardId
                        >>= (IntMap.!?) (st ^. cardMapEnabled) . keyToInt
                  cardMay <- forM selectedCardMay' $ return . entityVal
                  case cardMay of
                    Just card -> continue $ newState
                      & view .~ CardManagement
                      & previousView ?~ CardsOverview
                      & cardForm .~ editCardForm' (card ^. obverse) (card ^. reverse)
                    Nothing -> continue newState
                EvKey (KChar 'd') [] -> do
                  _ <- liftIO $ runMaybeT $ do
                    sdid <- MaybeT $ return selectedDeckId
                    scid <- MaybeT $ return selectedCardId
                    lift $ runRIO (newState ^. babel) $ runDB
                      $ assignCardDeck sdid scid
                  loadCards newState
                    >>= loadCurrCardMd
                    >>= continue
                EvKey (KChar 'd') [MCtrl] -> do
                  _ <- liftIO $ runMaybeT $ do
                    sdid <- MaybeT $ return selectedDeckId
                    scid <- MaybeT $ return selectedCardId
                    lift $ runRIO (newState ^. babel) $ runDB
                      $ unassignCardDeck sdid scid
                  loadCards newState
                    >>= loadCurrCardMd
                    >>= continue
                EvKey (KChar 'f') [] -> do
                  let answerFormPrefilled = newForm
                        [ editTextField id "userEntry" (Just 1) ]
                        $ fromMaybe mempty
                        $ newState ^. cardFilter
                  continue $ newState
                    & view .~ SetCardFilter
                    & answerForm .~ answerFormPrefilled
                EvKey (KChar 'f') [MCtrl] ->
                  loadCards (newState & cardFilter .~ Nothing)
                    >>= loadCurrCardMd
                    >>= continue
                EvKey (KChar 't') [] -> do
                  _ <- liftIO $ runMaybeT $ do
                    stid <- MaybeT $ return selectedTagId
                    scid <- MaybeT $ return selectedCardId
                    lift $ runRIO (newState ^. babel) $ runDB
                      $ assignCardTag stid scid
                  loadCards newState
                    >>= loadCurrCardMd
                    >>= continue
                EvKey (KChar 't') [MCtrl] -> do
                  _ <- liftIO $ runMaybeT $ do
                    stid <- MaybeT $ return selectedTagId
                    scid <- MaybeT $ return selectedCardId
                    lift $ runRIO (newState ^. babel) $ runDB
                      $ unassignCardTag stid scid
                  loadCards newState
                    >>= loadCurrCardMd
                    >>= continue
                _ -> continue newState

            CardsOverviewDisabled -> do
              updatedAvailCardsDisabled <- handleListEvent event
                $ st ^. availableCardsDisabled

              let newState = st
                    & availableCardsDisabled .~ updatedAvailCardsDisabled
                  selectedCardId = fmap snd
                    $ listSelectedElement
                    $ newState ^. availableCardsDisabled

              case event of
                EvKey KEsc [] -> continue $ newState & view .~ CardsOverview
                EvKey KEnter [] -> do
                  _ <- liftIO $ runMaybeT $ do
                    scid <- MaybeT $ return selectedCardId
                    lift $ runRIO (newState ^. babel) $ runDB
                      $ update scid [ CardEnabled =. True ]
                  loadCards newState
                    >>= loadCurrCardMd
                    >>= continue
                _ -> continue newState

            SetCardFilter -> do
              updatedForm <- handleFormEvent evt $ st ^. answerForm
              let newState = st & answerForm .~ updatedForm
              case event of
                EvKey KEsc [] -> continue $ newState & view .~ CardsOverview
                EvKey KEnter [] -> loadCards
                  (newState
                   & cardFilter ?~ formState updatedForm
                   & view .~ CardsOverview)
                  >>= loadCurrCardMd
                  >>= continue

                _ -> continue newState

            DecksOverview -> do
              updatedList <- handleListEvent event $ st ^. availableDecks
              let newState = st & availableDecks .~ updatedList

              case event of
                EvKey KEsc [] -> returnToStart newState
                EvKey KEnter [] -> loadCardsForDeck
                  (newState & view .~ DeckManagement)
                  >>= continue
                EvKey (KChar 'a') [] -> continue $ newState
                  & view .~ AddNewDeck
                  & deckForm .~ deckForm'
                EvKey KDel [] -> continue $ newState
                  & view .~ DeleteDeckConfirm
                  & answerForm .~ answerForm'
                _ -> continue newState

            TagsOverview -> do
              updatedList <- handleListEvent event $ st ^. availableTags
              let newState = st & availableTags .~ updatedList

              case event of
                EvKey KEsc [] -> returnToStart newState
                EvKey KEnter [] ->
                  -- NOTE: this ignores hitting inter with no tag selected
                  --       it would otherwise cause a crash
                  case listSelectedElement (st ^. availableTags) of
                    Just _ -> loadCardsForTag
                      (newState & view .~ TagManagement)
                      >>= continue
                    Nothing -> continue newState
                EvKey (KChar 'a') [] -> continue $ newState
                  & view .~ AddNewTag
                  & answerForm .~ answerForm'
                _ -> continue newState

            CardManagement -> do
              updatedForm <- handleFormEvent evt $ st ^. cardForm
              let newState = st & cardForm .~ updatedForm
                  selectedCardIdMay = listSelectedElement (st ^. availableCardsEnabled)

              case event of
                EvKey KEsc [] -> returnToPreviousView newState
                EvKey (KChar 'd') [MCtrl] -> do
                  liftIO $ forM_ selectedCardIdMay $ \(_, selectedCardId) -> do
                    runRIO (newState ^. babel) $ runDB $ update selectedCardId
                      [ CardObverse =. formState updatedForm ^. obverse
                      , CardReverse =. formState updatedForm ^. reverse
                      ]

                  returnToPreviousView newState

                _ -> continue newState

            DeckManagement -> do
              updatedAvailCardsEnabled <- handleListEvent event $ st ^. availableCardsEnabled

              let newState = st
                    & availableCardsEnabled .~ updatedAvailCardsEnabled
                  selectedCardId = fmap snd
                    $ listSelectedElement
                    $ newState ^. availableCardsEnabled
                  selectedDeckId = fmap snd
                    $ listSelectedElement
                    $ newState ^. availableDecks

              case event of
                EvKey KEsc [] -> loadDecks (newState & view .~ DecksOverview) >>= continue
                EvKey KDel [] -> do
                  _ <- liftIO $ runMaybeT $ do
                    scid <- MaybeT $ return selectedCardId
                    lift $ runRIO (newState ^. babel) $ runDB $ update scid [ CardEnabled =. False ]

                  loadCardsForDeck newState
                    >>= continue

                EvKey (KChar 'd') [MCtrl] -> do
                  _ <- liftIO $ runMaybeT $ do
                    sdid <- MaybeT $ return selectedDeckId
                    scid <- MaybeT $ return selectedCardId
                    lift $ runRIO (newState ^. babel) $ runDB
                      $ unassignCardDeck sdid scid
                  loadCardsForDeck newState
                    >>= continue

                EvKey (KChar 'e') [] -> do
                  let selectedCardMay' = selectedCardId
                        >>= (IntMap.!?) (st ^. cardMapEnabled) . keyToInt
                  cardMay <- forM selectedCardMay' $ return . entityVal
                  case cardMay of
                    Just card -> continue $ newState
                      & view .~ CardManagement
                      & cardForm .~ editCardForm' (card ^. obverse) (card ^. reverse)
                      & previousView ?~ DeckManagement
                    Nothing -> continue newState

                _             -> continue newState

            TagManagement -> do
              updatedAvailCardsEnabled <- handleListEvent event $ st ^. availableCardsEnabled

              let newState = st
                    & availableCardsEnabled .~ updatedAvailCardsEnabled
                  selectedCardId = fmap snd
                    $ listSelectedElement
                    $ newState ^. availableCardsEnabled
                  selectedTagId = fmap snd
                    $ listSelectedElement
                    $ newState ^. availableTags

              case event of
                EvKey KEsc [] -> loadTags (newState & view .~ TagsOverview) >>= continue
                EvKey (KChar 't') [MCtrl] -> do
                  _ <- liftIO $ runMaybeT $ do
                    stid <- MaybeT $ return selectedTagId
                    scid <- MaybeT $ return selectedCardId
                    lift $ runRIO (newState ^. babel) $ runDB
                      $ unassignCardTag stid scid
                  loadCardsForTag newState
                    >>= continue

                EvKey (KChar 'e') [] -> do
                  let selectedCardMay' = selectedCardId
                        >>= (IntMap.!?) (st ^. cardMapEnabled) . keyToInt
                  cardMay <- forM selectedCardMay' $ return . entityVal
                  case cardMay of
                    Just card -> continue $ newState
                      & view .~ CardManagement
                      & cardForm .~ editCardForm' (card ^. obverse) (card ^. reverse)
                      & previousView ?~ TagManagement
                    Nothing -> continue newState

                _             -> continue newState

            AddNewCard -> do
              updatedForm <- handleFormEvent evt $ st ^. cardForm
              let newState = st & cardForm .~ updatedForm
              case event of
                EvKey KEsc [] ->
                  continue $ newState & view .~ CardsOverview
                EvKey (KChar 'd') [MCtrl] -> do
                  _ <- liftIO $ runRIO (newState ^. babel) $ runDB
                       $ createCard $ formState updatedForm
                  loadTags (newState & view .~ CardsOverview)
                    >>= loadCards
                    >>= loadCurrCardMd
                    >>= continue
                _ -> continue newState

            AddNewTag -> do
              updatedForm <- handleFormEvent evt $ st ^. answerForm
              let newState = st & answerForm .~ updatedForm
              case event of
                EvKey KEsc [] -> continue $ newState & view .~ TagsOverview
                EvKey KEnter [] -> do
                  _ <- liftIO $ runRIO (newState ^. babel) $ runDB
                       $ insert $ Tag $ formState updatedForm
                  loadTags (newState & view .~ TagsOverview)
                    >>= continue
                _ -> continue newState

            AddNewDeck -> do
              updatedForm <- handleFormEvent evt $ st ^. deckForm
              let newState = st & deckForm .~ updatedForm
              case event of
                EvKey KEsc [] ->
                  continue $ newState & view .~ DecksOverview
                EvKey (KChar 'd') [MCtrl] -> do
                  _ <- liftIO $ runRIO (newState ^. babel) $ runDB
                       $ insert $ formState updatedForm
                  loadDecks (newState & view .~ DecksOverview)
                    >>= continue
                _ -> continue newState

            DeleteDeckConfirm -> do
              updatedForm <- handleFormEvent evt $ st ^. answerForm

              let newState = st & answerForm .~ updatedForm
                  userInput = formState updatedForm
                  selectedDeckId = snd $ fromJust
                    $ listSelectedElement (st ^. availableDecks)
                  selectedDeck = st ^?! deckMap . at (keyToInt selectedDeckId) . _Just
                  selectedDeckName = selectedDeck ^. deckEntity . val . name
                  userInputMatchesName = Just userInput == Just selectedDeckName

              case event of
                EvKey KEsc [] -> continue $ newState & view .~ DecksOverview
                EvKey KEnter [] -> do
                  case (userInputMatchesName, selectedDeckId) of
                    (True, deckId') -> do
                      liftIO $ runRIO (newState ^. babel) $ runDB $ do
                          deleteWhere [ QueueItemDeckId ==. deckId']
                          deleteWhere [ DeckMemberDeckId ==. deckId' ]
                          delete deckId'
                      loadDecks (newState & view .~ DecksOverview)
                        >>= continue
                    _ -> continue newState
                _ -> continue newState

            Start -> do
              updatedList <- handleListEvent event $ st ^. startOptions
              let newState = st & startOptions .~ updatedList
              case event of
                EvKey (KChar 'q') [] -> halt newState
                EvKey KEnter [] -> continue $ newState
                  & view
                  .~ maybe (st ^. view) (fst . snd) (listSelectedElement updatedList)
                _ -> continue newState

            DeckSelect -> do
              updatedList <- handleListEvent event $ st ^. availableDecks
              let newState = st & availableDecks .~ updatedList

              case event of
                EvKey KEsc [] -> returnToStart newState
                EvKey KEnter [] -> continue $ newState
                  & view .~ ModeSelect
                _ -> continue newState

            ModeSelect -> do
              updatedList <- handleListEvent event $ st ^. availableModes
              let newState = st
                    & availableModes .~ updatedList
                  selectedMode = snd $ fromJust
                    $ listSelectedElement updatedList

              case event of
                EvKey KEsc [] -> continue $ newState
                  & view .~ DeckSelect
                EvKey KEnter [] -> prepareStandardGame
                    $ newState
                    & matchMode ?~ selectedMode
                    & view .~ Playing
                _ -> continue newState

            GameOver -> case event of
              EvKey KEsc [] -> returnToStart st
              _             -> continue st

            Credits -> case event of
              EvKey KEsc [] -> returnToStart st
              _             -> continue st

          _ -> continue st

        appDraw st = catMaybes
          [ Just $ case st ^. view of
              Playing -> case st ^?! matchMode . _Just of
                Standard -> drawStandardGame st
                Reverse  -> drawReverseGame st

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
                      False
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
                      False
                      $ st ^. activeCardTags
                    ]
                  , borderWithLabel (str "Cards")
                    $ renderList
                    (renderCardOption $ st ^. cardMapEnabled)
                    (st ^. focusX == 2)
                    $ st ^. availableCardsEnabled
                  ]
                , hBox
                  [ hCenter $ str "Switch lists with left/right keys."
                  , hCenter $ str "Select list options with up/down keys."
                  ]
                , hBox
                  [ hCenter $ str "Press F to filter the cards shown."
                  , hCenter $ str "Press Ctrl+F to remove the card filter."
                  ]
                , hBox
                  [ hCenter $ str "Press A to add a new card."
                  , hCenter $ str "Press E to edit a card."
                  ]
                , hBox
                  [ hCenter $ str "Press T to assign the selected tag to a card."
                  , hCenter $ str "Press D to assign the selected deck to a card."
                  ]
                , hBox
                  [ hCenter $ str "Press Ctrl+T to unassign the selected tag from a card."
                  , hCenter $ str "Press Ctrl+D to unassign the selected deck from a card."
                  ]
                , hBox
                  [ hCenter $ str "Press INS to manage disabled cards."
                  , hCenter $ str "Press DEL to disable the selected card."
                  ]
                , hCenter $ str "Press ESC to return."
                ]

              CardsOverviewDisabled -> applicationTitle
                $ vBox
                [ hBorderWithLabel (str "Disabled Cards")
                , hCenter
                  $ vCenter
                  $ borderWithLabel (str "Cards")
                  $ renderList
                  (renderCardOption $ st ^. cardMapDisabled)
                  (st ^. focusX == 2)
                  $ st ^. availableCardsDisabled
                , hCenter $ str "Press ENTER to enable a card."
                , hCenter $ str "Press ESC to return."
                ]

              SetCardFilter -> applicationTitle
                $ vBox
                [ hBorderWithLabel (str "Filter Cards")
                , hCenter $ str "Only cards containing what you enter here will be displayed."
                , hCenter $ str "Filters apply to both the obverse and reverse of cards."
                , hCenter $ border $ renderForm $ st ^. answerForm
                , hCenter $ str "Press ENTER to apply."
                , hCenter $ str "Press ESC to cancel."
                ]

              DecksOverview -> applicationTitle
                $ vBox
                [ hBorderWithLabel (str "Decks")
                , vCenter
                  $ vBox
                  [ hCenter
                    $ renderList (renderDeckOverviewOption $ st ^. deckMap) True
                    $ st ^. availableDecks
                  , hCenter $ str "Press ENTER to make a selection."
                  , hCenter $ str "Press A to add a new deck."
                  , hCenter $ str "Press DEL to delete the selected deck."
                  , hCenter $ str "Press ESC to return."
                  ]
                ]

              TagsOverview -> applicationTitle
                $ vBox
                [ hBorderWithLabel (str "Tags")
                , vCenter
                  $ vBox
                  [ hCenter
                    $ renderList (renderTagOption $ st ^. tagMap) True
                    $ st ^. availableTags
                  , hCenter $ str "Press ENTER to make a selection."
                  , hCenter $ str "Press A to add a new tag."
                  -- , hCenter $ str "Press DEL to delete the selected tag."
                  , hCenter $ str "Press ESC to return."
                  ]
                ]

              CardManagement -> applicationTitle
                $ vBox
                [ hBorderWithLabel (str "Edit Card")
                , vCenter $ renderForm $ st ^. cardForm
                , hCenter $ str "Press TAB to proceed to the next field."
                , hCenter $ str "Press Shift+TAB to return to a previous field."
                , hCenter $ str "Press Ctrl+D when finished."
                ]

              DeckManagement ->
                let selectedDeckId = snd $ fromJust
                      $ listSelectedElement (st ^. availableDecks)
                    selectedDeck = st ^?! deckMap . at (keyToInt selectedDeckId) . _Just
                    selectedDeckName = selectedDeck ^. deckEntity . val . name
                in applicationTitle
                   $ vBox
                   [ hBorderWithLabel (str $ Text.unpack selectedDeckName)
                   , borderWithLabel (str "Cards")
                     $ renderList
                     (renderCardOption $ st ^. cardMapEnabled)
                     (st ^. focusX == 2)
                     $ st ^. availableCardsEnabled
                   , hCenter $ str "Press DEL to disable the selected card."
                   , hCenter $ str "Press E to edit the selected card."
                   , hCenter $ str "Press Ctrl+D to unassign the selected card from this deck."
                   -- TODO: deck form for editing the active deck's
                   --       name and desc
                   ]

              TagManagement ->
                let selectedTagId = snd $ fromJust
                      $ listSelectedElement (st ^. availableTags)
                    selectedTag = st ^?! tagMap . at (keyToInt selectedTagId) . _Just
                    selectedTagName = selectedTag ^. val . name
                in applicationTitle
                   $ vBox
                   [ hBorderWithLabel (str $ Text.unpack selectedTagName)
                   , borderWithLabel (str "Cards")
                     $ renderList
                     (renderCardOption $ st ^. cardMapEnabled)
                     (st ^. focusX == 2)
                     $ st ^. availableCardsEnabled
                   , hCenter $ str "Press DEL to disable the selected card."
                   , hCenter $ str "Press E to edit the selected card."
                   , hCenter $ str "Press Ctrl+T to unassign the selected card from this tag."
                   -- TODO: tag form for editing the active tag's
                   --       name and desc
                   ]

              AddNewCard -> applicationTitle
                $ vBox
                [ hBorderWithLabel (str "Add New Card")
                , vCenter $ renderForm $ st ^. cardForm
                , hCenter $ str "Press TAB to proceed to the next field."
                , hCenter $ str "Press Shift+TAB to return to a previous field."
                , hCenter $ str "Press Ctrl+D when finished."
                ]

              AddNewTag -> applicationTitle
                $ vBox
                [ hBorderWithLabel (str "Add New Tag")
                , vCenter $ border $ renderForm $ st ^. answerForm
                , hCenter $ str "Press ENTER when finished."
                ]

              AddNewDeck -> applicationTitle
                $ vBox
                [ hBorderWithLabel (str "Add New Deck")
                , vCenter $ renderForm $ st ^. deckForm
                , hCenter $ str "Press TAB to proceed to the next field."
                , hCenter $ str "Press Shift+TAB to return to a previous field."
                , hCenter $ str "Press Ctrl+D when finished."
                ]

              DeleteDeckConfirm ->
                let selectedDeckId = snd $ fromJust
                      $ listSelectedElement (st ^. availableDecks)
                    selectedDeck = st ^?! deckMap . at (keyToInt selectedDeckId) . _Just
                    selectedDeckName = selectedDeck ^. deckEntity . val . name
                in applicationTitle
                   $ vCenter
                   $ borderWithLabel (str "Delete Deck")
                   $ vBox
                   [ strWrap
                     ([i|To confirm deletion, write '#{selectedDeckName}' in the box below and press ENTER. This cannot be undone. To cancel, press ESC.|])
                   , hCenter $ border $ renderForm $ st ^. answerForm
                   ]

              Start -> applicationTitle
                $ vBox
                [ hCenter $ str "A flash-cards memorization tool."
                , hBorder
                , hCenter
                  $ vBox
                  [ renderList renderStartOption True $ st ^. startOptions
                  , hCenter $ str "Press ENTER to make a selection."
                  , hCenter $ str "Press Q to quit."
                  ]
                , hBorder
                , copyrightNotice
                ]

              DeckSelect -> applicationTitle
                $ vBox
                [ hBorderWithLabel (str "Select a deck")
                , vCenter
                  $ vBox
                  [ hCenter
                    $ renderList (renderDeckOverviewOption $ st ^. deckMap) True
                    $ st ^. availableDecks
                  , hCenter (str "Press ENTER to make a selection.")
                  , hCenter (str "Press ESC to return.")
                  ]
                ]

              ModeSelect -> applicationTitle
                $ vBox
                [ hBorderWithLabel (str "Select a mode")
                , vCenter
                  $ vBox
                  [ hCenter
                    $ renderList renderModeOption True
                    $ st ^. availableModes
                  ]
                ]

              GameOver -> applicationTitle
                $ vBox
                [ hBorderWithLabel (str "You Win!")
                , vCenter $ vBox
                  [ hCenter $ border $ str " Y O U   W I N ! "
                  , hCenter $ str "Studying is a game where you always win :)"
                  , hCenter $ str "Press ESC to return to main menu."
                  ]
                ]

              Credits -> applicationTitle
                $ vBox
                [ hBorderWithLabel (str "Credits")
                , vCenter copyrightNotice
                ]
          ]

        renderCardOption :: IntMap (Entity Card) -> Bool -> CardId -> Widget a
        renderCardOption cmap _ cardId' =
          let cardObverse = card ^. obverse
              cardReverse = card ^. reverse
              card = cmap ^?! at (keyToInt cardId') . _Just . val
              cardLabel = ([i|#{cardObverse} / #{cardReverse}|])
          in hCenter $ str cardLabel

        renderDeckOption :: IntMap DeckMetadata -> Bool -> DeckId -> Widget a
        renderDeckOption dmap _ deckId' = padRight Max
          $ str $ Text.unpack
          $ dmap ^?! at (keyToInt deckId') . _Just . deckEntity . val . name

        renderDeckOverviewOption :: IntMap DeckMetadata -> Bool -> DeckId -> Widget a
        renderDeckOverviewOption dmap _ deckId' = hBox
          [ padRight Max $ str $ Text.unpack
            $ dmap ^?! at (keyToInt deckId') . _Just . deckEntity . val . name
          , str $ printf "%7d"
            $ dmap ^?! at (keyToInt deckId') . _Just . cardCount
          , padLeft Max $ str
            $ maybe "Never" (formatTime defaultTimeLocale "%_Y-%m-%d %T")
            $ dmap ^? at (keyToInt deckId') . _Just . lastStudied . _Just
          ]
          <=> strWrap (Text.unpack $ dmap ^. at (keyToInt deckId') . _Just . deckEntity . val . description)

        renderModeOption :: Bool -> BabelMode -> Widget a
        renderModeOption _ = hCenter . str . show

        renderTagOption :: IntMap (Entity Tag) -> Bool -> TagId -> Widget a
        renderTagOption tmap _ tagId' = hCenter
          $ str $ Text.unpack
          $ tmap ^?! at (keyToInt tagId') . _Just . val . name

        renderStartOption _ (_, label) =
          hCenter $ str label

        applicationTitle = borderWithLabel (str "BabelCards")

        initialState env = BabelTUI
          { babelTUIBabel = env
          , babelTUIPreviousView = Nothing
          , babelTUIView = Start

          , babelTUIMatchMode = Nothing
          , babelTUIActiveCard = Nothing
          , babelTUIUserAnswer = Nothing
          , babelTUIAnswerDistance = Nothing
          , babelTUIReviewStartTimestamp = Nothing
          , babelTUIReviewEndTimestamp = Nothing

          , babelTUIFocusX = 0
          , babelTUIFocusY = 0

          , babelTUICardMapEnabled = mempty
          , babelTUICardMapDisabled = mempty
          , babelTUIDeckMap = mempty
          , babelTUITagMap  = mempty

          , babelTUIAnswerForm = answerForm'
          , babelTUICardForm = newCardForm'
          , babelTUIDeckForm = deckForm'

          , babelTUIActiveCardDecks = list "activeCardDecks" mempty 1
          , babelTUIActiveCardTags = list "activeCardTags" mempty 1

          , babelTUIAvailableCardsEnabled = list "availableCardsEnabled" mempty 1
          , babelTUIAvailableCardsDisabled = list "availableCardsDisabled" mempty 1
          , babelTUIAvailableDecks = list "availableDecks" mempty 1
          , babelTUIAvailableModes = list "availableModes" mempty 1
          , babelTUIAvailableTags  = list "availableTags"  mempty 1
          , babelTUIStartOptions = list "startOptions"
              (Seq.fromList
               [ (DeckSelect, "Study a Deck")
               , (CardsOverview, "Cards")
               , (DecksOverview, "Decks")
               , (TagsOverview, "Tags")
               , (Credits, "About")
               ])
              1

          , babelTUICardFilter = Nothing
          }

        answerForm' = newForm
          [ editTextField id "userEntry" (Just 1) ]
          mempty

        editCardForm' obverse' reverse' = newForm
          [ (padRight (Pad 1) (str "Obverse:") <+>)
            @@= editTextField obverse "cardObverse" (Just 1)
          , (padRight (Pad 1) (str "Reverse:") <+>)
            @@= editTextField reverse "cardReverse" (Just 1)
          ]
          (CardFormState obverse' reverse' "")

        newCardForm' = newForm
          [ (padRight (Pad 1) (str "Obverse:") <+>)
            @@= editTextField obverse "cardObverse" (Just 1)
          , (padRight (Pad 1) (str "Reverse:") <+>)
            @@= editTextField reverse "cardReverse" (Just 1)
          , (padRight (Pad 4) (str "Tags:") <+>)
            @@= editTextField tags "cardTagList" Nothing
          ]
          (CardFormState "" "" "")

        deckForm' = newForm
          [ (padRight (Pad 8) (str "Name:") <+>)
            @@= editTextField name "deckName" (Just 1)
          , (padRight (Pad 1) (str "Description:") <+>)
            @@= editTextField description "deckDescription" Nothing
          ]
          (Deck "" "")

        -- newDeckMetadata de = DeckMetadata
        --   { deckMetadataDeckEntity = de
        --   , deckMetadataCardCount = 0
        --   , deckMetadataLastStudied = Nothing
        --   }

        -- getSelectedCardId = fmap snd . listSelectedElement

        loadAll = loadModes >=> loadDecks >=> loadTags >=> loadCards

        loadCurrCardMd st = do
          let selectedCardId = snd
                <$> listSelectedElement
                (st ^. availableCardsEnabled)

          maybe (return st) (loadCardMd st) selectedCardId

        loadCardMd st cardId' = do
          (deckIds, tagIds) <- runRIO (st ^. babel) $ runDB $ do
            deckIds <- retrieveCardDecks cardId'
            tagIds  <- retrieveCardTags  cardId'
            return (deckIds, tagIds)

          return $ st
            & activeCardDecks .~ list "activeCardDecks" (Seq.fromList deckIds) 1
            & activeCardTags  .~ list "activeCardTags"  (Seq.fromList tagIds)  1

        loadCards = loadCardsEnabled >=> loadCardsDisabled
        loadCardsDisabled st = do
          availCards <- runRIO (st ^. babel) $ runDB
            $ maybe retrieveCardsDisabled retrieveCardsDisabledFilter
            $ st ^. cardFilter

          let cmap = IntMap.fromList
                $ (\ce -> (keyToInt $ ce ^. key, ce))
                <$> availCards
              cardIds = (^. key) <$> availCards
              oldListSelected = st ^. availableCardsDisabled . listSelectedL
              newCardsList = list "availableCardsDisabled" (Seq.fromList cardIds) 1
                & listSelectedL .~ oldListSelected

          return $ st
            & cardMapDisabled .~ cmap
            & availableCardsDisabled .~ newCardsList

        loadCardsEnabled st = do
          availCards <- runRIO (st ^. babel) $ runDB
            $ maybe retrieveCardsEnabled retrieveCardsEnabledFilter
            $ st ^. cardFilter

          let cmap = IntMap.fromList
                $ (\ce -> (keyToInt $ ce ^. key, ce))
                <$> availCards
              cardIds = (^. key) <$> availCards
              oldListSelected = st ^. availableCardsEnabled . listSelectedL
              newCardsList = list "availableCardsEnabled" (Seq.fromList cardIds) 1
                & listSelectedL .~ oldListSelected

          return $ st
            & cardMapEnabled .~ cmap
            & availableCardsEnabled .~ newCardsList

        loadCardsForDeck st = do
          let selectedDeckId = snd $ fromJust
                $ listSelectedElement (st ^. availableDecks)

          availCards <- runRIO (st ^. babel) $ runDB $ retrieveDeckCards selectedDeckId
          let cmap = IntMap.fromList
                $ (\ce -> (keyToInt $ ce ^. key, ce))
                <$> availCards
              cardIds = (^. key) <$> availCards
              oldListSelected = st ^. availableCardsEnabled . listSelectedL
              newCardsList = list "availableCardsEnabled" (Seq.fromList cardIds) 1
                & listSelectedL .~ oldListSelected

          return $ st
            & cardMapEnabled .~ cmap
            & availableCardsEnabled .~ newCardsList

        loadCardsForTag st = do
          let selectedTagId = snd $ fromJust
                $ listSelectedElement (st ^. availableTags)

          availCards <- runRIO (st ^. babel) $ runDB $ retrieveTagCards selectedTagId
          let cmap = IntMap.fromList
                $ (\ce -> (keyToInt $ ce ^. key, ce))
                <$> availCards
              cardIds = (^. key) <$> availCards
              oldListSelected = st ^. availableCardsEnabled . listSelectedL
              newCardsList = list "availableCardsEnabled" (Seq.fromList cardIds) 1
                & listSelectedL .~ oldListSelected

          return $ st
            & cardMapEnabled .~ cmap
            & availableCardsEnabled .~ newCardsList

        loadDecks st = do
          availDecks <- runRIO (st ^. babel) $ runDB $ retrieveDeckSummaries
          let dmap = IntMap.fromList
                $ (\dm -> (keyToInt $ dm ^. deckEntity . key, dm))
                <$> availDecks
              deckIds = (^. deckEntity . key) <$> availDecks

          return
            $ st
            & deckMap .~ dmap
            & availableDecks .~ list "availableDecks" (Seq.fromList deckIds) 1

        loadModes st = return $ st
          & availableModes .~ list "availableModes" (Seq.fromList [Standard, Reverse]) 1

        loadTags st = do
          availTags <- runRIO (st ^. babel) $ runDB retrieveTags
          let tmap = IntMap.fromList
                $ (\te -> (keyToInt $ te ^. key, te))
                <$> availTags
              tagIds = (^. key) <$> availTags
              oldListSelected = st ^. availableTags . listSelectedL
              newTagsList = list "availableTags" (Seq.fromList tagIds) 1
                & listSelectedL .~ oldListSelected

          return
            $ st
            & tagMap  .~ tmap
            & availableTags  .~ newTagsList

        returnToPreviousView st = do
          let nextView = fromMaybe CardsOverview $ st ^. previousView
              newState = st
                & view .~ nextView
                & previousView .~ Nothing
          case nextView of
            TagManagement -> do
              loadCardsForTag
                (newState & view .~ nextView)
                >>= loadCurrCardMd
                >>= continue
            DeckManagement -> do
              loadCardsForDeck
                (newState & view .~ nextView)
                >>= loadCurrCardMd
                >>= continue
            _ -> do
              loadTags
                (newState & view .~ nextView)
                >>= loadCards
                >>= loadCurrCardMd
                >>= continue

        returnToStart st = loadAll
          (st & view .~ Start)
          >>= continue

        drawReverseGame st =
          let selectedDeckId = snd $ fromJust
                $ listSelectedElement (st ^. availableDecks)
              selectedDeck = st ^?! deckMap . at (keyToInt selectedDeckId) . _Just
              selectedDeckName = selectedDeck ^. deckEntity . val . name
              currentCard = st ^?! activeCard . _Just
          in applicationTitle
              $ vBox
              [ hBorderWithLabel (str [i|Studying: #{selectedDeckName}|])
              , vCenter
                $ vBox
                $ catMaybes
                [ Just $ hCenter $ border $ str $ Text.unpack
                  $ currentCard ^. val . reverse
                , do
                    distance <- st ^. answerDistance
                    guard $ isJust (st ^. userAnswer) && distance == 0
                    return $ hBorderWithLabel (str "Correct!")
                , do
                    distance <- st ^. answerDistance
                    guard $ isJust (st ^. userAnswer) && distance /= 0
                    userAnswer' <- st ^. userAnswer
                    return $ hCenter $ border $ vBox
                      [ str ([i|Your answer: #{userAnswer'}|])
                      , str $ mappend "Card answer: "
                        $ Text.unpack
                        $ currentCard ^. val . obverse
                      , str ([i|Distance: #{distance}|])
                      ]
                , st ^. userAnswer
                  >> return (hCenter $ hBox $ border
                            <$> [ str "0 - Again"
                                , str "1 - Hard"
                                , str "2 - Good"
                                , str "3 - Easy"
                                ])

                , Just $ hCenter $ border
                  $ renderForm $ st ^. answerForm
                ]
              ]

        drawStandardGame st =
          let selectedDeckId = snd $ fromJust
                $ listSelectedElement (st ^. availableDecks)
              selectedDeck = st ^?! deckMap . at (keyToInt selectedDeckId) . _Just
              selectedDeckName = selectedDeck ^. deckEntity . val . name
              currentCard = st ^?! activeCard . _Just
              currentCardTags = toList $ st ^. activeCardTags . listElementsL
              currentCardTagNames = fmap Text.unpack
                . catMaybes
                $ fmap (tagName . entityVal)
                . (IntMap.!?) (st ^. tagMap)
                . keyToInt
                <$> currentCardTags
          in applicationTitle
              $ vBox
              [ hBorderWithLabel (str [i|Studying: #{selectedDeckName}|])
              , vCenter
                $ vBox
                $ catMaybes
                [ Just $ hCenter $ border $ str $ Text.unpack
                  $ currentCard ^. val . obverse
                , do
                    distance <- st ^. answerDistance
                    guard $ isJust (st ^. userAnswer) && distance == 0
                    return $ hBorderWithLabel (str "Correct!")
                , do
                    distance <- st ^. answerDistance
                    guard $ isJust (st ^. userAnswer) && distance /= 0
                    userAnswer' <- st ^. userAnswer
                    return $ hCenter $ border $ vBox
                      [ str ([i|Your answer: #{userAnswer'}|])
                      , str $ mappend "Card answer: "
                        $ Text.unpack
                        $ currentCard ^. val . reverse
                      , str ([i|Distance: #{distance}|])
                      ]
                , st ^. userAnswer
                  >> return (hCenter $ hBox $ border
                            <$> [ str "0 - Again"
                                , str "1 - Hard"
                                , str "2 - Good"
                                , str "3 - Easy"
                                ])

                , Just $ hCenter $ hBox
                  $ border . str <$> currentCardTagNames
                , Just $ hCenter $ border
                  $ renderForm $ st ^. answerForm
                ]
              ]

        playReverseGame st evt event = do
          updatedForm <- handleFormEvent evt $ st ^. answerForm

          let newState = st & answerForm .~ updatedForm
              userInput = formState updatedForm

          case event of
            EvKey KEsc [] -> returnToStart newState
            EvKey KEnter [] -> do
              now <- getCurrentTime

              let currentCard = st ^?! activeCard . _Just
                  cardAnswerText = currentCard ^. val . DB.obverse
                  answerDistance' =
                    restrictedDamerauLevenshteinDistance
                    defaultEditCosts
                    (Text.unpack userInput)
                    (Text.unpack cardAnswerText)
                  newState1 = newState
                    & userAnswer ?~ userInput
                    & answerDistance ?~ answerDistance'
                    & reviewEndTimestamp ?~ now

              continue newState1

            EvKey (KChar keyPressed) [] -> do
              let currentCardId =
                    st ^?! activeCard . _Just . key
                  selectedDeckId = snd $ fromJust
                    $ listSelectedElement (st ^. availableDecks)

              -- omnibus maybe-gating
              result <- runMaybeT $ do
                ease' <- MaybeT $ return $ readMaybe [keyPressed]
                guard $ ease' >= 0 && ease' <= 3

                -- NOTE: checking user answer for more understandable
                -- gating on completed reviews for ease reporting
                _userAnswer <- MaybeT $ return $ st ^. userAnswer
                startTime <- MaybeT $ return $ st ^. reviewStartTimestamp
                endTime <- MaybeT $ return $ st ^. reviewEndTimestamp
                distance <- MaybeT $ return $ st ^. answerDistance

                qiEntity <- MaybeT $ runRIO (newState ^. babel)
                  $ runDB $ getBy
                  $ UniqueQueueCard selectedDeckId currentCardId
                return (toEnum ease', qiEntity, diffUTCTime endTime startTime, distance)

              case result of
                Nothing -> continue newState
                Just (reviewEase, Entity _ qi, duration, distance) -> do
                  runRIO (newState ^. babel) $ runDB $ do
                    logReview selectedDeckId currentCardId
                      distance
                      duration
                      reviewEase
                      (queueItemIndex qi)

                    rescheduleCard selectedDeckId currentCardId
                      (distance == 0)
                      reviewEase

                  prepareStandardGame $ newState
                    & activeCard .~ Nothing
                    & userAnswer .~ Nothing
                    & answerDistance .~ Nothing
                    & reviewEndTimestamp .~ Nothing
                    & reviewStartTimestamp .~ Nothing

            _ -> continue newState

        playStandardGame st evt event = do
          updatedForm <- handleFormEvent evt $ st ^. answerForm

          let newState = st & answerForm .~ updatedForm
              userInput = formState updatedForm

          case event of
            EvKey KEsc [] -> returnToStart newState
            EvKey KEnter [] -> do
              now <- getCurrentTime

              let currentCard = st ^?! activeCard . _Just
                  cardAnswerText = currentCard ^. val . DB.reverse
                  answerDistance' =
                    restrictedDamerauLevenshteinDistance
                    defaultEditCosts
                    (Text.unpack userInput)
                    (Text.unpack cardAnswerText)
                  newState1 = newState
                    & userAnswer ?~ userInput
                    & answerDistance ?~ answerDistance'
                    & reviewEndTimestamp ?~ now

              continue newState1

            EvKey (KChar keyPressed) [] -> do
              let currentCardId =
                    st ^?! activeCard . _Just . key
                  selectedDeckId = snd $ fromJust
                    $ listSelectedElement (st ^. availableDecks)

              -- omnibus maybe-gating
              result <- runMaybeT $ do
                ease' <- MaybeT $ return $ readMaybe [keyPressed]
                guard $ ease' >= 0 && ease' <= 3

                -- NOTE: checking user answer for more understandable
                -- gating on completed reviews for ease reporting
                _userAnswer <- MaybeT $ return $ st ^. userAnswer
                startTime <- MaybeT $ return $ st ^. reviewStartTimestamp
                endTime <- MaybeT $ return $ st ^. reviewEndTimestamp
                distance <- MaybeT $ return $ st ^. answerDistance

                qiEntity <- MaybeT $ runRIO (newState ^. babel)
                  $ runDB $ getBy
                  $ UniqueQueueCard selectedDeckId currentCardId
                return (toEnum ease', qiEntity, diffUTCTime endTime startTime, distance)

              case result of
                Nothing -> continue newState
                Just (reviewEase, Entity _ qi, duration, distance) -> do
                  runRIO (newState ^. babel) $ runDB $ do
                    logReview selectedDeckId currentCardId
                      distance
                      duration
                      reviewEase
                      (queueItemIndex qi)

                    rescheduleCard selectedDeckId currentCardId
                      (distance == 0)
                      reviewEase

                  prepareStandardGame $ newState
                    & activeCard .~ Nothing
                    & userAnswer .~ Nothing
                    & answerDistance .~ Nothing
                    & reviewEndTimestamp .~ Nothing
                    & reviewStartTimestamp .~ Nothing

            _ -> continue newState

        prepareStandardGame st = do
          let selectedDeckId = snd $ fromJust
                $ listSelectedElement (st ^. availableDecks)

          (dueCardsCount, nextCard) <- runRIO (st ^. babel) $ runDB $ do
            dueCardsCount <- retrieveDueCardsCount selectedDeckId
            nextCard <- retrieveNextCard selectedDeckId
            return (dueCardsCount, nextCard)

          now <- getCurrentTime
          newState <- maybe (return st) (loadCardMd st . entityKey) nextCard

          continue $ newState
            & reviewStartTimestamp ?~ now
            & activeCard .~ nextCard
            & answerForm .~ answerForm'
            & view .~ (if isNothing nextCard then GameOver else Playing)

        setCursorBounds st mayMinX mayMaxX mayMinY mayMaxY = do
          let oldFocusX = st ^. focusX
              oldFocusY = st ^. focusY
              newFocusX = fromMaybe oldFocusX
                $ liftA2 max mayMinX
                $ liftA2 min mayMaxX
                $ Just oldFocusX
              newFocusY = fromMaybe oldFocusY
                $ liftA2 max mayMinY
                $ liftA2 min mayMaxY
                $ Just oldFocusY

          return $ st
            & focusX .~ newFocusX
            & focusY .~ newFocusY

        -- dateTimeFormat = iso8601DateFormat (Just "%H:%M:%S")

        copyrightNotice = vBox
          $ hCenter <$>
          [ str "BabelCards v0.1.0"
          , str "Copyright (c) 2021 Saad Rhoulam"
          , str "BabelCards comes with ABSOLUTELY NO WARRANTY."
          , str "BabelCards is distributed under the GPLv3 license."
          , hBorder
          , str "Development sponsored by Rhoulam Technologies LLC"
          , str "https://www.rhoulam.tech"
          ]
