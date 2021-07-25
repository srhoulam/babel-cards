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
import           Control.Monad.Trans.Maybe
import qualified Data.IntMap.Strict               as IntMap
import           Data.Maybe                       (fromJust)
import qualified Data.Sequence                    as Seq (filter, fromList,
                                                          singleton, (|>))
import           Data.String.Interpolate.IsString
import qualified Data.Text                        as Text (unpack)
import qualified Graphics.Vty                     (defaultConfig, mkVty)
import           Graphics.Vty.Attributes
import           Graphics.Vty.Input.Events
import           Lens.Micro.Platform              hiding (view)
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
  initialVty <- liftIO buildVty
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

        appStartEvent = loadModes >=> loadDecks >=> loadTags >=> loadCards

        appHandleEvent st evt = do
          case evt of
            AppEvent appEvent -> case appEvent of
              AssignCardDeck   deckId' cardId' -> do
                runRIO (st ^. babel) $ runDB $ assignCardDeck deckId' cardId'
                loadCards st >>= loadTags >>= loadCurrCardMd >>= continue

              UnassignCardDeck deckId' cardId' -> do
                runRIO (st ^. babel) $ runDB $ unassignCardDeck deckId' cardId'
                loadCards st >>= loadTags >>= loadCurrCardMd >>= continue

              AssignCardTag    tagId'  cardId' -> do
                runRIO (st ^. babel) $ runDB $ assignCardTag tagId' cardId'
                loadCards st >>= loadTags >>= loadCurrCardMd >>= continue

              UnassignCardTag  tagId'  cardId' -> do
                runRIO (st ^. babel) $ runDB $ unassignCardTag tagId' cardId'
                loadCards st >>= loadTags >>= loadCurrCardMd >>= continue

              CreateCard newCard -> do
                _ <- runRIO (st ^. babel) $ runDB $ createCard newCard
                loadCards st >>= loadTags >>= loadCurrCardMd >>= continue

              DisableCard cardId' -> do
                runRIO (st ^. babel) $ runDB $ update cardId' [ CardEnabled =. False ]
                loadCards st >>= loadTags >>= loadCurrCardMd >>= continue

              EnableCard cardId' -> do
                runRIO (st ^. babel) $ runDB $ update cardId' [ CardEnabled =. True ]
                loadCards st >>= loadTags >>= continue

              CreateDeck deck -> do
                _ <- runRIO (st ^. babel) $ runDB $ insert deck
                loadDecks st >>= continue

              DeleteDeck deckId' -> do
                runRIO (st ^. babel) $ runDB $ do
                  deleteWhere [ QueueItemDeckId ==. deckId']
                  deleteWhere [ DeckMemberDeckId ==. deckId' ]
                  delete deckId'
                loadDecks st >>= continue

            VtyEvent event -> case st ^. view of
              Playing -> error "playing"

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
                  EvKey KEsc [] -> continue $ newState & view .~ Start
                  EvKey KLeft [] -> continue $ newState
                    & focusX %~ max 0 . (\x -> x - 1)
                  EvKey KRight [] -> continue $ newState
                    & focusX %~ min 2 . (1+)
                  EvKey KIns [] ->
                    continue $ newState & view .~ CardsOverviewDisabled
                  EvKey KDel [] -> do
                    _ <- liftIO $ runMaybeT $ do
                      scid <- MaybeT $ return selectedCardId
                      lift $ writeBChan (st2 ^. chan) $ DisableCard scid
                    continue newState
                  EvKey (KChar 'a') [] -> continue $ newState
                    & view .~ AddNewCard
                    & cardForm .~ cardForm'
                  EvKey (KChar 'd') [] -> do
                    _ <- liftIO $ runMaybeT $ do
                      sdid <- MaybeT $ return selectedDeckId
                      scid <- MaybeT $ return selectedCardId
                      lift $ writeBChan (st2 ^. chan) $ AssignCardDeck sdid scid
                    continue newState
                  EvKey (KChar 'd') [MCtrl] -> do
                    _ <- liftIO $ runMaybeT $ do
                      sdid <- MaybeT $ return selectedDeckId
                      scid <- MaybeT $ return selectedCardId
                      lift $ writeBChan (st2 ^. chan) $ UnassignCardDeck sdid scid
                    continue newState
                  EvKey (KChar 't') [] -> do
                    _ <- liftIO $ runMaybeT $ do
                      stid <- MaybeT $ return selectedTagId
                      scid <- MaybeT $ return selectedCardId
                      lift $ writeBChan (st2 ^. chan) $ AssignCardTag stid scid
                    continue newState
                  EvKey (KChar 't') [MCtrl] -> do
                    _ <- liftIO $ runMaybeT $ do
                      stid <- MaybeT $ return selectedTagId
                      scid <- MaybeT $ return selectedCardId
                      lift $ writeBChan (st2 ^. chan) $ UnassignCardTag stid scid
                    continue newState
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
                      lift $ writeBChan (newState ^. chan) $ EnableCard scid
                    continue newState
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

              AddNewCard -> do
                updatedForm <- handleFormEvent evt $ st ^. cardForm
                let newState = st & cardForm .~ updatedForm
                case event of
                  EvKey KEsc [] ->
                    continue $ newState & view .~ CardsOverview
                  EvKey (KChar 'd') [MCtrl] -> do
                    liftIO $ writeBChan (st ^. chan) $ CreateCard $ formState updatedForm
                    continue $ newState & view .~ CardsOverview
                  _ -> continue newState

              AddNewTag -> error "add new tag"

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
                      (True, deckId') -> do
                        liftIO $ writeBChan (st ^. chan)
                          $ DeleteDeck deckId'
                        continue $ newState
                          & view .~ DecksOverview
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

              DeckSelect -> continue st

              ModeSelect -> continue st

              Credits -> case event of
                EvKey KEsc [] -> continue $ st & view .~ Start
                _ -> continue st

            _ -> continue st

        appDraw st = catMaybes
          [ Just $ case st ^. view of
              Playing ->
                error "playing"

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
                , hCenter $ strWrap "Switch lists with left/right keys."
                , hCenter $ strWrap "Select list options with up/down keys."
                , hCenter $ strWrap "Press A to add a new card."
                , hCenter $ strWrap "Press T to assign the selected tag to a card."
                , hCenter $ strWrap "Press D to assign the selected deck to a card."
                , hCenter $ strWrap "Press Ctrl+T to unassign the selected tag from a card."
                , hCenter $ strWrap "Press Ctrl+D to unassign the selected deck from a card."
                , hCenter $ strWrap "Press INS to manage disabled cards."
                , hCenter $ strWrap "Press DEL to disable the selected card."
                , hCenter $ strWrap "Press ESC to return."
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
                , hCenter $ strWrap "Press ENTER to enable a card."
                , hCenter $ strWrap "Press ESC to return."
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

              AddNewCard -> applicationTitle
                $ vBox
                [ hBorderWithLabel (str "Add New Card")
                , vCenter $ renderForm $ st ^. cardForm
                , hCenter $ strWrap "Press TAB to proceed to the next field."
                , hCenter $ strWrap "Press Shift+TAB to return to a previous field."
                , hCenter $ strWrap "Press Ctrl+D when finished."
                ]

              AddNewTag -> error "add new tag"

              AddNewDeck -> applicationTitle
                $ vBox
                [ hBorderWithLabel (str "Add New Deck")
                , vCenter $ renderForm $ st ^. deckForm
                , hCenter $ strWrap "Press TAB to proceed to the next field."
                , hCenter $ strWrap "Press Shift+TAB to return to a previous field."
                , hCenter $ strWrap "Press Ctrl+D when finished."
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

              Credits -> error "Credits" -- TODO
          ]

        renderCardOption :: IntMap (Entity Card) -> Bool -> CardId -> Widget a
        renderCardOption cmap _ cardId' =
          let cardObverse = card ^. obverse
              cardReverse = card ^. reverse
              card = cmap ^?! at (keyToInt cardId') . _Just . val
              cardLabel = ([i|#{cardObverse} / #{cardReverse}|])
          in padRight Max
             $ str cardLabel

        renderDeckOption :: IntMap DeckMetadata -> Bool -> DeckId -> Widget a
        renderDeckOption dmap _ deckId' = padRight Max
          $ str $ Text.unpack
          $ dmap ^?! at (keyToInt deckId') . _Just . deckEntity . val . name

        renderTagOption :: IntMap (Entity Tag) -> Bool -> TagId -> Widget a
        renderTagOption tmap _ tagId' = padRight Max
          $ str $ Text.unpack
          $ tmap ^?! at (keyToInt tagId') . _Just . val . name

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

        renderStartOption _ (_, label) =
          hCenter $ strWrap label

        applicationTitle = borderWithLabel (str "BabelCards")

        initialState env chan = BabelTUI
          { _babel = env
          , _view = Start
          , _chan = chan

          , _focusX = 0
          , _focusY = 0

          , _cardMapEnabled = mempty
          , _cardMapDisabled = mempty
          , _deckMap = mempty
          , _tagMap  = mempty

          , _answerForm = answerForm'
          , _cardForm = cardForm'
          , _deckForm = deckForm'

          , _activeCardDecks = list "activeCardDecks" mempty 1
          , _activeCardTags = list "activeCardTags" mempty 1

          , _availableCardsEnabled = list "availableCardsEnabled" mempty 1
          , _availableCardsDisabled = list "availableCardsDisabled" mempty 1
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

        -- getSelectedCardId = fmap snd . listSelectedElement

        loadCurrCardMd st = do
          let selectedCardId = fmap snd
                $ listSelectedElement
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
          availCards <- runRIO (st ^. babel) $ runDB retrieveCardsDisabled
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
          availCards <- runRIO (st ^. babel) $ runDB retrieveCardsEnabled
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

        loadModes = return
          -- TODO: load modes, when lua scripting is implemented

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

        copyrightNotice = vBox
          $ hCenter <$>
          [ strWrap "BabelCards v0.1.0"
          , strWrap "Copyright (c) 2021 Saad Rhoulam"
          , strWrap "BabelCards comes with ABSOLUTELY NO WARRANTY."
          , strWrap "BabelCards is distributed under the GPLv2 license."
          ]
