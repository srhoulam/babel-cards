{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
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
import qualified Data.Map.Strict                  as Map
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

        appHandleEvent st evt = case evt of
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

            CreateTag tag -> do
              _ <- runRIO (st ^. babel) $ runDB $ insert tag
              loadTags st >>= continue

          VtyEvent event -> case st ^. view of
            Playing -> case st ^?! gameState . _Just . mode of
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

            TagsOverview -> case event of
              EvKey KEsc [] -> continue $ st & view .~ Start
              EvKey (KChar 'a') [] -> continue $ st
                & view .~ AddNewTag
                & answerForm .~ answerForm'
              _ -> continue st

            DeckManagement -> case event of
              EvKey KEsc [] -> continue $ st & view .~ DecksOverview
              _             -> continue st

            AddNewCard -> do
              updatedForm <- handleFormEvent evt $ st ^. cardForm
              let newState = st & cardForm .~ updatedForm
              case event of
                EvKey KEsc [] ->
                  continue $ newState & view .~ CardsOverview
                EvKey (KChar 'd') [MCtrl] -> do
                  liftIO $ writeBChan (newState ^. chan) $ CreateCard $ formState updatedForm
                  continue $ newState & view .~ CardsOverview
                _ -> continue newState

            AddNewTag -> do
              updatedForm <- handleFormEvent evt $ st ^. answerForm
              let newState = st & answerForm .~ updatedForm
              case event of
                EvKey KEsc [] -> continue $ newState & view .~ TagsOverview
                EvKey KEnter [] -> do
                  liftIO $ writeBChan (newState ^. chan) $ CreateTag $ Tag $ formState updatedForm
                  continue $ newState & view .~ TagsOverview
                _ -> continue newState

            AddNewDeck -> do
              updatedForm <- handleFormEvent evt $ st ^. deckForm
              let newState = st & deckForm .~ updatedForm
              case event of
                EvKey KEsc [] ->
                  continue $ newState & view .~ DecksOverview
                EvKey (KChar 'd') [MCtrl] -> do
                  liftIO $ writeBChan (newState ^. chan) $ CreateDeck $ formState updatedForm
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

            -- FIXME: reload decks! if the user has added cards, then the loaded deck
            --        metadata is stale!
            DeckSelect -> do
              updatedList <- handleListEvent event $ st ^. availableDecks
              let newState = st & availableDecks .~ updatedList

              case event of
                EvKey KEsc [] -> continue $ newState & view .~ Start
                EvKey KEnter [] -> continue $ newState
                  & view .~ ModeSelect
                _ -> continue newState

            ModeSelect -> do
              updatedList <- handleListEvent event $ st ^. availableModes
              let newState = st & availableModes .~ updatedList
                  selectedMode = fromJust $ snd
                    <$> listSelectedElement updatedList

              case event of
                EvKey KEsc [] -> continue $ newState
                  & view .~ DeckSelect
                EvKey KEnter [] -> case selectedMode of
                  _ -> prepareStandardGame
                    $ newState
                    & view .~ Playing
                    & gameState ?~ GameState
                    { gameStateMode = selectedMode
                    , gameStateCards = mempty
                    , gameStateScore = 0
                    , gameStateDict = mempty
                    , gameStateUserInputStack = mempty
                    , gameStateMessagesStack = mempty
                    }
                _ -> continue newState

            GameOver -> case event of
              EvKey KEsc [] -> continue $ st & view .~ Start
              _             -> continue st

            Credits -> case event of
              EvKey KEsc [] -> continue $ st & view .~ Start
              _             -> continue st

          _ -> continue st

        appDraw st = catMaybes
          [ Just $ case st ^. view of
              Playing -> case st ^?! gameState . _Just . mode of
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

              TagsOverview -> applicationTitle
                $ vBox
                [ hBorderWithLabel (str "Tags")
                , vCenter
                  $ vBox
                  [ hCenter
                    $ renderList (renderTagOption $ st ^. tagMap) True
                    $ st ^. availableTags
                  -- , hCenter (strWrap "Press ENTER to make a selection.")
                  , hCenter (strWrap "Press A to add a new tag.")
                  -- , hCenter (strWrap "Press DEL to delete the selected tag.")
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

              AddNewTag -> applicationTitle
                $ vBox
                [ hBorderWithLabel (str "Add New Tag")
                , vCenter $ border $ renderForm $ st ^. answerForm
                , hCenter $ strWrap "Press ENTER when finished."
                ]

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

              DeckSelect -> applicationTitle
                $ vBox
                [ hBorderWithLabel (str "Select a deck")
                , vCenter
                  $ vBox
                  [ hCenter
                    $ renderList (renderDeckOverviewOption $ st ^. deckMap) True
                    $ st ^. availableDecks
                  , hCenter (strWrap "Press ENTER to make a selection.")
                  , hCenter (strWrap "Press ESC to return.")
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
                  [ hCenter $ border $ strWrap " Y O U   W I N ! "
                  , hCenter (strWrap "Studying is a game where you always win :)")
                  , hCenter (strWrap "Press ESC to return to main menu.")
                  ]
                ]

              Credits -> applicationTitle
                $ vBox
                [ hBorderWithLabel (str "Credits")
                , vCenter
                  $ vBox
                  [ str "Saad Rhoulam <saad@rhoulam.tech>"
                  ]
                ]
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
        renderTagOption tmap _ tagId' = padRight Max
          $ str $ Text.unpack
          $ tmap ^?! at (keyToInt tagId') . _Just . val . name

        renderStartOption _ (_, label) =
          hCenter $ strWrap label

        applicationTitle = borderWithLabel (str "BabelCards")

        initialState env chan' = BabelTUI
          { _babel = env
          , _view = Start
          , _chan = chan'

          , _gameState = Nothing

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
               , (CardsOverview, "Cards")
               , (DecksOverview, "Decks")
               , (TagsOverview, "Tags")
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

        -- TODO: load modes from files, when lua scripting is implemented
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

        drawReverseGame st =
          let selectedDeckId = fromJust $ snd
                <$> listSelectedElement (st ^. availableDecks)
              selectedDeck = st ^?! deckMap . at (keyToInt selectedDeckId) . _Just
              selectedDeckName = selectedDeck ^. deckEntity . val . name
              gameDict = st ^?! gameState . _Just . dict
              currentCard = st ^?! gameState . _Just . cards . each . _Just
          in applicationTitle
              $ vBox
              [ hBorderWithLabel (str [i|Studying: #{selectedDeckName}|])
              , vCenter
                $ vBox
                $ catMaybes
                [ Just $ hCenter $ border $ str $ Text.unpack
                  $ currentCard ^. val . reverse
                , do
                    distance <- gameDict Map.!? "distance"
                                >>= readMaybe . Text.unpack
                    guard (distance == 0)
                    return $ hBorderWithLabel (str "Correct!")
                , do
                    distance <- gameDict Map.!? "distance"
                                >>= readMaybe . Text.unpack
                    guard (distance /= 0)
                    userAnswer <- gameDict Map.!? "userAnswer"
                    return $ hCenter $ border $ vBox
                      [ strWrap ([i|Your answer: #{userAnswer}|])
                      , strWrap $ mappend "Card answer: "
                        $ Text.unpack
                        $ currentCard ^. val . obverse
                      , str ([i|Distance: #{distance}|])
                      ]
                , gameDict Map.!? "userAnswer"
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
          let selectedDeckId = fromJust $ snd
                <$> listSelectedElement (st ^. availableDecks)
              selectedDeck = st ^?! deckMap . at (keyToInt selectedDeckId) . _Just
              selectedDeckName = selectedDeck ^. deckEntity . val . name
              gameDict = st ^?! gameState . _Just . dict
              currentCard = st ^?! gameState . _Just . cards . each . _Just
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
                    distance <- gameDict Map.!? "distance"
                                >>= readMaybe . Text.unpack
                    guard (distance == 0)
                    return $ hBorderWithLabel (str "Correct!")
                , do
                    distance <- gameDict Map.!? "distance"
                                >>= readMaybe . Text.unpack
                    guard (distance /= 0)
                    userAnswer <- gameDict Map.!? "userAnswer"
                    return $ hCenter $ border $ vBox
                      [ strWrap ([i|Your answer: #{userAnswer}|])
                      , strWrap $ mappend "Card answer: "
                        $ Text.unpack
                        $ currentCard ^. val . reverse
                      , str ([i|Distance: #{distance}|])
                      ]
                , gameDict Map.!? "userAnswer"
                  >> return (hCenter $ hBox $ border
                            <$> [ str "0 - Again"
                                , str "1 - Hard"
                                , str "2 - Good"
                                , str "3 - Easy"
                                ])

                , Just $ hCenter $ hBox
                  $ border . strWrap <$> currentCardTagNames
                , Just $ hCenter $ border
                  $ renderForm $ st ^. answerForm
                ]
              ]

        playReverseGame st evt event = do
          updatedForm <- handleFormEvent evt $ st ^. answerForm

          let newState = st & answerForm .~ updatedForm
              userInput = formState updatedForm
              existingUserAnswer = join
                $ newState
                ^? gameState . _Just . dict . at "userAnswer"

          case event of
            EvKey KEsc [] -> continue $ newState & view .~ Start
            EvKey KEnter [] -> case existingUserAnswer of
              Just _ -> continue newState
              Nothing -> do
                now <- getCurrentTime

                let currentCard =
                      st ^?! gameState . _Just . cards . each
                    cardAnswerText =
                      currentCard ^?! _Just . val . DB.obverse
                    answerDistance =
                      restrictedDamerauLevenshteinDistance
                      defaultEditCosts
                      (Text.unpack userInput)
                      (Text.unpack cardAnswerText)
                    newState1 = newState
                      & gameState . _Just . dict . at "cardAnswer"
                      ?~ cardAnswerText
                      & gameState . _Just . dict . at "userAnswer"
                      ?~ userInput
                      & gameState . _Just . dict . at "distance"
                      ?~ Text.pack (show answerDistance)
                      & gameState . _Just . dict . at "endTimestamp"
                      ?~ Text.pack (formatTime defaultTimeLocale dateTimeFormat now)

                continue newState1

            EvKey (KChar keyPressed) [] -> case existingUserAnswer of
              Nothing -> continue newState
              Just _ -> do
                let currentCardId =
                      st ^?! gameState . _Just . cards . each . _Just . key
                    selectedDeckId = fromJust $ snd
                      <$> listSelectedElement (st ^. availableDecks)
                    gameStateDict =
                      st ^?! gameState . _Just . dict

                -- omnibus maybe-gating
                result <- runMaybeT $ do
                  ease' <- MaybeT $ return $ readMaybe [keyPressed]
                  guard $ ease' >= 0 && ease' <= 3

                  startTime <- MaybeT $ return
                    $ (gameStateDict Map.!? "startTimestamp")
                    >>= parseTimeM False defaultTimeLocale dateTimeFormat
                    . Text.unpack
                  endTime <- MaybeT $ return
                    $ (gameStateDict Map.!? "endTimestamp")
                    >>= parseTimeM False defaultTimeLocale dateTimeFormat
                    . Text.unpack

                  distance <- MaybeT $ return
                    $ (gameStateDict Map.!? "distance")
                    >>= readMaybe . Text.unpack

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
                      & gameState . _Just
                      %~ ((cards .~ [])
                          . (dict .~ mempty))
            _ -> continue newState

        playStandardGame st evt event = do
          updatedForm <- handleFormEvent evt $ st ^. answerForm

          let newState = st & answerForm .~ updatedForm
              userInput = formState updatedForm
              existingUserAnswer = join
                $ newState
                ^? gameState . _Just . dict . at "userAnswer"

          case event of
            EvKey KEsc [] -> continue $ newState & view .~ Start
            EvKey KEnter [] -> case existingUserAnswer of
              Just _ -> continue newState
              Nothing -> do
                now <- getCurrentTime

                let currentCard =
                      st ^?! gameState . _Just . cards . each
                    cardAnswerText =
                      currentCard ^?! _Just . val . DB.reverse
                    answerDistance =
                      restrictedDamerauLevenshteinDistance
                      defaultEditCosts
                      (Text.unpack userInput)
                      (Text.unpack cardAnswerText)
                    newState1 = newState
                      & gameState . _Just . dict . at "cardAnswer"
                      ?~ cardAnswerText
                      & gameState . _Just . dict . at "userAnswer"
                      ?~ userInput
                      & gameState . _Just . dict . at "distance"
                      ?~ Text.pack (show answerDistance)
                      & gameState . _Just . dict . at "endTimestamp"
                      ?~ Text.pack (formatTime defaultTimeLocale dateTimeFormat now)

                continue newState1

            EvKey (KChar keyPressed) [] -> case existingUserAnswer of
              Nothing -> continue newState
              Just _ -> do
                let currentCardId =
                      st ^?! gameState . _Just . cards . each . _Just . key
                    selectedDeckId = fromJust $ snd
                      <$> listSelectedElement (st ^. availableDecks)
                    gameStateDict =
                      st ^?! gameState . _Just . dict

                -- omnibus maybe-gating
                result <- runMaybeT $ do
                  ease' <- MaybeT $ return $ readMaybe [keyPressed]
                  guard $ ease' >= 0 && ease' <= 3

                  startTime <- MaybeT $ return
                    $ (gameStateDict Map.!? "startTimestamp")
                    >>= parseTimeM False defaultTimeLocale dateTimeFormat
                    . Text.unpack
                  endTime <- MaybeT $ return
                    $ (gameStateDict Map.!? "endTimestamp")
                    >>= parseTimeM False defaultTimeLocale dateTimeFormat
                    . Text.unpack

                  distance <- MaybeT $ return
                    $ (gameStateDict Map.!? "distance")
                    >>= readMaybe . Text.unpack

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
                      & gameState . _Just
                      %~ ((cards .~ [])
                          . (dict .~ mempty))
            _ -> continue newState

        prepareStandardGame st = do
          let selectedDeckId = fromJust $ snd
                <$> listSelectedElement (st ^. availableDecks)

          (dueCardsCount, nextCard) <- runRIO (st ^. babel) $ runDB $ do
            dueCardsCount <- retrieveDueCardsCount selectedDeckId
            nextCard <- retrieveNextCard selectedDeckId
            return (dueCardsCount, nextCard)

          now <- getCurrentTime
          let timestamp = Text.pack
                $ formatTime defaultTimeLocale dateTimeFormat now

          newState <- maybe (return st) (loadCardMd st . entityKey) nextCard

          continue $ newState
            & gameState . _Just
            %~ ((cards %~ (nextCard:))
                . (score .~ dueCardsCount))
            & gameState . _Just
            %~ (dict %~ Map.insert "startTimestamp" timestamp)
            & view .~ (if isNothing nextCard then GameOver else Playing)
            & answerForm .~ answerForm'

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

        dateTimeFormat = iso8601DateFormat (Just "%H:%M:%S")

        copyrightNotice = vBox
          $ hCenter <$>
          [ strWrap "BabelCards v0.1.0"
          , strWrap "Copyright (c) 2021 Saad Rhoulam"
          , strWrap "BabelCards comes with ABSOLUTELY NO WARRANTY."
          , strWrap "BabelCards is distributed under the GPLv3 license."
          ]
