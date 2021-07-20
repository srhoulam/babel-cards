{-# LANGUAGE NoImplicitPrelude #-}
module Application.TUI where

import           Application.Database
import           Brick
import           Brick.Main
import           Brick.Widgets.Border       (borderWithLabel, hBorderWithLabel)
import           Brick.Widgets.Border.Style (unicode)
import           Brick.Widgets.Center       (hCenter)
import           Brick.Widgets.Dialog       (dialog)
import           Brick.Widgets.Edit         (Editor, editor, handleEditorEvent,
                                             renderEditor)
import           Brick.Widgets.List         (GenericList, handleListEvent,
                                             listSelectedElement, renderList)
import           Graphics.Vty.Attributes    (defAttr)
import           Graphics.Vty.Input.Events

-- TODO
