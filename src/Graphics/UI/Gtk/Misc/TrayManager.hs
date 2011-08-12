{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}
-- | This module implements a TrayManager - an integral part of a
-- Linux system tray widget, though it is not itself a widget.  This
-- package exports a single GObject (for use with gtk2hs) that
-- implements the freedesktop.org system tray specification (it
-- handles receiving events and translating them into convenient
-- signals, along with the messy work of dealing with XEMBED).
--
-- The basic usage of the object is to:
--
-- 1. Instantiate the object with 'trayManagerNew'
--
-- 2. Have it manage a specific screen with 'trayManagerManageScreen'
--
-- 3. Set up handlers for the events exposed by the tray (e.g., 'trayIconAdded').
--
-- As an example, a functional system tray widget looks something like:
--
-- > import Graphics.UI.Gtk
-- > import Graphics.UI.Gtk.Misc.TrayManager
-- > systrayNew = do
-- >   box <- hBoxNew False 5
-- >   trayManager <- rayManagerNew
-- >   Just screen <- screenGetDefault
-- >   trayManagerManageScreen trayManager screen
-- >   on trayManager trayIconAdded $ \w -> do
-- >     widgetShowAll w
-- >     boxPackStart box w PackNatural 0
--
-- Note that the widgets made available in the event handlers are not
-- shown by default; you need to explicitly show them if you want that
-- (and you probably do).
module Graphics.UI.Gtk.Misc.TrayManager (
  -- * Types
  TrayManager,
  TrayManagerChild,
  TrayManagerClass,
  castToTrayManager,
  toTrayManager,
  gTypeTrayManager,

  -- * Functions
  trayManagerCheckRunning,
  trayManagerNew,
  trayManagerManageScreen,
  trayManagerGetChildTitle,

  -- * Signals
  trayIconAdded,
  trayIconRemoved,
  trayMessageSent,
  trayMessageCanceled,
  trayLostSelection
  ) where

import Graphics.UI.Gtk hiding ( after )
import Graphics.UI.Gtk.Abstract.Object ( makeNewObject )
import Graphics.UI.GtkInternals
import System.Glib.GError ( failOnGError )
import System.Glib.GObject
import System.Glib.GType
import System.Glib.UTFString ( peekUTFString )

import Control.Monad ( liftM )

import Foreign
import Foreign.C.String ( CString, peekCString )
import Foreign.C.Types
import Unsafe.Coerce ( unsafeCoerce )

newtype TrayManager = TrayManager (ForeignPtr TrayManager)
                    deriving (Eq, Ord)
type TrayManagerChild = Ptr EggTrayManagerChild

-- Empty data tags to classify some foreign pointers
data EggTrayManager
data EggTrayManagerChild

foreign import ccall "egg_tray_manager_check_running"
  c_egg_tray_manager_check_running :: Ptr Screen -> IO CInt

trayManagerCheckRunning :: Screen -> IO Bool
trayManagerCheckRunning gdkScreen = do
  let ptrScreen = unsafeCoerce gdkScreen :: ForeignPtr Screen
  withForeignPtr ptrScreen $ \realPtr -> do
    res <- c_egg_tray_manager_check_running realPtr
    return (res /= 0)

mkTrayManager = (TrayManager, objectUnrefFromMainloop)

unTrayManager :: TrayManager -> ForeignPtr TrayManager
unTrayManager (TrayManager o) = o

class GObjectClass o => TrayManagerClass o
toTrayManager :: TrayManagerClass o => o -> TrayManager
toTrayManager = unsafeCastGObject . toGObject

instance TrayManagerClass TrayManager
instance ObjectClass TrayManager
instance GObjectClass TrayManager where
  toGObject = GObject . castForeignPtr . unTrayManager
  unsafeCastGObject = TrayManager . castForeignPtr . unGObject

castToTrayManager :: GObjectClass o => o -> TrayManager
castToTrayManager = castTo gTypeTrayManager "TrayManager"

foreign import ccall "egg_tray_manager_get_type"
  c_egg_tray_manager_get_type :: CULong

gTypeTrayManager :: GType
gTypeTrayManager = fromIntegral c_egg_tray_manager_get_type

foreign import ccall "egg_tray_manager_new"
  c_egg_tray_manager_new :: IO (Ptr EggTrayManager)

trayManagerNew :: IO TrayManager
trayManagerNew =
  makeNewObject mkTrayManager $ liftM (castPtr :: Ptr EggTrayManager -> Ptr TrayManager) $
    c_egg_tray_manager_new
  -- tm <- c_egg_tray_manager_new
  -- return (unsafeCoerce tm)

foreign import ccall "egg_tray_manager_manage_screen"
  c_egg_tray_manager_manage_screen :: Ptr EggTrayManager -> Ptr Screen -> IO CInt

trayManagerManageScreen :: TrayManager -> Screen -> IO Bool
trayManagerManageScreen trayManager screen = do
  let ptrManager = unsafeCoerce trayManager :: ForeignPtr EggTrayManager
      ptrScreen = unsafeCoerce screen :: ForeignPtr Screen
  res <- withForeignPtr ptrManager $ \realManager -> do
    withForeignPtr ptrScreen $ \realScreen -> do
      c_egg_tray_manager_manage_screen realManager realScreen
  return (res /= 0)

foreign import ccall "egg_tray_manager_get_child_title"
  c_egg_tray_manager_get_child_title :: Ptr EggTrayManager -> Ptr EggTrayManagerChild -> IO (Ptr CChar)


trayManagerGetChildTitle :: TrayManager -> TrayManagerChild -> IO String
trayManagerGetChildTitle trayManager child = do
  let ptrManager = unsafeCoerce trayManager :: ForeignPtr EggTrayManager
  res <- withForeignPtr ptrManager $ \realManager -> do
    c_egg_tray_manager_get_child_title realManager child
  peekCString res

-- | The signal emitted when a new tray icon is added.  These are
-- delivered even for systray icons that already exist when the tray
-- manager is created.
trayIconAdded :: (TrayManagerClass self) => Signal self (Widget -> IO ())
trayIconAdded = Signal (connect_OBJECT__NONE "tray_icon_added")

-- | This signal is emitted when a tray icon is removed by its parent
-- application.  No action is really necessary here (the icon is
-- removed without any intervention).  You could do something here if
-- you wanted, though.
trayIconRemoved :: (TrayManagerClass self) => Signal self (Widget -> IO ())
trayIconRemoved = Signal (connect_OBJECT__NONE "tray_icon_removed")

-- | This signal is emitted when the application that displayed an
-- icon wants a semi-persistent notification displayed for its icon.
-- The standard doesn't seem to require that these be honored.
trayMessageSent :: (TrayManagerClass self) => Signal self (Widget -> String -> Int64 -> Int64 -> IO ())
trayMessageSent = Signal (connect_OBJECT_STRING_INT64_INT64__NONE "message_sent")

-- | Similarly, the applciation can send this to cancel a previous
-- persistent message.
trayMessageCanceled :: (TrayManagerClass self) => Signal self (Widget -> Int64 -> IO ())
trayMessageCanceled = Signal (connect_OBJECT_INT64__NONE "message_canceled")

-- | ??
trayLostSelection :: (TrayManagerClass self) => Signal self (IO ())
trayLostSelection = Signal (connect_NONE__NONE "lost_selection")


-- Boilerplate stolen from gtk to make this library compatible.  These
-- functions aren't exported at all so I just copied them.

-- stolen from gtk
castTo :: (GObjectClass obj, GObjectClass obj') => GType -> String -> (obj -> obj')
castTo gtype objTypeName obj =
  case toGObject obj of
    gobj@(GObject objFPtr)
      | typeInstanceIsA ((unsafeForeignPtrToPtr.castForeignPtr) objFPtr) gtype
                  -> unsafeCastGObject gobj
      | otherwise -> error $ "Cannot cast object to " ++ objTypeName

connect_OBJECT__NONE ::
  (GObjectClass a', GObjectClass obj) => SignalName ->
  ConnectAfter -> obj ->
  (a' -> IO ()) ->
  IO (ConnectId obj)
connect_OBJECT__NONE signal after obj user =
  connectGeneric signal after obj action
  where action :: Ptr GObject -> Ptr GObject -> IO ()
        action _ obj1 =
          failOnGError $
          makeNewGObject (GObject, objectUnrefFromMainloop) (return obj1) >>= \obj1' ->
          user (unsafeCastGObject obj1')

connect_NONE__NONE ::
  GObjectClass obj => SignalName ->
  ConnectAfter -> obj ->
  (IO ()) ->
  IO (ConnectId obj)
connect_NONE__NONE signal after obj user =
  connectGeneric signal after obj action
  where action :: Ptr GObject -> IO ()
        action _ =
          failOnGError $
          user

connect_OBJECT_INT64__NONE :: (GObjectClass a', GObjectClass obj)
                              => SignalName
                              -> ConnectAfter
                              -> obj
                              -> (a' -> Int64 -> IO ())
                              -> IO (ConnectId obj)
connect_OBJECT_INT64__NONE signal after obj user =
  connectGeneric signal after obj action
  where
    action :: Ptr GObject -> Ptr GObject -> Int64 -> IO ()
    action _ obj1 int2 =
      failOnGError $ makeNewGObject (GObject, objectUnrefFromMainloop) (return obj1) >>= \obj1' ->
        user (unsafeCastGObject obj1') int2

connect_OBJECT_STRING_INT64_INT64__NONE :: (GObjectClass a', GObjectClass obj)
                                           => SignalName
                                           -> ConnectAfter
                                           -> obj
                                           -> (a' -> String -> Int64 -> Int64 -> IO ())
                                           -> IO (ConnectId obj)
connect_OBJECT_STRING_INT64_INT64__NONE signal after obj user =
  connectGeneric signal after obj action
  where
    action :: Ptr GObject -> Ptr GObject -> CString -> Int64 -> Int64 -> IO ()
    action _ obj1 str2 int3 int4 =
      failOnGError $ makeNewGObject (GObject, objectUnrefFromMainloop) (return obj1) >>= \obj1' ->
        peekUTFString str2 >>= \str2' ->
          user (unsafeCastGObject obj1') str2' int3 int4
