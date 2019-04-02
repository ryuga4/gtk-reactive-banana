{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeFamilies #-}

module Main where
import Lib
import Reactive.Banana
import Reactive.Banana.GI.Gtk
import Reactive.Banana.Frameworks
import Data.Semigroup
import Control.Exception (catch)
import Data.Text (Text)
import qualified Data.Text as T

import Data.Maybe (fromJust)
import qualified GI.Gtk as Gtk
import GI.Gtk
    ( mainQuit
    , builderAddFromFile
    , builderNew
    , get
    , Window(..)
    , Stack(..)
    , Button(..)
    , Label(..)
    , GError(..)
    , gerrorMessage
    )
import GI.Gdk.Structs.EventKey (EventKey(..))


showT :: Show a => a -> Text
showT = T.pack . show

networkDescription :: MomentIO ()
networkDescription = do
    b <- builderNew
    builderAddFromFile b "calculator.glade"

    window <- castB b "window" Window
    destroyE <- signalE0 window #destroy
    reactimate $ mainQuit <$ destroyE
 
    button1 <- castB b "1" Button
    pressed1E <- signalE0 button1 #clicked

 
    button2 <- castB b "2" Button
    pressed2E <- signalE0 button2 #clicked

 
    button3 <- castB b "3" Button
    pressed3E <- signalE0 button3 #clicked

 
    button4 <- castB b "4" Button
    pressed4E <- signalE0 button4 #clicked


    button5 <- castB b "5" Button
    pressed5E <- signalE0 button5 #clicked


    button6 <- castB b "6" Button
    pressed6E <- signalE0 button6 #clicked


    button7 <- castB b "7" Button
    pressed7E <- signalE0 button7 #clicked


    button8 <- castB b "8" Button
    pressed8E <- signalE0 button8 #clicked


    button9 <- castB b "9" Button
    pressed9E <- signalE0 button9 #clicked

    button0 <- castB b "0" Button
    pressed0E <- signalE0 button0 #clicked


    buttonDot <- castB b "." Button
    pressedDotE <- signalE0 buttonDot #clicked


    buttonEq <- castB b "=" Button
    pressedEqE <- signalE0 buttonEq #clicked


    buttonAdd <- castB b "+" Button
    pressedAddE <- signalE0 buttonAdd #clicked


    buttonSub <- castB b "-" Button
    pressedSubE <- signalE0 buttonSub #clicked


    buttonMul <- castB b "*" Button
    pressedMulE <- signalE0 buttonMul #clicked


    buttonDiv <- castB b "/" Button
    pressedDivE <- signalE0 buttonDiv #clicked


    buttonDel <- castB b "delete" Button
    pressedDelE <- signalE0 buttonDel #clicked


    buttonClear <- castB b "clear" Button
    pressedClearE <- signalE0 buttonClear #clicked


    buttonPow <- castB b "^" Button
    pressedPowE <- signalE0 buttonPow #clicked

    result <- castB b "result" Label
    resultB <- accumB initialState $ unions
               [ eval (Character '0') <$ pressed0E
               , eval (Character '1') <$ pressed1E
               , eval (Character '2') <$ pressed2E
               , eval (Character '3') <$ pressed3E
               , eval (Character '4') <$ pressed4E
               , eval (Character '5') <$ pressed5E
               , eval (Character '6') <$ pressed6E
               , eval (Character '7') <$ pressed7E
               , eval (Character '8') <$ pressed8E
               , eval (Character '9') <$ pressed9E
               , eval (Character '0') <$ pressed0E
               , eval Add <$ pressedAddE
               , eval Sub <$ pressedSubE
               , eval Mul <$ pressedMulE
               , eval Div <$ pressedDivE
               , eval Pow <$ pressedPowE
               , eval Delete <$ pressedDelE
               , eval Clear <$ pressedClearE
               , eval Eq <$ pressedEqE
               ]

    sink result [#label :== showValue <$> resultB]
    #showAll window




leftmost :: Alternative a => [a b] -> a b
leftmost = foldl (<|>) empty

runGtk = do
    Gtk.init Nothing
    compile networkDescription >>= actuate
    Gtk.main

main :: IO ()
main = runGtk `catch` (\(e::GError) -> gerrorMessage e >>= putStrLn . T.unpack)
