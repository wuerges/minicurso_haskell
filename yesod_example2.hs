{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
import Yesod

data HelloWorld = HelloWorld

mkYesod "HelloWorld" [parseRoutes|
/ HomeR GET
/page1 Page1R GET
/page2 Page2R GET
|]

instance Yesod HelloWorld

getHomeR :: Handler Html
getHomeR = defaultLayout [whamlet|Hello World! <a href=@{Page1R}>Page 1|]

getPage1R = defaultLayout [whamlet|<a href=@{Page2R}>Page 2|]

getPage2R = defaultLayout [whamlet|<a href=@{HomeR}>Go home|]

main :: IO ()
main = warp 3000 HelloWorld

