{-# LANGUAGE OverloadedStrings #-}
module Network.Scraper.State (
  module Text.XML.Cursor,
  get,
  runScraper,
  runScraperDebug,
  InpFilter (..),
  FormAttr(..),
  Scraper,
  getCurrentCursor,
  toCursor,
  isDisplayed,
  hasDisplayNone,
  getFormBy,
  getCurrentHtml,
  fillForm,
  hasHide,
  getInputs,
  postToForm,
  printFormNames,
  toAbsUrl
  ) where

import           Control.Applicative
import           Control.Arrow                    ((***), first)
import           Control.Lens                     ((^.), (^?))
import           Control.Monad
import           Control.Monad.IO.Class           (liftIO)
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import qualified Control.Monad.Trans.State.Strict as ST
import qualified Data.ByteString.Lazy             as LBS
import qualified Data.Map                         as M
import           Data.Maybe                       (fromJust, fromMaybe, isJust,
                                                   listToMaybe)
import           Data.Monoid
import qualified Data.Text                        as T
import           Data.Text.Encoding               (encodeUtf8, decodeUtf8)
import qualified Data.Text.IO                     as TIO
import           Network.URL
import           Network.Wreq                     (FormParam (..))
import qualified Network.Wreq                     as Wreq
import           Network.Wreq.Session             (Session (..), withSession)
import qualified Network.Wreq.Session             as Sesh
import           Network.Wreq.Types
import           Safe
import           Text.HTML.DOM                    (parseLBS)
import qualified Text.XML                         as XML
import           Text.XML.Cursor
import qualified Text.XML.Cursor.Generic          as CG
import Data.Maybe
import Conversion
import Conversion.Text
import HTMLEntities.Decoder

data ScraperState =
  PS { currentOptions :: Wreq.Options
     , currentHtml    :: LBS.ByteString
     , currentCursor  :: Maybe Cursor
     , currentSession :: Session
     , currentURL     :: Maybe URL
     , currentDebug   :: Bool
     } deriving (Show)

type Scraper a = ExceptT String (ST.StateT ScraperState IO) a

toCursor = fromDocument . parseLBS

withInitialState :: (ScraperState -> IO a) -> IO a
withInitialState callback = withSession $ \s -> do
  let initialState = PS { currentOptions = Wreq.defaults
                        , currentHtml = ("" :: LBS.ByteString)
                        , currentCursor = Nothing
                        , currentSession = s
                        , currentURL = Nothing
                        , currentDebug = False
                        }
  callback initialState

-- TODO: Figure out how to get rid of this
withInitialDbgState :: (ScraperState -> IO a) -> IO a
withInitialDbgState callback = withSession $ \s -> do
  let initialState = PS { currentOptions = Wreq.defaults
                        , currentHtml = ("" :: LBS.ByteString)
                        , currentCursor = Nothing
                        , currentSession = s
                        , currentURL = Nothing
                        , currentDebug = True
                        }
  callback initialState

runScraper :: Scraper a -> IO (Either String a)
runScraper k = withInitialState (evalScraperWith k)

runScraperDebug :: Scraper a -> IO (Either String a)
runScraperDebug k = withInitialDbgState (evalScraperWith k)

evalScraperWith
  :: Monad m => ExceptT e (ST.StateT s m) a -> s -> m (Either e a)
evalScraperWith k s = (ST.evalStateT (runExceptT k) s)

-- TODO: Move somewhere else???
setCurrentOptions :: Wreq.Options -> Scraper ()
setCurrentOptions o = do
   scraper <- lift ST.get
   lift $ ST.put $ scraper { currentOptions = o }
   return ()

setCurrentURL :: Maybe URL -> Scraper ()
setCurrentURL u = lift $ ST.get >>= (\s -> ST.put $ s { currentURL = u })

getCurrentURL :: Scraper(Maybe URL)
getCurrentURL = lift $ ST.get >>= return . currentURL

getCurrentHtml :: Scraper(LBS.ByteString)
getCurrentHtml = lift $ ST.get >>= return . currentHtml

getCurrentDebug :: Scraper (Bool)
getCurrentDebug = lift $ ST.get >>= return . currentDebug

-- TODO: Move somewhere else???
-- getCurrentPage :: Shpider Page
getCurrentCursor :: Scraper (Maybe Cursor)
getCurrentCursor = do
   scraper <- lift ST.get
   return $ currentCursor scraper

-- TODO: Move somewhere else???
getCurrentSession :: Scraper (Session)
getCurrentSession = do
   scraper <- lift ST.get
   return $ currentSession scraper

-- TODO: Move somewhere else???
setCurrentSession :: Session -> Scraper ()
setCurrentSession s = do
   scraper <- lift ST.get
   lift $ ST.put $ scraper { currentSession = s}

-- TODO: Move somewhere else???
setCurrentCursor :: Cursor -> Scraper ( )
setCurrentCursor c = do
   scraper <- lift ST.get
   lift $ ST.put $ scraper { currentCursor = Just c }

-- TODO: Move somewhere else???
setCurrentHtml :: LBS.ByteString -> Scraper ()
setCurrentHtml html = do
   scraper <- lift ST.get
   lift $ ST.put $ scraper { currentHtml = html }

-- TODO: Move somewhere else???
formShortInfo' f = formInfo'
  where
    go Nothing = "N/A"
    go (Just x) = x
    formInfo = (headMay . attribute "name" $ f, headMay . attribute "action" $ f)
    formInfo' = (\(x,y) -> (go x, go y)) formInfo

-- TODO: Move somewhere else...
ppTuple :: (T.Text, T.Text) -> T.Text
ppTuple = \(x,y) -> "[" <> x <> "]" <> ": " <> y

-- TODO: Move somewhere else...
printFormNames :: Scraper ()
printFormNames = do
  c <- getCurrentCursor
  case c of
    Just c' -> do
      let forms = c' $// element "form"
          formInfo = map (ppTuple . formShortInfo') forms
      liftIO $ mapM_ (TIO.putStrLn) formInfo
    Nothing -> throwE "No cursor set"

-- TODO: Move somewhere else???
get :: String -> Scraper (LBS.ByteString)
get urlStr = do
  case (importURL urlStr) of
    Just url -> do
      let urlStr' = exportURL url

      whenM getCurrentDebug . liftIO $ do
        liftIO . putStrLn $ "GET: " <> urlStr' <> "\n"

      opts <- lift $ ST.gets currentOptions
      sesh <- lift $ ST.gets currentSession

      r <- liftIO $ Sesh.getWith opts sesh urlStr'
      let html = r ^. Wreq.responseBody
      setCurrentUrlAfterRequest url r
      setCurrentHtml html
      setCurrentCursor (toCursor html)
      return html
    Nothing -> throwE ("invalid urlStr: " ++ urlStr)

-- TODO: Move somewhere else???
post :: Postable a => String -> a -> Scraper (LBS.ByteString)
post urlStr params = do
  case (importURL urlStr) of
    Just url -> do
        opts <- lift $ ST.gets currentOptions
        sesh <- lift $ ST.gets currentSession
        -- TODO: should take an actual url.. makes api more difficult...
        -- TODO: Make url absolute by storing host of current site
        -- TODO: Display under debug mode
        -- liftIO . print $ params
        -- TODO: Make minimal repro and ask question... not sure how to print something so polymorphic
        absURL <- toAbsUrl url
        let url' = exportURL absURL
        whenM getCurrentDebug . liftIO $ do
          liftIO . putStrLn $ "POST: " ++ url' ++ "\n"

        r <- liftIO $ Sesh.postWith opts sesh url' params
        -- TODO: On non 200 status look for element with "error" and print text
        let status = r ^. Wreq.responseStatus
        whenM getCurrentDebug . liftIO $ do
          liftIO $ TIO.putStrLn $ (T.pack "post responseStatus:  ") <> T.pack (show status)
        let html = r ^. Wreq.responseBody
        setCurrentHtml html
        setCurrentCursor (toCursor html)
        setCurrentUrlAfterRequest absURL r

        return html

    Nothing -> throwE ("invalid urlStr: " ++ urlStr)

setCurrentUrlAfterRequest :: URL -> Wreq.Response body -> Scraper ()
setCurrentUrlAfterRequest absURL r = do
  let currentUrl = maybe absURL id $ toURL =<< r ^? Wreq.responseHeader "X-Wreq-Final-Request-URI"
      toURL      = importURL . T.unpack . decodeUtf8
  setCurrentURL $ Just currentUrl

toAbsUrl :: URL -> Scraper(URL)
toAbsUrl u@(URL (Absolute _) _ _) = return u
toAbsUrl u@(URL HostRelative _ _) = do
  hostUrl <- getCurrentURL
  case hostUrl of
    Just hostUrl' -> do
      let absUrl = u { url_type = url_type hostUrl' }
      return absUrl
    Nothing -> throwE errMsg
    where errMsg = "You must 'get' or 'post' to something before making urls absolute"
toAbsUrl u@(URL PathRelative _ _) = do
  hostUrl <- getCurrentURL
  maybe (throwE errMsg) (return . relativeTo) hostUrl
  where
    relativeTo hostUrl | null (url_path u) = hostUrl
    relativeTo hostUrl | otherwise         = u { url_type = url_type hostUrl, url_path = base hostUrl ++ "/" ++ url_path u }

    base = reverse . dropWhile (== '/') . dropWhile (/= '/') . reverse . url_path
    errMsg = "You must 'get' or 'post' to something before making urls absolute"

-- TODO: Move to tests
testToAbsUrl :: Scraper()
testToAbsUrl = do
  setCurrentURL (importURL "http://www.google.com")
  aUrl <- toAbsUrl (fromJust . importURL $ "blah.php")
  liftIO . print . exportURL $ aUrl

hasDisplayNone el = fromMaybe False . fmap (== "display: none;") . headMay $ (attribute "style" el)
hasHide el = fromMaybe False . fmap (T.isInfixOf "hide") . headMay $ (attribute "class" el)

anyParentIsHidden el = isJust . listToMaybe . join $ map (\c -> (c $/ check (hasHide))) cs
  where cs = ancestor el

-- checks to see if an eleemnt is displayed
-- isDisplayed el  = any (== True) $ [displayNone el, hide el]
isDisplayed :: Cursor -> Bool
isDisplayed el = all (== False) $
                 [ hasDisplayNone el
                 , hasHide el
                 , anyParentIsHidden el -- this was hiding inputs on the paypal billing form
                 ]

-- TODO: Move somewhere else???
 -- let aForm = toCursor "<form><input name=\"NOOO\" style=\"display: none;\"><input name=\"YES\"></form>"
getVisibleInputs :: Cursor -> M.Map T.Text T.Text
getVisibleInputs  c = do
  let inputs' = filter isDisplayed inputs
      mayPairs = map (\e -> (listToMaybe $ attribute "name" e, listToMaybe $ attribute "value" e)) inputs'
      pairs = map (fromMaybe "" *** fromMaybe "") mayPairs
  M.fromList $ filter ((/= "") . fst) pairs
  where inputs = c $// element "input"

getReasonableInputs :: Cursor -> M.Map T.Text T.Text
getReasonableInputs  c = do
  let mayPairs = map (\e -> (listToMaybe $ attribute "name" e,
                            listToMaybe $ attribute "value" e,
                            listToMaybe $ attribute "type" e,
                            listToMaybe $ attribute "checked" e)) inputs
      pairs = mapMaybe f mayPairs
      f (_, _, Just "submit", _)          = Nothing
      f (_, _, Just "radio", Nothing)     = Nothing
      f (_, _, Just "checkbox", Nothing)  = Nothing
      f (Just "", _, _, _)                = Nothing
      f (Just n, Just v, _, _)            = Just (n, convert $ htmlEncodedText v :: T.Text)
      f _                                 = Nothing
  M.fromList pairs
  where inputs = c $// element "input"

data InpFilter a = Custom ([T.Text]) | AllVisible | AllInps deriving (Show)

getInputs :: InpFilter a -> Cursor -> M.Map T.Text T.Text
getInputs (Custom paramFilterList) c = do
  let mayPairs = map (\e -> (listToMaybe $ attribute "name" e, listToMaybe $ attribute "value" e)) inputs
      pairs = map (fromMaybe "" *** fromMaybe "") mayPairs
      m = M.fromList $ filter ((/= "") . fst) pairs
      -- filter out keys user didn't want
  M.filterWithKey (\k _ ->  not . any (== k) $ paramFilterList) m
  where inputs = c $// element "input"
getInputs AllVisible c  = getVisibleInputs c
getInputs AllInps c  = getReasonableInputs c

getAllInputs :: Cursor -> M.Map T.Text T.Text
getAllInputs  c = do
  let mayPairs = map (\e -> (listToMaybe $ attribute "name" e, listToMaybe $ attribute "value" e)) inputs
      pairs = map (fromMaybe "" *** fromMaybe "") mayPairs
  M.fromList $ filter ((/= "") . fst) pairs
  where inputs = c $// element "input"

-- test get inputs
-- todo: Move to tests
tgi = do
  LBS.readFile "mismatchedinputkeyvalsform.html" >>= return . getAllInputs . toCursor

-- TODO: Move somewhere else???
getLoginForm url = get url >>= return . getAllInputs . toCursor

-- TODO: Move somewhere else???
toWreqFormParams :: [(T.Text, T.Text)] -> [FormParam]
toWreqFormParams = map ((uncurry (:=)) . first encodeUtf8)

-- TODO: Move somewhere else???
linkWithText :: T.Text -> Cursor -> Maybe Cursor
linkWithText t cursor = listToMaybe $ filter (\c -> (any (T.isInfixOf t)) (c $/ content)) (cursor $// element "a")

-- TODO: Move somewhere else???
addToMap pairs m = foldl (\m ->(\(k,v) -> M.insert k v m)) m pairs

-- TODO: Move somewhere else???
getFormByName :: T.Text -> Scraper (Maybe Cursor)
getFormByName name = do
  c <- getCurrentCursor
  case c of
    Nothing -> throwE "No cursor set"
    Just c' -> do
      let formList = c' $// element "form" >=> attributeIs "name" name
      return . listToMaybe $ formList

-- TODO: Move somewhere else???

data FormAttr = Name T.Text | ActionUrl T.Text | FormId T.Text deriving Show

-- TODO: Move somewhere else???
getFormBy :: FormAttr -> Scraper (Maybe Cursor)
getFormBy formAttr = do
  c <- getCurrentCursor
  case c of
    Nothing -> throwE "No cursor set"
    Just c' ->
      return . listToMaybe  $ formList formAttr c'
      where formList (Name val) c =
              c $// element "form" >=> attributeIs "name" val
            formList (ActionUrl val) c =
              c $// element "form" >=> attributeIs "action" val
            formList (FormId val) c =
              c $// element "form" >=> attributeIs "id" val

fillForm :: Maybe Cursor -> Maybe [(T.Text, T.Text)] -> InpFilter a -> [FormParam]
fillForm form Nothing paramFilterList = do
  -- liftIO . putStrLn $ "old inputs: " ++ show (map (attribute "class") $ filter (not . isDisplayed) form)
  case (getInputs paramFilterList <$> form) of
    -- TODO: Should I change this to throwE?
    -- Nothing -> throwE "no params in form"
    Nothing -> error "no params in form"
    Just formParams -> toWreqFormParams . M.toList $ formParams
fillForm form (Just params) paramFilterList = do
  -- putStrLn $ "filling form: " ++ show form
  case (getInputs paramFilterList <$> form) of
    -- TODO: Should I change this to throwE?
    -- Nothing -> throwE "no params in form"
    Nothing -> error "no params in form"
    Just formParams -> do
      let formParams' = addToMap params formParams
      toWreqFormParams . M.toList $ formParams'

whenM :: (Monad m) => m Bool -> m () -> m ()
whenM b x = b >>= flip when x

-- TODO: Move somewhere else???
-- Takes a form name, fields to fill out in the form, then submits the form
-- TODO: Change all[ (T.Text,T.Text)] to just be wreq formvalues... neater api anyway
postToForm :: FormAttr -> Maybe [(T.Text,T.Text)] -> InpFilter a -> Scraper (LBS.ByteString)
postToForm formAttr params paramFilterList = do
  form <- getFormBy formAttr
  c <- getCurrentCursor
  case form of
   Just _ -> do
     whenM getCurrentDebug . liftIO $ do
       putStrLn $ "Found form: " ++ show formAttr
     return ()
   Nothing -> do
     let forms = (fromJust c) $// element "form"
         getFormInfo form = ( fromMaybe "" . listToMaybe $ attribute "name" form
                            , fromMaybe "" . listToMaybe $ attribute "id" form
                            , fromMaybe "" . listToMaybe $ attribute "action" form
                            )
         formsInfo = map getFormInfo forms

     whenM getCurrentDebug . liftIO $ do
       liftIO $ do
         putStrLn "Forms Found"
         forM_ formsInfo $ \formInfo -> do
           -- TODO: print something out like this
           -- <form name id action>
           --   <input name value> -- attrs of forms
           -- </form>
           let (fName, fId, fAction) = formInfo
           TIO.putStrLn $ "=================================================="
           TIO.putStrLn $ "name: " <> fName
           TIO.putStrLn $ "id: " <> fId
           TIO.putStrLn $ "action: " <> fAction
           TIO.putStrLn $ "=================================================="

     throwE ("Couldn't find form: " ++ show formAttr)

  let formParams = fillForm form params paramFilterList
      -- todo, this is duplicated above... delete one or the other if possible
      mActionUrl = T.strip <$> (join $ listToMaybe <$> attribute "action" <$> form)

  case mActionUrl of
    Nothing -> throwE "Couldn't find action url in form"
    Just actionUrl -> do
      whenM getCurrentDebug . liftIO $ do
        TIO.putStrLn $ "POST " <> actionUrl
        print formParams

        -- getCurrentHtml >>= liftIO . LBS.writeFile "last.html"

      html <- post (T.unpack actionUrl) formParams
      return html
