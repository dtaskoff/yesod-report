{-|
This file is an example for a simple Yesod project. Its purpose
is to show the basic steps for developing a web application with Yesod.

I have intentionally missed the type signatures and some of the code is not
written "the haskell way", but it's all in order to not confuse the readers
not very familiar with the language.
-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns               #-}

import Control.Monad.Trans.Resource (runResourceT)
import Control.Monad.Logger (runStderrLoggingT)
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import Data.Maybe (fromJust)
import Data.Text (Text)
import Yesod


-- |Define our database schema and generate the types 'User' and 'Song'
share [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
    User
      username Text
      password Text
      Username username
    Song
      album  Text
      author Text
      title  Text
      year   Int
      userId UserId Maybe
      Name author title
|]

-- |The foundation type for the application.
-- Notice that here it has the additional field 'ConnectionPool'
-- needed by the persistent layer.
data YesodFM = YesodFM ConnectionPool

-- |Make 'YesodFM' an application with an @sql@ backend.
instance YesodPersist YesodFM where
    type YesodPersistBackend YesodFM = SqlBackend
  
-- |Define 'runDB's behaviour.
    runDB action = do
      YesodFM pool <- getYesod
      runSqlPool action pool

-- |Define the route table.
mkYesod "YesodFM" [parseRoutes|
  /       HomeR    GET
  /login  LoginR   GET POST
  /logout LogoutR  GET
  /signup SignupR  GET POST
  /song   SongR:
    /#UserId     NewSongR  POST
    /#Text/#Text SongInfoR GET
  /songs  SongsR   GET
|]

-- |Determine if there is someone logged in.
-- Returns a tuple with a 'Bool' value and a 'Text' value (wrapped in a Monad,
-- whatever that means) representing the username.
-- Defining it as a separate function since it will be used in many places.
loggedUser = do
  user <- lookupSession "user"
  return $ case user of
    Nothing -> (False, "")
    Just u  -> (True,  u)

-- |Make 'YesodFM' an 'Yesod' application and
-- define a customised'defaultLayout' function.
-- I added that in order to have @YesodFM@ in the title and
-- a navigation menu on every page.
instance Yesod YesodFM where
  defaultLayout widget = do
    (logged, user) <- loggedUser

-- |'widgetToPageContent' gives us a 'PageContent' value containing
-- a title, a head and a body. In this case we will be interested in
-- the body only.
    PageContent _ _ content <- widgetToPageContent widget
    withUrlRenderer [hamlet|
      $doctype 5
      <html>
        <head>
          <title>Yesod FM
        <body>
          <header>
            <nav>
              <ul #menu>
                <li>
                  <a href=@{HomeR}>homepage
                <li>
                  <a href=@{SongsR}>your songs
                $if logged
                  <li>
                    <a href=@{LogoutR}>log out(#{user})
          <div>^{content}
    |]

-- |Make the english translator the default one for the forms used.
instance RenderMessage YesodFM FormMessage where
    renderMessage _ _ = defaultFormMessage

-- |Handle request for '/' with @GET@ method:
-- * display appropriate message if user is logged in
-- * display links to the @login@ and @signup@ pages else.
getHomeR = do
  (logged, _) <- loggedUser

  defaultLayout [whamlet|
    $if logged
      <p>You can go to view your #
        <a href=@{SongsR}>songs
    $else
      <a href=@{LoginR}>login
      \ or
      <a href=@{SignupR}>signup
      \ if you don't have an account
  |]

-- |Define that an user form contains two fields required to be filled -
-- an @username@ and a @password@, both required.
userForm = renderDivs $ User
  <$> areq textField     "username" Nothing
  <*> areq passwordField "password" Nothing

-- |Handle request for '/login' with @GET@ method:
-- * display a login form
getLoginR = do
  (widget, enctype) <- generateFormPost userForm
  defaultLayout [whamlet|
    <p>Login to your account
      <form method=post action=@{LoginR} enctype=#{enctype}>
        ^{widget}
        <button>login
  |]

-- |Handle request for '/login' with @POST@ method:
-- * on login failure display appropriate message
-- * on success, set the 'user' for the session
postLoginR = do
  ((result, _), _) <- runFormPost userForm
  case result of
    FormSuccess (User username password) -> do
      user <- runDB $ getBy $ Username username
      case user of
        Nothing -> defaultLayout [whamlet|
          <p>Such user doesn't exist!
        |]
        Just (Entity _ (User _ pw)) ->
          if password /= pw
          then defaultLayout [whamlet|
            <p>Wrong user/password combination!
          |]
          else do
            setSession "user" username
            defaultLayout [whamlet|
              <p>You have successfully logged in as #{username}
              <p>Now you can go to your #
                <a href=@{SongsR}>songs
                page
            |]
    _ -> defaultLayout [whamlet|
      <p>Login failed!
    |]

-- |Handle request for '/logout' with @GET@ method:
-- * delete the 'user' from the session
getLogoutR = do
  deleteSession "user"
  defaultLayout [whamlet|
    <p>You have successfully logged out. We'll miss you!
  |]

-- |Handle request for '/signup' with @GET@ method:
-- * display a signup form 
getSignupR = do
  (widget, enctype) <- generateFormPost userForm
  defaultLayout [whamlet|
    <p>Signup to get started
      <form method=post action=@{SignupR} enctype=#{enctype}>
        ^{widget}
        <button>signup
  |]

-- |Handle request for '/signup' with @GET@ method:
-- * on registration failure display appropriate message
-- * on success register the username/password combination in the database
-- and set the 'user' for the session
postSignupR = do
  ((result, _), _) <- runFormPost userForm
  case result of
    FormSuccess (User username password) -> do
      user <- runDB $ getBy $ Username username
      case user of
        Just _ -> defaultLayout [whamlet|
          <p>That username has already been taken!
        |]
        Nothing -> do
          setSession "user" username
          runDB $ insert $ User username password
          defaultLayout [whamlet|
            <p>You have successfully registered as #{username}
            <p>Now go on and add some #
              <a href=@{SongsR}>songs
              !
          |]
    _ -> defaultLayout [whamlet|
      <p>Registration failed!
    |]

-- |Define that an song form contains four fields required to be filled -
-- the song's @album@, @author@, @title@ and @year@, the last one an 'intField'.
-- There is also an optional fifth field, which is hidden. Its purpose is just
-- to create a 'Song' constructor with five values, the last one is the 'Maybe userId',
-- a key for the user who added the song.
songForm = renderTable $ Song
  <$> areq textField   "album"  Nothing
  <*> areq textField   "author" Nothing
  <*> areq textField   "title"  Nothing
  <*> areq intField    "year"   Nothing
  <*> aopt hiddenField ""       Nothing

-- |Handle request for '/song' with @POST@ method:
-- * inserting the new song in the database
postNewSongR userId = do
  ((result, _), _) <- runFormPost songForm
  case result of
    FormSuccess (Song album author title year _) -> do
      runDB $ insert $ Song album author title year $ Just userId
      defaultLayout [whamlet|
        <p>You have successfully added #{title} by #{author} from #{album} #{year}
      |]
    _ -> defaultLayout [whamlet|
      <p>Adding song failed!
    |]

-- |Handle request for '/song' with @GET@ method:
-- * get the 'Song' contents from the database and display it
getSongInfoR author title = do
  Just (Entity _ (Song album author title year _)) <- runDB $ getBy $ Name author title 
  defaultLayout [whamlet|
    <p>#{title} by #{author} is from the album #{album} released in #{year}
  |]

-- |Handle request for '/songs' with @GET@ method:
-- * get all the songs added by the current user
getSongsR = do
  (logged, user) <- loggedUser
  userId <- fmap (\(Entity userId _) -> userId) <$> (runDB . getBy . Username) user 
-- |Select all songs belonging to the user with id 'userId'
  songs <- runDB $ selectList [SongUserId ==. userId] []

  (widget, enctype) <- generateFormPost songForm
  defaultLayout [whamlet|
    $if logged
      $if null songs
        <p>Your song list is currently empty.
      $else
        <p>Your added songs:
          <ul #song-list>
            $forall Entity songid (Song _ author title _ _) <- songs
              <li>
                <a href=@{SongR $ SongInfoR author title}>#{author} - #{title}
      <p>Add a new song:
        <form method=post action=@{SongR $ NewSongR $ fromJust userId} enctype=#{enctype}>
          ^{widget}
          <button>add song
    $else
      <a href=@{LoginR}>login
      \ or
      <a href=@{SignupR}>signup
      \ if you don't have an account
  |]


-- |Running the 'YesodFM' application with @warp@ on port 3000 using the @sqlite@ backend.
main = runStderrLoggingT $ withSqlitePool "songs.db3" 10 $ \pool -> liftIO $ do
  runResourceT $ runSqlPool (runMigration migrateAll) pool
  warp 3000 $ YesodFM pool
