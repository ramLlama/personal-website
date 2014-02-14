--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Data.Time
import           Data.Time.Clock
import           Data.Time.Format
import           Hakyll
import           System.Locale
import qualified Text.Pandoc.Options as Pandoc.Options

--------------------------------------------------------------------------------
config :: Configuration
config = defaultConfiguration
        {   deployCommand = "./deploy.sh"}

pandocWriterOptions :: Pandoc.Options.WriterOptions
pandocWriterOptions = defaultHakyllWriterOptions
                        { Pandoc.Options.writerHtml5 = True
                        , Pandoc.Options.writerHtmlQTags = True
                        --, Pandoc.Options.writerNumberSections = True
                        --, Pandoc.Options.writerNumberOffset = [1]
                        , Pandoc.Options.writerSectionDivs = True
                        , Pandoc.Options.writerTableOfContents = True
                    }

--------------------------------------------------------------------------------
main :: IO ()
main = hakyllWith config $ do

    match "templates/*" $ compile templateCompiler

    -- copy site icon to `favicon.ico`
    match "static/favicon.ico" $ do
        route   (constRoute "favicon.ico")
        compile copyFileCompiler

    -- copy static files
    match "static/**" $ do
        route   idRoute
        compile copyFileCompiler

    -- copy js
    match "js/**" $ do
      route idRoute
      compile copyFileCompiler

    match (fromRegex "^scss/[^_][^\\.]*\\.scss$") $ do
        route   $ gsubRoute "scss/" (const "static/css/") `composeRoutes` setExtension "css"
        compile $ getResourceString
            >>= withItemBody (unixFilter "sass" ["-s", "--scss", "--compass", "--style", "compressed"])
            >>= return . fmap compressCss

    -- Generate humans.txt
    match "humans.txt" $ do
        route   idRoute
        compile $ do
          let todayCtx = field "today" (\_ -> unsafeCompiler (getCurrentTime >>= (return . (formatTime defaultTimeLocale "%F"))))
          getResourceString
            >>= applyAsTemplate todayCtx

    -- Compile static pages to web root with Pandoc
    match "posts/*" $ do
        route $ setExtension ""
        compile $ do
          let baseCtx = constField "stylesheet" "/static/css/post.css" `mappend`
                        itemCtx
          pandocCompilerWith defaultHakyllReaderOptions pandocWriterOptions
            >>= loadAndApplyTemplate "templates/post.html"    itemCtx
            >>= loadAndApplyTemplate "templates/base.html" baseCtx
            >>= relativizeUrls

    match "index.html" $ do
        route idRoute
        compile $ do
            let indexCtx = field
                           "posts"
                           (\_ -> postList $ fmap (take 3) . recentFirst)

                           `mappend`

                           field
                           "publications"
                           (\_ -> publicationList $ fmap (take 3) . recentFirst)

                baseCtx = constField "stylesheet" "/static/css/index.css" `mappend`
                          itemCtx

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/base.html" baseCtx
                >>= relativizeUrls


    match "about.html" $ do
        route idRoute
        compile $ do
            let baseCtx = constField "stylesheet" "/static/css/about.css" `mappend`
                          itemCtx

            getResourceBody
                >>= loadAndApplyTemplate "templates/base.html" baseCtx
                >>= relativizeUrls

    match "colophon.html" $ do
        route idRoute
        compile $ do
            let baseCtx = constField "stylesheet" "/static/css/colophon.css" `mappend`
                          itemCtx

            getResourceBody
                >>= loadAndApplyTemplate "templates/base.html" baseCtx
                >>= relativizeUrls

    match "contact.html" $ do
        route idRoute
        compile $ do
            let baseCtx = constField "stylesheet" "/static/css/contact.css" `mappend`
                          itemCtx

            getResourceBody
                >>= loadAndApplyTemplate "templates/base.html" baseCtx
                >>= relativizeUrls

    -- Create post and publication indices
    create ["posts-index"] $ do
      route (constRoute "posts/index.html")
      compile $ do
        let postIndexCtx =
              constField "page-title" "Posts" `mappend`
              -- field "items" (\_ -> postList recentFirst) `mappend`
              constField "items" "Nothing yet!" `mappend`
              defaultContext

            baseCtx =
              constField "title" "Posts" `mappend`
              constField "description" "Previous posts on cs.cmu.edu/~rraghuna" `mappend`
              constField "stylesheet" "/static/css/item_index.css" `mappend`
              defaultContext
        makeItem ""
          >>= loadAndApplyTemplate "templates/item_index.html" postIndexCtx
          >>= loadAndApplyTemplate "templates/base.html" baseCtx
          >>= relativizeUrls

    create ["publications-index"] $ do
      route (constRoute "publications/index.html")
      compile $ do
        let publicationIndexCtx =
              constField "page-title" "Publications" `mappend`
              -- field "items" (\_ -> publicationList recentFirst) `mappend`
              constField "items" "Nothing yet!" `mappend`
              defaultContext

            baseCtx =
              constField "title" "Publications" `mappend`
              constField "description" "Publications findable on cs.cmu.edu/~rraghuna" `mappend`
              constField "stylesheet" "/static/css/item_index.css" `mappend`
              defaultContext
        makeItem ""
          >>= loadAndApplyTemplate "templates/item_index.html" publicationIndexCtx
          >>= loadAndApplyTemplate "templates/base.html" baseCtx
          >>= relativizeUrls

    match "miscellaneous/index_items.html" $ do
      route (constRoute "miscellaneous/index.html")
      compile $ do
        let miscellaneousIndexCtx =
              constField "page-title" "Miscellaneous" `mappend`
              bodyField "items" `mappend`
              defaultContext

            baseCtx =
              constField "title" "Miscellaneous" `mappend`
              constField "description" "Miscellaneous items on cs.cmu.edu/~rraghuna" `mappend`
              constField "stylesheet" "/static/css/item_index.css" `mappend`
              defaultContext
        getResourceBody
          >>= loadAndApplyTemplate "templates/item_index.html" miscellaneousIndexCtx
          >>= loadAndApplyTemplate "templates/base.html" baseCtx
          >>= relativizeUrls


--------------------------------------------------------------------------------
itemCtx :: Context String
itemCtx =
  dateField "date" "%B %e, %Y" `mappend`
  defaultContext


--------------------------------------------------------------------------------
itemList :: Pattern -> Identifier -> ([Item String] -> Compiler [Item String]) -> Compiler String
itemList glob template sortFilter = do
    posts   <- sortFilter =<< loadAll glob
    itemTpl <- loadBody template
    list    <- applyTemplateList itemTpl itemCtx posts
    return list

postList :: ([Item String] -> Compiler [Item String]) -> Compiler String
postList sortFilter = itemList "posts/*" "templates/post_listing.html" sortFilter

publicationList :: ([Item String] -> Compiler [Item String]) -> Compiler String
publicationList sortFilter = itemList "publications/*" "templates/publication_listing.html" sortFilter
