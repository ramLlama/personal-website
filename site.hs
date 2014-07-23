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

    match (fromRegex "^scss/[^_\\./][^\\./]*\\.scss$") $ do
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

    -- Create posts
    match "posts/**.adoc" $ version "html" $ do
      route $ setExtension "html"
      compile $ do
        let baseCtx = constField "stylesheet" "/static/css/post.css" `mappend`
                      itemCtx
        getResourceString
          >>= withItemBody (unixFilter "asciidoctor" ["--out-file", "-", "-T", "./asciidoctor-backends/html5-contentonly", "--attribute", "stylesheet!", "-"])
          >>= loadAndApplyTemplate "templates/post.html"    itemCtx
          >>= loadAndApplyTemplate "templates/base.html" baseCtx
          >>= relativizeUrls

    match "posts/**.adoc" $ version "adoc" $ do
      route idRoute
      compile copyFileCompiler

    -- Copy over static assets of post
    match ("posts/**" .&&. (complement (fromRegex "posts/.*\\.metadata"))) $ do
      route idRoute
      compile copyFileCompiler

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

                baseCtx = constField "isIndex" "<SHOULDN'T BE DISPLAYED!>" `mappend`
                          constField "stylesheet" "/static/css/index.css" `mappend`
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
              field "items" (\_ -> postList recentFirst) `mappend`
              defaultContext

            baseCtx =
              constField "title" "Posts" `mappend`
              constField "description" "Previous posts on cs.cmu.edu/~rraghuna" `mappend`
              constField "stylesheet" "/static/css/post_index.css" `mappend`
              defaultContext
        makeItem ""
          >>= loadAndApplyTemplate "templates/item_index.html" postIndexCtx
          >>= loadAndApplyTemplate "templates/base.html" baseCtx
          >>= relativizeUrls
          >>= removeIndexHtml

--------------------------------------------------------------------------------
itemCtx :: Context String
itemCtx =
  dateField "date" "%B %e, %Y" `mappend`
  defaultContext


--------------------------------------------------------------------------------
itemList :: Pattern -> Identifier -> ([Item String] -> Compiler [Item String]) -> Compiler String
itemList glob template sortFilter = do
    posts   <- sortFilter =<< loadAll (glob .&&. hasVersion "html")
    itemTpl <- loadBody template
    list    <- applyTemplateList itemTpl itemCtx posts
    return list

postList :: ([Item String] -> Compiler [Item String]) -> Compiler String
postList sortFilter = itemList "posts/**/index.adoc" "templates/post_listing.html" sortFilter

publicationList :: ([Item String] -> Compiler [Item String]) -> Compiler String
publicationList sortFilter = itemList "publications/*" "templates/publication_listing.html" sortFilter

--------------------------------------------------------------------------------
removeIndexHtml :: Item String -> Compiler (Item String)
removeIndexHtml item = return $ fmap cuttail item
  where
        cuttail = withUrls $ replaceAll "/index.html" (const "/")
