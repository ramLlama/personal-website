--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll
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

    match "static/*/*" $ do
        route   idRoute
        compile copyFileCompiler

    match ("scss/index.scss" .||. "scss/post.scss") $do
        route   $ gsubRoute "scss/" (const "static/css/") `composeRoutes` setExtension "css"
        compile $ getResourceString
            >>= withItemBody (unixFilter "sass" ["-s", "--scss", "--compass", "--style", "compressed"])
            >>= return . fmap compressCss

    -- copy humans.txt and robots.txt to web root
    match (fromList ["humans.txt", "robots.txt"]) $ do
        route   idRoute
        compile copyFileCompiler

    -- Compile static pages to web root with Pandoc
    match "posts/*" $ do
        route $ setExtension ""
        compile $ do
          let baseCtx = constField "stylesheet" "static/css/post.css" `mappend`
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
                           (\_ -> itemList "posts/*" $ fmap (take 3) . recentFirst)

                           `mappend`

                           field
                           "publications"
                           (\_ -> itemList "publications/*" $ fmap (take 3) . recentFirst)

                baseCtx = constField "stylesheet" "static/css/index.css" `mappend`
                          itemCtx

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/base.html" baseCtx
                >>= relativizeUrls


--------------------------------------------------------------------------------
itemCtx :: Context String
itemCtx =
  dateField "date" "%B %e, %Y" `mappend`
  defaultContext


--------------------------------------------------------------------------------
itemList :: Pattern -> ([Item String] -> Compiler [Item String]) -> Compiler String
itemList glob sortFilter = do
    posts   <- sortFilter =<< loadAll glob
    itemTpl <- loadBody "templates/post-item.html"
    list    <- applyTemplateList itemTpl itemCtx posts
    return list
