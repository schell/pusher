module Html.Url where

import Text.Blaze
import Data.String
import Data.List

-- | These are all the urls our RESTish server supports. Type-safe urls are
-- cool.
data Url = UrlHome
         | UrlUploadFile
         | UrlUploadTarball
         | UrlCopyFile
         | UrlCopyFolder
         | UrlBucketList
         | UrlBucketAdd
         | UrlBucketLinkCF
         | UrlUserSettings
         | UrlUserPassword
         | UrlUserAdd
         | UrlUserLogin
         | UrlUserLogout
         | UrlLogView
         | UrlTask
         | UrlUsersFile
         | UrlUsers
         | UrlUnknown
         deriving (Eq, Ord, Enum)

type Search = [(String, String)]

-- | TODO: Add the rest of uri elements (hash?)
data Uri = Uri Url Search

instance Show Uri where
    show (Uri url s) = concat [url'', "?", s'']
        where url'' = "/" ++ intercalate "/" url'
              url' = foldl (\ps (k,v) -> replace ps (':':k, v)) (paths url) s
              s'' = intercalate "&" $ map (\(k,v) -> concat [k,"=",v]) s'
              s' = foldl (\ss p@(k,_) -> if elem k rs then ss else p:ss) [] s
              rs = resourceNames url

instance ToValue Uri where
    toValue = toValue . show

-- | Takes a Url like "/task/:task/update/:num" and returns a list of resource names,
-- in this case [ "task", "num" ].
resourceNames :: Url -> [String]
resourceNames = foldl take_ [] . paths
    where take_ ns (':':str) = str:ns
          take_ ns _ = ns

replace :: Eq a => [a] -> (a, a) -> [a]
replace as (f, t) = map (\a -> if a == f then t else a) as

paths :: Url -> [String]
paths = reverse . split [] . show
    where split ns [] = ns
          split ns ('/':str) = let n = takeWhile (/= '/') str
                               in split (n:ns) $ dropWhile (/= '/') str
          split ns _ = ns

instance Show Url where
    show UrlHome          = "/"

    show UrlUploadFile    = "/upload/file"
    show UrlUploadTarball = "/upload/tarball"

    show UrlCopyFile      = "/copy/file"
    show UrlCopyFolder    = "/copy/folder"

    show UrlBucketList    = "/bucket/list"
    show UrlBucketAdd     = "/bucket/add"
    show UrlBucketLinkCF  = "/bucket/cloudfront"

    show UrlUserSettings  = "/user/settings"
    show UrlUserPassword  = "/user/password"
    show UrlUserAdd       = "/user/add"
    show UrlUserLogin     = "/user/login"
    show UrlUserLogout    = "/user/logout"

    show UrlLogView       = "/log/view"

    show UrlTask          = "/task/:task"

    show UrlUsers         = "/users"
    show UrlUsersFile     = "/users/file"

    show UrlUnknown       = "/404"

allUrls :: [(String, Url)]
allUrls = zip (map show urls) urls
    where urls = [UrlHome .. UrlUnknown]

instance Read Url where
    readsPrec _ url = maybe [] (\u -> [(u, "")]) $ lookup url allUrls

instance IsString Url where
    fromString url = maybe UrlUnknown id $ lookup url allUrls

instance ToValue Url where
    toValue = toValue . show
