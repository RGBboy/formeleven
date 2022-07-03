module SiteMap exposing (generate)

import Html as H exposing (Attribute, Html)
import Html.Attributes as A

generate : List String -> Html msg
generate pages =
  let
    urls = List.map siteMapURL pages
  in
    H.node "urlset" [ A.attribute "xmlns" "http://www.sitemaps.org/schemas/sitemap/0.9" ] urls

siteMapURL : String -> Html msg
siteMapURL location =
  H.node "url" []
    [ H.node "loc" [] [ H.text ("https://www.formeleven.com" ++ location) ] ]
