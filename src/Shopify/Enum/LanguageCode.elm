-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Shopify.Enum.LanguageCode exposing (..)

import Json.Decode as Decode exposing (Decoder)


{-| ISO 639-1 language codes supported by Shopify.

  - Af - Afrikaans.
  - Ak - Akan.
  - Am - Amharic.
  - Ar - Arabic.
  - As - Assamese.
  - Az - Azerbaijani.
  - Be - Belarusian.
  - Bg - Bulgarian.
  - Bm - Bambara.
  - Bn - Bangla.
  - Bo - Tibetan.
  - Br - Breton.
  - Bs - Bosnian.
  - Ca - Catalan.
  - Ce - Chechen.
  - Cs - Czech.
  - Cu - Church Slavic.
  - Cy - Welsh.
  - Da - Danish.
  - De - German.
  - Dz - Dzongkha.
  - Ee - Ewe.
  - El - Greek.
  - En - English.
  - Eo - Esperanto.
  - Es - Spanish.
  - Et - Estonian.
  - Eu - Basque.
  - Fa - Persian.
  - Ff - Fulah.
  - Fi - Finnish.
  - Fo - Faroese.
  - Fr - French.
  - Fy - Western Frisian.
  - Ga - Irish.
  - Gd - Scottish Gaelic.
  - Gl - Galician.
  - Gu - Gujarati.
  - Gv - Manx.
  - Ha - Hausa.
  - He - Hebrew.
  - Hi - Hindi.
  - Hr - Croatian.
  - Hu - Hungarian.
  - Hy - Armenian.
  - Ia - Interlingua.
  - Id - Indonesian.
  - Ig - Igbo.
  - Ii - Sichuan Yi.
  - Is - Icelandic.
  - It - Italian.
  - Ja - Japanese.
  - Jv - Javanese.
  - Ka - Georgian.
  - Ki - Kikuyu.
  - Kk - Kazakh.
  - Kl - Kalaallisut.
  - Km - Khmer.
  - Kn - Kannada.
  - Ko - Korean.
  - Ks - Kashmiri.
  - Ku - Kurdish.
  - Kw - Cornish.
  - Ky - Kyrgyz.
  - Lb - Luxembourgish.
  - Lg - Ganda.
  - Ln - Lingala.
  - Lo - Lao.
  - Lt - Lithuanian.
  - Lu - Luba-Katanga.
  - Lv - Latvian.
  - Mg - Malagasy.
  - Mi - Maori.
  - Mk - Macedonian.
  - Ml - Malayalam.
  - Mn - Mongolian.
  - Mr - Marathi.
  - Ms - Malay.
  - Mt - Maltese.
  - My - Burmese.
  - Nb - Norwegian (Bokmål).
  - Nd - North Ndebele.
  - Ne - Nepali.
  - Nl - Dutch.
  - Nn - Norwegian Nynorsk.
  - No - Norwegian.
  - Om - Oromo.
  - Or - Odia.
  - Os - Ossetic.
  - Pa - Punjabi.
  - Pl - Polish.
  - Ps - Pashto.
  - PtBr - Portuguese (Brazil).
  - PtPt - Portuguese (Portugal).
  - Qu - Quechua.
  - Rm - Romansh.
  - Rn - Rundi.
  - Ro - Romanian.
  - Ru - Russian.
  - Rw - Kinyarwanda.
  - Sd - Sindhi.
  - Se - Northern Sami.
  - Sg - Sango.
  - Si - Sinhala.
  - Sk - Slovak.
  - Sl - Slovenian.
  - Sn - Shona.
  - So - Somali.
  - Sq - Albanian.
  - Sr - Serbian.
  - Su - Sundanese.
  - Sv - Swedish.
  - Sw - Swahili.
  - Ta - Tamil.
  - Te - Telugu.
  - Tg - Tajik.
  - Th - Thai.
  - Ti - Tigrinya.
  - Tk - Turkmen.
  - To - Tongan.
  - Tr - Turkish.
  - Tt - Tatar.
  - Ug - Uyghur.
  - Uk - Ukrainian.
  - Ur - Urdu.
  - Uz - Uzbek.
  - Vi - Vietnamese.
  - Vo - Volapük.
  - Wo - Wolof.
  - Xh - Xhosa.
  - Yi - Yiddish.
  - Yo - Yoruba.
  - ZhCn - Chinese (Simplified).
  - ZhTw - Chinese (Traditional).
  - Zu - Zulu.
  - Zh - Chinese.
  - Pt - Portuguese.

-}
type LanguageCode
    = Af
    | Ak
    | Am
    | Ar
    | As
    | Az
    | Be
    | Bg
    | Bm
    | Bn
    | Bo
    | Br
    | Bs
    | Ca
    | Ce
    | Cs
    | Cu
    | Cy
    | Da
    | De
    | Dz
    | Ee
    | El
    | En
    | Eo
    | Es
    | Et
    | Eu
    | Fa
    | Ff
    | Fi
    | Fo
    | Fr
    | Fy
    | Ga
    | Gd
    | Gl
    | Gu
    | Gv
    | Ha
    | He
    | Hi
    | Hr
    | Hu
    | Hy
    | Ia
    | Id
    | Ig
    | Ii
    | Is
    | It
    | Ja
    | Jv
    | Ka
    | Ki
    | Kk
    | Kl
    | Km
    | Kn
    | Ko
    | Ks
    | Ku
    | Kw
    | Ky
    | Lb
    | Lg
    | Ln
    | Lo
    | Lt
    | Lu
    | Lv
    | Mg
    | Mi
    | Mk
    | Ml
    | Mn
    | Mr
    | Ms
    | Mt
    | My
    | Nb
    | Nd
    | Ne
    | Nl
    | Nn
    | No
    | Om
    | Or
    | Os
    | Pa
    | Pl
    | Ps
    | PtBr
    | PtPt
    | Qu
    | Rm
    | Rn
    | Ro
    | Ru
    | Rw
    | Sd
    | Se
    | Sg
    | Si
    | Sk
    | Sl
    | Sn
    | So
    | Sq
    | Sr
    | Su
    | Sv
    | Sw
    | Ta
    | Te
    | Tg
    | Th
    | Ti
    | Tk
    | To
    | Tr
    | Tt
    | Ug
    | Uk
    | Ur
    | Uz
    | Vi
    | Vo
    | Wo
    | Xh
    | Yi
    | Yo
    | ZhCn
    | ZhTw
    | Zu
    | Zh
    | Pt


list : List LanguageCode
list =
    [ Af, Ak, Am, Ar, As, Az, Be, Bg, Bm, Bn, Bo, Br, Bs, Ca, Ce, Cs, Cu, Cy, Da, De, Dz, Ee, El, En, Eo, Es, Et, Eu, Fa, Ff, Fi, Fo, Fr, Fy, Ga, Gd, Gl, Gu, Gv, Ha, He, Hi, Hr, Hu, Hy, Ia, Id, Ig, Ii, Is, It, Ja, Jv, Ka, Ki, Kk, Kl, Km, Kn, Ko, Ks, Ku, Kw, Ky, Lb, Lg, Ln, Lo, Lt, Lu, Lv, Mg, Mi, Mk, Ml, Mn, Mr, Ms, Mt, My, Nb, Nd, Ne, Nl, Nn, No, Om, Or, Os, Pa, Pl, Ps, PtBr, PtPt, Qu, Rm, Rn, Ro, Ru, Rw, Sd, Se, Sg, Si, Sk, Sl, Sn, So, Sq, Sr, Su, Sv, Sw, Ta, Te, Tg, Th, Ti, Tk, To, Tr, Tt, Ug, Uk, Ur, Uz, Vi, Vo, Wo, Xh, Yi, Yo, ZhCn, ZhTw, Zu, Zh, Pt ]


decoder : Decoder LanguageCode
decoder =
    Decode.string
        |> Decode.andThen
            (\string ->
                case string of
                    "AF" ->
                        Decode.succeed Af

                    "AK" ->
                        Decode.succeed Ak

                    "AM" ->
                        Decode.succeed Am

                    "AR" ->
                        Decode.succeed Ar

                    "AS" ->
                        Decode.succeed As

                    "AZ" ->
                        Decode.succeed Az

                    "BE" ->
                        Decode.succeed Be

                    "BG" ->
                        Decode.succeed Bg

                    "BM" ->
                        Decode.succeed Bm

                    "BN" ->
                        Decode.succeed Bn

                    "BO" ->
                        Decode.succeed Bo

                    "BR" ->
                        Decode.succeed Br

                    "BS" ->
                        Decode.succeed Bs

                    "CA" ->
                        Decode.succeed Ca

                    "CE" ->
                        Decode.succeed Ce

                    "CS" ->
                        Decode.succeed Cs

                    "CU" ->
                        Decode.succeed Cu

                    "CY" ->
                        Decode.succeed Cy

                    "DA" ->
                        Decode.succeed Da

                    "DE" ->
                        Decode.succeed De

                    "DZ" ->
                        Decode.succeed Dz

                    "EE" ->
                        Decode.succeed Ee

                    "EL" ->
                        Decode.succeed El

                    "EN" ->
                        Decode.succeed En

                    "EO" ->
                        Decode.succeed Eo

                    "ES" ->
                        Decode.succeed Es

                    "ET" ->
                        Decode.succeed Et

                    "EU" ->
                        Decode.succeed Eu

                    "FA" ->
                        Decode.succeed Fa

                    "FF" ->
                        Decode.succeed Ff

                    "FI" ->
                        Decode.succeed Fi

                    "FO" ->
                        Decode.succeed Fo

                    "FR" ->
                        Decode.succeed Fr

                    "FY" ->
                        Decode.succeed Fy

                    "GA" ->
                        Decode.succeed Ga

                    "GD" ->
                        Decode.succeed Gd

                    "GL" ->
                        Decode.succeed Gl

                    "GU" ->
                        Decode.succeed Gu

                    "GV" ->
                        Decode.succeed Gv

                    "HA" ->
                        Decode.succeed Ha

                    "HE" ->
                        Decode.succeed He

                    "HI" ->
                        Decode.succeed Hi

                    "HR" ->
                        Decode.succeed Hr

                    "HU" ->
                        Decode.succeed Hu

                    "HY" ->
                        Decode.succeed Hy

                    "IA" ->
                        Decode.succeed Ia

                    "ID" ->
                        Decode.succeed Id

                    "IG" ->
                        Decode.succeed Ig

                    "II" ->
                        Decode.succeed Ii

                    "IS" ->
                        Decode.succeed Is

                    "IT" ->
                        Decode.succeed It

                    "JA" ->
                        Decode.succeed Ja

                    "JV" ->
                        Decode.succeed Jv

                    "KA" ->
                        Decode.succeed Ka

                    "KI" ->
                        Decode.succeed Ki

                    "KK" ->
                        Decode.succeed Kk

                    "KL" ->
                        Decode.succeed Kl

                    "KM" ->
                        Decode.succeed Km

                    "KN" ->
                        Decode.succeed Kn

                    "KO" ->
                        Decode.succeed Ko

                    "KS" ->
                        Decode.succeed Ks

                    "KU" ->
                        Decode.succeed Ku

                    "KW" ->
                        Decode.succeed Kw

                    "KY" ->
                        Decode.succeed Ky

                    "LB" ->
                        Decode.succeed Lb

                    "LG" ->
                        Decode.succeed Lg

                    "LN" ->
                        Decode.succeed Ln

                    "LO" ->
                        Decode.succeed Lo

                    "LT" ->
                        Decode.succeed Lt

                    "LU" ->
                        Decode.succeed Lu

                    "LV" ->
                        Decode.succeed Lv

                    "MG" ->
                        Decode.succeed Mg

                    "MI" ->
                        Decode.succeed Mi

                    "MK" ->
                        Decode.succeed Mk

                    "ML" ->
                        Decode.succeed Ml

                    "MN" ->
                        Decode.succeed Mn

                    "MR" ->
                        Decode.succeed Mr

                    "MS" ->
                        Decode.succeed Ms

                    "MT" ->
                        Decode.succeed Mt

                    "MY" ->
                        Decode.succeed My

                    "NB" ->
                        Decode.succeed Nb

                    "ND" ->
                        Decode.succeed Nd

                    "NE" ->
                        Decode.succeed Ne

                    "NL" ->
                        Decode.succeed Nl

                    "NN" ->
                        Decode.succeed Nn

                    "NO" ->
                        Decode.succeed No

                    "OM" ->
                        Decode.succeed Om

                    "OR" ->
                        Decode.succeed Or

                    "OS" ->
                        Decode.succeed Os

                    "PA" ->
                        Decode.succeed Pa

                    "PL" ->
                        Decode.succeed Pl

                    "PS" ->
                        Decode.succeed Ps

                    "PT_BR" ->
                        Decode.succeed PtBr

                    "PT_PT" ->
                        Decode.succeed PtPt

                    "QU" ->
                        Decode.succeed Qu

                    "RM" ->
                        Decode.succeed Rm

                    "RN" ->
                        Decode.succeed Rn

                    "RO" ->
                        Decode.succeed Ro

                    "RU" ->
                        Decode.succeed Ru

                    "RW" ->
                        Decode.succeed Rw

                    "SD" ->
                        Decode.succeed Sd

                    "SE" ->
                        Decode.succeed Se

                    "SG" ->
                        Decode.succeed Sg

                    "SI" ->
                        Decode.succeed Si

                    "SK" ->
                        Decode.succeed Sk

                    "SL" ->
                        Decode.succeed Sl

                    "SN" ->
                        Decode.succeed Sn

                    "SO" ->
                        Decode.succeed So

                    "SQ" ->
                        Decode.succeed Sq

                    "SR" ->
                        Decode.succeed Sr

                    "SU" ->
                        Decode.succeed Su

                    "SV" ->
                        Decode.succeed Sv

                    "SW" ->
                        Decode.succeed Sw

                    "TA" ->
                        Decode.succeed Ta

                    "TE" ->
                        Decode.succeed Te

                    "TG" ->
                        Decode.succeed Tg

                    "TH" ->
                        Decode.succeed Th

                    "TI" ->
                        Decode.succeed Ti

                    "TK" ->
                        Decode.succeed Tk

                    "TO" ->
                        Decode.succeed To

                    "TR" ->
                        Decode.succeed Tr

                    "TT" ->
                        Decode.succeed Tt

                    "UG" ->
                        Decode.succeed Ug

                    "UK" ->
                        Decode.succeed Uk

                    "UR" ->
                        Decode.succeed Ur

                    "UZ" ->
                        Decode.succeed Uz

                    "VI" ->
                        Decode.succeed Vi

                    "VO" ->
                        Decode.succeed Vo

                    "WO" ->
                        Decode.succeed Wo

                    "XH" ->
                        Decode.succeed Xh

                    "YI" ->
                        Decode.succeed Yi

                    "YO" ->
                        Decode.succeed Yo

                    "ZH_CN" ->
                        Decode.succeed ZhCn

                    "ZH_TW" ->
                        Decode.succeed ZhTw

                    "ZU" ->
                        Decode.succeed Zu

                    "ZH" ->
                        Decode.succeed Zh

                    "PT" ->
                        Decode.succeed Pt

                    _ ->
                        Decode.fail ("Invalid LanguageCode type, " ++ string ++ " try re-running the @dillonkearns/elm-graphql CLI ")
            )


{-| Convert from the union type representing the Enum to a string that the GraphQL server will recognize.
-}
toString : LanguageCode -> String
toString enum____ =
    case enum____ of
        Af ->
            "AF"

        Ak ->
            "AK"

        Am ->
            "AM"

        Ar ->
            "AR"

        As ->
            "AS"

        Az ->
            "AZ"

        Be ->
            "BE"

        Bg ->
            "BG"

        Bm ->
            "BM"

        Bn ->
            "BN"

        Bo ->
            "BO"

        Br ->
            "BR"

        Bs ->
            "BS"

        Ca ->
            "CA"

        Ce ->
            "CE"

        Cs ->
            "CS"

        Cu ->
            "CU"

        Cy ->
            "CY"

        Da ->
            "DA"

        De ->
            "DE"

        Dz ->
            "DZ"

        Ee ->
            "EE"

        El ->
            "EL"

        En ->
            "EN"

        Eo ->
            "EO"

        Es ->
            "ES"

        Et ->
            "ET"

        Eu ->
            "EU"

        Fa ->
            "FA"

        Ff ->
            "FF"

        Fi ->
            "FI"

        Fo ->
            "FO"

        Fr ->
            "FR"

        Fy ->
            "FY"

        Ga ->
            "GA"

        Gd ->
            "GD"

        Gl ->
            "GL"

        Gu ->
            "GU"

        Gv ->
            "GV"

        Ha ->
            "HA"

        He ->
            "HE"

        Hi ->
            "HI"

        Hr ->
            "HR"

        Hu ->
            "HU"

        Hy ->
            "HY"

        Ia ->
            "IA"

        Id ->
            "ID"

        Ig ->
            "IG"

        Ii ->
            "II"

        Is ->
            "IS"

        It ->
            "IT"

        Ja ->
            "JA"

        Jv ->
            "JV"

        Ka ->
            "KA"

        Ki ->
            "KI"

        Kk ->
            "KK"

        Kl ->
            "KL"

        Km ->
            "KM"

        Kn ->
            "KN"

        Ko ->
            "KO"

        Ks ->
            "KS"

        Ku ->
            "KU"

        Kw ->
            "KW"

        Ky ->
            "KY"

        Lb ->
            "LB"

        Lg ->
            "LG"

        Ln ->
            "LN"

        Lo ->
            "LO"

        Lt ->
            "LT"

        Lu ->
            "LU"

        Lv ->
            "LV"

        Mg ->
            "MG"

        Mi ->
            "MI"

        Mk ->
            "MK"

        Ml ->
            "ML"

        Mn ->
            "MN"

        Mr ->
            "MR"

        Ms ->
            "MS"

        Mt ->
            "MT"

        My ->
            "MY"

        Nb ->
            "NB"

        Nd ->
            "ND"

        Ne ->
            "NE"

        Nl ->
            "NL"

        Nn ->
            "NN"

        No ->
            "NO"

        Om ->
            "OM"

        Or ->
            "OR"

        Os ->
            "OS"

        Pa ->
            "PA"

        Pl ->
            "PL"

        Ps ->
            "PS"

        PtBr ->
            "PT_BR"

        PtPt ->
            "PT_PT"

        Qu ->
            "QU"

        Rm ->
            "RM"

        Rn ->
            "RN"

        Ro ->
            "RO"

        Ru ->
            "RU"

        Rw ->
            "RW"

        Sd ->
            "SD"

        Se ->
            "SE"

        Sg ->
            "SG"

        Si ->
            "SI"

        Sk ->
            "SK"

        Sl ->
            "SL"

        Sn ->
            "SN"

        So ->
            "SO"

        Sq ->
            "SQ"

        Sr ->
            "SR"

        Su ->
            "SU"

        Sv ->
            "SV"

        Sw ->
            "SW"

        Ta ->
            "TA"

        Te ->
            "TE"

        Tg ->
            "TG"

        Th ->
            "TH"

        Ti ->
            "TI"

        Tk ->
            "TK"

        To ->
            "TO"

        Tr ->
            "TR"

        Tt ->
            "TT"

        Ug ->
            "UG"

        Uk ->
            "UK"

        Ur ->
            "UR"

        Uz ->
            "UZ"

        Vi ->
            "VI"

        Vo ->
            "VO"

        Wo ->
            "WO"

        Xh ->
            "XH"

        Yi ->
            "YI"

        Yo ->
            "YO"

        ZhCn ->
            "ZH_CN"

        ZhTw ->
            "ZH_TW"

        Zu ->
            "ZU"

        Zh ->
            "ZH"

        Pt ->
            "PT"


{-| Convert from a String representation to an elm representation enum.
This is the inverse of the Enum `toString` function. So you can call `toString` and then convert back `fromString` safely.

    Swapi.Enum.Episode.NewHope
        |> Swapi.Enum.Episode.toString
        |> Swapi.Enum.Episode.fromString
        == Just NewHope

This can be useful for generating Strings to use for <select> menus to check which item was selected.

-}
fromString : String -> Maybe LanguageCode
fromString enumString____ =
    case enumString____ of
        "AF" ->
            Just Af

        "AK" ->
            Just Ak

        "AM" ->
            Just Am

        "AR" ->
            Just Ar

        "AS" ->
            Just As

        "AZ" ->
            Just Az

        "BE" ->
            Just Be

        "BG" ->
            Just Bg

        "BM" ->
            Just Bm

        "BN" ->
            Just Bn

        "BO" ->
            Just Bo

        "BR" ->
            Just Br

        "BS" ->
            Just Bs

        "CA" ->
            Just Ca

        "CE" ->
            Just Ce

        "CS" ->
            Just Cs

        "CU" ->
            Just Cu

        "CY" ->
            Just Cy

        "DA" ->
            Just Da

        "DE" ->
            Just De

        "DZ" ->
            Just Dz

        "EE" ->
            Just Ee

        "EL" ->
            Just El

        "EN" ->
            Just En

        "EO" ->
            Just Eo

        "ES" ->
            Just Es

        "ET" ->
            Just Et

        "EU" ->
            Just Eu

        "FA" ->
            Just Fa

        "FF" ->
            Just Ff

        "FI" ->
            Just Fi

        "FO" ->
            Just Fo

        "FR" ->
            Just Fr

        "FY" ->
            Just Fy

        "GA" ->
            Just Ga

        "GD" ->
            Just Gd

        "GL" ->
            Just Gl

        "GU" ->
            Just Gu

        "GV" ->
            Just Gv

        "HA" ->
            Just Ha

        "HE" ->
            Just He

        "HI" ->
            Just Hi

        "HR" ->
            Just Hr

        "HU" ->
            Just Hu

        "HY" ->
            Just Hy

        "IA" ->
            Just Ia

        "ID" ->
            Just Id

        "IG" ->
            Just Ig

        "II" ->
            Just Ii

        "IS" ->
            Just Is

        "IT" ->
            Just It

        "JA" ->
            Just Ja

        "JV" ->
            Just Jv

        "KA" ->
            Just Ka

        "KI" ->
            Just Ki

        "KK" ->
            Just Kk

        "KL" ->
            Just Kl

        "KM" ->
            Just Km

        "KN" ->
            Just Kn

        "KO" ->
            Just Ko

        "KS" ->
            Just Ks

        "KU" ->
            Just Ku

        "KW" ->
            Just Kw

        "KY" ->
            Just Ky

        "LB" ->
            Just Lb

        "LG" ->
            Just Lg

        "LN" ->
            Just Ln

        "LO" ->
            Just Lo

        "LT" ->
            Just Lt

        "LU" ->
            Just Lu

        "LV" ->
            Just Lv

        "MG" ->
            Just Mg

        "MI" ->
            Just Mi

        "MK" ->
            Just Mk

        "ML" ->
            Just Ml

        "MN" ->
            Just Mn

        "MR" ->
            Just Mr

        "MS" ->
            Just Ms

        "MT" ->
            Just Mt

        "MY" ->
            Just My

        "NB" ->
            Just Nb

        "ND" ->
            Just Nd

        "NE" ->
            Just Ne

        "NL" ->
            Just Nl

        "NN" ->
            Just Nn

        "NO" ->
            Just No

        "OM" ->
            Just Om

        "OR" ->
            Just Or

        "OS" ->
            Just Os

        "PA" ->
            Just Pa

        "PL" ->
            Just Pl

        "PS" ->
            Just Ps

        "PT_BR" ->
            Just PtBr

        "PT_PT" ->
            Just PtPt

        "QU" ->
            Just Qu

        "RM" ->
            Just Rm

        "RN" ->
            Just Rn

        "RO" ->
            Just Ro

        "RU" ->
            Just Ru

        "RW" ->
            Just Rw

        "SD" ->
            Just Sd

        "SE" ->
            Just Se

        "SG" ->
            Just Sg

        "SI" ->
            Just Si

        "SK" ->
            Just Sk

        "SL" ->
            Just Sl

        "SN" ->
            Just Sn

        "SO" ->
            Just So

        "SQ" ->
            Just Sq

        "SR" ->
            Just Sr

        "SU" ->
            Just Su

        "SV" ->
            Just Sv

        "SW" ->
            Just Sw

        "TA" ->
            Just Ta

        "TE" ->
            Just Te

        "TG" ->
            Just Tg

        "TH" ->
            Just Th

        "TI" ->
            Just Ti

        "TK" ->
            Just Tk

        "TO" ->
            Just To

        "TR" ->
            Just Tr

        "TT" ->
            Just Tt

        "UG" ->
            Just Ug

        "UK" ->
            Just Uk

        "UR" ->
            Just Ur

        "UZ" ->
            Just Uz

        "VI" ->
            Just Vi

        "VO" ->
            Just Vo

        "WO" ->
            Just Wo

        "XH" ->
            Just Xh

        "YI" ->
            Just Yi

        "YO" ->
            Just Yo

        "ZH_CN" ->
            Just ZhCn

        "ZH_TW" ->
            Just ZhTw

        "ZU" ->
            Just Zu

        "ZH" ->
            Just Zh

        "PT" ->
            Just Pt

        _ ->
            Nothing
