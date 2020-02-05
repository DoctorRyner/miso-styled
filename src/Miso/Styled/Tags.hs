module Miso.Styled.Tags where

import qualified Miso
import           Miso.Styled.DOM

type Name = String

type Tag event = [Miso.Attribute event] -> [View event] -> View event

generate :: [String] -> IO ()
generate tagNames =
    writeFile
        "tags.hs"
        $ concatMap
            (\tagName ->
                concat
                    [ tagName <> "_ :: Tag event\n"
                    , tagName <> "_ = el \"" <> tagName <> "\"\n\n"
                    ]
            )
            tagNames

tags :: [String]
tags =
    [ "a"
    , "abbr"
    , "address"
    , "area"
    , "article"
    , "aside"
    , "audio"
    , "b"
    , "base"
    , "bdo"
    , "blockquote"
    , "body"
    , "br"
    , "button"
    , "canvas"
    , "caption"
    , "cite"
    , "code"
    , "col"
    , "colgroup"
    , "command"
    , "datalist"
    , "dd"
    , "del"
    , "details"
    , "dfn"
    , "div"
    , "dl"
    , "dt"
    , "em"
    , "embed"
    , "fieldset"
    , "figcaption"
    , "figure"
    , "footer"
    , "form"
    , "h1"
    , "h2"
    , "h3"
    , "h4"
    , "h5"
    , "h6"
    , "head"
    , "header"
    , "hr"
    , "html"
    , "i"
    , "iframe"
    , "img"
    , "input"
    , "ins"
    , "kdb"
    , "label"
    , "li"
    , "link"
    , "map"
    , "main"
    , "menu"
    , "menuitem"
    , "meta"
    , "nav"
    , "noscript"
    , "object"
    , "ol"
    , "option"
    , "p"
    , "param"
    , "svg"
    , "pre"
    , "progress"
    , "q"
    , "rp"
    , "rt"
    , "script"
    , "section"
    , "select"
    , "small"
    , "source"
    , "span"
    , "strong"
    , "style"
    , "sub"
    , "summary"
    , "table"
    , "sup"
    , "tbody"
    , "td"
    , "textarea"
    , "tfoot"
    , "th"
    , "template"
    , "thread"
    , "time"
    , "title"
    , "tr"
    , "track"
    , "ul"
    , "var"
    , "video"
    , "wbr"
    ]

a_ :: Tag event
a_ = el "a"

abbr_ :: Tag event
abbr_ = el "abbr"

address_ :: Tag event
address_ = el "address"

area_ :: Tag event
area_ = el "area"

article_ :: Tag event
article_ = el "article"

aside_ :: Tag event
aside_ = el "aside"

audio_ :: Tag event
audio_ = el "audio"

b_ :: Tag event
b_ = el "b"

base_ :: Tag event
base_ = el "base"

bdo_ :: Tag event
bdo_ = el "bdo"

blockquote_ :: Tag event
blockquote_ = el "blockquote"

body_ :: Tag event
body_ = el "body"

br_ :: Tag event
br_ = el "br"

button_ :: Tag event
button_ = el "button"

canvas_ :: Tag event
canvas_ = el "canvas"

caption_ :: Tag event
caption_ = el "caption"

cite_ :: Tag event
cite_ = el "cite"

code_ :: Tag event
code_ = el "code"

col_ :: Tag event
col_ = el "col"

colgroup_ :: Tag event
colgroup_ = el "colgroup"

command_ :: Tag event
command_ = el "command"

datalist_ :: Tag event
datalist_ = el "datalist"

dd_ :: Tag event
dd_ = el "dd"

del_ :: Tag event
del_ = el "del"

details_ :: Tag event
details_ = el "details"

dfn_ :: Tag event
dfn_ = el "dfn"

div_ :: Tag event
div_ = el "div"

dl_ :: Tag event
dl_ = el "dl"

dt_ :: Tag event
dt_ = el "dt"

em_ :: Tag event
em_ = el "em"

embed_ :: Tag event
embed_ = el "embed"

fieldset_ :: Tag event
fieldset_ = el "fieldset"

figcaption_ :: Tag event
figcaption_ = el "figcaption"

figure_ :: Tag event
figure_ = el "figure"

footer_ :: Tag event
footer_ = el "footer"

form_ :: Tag event
form_ = el "form"

h1_ :: Tag event
h1_ = el "h1"

h2_ :: Tag event
h2_ = el "h2"

h3_ :: Tag event
h3_ = el "h3"

h4_ :: Tag event
h4_ = el "h4"

h5_ :: Tag event
h5_ = el "h5"

h6_ :: Tag event
h6_ = el "h6"

head_ :: Tag event
head_ = el "head"

header_ :: Tag event
header_ = el "header"

hr_ :: Tag event
hr_ = el "hr"

html_ :: Tag event
html_ = el "html"

i_ :: Tag event
i_ = el "i"

iframe_ :: Tag event
iframe_ = el "iframe"

img_ :: [Miso.Attribute a] -> View a
img_ attrs = el "img" attrs []

input_ :: [Miso.Attribute a] -> View a
input_ attrs = el "input" attrs []

ins_ :: Tag event
ins_ = el "ins"

kdb_ :: Tag event
kdb_ = el "kdb"

label_ :: Tag event
label_ = el "label"

li_ :: Tag event
li_ = el "li"

link_ :: Tag event
link_ = el "link"

map_ :: Tag event
map_ = el "map"

main_ :: Tag event
main_ = el "main"

menu_ :: Tag event
menu_ = el "menu"

menuitem_ :: Tag event
menuitem_ = el "menuitem"

meta_ :: Tag event
meta_ = el "meta"

nav_ :: Tag event
nav_ = el "nav"

noscript_ :: Tag event
noscript_ = el "noscript"

object_ :: Tag event
object_ = el "object"

ol_ :: Tag event
ol_ = el "ol"

option_ :: Tag event
option_ = el "option"

p_ :: Tag event
p_ = el "p"

param_ :: Tag event
param_ = el "param"

svg_ :: Tag event
svg_ = el "svg"

pre_ :: Tag event
pre_ = el "pre"

progress_ :: Tag event
progress_ = el "progress"

q_ :: Tag event
q_ = el "q"

rp_ :: Tag event
rp_ = el "rp"

rt_ :: Tag event
rt_ = el "rt"

script_ :: Tag event
script_ = el "script"

section_ :: Tag event
section_ = el "section"

select_ :: Tag event
select_ = el "select"

small_ :: Tag event
small_ = el "small"

source_ :: Tag event
source_ = el "source"

span_ :: Tag event
span_ = el "span"

strong_ :: Tag event
strong_ = el "strong"

style_ :: Tag event
style_ = el "style"

sub_ :: Tag event
sub_ = el "sub"

summary_ :: Tag event
summary_ = el "summary"

table_ :: Tag event
table_ = el "table"

sup_ :: Tag event
sup_ = el "sup"

tbody_ :: Tag event
tbody_ = el "tbody"

td_ :: Tag event
td_ = el "td"

textarea_ :: Tag event
textarea_ = el "textarea"

tfoot_ :: Tag event
tfoot_ = el "tfoot"

th_ :: Tag event
th_ = el "th"

template_ :: Tag event
template_ = el "template"

thread_ :: Tag event
thread_ = el "thread"

time_ :: Tag event
time_ = el "time"

title_ :: Tag event
title_ = el "title"

tr_ :: Tag event
tr_ = el "tr"

track_ :: Tag event
track_ = el "track"

ul_ :: Tag event
ul_ = el "ul"

var_ :: Tag event
var_ = el "var"

video_ :: Tag event
video_ = el "video"

wbr_ :: Tag event
wbr_ = el "wbr"
