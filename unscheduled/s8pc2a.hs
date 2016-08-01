import Data.List
main=getLine>>=print.length.group.dropWhileEnd(/='I').dropWhile(/='I')