import Control.Monad
main=replicateM 2 getLine>>=putStrLn.(\[a,b,c,d]->if a==c||a==d||b==c||b==d then"YES"else"NO").concat.map words