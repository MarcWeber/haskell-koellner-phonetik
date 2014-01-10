module KoellnerPhonetik where
import qualified Data.Char as DC

-- soundex phonetic code for German words
-- http://de.wikipedia.org/wiki/K%C3%B6lner_Phonetik
-- maybe this implementation still has some bugs

{-
Buchstabe	Kontext	Code
A, E, I, J, O, U, Y		0
H		-
B		1
P	nicht vor H
D, T	nicht vor C, S, Z	2
F, V, W		3
P	vor H
G, K, Q		4
C	im Anlaut vor A, H, K, L, O, Q, R, U, X
vor A, H, K, O, Q, U, X außer nach S, Z
X	nicht nach C, K, Q	48
L		5
M, N		6
R		7
S, Z		8
C	nach S, Z
im Anlaut außer vor A, H, K, L, O, Q, R, U, X
nicht vor A, H, K, O, Q, U, X
D, T	vor C, S, Z
X	nach C, K, Q
-}
soundex_ger :: String -> String
soundex_ger s =
    dropDups $ toCode1 $ map (noUmlaute . DC.toLower) s
  where
    dropDups (a:b:rest) = if (a == b)
                              then a:(dropDups rest)
                              else a:(dropDups (b:rest))
    dropDups r = r


    noUmlaute 'ä' = 'a'
    noUmlaute 'ü' = 'u'
    noUmlaute 'ö' = 'o'
    noUmlaute k = k


    -- take care only if first character
    toCode1 :: String -> String
    toCode1 ('a':rest) = '0' : toCode rest
    toCode1 ('e':rest) = '0' : toCode rest
    toCode1 ('i':rest) = '0' : toCode rest
    toCode1 ('j':rest) = '0' : toCode rest
    toCode1 ('o':rest) = '0' : toCode rest
    toCode1 ('u':rest) = '0' : toCode rest
    toCode1 ('y':rest) = '0' : toCode rest
    toCode1 (rest) = toCode rest

    toCode :: String -> String
    toCode (before:'x':rest) =
              toCode (before:[])
              ++
                if (before /= 'c' && before /= 'k' && before /= 'q')
                then "48" ++ toCode rest
                else if (before == 'c' || before == 'k' || before == 'q')
                then '8' : toCode rest
                else toCode rest

    toCode (before:'c':next:rest) =
      toCode (before:[])
      ++ 
        if ( (before == ' ' && (  next == 'a' || next == 'h' || next == 'k' || next == 'l' || next == 'o' || next == 'q' || next == 'r' || next == 'u' || next == 'x'))
          || (before /= 's' && before /= 'z' && (next == 'a' || next == 'h' || next == 'k' || next == 'o' || next == 'q' || next == 'u' || next == 'x'))
        ) then "4" ++ toCode rest
        else if (
            (before == 's' || before == 'z')
         || (before == ' ' && ( next /= 'a' && next /= 'h' && next /= 'k' && next /= 'l' && next /= 'o' && next /= 'q' && next /= 'r' && next /= 'r' && next /= 'r' && next /= 'u' && next /= 'x'))
         || (next /= 'a' && next /= 'h' && next /= 'k' && next /= 'o' && next /= 'q' && next /= 'q' && next /= 'u' && next /= 'x')
         )
        then "8" ++ toCode rest
        else toCode rest

    toCode "" = ""
    toCode ('a':rest) = toCode rest
    toCode ('e':rest) = toCode rest
    toCode ('i':rest) = toCode rest
    toCode ('j':rest) = toCode rest
    toCode ('o':rest) = toCode rest
    toCode ('u':rest) = toCode rest
    toCode ('y':rest) = toCode rest

    toCode ('h':rest) = toCode rest
    toCode ('b':rest) = "1" ++ toCode rest
    toCode ('p':'h':rest) = "3" ++ toCode rest
    toCode ('p':rest) = "1" ++ toCode rest

    toCode ('d':n:rest) = dt n rest
    toCode ('t':n:rest) = dt n rest

    toCode ('f':rest) = '3': toCode rest
    toCode ('v':rest) = '3': toCode rest
    toCode ('w':rest) = '3': toCode rest

    toCode ('g':rest) = '4': toCode rest
    toCode ('k':rest) = '4': toCode rest
    toCode ('q':rest) = '4': toCode rest

    toCode ('s':rest) = '8': toCode rest
    toCode ('z':rest) = '8': toCode rest

    toCode ('l':rest) = '5': toCode rest

    toCode ('m':rest) = '6': toCode rest
    toCode ('n':rest) = '6': toCode rest

    toCode ('r':rest) = '7': toCode rest

    toCode (_:rest) = toCode rest

    dt n rest = if (n == 'c' || n == 's' || n == 'z')
      then '8': toCode rest
      else '2': toCode rest
