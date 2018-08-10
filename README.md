Collection of aliases and procedures that make easier to write C in nim. ;)

Contains many aliases for the nim's keyword bitwise operators. 

Nim's keyword operators don't support nim's strong-spaces feature, nor have an
assigment operator version (like `+=`). The reuse of the logical keywords for
`and`, `or` and `not` for the bitwise version also hinders readability somewhat
and have arguably the wrong precedence. This module attempts to fix all that.

It also includes pointer arithmetic, some useful bitops (most were removed as
we got the very nice bitops module in nim standard library!), and some misc 
aliases for shallowCopy.
