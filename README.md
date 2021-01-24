# Regex
Haskel implementation of python's regex  
Compile with ```ghc --make projectMain.hs```
## Functions
### getRegEx
takes a String representation of a regular expression and prints a list of a regex datatype  
```./projectMain getRegEx "[1-4][6-8] H*l+o.."```  

Valid String metacharacters include
- '^': Starts with 
  - ex: ^yo matches "you" or "your"
- '$': Ends with 
  - ex: ed$ matches "Rolled"
- '*': Zero or more
  - ex: ba*d matches "bd" or "baaaad"
- '+': One or more
  - ex: wow+ matches "wow" or "woooow"
- '{}': Exactly the specified number of times
  - ex: hm{3} only matches "hmmm"
- '|': Or
- '.': Any character
- '[]': set
  - '[aif]': a or i or f
  - '[a-f]': character between a and f
  - '[135]': 1 or 3 or 5
  - '[1-3]': number between 1 and 3
  
### Search
Search a file and return the number of regular expression matches  
```./projectMain Search “37 HHHlllozz” test.txt```  

### Generate
Generates a random string that matches the regular expression.  
```./projectMain Generate "[1-4][6-8] H*l+o.."```
