# calcParser-himadriali

you can install this project by issuing the following command:
```
stack install
```

When installation is succesful and complete, look for the following lines:

```
Copied executables to /..SOME_PATH../bin:
- calcParser-exe
```

You should be able to call the executable:

```
- /..SOME_PATH../bin/calcParser-exe
```

### Plans:

We want to export a module Calc which contains: 

 * Calc.hs - declares our data types. We currently use examples from lecture but we plan to improve upon them if it becomes necessary. 

 * Parser.hs - is our parser to take in user input and return expressions.

 * Implementation.hs - takes in user input, does the math, creates a CalcDoc.

 * Output.hs - formates our CalcDoc in the appropriate way - whether we decide to print it/output it in a differeny way. 

You can find our github repo here: https://github.com/himadrin/calcParser


### Question:
Do we add the cabal file to our git repo even though it was automatically added to the .gitignore? 
