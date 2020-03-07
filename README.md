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

    * At the moment data types have been moved into parser because stack install would not compile - still looking into this

 * Parser.hs - is our parser to take in user input and return expressions.
    
    * We have used several libraries and tutorials which we cite in comments

    * You can try out our parser by running cabal repl, loading our parser module, and then running parseTest pExpr on any string expression

        * ex1: parseTest pExpr "(x+y)+z"
        * ex2: parseTest pExpr "derive(x, x^2)"

 * Implementation.hs - takes in user input, does the math, creates a CalcDoc.
    
    * We now must write our Laws for calculus - we will do it here.

 * Output.hs - formates our CalcDoc in the appropriate way - whether we decide to print it/output it in a different way. 

You can find our github repo here: https://github.com/himadrin/calcParser


### Next Steps:
We would love to get this working with a web UI - unfortunately we did not have the time to do so!