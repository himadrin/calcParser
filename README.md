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

Alternatively to run the program - you can also type the command:
```
stack run
```

You may view the results of our test instances we made with the command:
```
stack test
```

### Input:

When running the program your input should be in the format derive(variable you are deriving in terms of, expression) for example:
```
derive(x, x^2)
```
You must use parenthesis around functions to control what is considered input such as:
```
derive(x, (x^2) + (2*x) + 3)
```

To see the reults of just our Expression parser you can first run :
```
stack ghci
```
And then 'parseTest pExpr "expression"' such as these:
```
parseTest pExpr "derive(x, sin(x) + (x^2))"
```
```
parseTest pExpr "(x^4) + (2 * (x^2)) + 7"
```

### Implementation:

We have created a calculus solver with the following modules: 

 * Calc.hs - declares our data types. We have included instances deriving Eq which we use for our calculations and instances deriving Show  which we use for printing our output. 

 * Parser.hs - contains our parser to take in a string and return expressions. We also include a parser for laws which are written in Laws.txt and read in at the Main.
    
 * Implementation.hs - contains our matchFunc, our apply function, and rewrites. Here is where we match expressions to laws in order to perform the derivation. 

    * We have use a special for p and q in our law handling in order to take care of constants

 * Output.hs - stepList calls rewrites and doMath recursively until we can no longer rewrite the function applying the laws in order to create a list of Steps associated with a law name. The last function final is used in main to handle the error from using parse from megaParsec.
  
    * We also implemented simple math in the case that two constants are separated by a binary operator. It should then do the math on the constants.

 * Laws.hs - includes our parsed list of laws - not necessary for running the code but could be useful for testing.

 * Laws.txt - this file includes all our laws for taking derivatives which is parsed and used in the main

 * Main.hs - First it takes in our law list, parses it, and creates a list of Law objects. Then it takes in an expression from stdin and parses it and then calls final on the parsed expression and prints it for the user!

 * We have used several libraries and tutorials which we cite in comments- including Megaparsec

You can find our github repo here: https://github.com/himadrin/calcParser

### Extra Functionality
The function simplify in Output.hs allows our app to add constants when they are left in the final solution step. You can see this in test case 6.

### Contributors:
Winter 2020: Himadri Narasimhamurthy, Ali Hagen, assistance from Prof. Joosten and from the textbook Thinking Functionally With Haskell.

Thank you so much for your help!


### Next Steps:
We would love to get this working with a web UI - unfortunately we did not have the time to do so but we are going to keep working on it!
