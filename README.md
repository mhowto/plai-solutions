Solutions for Programming Languages: Application and Interpretation

# Ch8. Mutation: Structures and Variables

# Ch9. Recursion and Cycles: Procedures and Data

## Exercise 1
```
Add lists and binary trees as built-in datatypes to the programming language.
```
Solution: 

Add such sub-types into ExprC:
```
[nilC]
[consC (head : ExprC) (tail : ExprC)]
[nodeC (value : ExprC) (left : ExprC) (right : ExprC)]
```

## Exercise 2
```
Run the equivalent program through your interpreter for boxes and make sure it produces a cyclic value. How do you check this?
```