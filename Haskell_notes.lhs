GHCI commands:
    :t  expr        #displays the type of the expression. Types have proper casing, and the type of a list/tuple is a list/tuple of the element type(s).
Interpreter: ghci
Compiler: ghc, runhaskell (to compile and run immediately)

All binary functions can be used as infix operators if surrounded by backticks (`), or if their name consists only of punctuation. (Surround such functions in brackets to use them normally.)
Apostrophes can be used in function names.

Operators:
    /=      the 'not equals' operator
    not     the 'not' operator

List representations: [1, 2, 3, 4], 1:2:3:4:[]
Lists are strongly typed - if you have nested lists, they must all be the same type (but not length).
Strings are lists with semantic sugar - can apply these functions to them.

List operations:
    concatenation:  [1, 2, 3] ++ [4, 5]          #This is O(N), as these are *linked lists*
    prepend/cons:   0:[1, 2, 3] = [0, 1, 2, 3]   #This is O(1)
    indexing:       [1, 2, 3, 4] !! 0 = 1
    comparison:     L1 < L2                      #lexicographic order - elements are compared one at a time (think Python tuples)
    functions:      head/tail, last/init, length, null (if list is empty), reverse, take/drop (returns (all but) the first N elements), maximum, minimum, sum, product, elem (if element is in list)
                    cycle (repeats a list infinitely), repeat (repeats an element infinitely), replicate (repeats an element N times), zip, zipWith (combines the elements from the lists using the supplied function)
                    odd, even, mod
                    error (equiv. of raising exceptions)
                    flip (swaps the first two arguments. immensely useful when combined with currying)
                    foldl/foldr (pops elements from the left/right side of the list, combining them with a binary function)
                    scanl/scanr (cumulative versions of foldl/foldr - return all values, not just the final result)

Ranges: [1..10], [10,9..1], [1,2..]
Note that instead of specifying the step, we specify the first two elements, and Haskell infers the rest.
This only works for linear sequences.
Avoid using with FP numbers, as the error will accumulate.
Omission of the last element results in an infinite list. (Useful when combined with take)

List comprehension:
>                       [f(x) | x <- xDomain]
>                       [f(x) | x <- xDomain, xCondition]
>                       [f() | _ <- domain]

We can draw variables from multiple lists, and have multiple predicates (constraints on the variables). All of these are comma separated.
We can use _ to discard the input from the domain, and simply loop (length domain) times.
Note that these must appear in [] brackets (like normal lists).

Tuples - fixed length record whose elements can have different types. 2-tuples are pairs, 3-tuples tripples, etc.
Uses () brackets (like Python)
Functions:      fst (first element of a pair), snd (second element of a pair)


Functions:
>   foo :: aType -> bType -> cType -> resType    --type declaration (optional). Note the lack of distinction between parameter and return types.
>   foo a,b,c = expr_in_terms_of_args

Pattern matching - can define a function multiple times, with parameters replaced by constants. Can be used to decompose tuples (trivial) and lists (x:xs). Note that x:xs implies a non-empty list. Note that we can use : (cons), but not ++.
Patterns - a parameter of form all@(x:y:xs) decomposes the argument, but aliases x:y:xs to all.
Guards - similar to switch/match. (The newlines below are optional but highly recommended.)
>   myfunc inf = "wow that's big"
>   myfunc x
>       | y == 0    = "so very close"
>       | z == 0    = "so very close"
>       | x <  0    = "negative"
>       | x >  0    = "positive"
>       | x == 0    = "zero"
>       | otherwise = "that's an odd looking number"
>       where   y = x+1
>               z = x-1

Note that the entire set of guards forms a *single pattern*. This means that instead of using otherwise, we can just add another pattern. Everything is evaluated top to bottom.
Where reduces repetition within a pattern by binding expressions (incl. functions) to variables. Note that the common indentation is mandatory.

Let is an expression providing local binding, with bindings separated by indentation or semicolons. Unlike where, it is specific to a single guard. It can be used in list comprehensions the same way as predicates. Omission of in results in implicit scope.
>   let x = 1
>       y = 2
>   in x + y
>
>   let z = (let x = 1; y = 2 in x + y)

All functions are implicitly curried. i.e. instead of being f(x, y, z), they're actually (((f x) y) z) meaning an insufficient no. of args results in a partially applied function. To use partial application with infix functions, write them as (/10) or (10/)
>   map (+3) [1,2...10]

Lambda functions: Brackets are optional, but needed in most contexts (and for clarity).
NOTE: Be careful not to use lambdas where partial application would work, partial application is much easier to read.
>   (\x y -> x*y)

The function application operator has the lowest precedence. This is useful because function application normally has the /highest/ precedence.
It also lets us eliminate long strings of brackets, providing a simple visual indicator that one function is being supplied to another.
>   sqrt 3 + 4 + 9      = (sqrt 3) + 4 + 9      -- normal rules of precedence
>   sqrt $ 3 + 4 + 9    = sqrt (3 + 4 + 9)      -- $ causes the RHS of the expression to be computed first
>   map ($ 2) $ map (^) [1,2..]                 -- it's also quite useful for working with lists of functions (notice both uses here)

Function composition - this is also useful for eliminating brackets.
>   map (f . g . h) x   = map (f (g h)) x

To convert a long string of function calls to function composition form, use the function application operator. Note that the no. of dots and brackets is the same.
>   foo a . bar . baz c $ x y z     = foo a (bar (baz c x y z))

Note that while function application ($) is generally pretty readable, composition (.) can make things worse at times.
    $ is analagous to putting the rest of the line in one set of parentheses (a single value)
    . is analagous to putting the rest of the line in nested parentheses (nested functions)

Case statements are an explicit form of pattern matching:
>   case expr of pattern1 -> result1
>                pattern2 -> result2
>                expr -> catch_all_result

Types:
    Int - bounded integer (32 or 64 bit)
    Integer - unbounded integer. Less efficient than Int.
    Float, Double
    Bool
    Char
    String - equiv to [Char]
    (a -> b) - a function with one parameter. Brackets are necessary if this type is used for another function.

Types are always properly cased. Lower case identifiers represent *type variables*. Functions which use these are called polymorphic. Convention is to use a, b, c, ... as type variables
Explicit type annotations are used to specify that an expression must be a certain type. This is required when the type cannot be inferred.
>   expr :: Type
>   read "5" :: Int

Type classes - like interfaces - require the type to implement some functionality
>   (Eq a) => a                         --any type a that is in the Eq type class.
>   (Num a, Integral b) => a -> b       --all type classes go inside the brackets

    Eq - supports ==
    Ord - supports <, etc.
    Show - can be converted to a string, via the show function
    Read - can be parsed into a string, via the read function
    Enum - sequentially ordered; can be used with list ranges, succ, and pred
    Bounded - can be used with minBound and maxBound functions.
    Num - any numeric type
    Integral, Floating - subsets of Num restricted to whole/FP numbers

Need to use fromIntegral to convert integers into more general numbers.

I/O
Use the <- operator to bind I/O actions to variables. i.e. to unbox them. This *must* be done before the result of an I/O function can be used.
To box a value into an I/O action, use return (note that this *does not* terminate the function.)
>   putStrLn "Enter your name"
>   name <- getLine
>   fakeName <- return "John Smith"
Use the sequence function to execute/unbox a list of I/O actions. mapM/mapM_/forM combine sequence with map.

print prints all types in a repr-like format (strings are wrapped in quotes). putStr(Ln) only prints strings, but preserves their formatting.

A sequence of imperative commands (I/O actions) must be wrapped in a do block; this combines them into the equivalent of a single I/O action. do blocks can have let clauses.
main must return an I/O, so it will return the result of the last command.

Conditional I/O:

>   if (a == 1)
>       then putStrLn "True"
>       else do
>           putStrLn "Very"
>           putStrLn "False"

Alternatively:

>   Control.Monad.when (a == 1) $ do
>       putStrLn "Very"
>       putStrLn "False"

getContents - reads all input from stdin *lazily*. That is, although it will read all data with a single call, only one line/buffer will be read into memory and processed at a time. Therefore, 'lines . getContents' is much simpler and preferrable to calling GetLine repeatedly. (This behaviour can be controlled with hSetBuffering)
interact f - read all input from stdin, pass it to f, then print the output of f

>   main = do
>       data <- getContents
>       putStr (foo data)
>
>   main = interact foo         -- this is equivalent to the above
>
>   foo = ...                   -- some pure function

openFile path mode -- opens a file
hGetContents, hClose -- read from, close a file handle
withFile -- execute a lambda with the handle, taking care of opening and closing the file
readFile/writeFile/appendFile -- convenience methods for accessing a file
hSetBuffering -- change buffer size (affects lazy reads/writes)
hFlush -- flushes the buffer

>   withFile "data.txt" ReadMode (\handle -> do
>       data = hGetContents handle
>       putStr data)
>
>   do
>       data <- readFile "data.txt"
>       writeFile "data2.txt" data

System.IO.openTempFile -- opens a file with some random letters appended to the filename
System.Environment.getArgs -- equivalent to sys.argv[1:]. i.e. omits program name

Strings are slow because chars have variable size and lists use lazy evaluation with thunks of 1-element (smaller thunks have more overhead). Prefer Data.ByteString when you need a more efficient approach (each char is one byte, entire array is evaluated at once).  Data.ByteString.Lazy is similar, but uses 64 KB thunks instead (Strings use 1-char thunks).
Use fromChunks/toChunks to convert between strict and lazy byte strings.
cons/cons' -- prefix a byte to a bytestring lazily/strictly
There are ByteString(.Lazy) equivalents of readFile, etc.

Exceptions can only be caught inside do blocks, because everything else has lazy evaluation (i.e. we don't know when the code will actually be run).
Since we want to minimize the amount of I/O code, prefer using Either and Maybe to represent failed results.

>   main = tryFunc `System.IO.Error.catch` catchFunc
>   tryFunc = ...
>   catchFunc
>       | System.IO.isDoesNotExistError e = putStrLn "File not found"
>       | otherwise = ioError e                                         -- re-throw the exception

userError -- raises a custom Exception
>   ioError $ userError "Well this sucks."

ioeGetFileName -- returns the file the exception was raised in. See System.IO.Error for other ioe... functions.

Functors: fmap maps the mapping a -> b to f a -> f b
>   fmap (++ "!") (Just "foo")  = Just "foo!"

Applicative functors: perform operations on data with context. e.g. results from operations that may have failed, results from operations which are non-deterministic and therefore have multiple possible values, etc. <_> functions/operators are from the Applicative type class
>   (-) <$> [3,4] <*> [1,2,3]   = [2,1,0,3,2,1]                 -- this code generates all possible results for multiple possible inputs
>   (*) <$> Just 2 <*> Just 8                                   -- returns Just 16 or Nothing

Monads: apply functions of type (a -> f b) to values of type (f a). (i.e. feed fancy inputs to functions that map normal values to fancy ones).
    >>= performs this operation (called bind)
    do blocks effectively chain multiple binds
    >= is more elegant for chaining together operations, while do is better when the instructions do not necessarily form a sequential chain


Functors revisted
    Short version: a functor is any kind of data structure (including ADTs like trees and lists) where we want to be able to apply
    a mapping to the contained data elements.
    fmap /lifts/ a function; it maps it from (a -> b) to (f a -> f b)
    A 'functor over X' is a functor whose elements are of type X.

    Definition:
>   class Functor f where
>       fmap :: (a -> b) -> f a -> f b

   f is a type constructor with one argument. e.g. Maybe. (Note that type constructors can use currying.)
   fmap applies a function (a -> b) to the contents of the box/functor, and outputs the result in the same kind of box. e.g.
>   instance Functor Maybe where
>       fmap f (Just x) = Just (f x)
>       fmap f Nothing = Nothing

    This is basically just glue code which allows us to wrap values in computational contexts.
>   let x = Just 2
>   let y = fmap (+ 1) x                        --Just 3
>   let z = fmap (+ 2 . * 3 . - 4) x            --Using function composition 

    Lists, trees, and other abstract data types are also boxes/functors. fmap = map for lists.
    This allows us to apply a mapping to all the values stored in a tree, without changing its structure.
    IO objects are also functors with syntactic sugar, as they wrap a value. (See monads.)
    Curried functions are also functors, because they wrap values. (fmap = function composition)

    Functor laws:
    1. Mapping the identity function (\x -> x) over a functor should return an equivalent functor
    2. fmap (f . g) = fmap f . fmap g

Applicative Functors
    Regular functors enable maps; functions taking 1 argument, which are applied to one box.
    Applicative functors wrap functions as well, which means they can be used for N args via currying.
    They are more general, which means you can have functions written for all AFs which use the definition as glue code.

    Definition:
>   class (Functor f) => Applicative f where                --AFs are a special case of functors
>       pure :: a -> f a                                    --pure wraps a value in the box/functor. i.e. gives it the default context
>       (<*>) :: f (a -> b) -> f a -> f b                   --applies a map from within a functor
> --    fmap  ::   (a -> b) -> f a -> f b                   --(for comparison)

>   instance Applicative Maybe where
>       pure = Just
>       Nothing <*> _ = Nothing
>       (Just f) <*> something = fmap f something           --use pattern matching to extract map, then apply via fmap

>       --Equivalent definition of <*>
>       (Just f) <*> (Just x) = Just (f x)                  --like fmap, but takes the map from within a functor
>       Nothing <*> _ = Nothing
>       _ <*> Nothing = Nothing

    Examples:
>   Just (+3) <*> Just 5                --Just 8
>   pure (+3) <*> pure 5                --Just 8    Now using pure for more general form.

>   pure (+)  <*> Just 3 <*> Just 5     --Just 8    We can now use <*> to apply functions of N args to functors of values.
>   fmap (+) Just 3 <*> Just 5          --Just 8    Equivalent, because function application has precedence.
>   (+)  <$> Just 3 <*> Just 5          --Just 8    Equivalent form using syntactic sugar instead of fmap.
>   liftA2 (+) (Just 3) (Just 5)        --Just 8    Equivalent form using a helper method from Control.Applicative to encapsulate AF.

    This is especially useful when working with functions like fold().
>   foldr (liftA2 (+)) (pure 0) [Just 3, Just 5, Just 2]    -- Just 10

    Lists are AFs - using <$> on a list of functions and args will output a list of all possible applications.
    ZipLists are AFs where <$> corresponds to zip(). i.e. pairwise/tuplewise application.
    IO objects are AFs - you can use <$> to perform operations on their contents.

    sequence is a useful function which maps a list of functors into a functor over the list of their contents.

Monoid
    A monoid is a type with an associative binary function which has a value that acts as the identity for that function.

>   class Monoid m where
>       mempty :: m
>       mappend :: m -> m -> m
>       mconcat :: [m] -> m
>       mconcat = foldr mappend mempty

    Monoid laws:
>   mempty `mappend` x = x
>   x `mappend` mempty = x
>   (x `mappend` y) `mappend` z = x `mappend` (y `mappend` z)

    Examples:
    Lists are monoids of (++) and [].
    Data.Monoid.Sum:     Numbers are monoids of (+) and 0.
    Data.Monoid.Product: Numbers are monoids of (*) and 1.
    Any:                 Bools are monoids of (||) and False.
    All:                 Bools are monoids of (&&) and True.
    Ordering:            Ords are monoids of compare and EQ. Useful for combining sorting criteria, since left-most has priority.
    First:               Maybes are monoids such that the result is the first non-null element.
    Last:                Maybes are monoids such that the result is the last non-null element.

    Foldable is a typeclass which is a special kind of monoid for folding; it generalises foldl/foldr to non-lists. e.g. maybe, trees

Monads
    Functors apply a function to a value with a context, yielding a value with a context.
    Applicative functors apply a function with a context to a value with context, yielding a value with a context.
    Monads apply a function to a value with a context, yielding a value with a context.

                            Function        Input       Output
    Functor                 (a -> b)        f a         f b
    Applicative functor     f (a -> b)      f a         f b
    Monad                   (a -> f b)      f a         f b

    Functors are containers (abstract data types).
    Applicative functors extend functors so that the functions can have contexts.
    Monads allow us to chain together operations where each operation generates a new context.

>   fmap  :: (a -> b)   -> f a -> f b           --Functor
>   (<*>) :: f (a -> b) -> f a -> f b           --Applicative Functor

>   Cf    :: (a -> f b) -> f a -> f b           --for comparison - like an AF, but the function has no context and *returns a new context*. e.g. we can return Nothing to indicate failure
>   (>>=) :: f a -> (a -> f b) -> f b           --Monad (same args, different order to above). The different order allows a more intuitive syntax (see below).


    Definition
>   class Monad m where
>       return :: a -> m a                      --wraps a value in minimal context; equiv to pure. Note this is a just a function, not a keyword.
>       (>>=) :: m a -> (a -> m b) -> m b       --bind operator: applies a function to a contextual value
>
>       (>>) :: m a -> m b -> m b               --equivalent to: ' >>= \_ -> '. Used to disregard the previous value, but not its context.
>       x >> y = x >>= \_ -> y
>
>       fail :: String -> m a                   --returned for failed pattern matches
>       fail msg = error msg

>   instance Monad Maybe where
>       return x = Just x
>       Nothing >>= f = Nothing
>       Just x  >>= f = f x
>       fail _ = Nothing

    >>= is used for applying a function, >> is used for replacing the old value with a new one.
>   return 1 >>= \x -> Just (x+1) >> Just 3  >>= \x -> Just (x*4)   --returns 3*4, since >> causes the previous value to be discarded
>   return 1 >>= \x -> Just (x+1) >> Nothing >>= \x -> Just (x*4)   --returns Nothing
>   Nothing  >>= \x -> Just (x+1) >> Just 3  >>= \x -> Just (x*4)   --returns Nothing

    Do blocks allow us to nest the (monadic) application of functions.
>   foo = Just 3   >>= (\x ->
>             Just "!" >>= (\y ->
>                 Just (show x ++ y)
>             )
>       )

>   foo = do
>       x <- Just 3
>       y <- Just "!"
>       Just (show x ++ y)

> --return 1 >>= \x -> Just (x+1) >> Nothing >>= \x -> Just (x*4)
>   foo = do
>       x0 <- return 1
>       x1 <- (\x -> Just (x+1)) x0             --each step applies the function to the previous result
>       Nothing                                 --even though there doesn't appear to be a dependency on this line,
> --                                              it will cause the result to be Nothing because of its context
> --    _ <- Nothing                            (equivalent syntax; the context, not the binding, is what matters.)
>       x2 <- (\x -> Just (x*4)) x1
>       x2                                      --the last line must *not* perform a binding, because we need to return it instead

    A non-linear tree of dependencies instead of the chain shown here would work similarly, as the Nothing would propagate upwards. (Consider how it would look as nested application.)
    Pattern matching is also available, similar to let bindings. If a pattern doesn't match, the fail function from the monad definition is used to return a context.

    Monad laws:
>   return x >>= f  = f x                       --left identity
>   m >>= return    = m                         --right identity
>   (m >>= f) >>= g = m >>= (\x -> f x >>= g)   --associativity

    <=< is the function composition operator.
>   f <=< g = (\x -> g x >>= f)

    Here are the monad laws rewritten to use it:
>   f <=< return    = f                         --left identity
>   return <= f     = f                         --right identity
>   f <=< (g <=< h) = (f <=< g) <=< h           --associativity


    Lists are monads where >>= applies an element-to-list mapping to each element, then concats all of the resulting lists.
    MonadPlus is a typeclass for monads that are also monoids.
    Control.Monad.State is a useful wrapper for implementing stateful operations (e.g. stack push/pop) within do blocks.

    liftM is equivalent to fmap - it lifts a function so that it takes/returns a boxed value.
    join flattens nested monads recursively. i.e. m (m a) -> m a
    filterM - like filter, but the filter function and result have monadic values. e.g. using lists means that we can non-deterministically include/reject different elements.
    foldM - like fold, but with a monadic result for the combination function.




http://learnyouahaskell.com/for-a-few-monads-more
