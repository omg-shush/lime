# Lime

### A functional, dynamically-typed language loosely inspired by Lisp and F#.

## The Basics

    "Hello world!" -> System.Console.WriteLine
    
Strings are strings. The "->" operator takes its left hand side as input and
feeds it into the right hand side---a function call.

    System.Console.ReadLine! -> System.Console.Write
    
I/O is handled by interoperating with the .NET standard library. Methods under
the System namespace are dynamically accessed through reflection, with Lime data
converted to and from .NET data as necessary. Currently limited to static
methods (since objects can't be represented in Lime yet), with signatures
using only the primitives bool, int64, double, string, and char, that don't
require generics/casting. System.Console.Read/Writes and System.Math work well.

The ! is sugar for "unit -> NoArgumentFunction".

    Loop = [ subroutine ]
        "I'm in a loop!" -> System.Console.WriteLine
        Loop!
    Loop!
    
Loop is a function that prints and then calls itself. The function body is
delimited by indentation; **use spaces only**. Note that "Loop!" appears twice;
the first makes the function always call itself, while the second actually
invokes Loop for the first time.

    TwoArgs = [ subroutine ]
        arg1, arg2 = in
        (arg1 + ", " + arg2) -> System.Console.WriteLine
        
    ("nice", "meme") -> TwoArgs
    
Tuples are constructed by separating their elements with commas ",". Note that
parentheses are optional unless required due to nesting or operator precedence
(in this case, since "->" has a very high precedence, the parentheses
are necessary).
Functions can access their input through the keyword "in." If a function takes
multiple arguments, it can "pattern-match" within an assignment statement:
assuming "in" is a 2-tuple, "arg1" gets the first value and "arg2" gets
the second value.

    ("I'm"; "the"; "same"; "list")
    ("I'm", ("the", ("same", ("list", unit))))
    
Lime uses Lisp-style lists: a list is either unit, or a 2-tuple of the head of
the list and the tail. The semicolon notation is just sugar for hardcoding
lists.

    WriteAllLines = [ subroutine ]
        if in == unit then
            unit
        else
            head, tail = in
            head -> System.Console.WriteLine
            tail -> WriteAllLines
            
    ("Here"; "I"; "come"; "I"; "am"; "cinammon!!!") -> WriteAllLines
    
As in most functional languages, if-then-else is an expression: all branches
must be present. Note parentheses in the condition are unnecessary. Also,
"if-then-else if-then-else" works for an arbitrary number of "-else if-" cases.

This is a classic recursive function: the base case, where it is given the empty
list, causes it to do nothing (by returning unit). In the recursive case, it
takes the top item off the list, does some work with it (printing), and then
recurses with the rest of the list. **In functions like this, it's important to
wait until you know "in" is not unit before attempting to pattern match on it,
as pattern matching on a non-tuple will result in a runtime error.**

    list = ("Here"; "I"; "come"; "I"; "am"; "cinammon!!!")
    
    ListIter = [ subroutine ]
        iterFunc, list = in
        if list == unit then
            unit
        else
            head, tail = list
            head -> iterFunc
            (iterFunc, tail) -> ListIter
    
    (System.Console.WriteLine, list) -> ListIter
    
All hail higher-order functions! This language is dynamically typed, but its
type would be ListIter: (iterFunc: 'a -> unit) * 'a list -> unit. It
runs iterFunc on every item in the list, in order. The function we pass in can
be a .NET method, as shown above, or any old [ subroutine ] we write.

    List = [ module ]
        Empty = unit
        Iter = [ subroutine ]
            ...
        Fold = [ subroutine ]
            ...
    
Modules can be used to organize related functions and values.

    Read = [ subroutine ]
        index = in
        nextLine = System.Console.ReadLine!
        if nextLine == unit then
            unit
        else
            ((index -> Integer.ToString) + ": " + nextLine), index + 1

    (System.Console.WriteLine, (Read, 0) -> Seq.Unfold) -> Seq.Iter
    
This is an example of using the higher-order function Seq.Unfold to read lines
of user input until end-of-stream.

## The Advanced
Compile Fifth.lime with the following libraries, in order: Bool List Integer Map Seq.
Run the progoram, type "1 2 + ." and hit enter.

    1 2 + .
    3 OK
    DONE
    
This is the beginnings of a Forth REPL (get it? Fifth?). The values 1 and 2 are
pushed onto the stack, in order; the word "+" adds the top two values on the
stack; and the word "." prints out the top value on the stack.

Currently, these are the only words known (arbitrary integers, +, and .), but it
won't be difficult to expand them. If you look at Fifth@RunLine, you'll see that
Forth code is parsed with a simple Seq.Unfold, and then executed with a
Seq.Fold. You'll also see how words are defined: the Map "BasicDictionary"
is a dictionary from Forth words like "+", ".", etc. to Lime functions that
implement those words---definitions. These definitions, PrintVal and Add, mostly
just modify the stack. Adding more words is as simple as (a) writing a new Lime
function to implement it, and (b) inserting it as a definition in the
BasicDictionary.

## Compiling & Running

Compiling files individually allows you to write code faster, since
only the files you've changed need to be recompiled. They compile into a binary
format that mirrors the code's representation in memory---it's fast to load,
but takes up much more space on disk. Note that compilation doesn't check
if symbols are valid, so if code depends on other files, those don't need to be specified.

    dotnet run --project path/to/limec.fsproj -- "Code.lime -> Code.ast" --target=ast
    
    dotnet run --project path/to/limec.fsproj -- "FirstLib.lime -> FirstLib.ast" --target=ast
    
    dotnet run --project path/to/limec.fsproj -- "SecondLib.lime -> SecondLib.ast" --target=ast
    
You can run either a raw code file or a precompiled AST. In either case, you can
specify additional "library" files to load before running the main program; this
is necessary for programs to use code from other files. Order matters: the
library files will be interpreted from left to right, before the main program.
If a library file has a dependency, that dependency must come **first** (to the
left) of it. Library files are specified by prepending "-L" to the file name.

    dotnet run --project path/to/limec.fsproj -- "Code.lime -> " --target=intr
    
    dotnet run --project path/to/limec.fsproj -- "Code.lime -> " --target=intr -LFirstLib.ast -LSecondLib.ast
    
    dotnet run --project path/to/limec.fsproj -- "Code.ast -> " --target=intr -LFirstLib.ast -LSecondLib.ast
