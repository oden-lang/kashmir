# The Language

## Packages

A *package* in Oden is the top level in which you write Oden source code.
Every `.oden` source file corresponds to a single package; a package cannot be
be spread over multiple source files. Every package must begin with a
*package declaration*. The `package` keyword is followed by a *fully
qualified package name*.

```{include=src/listings/syntax-package-declaration.html formatted=true}
```

The fully qualified name consists of one or more *segments* separated by
slashes.

```{include=src/listings/syntax-fully-qualified-name.html formatted=true}
```

### Import Declarations

After the package declaration follows zero or more *import declarations*.
Imports can be either *native* or *foreign*. A native import is an import of an
Oden package.

```{include=src/listings/syntax-import-native-declaration.html formatted=true}
```

A foreign import is written with the `foreign` keyword and a string literal for
the foreign package *import path*. The reason for this being a string is that
the Go specification [does not mandate the format of Go import
paths](https://golang.org/ref/spec#ImportPath).

```{include=src/listings/syntax-import-foreign-declaration.html formatted=true}
```

Imported packages are made visible in the scope of the importing package by an
*import alias*. For native packages, the last segment of the fully qualified
package name is used as the package alias. For native Go packages, the [package
name](https://golang.org/ref/spec#PackageName) specified in the Go *package
clause* is used, regardless of the import path. To access members of an
imported package you write the alias, a dot, and the member identifier.

```{include=src/listings/syntax-package-member-access.html formatted=true}
```

### Value Definitions

After package and import declarations comes *value definitions*. These can be
basic values like numbers or strings, but also functions. A value definition is
denoted by a name, an equals sign, and the expression to bind the name to.

```{include=src/listings/syntax-value-definition.html formatted=true}
```

The following program imports the `strconv` package from Go, defines a
function `shout` and a value `result`, and defines the *main* function, the
entry point of the program. The main function must be a function taking
no argument and returning `()`, i.e. have the type `(-> ())`. It must also be
defined in the *main* package.

```{.oden .playground-runnable language=oden include=src/listings/package-example.oden}
```

## Basic Literals

There are two types of number literals supported; `int` and `float64` literals.

```{.oden language=oden}
123
-588
0.00001
-400.123
```

There are two boolean literals, `true` and `false`, just as in Go.

```{.oden language=oden}
true
false
```

String literals are enclosed in double quotes.

```{.oden language=oden}
"hello"
""
"\nomg\nnewlines"
```

## Unit

*Unit* is a built-in type that is inhabited by a single value, the *unit
value*, which carries no information. Unit is returned by functions that cause
side effects and have no useful return value to give.

Both the unit type and the unit value literal are written with an empty set of
parenthesis. Here's how you define a function that returns unit and use
that function to perform a side effect.

```{.oden language=oden}
brewCoffee : Amount -> ()
brewCoffee(amount) = {
  // do the actual coffee brewing here

  // then return unit
  ()
}
```

Often the side effect functions you use already return unit, which means
you don't need to explicitly return unit like in the previous example.  In
the following example we use our `brewCoffee` function and then print to
the console. We return unit by ending with the application of the `println`
function, as it returns unit.

```{.oden language=oden}
coffeeBreak : -> ()
coffeeBreak() = {
  brewCoffee(twoCups)
  println("All right, ready to code again!")
}
```

Functions from Go, that has no return value, return unit when imported in Oden.
For example, the following Go function would have the type `string -> ()` in
Oden.

```{.go}
func sayHi(name string) {
  fmt.Println("Hi", name)
}
```

## Operators

There are two types of operators in Oden -- *unary* and *binary*.

Unary operators take one value as argument and are written before the value
expression. This is called *prefix notation*. Oden has only two unary
operators; negation and logical not.

```{.oden language=oden }
-1
!x
```

Binary operators take two values as arguments and are written in between
the value expressions. This is called *infix notation*. Let's look at some
binary operators:

```{.oden language=oden }
1 + 2
1 - 2
2 * 2
4 / 2
(100 - 50) / 2
100 * (50 + 25)
1 == 2
1 != 2
"Foo" ++ "Bar"
false && (true || (1 == 2))
```

During compilation operators are expanded in to predefined [*protocol method
applications*](#protocols). The following table shows what they expand to.

Operator  Protocol  Method
--------  --------  ------
`-`       Num       Negate
`!`       Logical   Not
`+`       Num       Add
`-`       Num       Subtract
`*`       Num       Multiply
`/`       Num       Divide
`&&`      Logical   Conjunction
`||`      Logical   Disjunction
`++`      Monoid    Apply

With operator expansion we can *overload* all operators with our own
types, as they are aliases for protocol methods. For more information on
overloading see ["Overloading Operators"](#overloading-operators).

### Using Operators As Function Values

Currently it is not possible to use these operators as first-class values,
passing them to functions or using them in a let, e.g. `map(+, numbers)`. You
can however wrap them in a standard function and pass that to a
higher-order function.

## Functions

Functions are the bread and butter of any functional programming language.
Functions map input values to output values and are used to structure a program
and to create reusable abstractions. If you know what the function
*uppercase* does, there's no need for you to know *how* it does it. You can use
it to get your strings uppercased without worrying about the implementation.

In Oden functions are first-class entities. You can create them on the fly
and pass them around as values. A *function expression* is written using an
argument list followed by an arrow and a function body.

```{include=src/listings/syntax-function-expression.html formatted=true}
```

The argument list can contain zero or more arguments,
separated by commas and enclosed in parenthesis. The function body is a single
expression that gets evaluated when the function is applied.

The following function takes an argument `x` and applies the `+` operator to
`x` and the literal `1`, effectively incrementing the number.

```{.oden language=oden}
(x) -> x + 1
```

### Defining Functions

Functions can be defined at the top level just like any other value. We write
the name, an equals sign, and the function expression.

```
increment = (x) -> x + 1
```

However, as defining functions is such a common task, Oden provides a *function
definition shorthand*. Using the shorthand you write the function name, the
argument list, an equals sign and the function body.

```
increment(x) = x + 1
```

The two definitions of `increment` are equivalent.

### Function Application

Functions are applied using parenthesis, contaning the parameters separated by
commas.

```{include=src/listings/syntax-function-application.html formatted=true}
```

Here we first define a function `square` and then we apply the function to
the literal `4`.

```{.oden language=oden}
square(x) = x * x
squareOfFour = square(4)
```

We can also define functions that take no arguments. In the following
expression the function `makeNum` is applied with no argument to give us
some number. This is useful for deferring a computation until it's needed.

```{.oden language=oden}
makeNum() * makeNum()
```

### Recursion

Top-level functions can apply themselves recursively. There is currently no
[*tail call optimization*](https://en.wikipedia.org/wiki/Tail_call) being done
in Oden, so be careful with these.

```{.oden language=oden}
factorial(n) =
  if n < 2
    then 1
    else n * factorial(n - 1)
```

### Function Types

Functions, being values, have types. A function type denotes the argument type
(the *domain*) and the return value type (the *range*), separated by an arrow.

```{include=src/listings/syntax-function-type.html formatted=true}
```

When defining a value or a function it is recommended to add an explicit type
signature. Type signatures must be written before the definition. In the
following code we specify `increment` to have type `int -> int`.

```oden
increment : int -> int
increment(x) = x + 1
```

Types of functions that take no argument are written in a similar way -- you
just omit the parameter type before the arrow. The following code specifies the
type of our `makeNum` function.

```oden
makeNum : -> int
makeNum() = 3
```

If we only write the type signature and omit the definition, we will get a
compiler error.

```{.oden language=oden}
anotherFunction : forall a b. a -> b -> a
// definition is missing!
```

### Currying

Oden functions are [*curried*](https://en.wikipedia.org/wiki/Currying). The
type of a function that takes values of types `a` and `b`, returning a `c` can
be written `a -> b -> c`. But it can also be written as `a -> (b -> c)`, which
is how the compiler sees it.

The parenthesis indicate that the function has just one argument, of type `a`,
and return another function from `b` to `c`. But how can we write functions
that take multiple arguments? The answer is that we can't! The compiler
performs a trick to make it look like we can.

Oden lets you write function expressions with multiple arguments:

```{.oden language=oden}
(x, y, z) -> x
```

Function expression with multiple arguments gets expanded to nested functions,
each having only one argument.

```{.oden language=oden}
(x) -> (y) -> (z) -> x
```

The type of such a function expression is:

```{.oden language=oden}
forall a b c. a -> (b -> (c -> a))
```

As curried functions are central in Oden their types can be written without
paranthesis, with arrows between each nesting.

```{.oden language=oden}
forall a b c. a -> b -> c -> a
```

In other words, `a -> (b -> c)` and `a -> b -> c` are equivalent.

#### Using Currying

With currying you can apply a function to its first argument, get another
function back, and use that function later -- similar to [partial application](
https://en.wikipedia.org/wiki/Partial_application).  In the following code we
create function `personSays` that takes two strings.  We apply the function
with only one string and get a function back. Later we apply it with the other
string to actually print something.

```{.oden .playground-runnable language=oden}
package main

personSays(who, what) =
  println(who ++ " says: " ++ what)

simonSays = personSays("Simon")

main() = simonSays("write a program in Oden")
```

#### Go Functions

When applying a function imported from Go the function gets curried
automatically, so you can partially apply it just like you can with Oden
functions.

## Type Variables

Functions that take values of unknown types are called *polymorphic functions*,
and have *polymorphic types*. A polymorphic type contains one or more *type
variables*. These are like placeholders for the specific types to be used
later.

Type variables are introduced into the scope of a type signature by using the
`forall` keyword, followed by one or more type variable names, and a
terminating dot. This is called *universal quantification*.

```{.oden language=oden}
identity : forall a. a -> a
```

Multiple type variable names are separated by spaces.

```{.oden language=oden}
compose : forall a b c. (b -> c) -> (a -> b) -> (a -> c)
```

When applying a function to a value the function type is *instantiated* to
match the type of the value. If we apply the `identity` function to a value of
type `int`, the instantiated type of `identity` will be `int -> int`. In other
words, all occurences of the type variable `a` are substituted for `int`. These
substitutions must match. The type `a -> a` cannot be instantiated to `int ->
string`, as the type variable `a` cannot be unified with both `int` and
`string`.

## Control Flow

The only control flow mechanism available right now is the `if` expression.

```{.oden language=oden}
if 10 + 20 == 30 then 1 else 0
```

If the clauses get big you should consider using [blocks](#blocks) around
expressions, even if you have only a single expression in each block.

```{.oden language=oden}
if 10 + 20 == 30 then {
  thisFunctionCallIsVeryLengthy()
} else {
  andPerhapsThisOneAlso()
}
```

The `if` expression can be seen as having the following type.

```{.oden language=oden}
forall a. bool -> a -> a -> a
```

It takes a condition of type `bool`, a *then* value, an *else* value,
and evaluate to some of those two branch values. Therefore, the branch
values and the if expression itself must have the same type.

#### Other Constructs

The plan is to support pattern matching as a central way to do control flow.
Other constructs may appear in the future as well, like `cond` from LISPs or
*Guards* from Erlang and Haskell.

## Blocks

A block is an expression containing *one or more* expressions -- it cannot be
empty. A block expression evaluates to the value of the last expression in the
block. Blocks can be used to perform side-effects.

```{.oden language=oden}
x = {
  println("Calculating...")
  9 * 1000
}
// x will be 9000
```

Discarding the value of an expression, of any type other than `()`, causes an
error.

```{.oden language=oden}
x = {
  ()       // ok to discard as it's of type ()
  9 * 1000 // causes an error
  println("Done wasting CPU.")
}
```

## Let Bindings

The *let expression* binds identifiers to values, exposing the bindings in the
body expression.

```{include=src/listings/syntax-let.html formatted=true}
```

The following example binds the identifier `x` to the value `1` and uses the
binding in its body expression `x + 2`.

```{.oden language=oden}
let x = 1 in x + 2
```

Let supports sequential binding, which means that binding expressions can use
the identifiers of previous bindings.

```{.oden language=oden}
let x = 1
    y = 1 + x
in y / 2
```

Shadowing names is forbidden, and will result in a compile error.

```{.oden language=oden}
let x = 1
    x = x * 2 // not ok as x is already defined
in x * 2
```

## Tuples

A *tuple* is an immutable finite ordered collection of values. The values in a
tuple can have different types. Tuples are useful for grouping related data
without having to name the values.

Tuple value literals are denoted using two or more expressions separated by
commas, enclosed in parenthesis.

```{include=src/listings/syntax-tuple-literal.html formatted=true}
```

In the following example we create some tuple values representing people
by storing the name and age as pairs.

```{.oden language=oden}
jessica = ("Jessica", 31)
frank = ("Frank", 26)
```

Tuple types are written in a similar way to tuple value literals. The types of
the tuple elements are separated by commas, and enclosed in parenthesis.

```{include=src/listings/syntax-tuple-type.html formatted=true}
```

Thus, the type of the values `jessica` and `frank`, from the previous example,
is written `(string, int)`.

### Go Functions With Multiple Return Values

Functions in Go can have multiple return values. When importing such a
function, Oden automatically converts it to return a single tuple value, with
the Go return values as elements. Thus, there is no special case of multiple
return values; all functions return one value.

## Slices

A *slice* is a collection of values typed only by the element type, not the
length of the collection. All values in a slice must have the same type.

Slice value literals are denoted using a left and a right square bracket, an
opening curly bracket, zero or more expressions separated by commas, and a
closing curly bracket.

```{include=src/listings/syntax-slice-literal.html formatted=true}
```

The following examples creates a slice of `string` values, called `names`, and
a slice of `int` values, called `numbers`.

```
names = []{"Sarah", "Joe", "Maxime"}

numbers = []{1, 2, 3, 4, 5}
```

To access the elements of a slice, we use the *subscript operator*. It is
denoted in suffix position using an index expression enclosed in square
brackets. The index expression must have type `int`.

```{include=src/listings/syntax-subscript.html formatted=true}
```

Slices indexes are *zero-based*, meaning the first element has index 0, the
second element has index 1, and so on.

The following example greets the first person in the slice of names.

```
greeting = "Hello, " ++ names[0]
```

Slices can be multi-dimensional. To access the inner elements, simply apply the
subscript operator multiple times. The following example creates a
multi-dimensional slice and access an inner element.

```
twoLevelSlice = []{[]{1, 2, 3}, []{4, 5, 6}}
isSix = twoLevelSlice[1][2] == 6
```

## Records

*Records* group related data into a composite structure using named elements,
called *fields*. They are similar to objects in Javascript, but are statically
typed by their fields. The order of fields in a record is not significant --
two records are considered equal if they have the same set of fields with the
same types.

To create a record you use curly brackets enclosing a comma-separated list of
field names and values to initialize the record value with. Each field name
and value is separated by an equals sign.

```{include=src/listings/syntax-record-literal.html formatted=true}
```

The following example creates a record value representing a player in a video
game. Note that the field `attack` is itself of a record type.

```{.oden language=oden}
player = {
  health = 100,
  attack = {
    cooldown = 3,
    damage = 5
  },
  armor = 30
}
```

A record type is denoted by a set of field name and type pairs, with colons in
between the names and types. The fields are separated by commas and enclosed in
curly brackets.

```{include=src/listings/syntax-record-type.html formatted=true}
```

The type of the value `player` from the previous example is:

```{.oden language=oden}
{
  health : int,
  attack : {
    cooldown : int,
    damage : int
  },
  armor: int
}
```

### Record Fields

Fields of records can be accessed using the dot operator.

```{include=src/listings/syntax-record-field-access.html formatted=true}
```

Continuing with our game theme, here's a function taking a player record
value and accessing its field `attack`, and the nested `damage` and
`cooldown` fields.

```{.oden language=oden}
damagePerMinute(p) =
  p.attack.damage * 60 / p.attack.cooldown
```

The type of `damagePerMinute` is inferred to take an argument `p` of the
following type.

```{.oden language=oden}
{
  attack : {
    damage : int,
    cooldown : int
    | b
  }
  | a
}
```

Here `a` and `b` are *row variables*. But what is a row variable? And what
does the pipe character mean in a record type? Read on and you will learn more
about them in the next section.


### Polymorphic Records and Row Variables

A record type can be polymorphic, i.e. it can contain an unknown set of
fields. This is useful when you want to write a function taking record
values having certain fields, but you don't care they have extra fields or not.
This can be expressed in the type system using a *row varible*. The row
variable captures any extra fields when the type is instantiated.

To denote a polymorphic record type, write a pipe character and the row
variable after the set of fields.

```{include=src/listings/syntax-record-type-extension.html formatted=true}
```

The following type signature says that `getName` takes any record with at least
the field `name` with type `string`. When instantiated with a concrete record
type the row variable `r` will be bound to a row all fields of the record
except `name`.

```
getName : forall r. { name : string | r } -> string
```

For more information on these concepts, see @gaster1996polymorphic and
@leijen2005extensible.

## Protocols

In Oden, and in funtional programming in general, you can go a long way with
data and functions. Sometimes, however, it is useful to be able to create
functions taking arbitrary data types as arguments, as long as they follow
certain contracts. In Oden we use *protocols* to encode such contracts.

A protocol has a name contains a set of zero or more *methods*. A *protocol
definition* is denoted using the `protocol` keyword, the protocol name, and the
set of methods, enclosed in curly braces. Methods are names and type
signatures, similar to top-level type annotations.

```{include=src/listings/syntax-protocol-definition.html formatted=true}
```

Given a defined protocol you can *implement* the protocol for a specific
type. The implementation is denoted using the `impl` keyword, the protocol name
followed, by the implementation type enclosed in parenthesis. Inside the curly
braces the methods are implemented using the same syntax as top-level
definitions.

```{include=src/listings/syntax-protocol-implementation.html formatted=true}
```

When the compiler encounters the use of a protocol method together with a data
type it checks if there's an implementation in scope that it can use. If
not, you will get a compile error. If there is *a single implementation*,
that one will be used in the compiled program. The dispatch is done at
compile time.

Let's use protocols to make some animal sounds.

```{.oden .playground-runnable language=oden}
package main

protocol Animal(a) {
  sound : a -> string
}

type Dog = { friendly : bool }
type Chicken = { female : bool }

impl Animal(Dog) {
  sound(dog) =
    if dog.friendly
      then "Woof!"
      else "Grrrrr..."
}

impl Animal(Chicken) {
  sound(chicken) =
    if chicken.female
      then "Cluck!"
      else "Cockadoodledoo!"
}

main() = {
  println(Animal::sound({ friendly = false }))
  println(Animal::sound({ female = true }))
}
```

#### Work In Progress

As Oden has only transparent type aliases right now, it is unclear in this
code that we create a `Dog` value and a `Chicken` value. Have patience; more
type constructs will arrive.

As you might have noticed, methods have to be fully qualified with the
`Protocol::Method` syntax. This restriction might be lifted in the future,
enabling you to write only the method name, if it can be unambigously
resolved.

### Overloading Operators

Protocols are used in Oden to provide *overloaded operators*. The built-in
operators are aliases for standard protocols methods, such as `Num`,
`Equality`, and `Monoid`. As you can implementation a protocol for a data
type, you can effectively overload any built-in operator for that data type.

In the following example we implement the `Monoid` protocol for our `Point2D`
data type, which enables us to use the `++` operator to add `Point2D` values.

```{.oden .playground-runnable language=oden}
package main

// A point in 2D space.
type Point2D = { x: int, y: int }

// We implement the built-in Monoid protocol
// for Point2D.
impl Monoid(Point2D) {
  Apply(p1, p2) = {
    x = p1.x + p2.x,
    y = p1.y + p2.y
  }
  Identity = { x = 0, y = 0 }
}

// Let's create some points.
position = { x = 1, y = 3 }
distance = { x = 5, y = 7 }

// With the implementation in scope we can
// now use the ++ operator together with
// our Point2D type.
target : Point2D
target = position ++ distance

main() = {
  println(target)
}
```

### Compatibility with Go

Protocols are not compatible with Go interfaces yet.

### Influences

The concept of protocols is not new, in fact it is very similar to [_type
classes_](https://www.haskell.org/tutorial/classes.html) in Haskell and
[_traits_](https://doc.rust-lang.org/book/traits.html) in Rust.
