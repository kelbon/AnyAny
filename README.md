## AnyAny
C++20 library for efficient dynamic polymorphism through type erasure.

Goals are efficiency, understandability and extensibility.

[![Clang](
https://github.com/kelbon/AnyAny/actions/workflows/clang.yml/badge.svg?branch=main)](
https://github.com/kelbon/AnyAny/actions/workflows/clang.yml)
[![GCC](
https://github.com/kelbon/AnyAny/actions/workflows/gcc.yml/badge.svg?branch=main)](
https://github.com/kelbon/AnyAny/actions/workflows/gcc.yml)
(MSVC works too)

Also there are tools for polymorphism such as multidispatching [`visit_invoke`](#visit_invoke) or [`type_switch`](#type_switch)

### See /examples folder for fast start!

* [`Design and understanding`](#design)
* [`How to build?`](#build)

## Basic usage example

Foundation of library'a type erase part is a _Methods_ - a description which part of the type we want to use after erasing.

Let's create one for erasing types with `void draw()`:

```C++
// For each type T do value.draw()
template<typename T>
struct Draw {
  static void do_invoke(const T& self) {
    self.draw();
  }
};
```

We can use `Draw` for type erasion:

```C++
#include "anyany.hpp"
using any_drawable = aa::any_with<Draw>;
```

And now we can use `any_drawable` to store any type with .draw()

```C++
// some types with .draw()
struct Circle {
  void draw() const {
    std::cout << "Draw Circle\n";
  }
};
struct Square {
  void draw() const {
    std::cout << "Draw Square\n";
  }
};

int main() {
  any_drawable shape = Circle{};
  aa::invoke<Draw>(shape); // prints "Draw Circle"
  shape = Square{};
  aa::invoke<Draw>(shape); // prints "Draw Square"
  // see /examples/basic_usage.hpp for more
}
```

There are no virtual functions, inheritance, pointers, memory management etc! Nice!


You can add any number of _Methods_:
```C++
using any_my = aa::any_with<Draw, Run, aa::copy, aa::move>;
```

Wait, copy... move? Yes, by default any is only have a destructor, so you can create move only any or ... copy only etc

Predefined _Methods_:

<details>
  <summary>copy</summary>
  
  enables copy constructor
</details>

<details>
  <summary>move</summary>
  
  enables move constructor, move assignment (and copy assignment if aa::copy method presented)
  
  Note: move constructor for `any_with` always noexcept
</details>

<details>
  <summary>hash</summary>
  
  enables `std::hash` specialization for `any_with` and `poly_ref`. If `any_with` is empty, then hash == 0.
  
  Note: `poly_ptr` has specialization of `std::hash` by default, but it is pointer-like hash.
  
</details>
<details>
  <summary>equal_to</summary>
  
  enables `operator==` for `any_with`.
  
  Two `any_with` objects are equal if they contain same type(or both empty) and stored values are equal.
</details>

<details>
  <summary>spaceship</summary>
  
  enables `operator<=>` and `operator==` for erased `any_with`.
  
  Always returns `std::partial_ordering`.
  
  If two `any_with` objects do not contain same type returns `unordered`, otherwise
  returns result of `operator <=>` for contained objects.
  
  Note: `equal` if both empty
  
</details>

<details>
  <summary>call&ltR(Args...)&gt</summary>
  
  adds `R operator()(Args...)` for `any_with` and `poly_ref`
  
  Note: also supported `const`, `noexcept` and `const noexcept` signatures
  
  example:
  ```C++
 
  template<typename Signature>
  using cpp23_function_ref = aa::poly_ref<aa::call<Signature>::template method>;

  cpp23_function_ref<int(float) const> fooref = foo;

  ```
</details>

<details>
  <summary>destroy</summary>
  
  Invokes destructor. `any_with` has it by default, but maybe you want to use it with `aa::poly_ptr` to manually manage lifetime
</details>

<details>
  <summary>copy_with&ltAlloc, SooS&gt</summary>
  
  used for creation copyable `basic_any_with` with custom `Allocator`
  
  example:
  ```C++
  // 16 here is a Small Object Optimization buffer size
  using my_any_with_alloc = aa::basic_any_with<MyAlloc, 16, aa::copy_with<MyAlloc, 16>::template method>;
  ```

</details>

<details>
  <summary>More formally about Methods</summary>
  
_Method_ is a template class/alias with one type argument, such that specializations of _Method_ for each type `T` contains addressable static method `do_invoke`

```C++
template<typename T>
struct MethodName {
  static Ret do_invoke(SelfType, Args...);
};
```
  * `SelfType` may be `T&` or `const T&` or `T` (copy `T` for each invoke of _Method_)
  * `Ret` and `Args...` must be same for all specializations of _Method_
 
_Method_ is a _const Method_ if `SelfType` is not `T&`.
_const Method_ may be called even when erased object is const qualified
</details>

#

<details>
  <summary>About type descriptors</summary>

There are `descriptor_t` and `descriptor_v<T>`, they are used to describe static or dynamic types of objects.

You need to use it only If you want to create custom poly traits.

</details>

Polymorphic types:

* [`any_with<Methods...>`](#any_with)
* [`basic_any_with<Methods...>`](#basic_any_with)
* [`poly_ref<Methods...>`](#poly_ref)
* [`poly_ptr<Methods...>`](#poly_ptr)
* [`const_poly_ref<Methods...>`](#const_poly_ref)
* [`const_poly_ptr<Methods...>`](#const_poly_ptr)

Actions:

* [`any_cast<T>`](#any_cast)
* [`invoke<Method>`](#invoke)
* [`invoke_unsafe<Method>`](#invoke_unsafe)
* [`type_switch`](#type_switch)
* [`visit_invoke`](#visit_invoke)

Polymorphic containers:

* [`variant_swarm<Ts...>`](#variant_swarm)
* [`data_parallel_vector<T, Alloc>`](#data_parallel_vector)

<details>
  <summary>Compile time information</summary>

```C++
// true if type created by any_with, basic_any_with or inherits from such type
template <typename T>
concept any_x = /*...*/;

// true if type can be used as poly traits argument in any_cast / type_switch / visit_invoke etc
template <typename T>
concept poly_traits = requires(T val, int some_val) {
                        { val.get_type_descriptor(some_val) } -> std::same_as<descriptor_t>;
                        { val.to_address(some_val) };
                      };
```
You can define traits for your polymorphic hierarchies(LLVM-like type ids, virutal functions etc).

Library has two such traits built-in(you can use them as example for implementing your own):

  * anyany_poly_traits - for types from <anyany.hpp>
  * std_variant_poly_traits - for std::variant

</details>


<details>
  <summary>Interface features</summary>

For example, you want to have method .foo() in type created by `any_with` or `poly_ref` with your _Method_.

Then you should use `plugins`:
```C++
template <typename T>
struct Foo {
    static void do_invoke(T&) { /* do something*/ }
   
    // any_with<Foo> will inherit plugin<SELF>
 
    template <typename CRTP>
    struct plugin {
      void foo() const {
        auto& self = *static_cast<const CRTP*>(this);
        // Note: *this may not contain value, you can check it by calling self.has_value()
        aa::invoke<Foo>(self);
      }
    };
};

//
any_with<Foo> val = /*...*/;
val.foo(); // we have method .foo from plugin!

```

If your _Method_ have inner alias `using explicit_interface = /*anything*/`,
then all types before inserting into `any_with` will be checked.

They must explicitly satisfy _Method_: `using satisfies = aa::satisfies<Method1, Method2...>;`

Example:

```C++
template<typename T>
struct Foo {
 static void do_invoke(T&) {}

 using explicit_interface = int;
};
struct BadType { };
// error - BadType do not satisfies Foo
// any_with<Foo> x = BadType{};

struct GoodType {
  using satisfies = aa::satisfies<Foo>;
};

// all works
any_with<Foo> y = GoodType{};

```

</details>

### `any_with`
Accepts any number of _Methods_ and creates a type which can hold any value, which supports those _Methods_. Similar to runtime concept

<details>
<summary>
Interface of created type
</summary>

```C++
// all interface from method's plugins(see basic_usage in /examples)

// All constructors and move assign operators are exception safe
// move and move assign always noexcept

struct Any {
  // aliases to poly ref/ptr
 
  using ptr = /*...*/;
  using ref = /*...*/;
  using const_ptr = /*...*/;
  using const_ref = /*...*/;

  // operator & for getting poly ptr
 
  poly_ptr<Methods...> operator&() noexcept;
  const_poly_ptr<Methods...> operator&() const noexcept;
  
  // main constructors
  
  // creates empty
  constexpr Any();
  Any(auto&& value); // from any type
 
  Any(const Any&) requires aa::copy
  Any& operator=(const Any&) requires aa::move and aa::copy

  Any(Any&&) noexcept requires aa::move;
  Any& operator=(Any&&) requires method aa::move;
  
  // observers
  
  bool has_value() const noexcept;
  
  // returns true if poly_ptr/ref to *this will not be invalidated after moving value
  bool is_stable_pointers() const noexcept
 
  // returns descriptor_v<void> if value is empty
  type_descriptor_t type_descriptor() const noexcept;
 
  // forces that after creation is_stable_pointers() == true (allocates memory)
  template <typename T>
  Any(force_stable_pointers_t, T&& value);

  // also emplace constructors(std::in_place_type_t<T>), initiaizer list versions
  // same with force_stable_pointers_t tag etc etc
  // same with Alloc...

  // modifiers
  
  // emplaces value in any, if exception thrown - any is empty(use operator= if you need strong exception guarantee here)
  template<typename T, typename... Args>
  std::decay_t<T>& emplace(Args&&...); // returns reference to emplaced value

  template <typename T, typename U, typename... Args>
  std::decay_t<T>& emplace(std::initializer_list<U> list, Args&&... args)

  // postcondition: has_value() == false
  void reset() noexcept; 

  // see aa::equal_to description for behavior
  bool operator==(const Any&) const requires aa::spaceship || aa::equal_to;
  // see aa::spaceship description for behavior
  std::partial_ordering operator<=>(const Any&) const requires aa::spaceship;
  
  // ... interface from plugins for Methods if presented ...
};

```
</details>

All constructors and copy/move assignment operators have strong exception guarantee

Note: if your type has noexcept move constructor, it can really increase perfomance(like in std::vector case).

Example:
```C++
using any_printable = aa::any_with<Print, aa::move>;
```

### `basic_any_with`
Same as `any_with`, but with custom alloc and small object optimization buffer size - if you need a copyable `basic_any_with` use `copy_with`

```C++
template<typename Alloc, size_t SooS, TTA... Methods>
using basic_any_with = /*...*/;
```

### `poly_ref`
Non owning, always not null, lightweight(~=void*)

<details>
<summary>Interface</summary>

```C++
template <template<typename> typename... Methods>
struct poly_ref {
  poly_ref(const poly_ref&) = default;
  poly_ref(poly_ref&&) = default;
  poly_ref& operator=(poly_ref&&) = default;
  poly_ref& operator=(const poly_ref&) = default;
  
  // only explicit rebind reference after creation
  void operator=(auto&&) = delete;
  
  descriptor_t type_descriptor() const noexcept;
 
  // from mutable lvalue
  template <not_const_type T> // not shadow copy ctor
  poly_ref(T& value) noexcept
  poly_ptr<Methods...> operator&() const noexcept;
  
  // ... interface from plugins for Methods if presented ...
}
```
</details>

### `const_poly_ref`
Same as `poly_ref`, but can be created from `poly_ref` and `const T&`

Note: do not extends lifetime

### `poly_ptr`
Non owning, nullable, lightweight(~=void*)

Note: `poly_ptr` and `const_poly_ptr` are trivially copyable, so `std::atomic<poly_ptr<...>>` works.

<details>
<summary>
Interface
</summary>

```C++
template <template<typename> typename... Methods>
struct poly_ptr {
  poly_ptr() = default;
  poly_ptr(std::nullptr_t) noexcept;
  poly_ptr& operator=(std::nullptr_t) noexcept;

  poly_ptr(not_const_type auto* ptr) noexcept;
 
  // from non-const pointer to Any with same methods
  template <any_x Any>
  poly_ptr(Any* ptr) noexcept;
  
  // observers
 
  // returns raw pointer to value
  void* raw() const noexcept;
  
  // returns descriptor_v<void> is nullptr
  descriptor_t type_descriptor() const noexcept;
  
  bool has_value() const noexcept;
  bool operator==(std::nullptr_t) const noexcept;
  explicit operator bool() const noexcept;

  // similar to common pointer operator* returns reference
 
  poly_ref<Methods...> operator*() const noexcept;
  const poly_ref<Methods...>* operator->() const noexcept;
}
```
</details>

### `const_poly_ptr`
Same as `poly_ptr`, but can be created from `poly_ptr` and `const T*` / `Any*`

### `any_cast`

Functional object with operator():

* any_cast\<T\>(any_x|poly_ref) -> std::remove_cv_t\<T\>
* any_cast<T&>(any_x|poly_ref) -> T&
* any_cast\<T\>(any_x*|poly_ptr) -> std::remove_reference_t\<T\>*

Version which returns pointer returns nullptr, if dynamic type is not T (ignores const/volatile etc)

Other versions throws std::bad_cast on failure

Example:
```C++
using any_comparable = aa::any_with<aa::copy, aa::spaceship, aa::move>;

void Foo() {
  any_comparable value = 5;
  value.emplace<std::vector<int>>({ 1, 2, 3, 4}); // constructed in-place
  // any_cast returns pointer to vector<int>(or nullptr if any do not containts vector<int>)
  aa::any_cast<std::vector<int>>(std::addressof(value))->back() = 0;
  // version for reference
  aa::any_cast<std::vector<int>&>(value).back() = 0;
  // version which returns by copy (or move, if 'value' is rvalue) 
  auto vec = aa::any_cast<std::vector<int>>(value);
}
```
### `invoke`
Functional object with operator().

Accepts polymorphic value or reference and arguments. Invokes Method.

Throws aa::empty_any_method_call if value was empty

Has method .with(MethodArgs...) which binds arguments and returns invocable for passing to algorithms

Example:
```C++
void foo(std::vector<aa::poly_ref<Foo>> vec) {
  std::for_each(vec, aa::invoke<Foo>);
}
```
If Any is const, then only **const Methods** permitted (method are const if Self is const T* / const T& / T (accepted by copy))
(call `destroy` method if forbidden)
Throws aa::empty_any_method_called if !any.has_value();
Example: 
```C++
// See first example on top
  any_animal Pet = Cat{};
  aa::invoke<Say>(Pet, std::cout);

```

### `invoke_unsafe`
Same as `invoke`, but more effective and if any has no value -> undefined behavior (never throws empty_any_method_call)

### `type_switch`
Selects .case based on input arg dynamic type and invokes 'visitor' with this dynamic type or default function
Also supports poly_traits as second template argument, so it supports any type for which you have poly traits
```C++
interface of type switch object:
  .case_<T>(auto&& f) - invokes 'f' if input matches type
  .cases<Ts...>(auto&& f) - invokes 'f' if input matches any of Ts
  Result default(auto&& f) - invokes 'f' with input arg if no one 'case' succeeded
  Result default(Result t) - returns default value 't' if no one 'case' succeeded
  optional<Result> no_default() - returns std::nullopt if no one 'case' succeeded
 
  Result val = aa::type_switch<Result>(value)
      .case_<float>(foo1)
      .case_<bool>(foo2)
      .cases<char, int, unsigned char, double>(foo3)
      .default(15);
```
### `visit_invoke`
Its... runtime overload resolution! `aa::make_visit_invoke<Foos...>` creates overload set object with method `.resolve(Args...)`,
which performs overload resolution based on Args... runtime types.
This example is very basic, see also /examples/visit_invoke_example.hpp for more
Example:
```C++

std::string ship_asteroid(spaceship s, asteroid a);
std::string ship_star(spaceship s, star);
std::string star_star(star a, star b);
std::string ship_ship(spaceship a, spaceship b);

// Create multidispacter
constexpr inline auto collision = aa::make_visit_invoke<
  ship_asteroid,
  ship_star,
  star_star,
  ship_ship
>();
...
// Perform runtime overload resolution
std::optional<std::string> foo(any_with<A> a, any_with<B> b) {
  return collision.resolve(a, b);
}

```
  
### `design`
 
Library provides several abstractions:

  Type creators:
  
  * any_with<Methods...> - creates a polymorphic value type
  * basic_any_with<Alloc, size_t, Methods...> - creates a polymorphic value type with custom alloc and SOO buffer size
  * const/poly_ref<Methods...> - creates a const/polymorphic reference type
  * const/poly_ptr<Methods...> - creates a const/polymorphic pointer type
**Polymorphic value, reference and pointer** - similar to just **T, T& and T***, but for polymorphic context.
  
 And several actions on these abstractions:
  * invoke\<Method\> - accepts reference or polymorphic value and arguments, invokes a method
  * any_cast\<T\> - accepts a pointer/ref/value and tryies to cast it into non polymorphic pointer/reference/value
  
Polymorphic value - is a type erased storage for one value(or empty)

Can be constructed from:
  * non polymorphic value, if type satisfies requirements (Methods)
  * other polymorphic value with same methods
  
Reference(aa::poly_ref) cannot be null and cannot be rebinded to another value after creating

Can be constructed from:
  * non-polymorphic value, if type satisfies requirements (Methods)
  * from polymorphic pointer(operator*)
  
Pointer(aa::poly_ptr) +-nullable polymorphic reference

Can be constructed from:
  * pointer to non polymorphic value, if type satisfies requirements (Methods)
  * polymorphic reference (operator&)
  * polymorphic value (operator&)
  * pointer to polymorphic value
  
Also there are casts poly_ptr -> const_poly_ptr / poly_ref -> const_poly_ref, similar to T&->const T& / T*->const T* too

These general concepts are enough to get started with the example

You want to create a `Machine` type which `engine` can be changed on runtime. Engine in this case is a **polymorphic value**

Classic way to do it(bad):
```C++
class IEngine {
public:
  virtual void Go() = 0;
  virtual ~IEngine() = default;
};

class Machine {
  IEngine* m_engine;
  // How to copy it? Move? Move/copy assign, destroy?
  // unique_ptr is not a solution, it is not a copy constructible, needs a constructors and some hand memory management
  // shared_ptr also do not work, all machines will use same engine, again useless allocations 
}
```
Seems like we dont want a **pointer** to polymoprhic value here, but we need an engine - **polymorphic value**.

it is obvious that the approach with virtual functions does not express our intentions in the code, which means that it becomes much more difficult for us and the compiler to understand what is happening in it.

Whats a solution?

In ideal world it must be something like: 
```C++

struct Machine {
  any_engine m_engime;
};

```
And it is possibile! But usually it is SO hard to write such type, that nobody do it!
This is why this library was created, it providies an instruments to create such types as fast and flexible as it possible
  
  ```C++
  template<typename T>
  struct Go {
    static int do_invoke(T& self, int value) {
      return self.go(value);
    }
  }
  
  using any_engine = aa::any_with<Go, aa::copy, aa::move>;
  // All ready to use!
  ```
Or another case : we want to create 'print' function, which prints all arguments

Typical way to implement it(bad):
```C++
  // now we have one function for every set of Ts..., for example different print for <int, double> and <double, int>
  // so we want to type erase it
  template<typename... Ts>
  void print(const Ts&... args) {
    (std::cout << ... << args);
  }
  int main() {
    print(5, 10, std::string{"abc"}, std::string_view{"hello world"});
  }
```
  How to reduce count of 'print's ? (and binary file size)? 
  With anyany we can easily erase each type only once and use same 'print' function for all of them
  
```C++
template <typename T>
struct Print {
    static void do_invoke(const T& self) {
      std::cout << self;
    }
};
// we can remove init list here, but its just an example
void print(std::initializer_list<aa::const_poly_ref<Print>> list) {
  // aa::invoke is a functional object with operator()
  std::ranges::for_each(list, aa::invoke<Print>);
}
int main() {
  print({5, 10, std::string{"abc"}, std::string_view{"hello world"}});
}
```
And even more - now the function in the signature explicitly indicates which methods it needs
 
## Using with CMake
1. Clone this repository into folder with your project
2. Add these lines to it's CMakeLists.txt
  
```CMake
add_subdirectory(AnyAny)
target_link_libraries(MyProjectName PUBLIC anyanylib)
```
### `build`
  
```shell
git clone https://github.com/kelbon/AnyAny
cd AnyAny
cmake . -B build
cmake --build build
```
