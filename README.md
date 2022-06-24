# AnyAny
C++20 library for comfortable and efficient dynamic polymorphism

[![Clang](
https://github.com/kelbon/AnyAny/actions/workflows/clang.yml/badge.svg?branch=main)](
https://github.com/kelbon/AnyAny/actions/workflows/clang.yml)
[![GCC](
https://github.com/kelbon/AnyAny/actions/workflows/gcc.yml/badge.svg?branch=main)](
https://github.com/kelbon/AnyAny/actions/workflows/gcc.yml)
(MSVC have bugs, which are already reported, you can use Visual Studio 2022 clang-cl now)

This is a library for dynamic polymorphism through type erasure with better code readability and reusage, performance, far less boilerplate then with usual way (virtual functions).
* [`Design and understanding`](#design)
* [`How to build?`](#build)

### Basic design knowledge (please read this example first)

The whole library is built on **Methods**, it is a short description of _captured_ method - template with one argument
and static function `do_invoke(Self, Args...)` where `Self` - value from which method will be invoked and `Args` is a _captured_ method arguments.

For example, i want to create a type to store any other type with .say() method:

```C++

// Let's write a Method - description for a library what type must have to be stored in our type and how to invoke it
template<typename T>
struct Say {
  static void do_invoke(const T& self, std::ostream& out) {
    self.say(out);
  }
};
// Self can be const T& / T& / const T* / T* or just T - in this case self will be provided by copy -
// (Self also can be cv void* but... even in this case, under it always lies an object of type T)

// Create a type which can contain any other type with method .say !
using any_animal = aa::any_with<Say>;

// Let's use it!
struct Cat {
  int field;
  
  void say(std::ostream& out) const {
   out << "Meow\n";
  }
};
struct Dog {
  std::string field;
  
  void say(std::ostream& out) const {
   out << "Woof!\n";
  }
};
int main() {
  any_animal Pet = Cat{};
  // there are several ways to invoke - external and internal.
  aa::invoke<Say>(Pet, std::cout); // external way
  // internal - if you write a plugin (see basic_usage.hpp in examples)
  // Pet.say(std::cout); // just an any other type
}
```
There are no virtual functions, inheritance, pointers, memory management etc! Nice!
It is modular and flexible:
You can add any number of methods like
`using any_my = aa::any_with<Say, Draw, Float, aa::copy, aa::move>;`

Wait, copy... Move? Yes, by default any is only have a destructor, so you can create move only any or ... copy only etc

Note: result type of do_invoke and Args must be same for all types T (as for virtual functions)

Type creators:
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

Compile time information:
* [`method_traits<Method>`](#method_traits)

Interface plugins and interface requirement:
* [`plugin`](#plugins)
* [`explicit_interface`](#explicit_interface)
### Methods

* [`destroy`](#destroy)
* [`copy`](#copy)
* [`move`](#move)
* [`RTTI`](#rtti)
* [`hash`](#hash)
* [`equal_to`](#equal_to)
* [`spaceship`](#spaceship)

### `any_with`
It is a template alias which accepts any number of Methods and creates a type which can hold any value, which supports those Methods. Its like a concept but in runtime.

Created type is a **polymorphic value type** (see [`design`](#design)) and has inner alliases for ptr, ref, const_ptr, const_ref (see [`design`](#design))

Interface of created type:
```C++
// all interface from method's plugins(see basic_usage in /examples)

// All constructors and move assign operators are exception safe
// move and move assign always noexcept

// creates empty poly value
default constructor

constructor(auto&& value); // from any type
constructor(Alloc)
constructor(std::allocator_arg_t, Alloc alloc, T&& value); // from any value, but with Alloc

copy constructor(if aa::copy Method used)

copy assign operator (if aa::copy AND aa::move Methods are used)

move constructor and move assign operator(if aa::move Method used)

// poly_ptr/poly_ref/const_poly_ptr/const_poly_ref
using ptr = /*...*/;
using ref = /*...*/;
using const_ptr = /*...*/;
using const_ref = /*...*/;

// creating with emplace
template <typename T, typename... Args>
constructor(std::in_place_type_t<T>, Args&&... args);

template <typename T, typename U, typename... Args>
constructor(std::in_place_type_t<T>, std::initializer_list<U> list, Args&&... args);

bool has_value() const noexcept; // true if not empty
// returns sizeof of type currently in Any, (0 if empty)
std::size_t sizeof_now() const noexcept;

// emplaces value in any, if exception thrown - any is empty(use operator= if you need strong exception guarantee here)
template<typename T, typename... Args>
std::decay_t<T>& emplace(Args&&...); // returns reference to emplaced value

template <typename T, typename U, typename... Args>
std::decay_t<T>& emplace(std::initializer_list<U> list, Args&&... args)

// after this method has_value() == false
void reset() noexcept; 

// presented if method aa::RTTI is used
const std::type_info& type() const noexcept;

// presented if aa::equal_to OR aa::spaceship Methods are used
bool operator==( ... ) const;

// presented if aa::spaceship
// Note: always returns std::partial_ordering, this means that two anyes can be unordered
std::partial_ordering operator<=>(...) const; 

poly_ptr<Methods...> operator&() noexcept;
const_poly_ptr<Methods...> operator&() const noexcept;

};

// Created type also has specialization of std::hash, if aa::hash method used!

```
**All constructors and copy/move assignment operators have strong exception guarantee**

**Important** - if your type has noexcept move constructor, it can really increase perfomance(like in std::vector case).

Example of type creation:

Creates type which is movable and satisfies user defined `Print` method:

`using any_printable = aa::any_with<Print, aa::move>;`

### `basic_any_with`
Same as `any_with`, but with custom alloc and small object optimization buffer size - if you need a copy method, then use **aa::copy_with<Alloc, SooS>::method**

interface:
```C++
template<typename Alloc, size_t SooS, TTA... Methods>
using basic_any_with = /*...*/;
```

### `poly_ref`
Non owning, always not null, lightweight(~=void*)

Interface:
```C++
template <template<typename> typename... Methods>
struct poly_ref {
  // cannot rebind reference after creation, but can copy it
  poly_ref(const poly_ref&) = default;
  poly_ref(poly_ref&&) = default;
  void operator=(poly_ref&&) = delete;
  void operator=(const poly_ref&) = delete;

  // from mutable lvalue
  template <not_const_type T> // not shadow copy ctor
  poly_ref(T& value) noexcept
  poly_ptr<Methods...> operator&() const noexcept;
}
```

### `const_poly_ref`
Same as poly_ref, but can be created from poly_ref and const T& (not extends lifetime)

### `poly_ptr`
Non owning, nullable, lightweight(~=void*)

Interface:
```C++
template <template<typename> typename... Methods>
struct poly_ptr {
  poly_ptr() = default;
  poly_ptr(std::nullptr_t) noexcept;
  poly_ptr& operator=(std::nullptr_t) noexcept;
  // from mutable pointer
  template <not_const_type T>
  poly_ptr(T* ptr) noexcept;
  // from mutable pointer to Any with same methods
  template <any_x Any>
  poly_ptr(Any* ptr) noexcept;
  
  // observers
 
  // returns raw pointer to value
  void* raw() const noexcept;
  bool has_value() const noexcept;
  bool operator==(std::nullptr_t) const noexcept;
  explicit operator bool() const noexcept;

  // access
  poly_ref<Methods...> operator*() const noexcept;
  const poly_ref<Methods...>* operator->() const noexcept;
}
```
### `const_poly_ptr`
Same as poly_ptr, but can be created from poly_ptr and const T*/Any*

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

## Methods
  Methods - user defined template types with one argument and static function `do_invoke`, first argument is a Self, others - method arguments
  
  Methods used to create polymorphic types - values, references and pointers(`design`) and invoking
  
  Methods 'from the box':

### `destroy`
  Polymorphic value have by default. Cannot be invoked explicitly
  
### `copy`
enables copy = )

### `move`
  enables move AND copy assignment operator (if you have copy). If inserted types have noexcept move constructor any will be more effective.
  
### `rtti`
  enables `.type()` method in basic_any, forces to store additional info about type

### `hash`
 enables specialization of std::hash, Hash for polymorphic value returns std::hash<T>{}(stored_value) or 0, if any is empty
 
### `equal_to`
  Incompatible with `spaceship`(spaceship already contains it). Enables operator== with other any.
  
### `spaceship`
  enables operator<=>

### `any_x`
concept of any type created by any_with, basic_any_with or successor of such type
```C++
template <typename T>
concept any_x = /*...*/;
```
### `method_traits`
Provides compile time information about Method such as is it const? What is Self type? What a signature of Method? Etc

### `plugins`
  With plugins you can add an interface for type, created by `aa::any_with`/`aa::poly_ref`, for example:
```C++
template <typename T>
struct Draw {
    static void do_invoke(T self, std::ostream& out, int val) {
      self.draw(out, val);
    }
  // interface plugin, Any with this method will have methods from plugin
    template <typename CRTP>
    struct plugin {
      void draw(std::ostream& out) const {
  // CRTP here is a resulting type, which will be created, its always interits from plugin
        aa::invoke<DrawExplicit>(*static_cast<const CRTP*>(this), out);
      }
    };
};
using any_drawable = aa::any_with<Draw>;
any_drawable val = /*...*/;
val.draw(std::cout); // we have method .draw from plugin!

```
### `explicit_interface`
  If your Method have inner alias `using explicit_interface = /*anything*/`;
  Then all types before inserting into created Any type will be checked - they must
  explicitly satisfy your Method by `using satisfies = aa::satisfies<Method1, Method2...>;`
  OR with specialization like that:
```C++
namespace aa {
  template <>
  constexpr inline bool satisfies_v<Circle, Draw> = true;
}  // namespace aa
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
# OR with testing # cmake . -DBUILD_TESTING=ON -B build
cmake --build build
```
