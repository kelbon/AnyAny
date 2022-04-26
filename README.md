# AnyAny
C++20 library for comfortable and efficient dynamic polymorphism

[![Clang](
https://github.com/kelbon/AnyAny/actions/workflows/clang.yml/badge.svg?branch=main)](
https://github.com/kelbon/AnyAny/actions/workflows/clang.yml)
[![GCC](
https://github.com/kelbon/AnyAny/actions/workflows/gcc.yml/badge.svg?branch=main)](
https://github.com/kelbon/AnyAny/actions/workflows/gcc.yml)
(MSVC works too) _(Note : in Visual Studio 19 v142 toolset have parser bug in C++20 code, if you want to use msvc use v143 toolset (Visual Studio 2022) )_

This is a library for dynamic polymorphism through type erasure with better code readability and reusage, performance, far less boilerplate then with usual way (virtual functions).

* [`How to build?`](#build)

### Basic design knowledge (please read this example first)

The whole library is built on **Methods**, it is a short description of _captured_ method - class template with one argument
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
// Every time do_invoke invoked by library self is a reference to type T with .say() method. 
// Self can be const T& / T& / const T* / T* or just T - in this case self will be provided by copy -
// (if it is copy constructible of course) (its like C++23 deducing this if you know)
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
  // internal - if you write a single row method in any_animal with exactly this->vtable_invoke<Say>(out);
  // Pet.say(std::cout); // just an any other type
}
```
There are no virtual functions, inheritance, pointers, memory management etc! Nice!
It is modular and flexible:
You can add any number of methods like
`using any_my = any_with<Say, Draw, Float, aa::copy, aa::move>;`

For _flexibility_ see basic_any!

Wait, copy... Move? Yes, by default any is only have a destructor, so you can create move only any or ... copy only etc

Note: result type of do_invoke and Args must be same for all types T (as for virtual functions)

* [`basic_any`](#basic_any)
* [`any`](#any)
* [`any_with<Methods...>`](#any_with)
* [`any_cast<T>`](#any_cast)
* [`invoke<Method>`](#invoke)
* [`invoke_unsafe<Method>`](#invoke_unsafe)
* [`method_traits<Method>`](#method_traits)

### Methods

* [`destroy`](#destroy)
* [`copy`](#copy)
* [`move`](#move)
* [`RTTI`](#rtti)
* [`equal_to`](#equal_to)
* [`spaceship`](#spaceship)

### `any_with`
It is a template which accepts a any number of Methods and creates a type which can hold any value, which supports those Methods. Its like a concept but in runtime.
Example : see first example on top!

### `basic_any`
interface:
```C++
// CRTP - inheritor of basic_any, popular pattern
// Alloc - yes, any can allocate memory. It must be allocator for char/std::byte/unsigned char (other is meaningless)
// SooS - Small Object Optimization Size(like SSO in strings).
// You can increase it to less allocate or decrease it, for example,
// if you know, that sizeof of all types which will be stored not more then X bytes
// Methods... - see first example

template <typename CRTP, typename Alloc, size_t SooS, template<typename> typename... Methods>
struct basic_any {

// constructors, move, copy assign, dctor
// move and move assign is noexcept always

bool has_value() const noexcept; // true if not empty

// emplaces value in any, if exception thrown - any is empty(use operator= if you need strong exception guarantee here)

template<typename T, typename... Args>
std::decay_t& emplace<T>(Args&&...); // returns reference to emplaced value

// there are also version of emplace with initializer list
// and emplace-ctor with std::in_place_type tag. You know if you know...

void reset() noexcept; // after this method has_value() == false 

// DISABLED BY DEFAULT to enable this method add Method aa::RTTI to your type creation(like in first example on top!)
const std::type_info& type() const noexcept;

// DISABLED BY DEFAULT to enable this add Method aa::equal_to OR aa::spaceship
bool operator==( ... ) const;

// DISABLED BY DEFAULT to enable this operator add Method aa::spaceship
std::partial_ordering operator<=>(...) const; 

};
```

**All constructor any copy/move assignment operators have strong exception guarantee**

_Note : operator spaceship for any always returns partial ordering (if enabled), this means that two anyes can be unordered_

**Important** it is important if your type has noexcept move constructor, it really can increase perfomance(like in std::vector case, if you know).

### `any`
It is just an basic_any with default alloc (std::allocator<std::byte>) and default SooS such that sizeof (any) == Machine Word Size (64 / 32 bytes) for perfomance.

Usually used to inherit from, when you want to invoke methods like in common type with .MethodName(Args...)

Example:

```C++
struct any_executor : any<any_executor, Execute, aa::copy> {
  // use all constructors and move/copy assign operators of base class
  using any_executor::any::any;

  void execute(std::function<void()> foo) {
    if(has_value()) // Note! any can be empty!
      vtable_invoke<Execute>(std::move(foo)); // vtable_invoke - basic_any's protected method
  }
};

int main () {
  any_executor a = SomeExecutor{};
  a.execute([] {}); 
}

// line 148 just a short-cut for
// using base_t = any<any_executor, Execute, aa::copy>
// using base_t::base_t;
```

### `any_cast`
There are two versions, for pointer (noexcept) and throwing:

```C++
template<typename T, any_x Any> // concept aa::any_x here
T* any_cast(Any*) noexcept;
/* also version for const T* */
```
Returns `nullptr` if Any is empty or dynamic type in Any is not T

```C++
template<typename T, any_x Any>
std::decay_t<T> any_cast(Any&&); // thrown aa::bad_any_cast if T is not contained in any

/* also versions for & && const & etc versions of Any */
```

Example:
```C++
using any_comparable = aa::any_with<aa::copy, aa::spaceship, aa::move>;

void Foo() {
  any_comparable value = 5;
  value.emplace<std::vector<int>>({ 1, 2, 3, 4}); // constructed in-place
  aa::any_cast<std::vector<int>>(&value)->back() = 0; // any_cast returns pointer to vector<int>(or nullptr if any do not containts vector<int>)
}
```
### `invoke`
tool for unified call syntax. Invokes a Method on value of some type Any
If Any is const, then only **const Methods** permitted (methods which Self is const T* / const T& / const void* / T (accepted by copy))
(call `destroy` method if forbidden)
Throws aa::empty_any_method_called if !any.has_value();
Example: 
```C++
// See first example on top
  any_animal Pet = Cat{};
  aa::invoke<Say>(Pet, std::cout);

```

### `invoke_unsafe`
Same as `invoke`, but more effective and if any has no value -> undefined behavior

### `method_traits`
Provides compile time information about Method such as is it const? What is Self type? What a signature of Method? Etc
basic_any also have compile time information, static member variables bool has_method<Method> and bool has_copy
  
## Methods
  Methods are modules of any, user defined methods - is a runtime _concept_ on type which can be emplaced in any and enables support for invoke<Method>.
  But there are library provided methods, which are enables basic_any things, such as type() or spaceship operator

### `destroy`
  `any` have it by default. Cannot be invoked explicitly
  
### `copy`
enables copy = )

### `move`
  enables move AND copy assignment operator (if you have copy). If inserted types have noexcept move constructor any will be more effective.
  
### `rtti`
  enables `.type()` method in basic_any, forces to store additional info about type

### `equal_to`
  Incompatible with `spaceship`(spaceship already contains it). Enables operator== with other any.
  
### `spaceship`
  enables operator<=>

### `any_x`
```C++
// concept of any value inherited from basic_any<...>
template <typename T>
concept any_x = requires {
  typename std::remove_cvref_t<T>::base_any_type;
};
```
  base_any_type - is a inner type alias in basic_any, usefull for inheriting (using base_t::base_t), convering etc
  
### Background and design

For example you want to create a Machine class which engine can be changed on runtime. Engine must have Go method.

Without this library it is a very annoying task
```C++
class IEngine {
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
And of course i dont need a **pointer** to polymoprhic object here, i need an engine, a **polymorphic value**.

it is obvious that the approach with virtual functions does not express our intentions in the code, which means that it becomes much more difficult for us and the compiler to understand what is happening in it.

As you can see, there are many problems. Whats a solution?

In ideal world it must be something like: 
```C++

struct Machine {
  any_engine m_engime;
};

```
And it is possibile! But usually it is SO hard to write such class, that nobody do it!
This is why this library was created, it providies an instruments to create such types as fast and flexible as it possible
  
  ```C++
  template<typename T>
  struct Go {
    static int do_invoke(T self, int value) {
     self.go(value);
     return value * 4;
    }
  }
  
  using any_engine = aa::any_with<Go, aa::copy, aa::move>;
  // All ready to use!
  ```

## Using with CMake
1. Copy this repository into folder with your project
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
