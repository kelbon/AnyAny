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

`poly_ref<Methods...>` implicitly converible to smaller count of Methods.

`poly_ref<A, B, C>` is converible to`poly_ref<A, B>`, `poly_ref<A>`, `poly_ref<B>`... etc etc.

This means you can add in interface of functions only _Methods_ they are really require.
Then if you add _Method_ to your `any_with` type there are NO abi/api break.

```C++
// you can invoke this function with any poly_ref<..., A, ...>
void foo(poly_ref<A>);
```

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

`poly_ptr<Methods...>` implicitly converible to smaller count of Methods.

`poly_ptr<A, B, C>` is converible to`poly_ptr<A, B>`, `poly_ptr<A>`, `poly_ptr<B>`... etc etc.

This means you can add in interface of functions only _Methods_ they are really require.
Then if you add _Method_ to your `any_with` type there are NO abi/api break.

```C++
// you can invoke this function with any poly_ptr<..., A, ...>
void foo(poly_ptr<A>);
```

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


# Actions

### `any_cast`

Functional object with operator():

<details> <summary>Interface</summary>

```C++

// specialization for anyany types

template<typename T>
struct any_cast_fn<T, anyany_poly_traits> {

  // noexcept versions returns nullptr if bad type
  // others throw std::bad_cast if cast is bad

  using X = std::remove_reference_t<T>;
 
  template <any_x U>
  std::add_pointer_t<T> operator()(U* ptr) const noexcept;

  const X* operator()(const any_x auto* ptr) const noexcept ;

  template <any_x U>
  decltype(auto) operator()(U&& any) const {
    (std::is_lvalue_reference_v<T>) => T&
    (std::is_rvalue_reference_v<T> && std::is_rvalue_reference_v<U&&>) => T&&
    (std::is_rvalue_reference_v<U&&>) => T (value moved out from any)
    else => T (copy value from any)
  }

  const X* operator()(const_poly_ptr<...> p) const noexcept;

  X* operator()(poly_ptr<...> p) const noexcept;
 
  std::conditional_t<std::is_rvalue_reference_v<T>, std::remove_cvref_t<T>,
                     std::conditional_t<std::is_reference_v<T>, T, std::remove_cv_t<T>>>
  operator()(poly_ref<...> p) const;
 
  std::conditional_t<std::is_reference_v<T>, const X&, std::remove_cv_t<T>>
  operator()(const_poly_ref<...> p) const;
 
}; 

// version for custom traits

template <typename T, poly_traits Traits>
struct any_cast_fn {
  // returns nullptr if type descriptors are not equal
  // const pointer for const U and just pointer otherwise
  // const U or not determinated by Traits.to_address
  template <typename U>
  auto* operator()(U&& val) const;
};

```

</details>

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

Accepts `any_with` or `poly_ref` and _Method_ arguments. Invokes _Method_.

If arg is const `any_with` or `const_poly_ref`, then only **const Methods** permitted.

Throws aa::empty_any_method_call in any is empty

<details>
<summary>Interface</summary>

```C++

template<TTA Method>
struct invoke_fn {

  // Here Args... are really Method arguments, so implicit conversions possible
 
  template <any_x U>
  result_t<Method> operator()(U&& any, Args... args) const;
  
  template <any_x U>
  result_t<Method> operator()(const U& any, Args... args) const {
    static_assert(const_method<Method>);
    //
  }

  // ill-formed if p has no Method
  result_t<Method> operator()(poly_ref<...> p, Args... args) const;
 
  result_t<Method> operator()(const_poly_ref<Methods...> p, Args... args) const {
    static_assert(const_method<Method>);
    //
  }

  // invokes Method without type erasure (for common interface for all values)
  template<typename T>
  requires (!any_x<T>)
  result_t<Method> operator()(T&& value, Args... args) const;
 
  // binds arguments and returns invocable<result_t<Method>(auto&&)> for passing to algorithms
  // each invoke of result can create copies of args, because assumes to be invoked more then one time
  constexpr auto with(Args... args) const;

};

```

</details>

Example:
```C++
void foo(std::vector<aa::poly_ref<Foo>> vec) {
  std::ranges::for_each(vec, aa::invoke<Foo>);
}

any_animal Pet = Cat{};
// operator & creates poly_ptr,
// then operator* creates poly_ref
aa::invoke<Say>(*&Pet, std::cout);

```

### `invoke_unsafe`
Same as `invoke`, but undefined behavior if any has no value instead of exception throwing

### `type_switch`

Selects .case based on input arg dynamic type and invokes `visitor` with this dynamic type or default function

Also supports `poly_traits` as second template argument, so it supports any type for which you have poly traits

<details>
<summary>Interface</summary>

```C++
template<typename Result = void, poly_traits Traits = anyany_poly_traits>
struct type_switch_fn {
  
  type_switch_fn(poly_ref<...>);

  // invokes Fn if T contained
  template <typename T, typename Fn>
  type_switch_impl& case_(Fn&& f);
 
  // If value is one of Ts... F invoked (invokes count <= 1)
  template <typename... Ts, typename Fn>
  type_switch_impl& cases(Fn&& f);
 
  // if no one case succeded invokes 'f' with input poly_ref argument
  template <typename Fn>
  Result default_(Fn&& f);
 
  // if no one case succeded returns 'v'
  Result default_(Result v);
  
  // if no on ecase succeded returns 'nullopt'
  std::optional<Result> no_default();

};

```

</details>

Example:
```C++
  Result val = aa::type_switch<Result>(value)
      .case_<float>(foo1)
      .case_<bool>(foo2)
      .cases<char, int, unsigned char, double>(foo3)
      .default(15);
```
### `visit_invoke`

Its... runtime overload resolution! `aa::make_visit_invoke<Foos...>` creates overload set object with method `.resolve(Args...)`,
which performs overload resolution based on Args... runtime types.

Resolve returns `nullopt` if no such function exist to accept input arguments

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
 ### `variant_swarm`
  Polymorphic container adaptor, which behaves as `Container<std::variant<Types...>>`, but much more effective.
  
  Supports operations:
  * `visit<Types...>(visitor)` - invokes `visitor` with all contained value of types `Types`
  * `view<T>` - returns reference to container of all stored values of type `T`
  
  Container is a `std::vector` by default.
  
  <details>
  <summary>Interface</summary>
 
```C++
template<template<typename> typename Container, typename... Ts>
struct basic_variant_swarm {

  // modifiers
 
  void swap(basic_variant_swarm& other) noexcept;
  friend void swap(basic_variant_swarm& a, basic_variant_swarm& b) noexcept;
  
  // selects right container and inserts [it, sent) into it
  template <std::input_iterator It>
    requires(tt::one_of<std::iter_value_t<It>, std::ranges::range_value_t<Container<Ts>>...>)
  auto insert(It it, It sent);

  // insert and erase overloads for each type in Ts...
  using inserters_type::erase;
  using inserters_type::insert;

  // observe

  bool empty() const noexcept;
  
  // returns count values, stored in container for T
  template <tt::one_of<Ts...> T>
    requires(std::ranges::sized_range<container_for<T>>)
  auto count() const;

  template <std::size_t I>
    requires(std::ranges::sized_range<decltype(std::get<I>(containers))>)
  auto count() const;
  
  // returns count of values stored in all containers
  constexpr auto size() const requires(std::ranges::sized_range<container_for<Ts>> && ...);

  // returns tuple of reference to containers #Is
  template <std::size_t... Is>
  auto view();
  
  template <std::size_t... Is>
  auto view() const;

  // returns tuple of reference to containers for Types
  template <tt::one_of<Ts...>... Types>
  auto view();
  
  template <tt::one_of<Ts...>... Types>
  auto view() const;

  // visit

  // visits with 'v' and passes its results into 'out_visitor' (if result is not void)
  template <tt::one_of<Ts...>... Types>
  void visit(visitor_for<Types...> auto&& v, auto&& out_visitor);
 
  // ignores visitor results
  template <tt::one_of<Ts...>... Types>
  void visit(visitor_for<Types...> auto&& v);
 
  // visits with 'v' and passes its results into 'out_visitor' (if result is not void)
  void visit_all(visitor_for<Ts...> auto&& v, auto&& out_visitor);
  
  // ignores visitor results
  constexpr void visit_all(visitor_for<Ts...> auto&& v);

  template <tt::one_of<Ts...>... Types, std::input_or_output_iterator Out>
  constexpr Out visit_copy(visitor_for<Types...> auto&& v, Out out);

  template <tt::one_of<Ts...>... Types, std::input_or_output_iterator Out>
  constexpr Out visit_copy(Out out);
  
  // visits with 'v' and passes its results into output iterator 'out', returns 'out" after all
  template <std::input_or_output_iterator Out>
  constexpr Out visit_copy_all(visitor_for<Ts...> auto&& v, Out out);
  
  // passes all values into 'out' iterator, returns 'out' after all
  template <std::input_or_output_iterator Out>
  constexpr Out visit_copy_all(Out out);

  // ...also const versions for visit...
};

```

</details>

Example:
```C++
  aa::variant_swarm<int, double, std::string> f;
  // no runtime dispatching here, its just overloads
  f.inesrt("hello world");
  f.insert(5);
  f.insert(3.14);
  auto visitor = [](auto&& x) {
    std::cout << x << '\t';
  };
  f.visit_all(visitor); // prints 5, 3.14, "hello world"
```
 ### `data_parallel_vector`
 
This container behaves as `std::vector<T>`, but stores fields separatelly.

Supported operation: `view<T>` / `view<I>` to get span to all fields of this index

`T` must be aggreagte or tuple-like type

Note: `data_parallel_vector` is a random access range
Note: ignores `std::vector<bool>` specialization, behaves as normal vector for bools

<details>
<summary>Interface</summary>

```C++

template <typename T, typename Alloc>
struct data_parallel_vector {
  using value_type = T;
  using allocator_type = Alloc;
  using difference_type = std::ptrdiff_t;
  using size_type = std::size_t;
  using reference = proxy; // similar to vector<bool>::reference type
  using const_reference = const_proxy;

  void swap(data_parallel_vector&) noexcept;
  friend void swap(data_parallel_vector&) noexcept;

  data_parallel_vector() = default;

  explicit data_parallel_vector(const allocator_type& alloc);
  data_parallel_vector(size_type count, const value_type& value,
                               const allocator_type& alloc = allocator_type());
  explicit data_parallel_vector(size_type count, const allocator_type& alloc = allocator_type());
  
  template <std::input_iterator It>
  data_parallel_vector(It first, It last, const allocator_type& alloc = allocator_type());
  data_parallel_vector(const data_parallel_vector& other, const allocator_type& alloc);
  data_parallel_vector(data_parallel_vector&& other, const allocator_type& alloc);
  data_parallel_vector(std::initializer_list<value_type> init,
                               const allocator_type& alloc = allocator_type());

  // copy-move all default

  data_parallel_vector& operator=(std::initializer_list<T> ilist);

  using iterator;
  using const_iterator;

  iterator begin();
  const_iterator begin() const;
  iterator end();
  const_iterator end() const;
  const_iterator cbegin() const;
  const_iterator cend() const;

  reference front();
  const_reference front() const;
  reference back();
  reference back() const;
  reference operator[](size_type pos);
  const_reference operator[](size_type pos) const;

  size_type capacity() const;
  size_type max_size() const;

  // returns tuple of spans to underlying containers
  template <typename... Types>
  auto view();
  template <typename... Types>
  auto view() const;
  template <std::size_t... Nbs>
  auto view();
  template <std::size_t... Nbs>
  auto view() const;

  bool empty() const;
  size_type size() const;

  bool operator==(const data_parallel_impl&) const = default;

  iterator emplace(const_iterator pos, element_t<Is>... fields);
  reference emplace_back(element_t<Is>... fields);

  void push_back(const value_type& v);
  void push_back(value_type&& v);

  iterator erase(const_iterator pos);
  iterator erase(const_iterator b, const_iterator e);

  iterator insert(const_iterator pos, const value_type& value);

  iterator insert(const_iterator pos, value_type&& value);

  iterator insert(const_iterator pos, size_type count, const T& value);

  template <std::input_iterator It>
  iterator insert(const_iterator pos, It first, It last);

  iterator insert(const_iterator pos, std::initializer_list<value_type> ilist);

  void assign(size_type count, const value_type& value);

  template <std::input_iterator It>
  void assign(It first, It last);

  void assign(std::initializer_list<T> ilist);

  void clear();
  void pop_back();
  void reserve(size_type new_cap);
  void resize(size_type sz);
  void resize(size_type sz, const value_type& v);
  void shrink_to_fit();
};

```

</details>

Example:

```C++
struct my_type {
  int x;
  float y;
  bool l;
};
void foo() {
  aa::data_parallel_vector<my_type> magic;
// ints, floats, bools are spans to all stored fields of my_type (&::x, &::y, &::l)
  auto [ints, floats, bools] = magic;
  magic.emplace_back(5, 6.f, true);
};
``` 
 
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
