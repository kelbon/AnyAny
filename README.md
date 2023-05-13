## AnyAny
Library for efficient dynamic polymorphism through type erasure(C++17 or later)

Goals are efficiency, understandability and extensibility.

clang, gcc, msvc
[![](
https://github.com/kelbon/AnyAny/actions/workflows/build_and_test.yml/badge.svg?branch=main)](
https://github.com/kelbon/AnyAny/actions/workflows/build_and_test.yml)

### See /examples folder for fast start!

* [`How to build?`](#build)

## Basic usage example

Foundation of library'a type erase part is a _Methods_ - a description which part of the type we want to use after erasing.

Let's create one for erasing types with `void draw()`:

<details>
  <summary>click here to see short syntax with macros</summary>
  
  There are `anyany_method` macro in <anyany/anyany_macro.hpp>
  For example, for Method 'foo', which accepts int and float + returns float
  
  ```C++
  #include <anyany/anyany_macro.hpp>
  anyany_method(foo, (&self, int i, float f) requires(self.foo(i, f)) -> float);
  
  ...
  
  void example(aa::any_with<foo> obj) {
    if(obj.has_value())
      float x = obj.foo(5, 3.14f); // all works
    obj = some_type_with_foo{};
    obj = some_other_type_with_foo();
  }
  ```
  
</details>


```C++
// For each type T do value.draw()
struct Draw {
  template<typename T>
  static void do_invoke(const T& self) {
    self.draw();
  }
};
```

We can use `Draw` for type erasion:

```C++
#include <anyany/anyany.hpp>
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
  // see /examples folder for more
}
```

There are no virtual functions, inheritance, pointers, memory management etc! Nice!

You can add any number of _Methods_:
```C++
using any_my = aa::any_with<Draw, Run, aa::copy>;
```

Wait, copy...? Yes, by default `aa::any_with` only have a destructor, you can add `aa::copy` method to do it copyable and movable or `aa::move` to make it
move only

Predefined _Methods_:

<details>
  <summary>copy</summary>
  
  makes `any_with` copyable and movable, enables `aa::materialize` for references(this also requires `aa::destroy` method)
  
  There are also `copy_with<Alloc, SooS>`, this enables copy when you using custom Allocator and Small Object Optimization Size(`aa::basic_any_with`)
</details>

<details>
  <summary>move</summary>
  
  makes 'any_with' movable
  
  Note: move constructor and move assignment operator for `any_with` always noexcept
</details>

<details>
  <summary>hash</summary>
  
  enables `std::hash` specialization for `any_with`,`poly_ref`/...etc. If `any_with` is empty, then hash == 0.
  
  Note: `poly_ptr` has specialization of `std::hash` by default, but it is pointer-like hash.
  
</details>

<details>
  <summary>type_info</summary>
  
  enables `aa::any_cast`, `aa::type_switch`, `aa::visit_invoke` by adding RTTI in vtable.
  
  Also adds `.type_descriptor() -> aa::descriptor_t` for `any_with`/`poly_ref`/...etc.

  
</details>

<details>
  <summary>equal_to</summary>
  
  enables `operator==` for `any_with`/`poly_ref`/...etc.
  
  Two objects are equal if they contain same type(or both empty) and stored values are equal.
</details>

<details>
  <summary>spaceship</summary>
  
  enables `operator<=>` and `operator==` for `any_with`/`poly_ref`/...etc.
  
  Note: operpator<=> always returns `std::partial_ordering`.
  
  If two objects do not contain same type returns `unordered`, otherwise
  returns result of `operator <=>` for contained objects.
  
  Note: returns `std::partial_ordering::equivalent` if both empty
  
</details>

<details>
  <summary>call&ltR(Args...)&gt</summary>
  
  adds `R operator()(Args...)` for `any_with`/`poly_ref`/...etc.
  
  Note: also supported `const`, `noexcept` and `const noexcept` signatures
  
  example:
  ```C++
 
  // stateful::cref is a lightweight thing,
  // it stores vtable in itself(in this case only one function ptr)
  // and const void* to value
  // This is most effective way to erase function
  template <typename Signature>
  using function_ref = aa::stateful::cref<aa::call<Signature>>;

  void foo(function_ref<int(float) const> ref) {
     int result = ref(3.14);
  }

  ```
</details>

<details>
  <summary>destroy</summary>
  
  adds destructor(`any_with` has it by default), but maybe you want to use it with `aa::poly_ptr` to manually manage lifetime.
  Also enables `aa::materialize` for references(also requires aa::copy)
</details>

See `method` concept in anyany.hpp if you want all details about _Methods_

#

Polymorphic types:

* [`any_with<Methods...>`](#any_with)
* [`basic_any_with<Methods...>`](#basic_any_with)
* [`poly_ref<Methods...>`](#poly_ref)
* [`poly_ptr<Methods...>`](#poly_ptr)
* [`cref<Methods...>`](#const_poly_ref)
* [`cptr<Methods...>`](#const_poly_ptr)
* [`stateful::ref<Methods...>`](#stateful_ref)
* [`stateful::cref<Methods...>`](#stateful_cref)

Actions:

* [`any_cast<T>`](#any_cast)
* [`invoke<Method>`](#invoke)
* [`type_switch`](#type_switch)
* [`visit_invoke`](#visit_invoke)

Polymorphic containers:

* [`variant_swarm<Ts...>`](#variant_swarm)
* [`data_parallel_vector<T, Alloc>`](#data_parallel_vector)

<details>
  <summary>Compile time information</summary>

```C++
template <typename T>
concept method = /*...*/; // see anyany.hpp

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
  * std_variant_poly_traits - for std::variant (<anyany/utility.hpp> header)

</details>


<details>
  <summary>Interface features</summary>

For example, you want to have method .foo() in type created by `any_with` or `poly_ref` with your _Method_.

Then you should use `plugins`:
```C++
struct Foo {
    template <typename T>
    static void do_invoke(T&) { /* do something*/ }
   
    // any_with<Foo> will inherit plugin<any_with<Foo>>
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
Note: You can 'shadow/override' other plugins by inherting from them in your plugin(even if inheritance is private)

See aa::spaceship/aa::copy_with plugins for example

Also you can specialize aa::plugin<Any, Method> for your Method or even for 'Any' with some requirements

</details>

### `any_with`
Accepts any number of _Methods_ and creates a type which can hold any value, which supports those _Methods_. Similar to runtime concept

Note: There is tag 'aa::force_stable_pointers' to force allocoation, so `poly_ptr/cptr` to `any_with<...>` will not be invalidated after move.

Note: `aa::unreachable_allocator`, which will break compilation, if `basic_any_with<unreachable_allocator, ...>` tries to allocate memory.
So you can force no-allocating in types

<details>
<summary>
Interface of created type
</summary>

```C++

// All constructors and move assign operators are exception safe
// move and move assign always noexcept

// See 'construct_interface' alias in anyany.hpp and 'struct plugin'for details how it works(comments)
struct Any : construct_interface<basic_any<Alloc, SooS, Methods...>, Methods...>{
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
 
  // returns count of bytes sufficient to store current value
  // (not guaranteed to be smallest)
  // return 0 if !has_value()
  size_t sizeof_now() const noexcept;
  
  // returns descriptor_v<void> if value is empty
  type_descriptor_t type_descriptor() const noexcept requires aa::type_info;
 
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
  
  descriptor_t type_descriptor() const noexcept requires aa::type_info;
 
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

`aa::cref` is a template alias to `aa::const_poly_ref`

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
  // NOTE: returns unspecified value if *this == nullptr
  const vtable<Methods...>* raw_vtable_ptr() const noexcept;
  
  // returns descriptor_v<void> is nullptr
  descriptor_t type_descriptor() const noexcept requires aa::type_info;
  
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

`aa::cptr` is a template alias to `aa::const_poly_ptr`

### `stateful_ref`
`aa::stateful::ref<Methods...>` contains vtable in itself.

Also can contain references to C-arrays and functions without decay

It has pretty simple interface, only creating from `T&/poly_ref` and invoking(by aa::invoke for example)

It will have maximum performance if you need to erase 1-2 _Methods_ and dont need to use `any_cast`.

Typical use-case - creating a function_ref

```C++
template <typename Signature>
using function_ref = aa::stateful::cref<aa::call<Signature>>;

bool foo(int) { return true; }

void example(function_ref<bool(int) const> ref) {
  ref(5);
}

int main() {
  example(&foo);
  example([](int x) { return false; });
}

```

### `stateful_cref`
Same as `stateful::ref`, but may be created from `const T&` and `aa::cref`

# Actions

### `any_cast`

requires aa::type_info method

Functional object with operator():

Works as std::any_cast - you can convert to T(copy), T&(take ref) (throws aa::bad_cast if cast is bad)

Or you can pass pointer(or poly_ptr) (returns nullptr, if cast is bad)

```C++

T* ptr = any_cast<T>(&any);

```

Example:
```C++
using any_comparable = aa::any_with<aa::copy, aa::spaceship, aa::move>;

void Foo() {
  any_comparable value = 5;
  value.emplace<std::vector<int>>({ 1, 2, 3, 4}); // constructed in-place
  // any_cast returns pointer to vector<int>(or nullptr if any do not contain vector<int>)
  aa::any_cast<std::vector<int>>(std::addressof(value))->back() = 0;
  // version for reference
  aa::any_cast<std::vector<int>&>(value).back() = 0;
  // version which returns by copy (or move, if 'value' is rvalue) 
  auto vec = aa::any_cast<std::vector<int>>(value);
}
```
### `invoke`
Functional object with operator(), which accepts `any_with/ref/cref/stateful::ref/stateful::cref` as first argument and then all _Method_'s arguments and invokes _Method_

If arg is const `any_with` or `cref`, then only **const Methods** permitted.

precondition: any.has_value() == true

Example:
```C++

void example(any_with<Say> pet) {
  if(!pet.has_value())
    return;
  // invokes Method `Say`, passes std::cout as first argument
  aa::invoke<Say>(pet, std::cout);
}
void foo(std::vector<aa::poly_ref<Foo>> vec) {
  // invokes Method `Foo` without arguments for each value in `vec`
  std::ranges::for_each(vec, aa::invoke<Foo>);
}


```

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
  
  // if no one case succeded returns 'nullopt'
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
      .default_(15);
```

### `visit_invoke`

Its... runtime overload resolution! `aa::make_visit_invoke<Foos...>` creates overload set object with method `.resolve(Args...)`,
which performs overload resolution based on Args... runtime types.

Resolve returns `nullopt` if no such function exist to accept input arguments

This example is very basic, see also /examples/visit_invoke_example.hpp for more

Example:
```C++

auto ship_asteroid = [](spaceship s, asteroid a) -> std::string { ... }
auto ship_star = [](spaceship s, star) -> std::string { ... }
auto star_star = [](star a, star b) -> std::string { ... }
auto ship_ship = [](spaceship a, spaceship b) -> std::string { ... }

// Create multidispacter
constexpr inline auto collision = aa::make_visit_invoke<std::string>(
  ship_asteroid,
  ship_star,
  star_star,
  ship_ship);

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
Fetch content:
```CMake

include(FetchContent)
FetchContent_Declare(
  AnyAny
  GIT_REPOSITORY https://github.com/kelbon/AnyAny
  GIT_TAG        origin/main
)
FetchContent_MakeAvailable(AnyAny)
target_link_libraries(MyTargetName anyanylib)
```
<details>
<summary>or use add_subdirectory</summary>
  1. Clone this repository into folder with your project
2. Add these lines to it's CMakeLists.txt
  
```CMake
add_subdirectory(AnyAny)
target_link_libraries(MyTargetName PUBLIC anyanylib)
```
</details>

### `build`
  
```shell
git clone https://github.com/kelbon/AnyAny
cd AnyAny
cmake . -B build
cmake --build build
```
### build examples
```shell
git clone https://github.com/kelbon/AnyAny
cd AnyAny/examples
cmake . -B build
```
