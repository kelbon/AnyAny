
#pragma once

#include <array>
#include <utility>      // std::exchange
#include <tuple>        // tuple for vtable
#include <cassert>      // assert
#include <memory>       // construct_at / destroy_at
#include <stdexcept>    // runtime_error
#include <compare>      // partical_ordering
#include <cstddef>      // max_align_t on gcc
#include <climits>      // CHAR_BIT on gcc

// TODO remove it when clang will support format(PLEASE)
#ifndef _MSC_VER
#define FORMAT(...) "empty string"
#elif defined(__clang__)
#define FORMAT(...) "empty string"
#else
#include <format>       // message for bad_any_cast
#define FORMAT(...) std::format(__VA_ARGS__)
#endif

#ifndef _MSC_VER
#ifdef __clang__
#define AA_PLEASE_INLINE __attribute__((always_inline))
#define AA_AXIOM(cond) __builtin_assume((cond))
#else
#define AA_PLEASE_INLINE __attribute__((always_inline))
#define AA_AXIOM(cond)         \
  do {                         \
    if (!(cond))               \
      __builtin_unreachable(); \
  } while (0)
#endif
#else
#define AA_PLEASE_INLINE __forceinline
#define AA_AXIOM(cond) __assume((cond))
#endif

// TTA == template template argument (just for better reading)
#define TTA template <typename> typename

namespace aa {

template <typename...>
struct type_list {};

}  // namespace aa

namespace noexport {

template <typename, typename U>
struct steal_qualifiers_from {
  using type = U;
};
template <typename T, typename U>
struct steal_qualifiers_from<T&, U> {
  using type = std::add_lvalue_reference_t<typename steal_qualifiers_from<T, U>::type>;
};
template <typename T, typename U>
struct steal_qualifiers_from<T&&, U> {
  using type = std::add_rvalue_reference_t<typename steal_qualifiers_from<T, U>::type>;
};
template <typename T, typename U>
struct steal_qualifiers_from<const T, U> {
  using type = const typename steal_qualifiers_from<T, U>::type;
};
template <typename T, typename U>
struct steal_qualifiers_from<volatile T, U> {
  using type = volatile typename steal_qualifiers_from<T, U>::type;
};
template <typename T, typename U>
struct steal_qualifiers_from<const volatile T, U> {
  using type = const volatile typename steal_qualifiers_from<T, U>::type;
};
template <typename From, typename To>
using steal_qualifiers_from_t = typename steal_qualifiers_from<From, std::remove_cvref_t<To>>::type;

inline constexpr size_t npos = size_t(-1);

template <size_t I, typename... Args>
struct number_of_impl {
  static constexpr size_t value = npos;  // no such element in pack
};
template <size_t I, typename T, typename... Args>
struct number_of_impl<I, T, T, Args...> {
  static constexpr size_t value = I;
};
template <size_t I, typename T, typename First, typename... Args>
struct number_of_impl<I, T, First, Args...> {
  static constexpr size_t value = number_of_impl<I + 1, T, Args...>::value;
};

// npos if no such type in pack
template <typename T, typename... Args>
inline constexpr size_t number_of_first = number_of_impl<0, T, Args...>::value;

template <typename T, typename... Args>
struct is_one_of : std::bool_constant<(std::is_same_v<T, Args> || ...)> {};

template <typename T>
constexpr inline bool always_false = false;

template <typename Self>
consteval bool is_const_method() noexcept {
    // looks like compiler bug (in msvc AND clang too) "reinterpret_cast from 'void*'
    // to 'const char*&' casts away qualifirs" what is obviosly a lie
  if (std::is_pointer_v<Self>)
    return std::is_const_v<std::remove_pointer_t<Self>>;
  else if (std::is_reference_v<Self>)
    return std::is_const_v<std::remove_reference_t<Self>>;
  return true;  // passing by value is a const method!
}

template <typename>
struct any_method_traits;

// Note: for signature && added to arguments for forwarding 
template <typename R, typename Self, typename... Args>
struct any_method_traits<R (*)(Self, Args...)> {
  using self_sample_type = Self;
  using result_type = R;
  static constexpr bool is_const = is_const_method<Self>();
  using type_erased_self_type = std::conditional_t<is_const, const void*, void*>;
  using type_erased_signature_type = R (*)(type_erased_self_type, Args&&...);
  using args = aa::type_list<Args...>;
};
// noexcept version
template <typename R, typename Self, typename... Args>
struct any_method_traits<R (*)(Self, Args...) noexcept> {
  using self_sample_type = Self;
  using result_type = R;
  static constexpr bool is_const = is_const_method<Self>();
  using type_erased_self_type = std::conditional_t<is_const, const void*, void*>;
  using type_erased_signature_type = R (*)(type_erased_self_type, Args&&...);
  using args = aa::type_list<Args...>;
};

}  // namespace noexport
using namespace noexport;

namespace aa {

template <TTA Method>
using method_traits = any_method_traits<decltype(&Method<int>::do_invoke)>;

template <TTA Method>
using result_t = typename method_traits<Method>::result_type;

template <TTA Method>
using self_sample_t = typename method_traits<Method>::self_sample_type;

template <TTA Method>
using signature_t = typename method_traits<Method>::signature_type;

template <TTA Method>
using type_erased_signature_t = typename method_traits<Method>::type_erased_signature_type;

template <TTA Method>
using type_erased_self_t = typename method_traits<Method>::type_erased_self_type;

template <TTA Method>
using args_list = typename method_traits<Method>::args;

template <TTA Method>
concept const_method = method_traits<Method>::is_const;

template <typename T, TTA Method, typename = args_list<Method>>
struct invoker_for;

template <typename T, TTA Method, typename... Args>
struct invoker_for<T, Method, type_list<Args...>> {
  static AA_PLEASE_INLINE auto value(type_erased_self_t<Method> self, Args&&... args) -> result_t<Method> {
    using self_sample = self_sample_t<Method>;

    if constexpr (std::is_lvalue_reference_v<self_sample>) {
      return Method<T>::do_invoke(
          *reinterpret_cast<std::conditional_t<const_method<Method>, const T, T>*>(self),
          static_cast<Args&&>(args)...);
    } else if constexpr (std::is_pointer_v<self_sample>) {
      using real_self = std::conditional_t<const_method<Method>, const T*, T*>;
      return Method<T>::do_invoke(reinterpret_cast<real_self>(self), static_cast<Args&&>(args)...);

    } else if constexpr (std::is_copy_constructible_v<T>) {
      return Method<T>::do_invoke(*reinterpret_cast<const T*>(self), static_cast<Args&&>(args)...);
    } else {
      static_assert(always_false<T>,
                    "You pass self by value and it is not a copy constructible... or by rvalue reference");
    }
  }
};

// concept of any value inherited from basic_any<Args...>
template <typename T>
concept any_x = requires {
  typename std::remove_cvref_t<T>::base_any_type;
};

// BASIC METHODS

template <std::destructible T>
struct destroy {
  static AA_PLEASE_INLINE void do_invoke(const T* self) noexcept {
    std::destroy_at(self);
  }
};
template <typename T>
struct move {
  static AA_PLEASE_INLINE void do_invoke(T* src, void* dest) {
    std::construct_at(reinterpret_cast<T*>(dest), std::move(*src));
  }
};
template <typename T>
struct copy {
  static AA_PLEASE_INLINE void do_invoke(const T* src, void* dest) {
    std::construct_at(reinterpret_cast<T*>(dest), *src);
  }
};
template <typename T>
struct noexcept_copy {
  static_assert(std::is_nothrow_copy_constructible_v<T>);

  static AA_PLEASE_INLINE void do_invoke(const T* src, void* dest) noexcept {
    std::construct_at(reinterpret_cast<T*>(dest), *src);
  }
};
template <typename T>
struct RTTI {
  static AA_PLEASE_INLINE const std::type_info& do_invoke() noexcept {
    return typeid(T);
  }
};

// since C++20 operator== it is a != too
template <typename T>
struct equal_to {
  static AA_PLEASE_INLINE bool do_invoke(const T* first, const void* second) {
    return *first == *reinterpret_cast<const T*>(second);
  }
};

template <typename T>
struct spaceship {
  // See basic_any::operator<=> to understand why it is partical ordering always
  // strong and weak ordering is implicitly convertible to partical ordeting by C++20 standard!
  static AA_PLEASE_INLINE std::partial_ordering do_invoke(const T* first, const void* second) {
    return *first <=> *reinterpret_cast<const T*>(second);
  }
};

struct empty_any_method_call : std::exception {
  [[nodiscard]] const char* what() const noexcept override {
    return "Empty any method was called";
  }
};

// regardless Method is a template,
// do_invoke signature must be same for any valid T (as for virtual functions)
template <TTA... Methods>
struct vtable {
  std::tuple<type_erased_signature_t<Methods>...> table;

  struct methods_must_be_unique : Methods<int>... {};

  template <TTA Method>
  static inline constexpr size_t number_of_method = number_of_first<Method<int>, Methods<int>...>;

  template <TTA Method>
  static inline constexpr bool has_method = number_of_method<Method> != npos;
  // clang-format off
  template <TTA Method, typename... Args>
  requires(has_method<Method>)
  constexpr AA_PLEASE_INLINE decltype(auto) invoke(Args&&... args) const {
    // clang-format on
    return std::get<number_of_method<Method>>(table)(std::forward<Args>(args)...);
  }
};

template <typename T, TTA... Methods>
constexpr vtable<Methods...> vtable_for = {{&invoker_for<T, Methods>::value...}};

// CRTP - inheritor of basic_any
// SooS == Small Object Optimization Size
// strong exception guarantee for all constructors and assignments,
// emplace<T> - *this is empty if exception thrown
// for alloc not all fancy pointers supported and construct / destroy not throught alloc
template <typename CRTP, typename Alloc, size_t SooS, TTA... Methods>
struct basic_any {
 private:
  // mutable for vtable_invoke in const methods
  alignas(std::max_align_t) mutable std::array<std::byte, SooS> data;
  void* value_ptr = &data;
  const vtable<Methods...>* vtable_ptr = nullptr;
  [[no_unique_address]] Alloc alloc;

  bool memory_allocated() const noexcept {
    return value_ptr != &data;
  }
  // invariant of basic_any - it is always in one of those states
  enum struct any_state {
    empty,  // has_value() == false, memory_allocated == false
    small,  // has_value() == true, memory_allocated == false, MOVE IS NOEXCEPT
    big,    // has_value() == true, memory_allocated == true, size remembered
            // and allocated_size() >= SooS
  };

  AA_PLEASE_INLINE any_state state() const noexcept {
    if (!has_value()) {
      assert(!memory_allocated());
      return any_state::empty;
    }
    if (!memory_allocated())
      return any_state::small;
    else
      return any_state::big;
  }

  // guarantees that small is nothrow movable(for noexcept move ctor/assign)
  template <typename T>
  static inline constexpr bool any_is_small_for =
      alignof(T) <= alignof(std::max_align_t) && std::is_nothrow_move_constructible_v<T> && sizeof(T) <= SooS;

 protected:
  template <TTA Method, typename... Args>
  decltype(auto) vtable_invoke(Args&&... args) const {
    assert(vtable_ptr != nullptr);
    return vtable_ptr->template invoke<Method>(static_cast<void*>(value_ptr), std::forward<Args>(args)...);
  }

 private:
  // used for exception-safe allocating and remembering size of allocated
  // + destroy value if was has_value() + forget size and deallocate if was big
  struct [[nodiscard]] allocate_guard {
   private:
    basic_any& any;
    size_t count;
    void* old_value_ptr;
    const vtable<Methods...>* any_vtable_ptr;

   public:
    allocate_guard(basic_any& any, size_t count)
        : any(any),
          count(count),
          old_value_ptr(std::exchange(any.value_ptr, alloc_traits::allocate(any.alloc, count))),
          any_vtable_ptr(any.vtable_ptr) {
      assert(count >= SooS);
    }
    // after successfull constructing or smth like (when no exception can happen already)
    void release() noexcept {
      if (any_vtable_ptr != nullptr) {  // has value
        any_vtable_ptr->template invoke<destroy>(old_value_ptr);
        if (old_value_ptr != &any.data) {  // was allocated
          alloc_traits::deallocate(any.alloc, reinterpret_cast<alloc_pointer_type>(old_value_ptr),
                                   alloc_size_type{any.allocated_size()});
          any.forget_size();
        }
      }
      any.remember_size(count);
      old_value_ptr = nullptr;
    }
    ~allocate_guard() {
      if (old_value_ptr == nullptr) [[likely]]  // release was called
        return;
      assert(std::current_exception() != nullptr);
      alloc_traits::deallocate(any.alloc, reinterpret_cast<alloc_pointer_type>(any.value_ptr),
                               alloc_size_type{count});
      any.value_ptr = old_value_ptr;
    }
  };

  template <typename T, typename... Args>
  void emplace_in_empty(Args&&... args) noexcept(
      std::is_nothrow_constructible_v<T, Args&&...>&& any_is_small_for<T>) {
    if constexpr (any_is_small_for<T>) {
      std::construct_at(reinterpret_cast<T*>(value_ptr), std::forward<Args>(args)...);
    } else {
      // invariant of big - allocated_size >= SooS
      constexpr size_t allocate_size = std::max(sizeof(T), SooS);
      allocate_guard guard{*this, allocate_size};
      std::construct_at(reinterpret_cast<T*>(value_ptr), std::forward<Args>(args)...);
      guard.release();
    }
    vtable_ptr = &vtable_for<T, Methods...>;
  }

  using alloc_traits = std::allocator_traits<Alloc>;
  using alloc_pointer_type = typename alloc_traits::pointer;
  using alloc_size_type = typename alloc_traits::size_type;
  // for any_cast / unified function call
  friend struct caster;

  template <TTA, typename>
  friend struct invoke_fn;

 public:
  template <TTA Method>
  static inline constexpr bool has_method = vtable<Methods...>::template has_method<Method>;
  static inline constexpr bool has_copy = has_method<copy> || has_method<noexcept_copy>;

  static_assert(
      !(has_method<spaceship> && has_method<equal_to>),
      "Spaceship already contains most effective way to equality compare, if class have operator ==");
  static_assert(SooS >= sizeof(size_t));  // for storing sizeof of big element after allocation
  static_assert(is_one_of<typename alloc_traits::value_type, std::byte, char, unsigned char>::value);
  static_assert(has_method<destroy>, "Any requires destructor!");
  static_assert((has_method<copy> + has_method<noexcept_copy>) != 2,
                "Any requires exactly zero or one copy method");

  using base_any_type = basic_any;

  constexpr basic_any() = default;
  basic_any(Alloc alloc) noexcept : alloc(std::move(alloc)) {
  }

  ~basic_any() {
    destroy_value();
  }

  // basic_any copy/move stuff

  basic_any(const basic_any& other) requires(has_copy)
      : alloc(alloc_traits::select_on_container_copy_construction(other.alloc)) {
    switch (other.state()) {
      case any_state::empty: {
        return;
      }
      case any_state::small: {
        if constexpr (has_method<noexcept_copy>) {
          other.vtable_invoke<noexcept_copy>(static_cast<void*>(value_ptr));
        } else {
          allocate_guard guard(*this, SooS);
          other.vtable_invoke<copy>(static_cast<void*>(value_ptr));
          guard.release();
        }
        break;
      }
      case any_state::big: {
        // no way to replace value into &data regardless of sizeof, because it can break
        // invariant "noexcept move" of small state
        allocate_guard guard(*this, other.allocated_size());
        other.vtable_invoke<copy>(static_cast<void*>(value_ptr));
        guard.release();
        break;
      }
    }
    vtable_ptr = other.vtable_ptr;
  }

  [[nodiscard]] Alloc get_allocator() const noexcept {
    static_assert(std::is_nothrow_copy_constructible_v<Alloc>, "C++ Standard requires it");
    return alloc;
  }

  basic_any(basic_any&& other) noexcept requires(has_method<move>) : alloc(std::move(other.alloc)) {
    move_value_from(std::move(other));
  }
  // TODO C++23 - deducing this here
  CRTP& operator=(basic_any&& other) noexcept requires(has_method<move>) {
    // nocheck about this == &other
    // because after move assign other by C++ standard in unspecified(valid) state
    reset();
    if constexpr (!alloc_traits::is_always_equal::value &&
                  alloc_traits::propagate_on_container_move_assignment::value) {
      if (alloc != other.alloc)
        alloc = std::move(other.alloc);
    }
    move_value_from(std::move(other));
    return static_cast<CRTP&>(*this);
  }

  CRTP& operator=(const basic_any& other) requires(has_copy&& has_method<move>) {
    basic_any value{other};
    if constexpr (!alloc_traits::is_always_equal::value &&
                  alloc_traits::propagate_on_container_copy_assignment::value) {
      if (alloc != other.alloc) {
        reset();  // my alloc will be destroyed so i need to deallocate(while i can)
        alloc = other.alloc;
      }
    }
    *this = std::move(value);
    return static_cast<CRTP&>(*this);
  }

  // making from any other type

  // postconditions : has_value() == true, *this is empty if exception thrown
  template <typename T, typename... Args>
  AA_PLEASE_INLINE std::decay_t<T>& emplace(Args&&... args) noexcept(
      std::is_nothrow_constructible_v<std::decay_t<T>, Args&&...>&& any_is_small_for<std::decay_t<T>>) {
    reset();
    emplace_in_empty<std::decay_t<T>>(std::forward<Args>(args)...);
    return *reinterpret_cast<std::decay_t<T>*>(value_ptr);
  }
  template <typename T, typename U, typename... Args>
  AA_PLEASE_INLINE std::decay_t<T>& emplace(std::initializer_list<U> list, Args&&... args) noexcept(
      std::is_nothrow_constructible_v<std::decay_t<T>, std::initializer_list<U>, Args&&...>&&
          any_is_small_for<std::decay_t<T>>) {
    reset();
    emplace_in_empty<std::decay_t<T>>(list, std::forward<Args>(args)...);
    return *reinterpret_cast<std::decay_t<T>*>(value_ptr);
  }
  template <typename T, typename... Args>
  AA_PLEASE_INLINE basic_any(std::in_place_type_t<T>, Args&&... args) noexcept(
      std::is_nothrow_constructible_v<std::decay_t<T>, Args&&...>&& any_is_small_for<std::decay_t<T>>) {
    emplace_in_empty<std::decay_t<T>>(std::forward<Args>(args)...);
  }
  template <typename T, typename U, typename... Args>
  AA_PLEASE_INLINE basic_any(std::in_place_type_t<T>, std::initializer_list<U> list, Args&&... args) noexcept(
      std::is_nothrow_constructible_v<std::decay_t<T>, std::initializer_list<U>, Args&&...>&&
          any_is_small_for<std::decay_t<T>>) {
    emplace_in_empty<std::decay_t<T>>(list, std::forward<Args>(args)...);
  }
  // clang-format off
  template <typename T>
  requires(!any_x<T>)
  basic_any(T&& value) noexcept(
      std::is_nothrow_constructible_v<std::decay_t<T>, T&&>&& any_is_small_for<std::decay_t<T>>)
      : basic_any(std::in_place_type<std::decay_t<T>>, std::forward<T>(value)) {
  }
  template <typename T>
  basic_any(std::allocator_arg_t, Alloc alloc, T&& value) noexcept(
      std::is_nothrow_constructible_v<std::decay_t<T>, T&&>&& any_is_small_for<std::decay_t<T>>)
      : alloc(std::move(alloc)) {
    emplace_in_empty<std::decay_t<T>>(std::forward<T>(value));
  }

  template <typename T> requires (!any_x<T>)
  CRTP& operator=(T&& value) noexcept(
      std::is_nothrow_constructible_v<std::decay_t<T>, T&&>&& any_is_small_for<std::decay_t<T>>) {
    *this = basic_any{std::forward<T>(value)};
    return static_cast<CRTP&>(*this);
  }
  // clang-format on

  // postconditions : has_value() == false
  void reset() noexcept {
    destroy_value();
    vtable_ptr = nullptr;
  }

  // observe

  [[nodiscard]] AA_PLEASE_INLINE bool has_value() const noexcept {
    return vtable_ptr != nullptr;
  }
  [[nodiscard]] const std::type_info& type() const noexcept requires(has_method<RTTI>) {
    if (!has_value())
      return typeid(void);
    return vtable_ptr->template invoke<RTTI>();
  }

  // COMPARE

  // there are NO operator==(auto&&) or operator<=>(auto&&)
  // It can be more effective on runtime(if types not equal, then return false / unordered)
  // but:
  // * it will be less effective(useless branching) if you 90% sure its this type
  // * it will cause compilation time, obj file increasing
  // * may cause problems with CRTP type(implicit incorrect overload resolution)

  [[nodiscard]] bool operator==(const basic_any& other) const
      requires(has_method<equal_to> || has_method<spaceship>) {
    if (vtable_ptr != other.vtable_ptr)
      return false;
    if (vtable_ptr == nullptr)  // other.vtable_ptr == nullptr too here
      return true;
    if constexpr (has_method<spaceship>)
      return vtable_invoke<spaceship>(static_cast<const void*>(other.value_ptr)) ==
             std::partial_ordering::equivalent;
    else
      return vtable_invoke<equal_to>(static_cast<const void*>(other.value_ptr));
  }

  std::partial_ordering operator<=>(const basic_any& other) const requires(has_method<spaceship>) {
    if (vtable_ptr != other.vtable_ptr)
      return std::partial_ordering::unordered;
    if (vtable_ptr == nullptr)  // other.vtable_ptr == nullptr too here
      return std::partial_ordering::equivalent;
    return vtable_invoke<spaceship>(static_cast<const void*>(other.value_ptr));
  }

 private :

     // precodition - has_value() == false
     // clang-format off
  void move_value_from(basic_any&& other) noexcept {
    // clang-format on
    switch (other.state()) {
      case any_state::empty: {
        return;
      }
      case any_state::small: {
        // move is noexcept (invariant of small)
        other.vtable_invoke<move>(static_cast<void*>(value_ptr));
        vtable_ptr = other.vtable_ptr;
        return;
      }
      case any_state::big: {
        assert(other.allocated_size() >= SooS);  // invariant of big
        value_ptr = std::exchange(other.value_ptr, &other.data);
        remember_size(other.allocated_size());
        other.forget_size();
        vtable_ptr = std::exchange(other.vtable_ptr, nullptr);
        return;
      }
    }
    assert(false);  // TODO C++23 std::unreachable
  }

  AA_PLEASE_INLINE void remember_size(size_t size) noexcept {
    std::construct_at(reinterpret_cast<size_t*>(&data), size);
  }
  // end of lifetime for remembered size of allocated value
  AA_PLEASE_INLINE void forget_size() noexcept {
    std::destroy_at(reinterpret_cast<size_t*>(&data));
  }
  AA_PLEASE_INLINE size_t allocated_size() const noexcept {
    // assume lifetime of size_t was started
    // two rows for copy elision here
    size_t result = *reinterpret_cast<size_t*>(&data);
    return result;
  }
  void destroy_value() noexcept {
    if (has_value()) {
      vtable_invoke<destroy>();
      if (memory_allocated()) {
        alloc_traits::deallocate(alloc, reinterpret_cast<alloc_pointer_type>(value_ptr),
                                 alloc_size_type{allocated_size()});
        forget_size();
        value_ptr = &data;
      }
    }
  }
};

// TEMPLATE FUNCTION any_cast<T>(any | any*) -> T | T* (type decayed)

struct bad_any_cast : std::exception {
 private:
  std::string message;

 public:
  // string was created before exception and it is not bad_alloc, move is noexcept, all good!
  bad_any_cast(std::string s) noexcept : message(std::move(s)) {
  }

  [[nodiscard]] const char* what() const noexcept override {
    return message.c_str();
  }
};

// friend of basic_any
struct caster {
  // clang-format off
  template <typename T, typename CRTP, typename Alloc, size_t SooS, TTA... Methods >
  static const T* any_cast_impl(const basic_any<CRTP, Alloc, SooS, Methods...>* any) noexcept {
    // clang-format on
    // T already remove_cv
    if (any == nullptr || !any->has_value() || any->vtable_ptr != &vtable_for<T, Methods...>)
      return nullptr;
    return reinterpret_cast<const T*>(any->value_ptr);
  }
  // clang-format off
  template <typename T, typename CRTP, typename Alloc, size_t SooS, TTA... Methods>
  static T* any_cast_impl(basic_any<CRTP, Alloc, SooS, Methods...>* any) noexcept {
    // clang-format on
    if (any == nullptr || !any->has_value() || any->vtable_ptr != &vtable_for<T, Methods...>)
      return nullptr;
    return reinterpret_cast<T*>(any->value_ptr);
  }
};

template <typename T, any_x U>
[[nodiscard]] AA_PLEASE_INLINE T* any_cast(U* ptr) noexcept {
  // references ill-formed already (because of T*)
  static_assert(!(std::is_array_v<T> || std::is_function_v<T> || std::is_void_v<T>),
                "Incorrect call, it will be always nullptr");
  return caster::any_cast_impl<std::remove_cv_t<T>>(static_cast<typename U::base_any_type*>(ptr));
}
template <typename T, any_x U>
[[nodiscard]] AA_PLEASE_INLINE const T* any_cast(const U* ptr) noexcept {
  static_assert(!(std::is_array_v<T> || std::is_function_v<T> || std::is_void_v<T>),
                "Incorrect call, it will be always nullptr");
  return caster::any_cast_impl<std::remove_cv_t<T>>(static_cast<typename U::base_any_type*>(ptr));
}

template <typename T, any_x U>
[[nodiscard]] std::remove_cv_t<T> any_cast(U&& any) {
  static_assert(!(std::is_array_v<T> || std::is_function_v<T>), "Incorrect call, it will be always nullptr");

  T* ptr = caster::any_cast_impl<std::remove_cv_t<T>>(std::addressof(any));
  if (!ptr) {  // MSVC parsing bug template after any. breaks compilation
    constexpr bool msvc_bug_workaround = std::remove_reference_t<U>::template has_method<RTTI>;
    if constexpr (msvc_bug_workaround)  // must be "any.has_method<RTTI>"!
    {
      std::string message =
          FORMAT("Bad any_cast : requested type : {}, real type : {}", typeid(T).name(), any.type().name());
      throw bad_any_cast{std::move(message)};
    } else {
      std::string message =
          FORMAT("Bad any_cast : requested type : {}, real type unknown(RTTI disabled)", typeid(T).name());
      throw bad_any_cast{std::move(message)};
    }
  }  // move if U&& rvalue
  std::remove_cv_t<T> result{static_cast<steal_qualifiers_from_t<U&&, T>>(*ptr)};
  return result;
}

// TEMPLATE VARIABLE (FUNCTION OBJECT) invoke <Method> (any)

// hack for compilation time / obj size reduce + accept exactly user args
// (for example i can write invoke<Foo>(Any, {}, {1, 2, 3}) because compiler knows what types in must be

template <TTA Method, typename = args_list<Method>>
struct invoke_fn;

template <TTA Method, typename... Args>
struct invoke_fn<Method, type_list<Args...>> {
  template <any_x U>
  AA_PLEASE_INLINE result_t<Method> operator()(U&& any, Args... args) const {
    if (!any.has_value())
      throw empty_any_method_call{};
    if constexpr (std::is_void_v<result_t<Method>>) {
      any.template vtable_invoke<Method>(static_cast<Args&&>(args)...);
      AA_AXIOM(any.has_value());
    } else {
      result_t<Method> result = any.template vtable_invoke<Method>(static_cast<Args&&>(args)...);
      // at this point compiler do not know, that 'any' state is not changed after vtable_invoke
      AA_AXIOM(any.has_value());
      return result;
    }
  }
  // clang-format off
  template <any_x U>
  AA_PLEASE_INLINE result_t<Method> operator()(const U& any, Args... args) const {
    // clang-format on
    static_assert(const_method<Method>);
    if (!any.has_value())
      throw empty_any_method_call{};
    if constexpr (std::is_void_v<result_t<Method>>) {
      any.template vtable_invoke<Method>(static_cast<Args&&>(args)...);
      AA_AXIOM(any.has_value());
    } else {
      // at this point compiler do not know, that 'any' state is not changed after vtable_invoke
      result_t<Method> result = any.template vtable_invoke<Method>(static_cast<Args&&>(args)...);
      AA_AXIOM(any.has_value());
      return result;
    }
  }
};

template <typename T>
struct invoke_fn<destroy, T> {
  static_assert(always_false<T>, "Invoking destructor of contained value in any by hands is a bad idea");
};

template <TTA Method>
constexpr invoke_fn<Method> invoke = {};

// TEMPLATE any - default any for inheritance from (creating your any with default allocator and SooS)

template <typename CRTP, TTA... Methods>
struct any : basic_any<CRTP, std::allocator<std::byte>, (sizeof(void*) - 2) * CHAR_BIT, destroy, Methods...> {
  using any::basic_any::basic_any;
};

// used to cover CRTP, if you do not want to add member-methods(and want to use unified function call with
// invoke)
template <template <typename, TTA...> typename Base, TTA... Methods>
struct strong_alias : Base<strong_alias<Base, Methods...>, Methods...> {
 private:
  using base_t = Base<strong_alias, Methods...>;

 public:
  using base_t::base_t;
};

template <TTA... Methods>
using any_with = strong_alias<any, Methods...>;

}  // namespace aa
