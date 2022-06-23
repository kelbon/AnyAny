
#pragma once

#include <array>
#include <new>          // hardware_constructive_interference_size on gcc
#include <utility>      // std::exchange
#include <tuple>        // tuple for vtable
#include <cassert>      // assert
#include <memory>       // construct_at / destroy_at
#include <stdexcept>    // runtime_error
#include <compare>      // partical_ordering
#include <cstddef>      // max_align_t on gcc
#include <climits>      // CHAR_BIT on gcc
#include <bit>
// TODO remove when it will be C++20 module
#undef AXIOM
#undef PLEASE_INLINE
#undef UNREACHABLE

// No prefix because it will be C++20 module when it will be possible on all compilers
#ifndef _MSC_VER
#ifdef __clang__
#define PLEASE_INLINE __attribute__((always_inline))
#define AXIOM(cond) __builtin_assume((cond))
#define UNREACHABLE() __builtin_unreachable()
#else
#define PLEASE_INLINE __attribute__((always_inline))
#define AXIOM(cond)         \
  do {                         \
    if (!(cond))               \
      __builtin_unreachable(); \
  } while (0)
#define UNREACHABLE() __builtin_unreachable()
#endif
#else
#define PLEASE_INLINE __forceinline
#define AXIOM(cond) __assume((cond))
#define UNREACHABLE() __assume(false)
#endif

// C++ features is not supported in clang ...
#ifdef __cpp_lib_hardware_interference_size
using std::hardware_constructive_interference_size;
using std::hardware_destructive_interference_size;
#else
// 64 bytes on x86-64 │ L1_CACHE_BYTES │ L1_CACHE_SHIFT │ __cacheline_aligned │ ...
constexpr inline std::size_t hardware_constructive_interference_size = 64;
constexpr inline std::size_t hardware_destructive_interference_size = 64;
#endif

// TTA == template template argument (just for better reading)
#define TTA template <typename> typename

namespace aa {

template <typename...>
struct type_list {};

inline constexpr size_t npos = size_t(-1);

// Method must have same signature for all types(except self),
// so this type used to check what signature Method have
// this means all methods must be specializable for interface_t
// typical error - concept on Method's template argument, which is false for interface_t
// (in this case you can explciitly specialize Method for interface_t)
struct interface_t {};

}  // namespace aa

namespace noexport {

template <size_t I, typename... Args>
struct number_of_impl {
  static constexpr size_t value = aa::npos;  // no such element in pack
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

template <typename Method, typename Alloc, size_t SooS>
static consteval bool check_copy() {
  if constexpr (requires { typename Method::allocator_type; })
    return std::is_same_v<typename Method::allocator_type, Alloc> && SooS == Method::SooS_value;
  else
    return true;
}
// clang-format off
template <class To, class From>
requires(sizeof(To) == sizeof(From)
&& std::is_trivially_copyable_v<From> && std::is_trivially_copyable_v<To>)
To bit_cast(const From& src) noexcept {
    return std::bit_cast<To>(src);
    // TODO - remove this shitty cast when possible))) (clang and gcc support...)
 // return *reinterpret_cast<const To*>(std::addressof(src));
}
// clang-format on
}  // namespace noexport

namespace aa {

// just unique type for every Method, because i need to inherit from plugins
template <TTA Method>
struct nullplugin {};

template <TTA Method>
concept has_plugin = requires {
  // nullplugin excepts template template argument with one input type. So it works
  typename nullplugin<Method<interface_t>::template plugin>;
};

template <TTA Method, typename>
struct plugin : std::type_identity<nullplugin<Method>> {};

template <TTA Method, typename Any>
requires has_plugin<Method>
struct plugin<Method, Any> : std::type_identity<typename Method<interface_t>::template plugin<Any>> {
};

template <TTA Method, typename Any>
using plugin_t = typename plugin<Method, Any>::type;

template <TTA Method>
concept has_explicit_interface = requires {
  typename Method<interface_t>::explicit_interface;
};

template <TTA Method>  // no matter nullplugin or what here, it is using for optimization
using satisfy = nullplugin<Method>;

// used if Method requires explicit subscribe,
// for example : struct A { using satisfies = aa::satisfies<Fooable, Drawable>; };
template <TTA... Methods>
struct satisfies : satisfy<Methods>... {
  static_assert((has_explicit_interface<Methods> && ...), "It's useless to subscribe if it is not required");
};
template <typename T, TTA... Methods>
constexpr inline bool satisfies_v = false;
// clang-format off
template<typename T, TTA Method>
concept is_satisfies = satisfies_v<T, Method> || std::is_base_of_v<satisfy<Method>, typename T::satisfies>;
// clang-format on

template <TTA Method>
using method_traits = noexport::any_method_traits<decltype(&Method<interface_t>::do_invoke)>;

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
  static PLEASE_INLINE auto value(type_erased_self_t<Method> self, Args&&... args) -> result_t<Method> {
    using self_sample = self_sample_t<Method>;
    if constexpr (has_explicit_interface<Method>)
      static_assert(is_satisfies<T, Method>,
                    "Method requires explicit subscribe and your type not satisfies it explicitly(using "
                    "satisfies = aa::satisfies<MethodName>;");

    if constexpr (std::is_lvalue_reference_v<self_sample>) {
      using real_self = std::conditional_t<const_method<Method>, const T*, T*>;
      return Method<T>::do_invoke(*std::launder(reinterpret_cast<real_self>(self)),
                                  static_cast<Args&&>(args)...);
    } else if constexpr (std::is_pointer_v<self_sample>) {
      using real_self = std::conditional_t<const_method<Method>, const T*, T*>;
      return Method<T>::do_invoke(std::launder(reinterpret_cast<real_self>(self)),
                                  static_cast<Args&&>(args)...);

    } else if constexpr (std::is_copy_constructible_v<T>) {
      return Method<T>::do_invoke(*std::launder(reinterpret_cast<const T*>(self)),
                                  static_cast<Args&&>(args)...);
    } else {
      static_assert(noexport::always_false<T>,
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
  static PLEASE_INLINE void do_invoke(const T* self) noexcept {
    std::destroy_at(self);
  }
};

template <std::destructible T>
struct move {
  // invoked only for situatuion "move small from src to EMPTY dest" and only when type is nothrow move
  // constructible. Actially relocates
  static PLEASE_INLINE void do_invoke(T* src, void* dest) {
    if constexpr (std::is_nothrow_move_constructible_v<T>) {
      std::construct_at(reinterpret_cast<T*>(dest), std::move(*src));
      std::destroy_at(src);
    } else {
      // not static assert, because will be never called, but needed to compile
      UNREACHABLE();
    }
  }
};

// Alloc without state
template <typename Alloc, size_t SooS>
struct copy_with {
  // copy logic here, because here we know type, is_nothrow_* and sizeof

  // Must return pointer to created copy,
  // if memory allocated then dest != returned value,
  // but under dest pointer size_t value (allocated bytes count)
  // Any is always empty (memory in dest is not allocated) when copy called
  template <typename T>
  struct method {
    using allocator_type = Alloc;
    static constexpr size_t SooS_value = SooS;

    static PLEASE_INLINE void* do_invoke(const T* src, void* dest) {
      if constexpr (sizeof(T) <= SooS && std::is_nothrow_copy_constructible_v<T>) {
        std::construct_at(reinterpret_cast<T*>(dest), *src);
        return dest;
      } else {
        using alloc_traits = std::allocator_traits<Alloc>;
        // alloc_sz must be std::max(SooS, sizeof(T)), it is invariant of big state
        constexpr typename alloc_traits::size_type alloc_sz = std::max(SooS, sizeof(T));
        // no fancy pointers supported
        void* ptr = Alloc{}.allocate(alloc_sz);
        if constexpr (std::is_nothrow_copy_constructible_v<T>) {
          std::construct_at(reinterpret_cast<T*>(ptr), *src);
        } else {
          try {
            std::construct_at(reinterpret_cast<T*>(ptr), *src);
          } catch (...) {
            Alloc{}.deallocate(reinterpret_cast<typename alloc_traits::pointer>(ptr), alloc_sz);
            throw;
          }
        }
        return ptr;
      }
    }
  };
};

// Alloc with state
// clang-format off
template <typename Alloc, size_t SooS>
requires(!std::is_empty_v<Alloc>)
struct copy_with<Alloc, SooS> {
  // clang-format on
  template <typename T>
  struct method {
    using allocator_type = Alloc;
    static constexpr size_t SooS_value = SooS;

    static PLEASE_INLINE void* do_invoke(const T* src, void* dest, Alloc* alloc) {
      if constexpr (sizeof(T) <= SooS && std::is_nothrow_copy_constructible_v<T>) {
        std::construct_at(reinterpret_cast<T*>(dest), *src);
        return dest;
      } else {
        using alloc_traits = std::allocator_traits<Alloc>;
        // alloc_sz must be std::max(SooS, sizeof(T)), it is invariant of big state
        constexpr typename alloc_traits::size_type alloc_sz = std::max(SooS, sizeof(T));
        // no fancy pointers supported
        void* ptr = alloc->allocate(alloc_sz);
        if constexpr (std::is_nothrow_copy_constructible_v<T>) {
          std::construct_at(reinterpret_cast<T*>(ptr), *src);
        } else {
          try {
            std::construct_at(reinterpret_cast<T*>(ptr), *src);
          } catch (...) {
            alloc->deallocate(reinterpret_cast<typename alloc_traits::pointer>(ptr), alloc_sz);
            throw;
          }
        }
        return ptr;
      }
    }
  };
};

constexpr inline auto default_any_soos = hardware_constructive_interference_size - 3 * sizeof(void*);

template <typename T>
using copy = copy_with<std::allocator<std::byte>, default_any_soos>::template method<T>;

template <typename T>
struct hash {
  static size_t do_invoke(const T& self) {
    return std::hash<T>{}(self);
  }
};

template <typename T>
struct RTTI {
  static PLEASE_INLINE const std::type_info& do_invoke() noexcept {
    return typeid(T);
  }
};

// since C++20 operator== it is a != too
template <typename T>
struct equal_to {
  static PLEASE_INLINE bool do_invoke(const T* first, const void* second) {
    return *first == *reinterpret_cast<const T*>(second);
  }
};

template <typename T>
struct spaceship {
  // See basic_any::operator<=> to understand why it is partical ordering always
  // strong and weak ordering is implicitly convertible to partical ordeting by C++20 standard!
  static PLEASE_INLINE std::partial_ordering do_invoke(const T* first, const void* second) {
    return *first <=> *reinterpret_cast<const T*>(second);
  }
};

struct empty_any_method_call : std::exception {
  [[nodiscard]] const char* what() const noexcept override {
    return "Empty any method was called";
  }
};
// utility method for basic_any
template<typename T>
struct size_of {
  static constexpr std::size_t do_invoke(const T&) noexcept {
    return sizeof(T);
  }
};
#ifdef AA_DLL_COMPATIBLE
// name 'n' because need to reduce name size - MAGIC ENUM
template <typename T>
consteval std::string_view n() {
#if defined(__clang__) || defined(__GNUC__)
  constexpr size_t prefix_len = sizeof("std::string_view n() [T = ") - sizeof("");
  return std::string_view(__PRETTY_FUNCTION__ + prefix_len,
                          sizeof(__PRETTY_FUNCTION__) - prefix_len - sizeof("]"));
#elif defined(_MSC_VER)
  constexpr size_t prefix_len =
      sizeof("class std::basic_string_view<char,struct std::char_traits<char> > __cdecl n<") - sizeof("");
  return std::string_view({__FUNCSIG__ + prefix_len, sizeof(__FUNCSIG__) - prefix_len - sizeof(">(void)")});
#else
#error Cant support dll with this compiler
#endif
}
// T always decayed, because all types decayed before constructing a vtable for them
template<typename T>
constexpr inline std::string_view type_name = n<std::decay_t<T>>();
#endif
// regardless Method is a template,
// do_invoke signature must be same for any valid T (as for virtual functions)
template <TTA... Methods>
struct vtable {
  std::tuple<type_erased_signature_t<Methods>...> table;
#ifdef AA_DLL_COMPATIBLE
  std::string_view name;
#endif

  template <TTA Method>
  static inline constexpr size_t number_of_method =
      noexport::number_of_first<Method<interface_t>, Methods<interface_t>...>;

  template <TTA Method>
  static inline constexpr bool has_method = number_of_method<Method> != npos;
  // clang-format off
  template <TTA Method, typename... Args>
  requires(has_method<Method>)
  constexpr PLEASE_INLINE decltype(auto) invoke(Args&&... args) const {
    // clang-format on
    return std::get<number_of_method<Method>>(table)(std::forward<Args>(args)...);
  }
};
// must be never called explicitly, because needed to decay type before
template <typename T, TTA... Methods>
constexpr vtable<Methods...> vtable_for = {{&invoker_for<T, Methods>::value...}
#ifdef AA_DLL_COMPATIBLE
                                           ,
                                           type_name<T>
#endif
};
// always decays type to reduce count of vtables
template<typename T, TTA... Methods>
constexpr const vtable<Methods...>* addr_vtable_for = &vtable_for<std::decay_t<T>, Methods...>; 

// Yes, msvc do not support EBO which is already GUARANTEED by C++ standard for ~13 years
#if defined(_MSC_VER)
#define AA_MSVC_EBO __declspec(empty_bases)
#else
#define AA_MSVC_EBO
#endif

// it is concept for removing ambigious ctors in polymorphic ptrs
template <typename T>
concept not_const_type = !std::is_const_v<T>;

// non nullable non owner view to any type which satisfies Methods...
template <TTA... Methods>
struct AA_MSVC_EBO polymorphic_ref : plugin_t<Methods, polymorphic_ref<Methods...>>... {
 private:
  const vtable<Methods...>* vtable_ptr;
  void* value_ptr;

  friend struct caster;
  template <TTA, typename>
  friend struct invoke_fn;
  template <TTA, typename>
  friend struct invoke_unsafe_fn;
  template <TTA...>
  friend struct polymorphic_ptr;
  // uninitialized for pointer implementation
  constexpr polymorphic_ref(std::nullptr_t) noexcept : vtable_ptr{nullptr}, value_ptr{nullptr} {
  }

 public:
  // cannot rebind reference
  polymorphic_ref(const polymorphic_ref&) = default;
  polymorphic_ref(polymorphic_ref&&) = default;
  void operator=(polymorphic_ref&&) = delete;
  void operator=(const polymorphic_ref&) = delete;
  // clang-format off
  // from mutable lvalue
  template <not_const_type T> // not shadow copy ctor
  requires(!std::same_as<polymorphic_ref<Methods...>, T>)
  constexpr polymorphic_ref(T& value) noexcept
      : vtable_ptr{addr_vtable_for<T, Methods...>}, value_ptr{std::addressof(value)} {
  }
  // clang-format on
  // returns polymorphic_ptr<Methods...>
  constexpr auto operator&() const noexcept;
};

// non nullable non owner view to any type which satisfies Methods...
template<TTA... Methods>
struct AA_MSVC_EBO const_polymorphic_ref : plugin_t<Methods, const_polymorphic_ref<Methods...>>... {
 private:
  const vtable<Methods...>* vtable_ptr;
  const void* value_ptr;

  friend struct caster;
  template <TTA, typename>
  friend struct invoke_fn;
  template <TTA, typename>
  friend struct invoke_unsafe_fn;
  template<TTA...>
  friend struct const_polymorphic_ptr;
  // uninitialized for pointer implementation
  constexpr const_polymorphic_ref(std::nullptr_t) noexcept : vtable_ptr{nullptr}, value_ptr{nullptr} {
  }
 public:
  // cannot rebind reference
  const_polymorphic_ref(const const_polymorphic_ref&) = default;
  const_polymorphic_ref(const_polymorphic_ref&&) = default;
  void operator=(const_polymorphic_ref&&) = delete;
  void operator=(const const_polymorphic_ref&) = delete;
  // clang-format off
  // from value
  template <typename T> // not shadow copy ctor
  requires(!std::same_as<const_polymorphic_ref<Methods...>, T>)
  constexpr const_polymorphic_ref(const T& value) noexcept
      : vtable_ptr{addr_vtable_for<T, Methods...>}, value_ptr{std::addressof(value)} {
  }
  // clang-format on
  // from non-const ref
  constexpr const_polymorphic_ref(polymorphic_ref<Methods...> p) noexcept
      : const_polymorphic_ref{::noexport::bit_cast<const_polymorphic_ref<Methods...>>(p)} {
  }
  // returns const_polymorphic_ptr<Methods...>
  constexpr auto operator&() const noexcept;
};

// non owning pointer-like type, behaves like pointer to mutable abstract base type
// usage example : void foo(polymorphic_ptr<Method0, Method1> p) (same Methods like in AnyAny)
template <TTA... Methods>
struct polymorphic_ptr {
 private:
  // uninitialized reference by default
  polymorphic_ref<Methods...> poly_ = nullptr;

  friend struct caster;

 public:
  // from nothing (empty)
  constexpr polymorphic_ptr() = default;
  constexpr polymorphic_ptr(std::nullptr_t) noexcept : polymorphic_ptr() {
  }
  constexpr polymorphic_ptr& operator=(std::nullptr_t) noexcept {
    poly_.value_ptr = nullptr;
    poly_.vtable_ptr = nullptr;
    return *this;
  }
  // from mutable pointer
  // clang-format off
  template <not_const_type T>
  constexpr polymorphic_ptr(T* ptr) noexcept {
      poly_.value_ptr = ptr;
      poly_.vtable_ptr = ptr != nullptr ? addr_vtable_for<T, Methods...> : nullptr;
  }
  // from mutable pointer to Any
  template <any_x Any>
  requires(not_const_type<Any> && std::same_as<typename Any::methods_list, type_list<Methods<interface_t>...>>)
  constexpr polymorphic_ptr(Any* ptr) noexcept {
      if(ptr == nullptr) [[unlikely]] {
          *this = nullptr;
          return;
      }
      poly_.vtable_ptr = ptr->vtable_ptr;
      poly_.value_ptr = ptr->value_ptr;
  }
  // clang-format on
  // observers

  constexpr void* raw() const noexcept {
    return poly_.value_ptr;
  }
  constexpr bool has_value() const noexcept {
    return poly_.value_ptr != nullptr;
  }
  constexpr bool operator==(std::nullptr_t) const noexcept {
    return !has_value();
  }
  constexpr explicit operator bool() const noexcept {
    return has_value();
  }

  // access

  constexpr polymorphic_ref<Methods...> operator*() const noexcept {
    assert(has_value());
    return poly_;
  }
  constexpr const polymorphic_ref<Methods...>* operator->() const noexcept {
    return std::addressof(poly_);
  }
};

// non owning pointer-like type, behaves like pointer to CONST abstract base type
// usage example : void foo(const_polymorphic_ptr<Method0, Method1> p) (same Methods like in AnyAny)
template <TTA... Methods>
struct const_polymorphic_ptr {
 private:
  // uninitialized reference by default
  const_polymorphic_ref<Methods...> poly_ = nullptr;

  friend struct caster;

 public:
  // from nothing(empty)
  constexpr const_polymorphic_ptr() = default;
  constexpr const_polymorphic_ptr(std::nullptr_t) noexcept : const_polymorphic_ptr() {
  }
  constexpr const_polymorphic_ptr& operator=(std::nullptr_t) noexcept {
    poly_.value_ptr = nullptr;
    poly_.vtable_ptr = nullptr;
    return *this;
  }
  // from pointer to value
  template <typename T>
  constexpr const_polymorphic_ptr(const T* ptr) noexcept {
    poly_.value_ptr = ptr;
    poly_.vtable_ptr = ptr != nullptr ? addr_vtable_for<T, Methods...> : nullptr;
  }
  // from pointer to Any
  // clang-format off
  template <any_x Any>
  requires(std::same_as<typename Any::methods_list, type_list<Methods<interface_t>...>>)
  constexpr const_polymorphic_ptr(const Any* p) noexcept {
    // clang-format on
    if (p == nullptr) [[unlikely]] {
      *this = nullptr;
      return;
    }
    poly_.vtable_ptr = p->vtable_ptr;
    poly_.value_ptr = p->value_ptr;
  }
  // from non-const polymorphic pointer
  constexpr const_polymorphic_ptr(polymorphic_ptr<Methods...> p) noexcept : poly_{*p} {
  }

  // observers

  constexpr const void* raw() const noexcept {
    return poly_.value_ptr;
  }
  constexpr bool has_value() const noexcept {
    return poly_.value_ptr != nullptr;
  }
  constexpr bool operator==(std::nullptr_t) const noexcept {
    return !has_value();
  }
  constexpr explicit operator bool() const noexcept {
    return has_value();
  }

  // access

  constexpr const_polymorphic_ref<Methods...> operator*() const noexcept {
    return poly_;
  }
  constexpr const const_polymorphic_ref<Methods...>* operator->() const noexcept {
    return std::addressof(poly_);
  }
};

template<TTA... Methods>
constexpr auto polymorphic_ref<Methods...>::operator&() const noexcept {
  return ::noexport::bit_cast<polymorphic_ptr<Methods...>>(*this);
}
template <TTA... Methods>
constexpr auto const_polymorphic_ref<Methods...>::operator&() const noexcept {
  return ::noexport::bit_cast<const_polymorphic_ptr<Methods...>>(*this);
}

// CRTP - inheritor of basic_any
// SooS == Small Object Optimization Size
// strong exception guarantee for all constructors and assignments,
// emplace<T> - *this is empty if exception thrown
// for alloc not all fancy pointers supported and construct / destroy not throught alloc
template <typename CRTP, typename Alloc, size_t SooS, TTA... Methods>
struct AA_MSVC_EBO basic_any : plugin_t<Methods, basic_any<CRTP, Alloc, SooS, Methods...>>... {
 private:
  const vtable<Methods...>* vtable_ptr = nullptr;
  void* value_ptr = &data;
  [[no_unique_address]] alignas(std::max_align_t) std::array<std::byte, SooS> data;
  [[no_unique_address]] Alloc alloc;

  // invariant of basic_any - it is always in one of those states:
  // empty - has_value() == false, memory_allocated == false
  // small - has_value() == true, memory_allocated == false, MOVE IS NOEXCEPT
  // big   - has_value() == true, memory_allocated == true,
  // count allocated bytes ALWAYS == std::max(SooS, sizeof(T)) (need to avoid UB in deallocate)

  // guarantees that small is nothrow movable(for noexcept move ctor/assign)
  template <typename T>
  static inline constexpr bool any_is_small_for =
      alignof(T) <= alignof(std::max_align_t) && std::is_nothrow_move_constructible_v<T> && sizeof(T) <= SooS;

 protected:
  template <TTA Method, typename... Args>
  PLEASE_INLINE decltype(auto) vtable_invoke(Args&&... args) {
    AXIOM(vtable_ptr != nullptr);
    return vtable_ptr->template invoke<Method>(static_cast<void*>(value_ptr), std::forward<Args>(args)...);
  }
  // clang-format off
  template <TTA Method, typename... Args> requires const_method<Method>
  PLEASE_INLINE decltype(auto) vtable_invoke(Args&&... args) const {
    AXIOM(vtable_ptr != nullptr);
    return vtable_ptr->template invoke<Method>(static_cast<const void*>(value_ptr), std::forward<Args>(args)...);
  }
  // clang-format on
 private:
  template <typename T, typename... Args>
  void emplace_in_empty(Args&&... args) {
    if constexpr (any_is_small_for<T>) {
      std::construct_at(reinterpret_cast<T*>(value_ptr), std::forward<Args>(args)...);
    } else {
      // invariant of big - allocated_size >= SooS
      constexpr size_t allocation_size = std::max(sizeof(T), SooS);
      void* old_value_ptr = std::exchange(value_ptr, alloc.allocate(allocation_size));
      if constexpr (std::is_nothrow_constructible_v<T, Args&&...>) {
        std::construct_at(reinterpret_cast<T*>(value_ptr), std::forward<Args>(args)...);
      } else {
        try {
          std::construct_at(reinterpret_cast<T*>(value_ptr), std::forward<Args>(args)...);
        } catch (...) {
          alloc.deallocate(reinterpret_cast<alloc_pointer_type>(value_ptr), allocation_size);
          value_ptr = old_value_ptr;
          throw;
        }
      }
    }
    vtable_ptr = addr_vtable_for<T, Methods...>;
  }

  using alloc_traits = std::allocator_traits<Alloc>;
  using alloc_pointer_type = typename alloc_traits::pointer;
  using alloc_size_type = typename alloc_traits::size_type;
  // for any_cast / unified function call
  friend struct caster;
  template <TTA...>
  friend struct polymorphic_ptr;
  template <TTA...>
  friend struct const_polymorphic_ptr;

  static bool equal_types_stored(const basic_any& a, const basic_any& b) noexcept {
#ifdef AA_DLL_COMPATIBLE
    if (a.vtable_ptr == nullptr || b.vtable_ptr == nullptr)
      return a.vtable_ptr == b.vtable_ptr;
    return a.vtable_ptr->name == b.vtable_ptr->name;
#else
    return a.vtable_ptr == b.vtable_ptr;
#endif
  }

  template <TTA, typename>
  friend struct invoke_fn;
  template <TTA, typename>
  friend struct invoke_unsafe_fn;

 public:
  template <TTA Method>
  static constexpr bool has_method = vtable<Methods...>::template has_method<Method>;
  static constexpr bool has_copy = has_method<copy_with<Alloc, SooS>::template method>;

  static_assert((std::is_empty_v<plugin_t<Methods, basic_any<CRTP, Alloc, SooS, Methods...>>> && ...));
  static_assert((noexport::check_copy<Methods<interface_t>, Alloc, SooS>() && ...),
                "Alloc and SooS in copy do not match Alloc in SooS in basic_any, "
                "use aa::copy_with<Alloc, SooS>::tempalte method!");
  static_assert(
      !(has_method<spaceship> && has_method<equal_to>),
      "Spaceship already contains most effective way to equality compare, if class have operator ==");
  static_assert(
      noexport::is_one_of<typename alloc_traits::value_type, std::byte, char, unsigned char>::value);
  static_assert(has_method<destroy>, "Any requires aa::destroy method");
  static_assert(has_method<size_of>, "Any requires aa::size_of method");
  static_assert(std::is_nothrow_copy_constructible_v<Alloc>, "C++ Standard requires it");

  using base_any_type = basic_any;
  using methods_list = type_list<Methods<interface_t>...>;

  using ptr = polymorphic_ptr<Methods...>;
  using const_ptr = const_polymorphic_ptr<Methods...>;
  using ref = polymorphic_ref<Methods...>;
  using const_ref = const_polymorphic_ref<Methods...>;

  constexpr ptr operator&() noexcept {
    return {this};
  }
  constexpr const_ptr operator&() const noexcept {
    return {this};
  }
  constexpr basic_any() = default;
  basic_any(Alloc alloc) noexcept : alloc(std::move(alloc)) {
  }

  constexpr ~basic_any() {
    if (has_value())
      destroy_value();
  }

  // basic_any copy/move stuff

  basic_any(const basic_any& other) requires(has_copy)
      : alloc(alloc_traits::select_on_container_copy_construction(other.alloc)) {
    if (other.has_value()) {
      if constexpr (std::is_empty_v<Alloc>)
        value_ptr =
            other.vtable_invoke<copy_with<Alloc, SooS>::template method>(static_cast<void*>(value_ptr));
      else
        value_ptr = other.vtable_invoke<copy_with<Alloc, SooS>::template method>(
            static_cast<void*>(value_ptr), &alloc);
    }
    vtable_ptr = other.vtable_ptr;
  }

  [[nodiscard]] Alloc get_allocator() const noexcept {
    return alloc;
  }
  // postcondition: other do not contain a value after move
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
  std::decay_t<T>& emplace(Args&&... args) noexcept(
      std::is_nothrow_constructible_v<std::decay_t<T>, Args&&...>&& any_is_small_for<std::decay_t<T>>) {
    reset();
    emplace_in_empty<std::decay_t<T>>(std::forward<Args>(args)...);
    return *reinterpret_cast<std::decay_t<T>*>(value_ptr);
  }
  template <typename T, typename U, typename... Args>
  std::decay_t<T>& emplace(std::initializer_list<U> list, Args&&... args) noexcept(
      std::is_nothrow_constructible_v<std::decay_t<T>, std::initializer_list<U>, Args&&...>&&
          any_is_small_for<std::decay_t<T>>) {
    reset();
    emplace_in_empty<std::decay_t<T>>(list, std::forward<Args>(args)...);
    return *reinterpret_cast<std::decay_t<T>*>(value_ptr);
  }
  template <typename T, typename... Args>
  basic_any(std::in_place_type_t<T>, Args&&... args) noexcept(
      std::is_nothrow_constructible_v<std::decay_t<T>, Args&&...>&& any_is_small_for<std::decay_t<T>>) {
    emplace_in_empty<std::decay_t<T>>(std::forward<Args>(args)...);
  }
  template <typename T, typename U, typename... Args>
  basic_any(std::in_place_type_t<T>, std::initializer_list<U> list, Args&&... args) noexcept(
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
  // clang-format on

  // postconditions : has_value() == false
  void reset() noexcept {
    if (!has_value())
      return;
    destroy_value();
    vtable_ptr = nullptr;
  }

  // observe

  [[nodiscard]] PLEASE_INLINE constexpr bool has_value() const noexcept {
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
    if (!equal_types_stored(*this, other))
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
    if (!equal_types_stored(*this, other))
      return std::partial_ordering::unordered;
    if (vtable_ptr == nullptr)  // other.vtable_ptr == nullptr too here
      return std::partial_ordering::equivalent;
    return vtable_invoke<spaceship>(static_cast<const void*>(other.value_ptr));
  }

 private :

     // precodition - has_value() == false
     // clang-format off
  void move_value_from(basic_any&& other) {
    // clang-format on
    if (!other.has_value())
      return;
    // `move` is noexcept (invariant of small state)
    // `move` also 'relocate' i.e. calls dctor of value(for remove vtable_invoke<destroy> call in future)
    if (!other.memory_allocated())
      other.vtable_invoke<move>(static_cast<void*>(value_ptr));
    else
      value_ptr = std::exchange(other.value_ptr, &other.data);
    vtable_ptr = std::exchange(other.vtable_ptr, nullptr);
  }

  PLEASE_INLINE bool memory_allocated() const {
    return value_ptr != &data;
  }
  PLEASE_INLINE size_t allocated_size() const {
    assert(has_value() && memory_allocated());
    return std::max(SooS, vtable_invoke<size_of>());
  }
  void destroy_value() {
    vtable_invoke<destroy>();
    if (memory_allocated()) {
      alloc_traits::deallocate(alloc, reinterpret_cast<alloc_pointer_type>(value_ptr), allocated_size());
      value_ptr = &data;
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
  template <typename T, typename CRTP, typename Alloc, size_t SooS, TTA... Methods>
  static const T* any_cast_impl(const basic_any<CRTP, Alloc, SooS, Methods...>* any) noexcept {
// T already remove_cv
#ifdef AA_DLL_COMPATIBLE
    if (any == nullptr || !any->has_value() || any->vtable_ptr->name != type_name<T>)
      return nullptr;
#else
    if (any == nullptr || any->vtable_ptr != addr_vtable_for<T, Methods...>)
      return nullptr;
#endif
    return std::launder(reinterpret_cast<const T*>(any->value_ptr));
  }
  template <typename T, typename CRTP, typename Alloc, size_t SooS, TTA... Methods>
  static T* any_cast_impl(basic_any<CRTP, Alloc, SooS, Methods...>* any) noexcept {
#ifdef AA_DLL_COMPATIBLE
    if (any == nullptr || !any->has_value() || any->vtable_ptr->name != type_name<T>)
      return nullptr;
#else
    if (any == nullptr || any->vtable_ptr != addr_vtable_for<T, Methods...>)
      return nullptr;
#endif
    return std::launder(reinterpret_cast<T*>(any->value_ptr));
  }
  template <typename T, TTA... Methods>
  static T any_cast_impl(polymorphic_ptr<Methods...> p) noexcept {
    static_assert(std::is_pointer_v<T>);
#ifdef AA_DLL_COMPATIBLE
    if (p == nullptr || p.poly_.vtable_ptr->name != type_name<std::remove_pointer_t<T>>)
      return nullptr;
#else
    if (p.poly_.vtable_ptr != addr_vtable_for<std::remove_pointer_t<T>, Methods...>)
      return nullptr;
#endif
    return reinterpret_cast<T>(p.raw());
  }
  template <typename T, TTA... Methods>
  static const std::remove_pointer_t<T>* any_cast_impl(const_polymorphic_ptr<Methods...> p) noexcept {
    static_assert(std::is_pointer_v<T>);
#ifdef AA_DLL_COMPATIBLE
    if (p == nullptr || p.poly_.vtable_ptr->name != type_name<std::remove_pointer_t<T>>)
      return nullptr;
#else
    if (p.poly_.vtable_ptr != addr_vtable_for<std::remove_pointer_t<T>, Methods...>)
      return nullptr;
#endif
    return reinterpret_cast<const std::remove_pointer_t<T>*>(p.raw());
  }
  template <typename T, TTA... Methods>
  static T any_cast_impl(polymorphic_ref<Methods...> p) {
    static_assert(std::is_lvalue_reference_v<T>);
#ifdef AA_DLL_COMPATIBLE
    if (p.vtable_ptr->name != type_name<T>) [[unlikely]]
      throw std::bad_cast{};
#else
    if (p.vtable_ptr != addr_vtable_for<T, Methods...>) [[unlikely]]
      throw std::bad_cast{};
#endif
    return *reinterpret_cast<std::remove_cvref_t<T>*>(p.value_ptr);
  }
  template <typename T, TTA... Methods>
  static const std::remove_reference_t<T>& any_cast_impl(const_polymorphic_ref<Methods...> p) {
    static_assert(std::is_lvalue_reference_v<T>);
#ifdef AA_DLL_COMPATIBLE
    if (p.vtable_ptr->name != type_name<T>) [[unlikely]]
      throw std::bad_cast{};
#else
    if (p.vtable_ptr != addr_vtable_for<T, Methods...>) [[unlikely]]
      throw std::bad_cast{};
#endif
    return *reinterpret_cast<const std::remove_cvref_t<T>*>(p.value_ptr);
  }
};

template <typename T, any_x U>
[[nodiscard]] PLEASE_INLINE T* any_cast(U* ptr) noexcept {
  // references ill-formed already (because of T*)
  static_assert(!(std::is_array_v<T> || std::is_function_v<T> || std::is_void_v<T>),
                "Incorrect call, it will be always nullptr");
  return caster::any_cast_impl<std::remove_cv_t<T>>(static_cast<typename U::base_any_type*>(ptr));
}
template <typename T, any_x U>
[[nodiscard]] PLEASE_INLINE const T* any_cast(const U* ptr) noexcept {
  static_assert(!(std::is_array_v<T> || std::is_function_v<T> || std::is_void_v<T>),
                "Incorrect call, it will be always nullptr");
  return caster::any_cast_impl<std::remove_cv_t<T>>(static_cast<const typename U::base_any_type*>(ptr));
}

template <typename T, any_x U>
[[nodiscard]] PLEASE_INLINE std::remove_cv_t<T> any_cast(U& any) {
  auto* ptr = any_cast<std::remove_cvref_t<T>>(std::addressof(any));
  if (!ptr)
    throw std::bad_cast{};
  return *ptr;
}
template <typename T, any_x U>
[[nodiscard]] PLEASE_INLINE std::remove_cv_t<T> any_cast(U&& any) {
  auto* ptr = any_cast<std::remove_cvref_t<T>>(std::addressof(any));
  if (!ptr)
    throw std::bad_cast{};
  return std::move(*ptr);
}
template <typename T, any_x U>
[[nodiscard]] PLEASE_INLINE std::remove_cv_t<T> any_cast(const U& any) {
  const auto* ptr = any_cast<std::remove_cvref_t<T>>(std::addressof(any));
  if (!ptr)
    throw std::bad_cast{};
  return *ptr;
}
template <typename T, TTA... Methods>
[[nodiscard]] PLEASE_INLINE auto* any_cast(polymorphic_ptr<Methods...> p) noexcept {
  return caster::any_cast_impl<T>(p);
}
template <typename T, TTA... Methods>
[[nodiscard]] PLEASE_INLINE const auto* any_cast(const_polymorphic_ptr<Methods...> p) noexcept {
  return caster::any_cast_impl<T>(p);
}
template <typename T, TTA... Methods>
[[nodiscard]] PLEASE_INLINE auto& any_cast(polymorphic_ref<Methods...> p) {
  return caster::any_cast_impl<T>(p);
}
template <typename T, TTA... Methods>
[[nodiscard]] PLEASE_INLINE const auto& any_cast(const_polymorphic_ref<Methods...> p) {
  return caster::any_cast_impl<T>(p);
}

// TEMPLATE VARIABLE (FUNCTION OBJECT) invoke <Method> (any)

// hack for compilation time / obj size reduce + accept exactly user args
// (for example i can write invoke<Foo>(Any, {}, {1, 2, 3}) because compiler knows what types in must be

template <TTA Method, typename = args_list<Method>>
struct invoke_unsafe_fn;

template <TTA Method, typename... Args>
struct invoke_unsafe_fn<Method, type_list<Args...>> {
  // FOR ANY

  template <any_x U>
  PLEASE_INLINE result_t<Method> operator()(U&& any, Args... args) const {
    AXIOM(any.vtable_ptr != nullptr);
    return any.template vtable_invoke<Method>(static_cast<Args&&>(args)...);
  }
  // clang-format off
  template <any_x U>
  PLEASE_INLINE result_t<Method> operator()(const U& any, Args... args) const {
    // clang-format on
    static_assert(const_method<Method>);
    AXIOM(any.vtable_ptr != nullptr);
    return any.template vtable_invoke<Method>(static_cast<Args&&>(args)...);
  }

  // FOR POLYMORPHIC REF

  template <TTA... Methods>
  PLEASE_INLINE result_t<Method> operator()(polymorphic_ref<Methods...> p, Args... args) const {
    return p.vtable_ptr->template invoke<Method>(p.value_ptr, static_cast<Args&&>(args)...);
  }
  template <TTA... Methods>
  PLEASE_INLINE result_t<Method> operator()(const_polymorphic_ref<Methods...> p, Args... args) const {
    static_assert(const_method<Method>);
    return p.vtable_ptr->template invoke<Method>(p.value_ptr, static_cast<Args&&>(args)...);
  }
};

template <typename T>
struct invoke_unsafe_fn<destroy, T> {
  static_assert(noexport::always_false<T>,
                "Invoking destructor of contained value in any by hands is a bad idea");
};
// for cases, when you sure any has value (so UB if !has_value), compilers bad at optimizations(
template <TTA Method>
constexpr invoke_unsafe_fn<Method> invoke_unsafe = {};

template <TTA Method, typename = args_list<Method>>
struct invoke_fn;

template <TTA Method, typename... Args>
struct invoke_fn<Method, type_list<Args...>> {
  // FOR ANY

  template <any_x U>
  PLEASE_INLINE result_t<Method> operator()(U&& any, Args... args) const {
    if (!any.has_value()) [[unlikely]]
      throw empty_any_method_call{};
    if constexpr (std::is_void_v<result_t<Method>>) {
      any.template vtable_invoke<Method>(static_cast<Args&&>(args)...);
      AXIOM(any.vtable_ptr != nullptr);
    } else {
      result_t<Method> result = any.template vtable_invoke<Method>(static_cast<Args&&>(args)...);
      // at this point compiler do not know, that 'any' state is not changed after vtable_invoke
      AXIOM(any.vtable_ptr != nullptr);
      return result;
    }
  }
  template <any_x U>
  PLEASE_INLINE result_t<Method> operator()(const U& any, Args... args) const {
    static_assert(const_method<Method>);
    if (!any.has_value()) [[unlikely]]
      throw empty_any_method_call{};
    if constexpr (std::is_void_v<result_t<Method>>) {
      any.template vtable_invoke<Method>(static_cast<Args&&>(args)...);
      AXIOM(any.vtable_ptr != nullptr);
    } else {
      // at this point compiler do not know, that 'any' state is not changed after vtable_invoke
      result_t<Method> result = any.template vtable_invoke<Method>(static_cast<Args&&>(args)...);
      AXIOM(any.vtable_ptr != nullptr);
      return result;
    }
  }

  // FOR POLYMORPHIC REF

  template <TTA... Methods>
  PLEASE_INLINE result_t<Method> operator()(polymorphic_ref<Methods...> p, Args... args) const {
    return p.vtable_ptr->template invoke<Method>(p.value_ptr, static_cast<Args&&>(args)...);
  }
  template <TTA... Methods>
  PLEASE_INLINE result_t<Method> operator()(const_polymorphic_ref<Methods...> p, Args... args) const {
    static_assert(const_method<Method>);
    return p.vtable_ptr->template invoke<Method>(p.value_ptr, static_cast<Args&&>(args)...);
  }
};

template <typename T>
struct invoke_fn<destroy, T> {
  static_assert(noexport::always_false<T>,
                "Invoking destructor of contained value in any by hands is a bad idea");
};

template <TTA Method>
constexpr invoke_fn<Method> invoke = {};

// Strong alias to basic_any with Alloc, SooS and Methods..., used to cover CRTP
template <typename Alloc, size_t SooS, TTA... Methods>
struct any_with_t : basic_any<any_with_t<Alloc, SooS, Methods...>, Alloc, SooS, Methods...> {
 private:
  using base_t = basic_any<any_with_t<Alloc, SooS, Methods...>, Alloc, SooS, Methods...>;

 public:
  using base_t::base_t;
};

template <TTA... Methods>
using any_with = any_with_t<std::allocator<std::byte>, default_any_soos, size_of, destroy, Methods...>;

template<typename Alloc, size_t SooS, TTA... Methods>
using basic_any_with = any_with_t<Alloc, SooS, size_of, destroy, Methods...>;

}  // namespace aa

namespace std {

// clang-format off
template<::aa::any_x T> requires (T::template has_method<::aa::hash>)
struct hash<T> {
  size_t operator()(const T& any) const {
      return any.has_value() ? aa::invoke_unsafe<::aa::hash>(any) : 0;
  }
};
// clang-format on

}  // namespace std

#undef AXIOM
#undef PLEASE_INLINE
#undef UNREACHABLE
