#pragma once

#include <array>
#include <utility>      // std::exchange
#include <cassert>      // assert
#include <memory>       // construct_at / destroy_at
#include <stdexcept>    // bad_cast/exception
#include <compare>      // partial_ordering
#include <cstddef>      // max_align_t
#include <optional>     // for type_switch
#include <functional>   // std::invoke
#include <variant>      // for std_variant_poly_traits only

// Yes, msvc do not support EBO which is already GUARANTEED by C++ standard for ~13 years
#if defined(_MSC_VER)
#define AA_MSVC_EBO __declspec(empty_bases)
#else
#define AA_MSVC_EBO
#endif

// TTA == template template argument (just for better reading)
#define TTA template <typename> typename

namespace aa {

template <typename...>
struct type_list {};

constexpr inline size_t npos = size_t(-1);

template <typename T>
concept lambda_without_capture =
    std::default_initializable<std::remove_cvref_t<T>> && requires {
                                                            { &std::remove_cvref_t<T>::operator() };
                                                          };

// Method must have same signature for all types(except self),
// so this type used to check what signature Method have
// this means all methods must be specializable for interface_t
// typical error - concept on Method's template argument, which is false for interface_t
// (in this case you can explciitly specialize Method for interface_t)
struct interface_t {};

}  // namespace aa

namespace noexport {

// this tuple exist only because i cant constexpr cast function pointer to void* for storing in vtable
template <typename T, size_t>
struct value_in_tuple {
  T value{};
};

template <typename...>
struct tuple_base;

template <size_t... Is, typename... Ts>
struct AA_MSVC_EBO tuple_base<std::index_sequence<Is...>, Ts...> : value_in_tuple<Ts, Is>... {
  constexpr tuple_base(Ts... args) noexcept : value_in_tuple<Ts, Is>{static_cast<Ts&&>(args)}... {
  }
};
// stores values always in right order(in memory), used only for function pointers in vtable
template <typename... Ts>
struct tuple : tuple_base<std::index_sequence_for<Ts...>, Ts...> {
  using tuple::tuple_base::tuple_base;
};

// in this library tuple used ONLY for function pointers in vtable, so get_value returns value
template <size_t I, typename U>
constexpr U get_value(value_in_tuple<U, I> v) noexcept {
  return v.value;
}

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
  if (std::is_reference_v<Self>)
    return std::is_const_v<std::remove_reference_t<Self>>;
  return true;  // passing by value is a const method!
}

template <typename>
struct any_method_traits;

// returns false if it is ill-formed to pass non-const reference into function which accepts T
template <typename T>
consteval bool is_const_arg() {
  return !std::is_reference_v<T> || std::is_const_v<std::remove_reference_t<T>>;
}
// Note: for signature && added to arguments for forwarding
template <typename R, typename Self, typename... Args>
struct any_method_traits<R (*)(Self, Args...)> {
  using self_sample_type = Self;
  using result_type = R;
  static constexpr bool is_const = is_const_method<Self>();
  using type_erased_self_type = std::conditional_t<is_const, const void*, void*>;
  using type_erased_signature_type = R (*)(type_erased_self_type, Args&&...);
  using args = aa::type_list<Args...>;
  // for invoke_match
  static constexpr bool is_noexcept = false;
  using all_args = aa::type_list<Self, Args...>;
  using decay_args = aa::type_list<std::decay_t<Self>, std::decay_t<Args>...>;
  static constexpr std::size_t args_count = sizeof...(Args) + 1;
  static constexpr std::array args_const = {is_const_arg<Self>(), is_const_arg<Args>()...};
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
  // for invoke_match
  static constexpr bool is_noexcept = true;
  using all_args = aa::type_list<Self, Args...>;
  using decay_args = aa::type_list<std::decay_t<Self>, std::decay_t<Args>...>;
  static constexpr std::size_t args_count = sizeof...(Args) + 1;
  static constexpr std::array args_const = {is_const_arg<Self>(), is_const_arg<Args>()...};
};
// for invoke_match (only lambdas without capture)
template <typename Class, typename R, typename... Args>
struct any_method_traits<R (Class::*)(Args...) const> {
  using result_type = R;
  static constexpr bool is_noexcept = false;
  using all_args = aa::type_list<Args...>;
  using decay_args = aa::type_list<std::decay_t<Args>...>;
  static constexpr std::size_t args_count = sizeof...(Args);
  static constexpr std::array args_const = {is_const_arg<Args>()...};
};
template <typename Class, typename R, typename... Args>
struct any_method_traits<R (Class::*)(Args...) const noexcept> {
  using result_type = R;
  static constexpr bool is_noexcept = true;
  using all_args = aa::type_list<Args...>;
  using decay_args = aa::type_list<std::decay_t<Args>...>;
  static constexpr std::size_t args_count = sizeof...(Args);
  static constexpr std::array args_const = {is_const_arg<Args>()...};
};

template <aa::lambda_without_capture T>
struct any_method_traits<T> : any_method_traits<decltype(&T::operator())> {};

template <typename Method, typename Alloc, size_t SooS>
static consteval bool check_copy() {
  if constexpr (requires { typename Method::allocator_type; })
    return std::is_same_v<typename Method::allocator_type, Alloc> && SooS == Method::SooS_value;
  else
    return true;
}

consteval bool starts_with(aa::type_list<>, auto&&) {
  return true;
}
// first type not equal
consteval bool starts_with(auto&&, auto&&) {
  return false;
}
template <typename Head, typename... Ts1, typename... Ts2>
consteval bool starts_with(aa::type_list<Head, Ts1...>, aa::type_list<Head, Ts2...>) {
  return starts_with(aa::type_list<Ts1...>{}, aa::type_list<Ts2...>{});
}

// returns index in list where first typelist starts as subset in second typelist or npos if no such index
template <typename... Ts1, typename Head, typename... Ts2>
consteval size_t find_subset(aa::type_list<Ts1...> needle, aa::type_list<Head, Ts2...> all,
                             size_t n = 0) noexcept {
  if constexpr (sizeof...(Ts1) >= sizeof...(Ts2) + 1)
    return std::is_same_v<aa::type_list<Ts1...>, aa::type_list<Head, Ts2...>> ? n : ::aa::npos;
  else if constexpr (starts_with(needle, all))
    return n;
  else
    return find_subset(needle, aa::type_list<Ts2...>{}, n + 1);
}

// name 'n' because need to reduce name size
template <typename T>
consteval const char* n() {
#if defined(__GNUC__) && !defined(__clang__)
  return __PRETTY_FUNCTION__ + sizeof("consteval const char* noexport::n() [with T =");
#elif defined(__clang__)
  return __PRETTY_FUNCTION__ + sizeof("const char *noexport::n() [T =");
#elif defined(_MSC_VER)
  const char* res = __FUNCSIG__ + sizeof("const char *__cdecl noexport::n<") - 1;
  // MSVC may return different names for SAME type if it was decalred as struct and implemented as class or
  // smth like
  std::string_view s{res};
  if (s.starts_with("class"))
    res += sizeof("class");
  else if (s.starts_with("struct"))
    res += sizeof("struct");
  // fundamental types has no 'prefix'
  return res;
#else
#define AA_CANT_GET_TYPENAME
  // it will not break dll support, because global variables addresses may differ only on windows, where
  // msvc/clang support type names getting
  return nullptr;
#endif
}
// do not use it explicitly, use aa::descriptor_v
#ifdef AA_CANT_GET_TYPENAME
template <typename T>
constexpr const void* descriptor = &descriptor<T>;
#else
template <typename T>
constexpr const char* descriptor = n<T>();
#endif
// inlinable(std::strcmp no)
// assumes a && b != nullptr
inline constexpr std::strong_ordering strcmp(const char* l, const char* r) noexcept {
  for (; *l == *r && *l != '\0'; ++l, ++r)
    ;
  return *l <=> *r;
}

// minimal flat_map impl for invoke_match
template<typename Key, typename Value, std::size_t N, typename Compare = std::less<Key>>
struct flat_map {
 private:
  std::array<Key, N> keys;
  std::array<Value, N> values;

 public:
  constexpr flat_map(std::array<std::pair<Key, Value>, N> arr) {
    // sort by keys
    std::ranges::sort(arr, [](auto& l, auto& r) { return l.first < r.first; });
    auto kb = keys.begin();
    auto kv = values.begin();
    // clang do not supports views like 'keys' now =(
    for (auto& [k, v] : arr) {
      *kb = k;
      *kv = v;
      ++kb, ++kv;
    }
    bool b = false;
    assert(b = true);
    if (std::is_constant_evaluated() || b) {
      auto it = std::unique(keys.begin(), keys.end(),
                            [](auto& l, auto& r) { return !Compare{}(l, r) && !Compare{}(r, l); });
      if (it != keys.end())
        throw "keys are not unique!";
    }
  }
  // its minimal impl, do not support transparent compare
  constexpr auto find(const Key& key) const noexcept(requires(Key v) {
                                                       { Compare{}(v, v) } noexcept;
                                                     }) {
    auto it = std::ranges::lower_bound(keys, key, Compare{});
    // lower_bound returns such 'it' for which 'key' <= *it, so check if it true, that key < *it
    if (it == keys.end() || Compare{}(key, *it))
      return values.end();
    return std::next(values.begin(), std::distance(keys.begin(), it));
  }
  constexpr auto end() const noexcept {
    return values.end();
  }
};

// helper for invoke_match
// mostly exist because MSVC exist (and make ICE when auto* in template decl)
template <auto V,
          typename = typename ::noexport::any_method_traits<std::remove_cvref_t<decltype(V)>>::all_args>
struct make_wrapper {};

template <aa::lambda_without_capture auto V, typename X>
struct make_wrapper<V, X> : std::type_identity<std::remove_cvref_t<decltype(V)>> {};

template <auto V, typename... Args>
  requires(!aa::lambda_without_capture<decltype(V)>)
struct make_wrapper<V, aa::type_list<Args...>> {
  struct type {
    decltype(auto) operator()(Args... args) const noexcept(noexcept(V(static_cast<Args&&>(args)...))) {
      return V(static_cast<Args&&>(args)...);
    }
  };
};

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

template<TTA... Methods>
constexpr inline bool satisfies_v<interface_t, Methods...> = true;

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
  static auto value(type_erased_self_t<Method> self, Args&&... args) -> result_t<Method> {
    using self_sample = self_sample_t<Method>;
    if constexpr (has_explicit_interface<Method>)
      static_assert(is_satisfies<T, Method>,
                    "Method requires explicit subscribe and your type not satisfies it explicitly(using "
                    "satisfies = aa::satisfies<MethodName>;");

    if constexpr (std::is_lvalue_reference_v<self_sample>) {
      using real_self = std::conditional_t<const_method<Method>, const T*, T*>;
      return Method<T>::do_invoke(*std::launder(reinterpret_cast<real_self>(self)),
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
  static void do_invoke(const T& self) noexcept {
    std::destroy_at(std::addressof(self));
  }
};

template <std::destructible T>
struct move {
  // invoked only for situatuion "move small from src to EMPTY dest" and only when type is nothrow move
  // constructible. Actially relocates
  static void do_invoke(T& src, void* dest) {
    if constexpr (std::is_nothrow_move_constructible_v<T>) {
        // TODO std::relocate ? When possible, if type is relocable
      std::construct_at(reinterpret_cast<T*>(dest), std::move(src));
      std::destroy_at(std::addressof(src));
    } else {
      // not static assert, because will be never called, but needed to compile
      assert(false);
    }
  }
};

// Alloc without state
template <typename Alloc, size_t SooS>
struct copy_with {
  // returns pointer to created copy
  // precondition: Any is empty
  // postcondition:
  // if memory allocated then 'dest' != returned value,
  // and 'dest' points to size_t value (allocated bytes count)
  template <typename T>
  struct method {
    using allocator_type = Alloc;
    static constexpr size_t SooS_value = SooS;

    static void* do_invoke(const T& src, void* dest) {
      if constexpr (sizeof(T) <= SooS && std::is_nothrow_copy_constructible_v<T>) {
        std::construct_at(reinterpret_cast<T*>(dest), src);
        return dest;
      } else {
        using alloc_traits = std::allocator_traits<Alloc>;
        // alloc_sz must be std::max(SooS, sizeof(T)), it is invariant of big state
        constexpr typename alloc_traits::size_type alloc_sz = std::max(SooS, sizeof(T));
        // no fancy pointers supported
        void* ptr = Alloc{}.allocate(alloc_sz);
        if constexpr (std::is_nothrow_copy_constructible_v<T>) {
          std::construct_at(reinterpret_cast<T*>(ptr), src);
        } else {
          try {
            std::construct_at(reinterpret_cast<T*>(ptr), src);
          } catch (...) {
            Alloc{}.deallocate(reinterpret_cast<typename alloc_traits::pointer>(ptr), alloc_sz);
            throw;
          }
        }
        std::construct_at(reinterpret_cast<std::size_t*>(dest), alloc_sz);
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

    static void* do_invoke(const T& src, void* dest, Alloc& alloc) {
      if constexpr (sizeof(T) <= SooS && std::is_nothrow_copy_constructible_v<T>) {
        std::construct_at(reinterpret_cast<T*>(dest), src);
        return dest;
      } else {
        using alloc_traits = std::allocator_traits<Alloc>;
        // alloc_sz must be std::max(SooS, sizeof(T)), it is invariant of big state
        constexpr typename alloc_traits::size_type alloc_sz = std::max(SooS, sizeof(T));
        // no fancy pointers supported
        void* ptr = alloc.allocate(alloc_sz);
        if constexpr (std::is_nothrow_copy_constructible_v<T>) {
          std::construct_at(reinterpret_cast<T*>(ptr), src);
        } else {
          try {
            std::construct_at(reinterpret_cast<T*>(ptr), src);
          } catch (...) {
            alloc.deallocate(reinterpret_cast<typename alloc_traits::pointer>(ptr), alloc_sz);
            throw;
          }
        }
        std::construct_at(reinterpret_cast<std::size_t*>(dest), alloc_sz);
        return ptr;
      }
    }
  };
};

constexpr inline auto default_any_soos = 64 - 3 * sizeof(void*);

template <typename T>
using copy = copy_with<std::allocator<std::byte>, default_any_soos>::template method<T>;

// enables std::hash specialization for polymorphic value and reference
template <typename T>
struct hash {
  static size_t do_invoke(const T& self) {
    return std::hash<T>{}(self);
  }
};

// since C++20 operator== it is a != too
template <typename T>
struct equal_to {
  static bool do_invoke(const T& first, const void* second) {
    return first == *reinterpret_cast<const T*>(second);
  }
};

template <typename T>
struct spaceship {
  // See basic_any::operator<=> to understand why it is partical ordering always
  // strong and weak ordering is implicitly convertible to partical ordeting by C++20 standard!
  static std::partial_ordering do_invoke(const T& first, const void* second) {
    return first <=> *reinterpret_cast<const T*>(second);
  }
};

// Creates a Method from invocable object with given signature
// usage example:
// template<typename T>
// using Size = aa::from_callable<std::size_t(), std::ranges::size>::const_method<T>;
// using any_sized_range = aa::any_with<Size>;
template <typename Signature, auto>
struct from_callable;

template <typename R, typename... Ts, auto Foo>
struct from_callable<R(Ts...), Foo> {
  template <typename T>
  struct method {
    static R do_invoke(T& self, Ts... args) {
      return Foo(self, static_cast<Ts&&>(args)...);
    }
  };
  template <typename T>
  struct const_method {
    static R do_invoke(const T& self, Ts... args) {
      return Foo(self, static_cast<Ts&&>(args)...);
    }
  };
};

// void by default
struct descriptor_t {
 private:
  using raw_desc_type = std::remove_cvref_t<decltype(::noexport::descriptor<int>)>;
  raw_desc_type _value;

 public:
  // creates descriptor for 'void' type
  constexpr descriptor_t() noexcept : _value(noexport::descriptor<void>) {
  }
  constexpr explicit descriptor_t(raw_desc_type value) noexcept : _value(value) {
  }
  constexpr bool operator==(const descriptor_t& other) const noexcept {
#ifdef AA_CANT_GET_TYPENAME
    return _value == other._value;
#else
    if (!std::is_constant_evaluated()) {
      // cant compare pointers from different objects on compile time
      if (_value == other._value) [[unlikely]]
        return true;
    }
    return ::noexport::strcmp(_value, other._value) == std::strong_ordering::equal;
#endif
  }

#ifdef AA_CANT_GET_TYPENAME
#define AA_CONSTEXPR
#define AA_CONSTEVAL
#else
#define AA_CONSTEXPR constexpr
#define AA_CONSTEVAL consteval
#endif
  AA_CONSTEXPR std::strong_ordering operator<=>(const descriptor_t& other) const noexcept {
#ifdef AA_CANT_GET_TYPENAME
    return (uintptr_t)_value <=> (uintptr_t)other._value;
#else
    return ::noexport::strcmp(_value, other._value);
#endif
  }
};
// always decays type
template<typename T>
constexpr descriptor_t descriptor_v{::noexport::descriptor<std::decay_t<T>>};

// regardless Method is a template,
// do_invoke signature must be same for any valid T (as for virtual functions)
template <TTA... Methods>
struct vtable {
  ::noexport::tuple<type_erased_signature_t<Methods>...> table;

  template <TTA Method>
  static inline constexpr size_t number_of_method =
      noexport::number_of_first<Method<interface_t>, Methods<interface_t>...>;

  template <TTA Method>
  static inline constexpr bool has_method = number_of_method<Method> != npos;
  // clang-format off
  template <TTA Method, typename... Args>
  requires(has_method<Method>)
  constexpr decltype(auto) invoke(Args&&... args) const {
    // clang-format on
    return ::noexport::get_value<number_of_method<Method>>(table)(std::forward<Args>(args)...);
  }
};

template<TTA... Methods>
struct vtable_with_metainfo {
  // TODO check не ломает ли это инварианты апкаста
  descriptor_t type_descriptor;
  const void* const terminator = nullptr;  // indicates vtable begin! invariant - always nullptr
  vtable<Methods...> table;
};

// precondition: vtable_ptr points to 'vtable<Methoods...>; which was created as field in struct 'vtable_with_metainfo'
// (any vtable created by poly_ptr/ref/any_with satisfies this requirement)
inline descriptor_t get_type_descriptor(const void* vtable_ptr) noexcept {
  static_assert(sizeof(vtable<destroy, copy>) == sizeof(std::array<void*, 2>));
  static_assert(sizeof(vtable_with_metainfo<destroy>) == sizeof(std::array<void*, 3>));
  // standard layout guaratees that 'this' can be converted to pointer to first field
  static_assert(std::is_standard_layout_v<vtable_with_metainfo<>>);
  // for example table is [A,B,C] and A, B, C is pointers to Method<X>::do_invoke
  // up-cast reinterprets some of A, B, C as new vtable begin
  // for creating pointer to table like [B, C] or [C])
  // so this method searches real start of vtable and returns type descriptor by using guarantee that
  // vtable always created as field in struct 'vtable_with_metainfo'
  using value_type = const void*;
  auto* p = reinterpret_cast<const value_type*>(vtable_ptr);
  do {
    --p;
  } while (*p);
  auto* type_desc_ptr = reinterpret_cast<const std::byte*>(p) - offsetof(vtable_with_metainfo<>, terminator);
  return *reinterpret_cast<const descriptor_t*>(type_desc_ptr);
}

// casts vtable to subvtable with smaller count of Methods if ToMethods are contigous subset of FromMethods
// For example vtable<M1,M2,M3,M4>* can be converted to vtable<M2,M3>*, but not to vtable<M2,M4>* 
// clang-format off
// first argument only for deducting ToMethods(or internal compiler error on gcc...)
template <TTA... ToMethods, TTA... FromMethods>
requires(::noexport::find_subset(
                            type_list<ToMethods<interface_t>...>{},
                            type_list<FromMethods<interface_t>...>{}) != npos)
const vtable<ToMethods...>* subtable_ptr(const vtable<FromMethods...>* ptr) noexcept {
  // clang-format on
  assert(ptr != nullptr);
  constexpr std::size_t Index = ::noexport::find_subset(type_list<ToMethods<interface_t>...>{},
                                                        type_list<FromMethods<interface_t>...>{});
  static_assert(sizeof(vtable<FromMethods...>) == sizeof(void*) * sizeof...(FromMethods));
  return reinterpret_cast<const vtable<ToMethods...>*>(reinterpret_cast<const std::byte*>(ptr) +
                                                       sizeof(void*) * Index);
}

// must be never named explicitly, use addr_vtable_for
template <typename T, TTA... Methods>
constexpr vtable_with_metainfo<Methods...> vtable_for = {
    descriptor_v<T>, nullptr /* terminator */, vtable<Methods...>{{&invoker_for<T, Methods>::value...}}};

// always decays type
template<typename T, TTA... Methods>
constexpr const vtable<Methods...>* addr_vtable_for = &vtable_for<std::decay_t<T>, Methods...>.table; 

// it is concept for removing ambigious ctors in poly ptrs
template <typename T>
concept not_const_type = !std::is_const_v<T>;

// non nullable non owner view to any type which satisfies Methods...
template <TTA... Methods>
struct AA_MSVC_EBO poly_ref : plugin_t<Methods, poly_ref<Methods...>>... {
 private:
  const vtable<Methods...>* vtable_ptr;  // unspecified value if value_ptr == nullptr
  void* value_ptr;

  static_assert((std::is_empty_v<plugin_t<Methods, poly_ref<Methods...>>> && ...));
  template <TTA, typename>
  friend struct invoke_fn;
  template <TTA, typename>
  friend struct invoke_unsafe_fn;
  template <TTA...>
  friend struct poly_ptr;
  template <TTA...>
  friend struct poly_ref;
  template <TTA...>
  friend struct const_poly_ptr;
  template <TTA...>
  friend struct const_poly_ref;
  // uninitialized for pointer implementation
  constexpr poly_ref(std::nullptr_t) noexcept : vtable_ptr{nullptr}, value_ptr{nullptr} {
  }

 public:
  poly_ref(const poly_ref&) = default;
  poly_ref(poly_ref&&) = default;
  // cannot rebind reference
  void operator=(poly_ref&&) = delete;
  void operator=(const poly_ref&) = delete;

  // clang-format off
  // from mutable lvalue
  template <not_const_type T> // not shadow copy ctor
  requires(!std::same_as<poly_ref<Methods...>, T> && !any_x<T>)
  constexpr poly_ref(T& value) noexcept
      : vtable_ptr{addr_vtable_for<T, Methods...>}, value_ptr{std::addressof(value)} {
    // clang-format on
    static_assert(!std::is_array_v<T> && !std::is_function_v<T>,
                  "Decay it before emplace, ambigious pointer");
  }

  template <
      TTA... FromMethods,
      typename = std::void_t<decltype(subtable_ptr<Methods...>(std::declval<vtable<FromMethods...>*>()))>>
  constexpr poly_ref(poly_ref<FromMethods...> p) noexcept

      : vtable_ptr(subtable_ptr<Methods...>(p.vtable_ptr)), value_ptr(p.value_ptr) {
  }
  // for same interface(in plugins for example), always returns true
  static consteval bool has_value() noexcept {
    return true;
  }
  // returns poly_ptr<Methods...>
  constexpr auto operator&() const noexcept;

  descriptor_t type_descriptor() const noexcept {
    return get_type_descriptor(vtable_ptr);
  }
};

// non nullable non owner view to any type which satisfies Methods...
// Do not extends lifetime!!! (when constructed from literal '5' for example)
template<TTA... Methods>
struct AA_MSVC_EBO const_poly_ref : plugin_t<Methods, const_poly_ref<Methods...>>... {
 private:
  const vtable<Methods...>* vtable_ptr;  // unspecified value if value_ptr == nullptr
  const void* value_ptr;

  static_assert((std::is_empty_v<plugin_t<Methods, poly_ref<Methods...>>> && ...));

  template <TTA, typename>
  friend struct invoke_fn;
  template <TTA, typename>
  friend struct invoke_unsafe_fn;
  template <TTA...>
  friend struct const_poly_ptr;
  template <TTA...>
  friend struct const_poly_ref;
  // uninitialized for pointer implementation
  constexpr const_poly_ref(std::nullptr_t) noexcept : vtable_ptr{nullptr}, value_ptr{nullptr} {
  }

 public:

  const_poly_ref(const const_poly_ref&) = default;
  const_poly_ref(const_poly_ref&&) = default;
  // cannot rebind reference
  void operator=(const_poly_ref&&) = delete;
  void operator=(const const_poly_ref&) = delete;

  // clang-format off
  // from value
  template <typename T> // not shadow copy ctor
  requires(!std::same_as<const_poly_ref<Methods...>, T> && !any_x<T>)
  constexpr const_poly_ref(const T& value) noexcept
      : vtable_ptr{addr_vtable_for<T, Methods...>}, value_ptr{std::addressof(value)} {
      static_assert(!std::is_array_v<T> && !std::is_function_v<T>, "Decay it before emplace, ambigious pointer");
  }
  // clang-format on
  // from non-const ref
  constexpr const_poly_ref(poly_ref<Methods...> p) noexcept
      : vtable_ptr(p.vtable_ptr), value_ptr(p.value_ptr) {
  }
  template <
      TTA... FromMethods,
      typename = std::void_t<decltype(subtable_ptr<Methods...>(std::declval<vtable<FromMethods...>*>()))>>
  constexpr const_poly_ref(const_poly_ref<FromMethods...> p) noexcept
      : vtable_ptr(subtable_ptr<Methods...>(p.vtable_ptr)), value_ptr(p.value_ptr) {
  }
  template <
      TTA... FromMethods,
      typename = std::void_t<decltype(subtable_ptr<Methods...>(std::declval<vtable<FromMethods...>*>()))>>
  constexpr const_poly_ref(poly_ref<FromMethods...> p) noexcept
      : vtable_ptr(subtable_ptr<Methods...>(p.vtable_ptr)), value_ptr(p.value_ptr) {
  }
  // for same interface(in plugins for example), always returns true
  static consteval bool has_value() noexcept {
    return true;
  }
  // returns const_poly_ptr<Methods...>
  constexpr auto operator&() const noexcept;

  descriptor_t type_descriptor() const noexcept {
    return get_type_descriptor(vtable_ptr);
  }
};

template <TTA... Methods>
const_poly_ref(poly_ref<Methods...>) -> const_poly_ref<Methods...>;

// non owning pointer-like type, behaves like pointer to mutable abstract base type
// usage example : void foo(poly_ptr<Method0, Method1> p) (same Methods like in AnyAny)
template <TTA... Methods>
struct poly_ptr {
 private:
  // uninitialized reference by default
  poly_ref<Methods...> poly_ = nullptr;

  template <TTA...>
  friend struct poly_ptr;
  template <TTA...>
  friend struct const_poly_ptr;
  template <TTA...>
  friend struct poly_ref;

 public:
  // from nothing (empty)
  constexpr poly_ptr() = default;
  constexpr poly_ptr(std::nullptr_t) noexcept : poly_ptr() {
  }
  constexpr poly_ptr& operator=(std::nullptr_t) noexcept {
    poly_.value_ptr = nullptr;
    // value of vtable_ptr does not matter if value_ptr == nullptr
    return *this;
  }
  // from mutable pointer
  // clang-format off
  template <not_const_type T>
  constexpr poly_ptr(T* ptr) noexcept {
      poly_.value_ptr = ptr;
      poly_.vtable_ptr = addr_vtable_for<T, Methods...>;
  }
  // from mutable pointer to Any
  template <any_x Any>
  requires(not_const_type<Any> && noexport::find_subset(type_list<Methods<interface_t>...>{}, typename Any::methods_list{}) != npos)
  constexpr poly_ptr(Any* ptr) noexcept {
    // clang-format on
    if (ptr != nullptr) [[likely]] {
      poly_.vtable_ptr = subtable_ptr<Methods...>(ptr->vtable_ptr);
      poly_.value_ptr = ptr->value_ptr;
    }
  }
  template <
      TTA... FromMethods,
      typename = std::void_t<decltype(subtable_ptr<Methods...>(std::declval<vtable<FromMethods...>*>()))>>
  constexpr poly_ptr(poly_ptr<FromMethods...> p) noexcept {
    poly_.value_ptr = p.poly_.value_ptr;
    poly_.vtable_ptr = subtable_ptr<Methods...>(p.poly_.vtable_ptr);
  }
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

  constexpr poly_ref<Methods...> operator*() const noexcept {
    assert(has_value());
    return poly_;
  }
  constexpr const poly_ref<Methods...>* operator->() const noexcept {
    return std::addressof(poly_);
  }

  // compare

  constexpr bool operator==(const poly_ptr& other) const noexcept {
    return raw() == other.raw() && poly_.vtable_ptr == other.poly_.vtable_ptr;
  }

  // returns descriptor for void if *this == nullptr
  descriptor_t type_descriptor() const noexcept {
    return has_value() ? poly_.type_descriptor() : descriptor_t{};
  }
};

// non owning pointer-like type, behaves like pointer to CONST abstract base type
// usage example : void foo(const_poly_ptr<Method0, Method1> p) (same Methods like in AnyAny)
template <TTA... Methods>
struct const_poly_ptr {
 private:
  // uninitialized reference by default
  const_poly_ref<Methods...> poly_ = nullptr;

  template <TTA...>
  friend struct const_poly_ptr;
  template <TTA...>
  friend struct const_poly_ref;

 public:
  // from nothing(empty)
  constexpr const_poly_ptr() = default;
  constexpr const_poly_ptr(std::nullptr_t) noexcept : const_poly_ptr() {
  }
  constexpr const_poly_ptr& operator=(std::nullptr_t) noexcept {
    poly_.value_ptr = nullptr;
    poly_.vtable_ptr = nullptr;
    return *this;
  }
  // from pointer to value
  template <typename T>
  constexpr const_poly_ptr(const T* ptr) noexcept {
    poly_.value_ptr = ptr;
    // if ptr == nullptr no matter what stored in vtable_ptr and i dont want branching here
    poly_.vtable_ptr = addr_vtable_for<T, Methods...>;
  }
  // from pointer to Any
  // clang-format off
  template <any_x Any>
  requires(noexport::find_subset(type_list<Methods<interface_t>...>{}, typename Any::methods_list{}) != npos)
  constexpr const_poly_ptr(const Any* p) noexcept {
    // clang-format on
    if (p != nullptr) [[likely]] {
      poly_.vtable_ptr = subtable_ptr<Methods...>(p->vtable_ptr);
      poly_.value_ptr = p->value_ptr;
    }
  }
  // from non-const poly pointer
  constexpr const_poly_ptr(poly_ptr<Methods...> p) noexcept {
    poly_.value_ptr = p.poly_.value_ptr;
    poly_.vtable_ptr = p.poly_.vtable_ptr;
  }
  template <
      TTA... FromMethods,
      typename = std::void_t<decltype(subtable_ptr<Methods...>(std::declval<vtable<FromMethods...>*>()))>>
  constexpr const_poly_ptr(const_poly_ptr<FromMethods...> p) noexcept {
    poly_.value_ptr = p.poly_.value_ptr;
    poly_.vtable_ptr = subtable_ptr<Methods...>(p.poly_.vtable_ptr);
  }
  template <
      TTA... FromMethods,
      typename = std::void_t<decltype(subtable_ptr<Methods...>(std::declval<vtable<FromMethods...>*>()))>>
  constexpr const_poly_ptr(poly_ptr<FromMethods...> p) noexcept
      : const_poly_ptr(const_poly_ptr<FromMethods...>{p}) {
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

  constexpr const_poly_ref<Methods...> operator*() const noexcept {
    return poly_;
  }
  constexpr const const_poly_ref<Methods...>* operator->() const noexcept {
    return std::addressof(poly_);
  }

  // compare

  constexpr bool operator==(const const_poly_ptr& other) const noexcept {
    return raw() == other.raw() && poly_.vtable_ptr == other.poly_.vtable_ptr;
  }

  // returns descriptor for void if *this == nullptr
  descriptor_t type_descriptor() const noexcept {
    return has_value() ? poly_.type_descriptor() : descriptor_t{};
  }
};

template<TTA... Methods>
const_poly_ptr(poly_ptr<Methods...>) -> const_poly_ptr<Methods...>;

template <TTA... Methods>
constexpr auto poly_ref<Methods...>::operator&() const noexcept {
  poly_ptr<Methods...> result;
  result.poly_.vtable_ptr = vtable_ptr;
  result.poly_.value_ptr = value_ptr;
  return result;
}
template <TTA... Methods>
constexpr auto const_poly_ref<Methods...>::operator&() const noexcept {
  const_poly_ptr<Methods...> result;
  result.poly_.vtable_ptr = vtable_ptr;
  result.poly_.value_ptr = value_ptr;
  return result;
}

template <TTA Method, typename = args_list<Method>>
struct invoke_unsafe_fn;

// traits describes invoke_match_t which types are polymorphic and which not
// T::get_type_descriptor return descriptor for static type of non-polymorphic types and dynamic type descripor for polymorphic types
// T::to_address returns void*/const void* to of runtime value for polymorphic types and just addressof(v) for non-polymorphic 'v'
template<typename T>
concept poly_traits = requires(T val, int some_val) {
                        { val.get_type_descriptor(some_val) } -> std::same_as<descriptor_t>;
                        { val.to_address(some_val) };
                      };

// these traits poly_ptr/ref/any_with are polymorphic values with dynamic type
// all other types considered as non-polymorphic
struct anyany_poly_traits {
 private:
  template <typename T>
  static constexpr bool is_polymorphic = requires(T v) {
                                           { v.type_descriptor() } -> std::same_as<descriptor_t>;
                                         };

 public:
  // default case for non-polymorphic types like 'int', 'string' etc
  template <typename T>
  static constexpr descriptor_t get_type_descriptor(T&&) noexcept {
    return descriptor_v<T>;
  }
  template <typename T>
    requires(is_polymorphic<T>)  // case for /const/ poly_ptr/ref and any_with and its inheritors
  static descriptor_t get_type_descriptor(T&& x) noexcept {
    return x.type_descriptor();
  }
  // non-polymorphic types, returns /const/ void*
  template <typename T>
  static constexpr auto* to_address(T&& v) noexcept {
    return reinterpret_cast<
        std::conditional_t<std::is_const_v<std::remove_reference_t<T>>, const void*, void*>>(
        std::addressof(v));
  }
  template <typename T>
    requires(is_polymorphic<T>)
  static auto* to_address(T&& v) noexcept {
    // for /const/poly_ptr
    if constexpr (requires { v.raw(); })
      return v.raw();
    else  // poly_ref/any_with and its inheritors
      return (&v).raw();
  }
};

// those traits may be used for visit many variants without O(n*m*...) instantiating.
// Its very usefull for pattern matching, but may be slower then matching with visit.
// All variants are polymorphic types for there traits, all other types considered as non-polymorphic
struct std_variant_poly_traits {
  template <typename... Ts>
  static descriptor_t get_type_descriptor(const std::variant<Ts...>& v) noexcept {
    return std::visit([]<typename T>(T&& v) { return descriptor_v<T>; }, v);
  }
  template <typename... Ts>
  static descriptor_t get_type_descriptor(std::variant<Ts...>& v) noexcept {
    return std::visit([]<typename T>(T&& v) { return descriptor_v<T>; }, v);
  }
  template <typename... Ts>
  static descriptor_t get_type_descriptor(std::variant<Ts...>&& v) noexcept {
    return std::visit([]<typename T>(T&& v) { return descriptor_v<T>; }, v);
  }
  template <typename T>
  static descriptor_t get_type_descriptor(T&&) noexcept {
    return descriptor_v<T>;
  }
  template <typename T>
  static auto* to_address(T&& v) noexcept {
    return reinterpret_cast<
        std::conditional_t<std::is_const_v<std::remove_reference_t<T>>, const void*, void*>>(
        std::addressof(v));
  }
  // Do not support cases like variant<int, const int> with mixed constness
  template <typename... Ts>
  static const void* to_address(const std::variant<Ts...>& v) noexcept {
    return std::visit([]<typename T>(const T& v) { return reinterpret_cast<const void*>(std::addressof(v)); },
                      v);
  }
  template <typename... Ts>
  static void* to_address(std::variant<Ts...>& v) noexcept {
    return std::visit([]<typename T>(T& v) { return reinterpret_cast<void*>(std::addressof(v)); }, v);
  }
  template <typename... Ts>
  static void* to_address(std::variant<Ts...>&& v) noexcept {
    return std::visit([]<typename T>(T&& v) { return reinterpret_cast<void*>(std::addressof(v)); }, v);
  }
};

// -- Result -- .resolve method will return std::optional<Result> by converting Foos results to this type 
// -- Traits -- is a customization point, it describes what types invoke_match_t sees as polymorphic.
// example: you can add Traits for some your virtual/LLVM-type-id hierarchy
// (see anyany_poly_traits or std_variant_poly_traits as examples)
template<typename Result, poly_traits Traits, lambda_without_capture... Foos>
struct invoke_match_t {
 private:
  template <typename F>
  using traits = ::noexport::any_method_traits<F>;

  static constexpr std::size_t overload_count = sizeof...(Foos);
  static constexpr std::size_t args_count = std::max({traits<Foos>::args_count...});
  using result_type = Result;
  using key_type = std::array<descriptor_t, args_count>;
  using value_type = result_type (*)(const std::array<void*, args_count>&);

  // map of overloads sorted by type descriptors of decayed types in signature
  ::noexport::flat_map<key_type, value_type, overload_count> map;
  [[no_unique_address]] Traits poly_traits;

  static_assert((... && std::is_convertible_v<typename traits<Foos>::result_type, result_type>),
                "invoke_match overloads result types must be convertible to result type");

  // verifies that atleast one of Foos may accept input args based on const arg or not
  template <typename... Voids>
  static consteval bool verify_const_correctness(Voids*...) noexcept {
#if defined(_MSC_VER) && !defined(__clang__)
    // really do not want support msvc, its full of bugs, cant compile
    return true;
#else
    std::array<bool, args_count> bitset;
    bitset.fill(false);
    std::ranges::copy(std::initializer_list<bool>{std::is_const_v<Voids>...}, bitset.begin());
    std::array<std::array<bool, args_count>, overload_count> all_foos_bitsets;
    for (auto& arr : all_foos_bitsets)
      arr.fill(true);
    std::apply([](auto&... arrs) { (..., std::ranges::copy(traits<Foos>::args_const, arrs.begin())); },
               all_foos_bitsets);
    auto may_accept_args = [&](auto& foo_args) {
      auto b = bitset.begin();
      for (bool may_accept_const : foo_args) {
        if (!may_accept_const && *b)
          return false;
        ++b;
      }
      return true;
    };
    return std::ranges::any_of(all_foos_bitsets, may_accept_args);
#endif
  }
  template<typename... Args>
  static bool runtime_const_correctness_check(auto* overload_winner) {
    auto make_value_constness_array_pair = []<typename F, typename... Ts>(std::type_identity<F>,
                                                                          type_list<Ts...>) {
      std::array<bool, args_count> value;
      value.fill(true);
      std::ranges::copy(traits<F>::args_const, value.begin());
      return std::pair(&match_invoker<F, Ts...>, value);
    };
    std::array m{
        make_value_constness_array_pair(std::type_identity<Foos>{}, typename traits<Foos>::all_args{})...};
    auto cit = std::ranges::find(m, overload_winner, [](auto& pair) { return pair.first; });
    auto& [_, can_accept_const] = *cit;
    constexpr std::array is_const_input_arg{
        std::is_const_v<std::remove_pointer_t<decltype(poly_traits.to_address(std::declval<Args>()))>>...};
    for (std::size_t i = 0; i < sizeof...(Args); ++i)
      if (is_const_input_arg[i] && !can_accept_const[i])
        return false;
    return true;
  }

  // type erases Foo signature, accepts void* array and invokes real Foo with them
  template <typename Foo, typename... Ts>
  static result_type match_invoker(const std::array<void*, args_count>& args) {
    std::array<void*, sizeof...(Ts)> necessary_args;
    std::ranges::copy_n(args.begin(), sizeof...(Ts), necessary_args.begin());
    return std::apply(
        [](auto*... void_ptrs) {
          return Foo{}(static_cast<Ts&&>(*reinterpret_cast<std::add_pointer_t<Ts>>(void_ptrs))...);
        },
        necessary_args);
  }

  template <typename F>
  static constexpr std::pair<key_type, value_type> make_key_value_pair_for() noexcept {
    return []<typename... Ts>(aa::type_list<Ts...>) {
      return std::pair{key_type{descriptor_v<Ts>...}, &match_invoker<F, Ts...>};
    }(typename traits<F>::all_args{});  // INVOKED HERE
  }
 public:
  AA_CONSTEVAL explicit invoke_match_t(Traits t = Traits{}) noexcept
      : map({make_key_value_pair_for<Foos>()...}), poly_traits(std::move(t)) {
    struct signatures_must_be_unique_after_decay : traits<Foos>::decay_args... {
    } _;
    (void)_;
  }

  // returns nullopt if no such function in overload/match set
  // or winner overload cant accept args due const-qualifiers(when Unsafe == false)
  // If Unsafe == false, then const qualifiers on overload-winner function and input args will be checked,
  // otherwise its UB to pass const-qualified arg into non-const qualified function
  template <bool Unsafe = true, typename... Args>
    requires(sizeof...(Args) <= args_count)
  std::optional<result_type> resolve(Args&&... args) const noexcept((traits<Foos>::is_noexcept && ...)) {
    static_assert(((traits<Foos>::args_count == sizeof...(Args)) || ...),
                  "No overload with this count of arguments");
    static_assert(verify_const_correctness(decltype(poly_traits.to_address(args)){}...),
                  "No overload which can accept those arguments, you are trying "
                  "to pass const argument into non-const qualified reference");
    key_type key{poly_traits.get_type_descriptor(args)...};
    std::fill(key.begin() + sizeof...(Args), key.end(), descriptor_v<void>);
    auto it = map.find(key);
    if (it == map.end()) [[unlikely]]
      return std::nullopt;
    if constexpr (Unsafe) {
      assert(runtime_const_correctness_check<Args...>(*it) &&
             "Trying to pass const input arg into non-const function arg");
    } else {
      if (!runtime_const_correctness_check<Args...>(*it))
        return std::nullopt;
    }
    return (*it)({const_cast<void*>(poly_traits.to_address(args))...});
  }
};

// invoke_match its like runtime overload resolution for Foos with converting result to Result
// Each in Foos may be function pointer or lambda without capture(with NON overloaded operator())
template<typename Result, auto... Foos>
AA_CONSTEXPR inline auto make_invoke_match() {
  return invoke_match_t<Result, anyany_poly_traits, typename ::noexport::make_wrapper<Foos>::type...>{};
}
template <typename Result, poly_traits Traits, auto... Foos>
AA_CONSTEXPR inline auto make_invoke_match() {
  return invoke_match_t<Result, Traits, typename ::noexport::make_wrapper<Foos>::type...>{};
}

template <TTA Method, typename... Args>
struct invoke_unsafe_fn<Method, type_list<Args...>> {
  // FOR ANY

  template <any_x U>
  result_t<Method> operator()(U&& any, Args... args) const {
    assert(any.vtable_ptr != nullptr);
    return any.vtable_ptr->template invoke<Method>(any.value_ptr, static_cast<Args&&>(args)...);
  }
  // clang-format off
  template <any_x U>
  result_t<Method> operator()(const U& any, Args... args) const {
    // clang-format on
    static_assert(const_method<Method>);
    assert(any.vtable_ptr != nullptr);
    return any.vtable_ptr->template invoke<Method>(any.value_ptr, static_cast<Args&&>(args)...);
  }

  // FOR POLYMORPHIC REF

  template <TTA... Methods>
  result_t<Method> operator()(poly_ref<Methods...> p, Args... args) const {
    return p.vtable_ptr->template invoke<Method>(p.value_ptr, static_cast<Args&&>(args)...);
  }
  template <TTA... Methods>
  result_t<Method> operator()(const_poly_ref<Methods...> p, Args... args) const {
    static_assert(const_method<Method>);
    return p.vtable_ptr->template invoke<Method>(p.value_ptr, static_cast<Args&&>(args)...);
  }
  // FOR NON POLYMORPHIC VALUE (common interface with all types)
  // clang-format off
  template<typename T>
  requires (!any_x<T>)
  result_t<Method> operator()(T&& value, Args... args) const {
    // clang-format on
    return Method<std::decay_t<T>>::do_invoke(std::forward<T>(value), static_cast<Args&&>(args)...);
  }
  // binds arguments and returns invocable<result_t<Method>(auto&&)> for passing to algorithms
  // (result can create useless copies of args, because assumes to be invoked more then one time)
  constexpr auto with(Args... args) const {
    return [tpl = std::make_tuple(static_cast<Args&&>(args)...)](auto&& self) mutable -> result_t<Method> {
      return std::apply(
          [&](Args&... args_) mutable
          -> result_t<Method> {  // cast to Args, so its move only if Args is rvalue reference
            return aa::invoke_unsafe_fn<Method>{}(std::forward<decltype(self)>(self),
                                                  static_cast<Args>(args_)...);
          },
          tpl);
    };
  }
};

// for cases, when you sure any has value (so UB if !has_value), compilers bad at optimizations(
template <TTA Method>
constexpr inline invoke_unsafe_fn<Method> invoke_unsafe = {};

// when used in ctor this tag forces anyany allocate memory, so pointers to value(poly_ptr/ref)
// will not invalidated after anyany move
struct force_stable_pointers_t {
  explicit force_stable_pointers_t() = default;
};
constexpr inline force_stable_pointers_t force_stable_pointers{};

// SooS == Small Object Optimization Size
// strong exception guarantee for all constructors and assignments,
// emplace<T> - *this is empty if exception thrown
// for alloc not all fancy pointers supported and construct / destroy not throught alloc
template <typename Alloc, size_t SooS, TTA... Methods>
struct AA_MSVC_EBO basic_any : plugin_t<Methods, basic_any<Alloc, SooS, Methods...>>... {
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

  // clang-format on
  template <typename T, typename ForceAllocate = void, typename... Args>
  void emplace_in_empty(Args&&... args) {
    if constexpr (any_is_small_for<T> && std::is_void_v<ForceAllocate>) {
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
      std::construct_at(reinterpret_cast<std::size_t*>(data.data()), allocation_size);
    }
    vtable_ptr = addr_vtable_for<T, Methods...>;
  }

  using alloc_traits = std::allocator_traits<Alloc>;
  using alloc_pointer_type = typename alloc_traits::pointer;
  using alloc_size_type = typename alloc_traits::size_type;

  template <TTA...>
  friend struct poly_ptr;
  template<TTA...>
  friend struct const_poly_ptr;
  template <TTA, typename>
  friend struct invoke_fn;
  template <TTA, typename>
  friend struct invoke_unsafe_fn;

 public:
  template <TTA Method>
  static constexpr bool has_method = vtable<Methods...>::template has_method<Method>;
  static constexpr bool has_copy = has_method<copy_with<Alloc, SooS>::template method>;

  static_assert((std::is_empty_v<plugin_t<Methods, basic_any<Alloc, SooS, Methods...>>> && ...));
  static_assert((noexport::check_copy<Methods<interface_t>, Alloc, SooS>() && ...),
                "Alloc and SooS in copy do not match Alloc in SooS in basic_any, "
                "use aa::copy_with<Alloc, SooS>::tempalte method!");
  static_assert(
      noexport::is_one_of<typename alloc_traits::value_type, std::byte, char, unsigned char>::value);
  static_assert(has_method<destroy>, "Any requires aa::destroy method");
  static_assert(std::is_nothrow_copy_constructible_v<Alloc>, "C++ Standard requires it");

  using base_any_type = basic_any;
  // typedef only because msvc parser is ****
  typedef type_list<Methods<interface_t>...> methods_list;

  private:
  template <TTA...>
   struct remove_utility_methods {};

  template <TTA... Methods1>
  struct remove_utility_methods<destroy, Methods1...> {
    using ptr = poly_ptr<Methods1...>;
    using const_ptr = const_poly_ptr<Methods1...>;
    using ref = poly_ref<Methods1...>;
    using const_ref = const_poly_ref<Methods1...>;
  };

  using purified = remove_utility_methods<Methods...>;
 public:
  using ptr = typename purified::ptr;
  using const_ptr = typename purified::const_ptr;
  using ref = typename purified::ref;
  using const_ref = typename purified::const_ref;

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
        value_ptr = invoke_unsafe<copy_with<Alloc, SooS>::template method>(other, value_ptr);
      else
        value_ptr = invoke_unsafe<copy_with<Alloc, SooS>::template method>(other, value_ptr, alloc);
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
  basic_any& operator=(basic_any&& other) noexcept requires(has_method<move>) {
    // nocheck about this == &other
    // because after move assign other by C++ standard in unspecified(valid) state
    reset();
    if constexpr (!alloc_traits::is_always_equal::value &&
                  alloc_traits::propagate_on_container_move_assignment::value) {
      if (alloc != other.alloc)
        alloc = std::move(other.alloc);
    }
    move_value_from(std::move(other));
    return *this;
  }

  basic_any& operator=(const basic_any& other) requires(has_copy&& has_method<move>) {
    basic_any value{other};
    if constexpr (!alloc_traits::is_always_equal::value &&
                  alloc_traits::propagate_on_container_copy_assignment::value) {
      if (alloc != other.alloc) {
        reset();  // my alloc will be destroyed so i need to deallocate(while i can)
        alloc = other.alloc;
      }
    }
    *this = std::move(value);
    return *this;
  }
  template<typename V>
    requires(!any_x<V>)
  basic_any& operator=(V&& val) {
    basic_any temp{std::forward<V>(val)};
    std::destroy_at(this);
    std::construct_at(this, std::move(temp));
    return *this;
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

  // force allocate versions

  template <typename T, typename... Args>
  basic_any(force_stable_pointers_t, std::in_place_type_t<T>, Args&&... args) noexcept(
      std::is_nothrow_constructible_v<std::decay_t<T>, Args&&...>&& any_is_small_for<std::decay_t<T>>) {
    emplace_in_empty<std::decay_t<T>, force_stable_pointers_t>(std::forward<Args>(args)...);
  }
  template <typename T, typename U, typename... Args>
  basic_any(force_stable_pointers_t, std::in_place_type_t<T>, std::initializer_list<U> list, Args&&... args) noexcept(
      std::is_nothrow_constructible_v<std::decay_t<T>, std::initializer_list<U>, Args&&...>&&
          any_is_small_for<std::decay_t<T>>) {
    emplace_in_empty<std::decay_t<T>, force_stable_pointers_t>(list, std::forward<Args>(args)...);
  }
  // clang-format off
  template <typename T>
  requires(!any_x<T>)
  basic_any(force_stable_pointers_t, T&& value) noexcept(
      std::is_nothrow_constructible_v<std::decay_t<T>, T&&>&& any_is_small_for<std::decay_t<T>>)
      : basic_any(force_stable_pointers, std::in_place_type<std::decay_t<T>>, std::forward<T>(value)) {
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

  // returns descriptor for 'void' type if do not contain type
  descriptor_t type_descriptor() const noexcept {
      return has_value() ? get_type_descriptor(vtable_ptr) : descriptor_t{};
  }
  // returns true if poly_ptr/ref to this basic_any will not be invalidated after move
  bool is_stable_pointers() const noexcept {
    return memory_allocated();
  }
  constexpr bool has_value() const noexcept {
    return vtable_ptr != nullptr;
  }
  // COMPARE

  // there are NO operator==(auto&&) or operator<=>(auto&&)
  // It can be more effective on runtime(if types not equal, then return false / unordered)
  // but:
  // * it will be less effective(useless branching) if you 90% sure its this type
  // * it will cause compilation time, obj file increasing

  [[nodiscard]] bool operator==(const basic_any& other) const
    requires(has_method<equal_to> || has_method<spaceship>)
  {
    auto desc = type_descriptor();
    if (desc != other.type_descriptor())
      return false;
    if (desc == descriptor_v<void>) [[unlikely]]
      return true;
    if constexpr (has_method<equal_to>)
      return invoke_unsafe<equal_to>(*this, other.value_ptr);
    else
      return invoke_unsafe<spaceship>(*this, other.value_ptr) == std::partial_ordering::equivalent;
  }

  std::partial_ordering operator<=>(const basic_any& other) const
    requires(has_method<spaceship>)
  {
    auto desc = type_descriptor();
    if (desc != other.type_descriptor())
      return std::partial_ordering::unordered;
    if (desc == descriptor_v<void>) [[unlikely]]
      return std::partial_ordering::equivalent;
    return invoke_unsafe<spaceship>(*this, other.value_ptr);
  }

 private:

  // precodition - has_value() == false
  void move_value_from(basic_any&& other) {
    if (!other.has_value())
      return;
    // `move` is noexcept (invariant of small state)
    // `move` also 'relocate' i.e. calls dctor of value(for remove invoke<destroy> in future)
    if (!other.memory_allocated())
      invoke_unsafe<move>(other, value_ptr);
    else {
      value_ptr = std::exchange(other.value_ptr, &other.data);
      std::construct_at(reinterpret_cast<std::size_t*>(data.data()),
                        *reinterpret_cast<std::size_t*>(other.data.data()));
    }
    vtable_ptr = std::exchange(other.vtable_ptr, nullptr);
  }

  bool memory_allocated() const {
    return value_ptr != &data;
  }
  size_t allocated_size() const {
    assert(has_value() && memory_allocated());
    // needs atleast sizeof(std::size_t) in buffer(SooS)
    // to store allocated size for passing it into deallocate(ptr, n)
    static_assert(SooS >= sizeof(std::size_t));
    // when allocates stores in size in unused buffer
    return *reinterpret_cast<const std::size_t*>(data.data());
  }
  void destroy_value() {
    invoke_unsafe<destroy>(*this);
    if (memory_allocated()) {
      alloc_traits::deallocate(alloc, reinterpret_cast<alloc_pointer_type>(value_ptr), allocated_size());
      value_ptr = &data;
    }
  }
};

// TEMPLATE FUNCTIONAL OBJECT any_cast
// any_cast<T>(any | any*) -> std::remove_cv_t<T> | T*
// any_cast<T&>(const|poly_ref) -> const|T&
// any_cast<T*>(const|poly_ptr) -> const|T*

template<typename T>
struct any_cast_fn {
 private:
  template <typename U, typename Alloc, size_t SooS, TTA... Methods>
  static const U* any_cast_impl(const basic_any<Alloc, SooS, Methods...>* any) noexcept {
    // U already remove_cv
    if (any == nullptr || any->type_descriptor() != descriptor_v<U>)
      return nullptr;
    return std::launder(reinterpret_cast<const U*>((&*any).raw()));
  }
  template <typename U, typename Alloc, size_t SooS, TTA... Methods>
  static U* any_cast_impl(basic_any<Alloc, SooS, Methods...>* any) noexcept {
    if (any == nullptr || any->type_descriptor() != descriptor_v<U>)
      return nullptr;
    return std::launder(reinterpret_cast<U*>((&*any).raw()));
  }
 public:
  static_assert(!(std::is_array_v<T> || std::is_function_v<T> || std::is_void_v<T>),
                "Incorrect call, it will be always nullptr");
  template <any_x U>
  auto* operator()(U* ptr) const noexcept {
    return any_cast_impl<std::remove_cvref_t<T>>(static_cast<typename U::base_any_type*>(ptr));
  }
  template <any_x U>
  const auto* operator()(const U* ptr) const noexcept {
    return any_cast_impl<std::remove_cvref_t<T>>(static_cast<const typename U::base_any_type*>(ptr));
  }

  template <any_x U>
  std::remove_cv_t<T> operator()(U& any) const {
    auto* ptr = any_cast_fn<std::remove_cvref_t<T>>{}(std::addressof(any));
    if (!ptr)
      throw std::bad_cast{};
    return *ptr;
  }
  template <any_x U>
  std::remove_cv_t<T> operator()(U&& any) const {
    auto* ptr = any_cast_fn<std::remove_cvref_t<T>>{}(std::addressof(any));
    if (!ptr)
      throw std::bad_cast{};
    return std::move(*ptr);
  }
  template <any_x U>
  std::remove_cv_t<T> operator()(const U& any) const {
    const auto* ptr = any_cast_fn<std::remove_cvref_t<T>>{}(std::addressof(any));
    if (!ptr)
      throw std::bad_cast{};
    return *ptr;
  }
  template <TTA... Methods>
  std::add_pointer_t<const T> operator()(const_poly_ptr<Methods...> p) const noexcept {
    if (p == nullptr || p.type_descriptor() != descriptor_v<T>)
      return nullptr;
    return reinterpret_cast<const std::remove_reference_t<T>*>(p.raw());
  }
  template <TTA... Methods>
  auto* operator()(poly_ptr<Methods...> p) const noexcept {
    const_poly_ptr pp = p;
    return const_cast<T*>(any_cast_fn<T>{}(pp));
  }
  template <TTA... Methods>
  std::remove_cv_t<T> operator()(poly_ref<Methods...> p) const {
    using U = std::remove_reference_t<T>;
    auto ptr = any_cast_fn<U>{}(&p);
    if (ptr == nullptr) [[unlikely]]
      throw std::bad_cast{};
    return *reinterpret_cast<std::remove_cvref_t<T>*>(const_cast<std::remove_const_t<U>*>(ptr));
  }
  template <TTA... Methods>
  std::conditional_t<std::is_reference_v<T>, const std::remove_reference_t<T>&, std::remove_cv_t<T>>
  operator()(const_poly_ref<Methods...> p) const {
    auto ptr = any_cast_fn<std::remove_reference_t<T>>{}(&p);
    if (ptr == nullptr) [[unlikely]]
      throw std::bad_cast{};
    return *reinterpret_cast<const std::remove_cvref_t<T>*>(ptr);
  }
};

template<typename T>
constexpr inline any_cast_fn<T> any_cast = {};

// TEMPLATE VARIABLE (FUNCTION OBJECT) invoke <Method> (any)

// hack for compilation time / obj size reduce + accept exactly user args
// (for example i can write invoke<Foo>(Any, {}, {1, 2, 3}) because compiler knows what types in must be

struct empty_any_method_call : std::exception {
  [[nodiscard]] const char* what() const noexcept override {
    return "Empty any method was called";
  }
};

template <TTA Method, typename = args_list<Method>>
struct invoke_fn;

template <TTA Method, typename... Args>
struct invoke_fn<Method, type_list<Args...>> {
  // FOR ANY

  template <any_x U>
  result_t<Method> operator()(U&& any, Args... args) const {
    if (!any.has_value()) [[unlikely]]
      throw empty_any_method_call{};
    return any.vtable_ptr->template invoke<Method>(any.value_ptr, static_cast<Args&&>(args)...);
  }
  template <any_x U>
  result_t<Method> operator()(const U& any, Args... args) const {
    static_assert(const_method<Method>);
    if (!any.has_value()) [[unlikely]]
      throw empty_any_method_call{};
    return any.vtable_ptr->template invoke<Method>(any.value_ptr, static_cast<Args&&>(args)...);
  }

  // FOR POLYMORPHIC REF

  template <TTA... Methods>
  result_t<Method> operator()(poly_ref<Methods...> p, Args... args) const {
    return p.vtable_ptr->template invoke<Method>(p.value_ptr, static_cast<Args&&>(args)...);
  }
  template <TTA... Methods>
  result_t<Method> operator()(const_poly_ref<Methods...> p, Args... args) const {
    static_assert(const_method<Method>);
    return p.vtable_ptr->template invoke<Method>(p.value_ptr, static_cast<Args&&>(args)...);
  }

  // FOR NON POLYMORPHIC VALUE (common interface with all types)
  // clang-format off
  template<typename T>
  requires (!any_x<T>)
  result_t<Method> operator()(T&& value, Args... args) const {
    // clang-format on
    return Method<std::decay_t<T>>::do_invoke(std::forward<T>(value), static_cast<Args&&>(args)...);
  }
  // binds arguments and returns invocable<result_t<Method>(auto&&)> for passing to algorithms
  // (result can create useless copies of args, because assumes to be invoked more then one time)
  constexpr auto with(Args... args) const {
    return [tpl = std::make_tuple(static_cast<Args&&>(args)...)](auto&& self) mutable -> result_t<Method> {
      return std::apply(
          [&](Args&... args_) mutable
          -> result_t<Method> {  // cast to Args, so its move only if Args is rvalue reference
            return aa::invoke_fn<Method>{}(std::forward<decltype(self)>(self), static_cast<Args>(args_)...);
          },
          tpl);
    };
  }
};

template <TTA Method>
constexpr inline invoke_fn<Method> invoke = {};

template <typename Alloc, size_t SooS, TTA... Methods>
using basic_any_with = basic_any<Alloc, SooS, destroy, Methods...>;

template<TTA... Methods>
using any_with = basic_any_with<std::allocator<std::byte>, default_any_soos, Methods...>;

template <typename PolyPtr, typename ResultT = void>
struct type_switch_impl {
 private:
  struct non_void {
    // no way to create it for user
    explicit non_void() = default;
  };
  using Result = std::conditional_t<std::is_void_v<ResultT>, non_void, ResultT>;

 public:
  constexpr explicit type_switch_impl(PolyPtr value) noexcept : value(std::move(value)) {
    assert(value != nullptr);
  }

  template <typename T, typename Fn>
  type_switch_impl& case_(Fn&& f) {
    if (result)
      return *this;
    if (auto* v = ::aa::any_cast<T>(value)) {
      if constexpr (std::is_void_v<ResultT>) {
        std::invoke(std::forward<Fn>(f), *v);
        result.emplace();
      } else {
        result = std::invoke(std::forward<Fn>(f), *v);
      }
    }
    return *this;
  }
  // If value is one of Ts... F invoked (invokes count <= 1)
  template <typename... Ts, typename Fn>
  type_switch_impl& cases(Fn&& f) {
    struct assert_ : std::type_identity<Ts>... {
    } assert_unique_types;
    (void)assert_unique_types;
    (case_<Ts>(std::forward<Fn>(f)), ...);
    return *this;
  }
  // As a default, invoke the given callable within the root value.
  template <typename Fn>
  [[nodiscard]] Result default_(Fn&& f) {
    if (result)
      return std::move(*result);
    return std::forward<Fn>(f)(*value);
  }
  // As a default, return the given value.
  [[nodiscard]] Result default_(Result v) {
    if (result)
      return std::move(*result);
    return v;
  }
  // explicitly says there are no default value
  // postcondition: return value setted if some 'case_' succeeded
  std::optional<Result> no_default() {
    return std::move(result);
  }

 private:
  // value for which switch created
  // invariant - initially always not null.
  PolyPtr value;
  // stored result and if it exist
  std::optional<Result> result = std::nullopt;
};

// Returns instance of type which
// implements a switch-like dispatch statement for poly_ptr/const_poly_ptr
// using any_cast.
// Each `Case<T>` takes a callable to be invoked
// if the root value is a <T>, the callable is invoked with any_cast<T>(ValueInSwitch)
//
// usage example:
//  any_operation<Methods...> op = ...;
//  ResultType result = type_switch<ResultType>(op)
//    .Case<ConstantOp>([](ConstantOp op) { ... })
//    .Default([](const_poly_ref<Methods...> ref) { ... });
template<typename Result = void, TTA... Methods>
constexpr auto type_switch(poly_ref<Methods...> p) noexcept {
  return type_switch_impl<poly_ptr<Methods...>, Result>{&p};
}
template <typename Result = void, TTA... Methods>
constexpr auto type_switch(const_poly_ref<Methods...> p) noexcept {
  return type_switch_impl<const_poly_ptr<Methods...>, Result>{&p};
}

}  // namespace aa

namespace std {

template <::aa::any_x T>
  requires(T::template has_method<::aa::hash>)
struct hash<T> {
  size_t operator()(const T& any) const noexcept {
    return any.has_value() ? aa::invoke_unsafe<::aa::hash>(any) : 0;
  }
};
template <TTA... Methods>
  requires(::aa::vtable<Methods...>::template has_method<::aa::hash>)
struct hash<::aa::poly_ref<Methods...>> {
  size_t operator()(const ::aa::poly_ref<Methods...>& r) const noexcept {
    return aa::invoke_unsafe<::aa::hash>(r);
  }
};
template <TTA... Methods>
  requires(::aa::vtable<Methods...>::template has_method<::aa::hash>)
struct hash<::aa::const_poly_ref<Methods...>> {
  size_t operator()(const ::aa::const_poly_ref<Methods...>& r) const noexcept {
    return aa::invoke_unsafe<::aa::hash>(r);
  }
};

template <TTA... Methods>
struct hash<::aa::poly_ptr<Methods...>> {
  size_t operator()(const ::aa::poly_ptr<Methods...>& p) const noexcept {
    return ::std::hash<void*>{}(p.raw());
  }
};
template <TTA... Methods>
struct hash<::aa::const_poly_ptr<Methods...>> {
  size_t operator()(const ::aa::const_poly_ptr<Methods...>& p) const noexcept {
    return ::std::hash<void*>{}(p.raw());
  }
};

template <>
struct hash<::aa::descriptor_t> {
  size_t operator()(const ::aa::descriptor_t& v) const noexcept {
#ifdef AA_CANT_GET_TYPENAME
    return hash<const void*>{}(bit_cast<const void*>(v));
#else
    return hash<string_view>{}(bit_cast<const char*>(v));
#endif
  }
};

}  // namespace std
