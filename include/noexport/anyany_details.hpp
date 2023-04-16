#pragma once

#include <type_traits>
#include <cstddef>
#include <array>
#ifdef AA_HAS_CPP20
#include <memory>      // construct_at / destroy_at
#endif
#include "type_descriptor.hpp"

// Yes, msvc do not support EBO which is already GUARANTEED by C++ standard for ~13 years
#if defined(_MSC_VER)
#define AA_MSVC_EBO __declspec(empty_bases)
#else
#define AA_MSVC_EBO
#endif

#if defined(__GNUC__) || defined(__clang__)
#define AA_ALWAYS_INLINE __attribute__((always_inline)) inline
#elif defined(_MSC_VER)
#define AA_ALWAYS_INLINE __forceinline
#else
#define AA_ALWAYS_INLINE inline
#endif

namespace aa {

template <typename...>
struct type_list {};

constexpr inline size_t npos = size_t(-1);

}  // namespace aa

namespace aa::noexport {

// this tuple exist only because i cant constexpr cast function pointer to void* for storing in vtable
template <typename T, size_t>
struct value_in_tuple {
  T value{};
};

template <typename...>
struct tuple_base {};

template <size_t... Is, typename... Ts>
struct AA_MSVC_EBO tuple_base<std::index_sequence<Is...>, Ts...> : value_in_tuple<Ts, Is>... {
  constexpr tuple_base() noexcept = default;
  constexpr tuple_base(Ts... args) noexcept : value_in_tuple<Ts, Is>{static_cast<Ts&&>(args)}... {
  }
};
template <>
struct tuple_base<std::index_sequence<>> {};

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
AA_CONSTEVAL_CPP20 bool is_const_method() noexcept {
  if (std::is_reference_v<Self>)
    return std::is_const_v<std::remove_reference_t<Self>>;
  return true;  // passing by value is a const method!
}

template <typename>
struct any_method_traits {};

// returns false if it is ill-formed to pass non-const reference into function which accepts T
template <typename T>
AA_CONSTEVAL_CPP20 bool is_const_arg() {
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
  // for visit_invoke
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
  // for visit_invoke
  static constexpr bool is_noexcept = true;
  using all_args = aa::type_list<Self, Args...>;
  using decay_args = aa::type_list<std::decay_t<Self>, std::decay_t<Args>...>;
  static constexpr std::size_t args_count = sizeof...(Args) + 1;
  static constexpr std::array args_const = {is_const_arg<Self>(), is_const_arg<Args>()...};
};
template <typename T>
AA_CONSTEVAL_CPP20 bool starts_with(aa::type_list<>, T) {
  return true;
}
// first type not equal
template <typename A, typename B>
AA_CONSTEVAL_CPP20 bool starts_with(A, B) {
  return false;
}
template <typename Head, typename... Ts1, typename... Ts2>
AA_CONSTEVAL_CPP20 bool starts_with(aa::type_list<Head, Ts1...>, aa::type_list<Head, Ts2...>) {
  return starts_with(aa::type_list<Ts1...>{}, aa::type_list<Ts2...>{});
}

AA_CONSTEVAL_CPP20 size_t find_subset(aa::type_list<>, aa::type_list<>) {
  return 0;
}
template <typename A>
AA_CONSTEVAL_CPP20 size_t find_subset(A, aa::type_list<>) {
  return npos;
}
// returns index in list where first typelist starts as subset in second typelist or npos if no such index
template <typename... Ts1, typename Head, typename... Ts2>
AA_CONSTEVAL_CPP20 size_t find_subset(aa::type_list<Ts1...> needle, aa::type_list<Head, Ts2...> all,
                                      size_t n = 0) noexcept {
  if constexpr (sizeof...(Ts1) >= sizeof...(Ts2) + 1)
    return std::is_same_v<aa::type_list<Ts1...>, aa::type_list<Head, Ts2...>> ? n : ::aa::npos;
  else if constexpr (starts_with(needle, all))
    return n;
  else
    return find_subset(needle, aa::type_list<Ts2...>{}, n + 1);
}

#ifndef AA_HAS_CPP20
template <typename T, typename... Args,
          typename = std::void_t<decltype(::new(std::declval<void*>()) T(std::declval<Args>()...))>>
constexpr T* construct_at(const T* location, Args&&... args) noexcept(noexcept(
    ::new(const_cast<void*>(static_cast<const volatile void*>(location))) T(std::forward<Args>(args)...))) {
  return ::new (const_cast<void*>(static_cast<const volatile void*>(location)))
      T(std::forward<Args>(args)...);
}
template <typename T>
void destroy_at(const T* location) noexcept {
  location->~T();
}

template <typename T>
using remove_cvref_t = std::remove_cv_t<std::remove_reference_t<T>>;
#else
using ::std::construct_at;
using ::std::destroy_at;
using ::std::remove_cvref_t;
#endif

// TODO? здесь прямо сделать && на аргументах + аттрибутты типа always inline?
#define trait_impl(CONST, NAME, REQUIREMENT, SIGNATURE, ... /*body*/)                                  \
  template <typename>                                                                                  \
  struct make_method_##NAME {};                                                                        \
                                                                                                       \
  template <typename AA_Ret, typename... AA_Args>                                                      \
  struct make_method_##NAME<AA_Ret(AA_Args...)> {                                                      \
    AA_IF_HAS_CPP20(template <typename Self>                                                           \
                    static constexpr bool requirement = requires(CONST Self & self, AA_Args... args) { \
                                                          static_cast<AA_Ret>(__VA_ARGS__);            \
                                                        };)                                            \
    template <typename Self>                                                                           \
    AA_IF_HAS_CPP20(requires(::std::is_same_v<Self, ::aa::interface_t> || requirement<Self>))          \
    struct aa_method {                                                                                 \
      static AA_Ret do_invoke(CONST Self& self, AA_Args... args) {                                     \
        return static_cast<AA_Ret>(__VA_ARGS__);                                                       \
      }                                                                                                \
      template <typename AA_CRTP>                                                                      \
      struct plugin {                                                                                  \
        AA_Ret NAME(AA_Args... args) CONST {                                                           \
          return static_cast<AA_Ret>(                                                                  \
              ::aa::invoke<make_method_##NAME<AA_Ret(AA_Args...)>::template aa_method>(                \
                  *static_cast<CONST AA_CRTP*>(this), static_cast<AA_Args&&>(args)...));               \
        }                                                                                              \
      };                                                                                               \
    };                                                                                                 \
  };                                                                                                   \
  template <typename Self>                                                                             \
  AA_IF_HAS_CPP20(REQUIREMENT)                                                                         \
  using NAME = typename make_method_##NAME<SIGNATURE>::template aa_method<Self>
}  // namespace aa::noexport
