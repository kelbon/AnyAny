#pragma once

#include <cstddef>

#include "type_descriptor.hpp"

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

template <typename>
struct any_method_traits {};

// Note: for signature && added to arguments for forwarding
template <typename R, typename Self, typename... Args>
struct any_method_traits<R (*)(Self, Args...)> {
  using self_sample_type = Self;
  using result_type = R;
  static constexpr bool is_const =
      !std::is_reference_v<Self> || std::is_const_v<std::remove_reference_t<Self>>;
  using type_erased_self_type = std::conditional_t<is_const, const void*, void*>;
  using type_erased_signature_type = R (*)(type_erased_self_type, Args&&...);
  using args = aa::type_list<Args...>;
};
// noexcept version
template <typename R, typename Self, typename... Args>
struct any_method_traits<R (*)(Self, Args...) noexcept> {
  using self_sample_type = Self;
  using result_type = R;
  static constexpr bool is_const =
      !std::is_reference_v<Self> || std::is_const_v<std::remove_reference_t<Self>>;
  using type_erased_self_type = std::conditional_t<is_const, const void*, void*>;
  using type_erased_signature_type = R (*)(type_erased_self_type, Args&&...);
  using args = aa::type_list<Args...>;
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

AA_CONSTEVAL_CPP20 size_t find_subset_impl(aa::type_list<>, aa::type_list<>) {
  return 0;
}
template <typename A>
AA_CONSTEVAL_CPP20 size_t find_subset_impl(A, aa::type_list<>) {
  return npos;
}
// returns index in list where first typelist starts as subset in second typelist or npos if
// no such index
template <typename... Ts1, typename Head, typename... Ts2>
AA_CONSTEVAL_CPP20 size_t find_subset_impl(aa::type_list<Ts1...> needle, aa::type_list<Head, Ts2...> all,
                                           size_t n = 0) noexcept {
  if constexpr (sizeof...(Ts1) >= sizeof...(Ts2) + 1)
    return std::is_same_v<aa::type_list<Ts1...>, aa::type_list<Head, Ts2...>> ? n : ::aa::npos;
  else if constexpr (starts_with(needle, all))
    return n;
  else
    return find_subset_impl(needle, aa::type_list<Ts2...>{}, n + 1);
}
// MSVC WORKAROUND! this wrapper just fixes MSVC bug with overload resolution and aliases
template <typename T, typename U>
AA_CONSTEVAL_CPP20 size_t find_subset(T a, U b) noexcept {
  return find_subset_impl(a, b);
}

template <typename T, typename... Args,
          typename = std::void_t<decltype(::new(std::declval<void*>()) T(std::declval<Args>()...))>>
constexpr T* construct_at(const T* location, Args&&... args) noexcept(noexcept(
    ::new(const_cast<void*>(static_cast<const volatile void*>(location))) T(std::forward<Args>(args)...))) {
  return ::new (const_cast<void*>(static_cast<const volatile void*>(location)))
      T(std::forward<Args>(args)...);
}
template <typename T>
constexpr void destroy_at(const T* location) noexcept {
  location->~T();
}

template <typename T>
using remove_cvref_t = std::remove_cv_t<std::remove_reference_t<T>>;

}  // namespace aa::noexport