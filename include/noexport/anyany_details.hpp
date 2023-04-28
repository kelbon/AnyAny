#pragma once

#include <cstddef>

#include "type_descriptor.hpp"
#include "anyany_macro.hpp"

namespace aa::noexport {

// this tuple exist only because i cant constexpr cast function pointer to void* for storing in vtable
template <typename T, size_t>
struct value_in_tuple {
#if __clang__
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wunknown-attributes"
#endif
  [[no_unique_address]] T value{};
#if __clang__
#pragma clang diagnostic pop
#endif
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
constexpr U& get_value(value_in_tuple<U, I>& v) noexcept {
  return v.value;
}
template <size_t I, typename U>
constexpr const U& get_value(const value_in_tuple<U, I>& v) noexcept {
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
constexpr inline size_t number_of_first = number_of_impl<0, T, Args...>::value;

template<typename T, typename... Ts>
constexpr inline size_t contain = number_of_first<T, Ts...> != npos;

template <typename T, typename... Args>
struct is_one_of : std::bool_constant<(std::is_same_v<T, Args> || ...)> {};

template <typename>
constexpr inline bool always_false = false;

struct aa_pseudomethod_tag {};

template <typename T>
struct any_method_traits {
  // pseudomethod signature(not a function, just type)
  using self_sample_type = void;  // dont need
  using result_type = T;
  static constexpr bool is_const = true;
  using type_erased_self_type = void;    // dont need
  using type_erased_signature_type = T;  // what vtable will store
  using args = aa_pseudomethod_tag;      // specialize for invoke_fn
};

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

// returns index in list where first typelist starts as subset in second typelist or npos if
// no such index
template <typename... Ts1, typename Head, typename... Ts2>
AA_CONSTEVAL_CPP20 size_t find_subset_impl(aa::type_list<Ts1...> needle, aa::type_list<Head, Ts2...> haystack,
                                           size_t n) noexcept {
  if constexpr (sizeof...(Ts1) >= sizeof...(Ts2) + 1)
    return std::is_same_v<aa::type_list<Ts1...>, aa::type_list<Head, Ts2...>> ? n : ::aa::npos;
  else if constexpr (starts_with(needle, haystack))
    return n;
  else
    return find_subset_impl(needle, aa::type_list<Ts2...>{}, n + 1);
}
// MSVC WORKAROUND! this wrapper just fixes MSVC bug with overload resolution and aliases
// (this is why it does not accepts type_lists)
template <typename Needle, typename Haystack>
AA_CONSTEVAL_CPP20 size_t find_subset(Needle needle, Haystack haystack) noexcept {
  if constexpr (std::is_same_v<Haystack, type_list<>>)
    return std::is_same_v<Needle, type_list<>> ? 0 : npos;
  else
    return find_subset_impl(needle, haystack, 0);
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

template <typename... Ts>
struct AA_MSVC_EBO inheritor_of : Ts... {};

template <typename... Results>
auto inherit_without_duplicates(type_list<>, type_list<Results...>) -> inheritor_of<Results...>;

template <typename Head, typename... Tail, typename... Results>
auto inherit_without_duplicates(type_list<Head, Tail...>, type_list<Results...> l) {
  if constexpr (std::is_same_v<void, Head>)
    return inherit_without_duplicates(type_list<Tail...>{}, l);
  else if constexpr ((std::is_base_of_v<Head, Results> || ...) || (std::is_base_of_v<Head, Tail> || ...))
    return inherit_without_duplicates(type_list<Tail...>{}, l);
  else
    return inherit_without_duplicates(type_list<Tail...>{}, type_list<Results..., Head>{});
}

template <typename... Ts>
using inheritor_without_duplicates_t = decltype(inherit_without_duplicates(type_list<Ts...>{}, type_list<>{}));

template <typename Any, typename Method>
auto get_plugin(int) -> typename Method::template plugin<Any>;
#if __cpp_explicit_this_parameter >= 202110L
template<typename Any, typename Method>
auto get_plugin(bool) -> typename Method::plugin;
#endif
template <typename, typename>
auto get_plugin(...) -> void;

template <typename Method>
auto get_method_signature_ptr(int) -> typename Method::signature_type*;
template <typename Method>
auto get_method_signature_ptr(bool) -> decltype(&Method::template do_invoke<erased_self_t>);
template <typename Method>
auto get_method_signature_ptr(...) -> typename Method::value_type;

AA_IS_VALID(has_has_value, decltype(std::declval<T>().has_value()));

template <typename T, size_t SooS>
constexpr inline bool is_fits_in_soo_buffer =
    alignof(std::decay_t<T>) <= alignof(std::max_align_t) &&
    std::is_nothrow_move_constructible_v<std::decay_t<T>> && sizeof(std::decay_t<T>) <= SooS;

}  // namespace aa::noexport