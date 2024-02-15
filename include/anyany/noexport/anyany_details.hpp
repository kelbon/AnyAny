#pragma once

#include <cstddef>
#include <type_traits>
#include <utility>  // forward
#include <cstring>  // memcpy

#include "common.hpp"

#include "file_begin.hpp"

namespace aa {

constexpr inline size_t npos = size_t(-1);

template <typename T>
struct is_type_list : std::false_type {};

template <typename... Types>
struct is_type_list<type_list<Types...>> : std::true_type {};

}  // namespace aa

namespace aa::noexport {

template <typename...>
constexpr inline bool always_false = false;

// this tuple exist only because i cant constexpr cast function pointer to void* for storing in vtable
template <typename T, size_t>
struct value_in_tuple {
  T value{};
};

template <typename...>
struct tuple_base {};

template <size_t... Is, typename... Ts>
struct tuple_base<std::index_sequence<Is...>, Ts...> : value_in_tuple<Ts, Is>... {
  constexpr tuple_base() = default;
  constexpr tuple_base(Ts... args) : value_in_tuple<Ts, Is>{static_cast<Ts&&>(args)}... {
  }
  template <typename... Types>  // for supporting non movable types too
  constexpr tuple_base(Types&&... args) : value_in_tuple<Ts, Is>{std::forward<Types>(args)}... {
  }
};
template <>
struct tuple_base<std::index_sequence<>> {};

// stores values always in right order(in memory)
template <typename... Ts>
struct tuple : tuple_base<std::index_sequence_for<Ts...>, Ts...> {
  using tuple::tuple_base::tuple_base;
};

template <size_t I, typename U>
constexpr U& get(value_in_tuple<U, I>& v) noexcept {
  return v.value;
}
template <size_t I, typename U>
constexpr const U& get(const value_in_tuple<U, I>& v) noexcept {
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

template <typename T, typename... Ts>
constexpr inline bool contains_v = number_of_first<T, Ts...> != npos;

template <typename T>
constexpr inline bool is_byte_like_v =
    std::is_same_v<std::byte, T> || std::is_same_v<char, T> || std::is_same_v<unsigned char, T>;

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
struct any_method_traits<R(Self, Args...)> {
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
struct any_method_traits<R(Self, Args...) noexcept> {
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

// returns index in list where first typelist starts as subsequence in second typelist or npos if
// no such index
template <typename... Ts1, typename Head, typename... Ts2>
AA_CONSTEVAL_CPP20 size_t find_subsequence_impl(aa::type_list<Ts1...> needle,
                                                aa::type_list<Head, Ts2...> haystack, size_t n) noexcept {
  if constexpr (sizeof...(Ts1) >= sizeof...(Ts2) + 1)
    return std::is_same_v<aa::type_list<Ts1...>, aa::type_list<Head, Ts2...>> ? n : ::aa::npos;
  else if constexpr (starts_with(needle, haystack))
    return n;
  else
    return find_subsequence_impl(needle, aa::type_list<Ts2...>{}, n + 1);
}
// MSVC WORKAROUND! this wrapper just fixes MSVC bug with overload resolution and aliases
// (this is why it does not accepts type_lists)
// returns index of type in Haystack, where subsequence Needle begins or npos if no such index
template <typename Needle, typename Haystack>
AA_CONSTEVAL_CPP20 size_t find_subsequence(Needle needle, Haystack haystack) noexcept {
  if constexpr (std::is_same_v<Haystack, type_list<>>)
    return std::is_same_v<Needle, type_list<>> ? 0 : npos;
  else
    return find_subsequence_impl(needle, haystack, 0);
}
template <typename Needle, typename Haystack>
AA_CONSTEVAL_CPP20 bool has_subsequence(Needle needle, Haystack haystack) {
  return find_subsequence(needle, haystack) != aa::npos;
}
template <typename... Needle, typename... Haystack>
AA_CONSTEVAL_CPP20 bool has_subset(type_list<Needle...>, type_list<Haystack...>) {
  return (contains_v<Needle, Haystack...> && ...);
}

template <typename T>
AA_ALWAYS_INLINE constexpr void destroy_at(const T* location) noexcept {
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

template <typename Any, typename Method>
auto get_plugin(int) -> typename Method::template plugin<Any>;
template <typename Any, typename Method>
auto get_plugin(bool) -> typename Method::plugin;
template <typename, typename>
auto get_plugin(...) -> void;

template <typename T>
struct type_identity {
  using type = T;
};

template <typename Method>
auto get_method_signature(int) -> type_identity<typename Method::signature_type>;
template <typename Method>
auto get_method_signature(bool)
    -> type_identity<std::remove_pointer_t<decltype(&Method::template do_invoke<erased_self_t>)>>;
template <typename Method>
auto get_method_signature(...) -> type_identity<typename Method::value_type>;

template <typename Method>
using signature_t = typename decltype(get_method_signature<Method>(0))::type;

AA_IS_VALID(has_has_value, decltype(std::declval<T>().has_value()));

template <typename T, size_t SooS>
constexpr inline bool is_fits_in_soo_buffer =
    alignof(std::decay_t<T>) <= alignof(std::max_align_t) &&
    std::is_nothrow_move_constructible_v<std::decay_t<T>> && sizeof(std::decay_t<T>) <= SooS;

// precondition !!src && !!dest
template <typename T>
void relocate(void* _src, void* dest) noexcept {
  T* src = static_cast<T*>(_src);
  new (dest) T(std::move(*src));
  noexport::destroy_at(src);
}
// precondition: 'dest' do not contain object, so never overlaps with 'src'
// used by 'move', 'copy_with' methods
// to reduce count of functions from CountOfTypes to
// CountOfDifferentSizeofs
template <size_t Sizeof>
void relocate_trivial(void* src, void* dest) noexcept {
  // memcpy inlined and optimized by gcc/clang
  std::memcpy(dest, src, Sizeof);
}

// preconditions:
//    * no object under 'dest'
//    * atleast sizeof(T) bytes under 'dest' and good align
//    * T object under 'src_raw'
template <typename T>
static void copy_fn(const void* src_raw, void* dest) {
  const T& src = *reinterpret_cast<const T*>(src_raw);
  new (dest) T(src);
}

template <size_t Sizeof>
static void trivial_copy_fn(const void* src_raw, void* dest) noexcept {
  std::memcpy(dest, src_raw, Sizeof);
}

template <typename... Types>
AA_CONSTEVAL_CPP20 auto flatten_type_lists_one_layer() {
  return (std::conditional_t<is_type_list<Types>::value, Types, type_list<Types>>{} + ... +
          aa::type_list<>{});
}

template <typename... Ts>
constexpr bool contains_second_layer_list(aa::type_list<Ts...>) {
  return (is_type_list<Ts>::value || ...);
}

template <template <typename...> typename Template, typename... Types>
auto insert_types(aa::type_list<Types...>) -> Template<Types...>;

template <typename, typename T>
using enable_if_impl = T;

template <template <typename> typename Pred, typename... Types>
AA_CONSTEVAL_CPP20 auto filter_types(type_list<Types...>) {
  return (type_list<>{} + ... + std::conditional_t<Pred<Types>::value, type_list<Types>, type_list<>>{});
}
template <typename Pred, typename... Types>
AA_CONSTEVAL_CPP20 auto filter_types(type_list<Types...>) {
  return (type_list<>{} + ... +
          std::conditional_t<Pred{}.template operator()<Types>(), type_list<Types>, type_list<>>{});
}

template <typename... Out>
AA_CONSTEVAL_CPP20 auto remove_duplicates(type_list<>, type_list<Out...>) -> type_list<Out...> {
  return {};
}

template <typename Head, typename... Types, typename... Out>
AA_CONSTEVAL_CPP20 auto remove_duplicates(type_list<Head, Types...>, type_list<Out...>) {
  if constexpr (contains_v<Head, Out...>)
    return remove_duplicates(type_list<Types...>{}, type_list<Out...>{});
  else
    return remove_duplicates(type_list<Types...>{}, type_list<Out..., Head>{});
}

// used in specialization std::default_delete for custom unique_ptr
struct deallocate_with_delete {
 private:
  template <typename T>
  static void delete_fn(const void* ptr) noexcept {
    delete reinterpret_cast<std::add_pointer_t<const T>>(ptr);
  }

 public:
  using value_type = void (*)(const void*) noexcept;

  template <typename T>
  static AA_CONSTEVAL_CPP20 value_type do_value() {
    return &delete_fn<T>;
  }
};
#if !__cpp_exceptions
struct noop_guard {
  constexpr void no_longer_needed() noexcept {}
};
#endif

}  // namespace aa::noexport

namespace aa {

template <typename... Types>
AA_CONSTEVAL_CPP20 auto flatten_types(aa::type_list<Types...> list) {
  if constexpr (noexport::contains_second_layer_list(list))
    return flatten_types(noexport::flatten_type_lists_one_layer<Types...>());
  else
    return list;
}
// accepts types and type lists, flattens all type lists in 'Ts' recursivelly into one type list
template <typename... Ts>
using flatten_types_t = decltype(flatten_types(aa::type_list<Ts...>{}));

template <typename... Ts>
using inheritor_without_duplicates_t =
    decltype(noexport::inherit_without_duplicates(type_list<Ts...>{}, type_list<>{}));

// behaves as non-dependent std::enable_if_t,
// usefull in partial specializations of structs
template <bool Cond, typename T = void>
using enable_if_t = noexport::enable_if_impl<std::enable_if_t<Cond>, T>;

template <typename Foo>
struct [[nodiscard("name it and set success points")]] scope_failure {
  Foo fn;
  bool failed = true;

  void operator=(scope_failure&&) = delete;

  ~scope_failure() noexcept(noexcept(fn())) {
    if (failed)
      (void)fn();
  }
  // should be called where the 'fn' call is no longer needed
  constexpr void no_longer_needed() noexcept {
    failed = false;
  }
};
template <typename T>
scope_failure(T&&) -> scope_failure<std::decay_t<T>>;

}  // namespace aa

#include "file_end.hpp"
