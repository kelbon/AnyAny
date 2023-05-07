#pragma once

#include <tuple>
#include <utility>
#include <concepts>

namespace aa::noexport {

struct any_type {
  template <typename T>
  operator T&();
  template <typename T>
  operator T&&();
};
template <std::size_t>
using any_type_t = any_type;

// there are types which is not constructible with (), but constructible with {}
// but i stil uusing is_constructible_v because of types which accept std::initalizer_list
// ( because it would break code, it accept any count of arguments)

// MSVC intrinsic for is_constructible has bug, nice(no)
#if __cpp_aggregate_paren_init >= 201902L && !defined(_MSC_VER)
template <typename T, typename... Ts>
constexpr bool is_constructible_v = std::is_constructible_v<T, Ts...>;
#else
template <typename T, typename... Ts>
constexpr bool is_constructible_v = requires(Ts... vals) {
                                      { T{vals...} };
                                    };
#endif
template <typename T, std::size_t... Is>
consteval auto field_count(std::index_sequence<Is...>) {
  if constexpr (is_constructible_v<T, any_type_t<Is>...>)
    return sizeof...(Is);
  else
    return field_count<T>(std::make_index_sequence<sizeof...(Is) - 1>{});
}
// max supported field count for aggreagtes == 30
template <typename T>
constexpr std::size_t fields_count_v = field_count<std::remove_cvref_t<T>>(std::make_index_sequence<30>{});

// returns lvalue ref to Field if T is not rvalue
// otherwise behaves as std::forward
template<typename T, typename Field>
constexpr inline decltype(auto) forward_save_prvalues(Field& val) noexcept {
  if constexpr (std::is_rvalue_reference_v<T&&>)
    return std::forward<Field>(val);
  else if constexpr (!std::is_reference_v<Field>)
    return static_cast<Field&>(val);
  else
    return static_cast<Field&&>(val);
}
#define frwd(name) forward_save_prvalues<T, decltype(name)>(name)

// do not work for C arrays in aggregates
template <typename T>
constexpr decltype(auto) magic_tie(T&& value) noexcept {
  constexpr auto sz = fields_count_v<T>;

  if constexpr (sz == 1) {
    auto&& [_0] = std::forward<T>(value);
    return std::forward_as_tuple(frwd(_0));
  } else if constexpr (sz == 2) {
    auto&& [_0, _1] = std::forward<T>(value);
    return std::forward_as_tuple(frwd(_0), frwd(_1));
  } else if constexpr (sz == 3) {
    auto&& [_0, _1, _2] = std::forward<T>(value);
    return std::forward_as_tuple(frwd(_0), frwd(_1), frwd(_2));
  } else if constexpr (sz == 4) {
    auto&& [_0, _1, _2, _3] = std::forward<T>(value);
    return std::forward_as_tuple(frwd(_0), frwd(_1), frwd(_2), frwd(_3));
  } else if constexpr (sz == 5) {
    auto&& [_0, _1, _2, _3, _4] = std::forward<T>(value);
    return std::forward_as_tuple(frwd(_0), frwd(_1), frwd(_2), frwd(_3), frwd(_4));
  } else if constexpr (sz == 6) {
    auto&& [_0, _1, _2, _3, _4, _5] = std::forward<T>(value);
    return std::forward_as_tuple(frwd(_0), frwd(_1), frwd(_2), frwd(_3), frwd(_4), frwd(_5));
  } else if constexpr (sz == 7) {
    auto&& [_0, _1, _2, _3, _4, _5, _6] = std::forward<T>(value);
    return std::forward_as_tuple(frwd(_0), frwd(_1), frwd(_2), frwd(_3), frwd(_4), frwd(_5), frwd(_6));
  } else if constexpr (sz == 8) {
    auto&& [_0, _1, _2, _3, _4, _5, _6, _7] = std::forward<T>(value);
    return std::forward_as_tuple(frwd(_0), frwd(_1), frwd(_2), frwd(_3), frwd(_4), frwd(_5), frwd(_6),
                                 frwd(_7));
  } else if constexpr (sz == 9) {
    auto&& [_0, _1, _2, _3, _4, _5, _6, _7, _8] = std::forward<T>(value);
    return std::forward_as_tuple(frwd(_0), frwd(_1), frwd(_2), frwd(_3), frwd(_4), frwd(_5), frwd(_6),
                                 frwd(_7), frwd(_8));
  } else if constexpr (sz == 10) {
    auto&& [_0, _1, _2, _3, _4, _5, _6, _7, _8, _9] = std::forward<T>(value);
    return std::forward_as_tuple(frwd(_0), frwd(_1), frwd(_2), frwd(_3), frwd(_4), frwd(_5), frwd(_6),
                                 frwd(_7), frwd(_8), frwd(_9));

  } else if constexpr (sz == 11) {
    auto&& [_0, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10] = std::forward<T>(value);
    return std::forward_as_tuple(frwd(_0), frwd(_1), frwd(_2), frwd(_3), frwd(_4), frwd(_5), frwd(_6),
                                 frwd(_7), frwd(_8), frwd(_9), frwd(_10));
  } else if constexpr (sz == 12) {
    auto&& [_0, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11] = std::forward<T>(value);
    return std::forward_as_tuple(frwd(_0), frwd(_1), frwd(_2), frwd(_3), frwd(_4), frwd(_5), frwd(_6),
                                 frwd(_7), frwd(_8), frwd(_9), frwd(_10), frwd(_11));
  } else if constexpr (sz == 13) {
    auto&& [_0, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12] = std::forward<T>(value);
    return std::forward_as_tuple(frwd(_0), frwd(_1), frwd(_2), frwd(_3), frwd(_4), frwd(_5), frwd(_6),
                                 frwd(_7), frwd(_8), frwd(_9), frwd(_10), frwd(_11), frwd(_12));
  } else if constexpr (sz == 14) {
    auto&& [_0, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13] = std::forward<T>(value);
    return std::forward_as_tuple(frwd(_0), frwd(_1), frwd(_2), frwd(_3), frwd(_4), frwd(_5), frwd(_6),
                                 frwd(_7), frwd(_8), frwd(_9), frwd(_10), frwd(_11), frwd(_12), frwd(_13));
  } else if constexpr (sz == 15) {
    auto&& [_0, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14] = std::forward<T>(value);
    return std::forward_as_tuple(frwd(_0), frwd(_1), frwd(_2), frwd(_3), frwd(_4), frwd(_5), frwd(_6),
                                 frwd(_7), frwd(_8), frwd(_9), frwd(_10), frwd(_11), frwd(_12), frwd(_13),
                                 frwd(_14));
  } else if constexpr (sz == 16) {
    auto&& [_0, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15] = std::forward<T>(value);
    return std::forward_as_tuple(frwd(_0), frwd(_1), frwd(_2), frwd(_3), frwd(_4), frwd(_5), frwd(_6),
                                 frwd(_7), frwd(_8), frwd(_9), frwd(_10), frwd(_11), frwd(_12), frwd(_13),
                                 frwd(_14), frwd(_15));
  } else if constexpr (sz == 17) {
    auto&& [_0, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16] =
        std::forward<T>(value);
    return std::forward_as_tuple(frwd(_0), frwd(_1), frwd(_2), frwd(_3), frwd(_4), frwd(_5), frwd(_6),
                                 frwd(_7), frwd(_8), frwd(_9), frwd(_10), frwd(_11), frwd(_12), frwd(_13),
                                 frwd(_14), frwd(_15), frwd(_16));
  } else if constexpr (sz == 18) {
    auto&& [_0, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16, _17] =
        std::forward<T>(value);
    return std::forward_as_tuple(frwd(_0), frwd(_1), frwd(_2), frwd(_3), frwd(_4), frwd(_5), frwd(_6),
                                 frwd(_7), frwd(_8), frwd(_9), frwd(_10), frwd(_11), frwd(_12), frwd(_13),
                                 frwd(_14), frwd(_15), frwd(_16), frwd(_17));
  } else if constexpr (sz == 19) {
    auto&& [_0, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16, _17, _18] =
        std::forward<T>(value);
    return std::forward_as_tuple(frwd(_0), frwd(_1), frwd(_2), frwd(_3), frwd(_4), frwd(_5), frwd(_6),
                                 frwd(_7), frwd(_8), frwd(_9), frwd(_10), frwd(_11), frwd(_12), frwd(_13),
                                 frwd(_14), frwd(_15), frwd(_16), frwd(_17), frwd(_18));
  }
  else if constexpr (sz == 20) {
    auto&& [_0, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16, _17, _18, _19] =
        std::forward<T>(value);
    return std::forward_as_tuple(frwd(_0), frwd(_1), frwd(_2), frwd(_3), frwd(_4), frwd(_5), frwd(_6),
                                 frwd(_7), frwd(_8), frwd(_9), frwd(_10), frwd(_11), frwd(_12), frwd(_13),
                                 frwd(_14), frwd(_15), frwd(_16), frwd(_17), frwd(_18), frwd(_19));
  
  } else if constexpr (sz == 21) {
    auto&& [_0, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16, _17, _18, _19, _20] =
        std::forward<T>(value);
    return std::forward_as_tuple(frwd(_0), frwd(_1), frwd(_2), frwd(_3), frwd(_4), frwd(_5), frwd(_6),
                                 frwd(_7), frwd(_8), frwd(_9), frwd(_10), frwd(_11), frwd(_12), frwd(_13),
                                 frwd(_14), frwd(_15), frwd(_16), frwd(_17), frwd(_18), frwd(_19), frwd(_20));
  
  } else if constexpr (sz == 22) {
    auto&& [_0, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16, _17, _18, _19, _20,
            _21] = std::forward<T>(value);
    return
        std::forward_as_tuple(frwd(_0), frwd(_1), frwd(_2), frwd(_3), frwd(_4), frwd(_5), frwd(_6),
        frwd(_7),
                              frwd(_8), frwd(_9), frwd(_10), frwd(_11), frwd(_12), frwd(_13), frwd(_14),
                              frwd(_15), frwd(_16), frwd(_17), frwd(_18), frwd(_19), frwd(_20),
                              frwd(_21));
  
  } else if constexpr (sz == 23) {
    auto&& [_0, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16, _17, _18, _19, _20,
            _21, _22] = std::forward<T>(value);
    return std::forward_as_tuple(
        frwd(_0), frwd(_1), frwd(_2), frwd(_3), frwd(_4), frwd(_5), frwd(_6), frwd(_7), frwd(_8), frwd(_9),
        frwd(_10), frwd(_11), frwd(_12), frwd(_13), frwd(_14), frwd(_15), frwd(_16), frwd(_17), frwd(_18),
        frwd(_19), frwd(_20), frwd(_21), frwd(_22));
  
  } else if constexpr (sz == 24) {
    auto&& [_0, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16, _17, _18, _19, _20,
            _21, _22, _23] = std::forward<T>(value);
    return std::forward_as_tuple(
        frwd(_0), frwd(_1), frwd(_2), frwd(_3), frwd(_4), frwd(_5), frwd(_6), frwd(_7), frwd(_8), frwd(_9),
        frwd(_10), frwd(_11), frwd(_12), frwd(_13), frwd(_14), frwd(_15), frwd(_16), frwd(_17), frwd(_18),
        frwd(_19), frwd(_20), frwd(_21), frwd(_22), frwd(_23));
  
  } else if constexpr (sz == 25) {
    auto&& [_0, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16, _17, _18, _19, _20,
            _21, _22, _23, _24] = std::forward<T>(value);
    return std::forward_as_tuple(
        frwd(_0), frwd(_1), frwd(_2), frwd(_3), frwd(_4), frwd(_5), frwd(_6), frwd(_7), frwd(_8), frwd(_9),
        frwd(_10), frwd(_11), frwd(_12), frwd(_13), frwd(_14), frwd(_15), frwd(_16), frwd(_17), frwd(_18),
        frwd(_19), frwd(_20), frwd(_21), frwd(_22), frwd(_23), frwd(_24));
  
  } else if constexpr (sz == 26) {
    auto&& [_0, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16, _17, _18, _19, _20,
            _21, _22, _23, _24, _25] = std::forward<T>(value);
    return std::forward_as_tuple(
        frwd(_0), frwd(_1), frwd(_2), frwd(_3), frwd(_4), frwd(_5), frwd(_6), frwd(_7), frwd(_8), frwd(_9),
        frwd(_10), frwd(_11), frwd(_12), frwd(_13), frwd(_14), frwd(_15), frwd(_16), frwd(_17), frwd(_18),
        frwd(_19), frwd(_20), frwd(_21), frwd(_22), frwd(_23), frwd(_24), frwd(_25));
  
  } else if constexpr (sz == 27) {
    auto&& [_0, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16, _17, _18, _19, _20,
            _21, _22, _23, _24, _25, _26] = std::forward<T>(value);
    return std::forward_as_tuple(
        frwd(_0), frwd(_1), frwd(_2), frwd(_3), frwd(_4), frwd(_5), frwd(_6), frwd(_7), frwd(_8), frwd(_9),
        frwd(_10), frwd(_11), frwd(_12), frwd(_13), frwd(_14), frwd(_15), frwd(_16), frwd(_17), frwd(_18),
        frwd(_19), frwd(_20), frwd(_21), frwd(_22), frwd(_23), frwd(_24), frwd(_25), frwd(_26));
  
  } else if constexpr (sz == 28) {
    auto&& [_0, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16, _17, _18, _19, _20,
            _21, _22, _23, _24, _25, _26, _27] = std::forward<T>(value);
    return std::forward_as_tuple(
        frwd(_0), frwd(_1), frwd(_2), frwd(_3), frwd(_4), frwd(_5), frwd(_6), frwd(_7), frwd(_8), frwd(_9),
        frwd(_10), frwd(_11), frwd(_12), frwd(_13), frwd(_14), frwd(_15), frwd(_16), frwd(_17), frwd(_18),
        frwd(_19), frwd(_20), frwd(_21), frwd(_22), frwd(_23), frwd(_24), frwd(_25), frwd(_26),
        frwd(_27));
  
  } else if constexpr (sz == 29) {
    auto&& [_0, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16, _17, _18, _19, _20,
            _21, _22, _23, _24, _25, _26, _27, _28] = std::forward<T>(value);
    return std::forward_as_tuple(frwd(_0), frwd(_1), frwd(_2), frwd(_3), frwd(_4), frwd(_5), frwd(_6),
                                 frwd(_7), frwd(_8), frwd(_9), frwd(_10), frwd(_11), frwd(_12), frwd(_13),
                                 frwd(_14), frwd(_15), frwd(_16), frwd(_17), frwd(_18), frwd(_19), frwd(_20),
                                 frwd(_21), frwd(_22), frwd(_23), frwd(_24), frwd(_25), frwd(_26), frwd(_27),
                                 frwd(_28));
  }
}
#undef frwd

template <std::size_t I, typename T>
constexpr decltype(auto) magic_get(T&& val) {
  return std::get<I>(magic_tie(std::forward<T>(val)));
}
// vector<T> do not supports const T and T& anyway, so i remove cvref here
template <std::size_t I, typename T>
using field_type_t = std::remove_cvref_t<decltype(magic_get<I>(std::declval<T>()))>;

template <typename T>
constexpr inline bool is_tuple_like = requires(T value) {
                                        { std::get<0>(value) };
                                      };

// Why you wrote such a strange code here?
// BECAUSE OF $^$%$% MSVC, it cant compile normal code!
template <std::size_t I, typename T>
  requires(is_tuple_like<T>)
consteval auto msvc_is_shit(int) {
  return std::type_identity<std::tuple_element_t<I, T>>{};
}
template <std::size_t I, typename T>
consteval auto msvc_is_shit(...) {
  return std::type_identity<noexport::field_type_t<I, T>>{};
}

// tuple-like traits
struct tl_traits {
  // selects tuple_size if exist, otherwise calculates field count
  template <typename Tpl>
  static constexpr std::size_t tuple_size = [] {
    using T = std::remove_cvref_t<Tpl>;
    if constexpr (noexport::is_tuple_like<T>)
      return std::tuple_size_v<Tpl>;
    else if constexpr (std::is_aggregate_v<T>)
      return noexport::fields_count_v<T>;
    else
      static_assert(
          ![] {}, "T must be aggreagte or tuple-like!");
  }();  // INVOKED HERE

  template <std::size_t I, typename Tpl>
  using tuple_element =
      typename decltype(noexport::msvc_is_shit<I, std::remove_cvref_t<Tpl>>(0))::type;  // INVOKED HERE

  template <std::size_t I, typename Tpl>
  static constexpr decltype(auto) get(Tpl&& v) {
    if constexpr (noexport::is_tuple_like<std::remove_cvref_t<Tpl>>)
      return std::get<I>(std::forward<Tpl>(v));
    else
      return noexport::magic_get<I>(std::forward<Tpl>(v));
  }
};

// std::vector have specialization for bools, this type behaves like bool and disables it
struct bool_ {
  bool b{};

  constexpr bool_() = default;
  constexpr bool_(const bool_&) = default;
  constexpr bool_& operator=(const bool_&) = default;
  constexpr bool_(bool_&&) = default;
  constexpr bool_& operator=(bool_&&) = default;

  constexpr bool_(bool x) noexcept : b(x) {
  }
  constexpr bool_& operator=(bool x) noexcept {
    b = x;
    return *this;
  }
  constexpr operator bool&&() && noexcept {
    return std::move(b);
  }
  constexpr operator const bool&&() const&& noexcept {
    return std::move(b);
  }
  constexpr operator bool&() & noexcept {
    return b;
  }
  constexpr operator const bool&() const & noexcept {
    return b;
  }
  // removes ambiguity
  template <std::same_as<bool> B>
  constexpr bool operator==(const B& x) const noexcept {
    return b == x;
  }
  // removes ambiguity
  template <std::same_as<bool> B>
  constexpr auto operator<=>(const B& x) const noexcept {
    return b <=> x;
  }
  constexpr bool operator==(const bool_& x) const noexcept {
    return b == x;
  }
  constexpr auto operator<=>(const bool_& x) const noexcept {
    return b <=> x.b;
  }
};

}  // namespace aa::noexport