#pragma once

#include <tuple>

namespace aa::noexport {

struct any_type {
  template <typename T>
  operator T&();
  template <typename T>
  operator T&&();
};
template<std::size_t>
using any_type_t = any_type;

// there are types which is not constructible with (), but constructible with {}
// but i stil uusing is_constructible_v because of types which accept std::initalizer_list
// ( because it would break code, it accept any count of arguments)

// clang do not support this C++20 featuure yet(almost 2023 now...)
#if __cpp_aggregate_paren_init >= 201902L
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

#define frwd(name) std::forward<decltype(name)>(name)

// do not work for C arrays in aggregates
template <std::size_t Index, typename T>
constexpr decltype(auto) magic_get(T&& value) noexcept {
  constexpr auto sz = fields_count_v<T>;

  if constexpr (sz == 1) {
    auto&& [_0] = std::forward<T>(value);
    auto&& getted_value = std::get<Index>(std::forward_as_tuple(frwd(_0)));
    return frwd(getted_value);
  } else if constexpr (sz == 2) {
    auto&& [_0, _1] = std::forward<T>(value);
    auto&& getted_value = std::get<Index>(std::forward_as_tuple(frwd(_0), frwd(_1)));
    return frwd(getted_value);
  } else if constexpr (sz == 3) {
    auto&& [_0, _1, _2] = std::forward<T>(value);
    auto&& getted_value = std::get<Index>(std::forward_as_tuple(frwd(_0), frwd(_1), frwd(_2)));
    return frwd(getted_value);
  } else if constexpr (sz == 4) {
    auto&& [_0, _1, _2, _3] = std::forward<T>(value);
    auto&& getted_value = std::get<Index>(std::forward_as_tuple(frwd(_0), frwd(_1), frwd(_2), frwd(_3)));
    return frwd(getted_value);
  } else if constexpr (sz == 5) {
    auto&& [_0, _1, _2, _3, _4] = std::forward<T>(value);
    auto&& getted_value =
        std::get<Index>(std::forward_as_tuple(frwd(_0), frwd(_1), frwd(_2), frwd(_3), frwd(_4)));
    return frwd(getted_value);
  } else if constexpr (sz == 6) {
    auto&& [_0, _1, _2, _3, _4, _5] = std::forward<T>(value);
    auto&& getted_value =
        std::get<Index>(std::forward_as_tuple(frwd(_0), frwd(_1), frwd(_2), frwd(_3), frwd(_4), frwd(_5)));
    return frwd(getted_value);
  } else if constexpr (sz == 7) {
    auto&& [_0, _1, _2, _3, _4, _5, _6] = std::forward<T>(value);
    auto&& getted_value = std::get<Index>(
        std::forward_as_tuple(frwd(_0), frwd(_1), frwd(_2), frwd(_3), frwd(_4), frwd(_5), frwd(_6)));
    return frwd(getted_value);
  } else if constexpr (sz == 8) {
    auto&& [_0, _1, _2, _3, _4, _5, _6, _7] = std::forward<T>(value);
    auto&& getted_value = std::get<Index>(std::forward_as_tuple(frwd(_0), frwd(_1), frwd(_2), frwd(_3),
                                                                frwd(_4), frwd(_5), frwd(_6), frwd(_7)));
    return frwd(getted_value);
  } else if constexpr (sz == 9) {
    auto&& [_0, _1, _2, _3, _4, _5, _6, _7, _8] = std::forward<T>(value);
    auto&& getted_value = std::get<Index>(std::forward_as_tuple(
        frwd(_0), frwd(_1), frwd(_2), frwd(_3), frwd(_4), frwd(_5), frwd(_6), frwd(_7), frwd(_8)));
    return frwd(getted_value);
  } else if constexpr (sz == 10) {
    auto&& [_0, _1, _2, _3, _4, _5, _6, _7, _8, _9] = std::forward<T>(value);
    auto&& getted_value = std::get<Index>(std::forward_as_tuple(
        frwd(_0), frwd(_1), frwd(_2), frwd(_3), frwd(_4), frwd(_5), frwd(_6), frwd(_7), frwd(_8), frwd(_9)));
    return frwd(getted_value);
  } else if constexpr (sz == 11) {
    auto&& [_0, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10] = std::forward<T>(value);
    auto&& getted_value =
        std::get<Index>(std::forward_as_tuple(frwd(_0), frwd(_1), frwd(_2), frwd(_3), frwd(_4), frwd(_5),
                                              frwd(_6), frwd(_7), frwd(_8), frwd(_9), frwd(_10)));
    return frwd(getted_value);
  } else if constexpr (sz == 12) {
    auto&& [_0, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11] = std::forward<T>(value);
    auto&& getted_value =
        std::get<Index>(std::forward_as_tuple(frwd(_0), frwd(_1), frwd(_2), frwd(_3), frwd(_4), frwd(_5),
                                              frwd(_6), frwd(_7), frwd(_8), frwd(_9), frwd(_10), frwd(_11)));
    return frwd(getted_value);
  } else if constexpr (sz == 13) {
    auto&& [_0, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12] = std::forward<T>(value);
    auto&& getted_value = std::get<Index>(
        std::forward_as_tuple(frwd(_0), frwd(_1), frwd(_2), frwd(_3), frwd(_4), frwd(_5), frwd(_6), frwd(_7),
                              frwd(_8), frwd(_9), frwd(_10), frwd(_11), frwd(_12)));
    return frwd(getted_value);
  } else if constexpr (sz == 14) {
    auto&& [_0, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13] = std::forward<T>(value);
    auto&& getted_value = std::get<Index>(
        std::forward_as_tuple(frwd(_0), frwd(_1), frwd(_2), frwd(_3), frwd(_4), frwd(_5), frwd(_6), frwd(_7),
                              frwd(_8), frwd(_9), frwd(_10), frwd(_11), frwd(_12), frwd(_13)));
    return frwd(getted_value);
  } else if constexpr (sz == 15) {
    auto&& [_0, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14] = std::forward<T>(value);
    auto&& getted_value = std::get<Index>(
        std::forward_as_tuple(frwd(_0), frwd(_1), frwd(_2), frwd(_3), frwd(_4), frwd(_5), frwd(_6), frwd(_7),
                              frwd(_8), frwd(_9), frwd(_10), frwd(_11), frwd(_12), frwd(_13), frwd(_14)));
    return frwd(getted_value);
  } else if constexpr (sz == 16) {
    auto&& [_0, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15] = std::forward<T>(value);
    auto&& getted_value = std::get<Index>(std::forward_as_tuple(
        frwd(_0), frwd(_1), frwd(_2), frwd(_3), frwd(_4), frwd(_5), frwd(_6), frwd(_7), frwd(_8), frwd(_9),
        frwd(_10), frwd(_11), frwd(_12), frwd(_13), frwd(_14), frwd(_15)));
    return frwd(getted_value);
  } else if constexpr (sz == 17) {
    auto&& [_0, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16] =
        std::forward<T>(value);
    auto&& getted_value = std::get<Index>(std::forward_as_tuple(
        frwd(_0), frwd(_1), frwd(_2), frwd(_3), frwd(_4), frwd(_5), frwd(_6), frwd(_7), frwd(_8), frwd(_9),
        frwd(_10), frwd(_11), frwd(_12), frwd(_13), frwd(_14), frwd(_15), frwd(_16)));
    return frwd(getted_value);
  } else if constexpr (sz == 18) {
    auto&& [_0, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16, _17] =
        std::forward<T>(value);
    auto&& getted_value = std::get<Index>(std::forward_as_tuple(
        frwd(_0), frwd(_1), frwd(_2), frwd(_3), frwd(_4), frwd(_5), frwd(_6), frwd(_7), frwd(_8), frwd(_9),
        frwd(_10), frwd(_11), frwd(_12), frwd(_13), frwd(_14), frwd(_15), frwd(_16), frwd(_17)));
    return frwd(getted_value);
  } else if constexpr (sz == 19) {
    auto&& [_0, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16, _17, _18] =
        std::forward<T>(value);
    auto&& getted_value = std::get<Index>(std::forward_as_tuple(
        frwd(_0), frwd(_1), frwd(_2), frwd(_3), frwd(_4), frwd(_5), frwd(_6), frwd(_7), frwd(_8), frwd(_9),
        frwd(_10), frwd(_11), frwd(_12), frwd(_13), frwd(_14), frwd(_15), frwd(_16), frwd(_17), frwd(_18)));
    return frwd(getted_value);
  } else if constexpr (sz == 20) {
    auto&& [_0, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16, _17, _18, _19] =
        std::forward<T>(value);
    auto&& getted_value = std::get<Index>(
        std::forward_as_tuple(frwd(_0), frwd(_1), frwd(_2), frwd(_3), frwd(_4), frwd(_5), frwd(_6), frwd(_7),
                              frwd(_8), frwd(_9), frwd(_10), frwd(_11), frwd(_12), frwd(_13), frwd(_14),
                              frwd(_15), frwd(_16), frwd(_17), frwd(_18), frwd(_19)));
    return frwd(getted_value);
  } else if constexpr (sz == 21) {
    auto&& [_0, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16, _17, _18, _19, _20] =
        std::forward<T>(value);
    auto&& getted_value = std::get<Index>(
        std::forward_as_tuple(frwd(_0), frwd(_1), frwd(_2), frwd(_3), frwd(_4), frwd(_5), frwd(_6), frwd(_7),
                              frwd(_8), frwd(_9), frwd(_10), frwd(_11), frwd(_12), frwd(_13), frwd(_14),
                              frwd(_15), frwd(_16), frwd(_17), frwd(_18), frwd(_19), frwd(_20)));
    return frwd(getted_value);
  } else if constexpr (sz == 22) {
    auto&& [_0, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16, _17, _18, _19, _20,
            _21] = std::forward<T>(value);
    auto&& getted_value = std::get<Index>(
        std::forward_as_tuple(frwd(_0), frwd(_1), frwd(_2), frwd(_3), frwd(_4), frwd(_5), frwd(_6), frwd(_7),
                              frwd(_8), frwd(_9), frwd(_10), frwd(_11), frwd(_12), frwd(_13), frwd(_14),
                              frwd(_15), frwd(_16), frwd(_17), frwd(_18), frwd(_19), frwd(_20), frwd(_21)));
    return frwd(getted_value);
  } else if constexpr (sz == 23) {
    auto&& [_0, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16, _17, _18, _19, _20,
            _21, _22] = std::forward<T>(value);
    auto&& getted_value = std::get<Index>(std::forward_as_tuple(
        frwd(_0), frwd(_1), frwd(_2), frwd(_3), frwd(_4), frwd(_5), frwd(_6), frwd(_7), frwd(_8), frwd(_9),
        frwd(_10), frwd(_11), frwd(_12), frwd(_13), frwd(_14), frwd(_15), frwd(_16), frwd(_17), frwd(_18),
        frwd(_19), frwd(_20), frwd(_21), frwd(_22)));
    return frwd(getted_value);
  } else if constexpr (sz == 24) {
    auto&& [_0, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16, _17, _18, _19, _20,
            _21, _22, _23] = std::forward<T>(value);
    auto&& getted_value = std::get<Index>(std::forward_as_tuple(
        frwd(_0), frwd(_1), frwd(_2), frwd(_3), frwd(_4), frwd(_5), frwd(_6), frwd(_7), frwd(_8), frwd(_9),
        frwd(_10), frwd(_11), frwd(_12), frwd(_13), frwd(_14), frwd(_15), frwd(_16), frwd(_17), frwd(_18),
        frwd(_19), frwd(_20), frwd(_21), frwd(_22), frwd(_23)));
    return frwd(getted_value);
  } else if constexpr (sz == 25) {
    auto&& [_0, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16, _17, _18, _19, _20,
            _21, _22, _23, _24] = std::forward<T>(value);
    auto&& getted_value = std::get<Index>(std::forward_as_tuple(
        frwd(_0), frwd(_1), frwd(_2), frwd(_3), frwd(_4), frwd(_5), frwd(_6), frwd(_7), frwd(_8), frwd(_9),
        frwd(_10), frwd(_11), frwd(_12), frwd(_13), frwd(_14), frwd(_15), frwd(_16), frwd(_17), frwd(_18),
        frwd(_19), frwd(_20), frwd(_21), frwd(_22), frwd(_23), frwd(_24)));
    return frwd(getted_value);
  } else if constexpr (sz == 26) {
    auto&& [_0, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16, _17, _18, _19, _20,
            _21, _22, _23, _24, _25] = std::forward<T>(value);
    auto&& getted_value = std::get<Index>(std::forward_as_tuple(
        frwd(_0), frwd(_1), frwd(_2), frwd(_3), frwd(_4), frwd(_5), frwd(_6), frwd(_7), frwd(_8), frwd(_9),
        frwd(_10), frwd(_11), frwd(_12), frwd(_13), frwd(_14), frwd(_15), frwd(_16), frwd(_17), frwd(_18),
        frwd(_19), frwd(_20), frwd(_21), frwd(_22), frwd(_23), frwd(_24), frwd(_25)));
    return frwd(getted_value);
  } else if constexpr (sz == 27) {
    auto&& [_0, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16, _17, _18, _19, _20,
            _21, _22, _23, _24, _25, _26] = std::forward<T>(value);
    auto&& getted_value = std::get<Index>(std::forward_as_tuple(
        frwd(_0), frwd(_1), frwd(_2), frwd(_3), frwd(_4), frwd(_5), frwd(_6), frwd(_7), frwd(_8), frwd(_9),
        frwd(_10), frwd(_11), frwd(_12), frwd(_13), frwd(_14), frwd(_15), frwd(_16), frwd(_17), frwd(_18),
        frwd(_19), frwd(_20), frwd(_21), frwd(_22), frwd(_23), frwd(_24), frwd(_25), frwd(_26)));
    return frwd(getted_value);
  } else if constexpr (sz == 28) {
    auto&& [_0, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16, _17, _18, _19, _20,
            _21, _22, _23, _24, _25, _26, _27] = std::forward<T>(value);
    auto&& getted_value = std::get<Index>(std::forward_as_tuple(
        frwd(_0), frwd(_1), frwd(_2), frwd(_3), frwd(_4), frwd(_5), frwd(_6), frwd(_7), frwd(_8), frwd(_9),
        frwd(_10), frwd(_11), frwd(_12), frwd(_13), frwd(_14), frwd(_15), frwd(_16), frwd(_17), frwd(_18),
        frwd(_19), frwd(_20), frwd(_21), frwd(_22), frwd(_23), frwd(_24), frwd(_25), frwd(_26), frwd(_27)));
    return frwd(getted_value);
  } else if constexpr (sz == 29) {
    auto&& [_0, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16, _17, _18, _19, _20,
            _21, _22, _23, _24, _25, _26, _27, _28] = std::forward<T>(value);
    auto&& getted_value = std::get<Index>(
        std::forward_as_tuple(frwd(_0), frwd(_1), frwd(_2), frwd(_3), frwd(_4), frwd(_5), frwd(_6), frwd(_7),
                              frwd(_8), frwd(_9), frwd(_10), frwd(_11), frwd(_12), frwd(_13), frwd(_14),
                              frwd(_15), frwd(_16), frwd(_17), frwd(_18), frwd(_19), frwd(_20), frwd(_21),
                              frwd(_22), frwd(_23), frwd(_24), frwd(_25), frwd(_26), frwd(_27), frwd(_28)));
    return frwd(getted_value);
  }
}
#undef frwd

// vector<T> do not supports const T and T& anyway, so i remove cvref here
template<size_t I, typename T>
using field_type_t = std::remove_cvref_t<decltype(magic_get<I>(std::declval<T>()))>;

}  // namespace aa::noexport