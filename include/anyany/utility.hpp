#pragma once
/*
  HEADER SYNOPSIS:

  * type_switch<Result, Traits = anyany_poly_traits>
  switch-like syntax for 
  * std_variant_poly_traits
  * visitor_interface<Types...> - any_with<visitor_interface<Types...>> will have .visit and operator() for each type in 'Types'
  * visitor2_interface<type_list1, type_list2> - any_with<visitor2_interface<...>> will have .visit and operator() FOR EACH PAIR
  of types from first type list(for first argument) and second (for second argument)
  * matcher (overloaded for usage with visitors / std::visit etc)
*/
#include "anyany.hpp"

#include <optional>    // for type_switch
#include <variant>     // only for std_variant_poly_traits
#include <concepts>

namespace aa::noexport {

template <typename PolyPtr, typename ResultT, typename Traits>
struct type_switch_fn {
 private:
  struct non_void {
    // no way to create it for user
    explicit non_void() = default;
  };
  using Result = std::conditional_t<std::is_void_v<ResultT>, non_void, ResultT>;

 public:
  constexpr explicit type_switch_fn(std::type_identity_t<PolyPtr>&& value) noexcept
      : value(std::move(value)) {
    assert(value != nullptr);
  }

  template <typename T, typename Fn>
  type_switch_fn& case_(Fn&& f) {
    if (result)
      return *this;
    if (auto* v = ::aa::any_cast<T, Traits>(value)) {
      if constexpr (std::is_void_v<ResultT>) {
        std::forward<Fn>(f)(*v);
        result.emplace();
      } else {
        result = std::forward<Fn>(f)(*v);
      }
    }
    return *this;
  }
  // If value is one of Ts... F invoked (invokes count <= 1)
  template <typename... Ts, typename Fn>
  type_switch_fn& cases(Fn&& f) {
    struct assert_ : noexport::type_identity<Ts>... {
    } assert_unique_types;
    (void)assert_unique_types;
    (case_<Ts>(std::forward<Fn>(f)), ...);
    return *this;
  }
  // As a default, invoke the given callable within the root value.
  [[nodiscard]] Result default_(std::invocable<decltype(*PolyPtr{})> auto&& f) {
    if (result)
      return std::move(*result);
    return std::forward<decltype(f)>(f)(*value);
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

}  // namespace noexport

// ######################## ACTION type_switch for all polymorphic types ########################

namespace aa {

// implements a switch-like dispatch statement for poly_ptr/const_poly_ptr
// Each `case_<T>` takes a callable to be invoked, if value is of type 'T'
//
// usage example:
//  using my_any = aa::any_with</* ... */>;
//  my_any value = /* ... */;
//  Result result = type_switch<Result>(value)
//    .case_<A>([](A op) { ... })
//    .cases<A, B, C, D>([](auto&&) { /* do smth */ }
//    .default_([](aa::cref<Methods...> ref) { ... });
template <typename Result = void, typename Traits = anyany_poly_traits>
constexpr auto type_switch(polymorphic auto&& value) noexcept {
  using ptr_t = decltype(&value);
  if constexpr (std::is_pointer_v<ptr_t>)  // poly ptr case
    return noexport::type_switch_fn<noexport::remove_cvref_t<decltype(value)>, Result, Traits>(value);
  else  // any_with/poly_ref case
    return noexport::type_switch_fn<ptr_t, Result, Traits>{&value};
}

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
  static constexpr auto* to_address(T&& v) noexcept {
    return static_cast<std::conditional_t<std::is_const_v<std::remove_reference_t<T>>, const void*, void*>>(
        std::addressof(v));
  }
  // Do not support cases like variant<int, const int> with mixed constness
  template <typename... Ts>
  static constexpr const void* to_address(const std::variant<Ts...>& v) noexcept {
    return std::visit([](const auto& v) { return static_cast<const void*>(std::addressof(v)); }, v);
  }
  template <typename... Ts>
  static constexpr void* to_address(std::variant<Ts...>& v) noexcept {
    return std::visit([](auto& v) { return static_cast<void*>(std::addressof(v)); }, v);
  }
  template <typename... Ts>
  static constexpr void* to_address(std::variant<Ts...>&& v) noexcept {
    return std::visit([](auto&& v) { return static_cast<void*>(std::addressof(v)); }, v);
  }
};

// tag type, which noop functions returns(usefull for optimizing visit/call)
struct noop {};

// Method for visiting 'T'
// adds .visit(T&) and operator()(T&) overloads into interface
template <typename T>
struct visit1 {
 private:
  template <typename Visitor, typename U>
  static void visit_fn(void* vtor, void* visitable) {
    (void)(*reinterpret_cast<const Visitor*>(vtor))(*reinterpret_cast<std::add_pointer_t<U>>(visitable));
  }
  static void noop_fn(void*, void*) {
  }

 public:
  using value_type = void (*)(void*, void*);

  template <typename Self>
    requires(std::invocable<const Self, T&>)
  static consteval value_type do_value() {
    if constexpr (std::is_same_v<std::invoke_result_t<const Self&, T&>, aa::noop>)
      return &noop_fn;
    else
      return &visit_fn<Self, T>;
  }
};

// supports visit one argument
template <typename... Ts>
using visitor_interface = aa::interface_alias<visit1<Ts>...>;

// Method for visiting 2 values
// adds .visit(T&, U&) and operator()(T&, U&) overloads into interface
template <typename T, typename U>
struct visit2 {
 private:
  template <typename Visitor, typename T1, typename U1>
  static void visit_fn(void* vtor, void* visitable1, void* visitable2) {
    (void)(*reinterpret_cast<const Visitor*>(vtor))(*reinterpret_cast<std::add_pointer_t<T1>>(visitable1),
                                                    *reinterpret_cast<std::add_pointer_t<U1>>(visitable2));
  }
  static void noop_fn(void*, void*, void*);

 public:
  using value_type = void (*)(void*, void*, void*);

  template <typename Self>
    requires(std::invocable<const Self, T&, U&>)
  static consteval value_type do_value() {
    if constexpr (std::is_same_v<std::invoke_result_t<const Self&, T&, U&>, aa::noop>)
      return &noop_fn;
    else
      return &visit_fn<Self, T, U>;
  }
};

namespace noexport {

template <typename CRTP, typename>
struct overload_for {};

template <typename CRTP, typename T>
struct overload_for<CRTP, visit1<T>> {
  static constexpr void do_visit(const CRTP& visitor, T& value) {
    aa::invoke<visit1<T>>(visitor)(const_cast<void*>(mate::get_value_ptr(visitor)), std::addressof(value));
  }
};
template <typename CRTP, typename T, typename U>
struct overload_for<CRTP, visit2<T, U>> {
  static constexpr void do_visit(const CRTP& visitor, T& v1, U& v2) {
    aa::invoke<visit2<T, U>>(visitor)(const_cast<void*>(mate::get_value_ptr(visitor)), std::addressof(v1),
                                      std::addressof(v2));
  }
};

template <typename CRTP, typename... Types>
struct visit_overload_set : private inheritor_without_duplicates_t<overload_for<CRTP, Types>...> {
  using overload_for<CRTP, Types>::do_visit...;
};

template <typename>
struct is_visit_method : std::false_type {};
template <typename T>
struct is_visit_method<visit1<T>> : std::true_type {};
template <typename T, typename U>
struct is_visit_method<visit2<T, U>> : std::true_type {};

template <typename CRTP>
using make_visit_overload_set =
    aa::insert_flatten_into<noexport::visit_overload_set,
                            decltype(aa::type_list<CRTP>{} +
                                     interface_of<CRTP>::template filtered_by<is_visit_method>())>;

template <typename T, typename... Ts>
using visit_one_with_all_others = aa::interface_alias<visit2<T, Ts>...>;

template <typename... Ts1, typename... Ts2>
auto visitor2_interface_impl(type_list<Ts1...>, type_list<Ts2...>)
    -> aa::interface_alias<visit_one_with_all_others<Ts1, Ts2...>...>;

// imitates behavior of overload set for all visit1/visit2 Methods in CRTP
template <typename CRTP>
struct full_visitor_interface {
  // makes 'make_visit_overload_set' in the function, so it not causes internal compiler errors on ALL
  // compilers, when trying to do so in CRTP context(when 'CRTP' type is not yet completed

  constexpr void visit(auto& v1) const {
    return make_visit_overload_set<CRTP>::do_visit(*static_cast<const CRTP*>(this), v1);
  }
  constexpr void visit(auto& v1, auto& v2) const {
    return make_visit_overload_set<CRTP>::do_visit(*static_cast<const CRTP*>(this), v1, v2);
  }
  constexpr void operator()(auto& value) const {
    return visit(value);
  }
  constexpr void operator()(auto& v1, auto& v2) const {
    return visit(v1, v2);
  }
};

}  // namespace noexport

template <type_list PossibleTypesFor1, type_list PossibleTypesFor2 = PossibleTypesFor1>
  requires(!std::is_same_v<decltype(PossibleTypesFor1), type_list<>> &&
           !std::is_same_v<decltype(PossibleTypesFor2), type_list<>>)
using visitor2_interface = decltype(noexport::visitor2_interface_impl(PossibleTypesFor1, PossibleTypesFor2));

template <typename Any, typename T, typename U>
struct plugin<Any, visit2<T, U>> {
  using type = noexport::full_visitor_interface<Any>;
};
template <typename Any, typename T>
struct plugin<Any, visit1<T>> {
  using type = noexport::full_visitor_interface<Any>;
};

// for using with visit
// usage example:
// std::visit(aa::matcher{
//  [](auto x, int) { ... },
//  [](int, float)  { ... }
// }, value);
//
template<typename... Foos>
struct matcher : Foos... {
  using Foos::operator()...;
};
template<typename... Foos>
matcher(Foos...) -> matcher<Foos...>;

}  // namespace aa
