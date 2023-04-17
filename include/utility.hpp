#pragma once

#include "anyany.hpp"

#include <functional>  // std::invoke
#include <optional>    // for type_switch
#include <variant>     // only for std_variant_poly_traits

namespace aa::noexport {

template <typename PolyPtr, typename ResultT = void, AA_CONCEPT(poly_traits) Traits = anyany_poly_traits>
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
    if (auto* v = ::aa::any_cast<T, Traits>(value)) {
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

template <typename... M>
struct tta_list {};

template <typename X>
struct extract_methods {};

template <typename Alloc, size_t SooS, typename... Methods>
struct extract_methods<basic_any<Alloc, SooS, Methods...>>
    : extract_methods<typename basic_any<Alloc, SooS, Methods...>::ref> {};

template <typename... Methods, template <typename...> typename Template>
struct extract_methods<Template<Methods...>> : std::type_identity<tta_list<Methods...>> {};

consteval auto merge_impl(auto result, tta_list<>) {
  return result;
}
template <typename Head, typename... Methods, typename... Results>
consteval auto merge_impl(tta_list<Results...> out, tta_list<Head, Methods...>) {
  if constexpr (!vtable<Results...>::template has_method<Head>)
    return merge_impl(tta_list<Results..., Head>{}, tta_list<Methods...>{});
  else
    return merge_impl(out, tta_list<Methods...>{});
}

template <template <typename...> typename Out, typename... Methods>
Out<Methods...> insert_ttas(tta_list<Methods...>);

}  // namespace noexport

// ######################## ACTION type_switch for all polymorphic types ########################

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
namespace aa {
template <typename Result = void, AA_CONCEPT(poly_traits) Traits = anyany_poly_traits, typename... Methods>
constexpr auto type_switch(poly_ref<Methods...> p) noexcept {
  return noexport::type_switch_impl<poly_ptr<Methods...>, Result, Traits>{&p};
}
template <typename Result = void, AA_CONCEPT(poly_traits) Traits = anyany_poly_traits, typename... Methods>
constexpr auto type_switch(const_poly_ref<Methods...> p) noexcept {
  return noexport::type_switch_impl<const_poly_ptr<Methods...>, Result, Traits>{&p};
}

// 'extracts' Methods from T and U, 'emplaces' them in 'Out' template.
// removes duplicates, behaves as std::set merge
// example: merged_any_t<poly_ptr<A, B, C>, poly_ref<D, A, C>> == any_with<A, B, C, D>
template <typename T, typename U, template <typename...> typename Out = aa::any_with>
using merged_any_t = decltype(noexport::insert_ttas<Out>(noexport::merge_impl(
    typename noexport::extract_methods<T>::type{}, typename noexport::extract_methods<U>::type{})));

// Creates a Method from invocable object with given signature
// usage example:
// template<typename T>
// using Size = aa::from_callable<std::size_t(), std::ranges::size>::const_method<T>;
// using any_sized_range = aa::any_with<Size>;
template <typename Signature, auto>
struct from_callable {};

template <typename R, typename... Ts, auto Foo>
struct from_callable<R(Ts...), Foo> {
  template <typename T>
    requires(std::is_invocable_v<decltype(Foo), T&, Ts && ...> || std::is_same_v<interface_t, T>)
  struct method {
    static R do_invoke(T& self, Ts... args) {
      return Foo(self, static_cast<Ts&&>(args)...);
    }
  };
};
template <typename R, typename... Ts, auto Foo>
struct from_callable<R(Ts...) const, Foo> {
  template <typename T>
    requires(std::is_invocable_v<decltype(Foo), const T&, Ts && ...> || std::is_same_v<interface_t, T>)
  struct method {
    static R do_invoke(const T& self, Ts... args) {
      return Foo(self, static_cast<Ts&&>(args)...);
    }
  };
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
    return std::visit([](const auto& v) { return reinterpret_cast<const void*>(std::addressof(v)); }, v);
  }
  template <typename... Ts>
  static void* to_address(std::variant<Ts...>& v) noexcept {
    return std::visit([](auto& v) { return reinterpret_cast<void*>(std::addressof(v)); }, v);
  }
  template <typename... Ts>
  static void* to_address(std::variant<Ts...>&& v) noexcept {
    return std::visit([](auto&& v) { return reinterpret_cast<void*>(std::addressof(v)); }, v);
  }
};

}  // namespace aa
