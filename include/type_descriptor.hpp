#pragma once

/*
  HEADER SYNOPSIS:

  Basic tool for describing static and dynamic type
  * descriptor_t
  * descriptor_v
  * concept poly_traits - see declaration for info
  * anyany_poly_traits
  * std_variant_poly_traits - examples of poly_traits
*/

#include <concepts>
#include <variant> // only for std_variant_poly_traits
#include <type_traits>

#include "noexport/type_descriptor_details.hpp"

namespace aa {

// Values of this type represent unique id of type,
// if descriptors compare equal, then types for are equal and vice versa
// describes 'void' type by default
struct descriptor_t {
 private:
  // uses name of type as C string by default (its fast to compare < and > which is usefull in 'visit_invoke'
  // And equality compare is faster too, because in many cases i can just check if pointers are equal(because
  // i compare type names) If type names are not available uses pointers
  using raw_desc_type = std::remove_cvref_t<decltype(noexport::descriptor<int>)>;
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
    return noexport::strcmp(_value, other._value) == std::strong_ordering::equal;
#endif
  }

  AA_CONSTEXPR std::strong_ordering operator<=>(const descriptor_t& other) const noexcept {
#ifdef AA_CANT_GET_TYPENAME
    return (uintptr_t)_value <=> (uintptr_t)other._value;
#else
    return noexport::strcmp(_value, other._value);
#endif
  }
};
// always decays type
template <typename T>
constexpr descriptor_t descriptor_v{noexport::descriptor<std::decay_t<T>>};

template <typename T>
concept lambda_without_capture =
    std::default_initializable<std::remove_cvref_t<T>> && requires {
                                                            { &std::remove_cvref_t<T>::operator() };
                                                          };

// traits describe 'visit_invoke_fn' which types are polymorphic and which are not
// -- T::get_type_descriptor must return descriptor for static type of non-polymorphic types and dynamic type
// descripor for polymorphic types
// -- T::to_address returns void*/const void* to of runtime value for polymorphic
// types and just addressof(v) for non-polymorphic 'v'
template <typename T>
concept poly_traits = requires(T val, int some_val) {
                        { val.get_type_descriptor(some_val) } -> std::same_as<descriptor_t>;
                        { val.to_address(some_val) };
                      };

template<typename T>
struct is_polymorphic {
  static constexpr inline bool value = requires(T v) {
                                         { v.type_descriptor() } -> std::same_as<descriptor_t>;
                                       };
};
template<typename T>
using is_not_polymorphic = std::negation<is_polymorphic<T>>;

// these traits poly_ptr/ref/any_with are polymorphic values with dynamic type
// all other types considered as non-polymorphic
struct anyany_poly_traits {
  // default case for non-polymorphic types like 'int', 'string' etc
  template <typename T>
  static constexpr descriptor_t get_type_descriptor(T&&) noexcept {
    return descriptor_v<T>;
  }
  template <typename T>
    requires(is_polymorphic<T>::value)  // case for /const/ poly_ptr/ref and any_with and its inheritors
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
    requires(is_polymorphic<T>::value)
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
    return std::visit([](const auto& v) { return reinterpret_cast<const void*>(std::addressof(v)); },
                      v);
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