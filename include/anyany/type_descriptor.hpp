#pragma once

/*
  HEADER SYNOPSIS:

  Basic tool for describing static and dynamic type
  * descriptor_t
  * descriptor_v
  * concept poly_traits - see declaration for info
  * anyany_poly_traits
*/

#include "noexport/type_descriptor_details.hpp"

#include <type_traits>
#include <string_view>

#include "noexport/file_begin.hpp"

namespace aa {

// Values of this type represent unique id of type,
// if descriptors compare equal, then types for are equal and vice versa
// describes 'void' type by default
struct descriptor_t {
 private:
  // uses name of type as C string by default (its fast to compare < and > which is usefull in 'visit_invoke'
  // And equality compare is faster too, because in many cases i can just check if pointers are equal(because
  // i compare type names) If type names are not available uses pointers
  using raw_desc_type = std::decay_t<decltype(noexport::descriptor<void>)>;
  raw_desc_type _value;

  friend struct std::hash<descriptor_t>;
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
#ifdef AA_HAS_CPP20
    if (!std::is_constant_evaluated()) {
      // cant compare pointers from different objects on compile time
      if (_value == other._value) [[unlikely]]
        return true;
    }
    return noexport::strcmp(_value, other._value) == std::strong_ordering::equal;
#else
    return noexport::strcmp(_value, other._value);
#endif
#endif
  }
#ifdef AA_HAS_CPP20
  AA_CONSTEXPR_TYPE_DESCRIPTOR std::strong_ordering operator<=>(const descriptor_t& other) const noexcept {
#ifdef AA_CANT_GET_TYPENAME
    return (uintptr_t)_value <=> (uintptr_t)other._value;
#else
    return noexport::strcmp(_value, other._value);
#endif
  }
#else
  constexpr bool operator!=(const descriptor_t& other) const noexcept {
    return !operator==(other);
  }
#endif
};
// always decays type
template <typename T>
constexpr descriptor_t descriptor_v{noexport::descriptor<std::decay_t<T>>};

// traits describe 'visit_invoke_fn' which types are polymorphic and which are not
// -- T::get_type_descriptor must return descriptor for static type of non-polymorphic types and dynamic type
// descripor for polymorphic types
// -- T::to_address returns void*/const void* to of runtime value for polymorphic
// types and just addressof(v) for non-polymorphic 'v'
#ifdef AA_HAS_CPP20
template <typename T>
concept poly_traits = requires(T val, int some_val) {
                        { val.get_type_descriptor(some_val) } -> std::same_as<descriptor_t>;
                        { val.to_address(some_val) };
                      };

#endif

AA_IS_VALID(is_polymorphic, typename T::aa_polymorphic_tag);
template <typename T>
using is_not_polymorphic = std::negation<is_polymorphic<T>>;

// these traits poly_ptr/ref/any_with are polymorphic values with dynamic type
// all other types considered as non-polymorphic
struct anyany_poly_traits {
  template <typename T>
  static constexpr descriptor_t get_type_descriptor(T&& x) noexcept {
    if constexpr (is_polymorphic<T>::value)
      return x.type_descriptor();
    else
      return descriptor_v<T>;
  }

 private:
  AA_IS_VALID(is_poly_ptr, decltype(std::declval<T>().raw()));

 public:
  // non-polymorphic types, returns /const/ void*
  template <typename T>
  static constexpr auto* to_address(T&& v) noexcept {
    if constexpr (is_polymorphic<T>::value) {
      if constexpr (is_poly_ptr<T>::value)
        return v.raw();
      else  // poly_ref/any_with and its inheritors
        return (&v).raw();
    } else {
      return static_cast<
          std::conditional_t<std::is_const_v<std::remove_reference_t<T>>, const void*, void*>>(
          std::addressof(v));
    }
  }
};

}  // namespace aa

namespace std {

template <>
struct hash<::aa::descriptor_t> {
  size_t operator()(const ::aa::descriptor_t& v) const noexcept {
#ifdef AA_CANT_GET_TYPENAME
    return hash<const void*>{}(v._value);
#else
    return hash<::std::string_view>{}(v._value);
#endif
  }
};

}  // namespace std
#include "noexport/file_end.hpp"
