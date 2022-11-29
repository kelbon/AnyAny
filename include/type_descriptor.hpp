#pragma once

/*
  HEADER SYNOPSIS:

  Basic tool for describing static and dynamic type
  * descriptor_t
  * descriptor_v

*/

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

}  // namespace aa