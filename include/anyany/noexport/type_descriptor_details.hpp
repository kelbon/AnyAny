#pragma once

#ifdef AA_HAS_CPP20
#include <compare>
#endif

#include "file_begin.hpp"

namespace aa::noexport {

inline AA_CONSTEVAL_CPP20 bool starts_with(const char* part, const char* all) noexcept {
  for (; *part == *all && *part != '\0'; ++part, ++all)
    ;
  return *part == '\0';
}
// precondition: !!l && !!r
inline constexpr auto strcmp(const char* l, const char* r) noexcept {
  for (; *l == *r && *l != '\0'; ++l, ++r)
    ;
#ifdef AA_HAS_CPP20
  return *l <=> *r;
#else
  return *l == *r;
#endif
}

// name 'n' because need to reduce name size
template <typename T>
constexpr const char* n() {
#if defined(__GNUC__) && !defined(__clang__)
  return __PRETTY_FUNCTION__ + sizeof("consteval const char* aa::noexport::n() [with T =");
#elif defined(__clang__)
  return __PRETTY_FUNCTION__ + sizeof("const char *aa::noexport::n() [T =");
#elif defined(_MSC_VER)
  constexpr const char* res = __FUNCSIG__ + sizeof("const char *__cdecl aa::noexport::n<") - 1;
  // MSVC may return different names for SAME type if it was decalred as struct and implemented as class or
  // smth like
  if (starts_with("class", res))
    return res + sizeof("class");
  else if (starts_with("struct", res))
    return res + sizeof("struct");
  // fundamental types has no 'prefix'
  return res;
#else
  // it will not break dll support, because global variables addresses may differ only on windows, where
  // msvc/clang support type names getting
  return nullptr;
#endif
}

// do not use it explicitly, use aa::descriptor_v
#ifdef AA_CANT_GET_TYPENAME
template <typename T>
constexpr const void* descriptor = &descriptor<T>;
#else
template <typename T>
constexpr const char* descriptor = n<T>();
#endif

}  // namespace aa::noexport
#include "file_end.hpp"
