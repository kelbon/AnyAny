#pragma once

#include <compare>
#include <string_view>

namespace aa::noexport {

#define AA_NO_UNIQUE_ADDRESS [[no_unique_address]]

// name 'n' because need to reduce name size
template <typename T>
consteval const char* n() {
#if defined(__GNUC__) && !defined(__clang__)
  return __PRETTY_FUNCTION__ + sizeof("consteval const char* aa::noexport::n() [with T =");
#elif defined(__clang__)
  return __PRETTY_FUNCTION__ + sizeof("const char *aa::noexport::n() [T =");
#elif defined(_MSC_VER)
  const char* res = __FUNCSIG__ + sizeof("const char *__cdecl aa::noexport::n<") - 1;
  // MSVC may return different names for SAME type if it was decalred as struct and implemented as class or
  // smth like
  std::string_view s{res};
  if (s.starts_with("class"))
    res += sizeof("class");
  else if (s.starts_with("struct"))
    res += sizeof("struct");
  // fundamental types has no 'prefix'
  return res;
#else
#define AA_CANT_GET_TYPENAME
  // it will not break dll support, because global variables addresses may differ only on windows, where
  // msvc/clang support type names getting
  return nullptr;
#endif
}
// do not use it explicitly, use aa::descriptor_v
#ifdef AA_CANT_GET_TYPENAME
#define AA_CONSTEXPR
#define AA_CONSTEVAL
template <typename T>
constexpr const void* descriptor = &descriptor<T>;
#else
#define AA_CONSTEXPR constexpr
#define AA_CONSTEVAL consteval
template <typename T>
constexpr const char* descriptor = n<T>();
#endif

// inlinable(std::strcmp no)
// assumes a && b != nullptr
inline constexpr std::strong_ordering strcmp(const char* l, const char* r) noexcept {
  for (; *l == *r && *l != '\0'; ++l, ++r)
    ;
  return *l <=> *r;
}

}  // namespace aa::noexport