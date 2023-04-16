#pragma once
#if __cplusplus >= 202002
#define AA_HAS_CPP20
#define AA_CONCEPT(...) __VA_ARGS__
#define AA_IF_HAS_CPP20(...) __VA_ARGS__
#define AA_CONSTEVAL_CPP20 consteval
#include <compare>
#else
#define AA_IF_HAS_CPP20(...)
#define AA_CONSTEVAL_CPP20 constexpr // TODO use everywhere
#define AA_CONCEPT(...) typename
#endif

#include <string_view>

namespace aa::noexport {

// name 'n' because need to reduce name size
template <typename T>
constexpr const char* n() {
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

// precondition: !!a && !!b
inline constexpr auto strcmp(const char* l, const char* r) noexcept {
  for (; *l == *r && *l != '\0'; ++l, ++r)
    ;
#ifdef AA_HAS_CPP20
  return *l <=> *r;
#else
  return *l == *r;
#endif
}

}  // namespace aa::noexport