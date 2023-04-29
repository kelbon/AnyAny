#pragma once
#if __cplusplus >= 202002L
#define AA_HAS_CPP20
#define AA_CONCEPT(...) __VA_ARGS__
#define AA_IF_HAS_CPP20(...) __VA_ARGS__
#define AA_CONSTEVAL_CPP20 consteval
#include <compare>
#else
#define AA_IF_HAS_CPP20(...)
#define AA_CONSTEVAL_CPP20 constexpr
#define AA_CONCEPT(...) typename
#endif

#if defined(__GNUC__) || defined(__clang__)
#define AA_UNREACHABLE __builtin_unreachable()
#elif defined(_MSC_VER)
#define AA_UNREACHABLE __assume(false)
#else
#define AA_UNREACHABLE
#endif

#if defined(__GNUC__) || defined(__clang__)
#define AA_ALWAYS_INLINE __attribute__((always_inline)) inline
#elif defined(_MSC_VER)
#define AA_ALWAYS_INLINE __forceinline
#else
#define AA_ALWAYS_INLINE inline
#endif

// Yes, msvc do not support EBO which is already GUARANTEED by C++ standard for ~13 years
#ifdef _MSC_VER
#define AA_MSVC_EBO __declspec(empty_bases)
#else
#define AA_MSVC_EBO
#endif

// MSVC cannot compile 99% of good code without workarounds, it is just unusable compiler
#if defined(__GNUC__) || defined(__clang__)
#define AA_MSVC_WORKAROUND(...)
#else
#define AA_MSVC_WORKAROUND(...) __VA_ARGS__
#endif

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
#define AA_CANT_GET_TYPENAME
  // it will not break dll support, because global variables addresses may differ only on windows, where
  // msvc/clang support type names getting
  return nullptr;
#endif
}
// do not use it explicitly, use aa::descriptor_v
#ifdef AA_CANT_GET_TYPENAME
#define AA_CONSTEXPR
template <typename T>
constexpr const void* descriptor = &descriptor<T>;
#else
#define AA_CONSTEXPR constexpr
template <typename T>
constexpr const char* descriptor = n<T>();
#endif

}  // namespace aa::noexport