// for multiple-including (no pragma once)
// each header which uses noexport macros must include it and then include "file_end.hpp"
#if __cplusplus >= 202002L
#define AA_HAS_CPP20
#define AA_CONCEPT(...) __VA_ARGS__
#define AA_IF_HAS_CPP20(...) __VA_ARGS__
#define AA_CONSTEVAL_CPP20 consteval
#elif __cplusplus >= 201703L
#define AA_IF_HAS_CPP20(...)
#define AA_CONSTEVAL_CPP20 constexpr
#define AA_CONCEPT(...) typename
#elif defined(_MSC_VER)
#error anyanylib requires atleast C++17, but your compiler is MSVC, it sets incorrect value of '__cplusplus'\
and maybe for some reason anyany/CMakeLists do not set "/Zc:__cplusplus" flag(this forces MSVC to work as C++ standard says).\
Try set it manually (see anyany/CMakeLists.txt for example)
#else
#error anyanylib requires atleast C++17
#endif

#if defined(__GNUC__) || defined(__clang__)
#define AA_UNREACHABLE __builtin_unreachable()
#define AA_ALWAYS_INLINE __attribute__((always_inline)) inline
#elif defined(_MSC_VER)
#define AA_UNREACHABLE __assume(false)
#define AA_ALWAYS_INLINE __forceinline
#else
#define AA_UNREACHABLE (void)0
#define AA_ALWAYS_INLINE inline
#define AA_CANT_GET_TYPENAME
#endif

#ifdef AA_CANT_GET_TYPENAME
#define AA_CONSTEXPR_TYPE_DESCRIPTOR
#else
#define AA_CONSTEXPR_TYPE_DESCRIPTOR constexpr
#endif
// Yes, msvc do not support EBO which is already GUARANTEED by C++ standard for ~13 years
// MSVC cannot compile 99% of good code without workarounds, it is just unusable compiler
#ifdef _MSC_VER
#define AA_MSVC_EBO __declspec(empty_bases)
#else
#define AA_MSVC_EBO
#endif
#if defined(_MSC_VER) && !defined(__clang__)
#define AA_MSVC_WORKAROUND(...) __VA_ARGS__
#else
#define AA_MSVC_WORKAROUND(...)
#endif

#define AA_IS_VALID(STRUCT_NAME, EXPR)                                       \
  template <typename U>                                                      \
  struct STRUCT_NAME {                                                       \
   private:                                                                  \
    template <typename T, typename = EXPR>                                   \
    static constexpr bool check_fn_impl(int) {                               \
      return true;                                                           \
    }                                                                        \
    template <typename>                                                      \
    static constexpr bool check_fn_impl(...) {                               \
      return false;                                                          \
    }                                                                        \
                                                                             \
   public:                                                                   \
    static constexpr inline bool value = check_fn_impl<std::decay_t<U>>(0);  \
  }
#ifdef __clang__
#define ANYANY_LIFETIMEBOUND [[clang::lifetimebound]]
#else
#define ANYANY_LIFETIMEBOUND
#endif
