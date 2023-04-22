#pragma once

// This very light header may be used without #include anyany.hpp
// for declaring anyany Methods

namespace aa {
struct error_if_you_forget_include_anyany_header_t {};
// this is 'forward declaration' for using macro without 'anyany' header
template <typename Method, typename = void>  // do not touch this param
constexpr inline error_if_you_forget_include_anyany_header_t invoke = {};

}  // namespace aa

#define AA_IMPL_REMOVE_PARENS(...) __VA_ARGS__
#define AA_IMPL_TOK_first(a, ...) a
#define AA_IMPL_TOK_all_except_first(a, ...) __VA_ARGS__
#define AA_IMPL_DO_GET_FIRST_TOKEN(out, x, ...) AA_IMPL_TOK_##out(x, __VA_ARGS__)
#define AA_IMPL_GET_FIRST_TOKEN(out, ...) AA_IMPL_DO_GET_FIRST_TOKEN(out, __VA_ARGS__)
#define AA_IMPL_SEPARATE_FIRST_TOK(...) (__VA_ARGS__),
#define AA_GET_TOKEN(out, ...) AA_IMPL_GET_FIRST_TOKEN(out, AA_IMPL_SEPARATE_FIRST_TOK __VA_ARGS__)
#define AA_IMPL_REMOVE_REQUIRESrequires(...) (__VA_ARGS__)
#define AA_IMPL_CONCAT_EXPAND(a, b) a##b
#define AA_IMPL_CONCAT(a, b) AA_IMPL_CONCAT_EXPAND(a, b)
#define AA_EXPAND(a) a
#define AA_GET_REQUIREMENT(...)                 \
  AA_EXPAND(AA_IMPL_REMOVE_PARENS AA_GET_TOKEN( \
      first, AA_IMPL_CONCAT(AA_IMPL_REMOVE_REQUIRES, AA_GET_TOKEN(all_except_first, __VA_ARGS__))))
#define AA_GET_ALL_AFTER_REQUIREMENT(...) \
  AA_GET_TOKEN(all_except_first,          \
               AA_IMPL_CONCAT(AA_IMPL_REMOVE_REQUIRES, AA_GET_TOKEN(all_except_first, __VA_ARGS__)))
#define AA_INJECT_SELF_IN_PARENS(...) (Self __VA_ARGS__)
#define AA_INJECT_INTERFACE_T_IN_PARENS(...) (int __VA_ARGS__)

// usage: anyany_method(<TRAIT_NAME>, (<SELF ARGUMENT>, <TRAIT_ARGS...>) requires(<EXPR>) -> <RETURN_TYPE>);
// where
// * <METHOD_NAME>    - is a name, which will be used later in invoke<NAME> or any_with<NAME...>
// * <SELF ARGUMENT> - is a '&' 'const &' or just nothing('') followed by self-argument name
// * <TRAIT_ARGS...> - set of Method parameters
// * <EXPR>          - what will Method do
// * <RETURN_TYPE>   - return type of Method. Must be non-dependent type
//                     (same for all 'Self' types, as for virtual functions)
//
// Notes:
//  * macro may use 'Self' type, which is decltype(<SELF_ARGUMENT>)
//  * ref/any_with, with Method created by this macro will have member function named NAME
//  * this macro supports 'template' on top of it, so you can easy create template Methods
//  * any_with/ref which uses Methods created by this macro are sfinae-friendly constructible
//    this means, constrctors do not exist in overload resolution if <EXPR> is invlaid for this type
//
//      anyany_method(foo, (& self, int i) requires(self.foo(x)) -> int)
//      // type 'int' do not have method .foo which accepts 'int' and return type is convertible to 'int'
//      static_assert(!is_constructible_v<any_with<foo>, int>);
//
//  * if you want to add requirement(like concept) on self type you can write it inside <EXPR>
// like
//      anyany_method(bar, (*something*) requires(std::enable_if_t<CONDITION>(), self + 5)->R);
//
// EXAMPLES:
//  Trait which accepts value by const&, invokes .foo(x + y).
//      * just simple Method, accepts 'self' by const&, returns self.foo(x, y)
//
//        anyany_method(foo, (const& self, int x, char y) requires(self.foo(x, y)) -> std::string);
//
//      * template Method, accepts self by non-const reference, invokes 'visitor_' with 'value'
//        returns void, so any return value from 'visitor_(value)' will be ignored even if it is not 'void'
//
//        template <typename T>
//        anyany_method(visit, (&visitor_, const T& value) requires(visitor_(value)) -> void);
//
//        using any_visitor = aa::any_with<visit<int>, visit<double>>;
//        any_visitor value = [](auto x) { std::cout << x; };
//        value = [](auto x) { foo(x); };
//
#define anyany_method(NAME, ...)                                                                            \
  struct NAME {                                                                                             \
   private:                                                                                                 \
    using method_t = NAME;                                                                                  \
    template <typename, typename, typename>                                                                 \
    struct make_plugin {};                                                                                  \
    template <typename CRTP, typename Ret, typename... Args, typename Method>                               \
    struct make_plugin<CRTP, Ret(int&, Args...), Method> {                                                  \
      using return_type = Ret;                                                                              \
      return_type NAME(Args... args) {                                                                      \
        return ::aa::invoke<Method>(*static_cast<CRTP*>(this), static_cast<Args&&>(args)...);               \
      }                                                                                                     \
    };                                                                                                      \
    template <typename CRTP, typename Ret, typename... Args, typename Method>                               \
    struct make_plugin<CRTP, Ret(const int&, Args...), Method> {                                            \
      using return_type = Ret;                                                                              \
      return_type NAME(Args... args) const {                                                                \
        return ::aa::invoke<Method>(*static_cast<const CRTP*>(this), static_cast<Args&&>(args)...);         \
      }                                                                                                     \
    };                                                                                                      \
    template <typename CRTP, typename Ret, typename... Args, typename Method>                               \
    struct make_plugin<CRTP, Ret(int, Args...), Method>                                                     \
        : make_plugin<CRTP, Ret(const int&, Args...), Method> {};                                           \
                                                                                                            \
   public:                                                                                                  \
    using signature_type = auto AA_EXPAND(AA_INJECT_INTERFACE_T_IN_PARENS AA_GET_TOKEN(first, __VA_ARGS__)) \
        AA_GET_ALL_AFTER_REQUIREMENT(__VA_ARGS__);                                                          \
    template <typename CRTP>                                                                                \
    using plugin = make_plugin<CRTP, signature_type, method_t>;                                             \
    using return_type = typename plugin<int>::return_type;                                                  \
                                                                                                            \
    template <typename Self>                                                                                \
    static auto do_invoke AA_EXPAND(AA_INJECT_SELF_IN_PARENS AA_GET_TOKEN(first, __VA_ARGS__))              \
        -> decltype(static_cast<return_type>(AA_GET_REQUIREMENT(__VA_ARGS__))) {                            \
      return static_cast<return_type>(AA_GET_REQUIREMENT(__VA_ARGS__));                                     \
    }                                                                                                       \
  }