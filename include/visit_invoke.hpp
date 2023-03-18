#pragma once

/*
  HEADER SYNOPSIS:

  Multidispatching tools.

  * make_visit_invoke<Foos...> and its overload with return type deducting
      Performs overload resolution beetween Foos on runtime based on runtime types on input args
  Traits for invoke_match_fn:
    * anyany_poly_traits
    * std_variant_poly_traits
*/

#include <optional>      // for visit_invoke.resolve result

#include "type_descriptor.hpp"
#include "noexport/visit_invoke_details.hpp"

namespace aa {

// -- Result -- .resolve method will return std::optional<Result> by converting Foos results to this type
// -- Traits -- is a customization point, it describes what types visit_invoke_fn sees as polymorphic.
// example: you can add Traits for some your virtual/LLVM-type-id hierarchy
// (see anyany_poly_traits or std_variant_poly_traits as examples)
template <typename Result, poly_traits Traits, lambda_without_capture... Foos>
struct visit_invoke_fn {
 private:
  template <typename F>
  using traits = noexport::any_method_traits<F>;

  static constexpr std::size_t overload_count = sizeof...(Foos);
  static constexpr std::size_t args_count = std::max({traits<Foos>::args_count...});
  using result_type = Result;
  using key_type = std::array<descriptor_t, args_count>;
  using value_type = result_type (*)(const std::array<void*, args_count>&);

  // map of overloads sorted by type descriptors of decayed types in signature
  noexport::flat_map<key_type, value_type, overload_count> map;
#if __clang__
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wunknown-attributes"
#endif
  [[no_unique_address]] Traits poly_traits;
#if __clang__
#pragma clang diagnostic pop
#endif

  static_assert((... && std::is_convertible_v<typename traits<Foos>::result_type, result_type>),
                "invoke_match overloads result types must be convertible to result type");

  // verifies that atleast one of Foos may accept input args based on const arg or not
  template <typename... Voids>
  static consteval bool verify_const_correctness(Voids*...) noexcept {
#if defined(_MSC_VER) && !defined(__clang__)
    // really do not want support msvc, its full of bugs, cant compile
    return true;
#else
    std::array<bool, args_count> bitset;
    bitset.fill(false);
    std::ranges::copy(std::initializer_list<bool>{std::is_const_v<Voids>...}, bitset.begin());
    std::array<std::array<bool, args_count>, overload_count> all_foos_bitsets;
    for (auto& arr : all_foos_bitsets)
      arr.fill(true);
    std::apply([](auto&... arrs) { (..., std::ranges::copy(traits<Foos>::args_const, arrs.begin())); },
               all_foos_bitsets);
    auto may_accept_args = [&](auto& foo_args) {
      auto b = bitset.begin();
      for (bool may_accept_const : foo_args) {
        if (!may_accept_const && *b)
          return false;
        ++b;
      }
      return true;
    };
    return std::ranges::any_of(all_foos_bitsets, may_accept_args);
#endif
  }
  template <typename... Args>
  static bool runtime_const_correctness_check(auto* overload_winner) {
    auto make_value_constness_array_pair = []<typename F, typename... Ts>(std::type_identity<F>,
                                                                          type_list<Ts...>) {
      std::array<bool, args_count> value;
      value.fill(true);
      std::ranges::copy(traits<F>::args_const, value.begin());
      return std::pair(&match_invoker<F, Ts...>, value);
    };
    std::array m{
        make_value_constness_array_pair(std::type_identity<Foos>{}, typename traits<Foos>::all_args{})...};
    auto cit = std::ranges::find(m, overload_winner, [](auto& pair) { return pair.first; });
    auto& [_, can_accept_const] = *cit;
    constexpr std::array is_const_input_arg{
        std::is_const_v<std::remove_pointer_t<decltype(poly_traits.to_address(std::declval<Args>()))>>...};
    for (std::size_t i = 0; i < sizeof...(Args); ++i)
      if (is_const_input_arg[i] && !can_accept_const[i])
        return false;
    return true;
  }

  // type erases Foo signature, accepts void* array and invokes real Foo with them
  template <typename Foo, typename... Ts>
  static result_type match_invoker(const std::array<void*, args_count>& args) {
    std::array<void*, sizeof...(Ts)> necessary_args;
    std::ranges::copy_n(args.begin(), sizeof...(Ts), necessary_args.begin());
    return std::apply(
        [](auto*... void_ptrs) {
          return Foo{}(static_cast<Ts&&>(*reinterpret_cast<std::add_pointer_t<Ts>>(void_ptrs))...);
        },
        necessary_args);
  }

  template <typename F>
  static constexpr std::pair<key_type, value_type> make_key_value_pair_for() noexcept {
    return []<typename... Ts>(aa::type_list<Ts...>) {
      return std::pair{key_type{descriptor_v<Ts>...}, &match_invoker<F, Ts...>};
    }(typename traits<F>::all_args{});  // INVOKED HERE
  }

 public:
  constexpr explicit visit_invoke_fn(Traits t = Traits{}) noexcept
      : map({make_key_value_pair_for<Foos>()...}), poly_traits(std::move(t)) {
    struct signatures_must_be_unique_after_decay : traits<Foos>::decay_args... {
    } _;
    (void)_;
  }

  // returns nullopt if no such function in overload/match set
  // or winner overload cant accept args due const-qualifiers(when Unsafe == false)
  // If Unsafe == false, then const qualifiers on overload-winner function and input args will be checked,
  // otherwise its UB to pass const-qualified arg into non-const qualified function
  template <bool Unsafe = true, typename... Args>
    requires(sizeof...(Args) <= args_count)
  std::optional<result_type> resolve(Args&&... args) const noexcept((traits<Foos>::is_noexcept && ...)) {
    static_assert(((traits<Foos>::args_count == sizeof...(Args)) || ...),
                  "No overload with this count of arguments");
    static_assert(verify_const_correctness(decltype(poly_traits.to_address(args)){}...),
                  "No overload which can accept those arguments, you are trying "
                  "to pass const argument into non-const qualified reference");
    key_type key{poly_traits.get_type_descriptor(args)...};
    std::fill(key.begin() + sizeof...(Args), key.end(), descriptor_v<void>);
    auto it = map.find(key);
    if (it == map.end()) [[unlikely]]
      return std::nullopt;
    if constexpr (Unsafe) {
      assert(runtime_const_correctness_check<Args...>(*it) &&
             "Trying to pass const input arg into non-const function arg");
    } else {
      if (!runtime_const_correctness_check<Args...>(*it))
        return std::nullopt;
    }
    return (*it)({const_cast<void*>(poly_traits.to_address(args))...});
  }
};

// vist_invoke its like runtime overload resolution for Foos with converting result to Result
// Each in Foos may be function pointer or lambda without capture(with NON overloaded operator())
template <typename Result, auto... Foos, poly_traits Traits = anyany_poly_traits>
AA_CONSTEVAL inline auto make_visit_invoke(Traits t = Traits{}) {
  return visit_invoke_fn<Result, Traits, typename noexport::make_wrapper<Foos>::type...>{std::move(t)};
}
// deducts return type if all return types of Foos are same
template <auto... Foos, poly_traits Traits = anyany_poly_traits>
AA_CONSTEVAL inline auto make_visit_invoke(Traits t = Traits{}) {
  using result_type =
      typename decltype((noexport::any_method_traits<std::decay_t<decltype(Foos)>>{}, ...))::result_type;
  static_assert(
      (std::is_same_v<result_type,
                      typename noexport::any_method_traits<std::decay_t<decltype(Foos)>>::result_type> &&
       ...),
      "Return type cannot be deducted, because Foos have different return types, use second "
      "'make_visit_invoke' overload and specify return type by hands");
  return visit_invoke_fn<result_type, Traits, typename noexport::make_wrapper<Foos>::type...>{std::move(t)};
}

}  // namespace aa