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
#include <tuple>         // apply

#include "type_descriptor.hpp"
#include "noexport/visit_invoke_details.hpp"

#include "noexport/file_begin.hpp"

namespace aa {

// -- Result -- .resolve method will return std::optional<Result> by converting Foos results to this type
// -- Traits -- is a customization point, it describes what types visit_invoke_fn sees as polymorphic.
// example: you can add Traits for some your virtual/LLVM-type-id hierarchy
// (see anyany_poly_traits or std_variant_poly_traits as examples)
template <typename Result, typename Traits, typename... Foos>
struct visit_invoke_fn {
 private:
  template <typename F>
  using traits = noexport::visit_invoke_traits<F>;

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

  // verifies that atleast one of Foos may accept input args based on const arg or not
  template <typename... Voids>
  static AA_CONSTEVAL_CPP20 bool verify_const_correctness(Voids*...) noexcept {
#if defined(_MSC_VER) && !defined(__clang__)
    // really do not want support msvc, its full of bugs, cant compile
    return true;
#else
    std::array<bool, args_count> bitset;
    auto it = std::copy_n(std::initializer_list<bool>{std::is_const_v<Voids>...}.begin(), sizeof...(Voids),
                          bitset.begin());
    std::fill(it, bitset.end(), false);
    std::array<std::array<bool, args_count>, overload_count> all_foos_bitsets;
    for (auto& arr : all_foos_bitsets)
      arr.fill(true);
    std::apply(
        [](auto&... arrs) {
          (...,
           std::copy(std::begin(traits<Foos>::args_const), std::end(traits<Foos>::args_const), arrs.begin()));
        },
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
  template <typename... Args, typename X>
  static bool runtime_const_correctness_check(X* overload_winner) {
    auto make_value_constness_array_pair = []<typename F, typename... Ts>(noexport::type_identity<F>,
                                                                          type_list<Ts...>) {
      std::array<bool, args_count> value;
      value.fill(true);
      std::copy(std::begin(traits<F>::args_const), std::end(traits<F>::args_const), value.begin());
      return std::pair(&match_invoker<F, Ts...>, value);
    };
    std::array m{make_value_constness_array_pair(noexport::type_identity<Foos>{},
                                                 typename traits<Foos>::all_args{})...};
    auto cit =
        std::find_if(std::begin(m), std::end(m), [&](auto& pair) { return pair.first == overload_winner; });
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
    std::copy_n(args.begin(), sizeof...(Ts), necessary_args.begin());
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
  template <bool Unsafe = true, typename... Args,
            std::enable_if_t<(sizeof...(Args) <= args_count), int> = 0>
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
template <typename Result, typename Traits = anyany_poly_traits, typename... Foos>
constexpr inline auto make_visit_invoke(Foos... f) {
  return visit_invoke_fn<Result, Traits, Foos...>{};
}

}  // namespace aa
#include "noexport/file_end.hpp"