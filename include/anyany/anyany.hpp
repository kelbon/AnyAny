#pragma once

/*
  HEADER SYNOPSIS:

  * polymorphic value (basic_any_with, any_with),
  * reference (const_poly_ref, poly_ref),
  * pointer (const_poly_ptr, poly_ptr),
  * cref/cptr aliases to const_poly_ref/const_poly_ptr
  * stateful::ref/cref
  Basic actions on polymorphic types:
  * any_cast<T>
  * invoke<Method>

*/

#include <cassert>    // assert
#include <exception>  // for any_cast(std::exception)
#include <utility>    // std::exchange

#include "type_descriptor.hpp"

#include "noexport/anyany_details.hpp"

#include "noexport/file_begin.hpp"

namespace aa {

// May be specialized for your Method or even for Any with some properties.
//
// Default version searches for Method::plugin<Any> or,
// if deducing this supported, typename Method::plugin
// or returns 'void' which means Method has no plugin(not a error)
template <typename Any, typename Method>
struct plugin : noexport::type_identity<decltype(noexport::get_plugin<Any, Method>(0))> {
  using aa_not_specialized_tag = int;
};

// ######################## compilt time information about Methods(Traits) ########################

// Searches for Method::signature_type if exist, or instanciates
// Method::do_invoke with 'aa::erased_self_t'
// Or, for pseudomethods, just uses typename Method::value_type
template <typename Method>
using method_traits = noexport::any_method_traits<noexport::signature_t<Method>>;

template <typename Method>
using result_t = typename method_traits<Method>::result_type;

template <typename Method>
using args_list = typename method_traits<Method>::args;

template <typename Method>
constexpr inline bool is_const_method_v = method_traits<Method>::is_const;

#ifdef AA_HAS_CPP20

namespace noexport {

// presented as function ptr in vtable
// searches signature as
//  * T::signature_type
//  * or (if not present) as decltype(&T::do_invoke<erased_self_t>)
//  * or, for pseudomethods, as T::value_type
template <typename T>
concept has_signature = requires { typename signature_t<T>; };

template <typename T>
concept signature_is_function = std::is_function_v<signature_t<T>>;

template <typename T>
concept empty_type = std::is_empty_v<T>;

}  // namespace noexport

// pseudomethod is just a value, which is stored in vtable
template <typename T>
concept pseudomethod =
    noexport::empty_type<T> && noexport::has_signature<T> && (!noexport::signature_is_function<T>) &&
    requires {
      typename T::value_type;
      // T::do_value<X>(),
      // where 'do_value' is a
      // static consteval function template and X
      // - some type for which 'do_value'
      // exist(not substitution failure)
    } && std::is_trivially_copyable_v<typename T::value_type>;  // for guarantee that poly_ref(specialization
                                                                // with 1 Method)/vtable is trivially copyable

template <typename T>
concept regular_method = noexport::empty_type<T> && noexport::has_signature<T> && noexport::signature_is_function<T>
    /* && requires { T::do_invoke<X>,}
    where 'do_invoke' is a static addressable function template and X - some type for which 'do_invoke'
    exist(not substitution failure)
    */
    ;

template <typename T>
concept method = regular_method<T> || pseudomethod<T>;
template <typename T>
concept const_method = method<T> && is_const_method_v<T>;
#define anyany_method_concept method
#else
#define anyany_method_concept typename
#endif

template <typename T, typename Method, typename = args_list<Method>>
struct invoker_for {};

template <typename T, typename Method, typename... Args>
struct invoker_for<T, Method, type_list<Args...>> {
  static result_t<Method> value(typename method_traits<Method>::type_erased_self_type self, Args&&... args) {
    using self_sample = typename method_traits<Method>::self_sample_type;
    // explicitly transfers <T> into do_invoke for case when 'self' is not
    // deductible(do_invoke(type_identity_t<Self>))
    if constexpr (std::is_lvalue_reference_v<self_sample>) {
      using real_self = std::conditional_t<is_const_method_v<Method>, const T*, T*>;
      return Method::template do_invoke<T>(*reinterpret_cast<real_self>(self), static_cast<Args&&>(args)...);
    } else if constexpr (std::is_copy_constructible_v<T>) {
      return Method::template do_invoke<T>(*reinterpret_cast<const T*>(self), static_cast<Args&&>(args)...);
    } else {
      static_assert(noexport::always_false<T>,
                    "You pass self by value and it is not a copy constructible... or by rvalue reference");
    }
  }
};

template <typename T, typename Method>
struct invoker_for<T, Method, noexport::aa_pseudomethod_tag> {
 private:
  struct value_getter {
    AA_CONSTEVAL_CPP20 result_t<Method> operator&() const {
      return Method::template do_value<T>();
    }
  };

 public:
  // uses operator& for producing values, because it easy for vtable creating
  static constexpr value_getter value = {};
};

AA_IS_VALID(is_any, typename T::base_any_type);

template <typename T>
using is_not_any = std::negation<is_any<T>>;

template <typename X, anyany_method_concept... Methods>
struct exist_for {
 private:
  template <typename T, typename Method>
  static auto check_fn(int) -> decltype(Method::template do_invoke<T>, std::true_type{});
  template <typename T, typename Method>  // for pseudomethods, must be consteval fn
  static auto check_fn(bool) -> decltype(Method::template do_value<T>(), std::true_type{});
  template <typename, typename>
  static auto check_fn(...) -> std::false_type;

 public:
  static constexpr inline bool value = (decltype(check_fn<X, Methods>(0))::value && ...);
};

// ######################## BASIC METHODS for basic_any ########################

struct destroy {
 private:
  template <typename T>
  static void destroy_fn(const void* p) noexcept {
    noexport::destroy_at(reinterpret_cast<const T*>(p));
  }
  static void noop_fn(const void*) noexcept {
  }

 public:
  using value_type = void (*)(const void*) noexcept;
  template <typename T>
  static AA_CONSTEVAL_CPP20 auto do_value() noexcept
      -> std::enable_if_t<std::is_nothrow_destructible_v<T>, value_type> {
    // makes address of noop_fn same for all trivial types in program, increase chance to be in processor
    // cache, this reduces binary size and improves performance
    if constexpr (std::is_trivially_destructible_v<T>)
      return &noop_fn;
    else
      return &destroy_fn<T>;
  }
};

struct move {
 private:
  static void noop_fn(void*, void*) noexcept {
  }

 public:
  using value_type = void (*)(void*, void*) noexcept;
  // invoked only for situatuion "move small from src to EMPTY dest" and only when type is nothrow move
  // constructible. Actually relocates
  template <typename T>
  static AA_CONSTEVAL_CPP20 auto do_value() noexcept
      -> std::enable_if_t<(std::is_move_constructible_v<T> && std::is_nothrow_destructible_v<T>),
                          value_type> {
    if constexpr (!noexport::is_fits_in_soo_buffer<T, static_cast<size_t>(-1)>)
      return nullptr;  // never called if value on heap
    // reduces count of functions as possible, because compiler cannot optimize them
    if constexpr (std::is_empty_v<T> && std::is_trivially_copyable_v<T> &&
                  std::is_trivially_destructible_v<T>)
      return &noop_fn;
    else if constexpr (std::is_trivially_copyable_v<T> && std::is_trivially_destructible_v<T>)
      return &noexport::relocate_trivial<sizeof(T)>;
    else
      return &noexport::relocate<T>;
  }
};

constexpr inline auto default_any_soos = 64 - 3 * sizeof(void*);

using default_allocator = std::allocator<std::byte>;

//  enables copy/copy assgin/move/move assign for any_with
//  enables 'materialize' for references
template <typename Alloc = default_allocator, size_t SooS = default_any_soos>
struct copy_with {
 private:
  template<typename T>
  static AA_CONSTEVAL_CPP20 auto* select_copy_fn() {
    if constexpr (!noexport::copy_requires_alloc<Alloc>()) {
      if constexpr (std::is_empty_v<T> && std::is_trivially_copyable_v<T> &&
                    noexport::is_fits_in_soo_buffer<T, SooS>)
        return &noexport::noop_copy_fn_empty_alloc;
      else if constexpr (std::is_trivially_copyable_v<T> && noexport::is_fits_in_soo_buffer<T, SooS>)
        return &noexport::trivial_copy_small_fn_empty_alloc<sizeof(T)>;
      else if constexpr (std::is_trivially_copyable_v<T>)
        return &noexport::trivial_copy_big_fn_empty_alloc<sizeof(T), Alloc>;
      else
        return &noexport::copy_fn_empty_alloc<T, Alloc, SooS>;
    } else {
      if constexpr (std::is_empty_v<T> && std::is_trivially_copyable_v<T> &&
                    noexport::is_fits_in_soo_buffer<T, SooS>)
        return &noexport::noop_copy_fn;
      else if constexpr (std::is_trivially_copyable_v<T> && noexport::is_fits_in_soo_buffer<T, SooS>)
        return &noexport::trivial_copy_small_fn<sizeof(T)>;
      else if constexpr (std::is_trivially_copyable_v<T>)
        return &noexport::trivial_copy_big_fn<sizeof(T), Alloc>;
      else
        return &noexport::copy_fn<T, Alloc, SooS>;
    }
  }
  using copy_fn_t = std::conditional_t<(!noexport::copy_requires_alloc<Alloc>()),
                                       void* (*)(const void*, void*), void* (*)(const void*, void*, void*)>;
 public:
  struct value_type {
    copy_fn_t copy_fn;
    void (*move_fn)(void* src, void* dest) noexcept;
  };
  template <typename T>
  static AA_CONSTEVAL_CPP20 auto do_value()
      -> std::enable_if_t<(std::is_copy_constructible_v<T> && std::is_move_constructible_v<T> &&
                           std::is_nothrow_destructible_v<T>),
                          value_type> {
    return value_type{select_copy_fn<T>(), move::do_value<T>()};
  }
};

using copy = copy_with<>;

namespace noexport {

auto get_any_copy_method(type_list<>) -> void;
template <typename A, size_t N, typename... Tail>
auto get_any_copy_method(type_list<copy_with<A, N>, Tail...>) -> copy_with<A, N>;
template <typename Head, typename... Tail>
auto get_any_copy_method(type_list<Head, Tail...>) {
  return get_any_copy_method(type_list<Tail...>{});
}
template <typename... Methods>
using some_copy_method = decltype(get_any_copy_method(type_list<Methods...>{}));

// copy_with contains 'move' function too, so if any has copy, then it has move too
template <typename... Methods>
constexpr inline bool has_move =
    !std::is_void_v<some_copy_method<Methods...>> || contains_v<move, Methods...>;

}  // namespace noexport

// enables std::hash specialization for polymorphic value and reference
struct hash {
  using signature_type = size_t(const erased_self_t&);
  template <typename T>
  static auto do_invoke(const T& self) -> decltype(size_t{std::hash<T>{}(std::declval<T>())}) {
    return size_t{std::hash<T>{}(self)};
  }
};

// ######################## VTABLE TYPE ########################

template <anyany_method_concept... Methods>
struct vtable : noexport::tuple<typename method_traits<Methods>::type_erased_signature_type...> {
 private:
  using base_t = noexport::tuple<typename method_traits<Methods>::type_erased_signature_type...>;

 public:
  using base_t::base_t;
  using base_t::operator=;

  template <typename Method>
  static inline constexpr bool has_method = noexport::contains_v<Method, Methods...>;

#ifdef AA_HAS_CPP20
 private:
  struct do_change_one {
    template <typename T>
    constexpr void operator()(const T& src, T& dest) {
      dest = src;
    }
    template <typename T, typename U>
    constexpr void operator()(const T&, const U&) {
    }
  };

 public:
  // sets new value to ALL values of 'Method' in this table
  template <method Method>
    requires(noexport::contains_v<Method, Methods...>)
  constexpr void change(typename method_traits<Method>::type_erased_signature_type new_value) noexcept(
      noexcept(new_value = new_value)) {
    [&]<size_t... Is>(std::index_sequence<Is...>) {
      (do_change_one{}(new_value, noexport::get<Is>(*this)), ...);
    }
    (std::index_sequence_for<Methods...>{});
  }
#endif
  template <typename Method, typename... Args>
  AA_ALWAYS_INLINE result_t<Method> invoke(Args&&... args) const {
    static_assert(has_method<Method>);
    return noexport::get<noexport::number_of_first<Method, Methods...>>(*this)(std::forward<Args>(args)...);
  }
  template <typename Method>
  AA_ALWAYS_INLINE constexpr result_t<Method> invoke() const
      noexcept(std::is_nothrow_copy_constructible_v<result_t<Method>>) {
    static_assert(std::is_same_v<args_list<Method>, noexport::aa_pseudomethod_tag>);
    return noexport::get<noexport::number_of_first<Method, Methods...>>(*this);
  }
};

template <size_t I, anyany_method_concept... Methods>
AA_ALWAYS_INLINE constexpr decltype(auto) get(vtable<Methods...>& v) noexcept {
  return noexport::get<I>(v);
}
template <size_t I, anyany_method_concept... Methods>
AA_ALWAYS_INLINE constexpr decltype(auto) get(const vtable<Methods...>& v) noexcept {
  return noexport::get<I>(v);
}

namespace noexport {

template <anyany_method_concept... ToMethods>
struct subtable_ptr_fn {
  template <anyany_method_concept... FromMethods>
  auto operator()(const vtable<FromMethods...>* ptr) const noexcept
      -> std::enable_if_t<noexport::has_subsequence(type_list<ToMethods...>{}, type_list<FromMethods...>{}),
                          const vtable<ToMethods...>*> {
    assert(ptr != nullptr);
    constexpr std::size_t Index =
        noexport::find_subsequence(type_list<ToMethods...>{}, type_list<FromMethods...>{});
    const auto* new_ptr = std::addressof(get<Index>(*ptr));
    return reinterpret_cast<const vtable<ToMethods...>*>(new_ptr);
  }
  template <anyany_method_concept... FromMethods>
  auto operator()(vtable<FromMethods...>* ptr) const noexcept
      -> std::enable_if_t<noexport::has_subsequence(type_list<ToMethods...>{}, type_list<FromMethods...>{}),
                          vtable<ToMethods...>*> {
    const vtable<FromMethods...>* cptr = ptr;
    return const_cast<vtable<ToMethods...>*>((*this)(cptr));
  }
  constexpr const vtable<ToMethods...>* operator()(const vtable<ToMethods...>* ptr) const noexcept {
    return ptr;
  }
  constexpr vtable<ToMethods...>* operator()(vtable<ToMethods...>* ptr) const noexcept {
    return ptr;
  }
};

}  // namespace noexport

// casts vtable to subvtable with smaller count of Methods if ToMethods are contigous subsequence of
// FromMethods For example vtable<M1,M2,M3,M4>* can be converted to vtable<M2,M3>*, but not to vtable<M2,M4>*
// precondition: vtable_ptr != nullptr
template <anyany_method_concept... ToMethods>
constexpr inline noexport::subtable_ptr_fn<ToMethods...> subtable_ptr = {};

// must be never named explicitly, use addr_vtable_for
template <typename T, anyany_method_concept... Methods>
constexpr vtable<Methods...> vtable_for{&invoker_for<T, Methods>::value...};

// always decays type
template <typename T, anyany_method_concept... Methods>
constexpr const vtable<Methods...>* addr_vtable_for = &vtable_for<std::decay_t<T>, Methods...>;

// ######################## poly_ref / poly_ptr  ########################

template <typename T>
using not_const_type = std::negation<std::is_const<T>>;

// takes friend's private fields (poly_ref/ptr/...)
struct mate {
 private:
  AA_IS_VALID(has_field_poly_, decltype(std::declval<T>().poly_));
  AA_IS_VALID(has_field_vtable_value, decltype(std::declval<T>().vtable_value));
 public:
  template <typename Friend>
  AA_ALWAYS_INLINE static constexpr auto& get_value_ptr(Friend& friend_) noexcept {
    if constexpr (has_field_poly_<Friend>::value)
      return get_value_ptr(friend_.poly_);
    else
      return friend_.value_ptr;
  }
  template <typename Friend>
  AA_ALWAYS_INLINE static constexpr auto* get_vtable_ptr(Friend& friend_) noexcept {
    if constexpr (has_field_poly_<Friend>::value)
      return get_vtable_ptr(friend_.poly_);
    else if constexpr (has_field_vtable_value<Friend>::value)
      return std::addressof(friend_.vtable_value);
    else
      return friend_.vtable_ptr;
  }
  template <typename Friend, typename Vtable>
  AA_ALWAYS_INLINE static constexpr void set_vtable_ptr(Friend& friend_, Vtable* vtable_ptr) {
    if constexpr (has_field_poly_<Friend>::value)
      set_vtable_ptr(friend_.poly_, vtable_ptr);
    else if constexpr (has_field_vtable_value<Friend>::value)
      friend_.vtable_value = *vtable_ptr;
    else
      friend_.vtable_ptr = vtable_ptr;
  }
  // stateful ref/cref
  template <typename Friend>
  AA_ALWAYS_INLINE static constexpr auto& get_vtable_value(Friend& friend_) noexcept {
    return friend_.vtable_value;
  }

  template <typename Friend>
  AA_ALWAYS_INLINE static constexpr auto& get_alloc(Friend& friend_) noexcept {
    return friend_.alloc;
  }
};

namespace noexport {

// if not specialized, then ::type searched,
// if specialized by user, then type itself used
template <typename Plugin, typename = typename Plugin::aa_not_specialized_tag>
auto get_plugin(int) -> typename Plugin::type;
template <typename Plugin>
auto get_plugin(...) -> Plugin;

}  // namespace noexport

template <typename Any, anyany_method_concept Method>
using plugin_t = decltype(noexport::get_plugin<plugin<Any, Method>>(0));

// creates type from which you can inherit from to get sum of Methods plugins
template <typename CRTP, anyany_method_concept... Methods>
using construct_interface = noexport::inheritor_without_duplicates_t<plugin_t<CRTP, Methods>...>;

namespace noexport {

template <typename... Methods>
struct vtable_view {
  using aa_polymorphic_tag = int;

 protected:
  friend struct aa::mate;
  const vtable<Methods...>* vtable_ptr = nullptr;

  constexpr vtable_view() noexcept = default;
  AA_ALWAYS_INLINE constexpr vtable_view(const vtable<Methods...>* vtable_ptr) noexcept
      : vtable_ptr(vtable_ptr) {
  }
};
// specialization for one Method, containing vtable itself
template <typename Method>
struct vtable_view<Method> {
  using aa_polymorphic_tag = int;

 private:
  friend struct aa::mate;
  vtable<Method> vtable_value{};

 public:
  constexpr vtable_view() noexcept = default;
  AA_ALWAYS_INLINE constexpr vtable_view(const vtable<Method>* vtable_ptr) noexcept
      : vtable_value(*vtable_ptr) {
  }
};

}  // namespace noexport

// non nullable non owner view to any type which satisfies Methods...
template <anyany_method_concept... Methods>
struct poly_ref : construct_interface<poly_ref<Methods...>, Methods...>, noexport::vtable_view<Methods...> {
 private:
  void* value_ptr = nullptr;

  using vtable_view_t = noexport::vtable_view<Methods...>;
  friend struct mate;
  template<anyany_method_concept...>
  friend struct poly_ptr;
  // only for poly_ptr implementation
  constexpr poly_ref() noexcept = default;

 public:
  // if user want rebind reference it must be explicit: REF = REF(value);
#define AA_TRIVIAL_COPY_EXPLICIT_REBIND(NAME) \
  NAME(const NAME&) = default;                \
  NAME(NAME&&) = default;                     \
  NAME& operator=(const NAME&) = default;     \
  NAME& operator=(NAME&&) = default;          \
  template <typename X>                       \
  void operator=(X&&) = delete

  AA_TRIVIAL_COPY_EXPLICIT_REBIND(poly_ref);

  // from mutable lvalue
  template <
      typename T,
      std::enable_if_t<std::conjunction_v<not_const_type<T>, is_not_polymorphic<T>, exist_for<T, Methods...>>,
                       int> = 0>
  constexpr poly_ref(T& value ANYANY_LIFETIMEBOUND) noexcept
      : vtable_view_t(addr_vtable_for<T, Methods...>), value_ptr(std::addressof(value)) {
  }

  template <
      typename... FromMethods,
      typename = std::void_t<decltype(subtable_ptr<Methods...>(std::declval<vtable<FromMethods...>*>()))>>
  constexpr poly_ref(poly_ref<FromMethods...> r) noexcept
      : vtable_view_t(subtable_ptr<Methods...>(mate::get_vtable_ptr(r))), value_ptr(mate::get_value_ptr(r)) {
  }
  // returns poly_ptr<Methods...>
  constexpr auto operator&() const noexcept;
};

// non nullable non owner view to any type which satisfies Methods...
// Note: do not extends lifetime
template <anyany_method_concept... Methods>
struct const_poly_ref : construct_interface<const_poly_ref<Methods...>, Methods...>, noexport::vtable_view<Methods...> {
 private:
  using vtable_view_t = noexport::vtable_view<Methods...>;
  const void* value_ptr = nullptr;

  friend struct mate;
  template <anyany_method_concept...>
  friend struct const_poly_ptr;
  // only for const_poly_ptr implementation
  constexpr const_poly_ref() noexcept = default;

 public:
  AA_TRIVIAL_COPY_EXPLICIT_REBIND(const_poly_ref);

  // from value
  template <typename T,
            std::enable_if_t<std::conjunction_v<is_not_polymorphic<T>, exist_for<T, Methods...>>, int> = 0>
  constexpr const_poly_ref(const T& value ANYANY_LIFETIMEBOUND) noexcept
      : vtable_view_t(addr_vtable_for<T, Methods...>), value_ptr(std::addressof(value)) {
  }
  // from non-const ref
  constexpr const_poly_ref(poly_ref<Methods...> r) noexcept
      : vtable_view_t(mate::get_vtable_ptr(r)), value_ptr(mate::get_value_ptr(r)) {
  }
  template <
      typename... FromMethods,
      typename = std::void_t<decltype(subtable_ptr<Methods...>(std::declval<vtable<FromMethods...>*>()))>>
  constexpr const_poly_ref(const_poly_ref<FromMethods...> r) noexcept
      : vtable_view_t(subtable_ptr<Methods...>(mate::get_vtable_ptr(r))), value_ptr(mate::get_value_ptr(r)) {
  }
  template <
      typename... FromMethods,
      typename = std::void_t<decltype(subtable_ptr<Methods...>(std::declval<vtable<FromMethods...>*>()))>>
  constexpr const_poly_ref(poly_ref<FromMethods...> r) noexcept
      : vtable_view_t(subtable_ptr<Methods...>(mate::get_vtable_ptr(r))), value_ptr(mate::get_value_ptr(r)) {
  }
  // returns const_poly_ptr<Methods...>
  constexpr auto operator&() const noexcept;
};

template <anyany_method_concept... Methods>
const_poly_ref(poly_ref<Methods...>) -> const_poly_ref<Methods...>;

// non owning pointer-like type, behaves like pointer to mutable abstract base type
template <anyany_method_concept... Methods>
struct poly_ptr {
  using aa_polymorphic_tag = int;

 private:
  poly_ref<Methods...> poly_;
  friend struct mate;

 public:
  constexpr poly_ptr() noexcept = default;
  constexpr poly_ptr(std::nullptr_t) noexcept : poly_ptr() {
  }
  constexpr poly_ptr& operator=(std::nullptr_t) noexcept ANYANY_LIFETIMEBOUND {
    *this = poly_ptr<Methods...>(nullptr);
    return *this;
  }
  // from mutable pointer
  // enable if removes ambiguity
  template <typename T,
            std::enable_if_t<std::conjunction_v<not_const_type<T>, is_not_any<T>, exist_for<T, Methods...>>,
                             int> = 0>
  constexpr poly_ptr(T* ptr ANYANY_LIFETIMEBOUND) noexcept {
    mate::set_vtable_ptr(*this, addr_vtable_for<T, Methods...>);
    mate::get_value_ptr(*this) = ptr;
  }
  // from mutable pointer to Any
  template <typename Any, std::enable_if_t<(std::conjunction_v<not_const_type<Any>, is_any<Any>> &&
                                            noexport::has_subsequence(type_list<Methods...>{},
                                                                      typename Any::methods_list{})),
                                           int> = 0>
  constexpr poly_ptr(Any* ptr ANYANY_LIFETIMEBOUND) noexcept {
    if (ptr != nullptr && ptr->has_value()) [[likely]] {
      mate::set_vtable_ptr(*this, subtable_ptr<Methods...>(mate::get_vtable_ptr(*ptr)));
      mate::get_value_ptr(*this) = mate::get_value_ptr(*ptr);
    }
  }
  template <typename... FromMethods,
            std::enable_if_t<noexport::has_subsequence(type_list<Methods...>{}, type_list<FromMethods...>{}),
                             int> = 0>
  constexpr poly_ptr(poly_ptr<FromMethods...> p) noexcept {
    if (p != nullptr) [[likely]] {
      mate::set_vtable_ptr(*this, subtable_ptr<Methods...>(mate::get_vtable_ptr(p)));
      mate::get_value_ptr(*this) = mate::get_value_ptr(p);
    }
  }
  // observers

  constexpr void* raw() const noexcept {
    return mate::get_value_ptr(*this);
  }
  constexpr bool has_value() const noexcept {
    return mate::get_value_ptr(*this) != nullptr;
  }
  constexpr bool operator==(std::nullptr_t) const noexcept {
    return !has_value();
  }
#ifndef AA_HAS_CPP20
  constexpr bool operator!=(std::nullptr_t) const noexcept {
    return !operator==(nullptr);
  }
#endif
  constexpr explicit operator bool() const noexcept {
    return has_value();
  }

  // access

  constexpr poly_ref<Methods...> operator*() const noexcept {
    assert(has_value());
    return poly_;
  }
  constexpr const poly_ref<Methods...>* operator->() const noexcept {
    return std::addressof(poly_);
  }
  // returns descriptor for void if *this == nullptr
  template <typename Ref = poly_ref<Methods...>>
  auto type_descriptor() const noexcept -> decltype(std::declval<Ref>().type_descriptor()) {
    return *this == nullptr ? descriptor_v<void> : (**this).type_descriptor();
  }
};

// non owning pointer-like type, behaves like pointer to CONST abstract base type
template <anyany_method_concept... Methods>
struct const_poly_ptr {
  using aa_polymorphic_tag = int;

 private:
  const_poly_ref<Methods...> poly_;
  friend struct mate;

 public:
  constexpr const_poly_ptr() noexcept = default;
  constexpr const_poly_ptr(std::nullptr_t) noexcept : const_poly_ptr() {
  }
  constexpr const_poly_ptr& operator=(std::nullptr_t) noexcept ANYANY_LIFETIMEBOUND {
    *this = const_poly_ptr<Methods...>(nullptr);
    return *this;
  }
  // from pointer to value
  template <typename T,
            std::enable_if_t<std::conjunction_v<is_not_polymorphic<T>, exist_for<T, Methods...>>, int> = 0>
  constexpr const_poly_ptr(const T* ptr ANYANY_LIFETIMEBOUND) noexcept {
    mate::set_vtable_ptr(*this, addr_vtable_for<T, Methods...>);
    mate::get_value_ptr(*this) = ptr;
  }
  // from pointer to Any
  template <typename Any,
            std::enable_if_t<(is_any<Any>::value &&
                              noexport::has_subsequence(typename Any::methods_list{}, type_list<Methods...>{})),
                             int> = 0>
  constexpr const_poly_ptr(const Any* p ANYANY_LIFETIMEBOUND) noexcept {
    if (p != nullptr && p->has_value()) [[likely]] {
      mate::set_vtable_ptr(*this, subtable_ptr<Methods...>(mate::get_vtable_ptr(*p)));
      mate::get_value_ptr(*this) = mate::get_value_ptr(*p);
    }
  }
  // from non-const poly pointer
  constexpr const_poly_ptr(poly_ptr<Methods...> p) noexcept {
    mate::set_vtable_ptr(*this, mate::get_vtable_ptr(p));
    mate::get_value_ptr(*this) = mate::get_value_ptr(p);
  }
  template <
      typename... FromMethods,
      typename = std::void_t<decltype(subtable_ptr<Methods...>(std::declval<vtable<FromMethods...>*>()))>>
  constexpr const_poly_ptr(const_poly_ptr<FromMethods...> p) noexcept {
    if (p == nullptr) [[unlikely]] {
      *this = nullptr;
      return;
    }
    mate::set_vtable_ptr(*this, subtable_ptr<Methods...>(mate::get_vtable_ptr(p)));
    mate::get_value_ptr(*this) = mate::get_value_ptr(p);
  }
  template <
      typename... FromMethods,
      typename = std::void_t<decltype(subtable_ptr<Methods...>(std::declval<vtable<FromMethods...>*>()))>>
  constexpr const_poly_ptr(poly_ptr<FromMethods...> p) noexcept
      : const_poly_ptr(const_poly_ptr<FromMethods...>{p}) {
  }
  // observers

  constexpr const void* raw() const noexcept {
    return mate::get_value_ptr(*this);
  }
  constexpr bool has_value() const noexcept {
    return raw() != nullptr;
  }
  constexpr bool operator==(std::nullptr_t) const noexcept {
    return !has_value();
  }
#ifndef AA_HAS_CPP20
  constexpr bool operator!=(std::nullptr_t) const noexcept {
    return !operator==(nullptr);
  }
#endif
  constexpr explicit operator bool() const noexcept {
    return has_value();
  }

  // access

  constexpr const_poly_ref<Methods...> operator*() const noexcept {
    return poly_;
  }
  constexpr const const_poly_ref<Methods...>* operator->() const noexcept {
    return std::addressof(poly_);
  }

  // returns descriptor for void if *this == nullptr
  template <typename Ref = const_poly_ref<Methods...>>
  auto type_descriptor() const noexcept -> decltype(std::declval<Ref>().type_descriptor()) {
    return *this == nullptr ? descriptor_v<void> : (**this).type_descriptor();
  }
};

template <anyany_method_concept... Methods>
const_poly_ptr(poly_ptr<Methods...>) -> const_poly_ptr<Methods...>;

template <anyany_method_concept... Methods>
constexpr auto poly_ref<Methods...>::operator&() const noexcept {
  poly_ptr<Methods...> result;
  mate::set_vtable_ptr(result, mate::get_vtable_ptr(*this));
  mate::get_value_ptr(result) = value_ptr;
  return result;
}
template <anyany_method_concept... Methods>
constexpr auto const_poly_ref<Methods...>::operator&() const noexcept {
  const_poly_ptr<Methods...> result;
  mate::set_vtable_ptr(result, mate::get_vtable_ptr(*this));
  mate::get_value_ptr(result) = value_ptr;
  return result;
}

// ######################## CONST POINTER CAST ###################

template <typename... Methods>
constexpr poly_ptr<Methods...> const_pointer_cast(const_poly_ptr<Methods...> from) noexcept {
  poly_ptr<Methods...> result;
  mate::set_vtable_ptr(result, mate::get_vtable_ptr(from));
  mate::get_value_ptr(result) = const_cast<void*>(mate::get_value_ptr(from));
  return result;
}

// ###################### STATEFULL REF/CREF ####################

namespace stateful {

// stores vtable in the reference, so better cache locality.
// for example it may be used as function arguments with 1-2 Methods
template <anyany_method_concept... Methods>
struct ref : construct_interface<::aa::stateful::ref<Methods...>, Methods...> {
  using aa_polymorphic_tag = int;

 private:
  void* value_ptr;
#if __clang__
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wunknown-attributes"
#endif
  [[no_unique_address]] vtable<Methods...> vtable_value;
#if __clang__
#pragma clang diagnostic pop
#endif
  friend struct ::aa::mate;
  template <anyany_method_concept...>
  friend struct cref;

  constexpr ref() noexcept = default;

  template <typename... Methods2,
            std::enable_if_t<(noexport::contains_v<Methods, Methods2...> && ...), int> = 0>
  static constexpr ref<Methods...> FOO(poly_ref<Methods2...> r) {
    ref result;
    result.value_ptr = mate::get_value_ptr(r);
    result.vtable_value =
        vtable<Methods...>{get<noexport::number_of_first<Methods, Methods2...>>(*mate::get_vtable_ptr(r))...};
    return result;
  }

  template <typename... Methods2,
            std::enable_if_t<(noexport::contains_v<Methods, Methods2...> && ...), int> = 0>
  static constexpr ref<Methods...> FOO(const stateful::ref<Methods2...>& r) {
    ref result;
    result.value_ptr = mate::get_value_ptr(r);
    result.vtable_value = vtable<Methods...>{
        get<noexport::number_of_first<Methods, Methods2...>>(mate::get_vtable_value(r))...};
    return result;
  }

 public:
  AA_TRIVIAL_COPY_EXPLICIT_REBIND(ref);

  template <
      typename T,
      std::enable_if_t<std::conjunction_v<not_const_type<T>, is_not_polymorphic<T>, exist_for<T, Methods...>>,
                       int> = 0>
  constexpr ref(T& value ANYANY_LIFETIMEBOUND) noexcept
      : value_ptr(std::addressof(value)),
        vtable_value(vtable<Methods...>{&invoker_for<std::decay_t<T>, Methods>::value...}) {
  }

  constexpr ref(poly_ref<Methods...> r) noexcept
      : value_ptr(mate::get_value_ptr(r)), vtable_value(*mate::get_vtable_ptr(r)) {
  }
  // accepts poly_ref and stateful::ref with more Methods
  // 'FOO' is a hack, because compilers really bad with deducing guides in this case
  // (not fixable now)
  template <typename X, std::enable_if_t<is_polymorphic<X>::value, int> = 0,
            std::void_t<decltype(FOO(std::declval<X>()))>* = nullptr>
  constexpr ref(const X& x) : ref(FOO(x)) {
  }
  constexpr poly_ref<Methods...> get_view() const noexcept ANYANY_LIFETIMEBOUND {
    poly_ptr<Methods...> ptr;
    mate::set_vtable_ptr(ptr, &vtable_value);
    mate::get_value_ptr(ptr) = value_ptr;
    return *ptr;
  }
};

// stores vtable in the reference, so better cache locality.
// for example it may be used as function arguments with 1-2 Methods
// also can reference arrays and functions without decay
template <anyany_method_concept... Methods>
struct cref : construct_interface<::aa::stateful::cref<Methods...>, Methods...> {
  using aa_polymorphic_tag = int;

 private:
  const void* value_ptr;
#if __clang__
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wunknown-attributes"
#endif
  [[no_unique_address]] vtable<Methods...> vtable_value;
#if __clang__
#pragma clang diagnostic pop
#endif
  friend struct ::aa::mate;
  constexpr cref() noexcept = default;

 public:
  AA_TRIVIAL_COPY_EXPLICIT_REBIND(cref);
#undef AA_TRIVIAL_COPY_EXPLICIT_REBIND

  template <typename T,
            std::enable_if_t<std::conjunction_v<is_not_polymorphic<T>, exist_for<T, Methods...>>, int> = 0>
  constexpr cref(const T& value ANYANY_LIFETIMEBOUND) noexcept
      : value_ptr(std::addressof(value)), vtable_value{&invoker_for<std::decay_t<T>, Methods>::value...} {
  }

  constexpr cref(const_poly_ref<Methods...> r) noexcept
      : value_ptr(mate::get_value_ptr(r)), vtable_value(*mate::get_vtable_ptr(r)) {
  }

  constexpr cref(poly_ref<Methods...> r) noexcept : cref(const_poly_ref(r)) {
  }

  constexpr cref(const stateful::ref<Methods...>& r) noexcept
      : value_ptr(mate::get_value_ptr(r)), vtable_value(mate::get_vtable_value(r)) {
  }

 private:
  template <typename... Methods2,
            std::enable_if_t<(noexport::contains_v<Methods, Methods2...> && ...), int> = 0>
  static constexpr cref<Methods...> FOO(const stateful::cref<Methods2...>& r) {
    cref result;
    result.value_ptr = mate::get_value_ptr(r);
    result.vtable_value = vtable<Methods...>{
        get<noexport::number_of_first<Methods, Methods2...>>(mate::get_vtable_value(r))...};
    return result;
  }

  template <typename... Methods2,
            std::enable_if_t<(noexport::contains_v<Methods, Methods2...> && ...), int> = 0>
  static constexpr cref<Methods...> FOO(const stateful::ref<Methods2...>& r) {
    return FOO(stateful::cref<Methods2...>(r));
  }

  template <typename... Methods2,
            std::enable_if_t<(noexport::contains_v<Methods, Methods2...> && ...), int> = 0>
  static constexpr cref<Methods...> FOO(poly_ref<Methods2...> r) {
    return stateful::ref<Methods...>::FOO(r);
  }

  template <typename... Methods2,
            std::enable_if_t<(noexport::contains_v<Methods, Methods2...> && ...), int> = 0>
  static constexpr cref<Methods...> FOO(const_poly_ref<Methods2...> p) {
    return FOO(*aa::const_pointer_cast(&p));
  }

 public:
  // accepts poly_ref/const_poly_ref and stateful::ref/cref with more Methods, effectivelly converts
  // 'FOO' is a hack, because compilers really bad with deducing guides in this case
  // (not fixable now)
  template <typename X, std::enable_if_t<is_polymorphic<X>::value, int> = 0,
            std::void_t<decltype(FOO(std::declval<X>()))>* = nullptr>
  constexpr cref(const X& x) : cref(FOO(x)) {
  }
  constexpr const_poly_ref<Methods...> get_view() const noexcept ANYANY_LIFETIMEBOUND {
    const_poly_ptr<Methods...> ptr;
    mate::set_vtable_ptr(ptr, &vtable_value);
    mate::get_value_ptr(ptr) = value_ptr;
    return *ptr;
  }
};

}  // namespace stateful

// ######################## ACTION invoke ########################

namespace noexport {

template <typename Method, typename... Args>
struct invoke_fn<Method, type_list<Args...>> {
#define AA_VTABLE_CALL(X, VTABLE, ARGS, OP) \
  mate::get_##VTABLE(X) OP template invoke<Method>(mate::get_value_ptr(X), static_cast<Args&&>(ARGS)...);

  // FOR ANY

  template <typename U, std::enable_if_t<is_any<U>::value, int> = 0>
  result_t<Method> operator()(U&& any, Args... args) const {
    return AA_VTABLE_CALL(any, vtable_ptr, args, ->);
  }

  template <typename U, std::enable_if_t<is_any<U>::value, int> = 0>
  result_t<Method> operator()(const U& any, Args... args) const {
    static_assert(is_const_method_v<Method>);
    return AA_VTABLE_CALL(any, vtable_ptr, args, ->);
  }

  // FOR POLYMORPHIC REF

  template <typename... Methods>
  result_t<Method> operator()(poly_ref<Methods...> r, Args... args) const {
    return AA_VTABLE_CALL(r, vtable_ptr, args, ->);
  }
  template <typename... Methods>
  result_t<Method> operator()(const_poly_ref<Methods...> ptr, Args... args) const {
    static_assert(is_const_method_v<Method>);
    return AA_VTABLE_CALL(ptr, vtable_ptr, args, ->);
  }

  // FOR STATEFUL REF

  template <typename... Methods>
  result_t<Method> operator()(const stateful::ref<Methods...>& r, Args... args) const {
    return AA_VTABLE_CALL(r, vtable_value, args, .);
  }
  template <typename... Methods>
  result_t<Method> operator()(const stateful::cref<Methods...>& r, Args... args) const {
    static_assert(is_const_method_v<Method>);
    return AA_VTABLE_CALL(r, vtable_value, args, .);
  }
#undef AA_VTABLE_CALL
};

template <typename Method>
struct invoke_fn<Method, noexport::aa_pseudomethod_tag> {
  template <typename T, std::enable_if_t<is_polymorphic<T>::value, int> = 0>
  [[nodiscard]] constexpr result_t<Method> operator()(const T& value) const noexcept {
    return mate::get_vtable_ptr(value)->template invoke<Method>();
  }
};

}  // namespace noexport

// for cases, when you sure any has value (so UB if !has_value), compilers bad at optimizations(
template <anyany_method_concept Method>
constexpr inline noexport::invoke_fn<Method, args_list<Method>> invoke = {};

// ######################## BASIC_ANY ########################

// when used in ctor this tag forces anyany allocate memory, so pointers to value(poly_ptr/ref)
// will not invalidated after anyany move
struct force_stable_pointers_t {
  explicit force_stable_pointers_t() = default;
};
constexpr inline force_stable_pointers_t force_stable_pointers{};

struct allocator_arg_t {
  explicit allocator_arg_t() = default;
};
constexpr inline allocator_arg_t allocator_arg{};

// produces compilation error if basic_any with this allocator tries to allocate
struct unreachable_allocator {
  using value_type = std::byte;

  template <typename>
  using rebind = unreachable_allocator;

  template <typename X = void>
  [[noreturn]] std::byte* allocate(size_t) const noexcept {
    static_assert(noexport::always_false<X>, "must never allocate");
    AA_UNREACHABLE;
  }
  [[noreturn]] void deallocate(void*, size_t) {
    AA_UNREACHABLE;
  }
};

// SooS == Small Object Optimization Size
// strong exception guarantee for all constructors and assignments,
// emplace<T> - *this is empty if exception thrown
// for alloc not all fancy pointers supported and construct / destroy not throught alloc
template <typename Alloc, size_t SooS, typename... Methods>
struct basic_any : construct_interface<basic_any<Alloc, SooS, Methods...>, Methods...> {
  using aa_polymorphic_tag = int;

 private:
  const vtable<Methods...>* vtable_ptr = nullptr;
  void* value_ptr = data;
  union {
    alignas(std::max_align_t) std::byte data[SooS];
    size_t size_allocated;  // stored when value allocated
  };
#if __clang__
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wunknown-attributes"
#endif
  [[no_unique_address]] Alloc alloc;
#if __clang__
#pragma clang diagnostic pop
#endif

  // invariant of basic_any - it is always in one of those states:
  // empty - has_value() == false, memory_allocated == false
  // small - has_value() == true, memory_allocated == false, MOVE IS NOEXCEPT
  // big   - has_value() == true, memory_allocated == true,

  // guarantees that small is nothrow movable(for noexcept move ctor/assign)
  template <typename T>
  static inline constexpr bool any_is_small_for = noexport::is_fits_in_soo_buffer<T, SooS>;

  template <typename T, typename ForceAllocate = void, typename... Args>
  void emplace_in_empty(Args&&... args) {
    if constexpr (any_is_small_for<T> && std::is_void_v<ForceAllocate>) {
      noexport::construct_at(reinterpret_cast<T*>(value_ptr), std::forward<Args>(args)...);
    } else {
      constexpr size_t allocation_size = sizeof(T);
      void* old_value_ptr = std::exchange(value_ptr, alloc.allocate(allocation_size));
      if constexpr (std::is_nothrow_constructible_v<T, Args&&...>) {
        noexport::construct_at(reinterpret_cast<T*>(value_ptr), std::forward<Args>(args)...);
      } else {
        try {
          noexport::construct_at(reinterpret_cast<T*>(value_ptr), std::forward<Args>(args)...);
        } catch (...) {
          alloc.deallocate(reinterpret_cast<alloc_pointer_type>(value_ptr), allocation_size);
          value_ptr = old_value_ptr;
          throw;
        }
      }
      size_allocated = allocation_size;
    }
    vtable_ptr = addr_vtable_for<T, Methods...>;
  }

  using alloc_traits = std::allocator_traits<Alloc>;
  using alloc_pointer_type = typename alloc_traits::pointer;

  friend struct mate;
  template<typename, size_t, typename...>
  friend struct basic_any;

 public:
  template <typename Method>
  static constexpr bool has_method = noexport::contains_v<Method, Methods...>;
  static constexpr bool has_copy = has_method<copy_with<Alloc, SooS>>;
  static constexpr bool has_move = noexport::has_move<Methods...>;

  static_assert(noexport::is_byte_like_v<typename alloc_traits::value_type>);
  static_assert(std::is_nothrow_copy_constructible_v<Alloc>, "C++ Standard requires it");
  using base_any_type = basic_any;
  using methods_list = ::aa::type_list<Methods...>;

 private:
  template <typename... Methods1>
  struct remove_utility_methods {
    // only way to create basic_any without first 'destroy' method - aa::materialize
    using ptr = poly_ptr<Methods1...>;
    using const_ptr = const_poly_ptr<Methods1...>;
    using ref = poly_ref<Methods1...>;
    using const_ref = const_poly_ref<Methods1...>;
  };
  template <typename... Methods1>
  struct remove_utility_methods<destroy, Methods1...> : remove_utility_methods<Methods1...> {};

  using purified = remove_utility_methods<Methods...>;

 public:
  using ptr = typename purified::ptr;
  using ref = typename purified::ref;
  using cptr = typename purified::const_ptr;
  using cref = typename purified::const_ref;

  using const_ptr = cptr;
  using const_ref = cref;
  // aliases without 'destroy' for usage like any_with<a, b, c>::ref
  // but operator& return with 'destroy' method(implicitly converitble anyway)
  constexpr poly_ptr<Methods...> operator&() noexcept ANYANY_LIFETIMEBOUND {
    return {this};
  }
  constexpr const_poly_ptr<Methods...> operator&() const noexcept ANYANY_LIFETIMEBOUND {
    return {this};
  }
  constexpr basic_any() = default;

  AA_IF_HAS_CPP20(constexpr) ~basic_any() {
    if (has_value())
      destroy_value();
  }

  // basic_any copy/move stuff

  basic_any(const basic_any& other) AA_IF_HAS_CPP20(requires(has_copy))
      : alloc(alloc_traits::select_on_container_copy_construction(other.alloc)) {
    if (!other.has_value())
      return;
    if constexpr (!noexport::copy_requires_alloc<Alloc>()) {
      value_ptr = invoke<copy_with<Alloc, SooS>>(other).copy_fn(other.value_ptr, value_ptr);
    } else {
      value_ptr =
          invoke<copy_with<Alloc, SooS>>(other).copy_fn(other.value_ptr, value_ptr, std::addressof(alloc));
    }
    vtable_ptr = other.vtable_ptr;
  }

  [[nodiscard]] Alloc get_allocator() const noexcept {
    return alloc;
  }
  // postcondition: other do not contain a value after move
  basic_any(basic_any&& other) noexcept AA_IF_HAS_CPP20(requires(has_move)) : alloc(std::move(other.alloc)) {
    move_value_from(std::move(other));
  }
  basic_any& operator=(basic_any&& other) noexcept ANYANY_LIFETIMEBOUND AA_IF_HAS_CPP20(requires(has_move)) {
    // nocheck about this == &other
    // because after move assign other by C++ standard in unspecified(valid) state
    reset();
    if constexpr (!alloc_traits::is_always_equal::value &&
                  alloc_traits::propagate_on_container_move_assignment::value) {
      if (alloc != other.alloc)
        alloc = std::move(other.alloc);
    }
    move_value_from(std::move(other));
    return *this;
  }

  basic_any& operator=(const basic_any& other) ANYANY_LIFETIMEBOUND AA_IF_HAS_CPP20(requires(has_copy)) {
    basic_any value = other;
    if constexpr (!alloc_traits::is_always_equal::value &&
                  alloc_traits::propagate_on_container_copy_assignment::value) {
      if (alloc != other.alloc) {
        reset();  // my alloc will be destroyed so i need to deallocate(while i can)
        alloc = other.alloc;
      }
    }
    *this = std::move(value);
    return *this;
  }
  // has strong exception guarantee
  // you can use .emplace() without exception guarantees
  // and without any requirements
  template <typename V, typename Self = basic_any,
            std::enable_if_t<(std::conjunction_v<is_not_polymorphic<V>, exist_for<V, Methods...>> &&
                              (Self::has_move || (std::is_nothrow_constructible_v<std::decay_t<V>, V&&> &&
                                                  any_is_small_for<std::decay_t<V>>))),
                             int> = 0>
  basic_any& operator=(V&& val) ANYANY_LIFETIMEBOUND {
    if constexpr (std::is_nothrow_constructible_v<std::decay_t<V>, V&&> &&
                  any_is_small_for<std::decay_t<V>>) {
      reset();
      emplace_in_empty<std::decay_t<V>>(std::forward<V>(val));
    } else {
      *this = basic_any{std::forward<V>(val)};
    }
    return *this;
  }

  // making from any other type

  // postconditions : has_value() == true, *this is empty if exception thrown
  template <typename T, typename... Args, std::enable_if_t<exist_for<T, Methods...>::value, int> = 0>
  std::decay_t<T>& emplace(Args&&... args) noexcept(
      std::is_nothrow_constructible_v<std::decay_t<T>, Args&&...>&& any_is_small_for<std::decay_t<T>>) ANYANY_LIFETIMEBOUND {
    reset();
    emplace_in_empty<std::decay_t<T>>(std::forward<Args>(args)...);
    return *reinterpret_cast<std::decay_t<T>*>(value_ptr);
  }
  template <typename T, typename U, typename... Args,
            std::enable_if_t<exist_for<T, Methods...>::value, int> = 0>
  std::decay_t<T>& emplace(std::initializer_list<U> list, Args&&... args) noexcept(
      std::is_nothrow_constructible_v<std::decay_t<T>, std::initializer_list<U>, Args&&...>&&
          any_is_small_for<std::decay_t<T>>) ANYANY_LIFETIMEBOUND {
    reset();
    emplace_in_empty<std::decay_t<T>>(list, std::forward<Args>(args)...);
    return *reinterpret_cast<std::decay_t<T>*>(value_ptr);
  }
  template <typename T, typename... Args, std::enable_if_t<exist_for<T, Methods...>::value, int> = 0>
  basic_any(std::in_place_type_t<T>, Args&&... args) noexcept(
      std::is_nothrow_constructible_v<std::decay_t<T>, Args&&...>&& any_is_small_for<std::decay_t<T>>) {
    emplace_in_empty<std::decay_t<T>>(std::forward<Args>(args)...);
  }
  template <typename T, typename U, typename... Args,
            std::enable_if_t<exist_for<T, Methods...>::value, int> = 0>
  basic_any(std::in_place_type_t<T>, std::initializer_list<U> list, Args&&... args) noexcept(
      std::is_nothrow_constructible_v<std::decay_t<T>, std::initializer_list<U>, Args&&...>&&
          any_is_small_for<std::decay_t<T>>) {
    emplace_in_empty<std::decay_t<T>>(list, std::forward<Args>(args)...);
  }
  template <typename T,
            std::enable_if_t<std::conjunction_v<is_not_polymorphic<T>, exist_for<T, Methods...>>, int> = 0>
  basic_any(T&& value) noexcept(
      std::is_nothrow_constructible_v<std::decay_t<T>, T&&>&& any_is_small_for<std::decay_t<T>>)
      : basic_any(std::in_place_type<std::decay_t<T>>, std::forward<T>(value)) {
  }
  constexpr basic_any(aa::allocator_arg_t, Alloc alloc) noexcept : alloc(std::move(alloc)) {
  }
  template <typename T, std::enable_if_t<exist_for<T, Methods...>::value, int> = 0>
  basic_any(aa::allocator_arg_t, Alloc alloc, T&& value) noexcept(
      std::is_nothrow_constructible_v<std::decay_t<T>, T&&>&& any_is_small_for<std::decay_t<T>>)
      : alloc(std::move(alloc)) {
    emplace_in_empty<std::decay_t<T>>(std::forward<T>(value));
  }

  // 'transmutate' constructors (from basic_any with more Methods)

  template <typename... OtherMethods,
            std::enable_if_t<(noexport::contains_v<copy_with<Alloc, SooS>, OtherMethods...> &&
                              noexport::has_subsequence(methods_list{}, type_list<OtherMethods...>{})),
                             int> = 0>
  basic_any(const basic_any<Alloc, SooS, OtherMethods...>& other)
      : alloc(alloc_traits::select_on_container_copy_construction(other.get_allocator())) {
    if (!other.has_value())
      return;
    if constexpr (!noexport::copy_requires_alloc<Alloc>()) {
      value_ptr = invoke<copy_with<Alloc, SooS>>(other).copy_fn(mate::get_value_ptr(other), value_ptr);
    } else {
      value_ptr = invoke<copy_with<Alloc, SooS>>(other).copy_fn(mate::get_value_ptr(other), value_ptr, alloc);
    }
    vtable_ptr = subtable_ptr<Methods...>(mate::get_vtable_ptr(other));
  }
  template <typename... OtherMethods,
            std::enable_if_t<(noexport::has_move<OtherMethods...> &&
                              noexport::has_subsequence(methods_list{}, type_list<OtherMethods...>{})),
                             int> = 0>
  basic_any(basic_any<Alloc, SooS, OtherMethods...>&& other) noexcept {
    // same logic as 'move_value_from', but for other type
    if (!other.has_value())
      return;
    // `move` is noexcept (invariant of small state)
    // `move` also 'relocate' i.e. calls dctor of value(for remove invoke<destroy> in future)
    if (!other.memory_allocated()) {
      other.get_move_fn()(other.value_ptr, value_ptr);
    } else {
      value_ptr = std::exchange(other.value_ptr, other.data);
      size_allocated = other.size_allocated;
    }
    vtable_ptr = subtable_ptr<Methods...>(other.vtable_ptr);
    other.vtable_ptr = nullptr;
  }

  // force allocate versions

  template <typename T, typename... Args, std::enable_if_t<exist_for<T, Methods...>::value, int> = 0>
  basic_any(force_stable_pointers_t, std::in_place_type_t<T>, Args&&... args) noexcept(
      std::is_nothrow_constructible_v<std::decay_t<T>, Args&&...>&& any_is_small_for<std::decay_t<T>>) {
    emplace_in_empty<std::decay_t<T>, force_stable_pointers_t>(std::forward<Args>(args)...);
  }
  template <typename T, typename U, typename... Args,
            std::enable_if_t<exist_for<T, Methods...>::value, int> = 0>
  basic_any(force_stable_pointers_t, std::in_place_type_t<T>, std::initializer_list<U> list,
            Args&&... args) noexcept(std::is_nothrow_constructible_v<std::decay_t<T>,
                                                                     std::initializer_list<U>, Args&&...>&&
                                         any_is_small_for<std::decay_t<T>>) {
    emplace_in_empty<std::decay_t<T>, force_stable_pointers_t>(list, std::forward<Args>(args)...);
  }
  template <typename T,
            std::enable_if_t<std::conjunction_v<is_not_polymorphic<T>, exist_for<T, Methods...>>, int> = 0>
  basic_any(force_stable_pointers_t, T&& value) noexcept(
      std::is_nothrow_constructible_v<std::decay_t<T>, T&&>&& any_is_small_for<std::decay_t<T>>)
      : basic_any(force_stable_pointers, std::in_place_type<std::decay_t<T>>, std::forward<T>(value)) {
  }

  // postconditions : has_value() == false
  void reset() noexcept {
    if (!has_value())
      return;
    destroy_value();
    vtable_ptr = nullptr;
  }

  // observe

  // returns true if poly_ptr/ref to this basic_any will not be invalidated after move
  constexpr bool is_stable_pointers() const noexcept {
    return memory_allocated();
  }
  constexpr bool has_value() const noexcept {
    return vtable_ptr != nullptr;
  }
  // returns count of bytes sufficient to store current value
  // (not guaranteed to be smallest)
  // return 0 if !has_value()
  constexpr size_t sizeof_now() const noexcept {
    if (!has_value())
      return 0;
    return memory_allocated() ? allocated_size() : SooS;
  }

 private:
  constexpr auto* get_move_fn() const noexcept {
    if constexpr (has_method<move>)
      return invoke<move>(*this);
    else
      return invoke<noexport::some_copy_method<Methods...>>(*this).move_fn;
  }
  // precodition - has_value() == false
  void move_value_from(basic_any&& other) noexcept {
    if (!other.has_value())
      return;
    // `move` is noexcept (invariant of small state)
    // `move` also 'relocate' i.e. calls dctor of value(for remove invoke<destroy> in future)
    if (!other.memory_allocated()) {
      other.get_move_fn()(other.value_ptr, value_ptr);
    } else {
      value_ptr = std::exchange(other.value_ptr, other.data);
      size_allocated = other.size_allocated;
    }
    vtable_ptr = std::exchange(other.vtable_ptr, nullptr);
  }

  constexpr bool memory_allocated() const noexcept {
    return value_ptr != data;
  }
  constexpr size_t allocated_size() const noexcept {
    assert(has_value() && memory_allocated());
    // needs atleast sizeof(std::size_t) in buffer(SooS)
    // to store allocated size for passing it into deallocate(ptr, n)
    static_assert(SooS >= sizeof(std::size_t));
    // when allocates stores in size in unused buffer
    return size_allocated;
  }
  // precondition - has_value() == true
  void destroy_value() noexcept {
    invoke<destroy> (*this)(value_ptr);
    if (memory_allocated()) {
      alloc_traits::deallocate(alloc, reinterpret_cast<alloc_pointer_type>(value_ptr), allocated_size());
      value_ptr = data;
    }
  }
};

// ######################## materialize(create any_with from polymorphic reference)

template <typename Alloc = default_allocator, size_t SooS = default_any_soos, typename... Methods>
auto materialize(const_poly_ref<Methods...> ref, Alloc alloc = Alloc{})
    -> std::enable_if_t<(noexport::contains_v<copy_with<Alloc, SooS>, Methods...> &&
                         noexport::contains_v<destroy, Methods...>),
                        basic_any<Alloc, SooS, Methods...>> {
  basic_any<Alloc, SooS, Methods...> result(aa::allocator_arg, std::move(alloc));
  mate::set_vtable_ptr(result, mate::get_vtable_ptr(ref));
  if constexpr (!noexport::copy_requires_alloc<Alloc>()) {
    mate::get_value_ptr(result) =
        invoke<copy_with<Alloc, SooS>>(ref).copy_fn(mate::get_value_ptr(ref), mate::get_value_ptr(result));
  } else {
    mate::get_value_ptr(result) = invoke<copy_with<Alloc, SooS>>(ref).copy_fn(
        mate::get_value_ptr(ref), mate::get_value_ptr(result), mate::get_alloc(result));
  }
  return result;
}
#define AA_DECLARE_MATERIALIZE(TEMPLATE, TRANSFORM)                                                  \
  template <typename Alloc = default_allocator, size_t SooS = default_any_soos, typename... Methods> \
  AA_ALWAYS_INLINE auto materialize(const TEMPLATE<Methods...>& value, Alloc alloc = Alloc{})        \
      ->decltype(materialize<Alloc, SooS, Methods...>(TRANSFORM, std::move(alloc))) {                \
    return materialize<Alloc, SooS, Methods...>(TRANSFORM, std::move(alloc));                        \
  }
AA_DECLARE_MATERIALIZE(poly_ref, const_poly_ref(value))
AA_DECLARE_MATERIALIZE(stateful::ref, const_poly_ref(value.get_view()))
AA_DECLARE_MATERIALIZE(stateful::cref, const_poly_ref(value.get_view()))
#undef AA_DECLARE_MATERIALIZE

// ######################## ACTION any_cast ########################

// generic version for every type
// Note: it can cast non-polymorphic types too, but only to
// them =) (int -> int)
// always casts to pointer to T or const T (nullptr if wrong dynamic type)
// example : any_cast<int>(x) -> int*
template <typename T, AA_CONCEPT(poly_traits) Traits = anyany_poly_traits>
struct any_cast_fn {
 private:
  using X = std::remove_reference_t<T>;

 public:
  template <typename U>
  auto* operator()(U&& val) const {
    using ptr_t = decltype(Traits{}.to_address(val));
    constexpr bool is_const_input = std::is_const_v<std::remove_pointer_t<ptr_t>>;
    using result_type = std::conditional_t<is_const_input, const X, X>*;
    if (Traits{}.get_type_descriptor(val) != descriptor_v<T>)
      return result_type(nullptr);
    return reinterpret_cast<result_type>(Traits{}.to_address(val));
  }
};

// may be throwed when casting not Any* / poly_ptr
struct bad_cast : std::exception {
  const char* what() const noexcept override {
    return "incorrect aa::any_cast";
  }
};

// specialization for anyany types
// any_cast<T>(any | any*) -> std::remove_cv_t<T> | T*
// any_cast<T&>(const|poly_ref) -> const|T&
// any_cast<T>(const|poly_ptr) -> const|T*
template <typename T>
struct any_cast_fn<T, anyany_poly_traits> {
 private:
  using X = std::remove_reference_t<T>;

  template <typename Alloc, size_t SooS, typename... Methods>
  static const X* any_cast_impl(const basic_any<Alloc, SooS, Methods...>* any) noexcept {
    // U already remove_cv
    if (any == nullptr || any->type_descriptor() != descriptor_v<T>)
      return nullptr;
    return std::launder(reinterpret_cast<const X*>(mate::get_value_ptr(*any)));
  }
  template <typename Alloc, size_t SooS, typename... Methods>
  static X* any_cast_impl(basic_any<Alloc, SooS, Methods...>* any) noexcept {
    if (any == nullptr || any->type_descriptor() != descriptor_v<T>)
      return nullptr;
    return std::launder(reinterpret_cast<X*>(mate::get_value_ptr(*any)));
  }

 public:
  template <typename U, std::enable_if_t<is_any<U>::value, int> = 0>
  std::add_pointer_t<T> operator()(U* ptr) const noexcept {
    return any_cast_impl(static_cast<typename U::base_any_type*>(ptr));
  }
  template <typename U, std::enable_if_t<is_any<U>::value, int> = 0>
  const X* operator()(const U* ptr) const noexcept {
    return any_cast_impl(static_cast<const typename U::base_any_type*>(ptr));
  }

  template <typename U, std::enable_if_t<is_any<U>::value, int> = 0>
  decltype(auto) operator()(U&& any) const {
    auto* ptr = (*this)(std::addressof(any));
    if (!ptr)
      throw aa::bad_cast{};
    // const T& + const U& == const
    // const T& + non const U& == const
    // non-const T& + const U& == const
    // non-const T& + non-const U& == non-const
    if constexpr (std::is_lvalue_reference_v<T>)
      return *ptr;
    else if constexpr (std::is_rvalue_reference_v<T> && std::is_rvalue_reference_v<U&&>)
      return std::move(*ptr);
    else if constexpr (std::is_rvalue_reference_v<U&&>)
      return noexport::remove_cvref_t<T>(std::move(*ptr));  // move value
    else
      return noexport::remove_cvref_t<T>(*ptr);  // copy value
  }

  template <typename... Methods>
  const X* operator()(const_poly_ptr<Methods...> p) const noexcept {
    if (p == nullptr || p.type_descriptor() != descriptor_v<T>)
      return nullptr;
    return reinterpret_cast<const X*>(p.raw());
  }
  template <typename... Methods>
  X* operator()(poly_ptr<Methods...> p) const noexcept {
    if (p == nullptr || p.type_descriptor() != descriptor_v<T>)
      return nullptr;
    return reinterpret_cast<X*>(p.raw());
  }
  template <typename... Methods>
  std::conditional_t<std::is_rvalue_reference_v<T>, noexport::remove_cvref_t<T>,
                     std::conditional_t<std::is_reference_v<T>, T, std::remove_cv_t<T>>>
  operator()(poly_ref<Methods...> p) const {
    X* ptr = (*this)(&p);
    if (ptr == nullptr) [[unlikely]]
      throw aa::bad_cast{};
    return *ptr;
  }
  // clang-format off
  template <typename... Methods>
  std::conditional_t<std::is_reference_v<T>, const X&, std::remove_cv_t<T>>
  operator()(const_poly_ref<Methods...> p) const {
    // clang-format on
    const X* ptr = (*this)(&p);
    if (ptr == nullptr) [[unlikely]]
      throw aa::bad_cast{};
    return *ptr;
  }
  template<typename... Methods>
  decltype(auto) operator()(const stateful::ref<Methods...>& r) const {
    return (*this)(r.get_view());
  }
  template <typename... Methods>
  decltype(auto) operator()(const stateful::cref<Methods...>& r) const {
    return (*this)(r.get_view());
  }
};

template <typename T, AA_CONCEPT(poly_traits) Traits = anyany_poly_traits>
constexpr inline any_cast_fn<T, Traits> any_cast = {};

template <typename Alloc, size_t SooS, anyany_method_concept... Methods>
using basic_any_with =
    std::conditional_t<noexport::contains_v<destroy, Methods...>, basic_any<Alloc, SooS, Methods...>,
                       basic_any<Alloc, SooS, destroy, Methods...>>;

template <anyany_method_concept... Methods>
using any_with = basic_any_with<default_allocator, default_any_soos, Methods...>;

template <anyany_method_concept... Methods>
using cptr = const_poly_ptr<Methods...>;

template <anyany_method_concept... Methods>
using cref = const_poly_ref<Methods...>;

// just an alias for set of interface requirements, may be used later in 'insert_flatten_into'
template<typename...MethodOrInterfaceAlias>
using interface_alias = type_list<MethodOrInterfaceAlias...>;
// Methods may be aa::interface_alias<...> or just anyany Methods
// using input_iterator_interface = aa::interface_alias<next, is_done, aa::move>; 
// example: insert_flatten_into<aa::any_with, input_iterator_interface, foo, bar>
// same as aa::any_with<next, is_done, aa::move, foo, bar>;
template <template <typename...> typename Template, typename... Methods>
using insert_flatten_into = typename noexport::insert_types<Template, noexport::flatten_types_t<Methods...>>::type;

// enables any_cast, type_switch, visit_invoke etc
// adds method 'type_descriptor' which returns descriptor of current dynamic type or
// descriptor<void> if !has_value()
struct type_info {
  // pseudomethod (just value in vtable)
  using value_type = descriptor_t;
  template <typename T>
  static AA_CONSTEVAL_CPP20 descriptor_t do_value() {
    return descriptor_v<T>;
  }

  template <typename CRTP>
  struct plugin {
#define AA_DECLARE_TYPE_DESCRIPTOR_METHOD(...)              \
  constexpr descriptor_t type_descriptor() const noexcept { \
    const auto& self = *static_cast<const CRTP*>(this);     \
    if constexpr (noexport::has_has_value<CRTP>::value) {   \
      if (!self.has_value())                                \
        return descriptor_v<void>;                          \
    }                                                       \
    return __VA_ARGS__;                                     \
  }
    // returns aa::descriptor_v<void> if !has_value() (references always have value)
    AA_DECLARE_TYPE_DESCRIPTOR_METHOD(invoke<type_info>(self));
  };
};

// adds operator==
struct equal_to {
 private:
  template <typename T>
  static bool fn(const void* first, const void* second) {
    return bool{*reinterpret_cast<const T*>(first) == *reinterpret_cast<const T*>(second)};
  }

 public:
  struct value_type {
    bool (*fn)(const void*, const void*);
    descriptor_t desc;
  };
  template <typename T>
  static AA_CONSTEVAL_CPP20 auto do_value()
      -> decltype(bool{std::declval<T>() == std::declval<T>()}, value_type{}) {
    return value_type{&fn<T>, descriptor_v<T>};
  }
  template <typename CRTP>
  struct plugin : private type_info::plugin<CRTP> {
    // TODO C++23 deducing this solves this
    // remove ambigious with C++20 reverse ordering(.......)
    bool operator==(const plugin<CRTP>& r) const {
      const auto& left = *static_cast<const CRTP*>(this);
      const auto& right = *static_cast<const CRTP*>(std::addressof(r));
      if constexpr (noexport::has_has_value<CRTP>::value) {
        if (!left.has_value())
          return !right.has_value();
      }
      auto [fn, desc] = invoke<equal_to>(left);
      auto [right_fn, right_desc] = invoke<equal_to>(right);
      (void)right_fn;
      return desc == right_desc && fn(mate::get_value_ptr(left), mate::get_value_ptr(right));
    }

#ifndef AA_HAS_CPP20
    bool operator!=(const plugin<CRTP>& other) const {
      return !operator==(other);
    }
#endif
    // returns aa::descriptor_v<void> if !has_value() (references always have value)
    AA_DECLARE_TYPE_DESCRIPTOR_METHOD(invoke<equal_to>(self).desc)
  };
};

#ifdef AA_HAS_CPP20
// enables operator<=> and operator== for any_with
struct spaceship {
 private:
  template <typename T>
  static std::partial_ordering fn(const void* first, const void* second) {
    return *reinterpret_cast<const T*>(first) <=> *reinterpret_cast<const T*>(second);
  }

 public:
  struct value_type {
    std::partial_ordering (*fn)(const void*, const void*);
    descriptor_t desc;
  };
  template <typename T>
  static AA_CONSTEVAL_CPP20 auto do_value()
      -> decltype(std::declval<T>() <=> std::declval<T>(), value_type{}) {
    return value_type{&fn<T>, descriptor_v<T>};
  }
  // inherits from equal_to plugin, for disabling it and shadows operator==
  template <typename CRTP>
  struct plugin : private equal_to::plugin<CRTP> {
    // TODO C++23 deducing this solves this
    // remove ambigious with C++20 reverse ordering(.......)
    bool operator==(const plugin<CRTP>& right) const {
      return ((*static_cast<const CRTP*>(this)) <=> *static_cast<const CRTP*>(std::addressof(right))) ==
             std::partial_ordering::equivalent;
    }
    std::partial_ordering operator<=>(const plugin<CRTP>& r) const {
      const auto& left = *static_cast<const CRTP*>(this);
      const auto& right = *static_cast<const CRTP*>(std::addressof(r));
      // TODO? ordering on descrictors of they are not equal?
      if constexpr (noexport::has_has_value<CRTP>::value) {
        if (!left.has_value())
          return right.has_value() ? std::partial_ordering::unordered : std::partial_ordering::equivalent;
      }
      auto [fn, desc] = invoke<spaceship>(left);
      auto [right_fn, right_desc] = invoke<spaceship>(right);
      (void)right_fn;
      if (desc != right_desc)
        return std::partial_ordering::unordered;
      return fn(mate::get_value_ptr(left), mate::get_value_ptr(right));
    }
    // returns aa::descriptor_v<void> if !has_value() (references always have value)
    AA_DECLARE_TYPE_DESCRIPTOR_METHOD(invoke<spaceship>(self).desc)
  };
};
#endif
#undef AA_DECLARE_TYPE_DESCRIPTOR_METHOD

template <typename... Methods>
constexpr auto operator==(poly_ptr<Methods...> left, poly_ptr<Methods...> right) noexcept
    -> decltype(left.type_descriptor(), true) {
  return left.raw() == right.raw() && left.type_descriptor() == right.type_descriptor();
}
template <typename... Methods>
constexpr auto operator==(const_poly_ptr<Methods...> left, const_poly_ptr<Methods...> right) noexcept
    -> decltype(left.type_descriptor(), true) {
  return left.raw() == right.raw() && left.type_descriptor() == right.type_descriptor();
}
template <typename... Methods>
constexpr auto operator==(poly_ptr<Methods...> left, const_poly_ptr<Methods...> right) noexcept
    -> decltype(const_poly_ptr(left) == right) {
  return const_poly_ptr(left) == right;
}
#ifndef AA_HAS_CPP20
template <typename... Methods>
constexpr auto operator==(const_poly_ptr<Methods...> left, poly_ptr<Methods...> right) noexcept
    -> decltype(left == const_poly_ptr(right)) {
  return left == const_poly_ptr(right);
}
template <typename... Methods>
constexpr auto operator!=(poly_ptr<Methods...> left, poly_ptr<Methods...> right) noexcept
    -> decltype(!(left == right)) {
  return !(left == right);
}
template <typename... Methods>
constexpr auto operator!=(const_poly_ptr<Methods...> left, poly_ptr<Methods...> right) noexcept
    -> decltype(!(left == const_poly_ptr(right))) {
  return !(left == const_poly_ptr(right));
}
template <typename... Methods>
constexpr auto operator!=(poly_ptr<Methods...> left, const_poly_ptr<Methods...> right) noexcept
    -> decltype(!(const_poly_ptr(left) == right)) {
  return !(const_poly_ptr(left) == right);
}
template <typename... Methods>
constexpr auto operator!=(const_poly_ptr<Methods...> left, const_poly_ptr<Methods...> right) noexcept
    -> decltype(!(left == right)) {
  return !(left == right);
}
#endif

// call<Ret(Args...)>::method adds operator() with Args... to basic_any
// (similar to std::function<Ret(Args...)>)
// it supports different signatures:
// * Ret(Args...) const
// * Ret(Args...) noexcept
// * Ret(Args...) const noexcept
template <typename Signature>
struct call {};

#define AA_CALL_IMPL(CONST, NOEXCEPT)                                                       \
  template <typename Ret, typename... Args>                                                 \
  struct call<Ret(Args...) CONST NOEXCEPT> {                                                \
    template <typename T>                                                                   \
    static auto do_invoke(CONST T& self, Args... args) NOEXCEPT                             \
        ->decltype(static_cast<Ret>(self(static_cast<Args&&>(args)...))) {                  \
      return static_cast<Ret>(self(static_cast<Args&&>(args)...));                          \
    }                                                                                       \
    using signature_type = Ret(CONST ::aa::erased_self_t&, Args...) NOEXCEPT;               \
    template <typename CRTP>                                                                \
    struct plugin {                                                                         \
      Ret operator()(Args... args) CONST NOEXCEPT {                                         \
        auto& self = *static_cast<CONST CRTP*>(this);                                       \
        return static_cast<Ret>(                                                            \
            invoke<call<Ret(Args...) CONST NOEXCEPT>>(self, static_cast<Args&&>(args)...)); \
      }                                                                                     \
    };                                                                                      \
  }
AA_CALL_IMPL(, );
AA_CALL_IMPL(const, );
AA_CALL_IMPL(, noexcept);
AA_CALL_IMPL(const, noexcept);
#undef AA_CALL_IMPL

}  // namespace aa

namespace std {

template <typename Alloc, size_t SooS, typename... Methods>
AA_IF_HAS_CPP20(requires(::aa::noexport::contains_v<::aa::hash, Methods...>))
struct hash<::aa::basic_any<Alloc, SooS, Methods...>> {
  size_t operator()(const ::aa::basic_any<Alloc, SooS, Methods...>& any) const noexcept {
    return any.has_value() ? aa::invoke<::aa::hash>(any) : 0;
  }
};
template <typename... Methods>
AA_IF_HAS_CPP20(requires(::aa::noexport::contains_v<::aa::hash, Methods...>))
struct hash<::aa::poly_ref<Methods...>> {
  size_t operator()(const ::aa::poly_ref<Methods...>& r) const noexcept {
    return aa::invoke<::aa::hash>(r);
  }
};
template <typename... Methods>
AA_IF_HAS_CPP20(requires(::aa::noexport::contains_v<::aa::hash, Methods...>))
struct hash<::aa::const_poly_ref<Methods...>> {
  size_t operator()(const ::aa::const_poly_ref<Methods...>& r) const noexcept {
    return aa::invoke<::aa::hash>(r);
  }
};
template <typename... Methods>
AA_IF_HAS_CPP20(requires(::aa::noexport::contains_v<::aa::hash, Methods...>))
struct hash<::aa::stateful::ref<Methods...>> {
  size_t operator()(const ::aa::stateful::ref<Methods...>& r) const noexcept {
    return aa::invoke<::aa::hash>(r);
  }
};
template <typename... Methods>
AA_IF_HAS_CPP20(requires(::aa::noexport::contains_v<::aa::hash, Methods...>))
struct hash<::aa::stateful::cref<Methods...>> {
  size_t operator()(const ::aa::stateful::cref<Methods...>& r) const noexcept {
    return aa::invoke<::aa::hash>(r);
  }
};
template <typename... Methods>
struct hash<::aa::poly_ptr<Methods...>> {
  size_t operator()(const ::aa::poly_ptr<Methods...>& p) const noexcept {
    return ::std::hash<void*>{}(p.raw());
  }
};
template <typename... Methods>
struct hash<::aa::const_poly_ptr<Methods...>> {
  size_t operator()(const ::aa::const_poly_ptr<Methods...>& p) const noexcept {
    return ::std::hash<void*>{}(p.raw());
  }
};

}  // namespace std

#include "noexport/file_end.hpp"
#undef anyany_method_concept