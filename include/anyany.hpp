#pragma once

/*
  HEADER SYNOPSIS:

  * polymorphic value (basic_any_with, any_with),
  * reference (const_poly_ref, ref),
  * pointer (const_poly_ptr, poly_ptr),
  Basic actions on polymorphic types:
  * any_cast<T>
  * invoke<Method>
  * type_switch(/const/ref)

*/
#include <array>
#include <utility>      // std::exchange
#include <cassert>      // assert
#include <memory>       // construct_at / destroy_at
#include <stdexcept>    // bad_cast/exception
#include <cstddef>      // max_align_t
#include <optional>     // for type_switch
#include <functional>   // std::invoke
#include <compare>

#include "type_descriptor.hpp"
#include "noexport/anyany_details.hpp"

// TTA == template template argument (just for better reading)
#define TTA template <typename> typename

namespace aa {

// Method must have same signature for all types(except self),
// so this type used to check what signature Method have
// this means all methods must be specializable for interface_t
// typical error - concept on Method's template argument, which is false for interface_t
// (in this case you can explciitly specialize Method for interface_t)
struct interface_t {
  // these operators just for satisfying concepts of equal_to/spaceship/call methods
  constexpr bool operator==(const interface_t&) const noexcept;
  constexpr std::strong_ordering operator<=>(const interface_t&) const noexcept;
  constexpr void operator()(auto&&...) noexcept;
  constexpr void operator()(auto&&...) const noexcept;
};

// ######################## explicit_interface support ########################

// just unique type for every Method, because i need to inherit from plugins
template <TTA Method>
struct nullplugin {};

template <TTA Method>
concept has_plugin = requires {
  // nullplugin excepts template template argument with one input type. So it works
  typename nullplugin<Method<interface_t>::template plugin>;
};

template <TTA Method, typename>
struct plugin : std::type_identity<nullplugin<Method>> {};

template <TTA Method, typename Any>
requires has_plugin<Method>
struct plugin<Method, Any> : std::type_identity<typename Method<interface_t>::template plugin<Any>> {
};

template <TTA Method, typename Any>
using plugin_t = typename plugin<Method, Any>::type;

template <TTA Method>
concept has_explicit_interface = requires {
  typename Method<interface_t>::explicit_interface;
};

template <TTA Method>  // no matter nullplugin or what here, it is using for optimization
using satisfy = nullplugin<Method>;

// used if Method requires explicit subscribe,
// for example : struct A { using satisfies = aa::satisfies<Fooable, Drawable>; };
template <TTA... Methods>
struct satisfies : satisfy<Methods>... {
  static_assert((has_explicit_interface<Methods> && ...), "It's useless to subscribe if it is not required");
};

// You can add specialization for your type,
// so it is equal to 'using satisfies = aa::satisfies<Method>;' in type
template <typename T, TTA... Methods>
constexpr inline bool satisfies_v = false;
template<TTA... Methods>
constexpr inline bool satisfies_v<interface_t, Methods...> = true;

// true if satisfies_v specialized for T or T has inner 'using satisfies = aa::satisfies<...Method...>'
template<typename T, TTA Method>
concept is_satisfies = satisfies_v<T, Method> || std::is_base_of_v<satisfy<Method>, typename T::satisfies>;

// ######################## compilt time information about Methods(Traits) ########################

template <TTA Method>
using method_traits = noexport::any_method_traits<decltype(&Method<interface_t>::do_invoke)>;

template <TTA Method>
using result_t = typename method_traits<Method>::result_type;

template <TTA Method>
using self_sample_t = typename method_traits<Method>::self_sample_type;

template <TTA Method>
using signature_t = typename method_traits<Method>::signature_type;

template <TTA Method>
using type_erased_signature_t = typename method_traits<Method>::type_erased_signature_type;

template <TTA Method>
using type_erased_self_t = typename method_traits<Method>::type_erased_self_type;

template <TTA Method>
using args_list = typename method_traits<Method>::args;

template <TTA Method>
concept const_method = method_traits<Method>::is_const;

template <typename T, TTA Method, typename = args_list<Method>>
struct invoker_for {};

template <typename T, TTA Method, typename... Args>
struct invoker_for<T, Method, type_list<Args...>> {
  static auto value(type_erased_self_t<Method> self, Args&&... args) -> result_t<Method> {
    using self_sample = self_sample_t<Method>;
    if constexpr (has_explicit_interface<Method>)
      static_assert(is_satisfies<T, Method>,
                    "Method requires explicit subscribe and your type not satisfies it explicitly(using "
                    "satisfies = aa::satisfies<MethodName>;");

    if constexpr (std::is_lvalue_reference_v<self_sample>) {
      using real_self = std::conditional_t<const_method<Method>, const T*, T*>;
      return Method<T>::do_invoke(*std::launder(reinterpret_cast<real_self>(self)),
                                  static_cast<Args&&>(args)...);
    } else if constexpr (std::is_copy_constructible_v<T>) {
      return Method<T>::do_invoke(*std::launder(reinterpret_cast<const T*>(self)),
                                  static_cast<Args&&>(args)...);
    } else {
      static_assert(noexport::always_false<T>,
                    "You pass self by value and it is not a copy constructible... or by rvalue reference");
    }
  }
};

namespace noexport {

template <typename, typename, TTA...>
struct do_invocable_impl : std::false_type {};
template <typename T, TTA... Methods>
struct do_invocable_impl<T, std::void_t<decltype(&Methods<T>::do_invoke)...>, Methods...> : std::true_type {};

}  // namespace noexport

// checks if each of Methods has addressable do_invoke for T
// it provides a way to restrict constructors in SFINAE-friendly way
template <typename T, TTA... Methods>
concept do_invokable = noexport::do_invocable_impl<std::decay_t<T>, void, Methods...>::value;

// concept of any value inherited from basic_any<Args...>
template <typename T>
concept any_x = requires {
  typename std::remove_cvref_t<T>::base_any_type;
};

// ######################## BASIC METHODS for basic_any ########################

template <std::destructible T>
struct destroy {
  static void do_invoke(const T& self) noexcept {
    std::destroy_at(std::addressof(self));
  }
};

template <std::destructible T>
struct move {
  // invoked only for situatuion "move small from src to EMPTY dest" and only when type is nothrow move
  // constructible. Actially relocates
  static void do_invoke(T& src, void* dest) {
    if constexpr (std::is_nothrow_move_constructible_v<T>) {
      std::construct_at(reinterpret_cast<T*>(dest), std::move(src));
      std::destroy_at(std::addressof(src));
    } else {
      // not static assert, because will be never called, but needed to compile
      assert(false);
    }
  }
};

// Alloc without state
template <typename Alloc, size_t SooS>
struct copy_with {
  // returns pointer to created copy
  // precondition: Any is empty
  // postcondition:
  // if memory allocated then 'dest' != returned value,
  // and 'dest' points to size_t value (allocated bytes count)
  template <typename T>
  struct method {
    using allocator_type = Alloc;
    static constexpr size_t SooS_value = SooS;

    static void* do_invoke(const T& src, void* dest) {
      if constexpr (sizeof(T) <= SooS && std::is_nothrow_copy_constructible_v<T>) {
        std::construct_at(reinterpret_cast<T*>(dest), src);
        return dest;
      } else {
        using alloc_traits = std::allocator_traits<Alloc>;
        // alloc_sz must be std::max(SooS, sizeof(T)), it is invariant of big state
        constexpr typename alloc_traits::size_type alloc_sz = std::max(SooS, sizeof(T));
        // no fancy pointers supported
        void* ptr = Alloc{}.allocate(alloc_sz);
        if constexpr (std::is_nothrow_copy_constructible_v<T>) {
          std::construct_at(reinterpret_cast<T*>(ptr), src);
        } else {
          try {
            std::construct_at(reinterpret_cast<T*>(ptr), src);
          } catch (...) {
            Alloc{}.deallocate(reinterpret_cast<typename alloc_traits::pointer>(ptr), alloc_sz);
            throw;
          }
        }
        std::construct_at(reinterpret_cast<std::size_t*>(dest), alloc_sz);
        return ptr;
      }
    }
  };
};

// Alloc with state
// clang-format off
template <typename Alloc, size_t SooS>
requires(!std::is_empty_v<Alloc>)
struct copy_with<Alloc, SooS> {
  // clang-format on
  template <typename T>
  struct method {
    using allocator_type = Alloc;
    static constexpr size_t SooS_value = SooS;

    static void* do_invoke(const T& src, void* dest, Alloc& alloc) {
      if constexpr (sizeof(T) <= SooS && std::is_nothrow_copy_constructible_v<T>) {
        std::construct_at(reinterpret_cast<T*>(dest), src);
        return dest;
      } else {
        using alloc_traits = std::allocator_traits<Alloc>;
        // alloc_sz must be std::max(SooS, sizeof(T)), it is invariant of big state
        constexpr typename alloc_traits::size_type alloc_sz = std::max(SooS, sizeof(T));
        // no fancy pointers supported
        void* ptr = alloc.allocate(alloc_sz);
        if constexpr (std::is_nothrow_copy_constructible_v<T>) {
          std::construct_at(reinterpret_cast<T*>(ptr), src);
        } else {
          try {
            std::construct_at(reinterpret_cast<T*>(ptr), src);
          } catch (...) {
            alloc.deallocate(reinterpret_cast<typename alloc_traits::pointer>(ptr), alloc_sz);
            throw;
          }
        }
        std::construct_at(reinterpret_cast<std::size_t*>(dest), alloc_sz);
        return ptr;
      }
    }
  };
};

constexpr inline auto default_any_soos = 64 - 3 * sizeof(void*);

template <typename T>
using copy = copy_with<std::allocator<std::byte>, default_any_soos>::template method<T>;

// enables std::hash specialization for polymorphic value and reference
template <typename T>
struct hash {
  static size_t do_invoke(const T& self) {
    return std::hash<T>{}(self);
  }
};

// enables operator== for any_with
template <std::equality_comparable T>
struct equal_to {
  static bool do_invoke(const T& first, const void* second) {
    return first == *reinterpret_cast<const T*>(second);
  }
};

// enables operator<=> and operator== for any_with
template <std::three_way_comparable T>
struct spaceship {
  // See basic_any::operator<=> to understand why it is partical ordering always
  // strong and weak ordering is implicitly convertible to partical ordeting by C++20 standard!
  static std::partial_ordering do_invoke(const T& first, const void* second){
    return first <=> *reinterpret_cast<const T*>(second);
  }
};

// ######################## VTABLE TYPE ########################

// regardless Method is a template,
// do_invoke signature must be same for any valid T (as for virtual functions)
template <TTA... Methods>
struct vtable {
  noexport::tuple<type_erased_signature_t<Methods>...> table;

  template <TTA Method>
  static inline constexpr size_t number_of_method =
      noexport::number_of_first<Method<interface_t>, Methods<interface_t>...>;

  template <TTA Method>
  static inline constexpr bool has_method = number_of_method<Method> != npos;
  // clang-format off
  template <TTA Method, typename... Args>
  requires(has_method<Method>)
  constexpr decltype(auto) invoke(Args&&... args) const {
    // clang-format on
    return noexport::get_value<number_of_method<Method>>(table)(std::forward<Args>(args)...);
  }
};

template<TTA... Methods>
struct vtable_with_metainfo {
  descriptor_t type_descriptor;
  const void* const terminator = nullptr;  // indicates vtable begin! invariant - always nullptr
  vtable<Methods...> table;
};

namespace noexport {
// precondition: vtable_ptr points to 'vtable<Methoods...>; which was created as field in struct
// 'vtable_with_metainfo' (any vtable created by poly_ptr/ref/any_with satisfies this requirement)
inline descriptor_t get_type_descriptor(const void* vtable_ptr) noexcept {
  static_assert(sizeof(vtable<destroy, copy>) == sizeof(std::array<void*, 2>));
  static_assert(sizeof(vtable_with_metainfo<destroy>) == sizeof(std::array<void*, 3>));
  // standard layout guaratees that 'this' can be converted to pointer to first field
  static_assert(std::is_standard_layout_v<vtable_with_metainfo<>>);
  // for example table is [A,B,C] and A, B, C is pointers to Method<X>::do_invoke
  // up-cast reinterprets some of A, B, C as new vtable begin
  // for creating pointer to table like [B, C] or [C])
  // so this method searches real start of vtable and returns type descriptor by using guarantee that
  // vtable always created as field in struct 'vtable_with_metainfo'
  using value_type = const void*;
  auto* p = reinterpret_cast<const value_type*>(vtable_ptr);
  do {
    --p;
  } while (*p);
  auto* type_desc_ptr = reinterpret_cast<const std::byte*>(p) - offsetof(vtable_with_metainfo<>, terminator);
  return *reinterpret_cast<const descriptor_t*>(type_desc_ptr);
}

}  // namespace noexport

// casts vtable to subvtable with smaller count of Methods if ToMethods are contigous subset of FromMethods
// For example vtable<M1,M2,M3,M4>* can be converted to vtable<M2,M3>*, but not to vtable<M2,M4>* 
// clang-format off
// first argument only for deducting ToMethods(or internal compiler error on gcc...)
template <TTA... ToMethods, TTA... FromMethods>
requires(noexport::find_subset(
                            type_list<ToMethods<interface_t>...>{},
                            type_list<FromMethods<interface_t>...>{}) != npos)
const vtable<ToMethods...>* subtable_ptr(const vtable<FromMethods...>* ptr) noexcept {
  // clang-format on
  assert(ptr != nullptr);
  constexpr std::size_t Index = noexport::find_subset(type_list<ToMethods<interface_t>...>{},
                                                        type_list<FromMethods<interface_t>...>{});
  static_assert(sizeof(vtable<FromMethods...>) == sizeof(void*) * sizeof...(FromMethods));
  return reinterpret_cast<const vtable<ToMethods...>*>(reinterpret_cast<const std::byte*>(ptr) +
                                                       sizeof(void*) * Index);
}

// must be never named explicitly, use addr_vtable_for
template <typename T, TTA... Methods>
constexpr vtable_with_metainfo<Methods...> vtable_for = {
    descriptor_v<T>, nullptr /* terminator */, vtable<Methods...>{{&invoker_for<T, Methods>::value...}}};

// always decays type
template<typename T, TTA... Methods>
constexpr const vtable<Methods...>* addr_vtable_for = &vtable_for<std::decay_t<T>, Methods...>.table; 

// ######################## ref / poly_ptr  ########################

// it is concept for removing ambigious ctors in poly ptrs
template <typename T>
concept not_const_type = !std::is_const_v<T>;

// non nullable non owner view to any type which satisfies Methods...
template <TTA... Methods>
struct AA_MSVC_EBO poly_ref : plugin_t<Methods, poly_ref<Methods...>>... {
 private:
  const vtable<Methods...>* vtable_ptr;  // unspecified value if value_ptr == nullptr
  void* value_ptr;

  static_assert((std::is_empty_v<plugin_t<Methods, poly_ref<Methods...>>> && ...));

  template <TTA, typename>
  friend struct invoke_fn;
  template <TTA...>
  friend struct poly_ptr;
  template <TTA...>
  friend struct poly_ref;
  template <TTA...>
  friend struct const_poly_ptr;
  template <TTA...>
  friend struct const_poly_ref;

  // uninitialized for pointer implementation
  constexpr poly_ref(std::nullptr_t) noexcept : vtable_ptr{nullptr}, value_ptr{nullptr} {
  }

 public:
  poly_ref(const poly_ref&) = default;
  poly_ref(poly_ref&&) = default;

  poly_ref& operator=(poly_ref&&) = default;
  poly_ref& operator=(const poly_ref&) = default;
  // cannot implicitly rebind reference
  void operator=(auto&&) = delete;

  // from mutable lvalue
  template <not_const_type T>  // not shadow copy ctor
    requires(do_invokable<T, Methods...> && !std::same_as<poly_ref<Methods...>, T> && !any_x<T>)
  constexpr poly_ref(T& value) noexcept
      : vtable_ptr{addr_vtable_for<T, Methods...>}, value_ptr{std::addressof(value)} {
    static_assert(!std::is_array_v<T> && !std::is_function_v<T>,
                  "Decay it before emplace, ambigious pointer");
  }

  template <
      TTA... FromMethods,
      typename = std::void_t<decltype(subtable_ptr<Methods...>(std::declval<vtable<FromMethods...>*>()))>>
  constexpr poly_ref(poly_ref<FromMethods...> p) noexcept

      : vtable_ptr(subtable_ptr<Methods...>(p.vtable_ptr)), value_ptr(p.value_ptr) {
  }
  // for same interface(in plugins for example), always returns true
  static consteval bool has_value() noexcept {
    return true;
  }
  // returns poly_ptr<Methods...>
  constexpr auto operator&() const noexcept;

  descriptor_t type_descriptor() const noexcept {
    return noexport::get_type_descriptor(vtable_ptr);
  }

 private:
  template <TTA Method>
  static consteval bool has_method() {
    return vtable<Methods...>::template has_method<Method>;
  }

 public:
  // references are equal if they reference to equal objects
  [[nodiscard]] bool operator==(const poly_ref& other) const
    requires(has_method<equal_to>() || has_method<spaceship>())
  {
    auto desc = type_descriptor();
    if (desc != other.type_descriptor())
      return false;
    assert(desc != descriptor_v<void>);
    if constexpr (has_method<equal_to>())
      return vtable_ptr->template invoke<equal_to>(value_ptr, static_cast<const void*>(other.value_ptr));
    else
      return vtable_ptr->template invoke<spaceship>(value_ptr, static_cast<const void*>(other.value_ptr)) ==
             std::partial_ordering::equivalent;
  }
  std::partial_ordering operator<=>(const poly_ref& other) const
    requires(has_method<spaceship>())
  {
    auto desc = type_descriptor();
    if (desc != other.type_descriptor())
      return std::partial_ordering::unordered;
    assert(desc != descriptor_v<void>);
    return vtable_ptr->template invoke<spaceship>(value_ptr, static_cast<const void*>(other.value_ptr));
  }
};

// non nullable non owner view to any type which satisfies Methods...
// Note: do not extends lifetime
template<TTA... Methods>
struct AA_MSVC_EBO const_poly_ref : plugin_t<Methods, const_poly_ref<Methods...>>... {
 private:
  const vtable<Methods...>* vtable_ptr;  // unspecified value if value_ptr == nullptr
  const void* value_ptr;

  static_assert((std::is_empty_v<plugin_t<Methods, const_poly_ref<Methods...>>> && ...));

  template <TTA, typename>
  friend struct invoke_fn;
  template <TTA...>
  friend struct const_poly_ptr;
  template <TTA...>
  friend struct const_poly_ref;
  // uninitialized for pointer implementation
  constexpr const_poly_ref(std::nullptr_t) noexcept : vtable_ptr{nullptr}, value_ptr{nullptr} {
  }

 public:

  const_poly_ref(const const_poly_ref&) = default;
  const_poly_ref(const_poly_ref&&) = default;

  const_poly_ref& operator=(const_poly_ref&&) = default;
  const_poly_ref& operator=(const const_poly_ref&) = default;
  // cannot implicitly rebind reference
  void operator=(auto&&) = delete;

  // from non-polymorphic const reference
  template <do_invokable<Methods...> T>
    requires(!any_x<T>)
  constexpr const_poly_ref(const T& value) noexcept
      : vtable_ptr(addr_vtable_for<T, Methods...>), value_ptr(std::addressof(value)) {
    static_assert(!std::is_array_v<T> && !std::is_function_v<T>,
                  "Decay it before emplace, ambigious pointer");
  }

  constexpr const_poly_ref(poly_ref<Methods...> p) noexcept : vtable_ptr(p.vtable_ptr), value_ptr(p.value_ptr) {
  }
  template <
      TTA... FromMethods,
      typename = std::void_t<decltype(subtable_ptr<Methods...>(std::declval<vtable<FromMethods...>*>()))>>
  constexpr const_poly_ref(const_poly_ref<FromMethods...> p) noexcept
      : vtable_ptr(subtable_ptr<Methods...>(p.vtable_ptr)), value_ptr(p.value_ptr) {
  }
  template <
      TTA... FromMethods,
      typename = std::void_t<decltype(subtable_ptr<Methods...>(std::declval<vtable<FromMethods...>*>()))>>
  constexpr const_poly_ref(poly_ref<FromMethods...> p) noexcept
      : vtable_ptr(subtable_ptr<Methods...>(p.vtable_ptr)), value_ptr(p.value_ptr) {
  }
  // for same interface(in plugins for example), always returns true
  static consteval bool has_value() noexcept {
    return true;
  }
  // returns const_poly_ptr<Methods...>
  constexpr auto operator&() const noexcept;

  descriptor_t type_descriptor() const noexcept {
    return noexport::get_type_descriptor(vtable_ptr);
  }
 private:
  poly_ref<Methods...> const_casted() const noexcept {
    poly_ref<Methods...> me = nullptr;
    me.value_ptr = const_cast<void*>(value_ptr);
    me.vtable_ptr = vtable_ptr;
    return me;
  }
  template <TTA Method>
  static consteval bool has_method() {
    return vtable<Methods...>::template has_method<Method>;
  }

 public:
  // references are equal if they reference to equal objects
  [[nodiscard]] bool operator==(const poly_ref<Methods...>& ref) const
    requires(has_method<equal_to>() || has_method<spaceship>())
  {
    return const_casted() == ref;
  }
  [[nodiscard]] bool operator==(const const_poly_ref& ref) const
    requires(has_method<equal_to>() || has_method<spaceship>())
  {
    return const_casted() == ref.const_casted();
  }
  std::partial_ordering operator<=>(const poly_ref<Methods...> ref) const
    requires(has_method<spaceship>())
  {
    return const_casted() <=> ref;
  }
  std::partial_ordering operator<=>(const const_poly_ref& ref) const
    requires(has_method<spaceship>())
  {
    return const_casted() <=> ref.const_casted();
  }
};

template <TTA... Methods>
const_poly_ref(poly_ref<Methods...>) -> const_poly_ref<Methods...>;

// non owning pointer-like type, behaves like pointer to mutable abstract base type
template <TTA... Methods>
struct poly_ptr {
 private:
  // uninitialized reference by default
  poly_ref<Methods...> poly_ = nullptr;

  template <TTA...>
  friend struct poly_ptr;
  template <TTA...>
  friend struct const_poly_ptr;
  template <TTA...>
  friend struct poly_ref;

 public:
  // from nothing (empty)
  constexpr poly_ptr() = default;
  constexpr poly_ptr(std::nullptr_t) noexcept : poly_ptr() {
  }
  constexpr poly_ptr& operator=(std::nullptr_t) noexcept {
    poly_.value_ptr = nullptr;
    // value of vtable_ptr does not matter if value_ptr == nullptr
    return *this;
  }
  // from mutable pointer enable_if removes potential ambigious
  template <not_const_type T, typename = std::enable_if_t<do_invokable<T, Methods...>>>
    requires(!any_x<T>)
  constexpr poly_ptr(T* ptr) noexcept {
    poly_.value_ptr = ptr;
    poly_.vtable_ptr = addr_vtable_for<T, Methods...>;
  }
  // from mutable pointer to Any
  template <not_const_type Any>
    requires(any_x<Any> && noexport::find_subset(type_list<Methods<interface_t>...>{},
                                                          typename Any::methods_list{}) != npos)
  constexpr poly_ptr(Any* ptr) noexcept {
    if (ptr != nullptr) [[likely]] {
      poly_.vtable_ptr = subtable_ptr<Methods...>(ptr->vtable_ptr);
      poly_.value_ptr = ptr->value_ptr;
    }
  }

  template <
      TTA... FromMethods,
      typename = std::void_t<decltype(subtable_ptr<Methods...>(std::declval<vtable<FromMethods...>*>()))>>
  constexpr poly_ptr(poly_ptr<FromMethods...> p) noexcept {
    poly_.value_ptr = p.poly_.value_ptr;
    poly_.vtable_ptr = subtable_ptr<Methods...>(p.poly_.vtable_ptr);
  }
  // observers

  constexpr void* raw() const noexcept {
    return poly_.value_ptr;
  }
  constexpr const vtable<Methods...>* raw_vtable_ptr() const noexcept {
    return poly_.vtable_ptr;
  }
  constexpr bool has_value() const noexcept {
    return poly_.value_ptr != nullptr;
  }
  constexpr bool operator==(std::nullptr_t) const noexcept {
    return !has_value();
  }
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

  // compare

  // returns true if pointed to same logical object(same type and address)
  constexpr bool operator==(const poly_ptr& other) const noexcept {
    return raw() == other.raw() && type_descriptor() == other.type_descriptor();
  }

  // returns descriptor for void if *this == nullptr
  descriptor_t type_descriptor() const noexcept {
    return has_value() ? poly_.type_descriptor() : descriptor_t{};
  }
};

// non owning pointer-like type, behaves like pointer to CONST abstract base type
template <TTA... Methods>
struct const_poly_ptr {
 private:
  // uninitialized reference by default
  const_poly_ref<Methods...> poly_ = nullptr;

  template <TTA...>
  friend struct const_poly_ptr;
  template <TTA...>
  friend struct const_poly_ref;

 public:
  // from nothing(empty)
  constexpr const_poly_ptr() = default;
  constexpr const_poly_ptr(std::nullptr_t) noexcept : const_poly_ptr() {
  }
  constexpr const_poly_ptr& operator=(std::nullptr_t) noexcept {
    poly_.value_ptr = nullptr;
    poly_.vtable_ptr = nullptr;
    return *this;
  }
  // from pointer to value
  template <typename T, typename = std::enable_if_t<do_invokable<T, Methods...>>>
    requires(!any_x<T>)
  constexpr const_poly_ptr(const T* ptr) noexcept {
    poly_.value_ptr = ptr;
    // if ptr == nullptr no matter what stored in vtable_ptr and i dont want branching here
    poly_.vtable_ptr = addr_vtable_for<T, Methods...>;
  }
  // from pointer to Any
  template <any_x Any>
    requires(noexport::find_subset(type_list<Methods<interface_t>...>{}, typename Any::methods_list{}) !=
             npos)
  constexpr const_poly_ptr(const Any* p) noexcept {
    if (p != nullptr) [[likely]] {
      poly_.vtable_ptr = subtable_ptr<Methods...>(p->vtable_ptr);
      poly_.value_ptr = p->value_ptr;
    }
  }
  // from non-const poly pointer
  constexpr const_poly_ptr(poly_ptr<Methods...> p) noexcept {
    poly_.value_ptr = p.poly_.value_ptr;
    poly_.vtable_ptr = p.poly_.vtable_ptr;
  }
  template <
      TTA... FromMethods,
      typename = std::void_t<decltype(subtable_ptr<Methods...>(std::declval<vtable<FromMethods...>*>()))>>
  constexpr const_poly_ptr(const_poly_ptr<FromMethods...> p) noexcept {
    poly_.value_ptr = p.poly_.value_ptr;
    poly_.vtable_ptr = subtable_ptr<Methods...>(p.poly_.vtable_ptr);
  }
  template <
      TTA... FromMethods,
      typename = std::void_t<decltype(subtable_ptr<Methods...>(std::declval<vtable<FromMethods...>*>()))>>
  constexpr const_poly_ptr(poly_ptr<FromMethods...> p) noexcept : const_poly_ptr(const_poly_ptr<FromMethods...>{p}) {
  }
  // observers

  constexpr const void* raw() const noexcept {
    return poly_.value_ptr;
  }
  constexpr const vtable<Methods...>* raw_vtable_ptr() const noexcept {
    return poly_.vtable_ptr;
  }
  constexpr bool has_value() const noexcept {
    return poly_.value_ptr != nullptr;
  }
  constexpr bool operator==(std::nullptr_t) const noexcept {
    return !has_value();
  }
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

  // compare

  // returns true if pointed to same logical object(same type and address)
  constexpr bool operator==(const const_poly_ptr& other) const noexcept {
    return raw() == other.raw() && type_descriptor() == other.type_descriptor();
  }

  // returns descriptor for void if *this == nullptr
  descriptor_t type_descriptor() const noexcept {
    return has_value() ? poly_.type_descriptor() : descriptor_t{};
  }
};

template<TTA... Methods>
const_poly_ptr(poly_ptr<Methods...>) -> const_poly_ptr<Methods...>;

template <TTA... Methods>
constexpr auto poly_ref<Methods...>::operator&() const noexcept {
  poly_ptr<Methods...> result;
  result.poly_.vtable_ptr = vtable_ptr;
  result.poly_.value_ptr = value_ptr;
  return result;
}
template <TTA... Methods>
constexpr auto const_poly_ref<Methods...>::operator&() const noexcept {
  const_poly_ptr<Methods...> result;
  result.poly_.vtable_ptr = vtable_ptr;
  result.poly_.value_ptr = value_ptr;
  return result;
}

// ######################## ACTION invoke ########################

template <TTA Method, typename = args_list<Method>>
struct invoke_fn {};

template <TTA Method, typename... Args>
struct invoke_fn<Method, type_list<Args...>> {
  // FOR ANY

  template<any_x Any>
  result_t<Method> operator()(Any&& any, Args... args) const {
    assert(any.vtable_ptr != nullptr);
    return any.vtable_ptr->template invoke<Method>(any.value_ptr, static_cast<Args&&>(args)...);
  }
  template<any_x Any>
  result_t<Method> operator()(const Any& any, Args... args) const {
    static_assert(const_method<Method>);
    assert(any.vtable_ptr != nullptr);
    return any.vtable_ptr->template invoke<Method>(any.value_ptr, static_cast<Args&&>(args)...);
  }

  // FOR POLYMORPHIC REF

  template <TTA... Methods>
  result_t<Method> operator()(poly_ref<Methods...> p, Args... args) const {
    return p.vtable_ptr->template invoke<Method>(p.value_ptr, static_cast<Args&&>(args)...);
  }
  template <TTA... Methods>
  result_t<Method> operator()(const_poly_ref<Methods...> p, Args... args) const {
    static_assert(const_method<Method>);
    return p.vtable_ptr->template invoke<Method>(p.value_ptr, static_cast<Args&&>(args)...);
  }
};

// for cases, when you sure any has value (so UB if !has_value), compilers bad at optimizations(
template <TTA Method>
constexpr inline invoke_fn<Method> invoke = {};

// ######################## BASIC_ANY ########################

// when used in ctor this tag forces anyany allocate memory, so pointers to value(poly_ptr/poly_ref)
// will not invalidated after anyany move
struct force_stable_pointers_t {
  explicit force_stable_pointers_t() = default;
};
constexpr inline force_stable_pointers_t force_stable_pointers{};

// SooS == Small Object Optimization Size
// strong exception guarantee for all constructors and assignments,
// emplace<T> - *this is empty if exception thrown
// for alloc not all fancy pointers supported and construct / destroy not throught alloc
template <typename Alloc, size_t SooS, TTA... Methods>
struct AA_MSVC_EBO basic_any : plugin_t<Methods, basic_any<Alloc, SooS, Methods...>>... {
 private:
  const vtable<Methods...>* vtable_ptr = nullptr;
  void* value_ptr = &data;
  alignas(std::max_align_t) std::array<std::byte, SooS> data;
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
  // count allocated bytes ALWAYS == std::max(SooS, sizeof(T)) (need to avoid UB in deallocate)

  public:
  // guarantees that small is nothrow movable(for noexcept move ctor/assign)
  template <typename U>
  consteval static bool any_is_small_for() noexcept {
    using T = std::decay_t<U>;
    return alignof(T) <= alignof(std::max_align_t) && std::is_nothrow_move_constructible_v<T> &&
           sizeof(T) <= SooS;
  }

  template <typename T, typename ForceAllocate = void, typename... Args>
  void emplace_in_empty(Args&&... args) {
    if constexpr (any_is_small_for<T>() && std::is_void_v<ForceAllocate>) {
      std::construct_at(reinterpret_cast<T*>(value_ptr), std::forward<Args>(args)...);
    } else {
      // invariant of big - allocated_size >= SooS
      constexpr size_t allocation_size = std::max(sizeof(T), SooS);
      void* old_value_ptr = std::exchange(value_ptr, alloc.allocate(allocation_size));
      if constexpr (std::is_nothrow_constructible_v<T, Args&&...>) {
        std::construct_at(reinterpret_cast<T*>(value_ptr), std::forward<Args>(args)...);
      } else {
        try {
          std::construct_at(reinterpret_cast<T*>(value_ptr), std::forward<Args>(args)...);
        } catch (...) {
          alloc.deallocate(reinterpret_cast<alloc_pointer_type>(value_ptr), allocation_size);
          value_ptr = old_value_ptr;
          throw;
        }
      }
      std::construct_at(reinterpret_cast<std::size_t*>(data.data()), allocation_size);
    }
    vtable_ptr = addr_vtable_for<T, Methods...>;
  }

  using alloc_traits = std::allocator_traits<Alloc>;
  using alloc_pointer_type = typename alloc_traits::pointer;
  using alloc_size_type = typename alloc_traits::size_type;

  template <TTA...>
  friend struct poly_ptr;
  template<TTA...>
  friend struct const_poly_ptr;
  template <TTA, typename>
  friend struct invoke_fn;

 public:
  template <TTA Method>
  static constexpr bool has_method = vtable<Methods...>::template has_method<Method>;
  static constexpr bool has_copy = has_method<copy_with<Alloc, SooS>::template method>;

  static_assert((std::is_empty_v<plugin_t<Methods, basic_any<Alloc, SooS, Methods...>>> && ...));
  static_assert((noexport::check_copy<Methods<interface_t>, Alloc, SooS>() && ...),
                "Alloc and SooS in copy do not match Alloc in SooS in basic_any, "
                "use aa::copy_with<Alloc, SooS>::tempalte method!");
  static_assert(
      noexport::is_one_of<typename alloc_traits::value_type, std::byte, char, unsigned char>::value);
  static_assert(has_method<destroy>, "Any requires aa::destroy method");
  static_assert(std::is_nothrow_copy_constructible_v<Alloc>, "C++ Standard requires it");

  using base_any_type = basic_any;
  // typedef only because msvc parser is ****
  typedef type_list<Methods<interface_t>...> methods_list;

  private:
  template <TTA...>
   struct remove_utility_methods {};

  template <TTA... Methods1>
  struct remove_utility_methods<destroy, Methods1...> {
    using ptr = poly_ptr<Methods1...>;
    using const_ptr = const_poly_ptr<Methods1...>;
    using ref = poly_ref<Methods1...>;
    using const_ref = const_poly_ref<Methods1...>;
  };

  using purified = remove_utility_methods<Methods...>;

 public:
  using ptr = typename purified::ptr;
  using ref = typename purified::ref;
  using cptr = typename purified::const_ptr;
  using cref = typename purified::const_ref;

  using const_ptr [[deprecated("use cptr")]] = cptr;
  using const_ref [[deprecated("user cref")]] = cref;

  constexpr ptr operator&() noexcept {
    return {this};
  }
  constexpr cptr operator&() const noexcept {
    return {this};
  }
  constexpr basic_any() = default;
  basic_any(Alloc alloc) noexcept : alloc(std::move(alloc)) {
  }

  constexpr ~basic_any() {
    if (has_value())
      destroy_value();
  }

  // basic_any copy/move stuff

  basic_any(const basic_any& other) requires(has_copy)
      : alloc(alloc_traits::select_on_container_copy_construction(other.alloc)) {
    if (other.has_value()) {
      if constexpr (std::is_empty_v<Alloc>)
        value_ptr = invoke<copy_with<Alloc, SooS>::template method>(other, value_ptr);
      else
        value_ptr = invoke<copy_with<Alloc, SooS>::template method>(other, value_ptr, alloc);
    }
    vtable_ptr = other.vtable_ptr;
  }

  [[nodiscard]] Alloc get_allocator() const noexcept {
    return alloc;
  }
  // postcondition: other do not contain a value after move
  basic_any(basic_any&& other) noexcept requires(has_method<move>) : alloc(std::move(other.alloc)) {
    move_value_from(std::move(other));
  }

  basic_any& operator=(basic_any&& other) noexcept requires(has_method<move>) {
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

  basic_any& operator=(const basic_any& other) requires(has_copy&& has_method<move>) {
    basic_any value{other};
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
  template <do_invokable<Methods...> V>
    requires(!any_x<V>)
  basic_any& operator=(V&& val)
    requires(has_method<move> ||
             (std::is_nothrow_constructible_v<std::decay_t<V>, V&&> && any_is_small_for<V>()))
  {
    if constexpr (std::is_nothrow_constructible_v<std::decay_t<V>, V&&> &&
                  any_is_small_for<V>()) {
      reset();
      emplace_in_empty<std::decay_t<V>>(std::forward<V>(val));
    } else {
      basic_any temp{std::forward<V>(val)};
      std::destroy_at(this);
      std::construct_at(this, std::move(temp));
    }
    return *this;
  }

  // making from any other type

  // postconditions : has_value() == true, *this is empty if exception thrown
  template <do_invokable<Methods...> T, typename... Args>
  std::decay_t<T>& emplace(Args&&... args) noexcept(
      std::is_nothrow_constructible_v<std::decay_t<T>, Args&&...>&& any_is_small_for<T>()) {
    reset();
    emplace_in_empty<std::decay_t<T>>(std::forward<Args>(args)...);
    return *reinterpret_cast<std::decay_t<T>*>(value_ptr);
  }
  template <do_invokable<Methods...> T, typename U, typename... Args>
  std::decay_t<T>& emplace(std::initializer_list<U> list, Args&&... args) noexcept(
      std::is_nothrow_constructible_v<std::decay_t<T>, std::initializer_list<U>, Args&&...>&&
          any_is_small_for<T>()) {
    reset();
    emplace_in_empty<std::decay_t<T>>(list, std::forward<Args>(args)...);
    return *reinterpret_cast<std::decay_t<T>*>(value_ptr);
  }
  template <do_invokable<Methods...> T, typename... Args>
  basic_any(std::in_place_type_t<T>, Args&&... args) noexcept(
      std::is_nothrow_constructible_v<std::decay_t<T>, Args&&...>&& any_is_small_for<T>()) {
    emplace_in_empty<std::decay_t<T>>(std::forward<Args>(args)...);
  }
  template <do_invokable<Methods...> T, typename U, typename... Args>
  basic_any(std::in_place_type_t<T>, std::initializer_list<U> list, Args&&... args) noexcept(
      std::is_nothrow_constructible_v<std::decay_t<T>, std::initializer_list<U>, Args&&...>&&
          any_is_small_for<T>()) {
    emplace_in_empty<std::decay_t<T>>(list, std::forward<Args>(args)...);
  }

  template <do_invokable<Methods...> T>
  requires(!any_x<T>)
  basic_any(T&& value) noexcept(
      std::is_nothrow_constructible_v<std::decay_t<T>, T&&>&& any_is_small_for<T>())
      : basic_any(std::in_place_type<std::decay_t<T>>, std::forward<T>(value)) {
  }
  template <do_invokable<Methods...> T>
  basic_any(std::allocator_arg_t, Alloc alloc, T&& value) noexcept(
      std::is_nothrow_constructible_v<std::decay_t<T>, T&&>&& any_is_small_for<T>())
      : alloc(std::move(alloc)) {
    emplace_in_empty<std::decay_t<T>>(std::forward<T>(value));
  }

  // force allocate versions

  template <do_invokable<Methods...> T, typename... Args>
  basic_any(force_stable_pointers_t, std::in_place_type_t<T>, Args&&... args) noexcept(
      std::is_nothrow_constructible_v<std::decay_t<T>, Args&&...>&& any_is_small_for<T>()) {
    emplace_in_empty<std::decay_t<T>, force_stable_pointers_t>(std::forward<Args>(args)...);
  }
  template <do_invokable<Methods...> T, typename U, typename... Args>
  basic_any(force_stable_pointers_t, std::in_place_type_t<T>, std::initializer_list<U> list, Args&&... args) noexcept(
      std::is_nothrow_constructible_v<std::decay_t<T>, std::initializer_list<U>, Args&&...>&&
          any_is_small_for<T>()) {
    emplace_in_empty<std::decay_t<T>, force_stable_pointers_t>(list, std::forward<Args>(args)...);
  }
  template <do_invokable<Methods...>  T>
  requires(!any_x<T>)
  basic_any(force_stable_pointers_t, T&& value) noexcept(
      std::is_nothrow_constructible_v<std::decay_t<T>, T&&>&& any_is_small_for<T>())
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

  // returns descriptor for 'void' type if do not contain type
  descriptor_t type_descriptor() const noexcept {
      return has_value() ? noexport::get_type_descriptor(vtable_ptr) : descriptor_t{};
  }
  // returns true if poly_ptr/poly_ref to this basic_any will not be invalidated after move
  bool is_stable_pointers() const noexcept {
    return memory_allocated();
  }
  constexpr bool has_value() const noexcept {
    return vtable_ptr != nullptr;
  }
  // COMPARE

  // there are NO operator==(auto&&) or operator<=>(auto&&)
  // It can be more effective on runtime(if types not equal, then return false / unordered)
  // but:
  // * it will be less effective(useless branching) if you 90% sure its this type
  // * it will cause compilation time, obj file increasing
  [[nodiscard]] bool operator==(const basic_any& other) const
    requires(has_method<equal_to> || has_method<spaceship>)
  {
    auto desc = type_descriptor();
    if (desc != other.type_descriptor())
      return false;
    if (desc == descriptor_v<void>) [[unlikely]]
      return true;
    if constexpr (has_method<equal_to>)
      return invoke<equal_to>(*this, other.value_ptr);
    else
      return invoke<spaceship>(*this, other.value_ptr) == std::partial_ordering::equivalent;
  }

  std::partial_ordering operator<=>(const basic_any& other) const
    requires(has_method<spaceship>)
  {
    auto desc = type_descriptor();
    if (desc != other.type_descriptor())
      return std::partial_ordering::unordered;
    if (desc == descriptor_v<void>) [[unlikely]]
      return std::partial_ordering::equivalent;
    return invoke<spaceship>(*this, other.value_ptr);
  }
 private:

  // precodition - has_value() == false
  void move_value_from(basic_any&& other) {
    if (!other.has_value())
      return;
    // `move` is noexcept (invariant of small state)
    // `move` also 'relocate' i.e. calls dctor of value(for remove invoke<destroy> in future)
    if (!other.memory_allocated())
      invoke<move>(other, value_ptr);
    else {
      value_ptr = std::exchange(other.value_ptr, &other.data);
      std::construct_at(reinterpret_cast<std::size_t*>(data.data()),
                        *reinterpret_cast<std::size_t*>(other.data.data()));
    }
    vtable_ptr = std::exchange(other.vtable_ptr, nullptr);
  }

  bool memory_allocated() const {
    return value_ptr != &data;
  }
  size_t allocated_size() const {
    assert(has_value() && memory_allocated());
    // needs atleast sizeof(std::size_t) in buffer(SooS)
    // to store allocated size for passing it into deallocate(ptr, n)
    static_assert(SooS >= sizeof(std::size_t));
    // when allocates stores in size in unused buffer
    return *reinterpret_cast<const std::size_t*>(data.data());
  }
  // precondition: has_value
  void destroy_value() {
    invoke<destroy>(*this);
    if (memory_allocated()) {
      alloc_traits::deallocate(alloc, reinterpret_cast<alloc_pointer_type>(value_ptr), allocated_size());
      value_ptr = &data;
    }
  }
};

template<TTA... Methods>
constexpr poly_ptr<Methods...> const_pointer_cast(const_poly_ptr<Methods...> p) {
  return std::bit_cast<poly_ptr<Methods...>>(p);
}

// ################# OPERATORS (equality, compare, &, *) ##############################

// TODO sfinae на свои встроенные трейты очевидно.. Это сложно

// TODO static asserts test for constructible / comparable

// ######################## ACTION any_cast ########################

// generic version for every type
// Note: it can cast non-polymorphic types too, but only to
// them =) (int -> int)
// always casts to pointer to T or const T (nullptr if wrong dynamic type)
// example : any_cast<int>(x) -> int*
template <typename T, poly_traits Traits = anyany_poly_traits>
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

// specialization for anyany types
// any_cast<T>(any | any*) -> std::remove_cv_t<T> | T*
// any_cast<T&>(c|poly_ref) -> const|T&
// any_cast<T*>(c|poly_ptr) -> const|T*
template<typename T>
struct any_cast_fn<T, anyany_poly_traits> {
 private:
  using X = std::remove_reference_t<T>;

  template <typename Alloc, size_t SooS, TTA... Methods>
  static const X* any_cast_impl(const basic_any<Alloc, SooS, Methods...>* any) noexcept {
    // U already remove_cv
    if (any == nullptr || any->type_descriptor() != descriptor_v<T>)
      return nullptr;
    return std::launder(reinterpret_cast<const X*>((&*any).raw()));
  }
  template <typename Alloc, size_t SooS, TTA... Methods>
  static X* any_cast_impl(basic_any<Alloc, SooS, Methods...>* any) noexcept {
    if (any == nullptr || any->type_descriptor() != descriptor_v<T>)
      return nullptr;
    return std::launder(reinterpret_cast<X*>((&*any).raw()));
  }

 public:
  static_assert(!(std::is_array_v<T> || std::is_function_v<T> || std::is_void_v<T>),
                "Incorrect call, it will be always nullptr");
  template <any_x U>
  std::add_pointer_t<T> operator()(U* ptr) const noexcept {
    return any_cast_impl(static_cast<typename U::base_any_type*>(ptr));
  }
  template <any_x U>
  const X* operator()(const U* ptr) const noexcept {
    return any_cast_impl(static_cast<const typename U::base_any_type*>(ptr));
  }

  template <any_x U>
  decltype(auto) operator()(U&& any) const {
    auto* ptr = (*this)(std::addressof(any));
    if (!ptr)
      throw std::bad_cast{};
    // const T& + const U& == const
    // const T& + non const U& == const
    // non-const T& + const U& == const
    // non-const T& + non-const U& == non-const
    if constexpr (std::is_lvalue_reference_v<T>)
      return *ptr;
    else if constexpr (std::is_rvalue_reference_v<T> && std::is_rvalue_reference_v<U&&>)
      return std::move(*ptr);
    else if constexpr (std::is_rvalue_reference_v<U&&>)
      return std::remove_cvref_t<T>(std::move(*ptr));  // move value
    else
      return std::remove_cvref_t<T>(*ptr);  // copy value
  }

  template <TTA... Methods>
  const X* operator()(const_poly_ptr<Methods...> p) const noexcept {
    if (p == nullptr || p.type_descriptor() != descriptor_v<T>)
      return nullptr;
    return reinterpret_cast<const X*>(p.raw());
  }
  template <TTA... Methods>
  X* operator()(poly_ptr<Methods...> p) const noexcept {
    if (p == nullptr || p.type_descriptor() != descriptor_v<T>)
      return nullptr;
    return reinterpret_cast<X*>(p.raw());
  }
  template <TTA... Methods>
  std::conditional_t<std::is_rvalue_reference_v<T>, std::remove_cvref_t<T>,
                     std::conditional_t<std::is_reference_v<T>, T, std::remove_cv_t<T>>>
  operator()(poly_ref<Methods...> p) const {
    X* ptr = (*this)(&p);
    if (ptr == nullptr) [[unlikely]]
      throw std::bad_cast{};
    return *ptr;
  }
  // clang-format off
  template <TTA... Methods>
  std::conditional_t<std::is_reference_v<T>, const X&, std::remove_cv_t<T>>
  operator()(const_poly_ref<Methods...> p) const {
    // clang-format on
    const X* ptr = (*this)(&p);
    if (ptr == nullptr) [[unlikely]]
      throw std::bad_cast{};
    return *ptr;
  }
};

template<typename T, poly_traits Traits = anyany_poly_traits>
constexpr inline any_cast_fn<T, Traits> any_cast = {};

template <typename Alloc, size_t SooS, TTA... Methods>
using basic_any_with = basic_any<Alloc, SooS, destroy, Methods...>;

template<TTA... Methods>
using any_with = basic_any_with<std::allocator<std::byte>, default_any_soos, Methods...>;

#define trait(NAME, ... /*Signature like void(int, float)*/) trait_impl(, NAME, __VA_ARGS__)
#define const_trait(NAME, ... /*Signature like void(int, float)*/) trait_impl(const, NAME, __VA_ARGS__)

template<TTA... Methods>
using cref = const_poly_ref<Methods...>;

template<TTA... Methods>
using cptr = const_poly_ptr<Methods...>;

namespace noexport {

template <typename PolyPtr, typename ResultT = void, poly_traits Traits = anyany_poly_traits>
struct type_switch_impl {
 private:
  struct non_void {
    // no way to create it for user
    explicit non_void() = default;
  };
  using Result = std::conditional_t<std::is_void_v<ResultT>, non_void, ResultT>;

 public:
  constexpr explicit type_switch_impl(PolyPtr value) noexcept : value(std::move(value)) {
    assert(value != nullptr);
  }

  template <typename T, typename Fn>
  type_switch_impl& case_(Fn&& f) {
    if (result)
      return *this;
    if (auto* v = ::aa::any_cast<T, Traits>(value)) {
      if constexpr (std::is_void_v<ResultT>) {
        std::invoke(std::forward<Fn>(f), *v);
        result.emplace();
      } else {
        result = std::invoke(std::forward<Fn>(f), *v);
      }
    }
    return *this;
  }
  // If value is one of Ts... F invoked (invokes count <= 1)
  template <typename... Ts, typename Fn>
  type_switch_impl& cases(Fn&& f) {
    struct assert_ : std::type_identity<Ts>... {
    } assert_unique_types;
    (void)assert_unique_types;
    (case_<Ts>(std::forward<Fn>(f)), ...);
    return *this;
  }
  // As a default, invoke the given callable within the root value.
  template <typename Fn>
  [[nodiscard]] Result default_(Fn&& f) {
    if (result)
      return std::move(*result);
    return std::forward<Fn>(f)(*value);
  }
  // As a default, return the given value.
  [[nodiscard]] Result default_(Result v) {
    if (result)
      return std::move(*result);
    return v;
  }
  // explicitly says there are no default value
  // postcondition: return value setted if some 'case_' succeeded
  std::optional<Result> no_default() {
    return std::move(result);
  }

 private:
  // value for which switch created
  // invariant - initially always not null.
  PolyPtr value;
  // stored result and if it exist
  std::optional<Result> result = std::nullopt;
};

}  // namespace noexport

// ######################## ACTION type_switch for all polymorphic types ########################

// Returns instance of type which
// implements a switch-like dispatch statement for poly_ptr/const_poly_ptr
// using any_cast.
// Each `Case<T>` takes a callable to be invoked
// if the root value is a <T>, the callable is invoked with any_cast<T>(ValueInSwitch)
//
// usage example:
//  any_operation<Methods...> op = ...;
//  ResultType result = type_switch<ResultType>(op)
//    .Case<ConstantOp>([](ConstantOp op) { ... })
//    .Default([](const_poly_ref<Methods...> ref) { ... });
template<typename Result = void, poly_traits Traits = anyany_poly_traits, TTA... Methods>
constexpr auto type_switch(poly_ref<Methods...> p) noexcept {
  return noexport::type_switch_impl<poly_ptr<Methods...>, Result, Traits>{&p};
}
template <typename Result = void, poly_traits Traits = anyany_poly_traits, TTA... Methods>
constexpr auto type_switch(const_poly_ref<Methods...> p) noexcept {
  return noexport::type_switch_impl<const_poly_ptr<Methods...>, Result, Traits>{&p};
}

// call<Ret(Args...)>::method adds operator() with Args... to basic_any
// (similar to std::function<Ret(Args...)>)
// it supports different signatures:
// * Ret(Args...) const
// * Ret(Args...) noexcept
// * Ret(Args...) const noexcept
template <typename Signature>
struct call {};
// non-const version
template <typename Ret, typename... Args>
struct call<Ret(Args...)> {
  template <typename T>
  struct method {
    static Ret do_invoke(T& self, Args... args) {
      return self(static_cast<Args&&>(args)...);
    }
    template <typename CRTP>
    struct plugin {
      Ret operator()(Args... args) {
        auto& self = *static_cast<CRTP*>(this);
        return invoke<method>(self, static_cast<Args&&>(args)...);
      }
    };
  };
};
// const version
template <typename Ret, typename... Args>
struct call<Ret(Args...) const> {
  template <typename T>
  struct method {
    static Ret do_invoke(const T& self, Args... args) {
      return self(static_cast<Args&&>(args)...);
    }
    template <typename CRTP>
    struct plugin {
      Ret operator()(Args... args) const {
        auto& self = *static_cast<const CRTP*>(this);
        return invoke<method>(self, static_cast<Args&&>(args)...);
      }
    };
  };
};
// noexcept version
template <typename Ret, typename... Args>
struct call<Ret(Args...) noexcept> {
  template <typename T>
  struct method {
    static Ret do_invoke(T& self, Args... args) noexcept {
      return self(static_cast<Args&&>(args)...);
    }
    template <typename CRTP>
    struct plugin {
      Ret operator()(Args... args) noexcept {
        auto& self = *static_cast<CRTP*>(this);
        return invoke<method>(self, static_cast<Args&&>(args)...);
      }
    };
  };
};
// const noexcept version
template <typename Ret, typename... Args>
struct call<Ret(Args...) const noexcept> {
  template <typename T>
  struct method {
    static Ret do_invoke(const T& self, Args... args) noexcept {
      return self(static_cast<Args&&>(args)...);
    }
    template <typename CRTP>
    struct plugin {
      Ret operator()(Args... args) const noexcept {
        auto& self = *static_cast<const CRTP*>(this);
        return invoke<method>(self, static_cast<Args&&>(args)...);
      }
    };
  };
};

// Creates a Method from invocable object with given signature
// usage example:
// template<typename T>
// using Size = aa::from_callable<std::size_t(), std::ranges::size>::const_method<T>;
// using any_sized_range = aa::any_with<Size>;
template <typename Signature, auto>
struct from_callable {};

template <typename R, typename... Ts, auto Foo>
struct from_callable<R(Ts...), Foo> {
  template <typename T>
  struct method {
    static R do_invoke(T& self, Ts... args) {
      return Foo(self, static_cast<Ts&&>(args)...);
    }
  };
};
template <typename R, typename... Ts, auto Foo>
struct from_callable<R(Ts...) const, Foo> {
  template <typename T>
  struct method {
    static R do_invoke(const T& self, Ts... args) {
      return Foo(self, static_cast<Ts&&>(args)...);
    }
  };
};

template <TTA Method>
[[deprecated("use invoke")]] constexpr inline invoke_fn<Method> invoke_unsafe = {};
}  // namespace aa

namespace std {

// for satisfiying Method ::aa::hash
template<>
struct hash<::aa::interface_t> {
  size_t operator()(const ::aa::interface_t&) const {
    return 0;
  }
};
template <::aa::any_x T>
  requires(T::template has_method<::aa::hash>)
struct hash<T> {
  size_t operator()(const T& any) const noexcept {
    return any.has_value() ? aa::invoke<::aa::hash>(any) : 0;
  }
};
template <TTA... Methods>
  requires(::aa::vtable<Methods...>::template has_method<::aa::hash>)
struct hash<::aa::poly_ref<Methods...>> {
  size_t operator()(const ::aa::poly_ref<Methods...>& r) const noexcept {
    return aa::invoke<::aa::hash>(r);
  }
};
template <TTA... Methods>
  requires(::aa::vtable<Methods...>::template has_method<::aa::hash>)
struct hash<::aa::const_poly_ref<Methods...>> {
  size_t operator()(const ::aa::const_poly_ref<Methods...>& r) const noexcept {
    return aa::invoke<::aa::hash>(r);
  }
};

template <TTA... Methods>
struct hash<::aa::poly_ptr<Methods...>> {
  size_t operator()(const ::aa::poly_ptr<Methods...>& p) const noexcept {
    return ::std::hash<void*>{}(p.raw());
  }
};
template <TTA... Methods>
struct hash<::aa::const_poly_ptr<Methods...>> {
  size_t operator()(const ::aa::const_poly_ptr<Methods...>& p) const noexcept {
    return ::std::hash<void*>{}(p.raw());
  }
};

template <>
struct hash<::aa::descriptor_t> {
  size_t operator()(const ::aa::descriptor_t& v) const noexcept {
#ifdef AA_CANT_GET_TYPENAME
    return hash<const void*>{}(bit_cast<const void*>(v));
#else
    return hash<string_view>{}(bit_cast<const char*>(v));
#endif
  }
};

}  // namespace std
