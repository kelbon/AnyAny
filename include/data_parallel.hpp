#pragma once

#include <tuple>
#include <vector>
#include <cassert>
#include <compare>
#include <span>

#include "noexport/data_parallel_details.hpp"
// TODO rename vector_pack or smth? data_parallel_vector<T>?
namespace aa {

template <typename...>
struct data_parallel_impl {};

// std::vector have specialization for bools, this type behaves like bool and disables it
struct bool_ {
  bool b{};

  constexpr bool_() = default;
  constexpr bool_(const bool_&) = default;
  constexpr bool_& operator=(const bool_&) = default;
  constexpr bool_(bool_&&) = default;
  constexpr bool_& operator=(bool_&&) = default;

  constexpr bool_(bool x) noexcept : b(x) {
  }
  constexpr bool_& operator=(bool x) noexcept {
    b = x;
    return *this;
  }

  constexpr operator bool() const noexcept {
    return b;
  }
  constexpr bool operator==(const bool& x) const noexcept {
    return b == x;
  }
  constexpr bool operator==(const bool_&) const noexcept = default;
  constexpr auto operator<=>(const bool_&) const noexcept = default;
};
// disables specialization std::vector<bool>
// Its not in data_parallel_impl only because ofr MSVC bug (AGAIN)
template <typename X, typename Alloc>
struct container_for {
  using U = std::conditional_t<std::is_same_v<bool, X>, bool_, X>;
  using rebind_alloc = typename std::allocator_traits<Alloc>::template rebind_alloc<U>;
  using type = std::vector<U, rebind_alloc>;
};

template <typename T, typename Alloc, typename Traits, std::size_t... Is>
struct data_parallel_impl<T, Alloc, Traits, std::index_sequence<Is...>> {
  using value_type = T;
  using allocator_type = Alloc;
  using difference_type = std::ptrdiff_t;

  // it may be much prettier with second default arg-alias, but MSVC cant expand it in function declaration like 
  // foo(element_t<Is>...)
  template <std::size_t I>
  using element_t = std::conditional_t<
      std::is_same_v<bool, typename Traits::template tuple_element<I, T>>,
      bool_, typename Traits::template tuple_element<I, T>>;

  template<typename X>
  using container_for_t = typename container_for<X, Alloc>::type;

  template <std::size_t I>
  using container_t = container_for_t<element_t<I>>;

 protected:
  // invariant - all containers have same .size()
  std::tuple<container_t<Is>...> parts;

  // Iterator helpers
  struct proxy {
    std::tuple<element_t<Is>&...> ref;

    constexpr operator value_type() const noexcept
      requires(noexport::is_constructible_v<value_type, element_t<Is>...>)
    {
      // clang breaks on arg(a,b,c) is arg is aggregate (december 2022)
#if __cpp_aggregate_paren_init >= 201902L
      return std::make_from_tuple<value_type>(ref);
#else
      return value_type{std::get<Is>(ref)...};
#endif
    }
    template <typename U = value_type>
    constexpr const proxy& operator=(U&& val) const {
      // multi forward valid, because each field accesed only once
      if constexpr (std::is_same_v<std::remove_cvref_t<U>, T>) {
        ((std::get<Is>(ref) = Traits::template get<Is>(std::forward<U>(val))), ...);
      } else {
        value_type v = std::forward<U>(val);
        ((std::get<Is>(ref) = Traits::template get<Is>(std::move(v))), ...);
      }
      return *this;
    }

    // NOTE: behaves as defauled(=default) operator== for T
    // template for remove ambiguity
    template<std::same_as<value_type> X>
    constexpr bool operator==(const X& v) const {
      return ((std::get<Is>(ref) == Traits::template get<Is>(v)) && ...);
    }
    constexpr bool operator==(const proxy&) const = default;

    // NOTE: behaves as defauled(=default) operator<=> for T
    // TODO check working
    constexpr auto operator<=>(const value_type& v) const {
      using result_type =
          std::common_comparison_category_t<std::compare_three_way_result_t<element_t<Is>>...>;
      result_type cmp_res;
      auto is_equal_field = [&]<std::size_t I>(std::integral_constant<std::size_t, I>) {
        cmp_res = std::get<I>(ref) <=> Traits::template get<I>(v);
        return cmp_res == std::strong_ordering::equal;
      };
      bool unequal_finded = false;
      // finds first UNEQUAL fields and sets result of their comparison into 'cmp_res'
      ((unequal_finded || is_equal_field(std::integral_constant<std::size_t, Is>{}) || unequal_finded = true),
       ...);
      return cmp_res;
    }
  };
  struct proxy_ptr {
    proxy p;

    constexpr const proxy& operator*() const noexcept {
      return p;
    }
  };
  struct const_proxy {
    std::tuple<std::add_const_t<element_t<Is>>&...> ref;

    constexpr operator T() const noexcept
      requires(std::constructible_from<T, element_t<Is>...>)
    {
      return std::make_from_tuple<T>(ref);
    }
  };
  struct const_proxy_ptr {
    const_proxy p;

    constexpr const const_proxy& operator*() const noexcept {
      return p;
    }
  };

  // Iterator impl

  template <bool IsConst>
  struct iterator_ {
    using iters_t = std::tuple<
        std::ranges::iterator_t<std::conditional_t<IsConst, const container_t<Is>, container_t<Is>>>...>;

    iters_t iters;

    using value_type = T;
    using reference = std::conditional_t<IsConst, const_proxy, proxy>;
    using pointer = std::conditional_t<IsConst, const_proxy_ptr, proxy_ptr>;
    using difference_type = std::ptrdiff_t;
    using iterator_category = std::random_access_iterator_tag;

    constexpr iterator_() = default;
    constexpr iterator_(iters_t init) : iters(std::move(init)) {
    }
    constexpr iterator_(const iterator_&) = default;
    constexpr iterator_(iterator_&&) = default;
    constexpr iterator_& operator=(const iterator_&) = default;
    constexpr iterator_& operator=(iterator_&&) = default;

    // const iterator must be constructible from non-const
    // it is const & only because MSVC bug thinks its 'illegal copy ctor'
    constexpr iterator_(const iterator_<false>& it)
      requires(IsConst)
        : iters(std::make_from_tuple<decltype(iters)>(it.iters)) {
    }
    constexpr reference operator*() const {
      return std::apply([](auto&... its) { return reference({std::tie(*its...)}); }, iters);
    }
    constexpr iterator_& operator++() {
      std::apply([](auto&... its) { (++its, ...); }, iters);
      return *this;
    }
    constexpr iterator_ operator++(int) {
      auto copy = *this;
      ++(*this);
      return copy;
    }
    constexpr iterator_& operator--() {
      std::apply([](auto&... its) { (--its, ...); }, iters);
      return *this;
    }
    constexpr iterator_ operator--(int) {
      auto copy = *this;
      --(*this);
      return copy;
    }
    constexpr iterator_& operator+=(difference_type dif) noexcept {
      std::apply([&](auto&... its) { ((its += dif), ...); }, iters);
      return *this;
    }
    constexpr iterator_ operator+(difference_type dif) noexcept {
      auto copy = *this;
      return copy += dif;
    }
    constexpr iterator_& operator-=(difference_type dif) noexcept {
      std::apply([&](auto&... its) { ((its -= dif), ...); }, iters);
      return *this;
    }
    constexpr iterator_ operator-(difference_type dif) noexcept {
      auto copy = *this;
      return copy -= dif;
    }
    constexpr difference_type operator-(const iterator_& other) const noexcept {
      return std::distance(std::get<0>(other.iters), std::get<0>(iters));
    }
    constexpr reference operator[](difference_type dif) const noexcept {
      return *(*this + dif);
    }
    constexpr auto operator<=>(const iterator_&) const = default;
  };

 public:
  using size_type = std::size_t;//typename std::tuple_element_t<0, decltype(parts)>::size_type;
  using reference = proxy;
  using const_reference = const_proxy;

  // CONSTRUCTORS

  constexpr data_parallel_impl() = default;

  constexpr explicit data_parallel_impl(const allocator_type& alloc) noexcept
      : parts(container_t<Is>(alloc)...) {
    static_assert(std::is_nothrow_copy_constructible_v<allocator_type>, "C++ standard require it");
  }
  constexpr data_parallel_impl(size_type count, const value_type& value,
                               const allocator_type& alloc = allocator_type())
      : parts(container_t<Is>(count, Traits::template get<Is>(value), alloc)...) {
  }
  constexpr explicit data_parallel_impl(size_type count, const allocator_type& alloc = allocator_type())
      : parts(container_t<Is>(count, alloc)...) {
  }
  template <std::input_iterator It>
  constexpr data_parallel_impl(It first, It last, const allocator_type& alloc = allocator_type())
      : parts(container_t<Is>(alloc)...) {
    insert(begin(), first, last);
  }
  constexpr data_parallel_impl(const data_parallel_impl& other, const allocator_type& alloc)
      : parts(container_t<Is>(std::get<Is>(other.parts), alloc)...) {
  }
  constexpr data_parallel_impl(data_parallel_impl&& other, const allocator_type& alloc)
      : parts(container_t<Is>(std::get<Is>(std::move(other.parts)), alloc)...) {
  }
  constexpr data_parallel_impl(std::initializer_list<value_type> init,
                               const allocator_type& alloc = allocator_type())
      : parts(container_t<Is>(alloc)...) {
    insert(begin(), init);
  }

  // copy-move-assing-dctor

  constexpr data_parallel_impl(const data_parallel_impl&) = default;
  constexpr data_parallel_impl(data_parallel_impl&&) = default;
  constexpr data_parallel_impl& operator=(const data_parallel_impl&) = default;
  constexpr data_parallel_impl& operator=(data_parallel_impl&&) = default;
  constexpr ~data_parallel_impl() = default;

  constexpr data_parallel_impl& operator=(std::initializer_list<T> ilist) {
    assign(ilist);
    return *this;
  }

  // RANGE SUPPORT

  using iterator = iterator_<false>;
  using const_iterator = iterator_<true>;

  constexpr iterator begin() noexcept {
    return std::apply([](auto&... conts) { return iterator(std::tuple{conts.begin()...}); }, parts);
  }
  constexpr const_iterator begin() const noexcept {
    return std::apply([](auto&... conts) { return const_iterator(std::tuple{conts.begin()...}); }, parts);
  }
  constexpr iterator end() noexcept {
    return std::apply([](auto&... conts) { return iterator(std::tuple{conts.end()...}); }, parts);
  }
  constexpr const_iterator end() const noexcept {
    return std::apply([](auto&... conts) { return const_iterator(std::tuple{conts.end()...}); }, parts);
  }
  constexpr const_iterator cbegin() const noexcept {
    return begin();
  }
  constexpr const_iterator cend() const noexcept {
    return end();
  }

  // ELEMENT ACCESS

  constexpr reference front() noexcept {
    assert(!empty());
    return *begin();
  }
  constexpr const_reference front() const noexcept {
    assert(!empty());
    return *begin();
  }
  constexpr reference back() noexcept {
    assert(!empty());
    return *--end();
  }
  constexpr reference back() const noexcept {
    assert(!empty());
    return *--end();
  }
  constexpr reference operator[](size_type pos) {
    return *(begin() += pos);
  }
  constexpr const_reference operator[](size_type pos) const {
    return *(begin() += pos);
  }

  // OBSERVE

  constexpr size_type capacity() const noexcept {
    // allocate_atleast or similar things may allocate different count of capacity for different containers
    return std::apply([](auto&... conts) { return std::min({conts.capacity()...}); }, parts);
  }
  constexpr size_type max_size() const noexcept {
    return std::apply([](auto&... conts) { return std::min({conts.max_size()...}); }, parts);
  }

  // returns tuple of spans to underlying containers(so user cant break invariants of containers)
 private:
     template<typename X>
  std::span<X> get_span() noexcept {
    auto& c = std::get<container_for_t<X>>(parts);
    if constexpr (std::is_same_v<bool, X>)
      return std::span<bool>(reinterpret_cast<bool*>(c.data()), c.size());
    else
      return std::span(c);
  }
  template <typename X>
  std::span<const X> get_span() const noexcept {
    auto& c = std::get<container_for_t<X>>(parts);
    if constexpr (std::is_same_v<bool, X>)
      return std::span<bool>(reinterpret_cast<const bool*>(c.data()), c.size());
    else
      return std::span(c);
  }
  template <std::size_t I>
  auto get_span() noexcept {
    auto& c = std::get<I>(parts);
    if constexpr (std::is_same_v<bool, typename Traits::template tuple_element<I, value_type>>)
      return std::span<bool>(reinterpret_cast<bool*>(c.data()), c.size());
    else
      return std::span(c);
  }
  template <std::size_t I>
  auto get_span() const noexcept {
    auto& c = std::get<I>(parts);
    if constexpr (std::is_same_v<bool, typename Traits::template tuple_element<I, value_type>>)
      return std::span<bool>(reinterpret_cast<const bool*>(c.data()), c.size());
    else
      return std::span(c);
  }
 public:
  template <typename... Types>
  constexpr auto view() noexcept {
    return std::tuple(get_span<Types>()...);
  }
  template <typename... Types>
  constexpr auto view() const noexcept {
    return std::tuple(get_span<Types>()...);
  }
  template <std::size_t... Nbs>
  constexpr auto view() noexcept {
    return std::tuple(get_span<Nbs>()...);
  }
  template <std::size_t... Nbs>
  constexpr auto view() const noexcept {
    return std::tuple(get_span<Nbs>()...);
  }

  constexpr bool empty() const noexcept {
    // invariant - all containers have same size
    return std::get<0>(parts).empty();
  }
  constexpr size_type size() const noexcept {
    // invariant - all containers have same size
    return std::get<0>(parts).size();
  }
  // there are no operator <=> because its ambigious how to compare
  // by first field, second etc? lexicographical compare like vector<value_type>?
  // But we need to construct them first, which is inefficient

  constexpr bool operator==(const data_parallel_impl&) const = default;

  // MODIFIERS

  // emplace

  constexpr iterator emplace(const_iterator pos, element_t<Is>... fields) {
    return std::apply(
        [&](auto&... conts) {
          return iterator(std::tuple{conts.emplace(std::get<Is>(pos.iters), fields)...});
        },
        parts);
  }
  constexpr reference emplace_back(element_t<Is>... fields) {
    return std::apply([&](auto&... conts) { return reference{std::tie(conts.emplace_back(fields)...)}; },
                      parts);
  }

  // push_back

  constexpr void push_back(const value_type& v) {
    emplace_back(Traits::template get<Is>(v)...);
  }
  constexpr void push_back(value_type&& v) {
    // multi-invoke valid because each fild accesed only once
    emplace_back(Traits::template get<Is>(std::move(v))...);
  }

  // erase

  constexpr iterator erase(const_iterator pos) {
    return std::apply(
        [&](auto&... conts) { return iterator(std::tuple{conts.erase(std::get<Is>(pos.iters))...}); }, parts);
  }
  constexpr iterator erase(const_iterator b, const_iterator e) {
    return std::apply(
        [&](auto&... conts) {
          return iterator(std::tuple{conts.erase(std::get<Is>(b.iters), std::get<Is>(e.iters))...});
        },
        parts);
  }

  // insert

  constexpr iterator insert(const_iterator pos, const value_type& value) {
    return emplace(pos, Traits::template get<Is>(value)...);
  }

  constexpr iterator insert(const_iterator pos, value_type&& value) {
    // valid multi-move because each field accesed only once
    return emplace(pos, Traits::template get<Is>(std::move(value))...);
  }

  constexpr iterator insert(const_iterator pos, size_type count, const T& value) {
    return std::apply(
        [&](auto&... conts) {
          return iterator(
              std::tuple{conts.insert(std::get<Is>(pos.iters), count, Traits::template get<Is>(value))...});
        },
        parts);
  }

  template <std::input_iterator It>
  constexpr iterator insert(const_iterator pos, It first, It last) {
    // calculate it before pos invalidated
    auto diff = std::distance(cbegin(), pos);
    if (pos == end()) {
      for (; first != last; ++first)
        emplace_back(Traits::template get<Is>(*first)...);
      return begin() + diff;  // first inserted
    }
    if constexpr (std::random_access_iterator<It>) {
      reserve(size() + std::distance(first, last));
      pos = begin() + diff;  // restore pos after invalidating
    }
    if (first != last) [[likely]] {
      pos = emplace(pos, Traits::template get<Is>(value_type(*first))...);
      ++first;
    }
    for (; first != last; ++first) {
      // insert before next, renew pos
      value_type v = *first;
      pos = emplace(++pos, Traits::template get<Is>(std::move(v))...);
    }
    return begin() + diff;  // first inserted
  }

  constexpr iterator insert(const_iterator pos, std::initializer_list<value_type> ilist) {
    return insert(pos, ilist.begin(), ilist.end());
  }

  // assign

  constexpr void assign(size_type count, const value_type& value) {
    *this = data_parallel_impl(count, value);
  }

  template <std::input_iterator It>
  constexpr void assign(It first, It last) {
    *this = data_parallel_impl(first, last);
  }

  constexpr void assign(std::initializer_list<T> ilist) {
    *this = data_parallel_impl(ilist);
  }

  //

  constexpr void clear() noexcept {
    std::apply([](auto&... conts) { ((conts.clear()), ...); }, parts);
  }
  constexpr void pop_back() noexcept {
    std::apply([](auto&... conts) { ((conts.pop_back()), ...); }, parts);
  }
  constexpr void reserve(size_type new_cap) {
    std::apply([&](auto&... conts) { (conts.reserve(new_cap), ...); }, parts);
  }
  constexpr void resize(size_type sz) {
    std::apply([&](auto&... conts) { (conts.resize(sz), ...); }, parts);
  }
  constexpr void resize(size_type sz, const value_type& v) {
    std::apply([&](auto&... conts) { (conts.resize(sz, Traits::template get<Is>(v)), ...); }, parts);
  }
  constexpr void shrink_to_fit() {
    std::apply([](auto&... conts) { (conts.shrink_to_fit(), ...); }, parts);
  }
};

template <typename T, typename U>
concept tuple_traits_for = requires(U value) {
                             { T::template get<0>(value) };
                             //MSVC bug typename T::template tuple_element<0, U>;
                             { T::template tuple_size<U> } -> std::same_as<const std::size_t&>;
                           };

// Supports tuple-like objects(for which exist std::tuple_element/std::tuple_size)
// and aggregates with less then 30 fields
// TODO check for structs with C arrays in it(hmm, difference between C and std array...)
struct tuple_like_traits {
  template <typename T>
  static constexpr bool is_tuple_like = requires { typename std::tuple_element<0, T>::type; };

  // selects tuple_size if exist, otherwise calculates field count
  template <typename Tpl>
  static constexpr std::size_t tuple_size = [] {
    using T = std::remove_cvref_t<Tpl>;
    if constexpr (is_tuple_like<T>)
      return std::tuple_size_v<Tpl>;
    else if constexpr (std::is_aggregate_v<T>)
      return noexport::fields_count_v<T>;
    else
      static_assert(
          ![] {}, "T must be aggreagte or tuple-like!");
  }();  // INVOKED HERE
  template <std::size_t I, typename Tpl>
  using tuple_element = typename decltype([] {
    using T = std::remove_cvref_t<Tpl>;
    if constexpr (is_tuple_like<T>)
      return std::type_identity<std::tuple_element_t<I, T>>{};
    else if constexpr (std::is_aggregate_v<T>)
      return std::type_identity<noexport::field_type_t<I, T>>{};
    else
      static_assert(
          ![] {}, "T must be aggreagte or tuple-like!");
  }())::type;  // INVOKED HERE

  template <std::size_t I, typename Tpl>
  static constexpr decltype(auto) get(Tpl&& v) {
    if constexpr (is_tuple_like<std::remove_cvref_t<Tpl>>)
      return std::get<I>(std::forward<Tpl>(v));
    else
      return noexport::magic_get<I>(std::forward<Tpl>(v));
  }
};

// behaves as vector of T, but stores its fields separately
// (if T contains 'bool' behaves as normal non-specialized vector of bool)
// random_access_range (not contigious)
// NOTE: iterator::reference compares with T as if T has = default operator<=>
template <typename T, typename Alloc = std::allocator<T>, tuple_traits_for<T> Traits = tuple_like_traits>
struct data_parallel_vector
    : data_parallel_impl<T, Alloc, Traits, std::make_index_sequence<Traits::template tuple_size<T>>> {
 private:
  using base_t =
      data_parallel_impl<T, Alloc, Traits, std::make_index_sequence<Traits::template tuple_size<T>>>;

 public:
  using base_t::base_t;
  constexpr data_parallel_vector() = default;
  constexpr data_parallel_vector(const data_parallel_vector&) = default;
  constexpr data_parallel_vector(data_parallel_vector&&) noexcept = default;

  constexpr data_parallel_vector& operator=(std::initializer_list<T> init) {
    base_t::operator=(init);
    return *this;
  }
  constexpr data_parallel_vector& operator=(const data_parallel_vector&) = default;
  constexpr data_parallel_vector& operator=(data_parallel_vector&&) = default;

  constexpr void swap(data_parallel_vector& other) noexcept {
    /*
    Rly. MSVC writes here "'__this': undeclared identifier" omg.
    this bug with several packs from different contexts expanding was "FIXED" two fucking times by my report!
    using std::swap;
    std::apply(
        [&](auto&... conts) {
          std::apply([&](auto&... other_conts) { (swap(conts, other_conts), ...); }, other.parts);
        },
        this->parts);
    */
    [&]<std::size_t... Ms>(std::index_sequence<Ms...>) {
      using std::swap;
      (swap(std::get<Ms>(this->parts), std::get<Ms>(other.parts)), ...);
    }
    (std::make_index_sequence<Traits::template tuple_size<T>>{});  // INVOKED HERE
  }
  friend constexpr void swap(data_parallel_vector& a, data_parallel_vector& b) noexcept {
    a.swap(b);
  }

  constexpr bool operator==(const data_parallel_vector&) const = default;
};

}  // namespace aa

namespace std {

template <typename T, typename Alloc, typename Traits>
struct tuple_size<::aa::data_parallel_vector<T, Alloc, Traits>>
    : integral_constant<size_t, Traits::template tuple_size<T>> {};

template <size_t I, typename T, typename Alloc, typename Traits>
struct tuple_element<I, ::aa::data_parallel_vector<T, Alloc, Traits>> {
    using X = typename Traits::template tuple_element<I, T>;
    using type = std::span<X>;
};
template <size_t I, typename T, typename Alloc, typename Traits>
struct tuple_element<I, const ::aa::data_parallel_vector<T, Alloc, Traits>> {
    using X = typename Traits::template tuple_element<I, T>;
    using type = std::span<const X>;
};

// returns span

template <size_t I, typename T, typename Alloc, typename Traits>
constexpr auto get(::aa::data_parallel_vector<T, Alloc, Traits>& value) {
  return get<0>(value.template view<I>());
}
template <size_t I, typename T, typename Alloc, typename Traits>
constexpr auto get(const ::aa::data_parallel_vector<T, Alloc, Traits>& value) {
  return get<0>(value.template view<I>());
}
template <size_t I, typename T, typename Alloc, typename Traits>
constexpr auto get(::aa::data_parallel_vector<T, Alloc, Traits>&& value) {
  return get<0>(value.template view<I>());
}
template <size_t I, typename T, typename Alloc, typename Traits>
constexpr auto get(const ::aa::data_parallel_vector<T, Alloc, Traits>&& value) {
  return get<0>(value.template view<I>());
}

}  // namespace std