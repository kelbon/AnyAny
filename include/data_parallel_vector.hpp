#pragma once

/*
  HEADER SYNOPSIS:

  * data_parallel_vector<T, Alloc>

    random_access_range, behaves as vector of T, but stores its fields separately
    (ignores specialization for bool),
    T may be:
        * tuple-like object (for which exist std::tuple_element/std::tuple_size/std::get)
        * aggregate with less then 30 fields (without const/C-array fields)

  * specializations std::tuple_size/std::tuple_element/std::get for data_parallel_vector
  It means:
    auto [a, b, c] = data_parallel_vector<T>{};
  will work and a, b, c - std::spans to separately stored fields of T

*/

#include <tuple>
#include <vector>
#include <cassert>
#include <compare>
#include <span>
#include <concepts>

#include "noexport/data_parallel_vector_details.hpp"

namespace aa::noexport {

template <typename...>
struct data_parallel_impl {};

template <typename T, typename Alloc, std::size_t... Is>
struct data_parallel_impl<T, Alloc, std::index_sequence<Is...>> {
  using value_type = T;
  using allocator_type = Alloc;
  using difference_type = std::ptrdiff_t;
  using size_type = std::vector<int>::size_type;
 protected:
  using Traits = noexport::tl_traits;

  template <std::size_t I>
  using element_t = typename Traits::template tuple_element<I, T>;

  // disables specialization std::vector<bool>
  template <typename X>
  struct container_for {
    using U = std::conditional_t<std::is_same_v<bool, X>, noexport::bool_, X>;
    using rebind_alloc = typename std::allocator_traits<Alloc>::template rebind_alloc<U>;
    using type = std::vector<U, rebind_alloc>;
  };
  template <typename X>
  using container_for_t = typename container_for<X>::type;

  template <std::size_t I>
  using container_t = container_for_t<element_t<I>>;

 protected:
  // invariant - all containers have same .size()
  std::tuple<container_t<Is>...> parts;

 private:
  // Iterator helpers
  struct proxy {
    // for tuple-like interface specializations
    using aa_proxy_tag = int;
    // returns tuple of references to fields of referenced obj
    constexpr auto tie() const noexcept {
      return std::tie(static_cast<element_t<Is>&>(std::get<Is>(owner->parts)[index])...);
    }
    template<size_t I>
    friend constexpr auto& get(const proxy& p) noexcept {
      return std::get<I>(p.tie());
    }
    data_parallel_impl* owner;
    size_type index;

    constexpr proxy(data_parallel_impl* owner, size_type index) noexcept : owner(owner), index(index) {
      assert(owner != nullptr && owner->size() > index);
    }
    constexpr operator value_type() const {
      // clang breaks on arg(a,b,c) if arg is aggregate (december 2022)
#if __cpp_aggregate_paren_init >= 201902L
      return value_type(static_cast<element_t<Is>&>(std::get<Is>(owner->parts)[index])...);
#else
      return value_type{static_cast<element_t<Is>&>(std::get<Is>(owner->parts)[index])...};
#endif
    }

    constexpr const proxy& operator=(value_type&& val) const {
      // multi move is valid, because each field accesed only once
      ((std::get<Is>(owner->parts)[index] = Traits::template get<Is>(std::move(val))), ...);
      return *this;
    }
    constexpr const proxy& operator=(const value_type& val) const {
      ((std::get<Is>(owner->parts)[index] = Traits::template get<Is>(val)), ...);
      return *this;
    }
    constexpr const proxy& operator=(const proxy& other) const {
      ((std::get<Is>(owner->parts)[index] = std::get<Is>(other.owner->parts)[other.index]), ...);
      return *this;
    }
    constexpr const proxy& operator=(proxy&& other) const {
      return *this = other;
    }
    // have friend access to owner->parts(so no error on gcc...)
    void swap(const proxy& other) const {
      using std::swap;
      (swap(std::get<Is>(owner->parts)[index], std::get<Is>(other.owner->parts)[other.index]), ...);
    }
    friend void swap(const proxy& l, const proxy& r) {
      l.swap(r);
    }

    constexpr auto compare_by_fields(const value_type& v) const {
      using result_type =
          std::common_comparison_category_t<std::compare_three_way_result_t<element_t<Is>>...>;
      result_type cmp_res = static_cast<result_type>(std::strong_ordering::equal);
      auto is_equal_field = [&]<std::size_t I>(std::integral_constant<std::size_t, I>) {
        cmp_res = std::get<I>(owner->parts)[index] <=> Traits::template get<I>(v);
        return cmp_res == std::strong_ordering::equal;
      };
      bool unequal_finded = false;
      // finds first UNEQUAL fields and sets result of their comparison into 'cmp_res'
      ((unequal_finded || is_equal_field(std::integral_constant<std::size_t, Is>{}) ||
        (unequal_finded = true)),
       ...);
      return cmp_res;
    }
    constexpr auto compare_by_fields(const proxy& other) const {
      using result_type =
          std::common_comparison_category_t<std::compare_three_way_result_t<element_t<Is>>...>;
      result_type cmp_res = static_cast<result_type>(std::strong_ordering::equal);
      auto is_equal_field = [&]<std::size_t I>(std::integral_constant<std::size_t, I>) {
        cmp_res = std::get<I>(owner->parts)[index] <=> std::get<I>(other.owner->parts)[other.index];
        return cmp_res == std::strong_ordering::equal;
      };
      bool unequal_finded = false;
      // finds first UNEQUAL fields and sets result of their comparison into 'cmp_res'
      ((unequal_finded || is_equal_field(std::integral_constant<std::size_t, Is>{}) ||
        (unequal_finded = true)),
       ...);
      return cmp_res;
    }

    // NOTE: compares by fields - behaves as defauled(=default) operator== / <=> for value_type

    // template removes ambiguity here
    template <std::same_as<value_type> X>
    constexpr bool operator==(const X& val) const {
      return (*this <=> val) == std::strong_ordering::equal;
    }

    constexpr bool operator==(const proxy& v) const {
      return (*this <=> v) == std::strong_ordering::equal;
    }
    constexpr auto operator<=>(const value_type& v) const {
      return compare_by_fields(v);
    }
    constexpr auto operator<=>(const proxy& v) const {
      return compare_by_fields(v);
    }
  };
  struct proxy_ptr {
    proxy p;

    constexpr const proxy& operator*() const noexcept {
      return p;
    }
  };
  struct const_proxy {
    // for tuple-like interface specializations
    using aa_const_proxy_tag = int;

    // returns tuple of const references to fields of referenced obj
    constexpr auto tie() const noexcept {
      return std::tie(static_cast<const element_t<Is>&>(std::get<Is>(std::as_const(p.owner)->parts)[p.index])...);
    }
    template <size_t I>
    friend constexpr const auto& get(const const_proxy& p) noexcept {
      return std::get<I>(p.p.tie());
    }
   private:
    proxy p;

   public:
    constexpr const_proxy(const data_parallel_impl* owner, size_type index) noexcept
        : p(const_cast<data_parallel_impl*>(owner), index) {
    }
    constexpr const_proxy(const proxy& p) noexcept : p(p) {
    }

    void operator=(const value_type&) = delete;
    void operator=(value_type&&) = delete;
    void operator=(const const_proxy&) = delete;
    void operator=(const_proxy&&) = delete;

    constexpr operator value_type() const {
      return static_cast<value_type>(p);
    }

    // NOTE: compares by fields - behaves as defauled(=default) operator== / <=> for value_type

    // template removes ambiguity here
    template <std::same_as<value_type> X>
    constexpr bool operator==(const X& val) const {
      return p == val;
    }

    constexpr bool operator==(const const_proxy& v) const {
      return p == v.p;
    }
    constexpr auto operator<=>(const value_type& v) const {
      return p <=> v;
    }
    constexpr auto operator<=>(const const_proxy& v) const {
      return p <=> v.p;
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
    using index_t = std::vector<int>::size_type;
    using owner_t = std::conditional_t<IsConst, const data_parallel_impl, data_parallel_impl>;

    owner_t* owner = nullptr;
    index_t index;

    using value_type = T;
    using reference = std::conditional_t<IsConst, const_proxy, proxy>;
    using pointer = std::conditional_t<IsConst, const_proxy_ptr, proxy_ptr>;
    using difference_type = std::ptrdiff_t;
    using iterator_category = std::random_access_iterator_tag;

    constexpr iterator_() = default;
    constexpr iterator_(owner_t* owner, index_t i) noexcept : owner(owner), index(i) {
      assert(owner && index <= owner->size());
    }
    constexpr iterator_(const iterator_&) = default;
    constexpr iterator_(iterator_&&) = default;
    constexpr iterator_& operator=(const iterator_&) = default;
    constexpr iterator_& operator=(iterator_&&) = default;

    // have friend access to owner->parts...
    constexpr value_type do_iter_move() const requires (!IsConst) {
      // clang breaks on arg(a,b,c) is arg is aggregate (december 2022)
#if __cpp_aggregate_paren_init >= 201902L
      return value_type(std::move(std::get<Is>(owner->parts)[index])...);
#else
      return value_type{std::move(std::get<Is>(owner->parts)[index])...};
#endif
    }
    // ranges::iter_move customization point object
    friend value_type iter_move(iterator_ it)
      requires(!IsConst)
    {
      return it.do_iter_move();
    }
    // have friend access to owner->parts...
    constexpr void do_iter_swap(iterator_ other) {
      using std::swap;
      ((swap(std::get<Is>(owner->parts)[index], std::get<Is>(other.owner->parts)[other.index])), ...);
    }
    // ranges::iter_swap customization point object
    friend constexpr void iter_swap(iterator_ l, iterator_ r)
      requires(!IsConst)
    {
      l.do_iter_swap(r);
    }
    // const iterator must be constructible from non-const
    // it is const & only because MSVC bug thinks its 'illegal copy ctor'
    constexpr iterator_(const iterator_<false>& it) noexcept 
      requires(IsConst)
        : owner(it.owner), index(it.index) {
    }
    constexpr reference operator*() const noexcept {
      return reference(owner, index);
    }
    constexpr iterator_& operator++() noexcept {
      assert(owner != nullptr && index != owner->size());
      ++index;
      return *this;
    }
    constexpr iterator_ operator++(int) noexcept {
      auto copy = *this;
      ++(*this);
      return copy;
    }
    constexpr iterator_& operator--() noexcept {
      assert(index != 0);
      --index;
      return *this;
    }
    constexpr iterator_ operator--(int) noexcept {
      auto copy = *this;
      --(*this);
      return copy;
    }
    constexpr iterator_& operator+=(difference_type dif) noexcept {
      assert(owner != nullptr);
      index += dif;
      return *this;
    }
    constexpr iterator_ operator+(difference_type dif) const noexcept {
      auto copy = *this;
      copy += dif;
      return copy;
    }
    friend constexpr iterator_ operator+(difference_type dif, const iterator_& it) noexcept {
      return it + dif;
    }
    constexpr iterator_& operator-=(difference_type dif) noexcept {
      assert(dif <= index);
      index -= dif;
      return *this;
    }
    constexpr iterator_ operator-(difference_type dif) const noexcept {
      auto copy = *this;
      copy -= dif;
      return copy;
    }
    constexpr difference_type operator-(const iterator_& other) const noexcept {
      return index - other.index;
    }
    constexpr reference operator[](difference_type dif) const noexcept {
      return *(*this + dif);
    }
    constexpr auto operator==(std::default_sentinel_t) const noexcept {
      return !owner || owner->size() == index;
    }
    constexpr bool operator==(const iterator_& other) const noexcept {
      assert(owner == other.owner);
      return index == other.index;
    }
    constexpr auto operator<=>(const iterator_& other) const noexcept {
      assert(owner == other.owner);
      return index <=> other.index;
    };
  };

 public:
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
    return iterator(this, 0);
  }
  constexpr const_iterator begin() const noexcept {
    return const_iterator(this, 0);
  }
  constexpr iterator end() noexcept {
    return iterator(this, size());
  }
  constexpr const_iterator end() const noexcept {
    return const_iterator(this, size());
  }
  constexpr const_iterator cbegin() const noexcept {
    return const_iterator(this, 0);
  }
  constexpr const_iterator cend() const noexcept {
    return const_iterator(this, size());
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
  template <typename X>
  std::span<X> get_span() noexcept {
    auto& c = std::get<container_for_t<std::remove_const_t<X>>>(parts);
    if constexpr (std::is_same_v<bool, X>)
      return std::span<bool>(reinterpret_cast<bool*>(c.data()), c.size());
    else if constexpr (std::is_same_v<const bool, X>)
      return std::span<const bool>(reinterpret_cast<const bool*>(c.data()), c.size());
    else
      return std::span<X>(c);
  }
  template <typename X>
  std::span<const X> get_span() const noexcept {
    auto& c = std::get<container_for_t<std::remove_const_t<X>>>(parts);
    if constexpr (std::is_same_v<bool, std::remove_const_t<X>>)
      return std::span<const bool>(reinterpret_cast<const bool*>(c.data()), c.size());
    else
      return std::span<const X>(c);
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
      return std::span<const bool>(reinterpret_cast<const bool*>(c.data()), c.size());
    else
      return std::span(c);
  }

 public:
  template <typename X>
  constexpr std::span<X> view_only() noexcept {
    return get_span<X>();
  }
  template<typename X>
  constexpr std::span<const X> view_only() const noexcept {
    return get_span<X>();
  }
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
  // note: compares T values by fields
  constexpr bool operator==(const data_parallel_impl& other) const
    requires((std::equality_comparable<element_t<Is>> && ...))
  {
    return std::ranges::equal(*this, other);
  }

  // MODIFIERS
 private:
  static constexpr std::size_t last_index = (Is, ...);
  constexpr iterator iter_from_last_iter(typename container_t<last_index>::iterator last_it) noexcept {
    return iterator(this, std::distance(std::get<last_index>(parts).begin(), last_it));
  }
  constexpr const_iterator iter_from_last_iter(
      typename container_t<last_index>::const_iterator last_it) const noexcept {
    return const_iterator(this, std::distance(std::get<last_index>(parts).begin(), last_it));
  }

 public:
  // emplace

  constexpr iterator emplace(const_iterator pos, element_t<Is>... fields) {
    std::apply([&](auto&... conts) { (conts.emplace(conts.begin() + pos.index, fields), ...); }, parts);
    return iterator(this, pos.index);
  }
  constexpr reference emplace_back(element_t<Is>... fields) {
    std::apply([&](auto&... conts) { (conts.emplace_back(fields), ...); }, parts);
    return back();
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
    auto last_it =
        std::apply([&](auto&... conts) { return (conts.erase(conts.begin() + pos.index), ...); }, parts);
    return iter_from_last_iter(last_it);
  }
  constexpr iterator erase(const_iterator b, const_iterator e) {
    auto last_it = std::apply(
        [&](auto&... conts) { return (conts.erase(std::get<Is>(b.iters), std::get<Is>(e.iters)), ...); },
        parts);
    return iter_from_last_iter(last_it);
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
    auto last_it = std::apply(
        [&](auto&... conts) {
          return (conts.insert(conts.begin() + pos.index, count, Traits::template get<Is>(value)), ...);
        },
        parts);
    return iter_from_last_iter(last_it);
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

}  // namespace aa::noexport

namespace aa {

// random_access_range, behaves as vector of T, but stores fields separately
//
// NOTES:
// * T will be compared by fields by default(similar to default operator<=>)
// * ignores specialization vector<bool>
// * ::reference is a proxy object convertible to T
// * supports tuple-like objects and aggregates with less then 30 fields
// * aggregate with C array in it is BAD(use std::array)
template <typename T, typename Alloc = std::allocator<T>>
struct data_parallel_vector
    : noexport::data_parallel_impl<T, Alloc,
                                   std::make_index_sequence<noexport::tl_traits::template tuple_size<T>>> {
 private:
  using tl_traits = noexport::tl_traits;
  using base_t =
      noexport::data_parallel_impl<T, Alloc, std::make_index_sequence<tl_traits::template tuple_size<T>>>;

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
    this->parts.swap(other.parts);
  }
  friend constexpr void swap(data_parallel_vector& a, data_parallel_vector& b) noexcept {
    a.swap(b);
  }

  constexpr bool operator==(const data_parallel_vector&) const
    requires(std::equality_comparable<base_t>)  // weird gcc behavior with = default operators
  = default;
};
// helper concept
template<typename T>
concept dp_vector_ref = requires { typename std::remove_cvref_t<T>::aa_proxy_tag; } ||
                        requires { typename std::remove_cvref_t<T>::aa_const_proxy_tag; };

}  // namespace aa

namespace std {

template <typename T, typename Alloc>
struct tuple_size<::aa::data_parallel_vector<T, Alloc>>
    : integral_constant<size_t, ::aa::noexport::tl_traits::template tuple_size<T>> {};

template <size_t I, typename T, typename Alloc>
struct tuple_element<I, ::aa::data_parallel_vector<T, Alloc>> {
  using X = typename ::aa::noexport::tl_traits::template tuple_element<I, T>;
  using type = std::span<X>;
};
template <size_t I, typename T, typename Alloc>
struct tuple_element<I, const ::aa::data_parallel_vector<T, Alloc>> {
  using X = typename ::aa::noexport::tl_traits::template tuple_element<I, T>;
  using type = std::span<const X>;
};

// returns span

template <size_t I, typename T, typename Alloc>
constexpr auto get(::aa::data_parallel_vector<T, Alloc>& value) {
  return get<0>(value.template view<I>());
}
template <size_t I, typename T, typename Alloc>
constexpr auto get(const ::aa::data_parallel_vector<T, Alloc>& value) {
  return get<0>(value.template view<I>());
}
template <size_t I, typename T, typename Alloc>
constexpr auto get(::aa::data_parallel_vector<T, Alloc>&& value) {
  return get<0>(value.template view<I>());
}
template <size_t I, typename T, typename Alloc>
constexpr auto get(const ::aa::data_parallel_vector<T, Alloc>&& value) {
  return get<0>(value.template view<I>());
}

// specializations for data_parallel_vector<T>::reference/const_reference

template <::aa::dp_vector_ref T>
struct tuple_size<T> : tuple_size<decltype(std::declval<T>().tie())> {};

template <size_t I, ::aa::dp_vector_ref T>
struct tuple_element<I, T> : tuple_element<I, decltype(std::declval<T>().tie())> {};

}  // namespace std