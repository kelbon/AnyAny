#pragma once

namespace aa::noexport {

// binds std::allocator so 'vector' has only one template arg(or error on clang)
template <typename T>
using vector = std::vector<T>;

// Just make 'insert' overloads for each T in Ts... for basic_variant_swarm
template <typename CRTP, typename Container>
struct inserter {
 private:
  constexpr Container& my_container() noexcept {
    return std::get<Container>(static_cast<CRTP*>(this)->containers);
  }

 public:
  using const_iter = typename Container::const_iterator;
  using value_t = std::ranges::range_value_t<Container>;
  static constexpr bool is_associative = requires(Container v, value_t value) {
                                           { v.insert(value) };
                                         };
  constexpr decltype(auto) insert(value_t&& value) {
    auto& c = my_container();
    if constexpr (is_associative)
      return c.insert(std::move(value));
    else
      return c.insert(c.end(), std::move(value));
  }
  constexpr decltype(auto) insert(const value_t& value) {
    auto& c = my_container();
    if constexpr (is_associative)
      return c.insert(value);
    else
      return c.insert(c.end(), value);
  }
  constexpr auto insert(std::initializer_list<value_t> init) {
    auto& c = my_container();
    if constexpr (is_associative)
      return c.insert(init.begin(), init.end());
    else
      return c.insert(std::ranges::end(c), init.begin(), init.end());
  }

  constexpr decltype(auto) erase(const_iter pos) {
    return my_container().erase(pos);
  }
  constexpr decltype(auto) erase(const_iter b, const_iter e) {
    return my_container().erase(b, e);
  }
};

#if defined(_MSC_VER)
#define AA_VS_MSVC_EBO __declspec(empty_bases)
#else
#define AA_VS_MSVC_EBO
#endif

template <typename CRTP, template <typename> typename Container, typename... Ts>
struct AA_VS_MSVC_EBO inserters : inserter<CRTP, Container<Ts>>... {
  using inserter<CRTP, Container<Ts>>::insert...;
  using inserter<CRTP, Container<Ts>>::erase...;
};

#undef AA_VS_MSVC_EBO

}  // namespace aa::noexport
