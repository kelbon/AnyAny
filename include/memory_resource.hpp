#pragma once

#include "anyany.hpp"

#include <atomic>
#include <new>

namespace aa::noexport {

template <typename>
constexpr inline bool always_true = true;

}

namespace aa::pmr {
// TODO - беру мелкий пример из стд либы, переписываю ресурс сюда в невиртуальной форме,
// TODO - any_memory_resource на аниани
// TODO - сделать аллокатор с шаблонным ресурсом (нужно как то сделать поинтер на него... трейты или чет
// такое)
// TODO - вставить в аллокатор с предыдущего шага any_memory_resource
// TODO - сравнение сгенерированного кода/бенчмарки

template <typename T>
concept memory_resource = std::equality_comparable<T> &&
                          requires(T value, void* p, std::size_t sz, std::size_t align) {
                            { value.allocate(sz) } -> std::same_as<void*>;
                            { value.allocate(sz, align) } -> std::same_as<void*>;
                            { value.deallocate(p, sz) } -> std::same_as<void>;
                            { value.deallocate(p, sz, align) } -> std::same_as<void>;
                            { value == value } noexcept -> std::convertible_to<bool>;
                          };

// anyany Methods

template <typename T>
struct Allocate {
  static void* do_invoke(T& self, std::size_t sz, std::size_t align) {
    static_assert(memory_resource<T>);
    return self.allocate(sz, align);
  }
  template <typename CRTP>
  struct plugin {
    void* allocate(std::size_t sz, std::size_t align = alignof(std::max_align_t)) {
      return invoke_unsafe<Allocate>(*static_cast<CRTP*>(this), sz, align);
    }
  };
};

template <typename T>
struct Deallocate {
  static void do_invoke(T& self, void* p, std::size_t sz, std::size_t align) {
    static_assert(memory_resource<T>);
    return self.deallocate(p, sz, align);
  }
  template <typename CRTP>
  struct plugin {
    void deallocate(void* p, std::size_t sz, std::size_t align = alignof(std::max_align_t)) {
      invoke_unsafe<Deallocate>(*static_cast<CRTP*>(this), p, sz, align);
    }
  };
};

// TODO allocate atleast
using any_memory_resource = aa::any_with<Allocate, Deallocate, equal_to>;

// TODO UB resource?) ub on allocate(unreachable) (and deallocate too)(and always equal)
// never_used_resource

struct null_memory_resource {
  [[noreturn]] void* allocate(std::size_t, std::size_t = alignof(std::max_align_t)) const {
    throw std::bad_alloc{};
  }
  // TODO? не писать везде = alignof(std::max_align_t) и вместо этого внутри аниани метода вызывать с 3 аргументами?
  void deallocate(void*, std::size_t, std::size_t = alignof(std::max_align_t)) const noexcept {
  }
  constexpr bool operator==(const null_memory_resource& other) const noexcept {
    return this == &other;
  }
};
struct new_delete_resource_t {
  void* allocate(std::size_t sz, std::size_t align = alignof(std::max_align_t)) const {
    // fuck msvc if this is not working
    return ::operator new(sz, static_cast<std::align_val_t>(align));
  }
  void deallocate(void* p, std::size_t sz, std::size_t align = alignof(std::max_align_t)) const noexcept {
    ::operator delete(p, sz, static_cast<std::align_val_t>(align));
  }
  constexpr bool operator==(const new_delete_resource_t& other) const noexcept {
    return this == &other;
  }
};

namespace noexport {

// workaround for static constexpr variable in constexpr function before C++23
struct new_delete_res_t {
  static constexpr inline new_delete_resource_t value = {};
};

}  // namespace noexport

constexpr inline any_memory_resource::ref new_delete_resource() noexcept {
  // there are no mutable operations for this type, so its correct
  return *const_cast<new_delete_resource_t*>(&noexport::new_delete_res_t::value);
}

namespace noexport {

struct default_res_t {
  static inline constinit std::atomic<any_memory_resource::ptr> value =
      any_memory_resource::ptr(&new_delete_resource());
};

}  // namespace noexport

inline any_memory_resource::ref get_default_resource() noexcept {
  return *noexport::default_res_t::value.load(std::memory_order_acquire); // acquire
}
inline any_memory_resource::ref set_default_resource(any_memory_resource::ref ref) noexcept {
  return *noexport::default_res_t::value.exchange(&ref, std::memory_order_acq_rel);
}

// allocator

template <typename T = std::byte>
struct polymorphic_allocator {
 private:
  any_memory_resource::ref _res = get_default_resource();

 public:
  using value_type = T;

  constexpr polymorphic_allocator() noexcept = default;
  constexpr polymorphic_allocator(const polymorphic_allocator&) = default;
  template <typename Other>
  constexpr polymorphic_allocator(const polymorphic_allocator<Other>& other) noexcept
      : _res(other.resource()) {
  }
  constexpr polymorphic_allocator(any_memory_resource::ref res) noexcept : _res(res) {
  }
  void operator=(const polymorphic_allocator&) = delete;

  // TODO allocate atleast support? and in resources too?
  [[nodiscard]] T* allocate(std::size_t n) {
    return static_cast<T*>(resource().allocate(n * sizeof(T), alignof(T)));
  }
  void deallocate(T* p, std::size_t n) {
    resource().deallocate(p, n * sizeof(T), alignof(T));
  }
  constexpr any_memory_resource::ref resource() const noexcept {
    return _res;
  }
  // TODO остальные методы
  polymorphic_allocator select_on_container_copy_construction() const noexcept {
    return {};
  }
  bool operator==(const polymorphic_allocator&) const = default;
};


// TODO
template<>
struct polymorphic_allocator<std::byte> {

};
// TODO static assert any_memory_resource::ref is a memory resource

}  // namespace aa::pmr