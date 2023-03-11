#pragma once

#include "anyany.hpp"

#include <atomic>
#include <new>
#include <bit>

namespace aa::pmr {

template <typename T>
concept memory_resource =
    std::equality_comparable<T> && requires(T value, void* p, std::size_t sz, std::size_t align) {
                                     { value.allocate(sz) } -> std::same_as<void*>;
                                     { value.allocate(sz, align) } -> std::same_as<void*>;
                                     { value.deallocate(p, sz) } -> std::same_as<void>;
                                     { value.deallocate(p, sz, align) } -> std::same_as<void>;
                                     { value == value } -> std::convertible_to<bool>;
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
  [[noreturn]] static void* allocate(std::size_t, std::size_t = alignof(std::max_align_t)) {
    throw std::bad_alloc{};
  }
  // TODO? не писать везде = alignof(std::max_align_t) и вместо этого внутри аниани метода вызывать с 3
  // аргументами?
  static void deallocate(void*, std::size_t, std::size_t = alignof(std::max_align_t)) noexcept {
  }
  constexpr bool operator==(const null_memory_resource& other) const noexcept {
    return this == &other;
  }
};
struct new_delete_resource {
  static void* allocate(std::size_t sz, std::size_t align = alignof(std::max_align_t)) {
    // fuck msvc if this is not working
    return ::operator new(sz, std::bit_cast<std::align_val_t>(align));
  }
  static void deallocate(void* p, std::size_t sz,
                         std::size_t align = alignof(std::max_align_t)) noexcept {
    ::operator delete(p, sz, std::bit_cast<std::align_val_t>(align));
  }
  constexpr bool operator==(const new_delete_resource& other) const noexcept {
    return this == &other;
  }
};

// allocator

// TODO всё же нужно ресурс базед видимо... Хм. И интерфейс нужно другой нежели у стандартных немного
// в частности как можно ближе доносить реальный тип без стирания(в идеале стирания либо вовсе нет, либо только на входе в создание аллокатора)

// TODO
// Может ресурс ещё с N штуками ресурсов под 1 2 4 8 и т.д. байт + фалбек?
// template<memory_resource Fallback>
// span resource
// struct monothonic_buffer_resource {
// ...
// };
// TODO не писать свои ресурсы, а сделать только стирание и аллокатор с шаблонным ресурсом, так чтобы стандартные
// ресурсы можно было использовать(но компилятор понимал, что это их реальный тип и можно девиртуализировать)
// + поддержка allocate atleast
// TODO крч компилятор не оптимизирует, так что нужно типизировать, а на ребинде потенциально делать any?
// основная проблема это дефолт конструктор
template <typename T, memory_resource R>
struct polymorphic_allocator {
 private:
  [[no_unique_address]] R _res;

 public:
  using value_type = T;

  constexpr polymorphic_allocator() noexcept
    requires(std::default_initializable<R>)
  = default;
  constexpr polymorphic_allocator(const polymorphic_allocator&) = default;
  template <typename Other>
  constexpr polymorphic_allocator(const polymorphic_allocator<Other, R>& other) noexcept
      : _res(other.resource()) {
  }
  template<typename T>
    requires(std::convertible_to<R, T&&>)
  constexpr polymorphic_allocator(T&& res) noexcept : _res(std::forward<T>(res)) {
  }
  void operator=(const polymorphic_allocator&) = delete;

  // TODO allocate atleast support? and in resources too?
  [[nodiscard]] T* allocate(std::size_t n) {
    return static_cast<T*>(resource().allocate(n * sizeof(T), alignof(T)));
  }
  void deallocate(T* p, std::size_t n) {
    resource().deallocate(p, n * sizeof(T), alignof(T));
  }
  constexpr R& resource() noexcept {
    return _res;
  }
  constexpr const R& resource() const noexcept {
    return _res;
  }
  // TODO остальные методы
  // result 
  polymorphic_allocator select_on_container_copy_construction() const noexcept {
    return {};
  }
  bool operator==(const polymorphic_allocator&) const = default;
};

// TODO
//template<>
//struct polymorphic_allocator<std::byte> {
//
//};
// TODO static assert any_memory_resource::ref is a memory resource

}  // namespace aa::pmr