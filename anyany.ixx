
//export module anyany;
//import<array>;
//import<tuple>;
//import<typeinfo>;
//import<cassert>;
#pragma once
#include <array>
#include <tuple>
#include <typeinfo>
#include <cassert>
#include <memory> // construct_at / destsroy_at
//export 
namespace anyany {

inline constexpr size_t npos = size_t(-1);

template<typename T, typename... Args>
concept one_of = (std::same_as<T, Args> || ...);

template <size_t I, typename... Args>
struct number_of_impl {
  static constexpr size_t value = npos;  // no such element in pack
};
template <size_t I, typename T, typename... Args>
struct number_of_impl<I, T, T, Args...> {
  static constexpr size_t value = I;
};
template <size_t I, typename T, typename First, typename... Args>
struct number_of_impl<I, T, First, Args...> {
  static constexpr size_t value = number_of_impl<I + 1, T, Args...>::value;
};
template <typename T, typename... Args>
inline constexpr size_t number_of_first = number_of_impl<0, T, Args...>::value;

template <typename T>
struct destroy {
  static void do_(const void* ptr) noexcept {
    std::destroy_at(reinterpret_cast<const T*>(ptr));
  }
};
template <typename T>
struct move {
  static void do_(void* src, void* dest) {
    std::construct_at(reinterpret_cast<T*>(dest), std::move(*reinterpret_cast<T*>(src)));
  }
};
template <typename T>
struct noexcept_move {
  static_assert(std::is_nothrow_move_constructible_v<T>);

  static void do_(void* src, void* dest) noexcept {
    std::construct_at(reinterpret_cast<T*>(dest), std::move(*reinterpret_cast<T*>(src)));
  }
};
template <typename T>
struct copy {
  static void do_(const void* src, void* dest) {
    std::construct_at(reinterpret_cast<T*>(dest), *reinterpret_cast<const T*>(src));
  }
};

template <typename T>
struct RTTI {
  static std::type_info do_(const void*) noexcept {
    return typeid(T);
  }
};
// TODO typeid
// Кстати так можно будет делать get не по номеру, а по типу (например destroy<T>)

template <typename T, template <typename> typename... Methods>
constexpr std::array<const void*, sizeof...(Methods)> vtable_for = {&Methods<T>::do_...};

// NO strong exception guarantee, empty after exception
// SOO == small object size(object that be constructed without allocate)
// Alloc must be for std::byte, allocate / deallocate only required
template <typename Alloc, size_t Soz, template <typename> typename... Methods>
struct any_base {
 private:
  // mutable for vtable_invoke in const methods
  alignas(std::max_align_t) mutable std::array<std::byte, Soz> data;
  const std::array<const void*, sizeof...(Methods)>* vtable_ptr = nullptr;

  // regardless Method is a template, do_ signature must be same for any valid T (as for virtual functions)
  template <template <typename> typename M>
  consteval static size_t number_of_method() {
    return number_of_first<M<int>, Methods<int>...>;
  }
  template <template <typename> typename M>
  consteval static bool has_method() {
    return number_of_method<M>() != npos;
  }

  //static_assert(one_of<std::declval<Alloc>().allocate(50), std::byte*, void*>);
  static_assert(has_method<destroy>(), "Any requires destructor!");
  static_assert((has_method<move>() + has_method<noexcept_move>()) == 1, "Any requires exactly one move");
  struct methods_must_be_unique : Methods<int>... {};

  template <typename T, typename... Args>
  void construct_and_remember(Args&&... args) noexcept(std::is_nothrow_constructible_v<T, Args&&...>) {
    std::construct_at(reinterpret_cast<T*>(my_value_ptr()), std::forward<Args>(args)...);
    vtable_ptr = &vtable_for<T, Methods...>;
  }

  template <template <typename> typename M>
  using signature_of = decltype(&M<int>::do_);

 protected:
  template <template <typename> typename Method, typename... Args>
  decltype(auto) vtable_invoke(Args&&... args) const {
    assert(vtable_ptr != nullptr);
    return reinterpret_cast<signature_of<Method>>((*vtable_ptr)[number_of_method<Method>()])(
        my_value_ptr(), std::forward<Args>(args)...);
  }

 public:
  constexpr any_base() = default;
  ~any_base() {
    reset_value();
  }

  // any_base copy/move stuff

  // clang bug - no constant expression call to consteval function in requires closure
  any_base(const any_base& other) requires(has_method<copy>()) {
    if (!other.has_value()) {
      vtable_ptr = nullptr;  // may be dont needed(already default)
      return;
    }
    other.vtable_invoke<copy>(my_value_ptr());
    vtable_ptr = other.vtable_ptr;
  }

  any_base& operator=(const any_base& other) requires(has_method<copy>()) {
    if (&other == this) [[unlikely]]
      return *this;
    reset();  // empty vtable_ptr, if copy exited with exception then *this is empty
    other.vtable_invoke<copy>(my_value_ptr());
    vtable_ptr = other.vtable_ptr;
    return *this;
  }
  any_base(any_base&& other) noexcept(has_method<noexcept_move>()) {
    if (!other.has_value()) {
      vtable_ptr = nullptr;  // may be dont needed(already default)
      return;
    }
    other.move_value_to(*this);
    vtable_ptr = other.vtable_ptr;
  }
  any_base& operator=(any_base&& other) noexcept(has_method<noexcept_move>()) {
    if (&other == this) [[unlikely]]
      return *this;
    reset();
    other.move_value_to(*this);
    vtable_ptr = other.vtable_ptr;
    return *this;
  }

  // making from any other type

  template <typename T, typename... Args>
  void emplace(Args&&... args) noexcept(std::is_nothrow_constructible_v<T, Args&&...>) {
    reset();
    if constexpr (sizeof(T) <= Soz) {
      construct_and_remember<T>(std::forward<Args>(args)...);
    } else {
      // TODO TODO!
      // СТОП! Я же могу вставить вместо типа unique ptr на тип лол! Ну или специальный тип который является
      // unqiue ptr(move only кстати)
      // TODO - any_big_handler что то типа юник поинтера, возможно с аллокатором, которое умеет в deep
      // copy(копируемый) и ведёт себя в целом как значение под ним ( == глубоко копируется, имеет копи, мув
      // конструктор + конструктор от значения(через inaplce_type) Всё.
      // + поддержку задания soo своего, через using наверное(по другому никак)
      // КСТАТИ можно же сделать с аллокатором и тогда НА КОМПИЛЯЦИИ узнать что кто то пытается вызвать
      // аллокацию и запретить её(таким образом получить не аллоцирующий any) опять же здесь хватит
      // MemoryResource а аллокатор кажется избыточным
    }
  }

  template <typename T, typename... Args>
  any_base(std::in_place_type_t<T>, Args&&... args) noexcept(std::is_nothrow_constructible_v<T, Args&&...>) {
    construct_and_remember<std::remove_reference_t<T>>(std::forward<Args>(args)...);
  }
  template <typename T>
  any_base(T&& value) noexcept(std::is_nothrow_constructible_v<std::remove_reference_t<T>, T&&>)
      : any_base(std::in_place_type<std::remove_reference_t<T>>, std::forward<T>(value)) {
  }

  template <typename T>
  any_base& operator=(T&& value) noexcept(std::is_nothrow_constructible_v<std::remove_reference_t<T>, T&&>) {
    emplace<std::remove_reference_t<T>>(std::forward<T>(value));
    return *this;
  }

  void reset() noexcept {
    reset_value();
    vtable_ptr = nullptr;
  }

  // observe

  bool has_value() const noexcept {
    return vtable_ptr != nullptr;
  }
  std::type_info type() noexcept requires(has_method<RTTI>()) {
    return vtable_invoke<RTTI>();
  }

 private :
     // strong exception guarantee
     void
     move_value_to(any_base& dest) noexcept(has_method<noexcept_move>()) {
    assert(has_value());
    if constexpr (has_method<noexcept_move>())
      vtable_invoke<noexcept_move>(dest.my_value_ptr());
    else {
      if (!dest.has_value())
        vtable_invoke<move>(dest.my_value_ptr());
      else {
        // do not corrupt destination value
        const auto old_bytes = dest.data;
        try {
          vtable_invoke<move>(dest.my_value_ptr());
        } catch (...) {
          dest.data = old_bytes;
          throw;
        }
      }
    }
  }
  void* my_value_ptr() const noexcept {
    return reinterpret_cast<void*>(data.data());
  }
  void reset_value() noexcept {
    if (has_value())
      vtable_invoke<destroy>();
  }
};

//static constexpr auto i = (sizeof(void*) - 1) * CHAR_BIT;
//
//template <template <typename> typename... Methods>
//using any_with = any_base<std::allocator<std::byte>, i, Methods...>;

}  // namespace anyany
