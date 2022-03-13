
//import anyany;
#include "anyany.ixx"
using namespace anyany;

#include <functional>

template <typename T>
struct execute_method {
  static void do_(void* exe, std::function<void()> foo) {
    reinterpret_cast<T*>(exe)->execute(std::move(foo));
  }
};
struct any_executor : any_base<std::allocator<std::byte>, 54, destroy, move, execute_method> {
  void execute(std::function<void()> f) {
    vtable_invoke<execute_method>(std::move(f));
  }
};

#include <iostream>

// TODO - may be либо настройку аллокатора дл€ Any, либо регул€цию soo дл€ него же.
// TODO - нека€ операци€ сложени€ виртуальных таблиц(и any то есть их методов, что то типа операции над
// концептами && ||) есть и те и другие методы или есть или те или другие
// TODO - после реализации any тут на основе этого сделать небольшой кодген в clang, который бы это переводил
// на уровень €зыковой фичи(а может и не надо, т.к. как либа достаточно годно выгл€дит)

// TODO - закончил на том что добавл€л поддержку Sozи аллокатора(не сделал ещЄ) (и надо подумать насчет construct из аллокатора юзать(тогда мемори ресурсы уже не подход€т))
// TODO - модулем сделать и разобратьс€ с cmake add_library / add_executable что мне нужно в kelcoro
struct SomeExe1 {
  SomeExe1(int) {
  }
  void execute(auto) {
    std::cout << "Hello1\n";
  }
};
struct SomeExe2 {
  SomeExe2(std::string) {
  }
  void execute(std::function<void()>) {
    std::cout << "Hello2\n";
  }
};
int main() {
  any_executor x;
  x.emplace<SomeExe1>(5);
  x.execute([] {});
  x.emplace<SomeExe2>("Hello world");
  x.execute([] {});
}