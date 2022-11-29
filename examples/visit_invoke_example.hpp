#pragma once

#include <version>

#if __cpp_lib_format >= 201907L
#include <format>

#include "visit_invoke.hpp"
#include <anyany.hpp>

namespace aa::example {

// common use cases for multidispatching are the interaction of several things,
// such as the intersection of geometric shapes or the collision of objects in games

// This example demonstrates multidispatching for space objects in some game

struct spaceship {
  int id = 0;
};
struct asteroid {
  std::string galaxy_name;
};
struct star {
  double mass = 3.14;
};

std::string spaceship_asteroid_case(const spaceship& s, const asteroid& a) {
  return std::format("collision between spaceship #{} and asteroid from galaxy {}", s.id, a.galaxy_name);
}
std::string spaceship_star_case(const spaceship& s, const star&) {
  return std::format("spaceship #{} burning in star!", s.id);
}

// clang-format off

// return type may be dedacted if all functions have same return type
constexpr inline auto space_objects_collision = aa::make_visit_invoke<
    spaceship_asteroid_case,
    spaceship_star_case,
    [](const star& a, const star& b) {
      return std::format("created blackhole with mass {}", a.mass + b.mass);
    },
    [](const spaceship& a, const spaceship& b) {
      return std::format("spaceship {} crashed into spaceship {}", a.id, b.id);                      
    }
>();

// clang-format on

// we dont need any Methods(Traits) here
// optional string returned, because may be no such function for such dynamic types
std::optional<std::string> spaceship_collision_example(aa::const_poly_ptr<> a, aa::const_poly_ref<> b) {
  return space_objects_collision.resolve(a, b);
}

void multidispatch_usage() {
  spaceship player{14};
  asteroid a{.galaxy_name = "Andromeda"};
  // values are type erased here, so it is runtime overload resolution
  std::optional result = spaceship_collision_example(&player, a);
  assert(result.has_value());
  assert(*result == "collision between spaceship #14 and asteroid from galaxy Andromeda");
}

// Another example - just demonstrates, that visit_invoke
// may accept any count of arguments and even different count of arguments

struct A {};
struct B {};
struct C {};

template<int Res, typename... Ts>
int foo(Ts... args) {
  return Res;
}

// clang-format off
constexpr inline auto wow_what = aa::make_visit_invoke<
    foo<0, B>,
    foo<1, A, B, C>,
    foo<2, A, C, B>,
    foo<3, C, B, A>,
    foo<4, C, A, B>,
    foo<5, C>,
    foo<6, A, B>
>();

void multidispatch_usage2() {
    A a;
    B b;
    C c;
    aa::poly_ref<> aref = a;
    aa::const_poly_ref<> bcref = b;
    aa::any_with<> cval = c;
    if(wow_what.resolve(cval, aref, bcref) != 4)
        throw false;
    if(wow_what.resolve(cval) != 5)
        throw false;
    // may accepts non-polymorphic types too
    if(wow_what.resolve(A{}, bcref) != 6)
        throw false;
    if(wow_what.resolve(B{}) != 0)
        throw false;
}

}  // namespace aa::example
#else // std::format is not supported
namespace aa::example {
void multidispatch_usage() {}
void multidispatch_usage2() {}
}
#endif