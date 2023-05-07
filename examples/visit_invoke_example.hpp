#pragma once

#include <sstream>

#include <anyany/visit_invoke.hpp>
#include <anyany/anyany.hpp>

namespace example {

// common use cases for multidispatching are the interaction of several things,
// such as the intersection of geometric shapes or the collision of objects in games

// This example demonstrates multidispatching for space objects in some game

// let we have a game with 3 types: spaceship, asteroid, star
struct spaceship {
  int id = 0;
};
struct asteroid {
  std::string galaxy_name;
};
struct star {
  double mass = 3.14;
};
// and we have collision functions for them
constexpr inline auto ship_asteroid = [](const spaceship& s, const asteroid& a) {
  std::stringstream result;
  result << "collision between spaceship #" << s.id << " and asteroid from galaxy " << a.galaxy_name;
  return std::move(result).str();
};
constexpr inline auto ship_star = [](const spaceship& s, const star&) {
  std::stringstream result;
  result << "spaceship #" << s.id << " burning in star!";
  return std::move(result).str();
};
constexpr inline auto star_star = [](const star& a, const star& b) {
  std::stringstream result;
  result << "created blackhole with mass" << a.mass + b.mass;
  return std::move(result).str();
};
constexpr inline auto ship_ship = [](const spaceship& a, const spaceship& b) {
  std::stringstream result;
  result << "spaceship " << a.id << " crashed into spaceship " << b.id;
  return std::move(result).str();
};

// Create multidispacter
constexpr inline auto space_objects_collision =
    aa::make_visit_invoke<std::string>(ship_asteroid, ship_star, star_star, ship_ship);
// return type can be dedacted if all functions have same return type

void use_multidispatch() {
  // values are type erased here, so it is runtime overload resolution
  // we dont need any Methods(Traits) here, so <> (empty Methods pack) used
  aa::any_with<aa::type_info> player = spaceship{.id = 14};
  aa::any_with<aa::type_info> stone = asteroid{.galaxy_name = "Andromeda"};
  // optional string returned, because may be no such function for such dynamic types
  std::optional result = space_objects_collision.resolve(player, stone);
  assert(result == "collision between spaceship #14 and asteroid from galaxy Andromeda");
}

}  // namespace example

namespace example::advanced {

// Another example - just demonstrates, that visit_invoke
// may accept any count of functions and even functions with different count of arguments

struct A {};
struct B {};
struct C {};

template <int Res, typename... Ts>
constexpr inline auto foo = [](Ts... args) { return Res; };

constexpr inline auto wow_what = aa::make_visit_invoke<int>(
    foo<0, B>, foo<1, A, B, C>, foo<2, A, C, B>, foo<3, C, B, A>, foo<4, C, A, B>, foo<5, C>, foo<6, A, B>);

void use_multidispatch() {
  A a;
  B b;
  C c;
  aa::poly_ref<aa::type_info> aref = a;
  aa::const_poly_ref<aa::type_info> bcref = b;
  aa::any_with<aa::type_info> cval = c;
  if (wow_what.resolve(cval, aref, bcref) != 4)
    throw false;
  if (wow_what.resolve(cval) != 5)
    throw false;
  // may accepts non-polymorphic types too
  if (wow_what.resolve(A{}, bcref) != 6)
    throw false;
  if (wow_what.resolve(B{}) != 0)
    throw false;
}

}  // namespace example::advanced
