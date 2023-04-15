#include "visit_invoke_example.hpp"

struct spaceship {
  std::string s = "hello";
};
struct planet {
  int val = 150;
};

std::string case_sp(const spaceship& x, const planet& y) {
  if (x.s != "hello" || y.val != 150)
    throw false;
  return "sp";
}
std::string case_ps(const planet& x, const spaceship& y) {
  if (x.val != 150 || y.s != "hello")
    throw false;
  return "ps";
}

// clang-format off
constexpr inline auto collisions = aa::make_visit_invoke<
    case_sp,
    [](spaceship&, const spaceship&) { return std::string("ss"); },
    case_ps,
    [](planet a, planet& b) {
      if(a.val != 150 || b.val != 150)
        throw false;
      return std::string("pp");
    },
    [](int&&) {
        return std::string("int");
    }
>();
constexpr inline auto vars_collision = aa::make_visit_invoke<std::string,
    case_sp,
    case_ps,
    [](int) {
      return "int";
    }
>(aa::std_variant_poly_traits{});
// clang-format on

int main() {
  aa::example::multidispatch_usage();
  aa::example::multidispatch_usage2();
  using var_type = std::variant<int, spaceship, planet, double, char>;
  var_type var1 = spaceship{};
  const var_type var2 = planet{};
  if (*vars_collision.resolve(var1, var2) != "sp")
    return -11;
  if (!aa::any_cast<spaceship, aa::std_variant_poly_traits>(var1))
    return -12;
  if (!aa::any_cast<planet, aa::std_variant_poly_traits>(var2))
    return -13;
  if (!aa::any_cast<const spaceship, aa::std_variant_poly_traits>(var1))
    return -14;
  if (!aa::any_cast<const planet&, aa::std_variant_poly_traits>(var2))
    return -15;
  if (aa::any_cast<int, aa::std_variant_poly_traits>(var2))
    return -16;

  spaceship space_ship;
  planet space_planet;
  aa::const_poly_ref<> space_ref1 = space_ship;
  int fake_space = 0;
  aa::const_poly_ptr<> space_ref2 = &space_planet;
  if (collisions.resolve(5, 5) != std::nullopt)
    return -1;
  if (collisions.resolve(5, space_ref1) != std::nullopt)
    return -2;
  if (*collisions.resolve(space_ref1, space_ref2) != "sp")
    return -3;
  if (collisions.resolve(space_ref1, aa::poly_ptr<>(&fake_space)) != std::nullopt)
    return -4;
  if (collisions.resolve(aa::poly_ref<>(fake_space)) != "int")
    return -5;
}