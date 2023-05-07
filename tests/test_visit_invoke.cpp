#if __cplusplus >= 202002L 
#include <anyany/visit_invoke.hpp>
#include <anyany/utility.hpp>

#include <string>

struct spaceship {
  std::string s = "hello";
};
struct planet {
  int val = 150;
};

constexpr inline auto case_sp = [](const spaceship& x, const planet& y) -> std::string {
  if (x.s != "hello" || y.val != 150)
    throw false;
  return "sp";
};
constexpr inline auto case_ps = [](const planet& x, const spaceship& y) -> std::string {
  if (x.val != 150 || y.s != "hello")
    throw false;
  return "ps";
};

constexpr inline auto collisions = aa::make_visit_invoke<std::string>(
    case_sp, [](spaceship&, const spaceship&) { return std::string("ss"); }, case_ps,
    [](planet a, planet& b) {
      if (a.val != 150 || b.val != 150)
        throw false;
      return std::string("pp");
    },
    [](int&&) { return std::string("int"); });
constexpr inline auto vars_collision = aa::make_visit_invoke<std::string, aa::std_variant_poly_traits>(
    case_sp, case_ps, [](int) { return "int"; });

int main() {
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
  aa::const_poly_ref<aa::type_info> space_ref1 = space_ship;
  int fake_space = 0;
  aa::const_poly_ptr<aa::type_info> space_ref2 = &space_planet;
  if (collisions.resolve(5, 5) != std::nullopt)
    return -1;
  if (collisions.resolve(5, space_ref1) != std::nullopt)
    return -2;
  if (*collisions.resolve(space_ref1, space_ref2) != "sp")
    return -3;
  if (collisions.resolve(space_ref1, aa::poly_ptr<aa::type_info>(&fake_space)) != std::nullopt)
    return -4;
  if (collisions.resolve(aa::poly_ref<aa::type_info>(fake_space)) != "int")
    return -5;
}
#else
int main() {}
#endif