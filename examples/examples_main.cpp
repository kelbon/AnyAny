
#include "basic_usage.hpp"
#include "functional_paradigm.hpp"
#include "polyref.hpp"
#include "advanced.hpp"
#if __cplusplus >= 202002L
#include "visit_invoke_example.hpp"
#endif
int main() {
  example::use_drawables();
  example::advanced::use_drawables();
  example::use_functions();
  example::use_polyref();
  example::use_visitors();
#if __cplusplus >= 202002L
  example::use_multidispatch();
  example::advanced::use_multidispatch();
#endif
}