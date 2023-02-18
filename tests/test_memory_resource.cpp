
#include "memory_resource.hpp"

int main() {
  std::vector<int, aa::pmr::polymorphic_allocator<int>> vec(aa::pmr::new_delete_resource());
  vec.resize(100);
}