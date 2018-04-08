#include <memory>
#include "derivable.h"

namespace Function {
  namespace Factory {
    Derivable_ptr parse(std::string const& str);
  }
}
