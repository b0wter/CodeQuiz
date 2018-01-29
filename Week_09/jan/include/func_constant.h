#pragma once

#include <string>
#include "derivable.h"

namespace Function {

  class Constant : public Derivable {
  public:
    Constant(std::string);
    virtual ~Constant();

    double eval(double) override;
    std::string derive() override;
    std::string print() override;
  };
}