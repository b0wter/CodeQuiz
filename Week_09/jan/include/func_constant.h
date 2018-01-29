#pragma once

#include "derivable.h"

namespace Function {

  class Constant : public Derivable {
  public:
    Constant(std::string const&);
    virtual ~Constant() = default;

    double eval(double) const override;
    std::string derive() const override;
    std::string print() const override;
    std::string print_debug() const override;

  private:
    double value;
  };
  //typedef std::unique_ptr<(.+)> .+_ptr;
typedef Constant* Constant_ptr; 
}