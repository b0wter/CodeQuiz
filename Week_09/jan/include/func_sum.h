#pragma once

#include <memory>
#include <string>
#include <list>
#include "derivable.h"

namespace Function {

  class Sum : public Derivable {
  public:

    Sum(std::string const&) = delete;
    Sum(Derivable_ptr&&, Derivable_ptr&&);
    Sum(std::list<Derivable_ptr>);
    virtual ~Sum() = default;

    double eval(double) const override;
    std::string derive() const override;
    std::string print() const override;
    std::string print_debug() const override;

  private:
    std::list<Derivable_ptr> summands;
    void append(std::string&, std::string const&) const;
  };
  //typedef std::unique_ptr<(.+)> .+_ptr;
  typedef Sum* Sum_ptr;
}