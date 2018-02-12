#pragma once

#include <memory>
#include <string>
#include <list>
#include "derivable.h"

namespace Function {

  class Product : public Derivable {
  public:

    Product(std::string const&) = delete;
    Product(Derivable_ptr&&, Derivable_ptr&&);
    Product(std::list<Derivable_ptr>);
    virtual ~Product() = default;

    std::string print() const override;
    std::string derive() const override;
    std::string print_debug() const override;

  protected:
    double eval(double) const override;
  
  private:
    std::list<Derivable_ptr> factors;
    void append_prod(std::string&, std::string const&) const;
    void append_sum(std::string&, std::string const&) const;
  };
  //typedef std::unique_ptr<(.+)> .+_ptr;
typedef Product* Product_ptr;
}