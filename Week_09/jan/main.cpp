#include <iostream>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <string.h>

#include "include/factory.h"
#include <gtest/gtest.h>

int main(int argc, char **argv) {
    if (argc > 1 && argv[1][0] == '-') {
        switch (argv[1][1]) {
            case 'd': {
                try {
                    auto const fn = Function::Factory::parse(argv[2]);
                    std::cout << fn->print();
                    std::cout << std::endl;
                    std::cout << fn->derive();
                } catch (...) {
                    std::cout << "Parsing error" << std::endl;
                }
            }
            break;
            default:
                std::cout << "Keine Angabe unter dieser Nummer. " << argv[1][1];
            
        }       
        return 0;
    } else {
        testing::InitGoogleTest(&argc, argv);
        return RUN_ALL_TESTS();
    }
}