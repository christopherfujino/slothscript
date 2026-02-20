#include "test.hpp"
#include "bytecode.hpp"
#include <stdint.h>

using namespace _CHRIS_MONOREPO_CPP_TEST;

static std::vector<Test> tests = {
    {"RETURN is 0", []() { expect((int)OpCode::RETURN, 0); }},
    {"Chunk is just a vector",
     []() { expect((int)sizeof(Chunk), (int)sizeof(std::vector<void *>)); }},
    {"Create a RETURN chunk",
     []() {
       Chunk chunk{{(uint8_t)OpCode::RETURN}};
       expect((int)chunk.bytes.size(), 1);
       expect(chunk.debug(), std::string("   0 RETURN\n"));
     }},
};

int main() { return Runner(tests).run(); }
