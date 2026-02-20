#pragma once

#include <cstdint>
#include <limits>
#include <stddef.h> // for size_t
#include <string>
#include <utility> // for pair
#include <vector>

static_assert(sizeof(double) == 8, "double is not 64-bit");
static_assert(std::numeric_limits<double>::is_iec559,
              "double is not IEEE 754 compliant");

typedef double Value;

enum class OpCode : uint8_t {
  RETURN,
};

class Chunk {
public:
  Chunk(std::vector<uint8_t> bytes);

  std::string debug();

  std::vector<uint8_t> bytes;

private:
  std::pair<std::string, size_t> _debugInstruction(size_t offset);
};

template <typename T> inline void ignore(T) {}

class VM {
public:
  VM(std::vector<Chunk> chunks);

private:
  std::vector<Chunk> _chunks;
};
