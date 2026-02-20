#include "bytecode.hpp"

#include <format>
#include <stdexcept>
#include <utility>

std::string Chunk::debug() {
  std::string message;
  size_t byteSize = bytes.size();
  for (size_t offset = 0; offset < byteSize;) {
    auto pair = _debugInstruction(offset);
    message += pair.first;
    offset = pair.second;
  }

  return message;
}

std::pair<std::string, size_t> Chunk::_debugInstruction(size_t offset) {
  std::string message = std::format("{: 4} ", offset);

  switch ((OpCode)bytes[offset]) {
  case OpCode::RETURN:
    message += "RETURN\n";
    return {message, offset + 1};
  }
  throw std::runtime_error("Unreachable");
}

Chunk::Chunk(std::vector<uint8_t> bytes) : bytes(bytes) {}

VM::VM(std::vector<Chunk> chunks) : _chunks(chunks) {
  for (Chunk &chunk : chunks) {
    // TODO: implement
    ignore(chunk);
  }
}
