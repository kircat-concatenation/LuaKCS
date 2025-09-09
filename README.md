# KCS-Lua

A Lua implementation of the Kansas City Standard (KCS) audio cassette data format.  
This tool can encode binary files into FSK audio WAV files and decode them back into binary data.

---

## Features

- Encode binary files to 1200 baud KCS audio (`.wav`)
- Decode `.wav` files back to binary
- Generates standard WAV PCM (16-bit mono) files
- Simple command-line interface (CLI)
- Pure Lua implementation (no external dependencies)

---

## Requirements

- Lua 5.3 or newer (bitwise operators required)
- A system with audio playback/recording (for real cassette transfer)

---

## Usage

### Encode binary to KCS WAV
```bash
lua kcs.lua encode input.bin output.wav
