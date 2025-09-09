------------------------------------------------------------
-- KCS-Lua: Kansas City Standard Encoder/Decoder
-- Refactored with CRC32 + CLI config + --no-crc option
------------------------------------------------------------

-- Default configuration
local CONFIG = {
    SAMPLE_RATE = 44100,
    BAUD_RATE   = 1200,
    TONE_0      = 1200,
    TONE_1      = 2400,
    AMPLITUDE   = 30000,
    USE_CRC     = true   -- default: CRC enabled
}

------------------------------------------------------------
-- Logging
------------------------------------------------------------
local VERBOSE = true
local function log(level, ...)
    if VERBOSE or level ~= "debug" then
        io.stderr:write("[" .. level:upper() .. "] " .. table.concat({...}, " ") .. "\n")
    end
end

------------------------------------------------------------
-- Helpers
------------------------------------------------------------
local function safe_open(path, mode)
    local f, err = io.open(path, mode)
    if not f then
        log("error", "Cannot open file:", path, err)
        os.exit(1)
    end
    return f
end

local function write_le_int(n, bytes)
    local t = {}
    for i = 1, bytes do
        t[i] = string.char(n & 0xFF)
        n = n >> 8
    end
    return table.concat(t)
end

------------------------------------------------------------
-- CRC32
------------------------------------------------------------
local CRC32_POLY = 0xEDB88320
local CRC32_TABLE = {}
for i = 0, 255 do
    local crc = i
    for _ = 1, 8 do
        if (crc & 1) ~= 0 then
            crc = (crc >> 1) ~ CRC32_POLY
        else
            crc = crc >> 1
        end
    end
    CRC32_TABLE[i] = crc
end

local function crc32(str)
    local crc = 0xFFFFFFFF
    for i = 1, #str do
        local byte = str:byte(i)
        crc = (crc >> 8) ~ CRC32_TABLE[(crc ~ byte) & 0xFF]
    end
    return (~crc) & 0xFFFFFFFF
end

------------------------------------------------------------
-- Tone generation
------------------------------------------------------------
local function generate_tone(freq, length_samples)
    local samples = {}
    local two_pi_f = 2 * math.pi * freq
    for i = 0, length_samples - 1 do
        samples[#samples + 1] = math.floor(CONFIG.AMPLITUDE * math.sin(two_pi_f * i / CONFIG.SAMPLE_RATE))
    end
    return samples
end

------------------------------------------------------------
-- Encode/decode bits
------------------------------------------------------------
local function encode_byte(byte)
    local bits = {}
    bits[#bits + 1] = 0 -- start bit
    for i = 0, 7 do
        bits[#bits + 1] = (byte >> i) & 1
    end
    bits[#bits + 1] = 1 -- stop bit
    return bits
end

local function encode_data(data, tone_table, samples_per_bit)
    local samples = {}
    for i = 1, #data do
        local byte = data:byte(i)
        local bits = encode_byte(byte)
        for _, bit_val in ipairs(bits) do
            local tone_samples = tone_table[bit_val]
            for j = 1, #tone_samples do
                samples[#samples + 1] = tone_samples[j]
            end
        end
    end
    return samples
end

------------------------------------------------------------
-- WAV I/O
------------------------------------------------------------
local function save_wav(samples, filename)
    local f = safe_open(filename, "wb")
    local num_samples = #samples
    local byte_rate = CONFIG.SAMPLE_RATE * 2 -- mono, 16-bit

    f:write("RIFF")
    f:write(write_le_int(36 + num_samples * 2, 4))
    f:write("WAVE")

    f:write("fmt ")
    f:write(write_le_int(16, 4)) -- PCM
    f:write(write_le_int(1, 2))  -- PCM format
    f:write(write_le_int(1, 2))  -- mono
    f:write(write_le_int(CONFIG.SAMPLE_RATE, 4))
    f:write(write_le_int(byte_rate, 4))
    f:write(write_le_int(2, 2))  -- block align
    f:write(write_le_int(16, 2)) -- bits/sample

    f:write("data")
    f:write(write_le_int(num_samples * 2, 4))

    for i = 1, num_samples do
        local s = samples[i]
        if s < -32768 then s = -32768 end
        if s > 32767 then s = 32767 end
        f:write(string.char(s & 0xFF, (s >> 8) & 0xFF))
    end
    f:close()
end

local function load_wav(filename)
    local f = safe_open(filename, "rb")
    f:seek("set", 44)
    local samples, idx = {}, 1
    while true do
        local bytes = f:read(2)
        if not bytes or #bytes < 2 then break end
        local lo, hi = bytes:byte(1, 2)
        local val = lo + hi * 256
        if val >= 0x8000 then val = val - 0x10000 end
        samples[idx] = val
        idx = idx + 1
    end
    f:close()
    return samples
end

------------------------------------------------------------
-- Decode samples
------------------------------------------------------------
local function decode_samples(samples, samples_per_bit)
    local data_bytes = {}
    local bits_collected, byte_val, sample_pos = 0, 0, 1

    local function count_zero_crossings(start_sample, length)
        local count, prev_sign = 0, nil
        local end_sample = math.min(start_sample + length - 1, #samples)
        for i = start_sample, end_sample do
            local sample = samples[i]
            if not sample then return count end
            local sign = (sample >= 0) and 1 or -1
            if prev_sign and sign ~= prev_sign then count = count + 1 end
            prev_sign = sign
        end
        return count
    end

    while sample_pos + samples_per_bit - 1 <= #samples do
        local crossings = count_zero_crossings(sample_pos, samples_per_bit)
        local freq_estimate = (crossings / 2) * CONFIG.BAUD_RATE
        local bit_val = (math.abs(freq_estimate - CONFIG.TONE_0) < math.abs(freq_estimate - CONFIG.TONE_1)) and 0 or 1

        if bits_collected == 0 then
            if bit_val ~= 0 then
                sample_pos = sample_pos + math.floor(samples_per_bit / 2)
            else
                bits_collected, byte_val = 1, 0
                sample_pos = sample_pos + samples_per_bit
            end
        elseif bits_collected >= 1 and bits_collected <= 8 then
            byte_val = byte_val + bit_val * (2 ^ (bits_collected - 1))
            bits_collected = bits_collected + 1
            sample_pos = sample_pos + samples_per_bit
        elseif bits_collected == 9 then
            if bit_val == 1 then data_bytes[#data_bytes + 1] = byte_val end
            bits_collected = 0
            sample_pos = sample_pos + samples_per_bit
        end
    end

    local out = {}
    for _, v in ipairs(data_bytes) do
        out[#out + 1] = string.char(v)
    end
    return table.concat(out)
end

------------------------------------------------------------
-- File I/O helpers
------------------------------------------------------------
local function read_binary_file(path)
    local f = safe_open(path, "rb")
    local data = f:read("*all")
    f:close()
    return data
end

local function write_binary_file(path, data)
    local f = safe_open(path, "wb")
    f:write(data)
    f:close()
end

------------------------------------------------------------
-- CLI parsing
------------------------------------------------------------
local function parse_args(args)
    local opts, pos_args = {}, {}
    for _, a in ipairs(args) do
        local k, v = a:match("^%-%-(%w+)=?(.*)$")
        if k then
            if v == "" then v = true end
            opts[k] = tonumber(v) ~= nil and tonumber(v) or v
        else
            pos_args[#pos_args + 1] = a
        end
    end
    return opts, pos_args
end

------------------------------------------------------------
-- Main
------------------------------------------------------------
local function main(...)
    local args = {...}
    local opts, pos_args = parse_args(args)

    -- Override CONFIG
    for k, v in pairs(opts) do
        local key = k:upper()
        if key == "NO-CRC" or key == "NOCRC" then
            CONFIG.USE_CRC = false
            log("info", "CRC disabled by flag")
        elseif CONFIG[key] ~= nil then
            CONFIG[key] = v
            log("info", "Set", key, "=", v)
        end
    end

    if #pos_args < 3 then
        io.stderr:write("Usage:\n")
        io.stderr:write("  lua kcs.lua encode <input.bin> <output.wav> [--rate=48000 --baud=2400 --no-crc]\n")
        io.stderr:write("  lua kcs.lua decode <input.wav> <output.bin> [--rate=44100 --baud=1200 --no-crc]\n")
        os.exit(1)
    end

    local mode, input_path, output_path = pos_args[1], pos_args[2], pos_args[3]
    local samples_per_bit = math.floor(CONFIG.SAMPLE_RATE / CONFIG.BAUD_RATE + 0.5)

    local TONE_TABLE = {
        [0] = generate_tone(CONFIG.TONE_0, samples_per_bit),
        [1] = generate_tone(CONFIG.TONE_1, samples_per_bit)
    }

    if mode == "encode" then
        local data = read_binary_file(input_path)
        if CONFIG.USE_CRC then
            local crc = crc32(data)
            data = data .. write_le_int(crc, 4)
            log("info", "Appended CRC32 = 0x" .. string.format("%08X", crc))
        end
        local samples = encode_data(data, TONE_TABLE, samples_per_bit)
        save_wav(samples, output_path)
        log("info", "Encoded", #data, "bytes to", output_path)

    elseif mode == "decode" then
        local samples = load_wav(input_path)
        local decoded = decode_samples(samples, samples_per_bit)

        if CONFIG.USE_CRC then
            if #decoded < 4 then
                log("error", "Decoded file too short for CRC check")
                write_binary_file(output_path, decoded)
                return
            end

            local data, crc_bytes = decoded:sub(1, -5), decoded:sub(-4)
            local stored_crc = crc_bytes:byte(1) + (crc_bytes:byte(2) << 8) +
                               (crc_bytes:byte(3) << 16) + (crc_bytes:byte(4) << 24)
            local calc_crc = crc32(data)

            if stored_crc == calc_crc then
                log("info", "CRC32 check passed (0x" .. string.format("%08X", calc_crc) .. ")")
            else
                log("warn", "CRC32 check FAILED. Expected 0x" .. string.format("%08X", stored_crc) ..
                    ", got 0x" .. string.format("%08X", calc_crc))
            end

            write_binary_file(output_path, data)
        else
            write_binary_file(output_path, decoded)
            log("info", "CRC skipped by flag")
        end

        log("info", "Decoded WAV", input_path, "to", output_path)

    else
        log("error", "Unknown mode:", mode)
        os.exit(1)
    end
end

main(...)

