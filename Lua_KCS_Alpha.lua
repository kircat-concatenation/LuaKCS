-- kcs_lua_full.lua
-- Full KCS encode/decode with WAV and file I/O
-- Uses Lua 5.3+ native bitwise operators, no bit32 dependency

local SAMPLE_RATE = 44100
local BAUD_RATE = 1200

local TONE_0 = 1200
local TONE_1 = 2400

local AMPLITUDE = 30000 -- max amplitude for 16-bit signed PCM
local samples_per_bit = math.floor(SAMPLE_RATE / BAUD_RATE + 0.5)

-- Generate samples for a single bit tone
local function generate_tone(freq, length_samples)
    local samples = {}
    local two_pi_f = 2 * math.pi * freq
    for i = 0, length_samples - 1 do
        samples[#samples+1] = math.floor(AMPLITUDE * math.sin(two_pi_f * i / SAMPLE_RATE))
    end
    return samples
end

-- Encode one byte to KCS bits (start bit + 8 data bits LSB first + stop bit)
local function encode_byte(byte)
    local bits = {}

    table.insert(bits, 0) -- start bit

    for i = 0, 7 do
        local b = (byte >> i) & 1
        table.insert(bits, b)
    end

    table.insert(bits, 1) -- stop bit

    return bits
end

-- Encode entire data string to KCS samples
local function encode_data(data)
    local samples = {}

    for i = 1, #data do
        local byte = data:byte(i)
        local bits = encode_byte(byte)
        for _, bit_val in ipairs(bits) do
            local tone_freq = (bit_val == 0) and TONE_0 or TONE_1
            local tone_samples = generate_tone(tone_freq, samples_per_bit)
            for _, s in ipairs(tone_samples) do
                table.insert(samples, s)
            end
        end
    end

    return samples
end

-- Write little-endian integers to WAV header
local function write_le_int(n, bytes)
    local t = {}
    for i = 1, bytes do
        t[i] = string.char(n & 0xFF)
        n = n >> 8
    end
    return table.concat(t)
end

-- Save samples as 16-bit mono PCM WAV file
local function save_wav(samples, filename)
    local f = assert(io.open(filename, "wb"))

    local num_samples = #samples
    local byte_rate = SAMPLE_RATE * 2 -- mono, 16-bit = 2 bytes/sample

    f:write("RIFF")
    f:write(write_le_int(36 + num_samples * 2, 4)) -- ChunkSize
    f:write("WAVE")

    -- fmt chunk
    f:write("fmt ")
    f:write(write_le_int(16, 4)) -- Subchunk1Size (PCM)
    f:write(write_le_int(1, 2))  -- AudioFormat (PCM)
    f:write(write_le_int(1, 2))  -- NumChannels (mono)
    f:write(write_le_int(SAMPLE_RATE, 4))
    f:write(write_le_int(byte_rate, 4))
    f:write(write_le_int(2, 2))  -- BlockAlign (NumChannels * BitsPerSample/8)
    f:write(write_le_int(16, 2)) -- BitsPerSample

    -- data chunk
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

-- Load WAV samples (assumes 16-bit mono PCM, 44-byte header)
local function load_wav(filename)
    local f = assert(io.open(filename, "rb"))
    f:seek("set", 44) -- skip standard WAV header

    local samples = {}
    local idx = 1

    while true do
        local bytes = f:read(2)
        if not bytes or #bytes < 2 then break end
        local lo, hi = bytes:byte(1, 2)
        local val = lo + hi * 256  -- Correct little-endian conversion
        if val >= 0x8000 then val = val - 0x10000 end -- signed 16-bit
        samples[idx] = val
        idx = idx + 1
    end

    f:close()
    return samples
end


-- Decode samples back to data bytes
local function decode_samples(samples)
    local data_bytes = {}
    local bits_collected = 0
    local byte_val = 0
    local sample_pos = 1

    local function count_zero_crossings(start_sample, length)
        local count = 0
        local prev_sign = nil
        local end_sample = math.min(start_sample + length - 1, #samples)
    
        for i = start_sample, end_sample do
            local sample = samples[i]
            if sample == nil then
                print("Warning: Nil sample at index", i)
                return count
            end
    
            local sign = (sample >= 0) and 1 or -1
    
            if prev_sign ~= nil and sign ~= prev_sign then
                count = count + 1
            end
    
            prev_sign = sign
        end
    
        return count
    end
    
    


    while sample_pos + samples_per_bit - 1 <= #samples do
        local crossings = count_zero_crossings(sample_pos, samples_per_bit)
        local freq_estimate = (crossings / 2) * BAUD_RATE

        local bit_val = (math.abs(freq_estimate - TONE_0) < math.abs(freq_estimate - TONE_1)) and 0 or 1

        if bits_collected == 0 then
            if bit_val ~= 0 then
                -- Bad start bit, skip half bit to resync
                sample_pos = sample_pos + math.floor(samples_per_bit / 2)
            else
                bits_collected = 1
                byte_val = 0
                sample_pos = sample_pos + samples_per_bit
            end
        elseif bits_collected >= 1 and bits_collected <= 8 then
            byte_val = byte_val + bit_val * (2 ^ (bits_collected - 1))
            bits_collected = bits_collected + 1
            sample_pos = sample_pos + samples_per_bit
        elseif bits_collected == 9 then
            if bit_val == 1 then -- valid stop bit
                table.insert(data_bytes, byte_val)
            end
            bits_collected = 0
            sample_pos = sample_pos + samples_per_bit
        end
    end

    local decoded_str = {}
    for _, v in ipairs(data_bytes) do
        decoded_str[#decoded_str + 1] = string.char(v)
    end
    return table.concat(decoded_str)
end

-- Read/write binary files
local function read_binary_file(path)
    local f = assert(io.open(path, "rb"))
    local data = f:read("*all")
    f:close()
    return data
end

local function write_binary_file(path, data)
    local f = assert(io.open(path, "wb"))
    f:write(data)
    f:close()
end

-- Main CLI
local function main(...)
    local args = {...}
    if #args < 3 then
        print("Usage:")
        print("  lua kcs_lua_full.lua encode <input.bin> <output.wav>")
        print("  lua kcs_lua_full.lua decode <input.wav> <output.bin>")
        os.exit(1)
    end

    local mode, input_path, output_path = args[1], args[2], args[3]

    if mode == "encode" then
        local data = read_binary_file(input_path)
        local samples = encode_data(data)
        save_wav(samples, output_path)
        print("Encoded", #data, "bytes to", output_path)

    elseif mode == "decode" then
        local samples = load_wav(input_path)
        local decoded = decode_samples(samples)
        write_binary_file(output_path, decoded)
        print("Decoded WAV", input_path, "to", output_path)

    else
        print("Unknown mode:", mode)
        os.exit(1)
    end
end

main(...)
