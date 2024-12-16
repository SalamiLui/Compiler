#pragma once
#include <functional>
#include <cstdlib>

class ArenaAllocator {
public:
    inline explicit ArenaAllocator(size_t const bytes) : m_size(bytes) {
        m_buffer = static_cast<std::byte*>(malloc(m_size));
        m_offset = m_buffer;
    }

    template <typename  T>
    inline T* alloc() {
        void* offset = m_offset;
        m_offset += sizeof(T);
        return static_cast<T*>(offset);

    }

    inline ArenaAllocator(const ArenaAllocator &) = delete;
    inline ArenaAllocator operator=(const ArenaAllocator &) = delete;

    inline ~ArenaAllocator() {
        free(m_buffer);
    }

private:
    size_t m_size;
    std::byte* m_buffer;
    std::byte* m_offset;
};
