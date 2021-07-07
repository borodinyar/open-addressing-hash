#pragma once

#include "policy.h"

#include <memory>
#include <optional>
#include <vector>

template <
        class Key,
        class CollisionPolicy = LinearProbing,
        class Hash = std::hash<Key>,
        class Equal = std::equal_to<Key>>
class HashSet
{
public:
    // types
    using key_type = Key;
    using value_type = Key;
    using size_type = std::size_t;
    using difference_type = std::ptrdiff_t;
    using hasher = Hash;
    using key_equal = Equal;
    using reference = value_type &;
    using const_reference = const value_type &;
    using pointer = value_type *;
    using const_pointer = const value_type *;

    class Iterator;
    using iterator = Iterator;
    using const_iterator = Iterator;

    using container = std::vector<std::optional<value_type>>;

private:
    size_type cnt_elements = 0;
    size_type max_cnt_elements;
    container elements;
    std::vector<bool> exists;
    const hasher fn_hash;
    const key_equal fn_equal;
    size_type start = max_cnt_elements * 2;

public:
    class Iterator
    {
        friend HashSet;

        const container * this_el;
        const std::vector<bool> * used;
        size_type index;

        Iterator(const container * this_el, const std::vector<bool> * used, size_type index = 0)
            : this_el(this_el)
            , used(used)
            , index(index)
        {
        }

    public:
        using iterator_category = std::forward_iterator_tag;
        using difference_type = std::ptrdiff_t;
        using distance_type = std::size_t;
        using value_type = value_type;
        using pointer = const value_type *;
        using reference = const value_type &;

        Iterator() = default;

        Iterator(const iterator & iterator)
            : Iterator(iterator.this_el, iterator.used, iterator.index){};

        Iterator & operator=(const Iterator & other) = default;

        reference operator*() const
        {
            return *((*this_el)[index]);
        }

        pointer operator->() const
        {
            return &(*((*this_el)[index]));
        }

        Iterator & operator++()
        {
            do {
                ++index;
            } while (index < this_el->size() && (!(*this_el)[index] || !(*used)[index]));

            return *this;
        }

        Iterator operator++(int)
        {
            Iterator res = *this;
            ++(*this);
            return res;
        }

        bool operator==(const Iterator & other) const
        {
            return this_el == other.this_el && index == other.index;
        }

        bool operator!=(const Iterator & other) const
        {
            return !(*this == other);
        }
    };

    explicit HashSet(size_type expected_max_size = 0,
                     const hasher & hash = hasher(),
                     const key_equal & equal = key_equal())
        : max_cnt_elements(expected_max_size)
        , elements(expected_max_size * 2)
        , exists(expected_max_size * 2)
        , fn_hash(hash)
        , fn_equal(equal)
        , start(expected_max_size * 2){};

    template <class InputIt>
    HashSet(InputIt first, InputIt last, size_type expected_max_size = 0, const hasher & hash = hasher(), const key_equal & equal = key_equal())
        : HashSet(expected_max_size, hash, equal)
    {
        for (auto it = first; it != last; ++it) {
            insert(*it);
        }
    }

    HashSet(const HashSet & set)
        : cnt_elements(set.cnt_elements)
        , max_cnt_elements(set.max_cnt_elements)
        , elements(set.elements)
        , exists(set.exists)
        , fn_hash(set.fn_hash)
        , fn_equal(set.fn_equal)
        , start(set.start){};
    HashSet(HashSet &&) = default;

    HashSet(std::initializer_list<value_type> init,
            size_type expected_max_size = 0,
            const hasher & hash = hasher(),
            const key_equal & equal = key_equal())
        : HashSet(init.begin(), init.end(), expected_max_size, hash, equal)
    {
    }

    ~HashSet() = default;

    HashSet & operator=(const HashSet & other)
    {
        *this = HashSet(other);
        return *this;
    };
    HashSet & operator=(HashSet && other) noexcept
    {
        swap(other);
        return *this;
    };
    HashSet & operator=(std::initializer_list<value_type> init)
    {
        return *this = HashSet(init, init.size());
    };

    iterator begin() noexcept
    {
        return iterator(&elements, &exists, start);
    };
    const_iterator begin() const noexcept
    {
        return cbegin();
    };
    const_iterator cbegin() const noexcept
    {
        return const_iterator(&elements, &exists, start);
    };

    iterator end() noexcept
    {
        return iterator(&elements, &exists, max_cnt_elements * 2);
    };
    const_iterator end() const noexcept
    {
        return cend();
    };
    const_iterator cend() const noexcept
    {
        return const_iterator(&elements, &exists, max_cnt_elements * 2);
    };

    bool empty() const
    {
        return cnt_elements == 0;
    };
    size_type size() const
    {
        return cnt_elements;
    };
    size_type max_size() const
    {
        return max_cnt_elements;
    };

    void clear()
    {
        elements.clear();
        exists.clear();
        container new_map(max_cnt_elements * 2);
        std::vector<bool> new_del(max_cnt_elements * 2);
        std::swap(elements, new_map);
        std::swap(exists, new_del);

        cnt_elements = 0;
        start = 0;
    };
    std::pair<iterator, bool> insert(const value_type & key)
    {
        return emplace(key);
    };
    std::pair<iterator, bool> insert(value_type && key)
    {
        check_size();
        size_type insert_ind = find_index(key, true);
        bool inserted = false;
        if (!elements[insert_ind] || !exists[insert_ind]) {
            inserted = true;
            elements[insert_ind].emplace(std::forward<value_type>(key));
            update_size(insert_ind);
        }

        return std::make_pair(iterator(&elements, &exists, insert_ind), inserted);
    };
    iterator insert(const_iterator /*hint*/, const value_type & key)
    {
        return insert(key).first;
    };
    iterator insert(const_iterator /*hint*/, value_type && key)
    {
        return insert(key).first;
    };
    template <class InputIt>
    void insert(InputIt first, InputIt last)
    {
        for (auto it = first; it != last; ++it) {
            insert(*it);
        }
    }
    void insert(std::initializer_list<value_type> init)
    {
        insert(init.begin(), init.end());
    };

    // construct element in-place, no copy or move operations are performed;
    // element's constructor is called with exact same arguments as `emplace` method
    // (using `std::forward<Args>(args)...`)
    template <class... Args>
    std::pair<iterator, bool> emplace(Args &&... args)
    {
        check_size();
        auto key = key_type(std::forward<Args>(args)...);
        size_type insert_ind = find_index(key, true);
        bool inserted = false;
        if (!elements[insert_ind] || !exists[insert_ind]) {
            inserted = true;
            elements[insert_ind].emplace(std::forward<key_type>(key));
            update_size(insert_ind);
        }

        return std::make_pair(iterator(&elements, &exists, insert_ind), inserted);
    }
    template <class... Args>
    iterator emplace_hint(const_iterator /*hint*/, Args &&... args)
    {
        return emplace(std::forward<Args>(args)...).first;
    }

    iterator erase(const_iterator pos)
    {
        iterator it = iterator(&elements, &exists, pos.index);
        if (!elements[pos.index]) {
            return it;
        }

        exists[pos.index] = false;
        --cnt_elements;
        ++it;

        if (pos.index == start)
            start = it.index;

        return it;
    };
    iterator erase(const_iterator first, const_iterator last)
    {
        const_iterator it = first;
        while (it != last) {
            erase(it);
            ++it;
        }

        return iterator(&elements, &exists, it.index);
    };
    size_type erase(const key_type & key)
    {
        const_iterator it = find(key);
        if (it == cend()) {
            return 0;
        }

        erase(it);
        return 1;
    };

    // exchanges the contents of the container with those of other;
    // does not invoke any move, copy, or swap operations on individual elements
    void swap(HashSet & other) noexcept
    {
        std::swap(elements, other.elements);
        std::swap(exists, other.exists);
        std::swap(start, other.start);
        std::swap(max_cnt_elements, other.max_cnt_elements);
        std::swap(cnt_elements, other.cnt_elements);
    };

    size_type count(const key_type & key) const
    {
        return (find(key) == end()) ? 0 : 1;
    };
    iterator find(const key_type & key)
    {
        size_type find_ind = find_index(key, false);
        return (find_ind == max_cnt_elements * 2) ? end() : iterator(&elements, &exists, find_ind);
    };
    const_iterator find(const key_type & key) const
    {
        size_type find_ind = find_index(key, false);
        return (find_ind == max_cnt_elements * 2) ? cend() : const_iterator(&elements, &exists, find_ind);
    };
    bool contains(const key_type & key) const
    {
        return find(key) != cend();
    };
    std::pair<iterator, iterator> equal_range(const key_type & key)
    {
        return common_equal_iterator<iterator>(key);
    };
    std::pair<const_iterator, const_iterator> equal_range(const key_type & key) const
    {
        return common_equal_iterator<const_iterator>(key);
    };

    size_type bucket_count() const
    {
        return max_cnt_elements * 2;
    };
    size_type max_bucket_count() const
    {
        return bucket_count();
    };
    size_type bucket_size(const size_type) const
    {
        return 1;
    };
    size_type bucket(const key_type & key) const
    {
        return fn_hash(key) % elements.size();
    };

    float load_factor() const
    {
        return max_cnt_elements ? cnt_elements / static_cast<float>(bucket_count()) : 1;
    };
    float max_load_factor() const
    {
        return 0.5;
    };
    void rehash(const size_type count)
    {
        size_type new_size = 2;
        if (count != 0 || !elements.empty()) {
            new_size += (count < 2 * cnt_elements) ? 2 * cnt_elements : count - 1;
        }

        container new_map(new_size * 2);
        std::vector<bool> new_del(new_size * 2);
        std::swap(elements, new_map);
        std::swap(exists, new_del);
        max_cnt_elements = new_size;
        cnt_elements = 0;
        start = new_size * 2;

        for (size_type i = 0; i < new_map.size(); ++i) {
            if (new_map[i] && new_del[i]) {
                emplace(std::move(*new_map[i]));
            }
        }
    };
    void reserve(size_type count)
    {
        if (count > elements.size())
            rehash(count);
    };

    // compare two containers contents
    friend bool operator==(const HashSet & lhs, const HashSet & rhs)
    {
        if (lhs.size() != rhs.size())
            return false;

        for (const auto & element : lhs) {
            if (!rhs.contains(element)) {
                return false;
            }
        }
        return true;
    };
    friend bool operator!=(const HashSet & lhs, const HashSet & rhs)
    {
        return !(lhs == rhs);
    };

private:
    template <class M>
    std::pair<M, M> common_equal_iterator(const key_type & key)
    {
        M it = find(key);
        if (it == end()) {
            return {it, it};
        }

        M curr = it;
        return {curr, ++it};
    }

    size_type find_index(const key_type & key, bool insert) const
    {
        if (elements.empty() && !insert)
            return max_cnt_elements * 2;

        size_type current = fn_hash(key) % elements.size();
        CollisionPolicy pol = CollisionPolicy(elements.size(), current);

        while (elements[current] && (exists[current] || !insert) && !(fn_equal(*elements[current], key) && exists[current])) {
            current = pol.next();
        }

        return (insert || (fn_equal(*elements[current], key) && exists[current])) ? current : max_cnt_elements * 2;
    }

    void check_size()
    {
        if (load_factor() >= max_load_factor()) {
            rehash(max_cnt_elements * 2);
        }
    }

    void update_size(size_type index)
    {
        ++cnt_elements;
        start = empty() ? index : std::min(start, index);
        exists[index] = true;
    }
};
