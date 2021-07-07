#pragma once

#include "policy.h"

#include <cassert>
#include <iostream>
#include <memory>
#include <optional>
#include <vector>

template <
        class Key,
        class T,
        class CollisionPolicy = LinearProbing,
        class Hash = std::hash<Key>,
        class Equal = std::equal_to<Key>>
class HashMap
{
public:
    // types
    using key_type = Key;
    using mapped_type = T;
    using value_type = std::pair<const Key, T>;
    using size_type = std::size_t;
    using difference_type = std::ptrdiff_t;
    using hasher = Hash;
    using key_equal = Equal;
    using reference = value_type &;
    using const_reference = const value_type &;
    using pointer = value_type *;
    using const_pointer = const value_type *;

    template <bool isConst>
    class Iterator;
    using iterator = Iterator<false>;
    using const_iterator = Iterator<true>;

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
    template <bool isConst>
    class Iterator
    {
        friend HashMap;

        using container_el = std::conditional_t<isConst, const container *, container *>;
        using exists_el = std::conditional_t<isConst, const std::vector<bool> *, std::vector<bool> *>;

        container_el this_el;
        exists_el used;
        size_type index;

        Iterator(container_el this_el, exists_el used, size_type index = 0)
            : this_el(this_el)
            , used(used)
            , index(index)
        {
        }

    public:
        using iterator_category = std::forward_iterator_tag;
        using difference_type = std::ptrdiff_t;
        using distance_type = std::size_t;
        using value_type = std::conditional_t<isConst, const value_type, value_type>;
        using pointer = std::conditional_t<isConst, const value_type *, value_type *>;
        using reference = std::conditional_t<isConst, const value_type &, value_type &>;

        Iterator() = default;

        Iterator(const iterator & iterator)
            : Iterator(iterator.this_el, iterator.used, iterator.index){};

        Iterator(const const_iterator & iterator)
            : Iterator(iterator.this_el, iterator.used, iterator.index){};

        Iterator & operator=(const Iterator & other) = default;

        reference operator*()
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

    explicit HashMap(size_type expected_max_size = 0,
                     const hasher & hash = hasher(),
                     const key_equal & equal = key_equal())
        : max_cnt_elements(expected_max_size)
        , elements(expected_max_size * 2)
        , exists(expected_max_size * 2)
        , fn_hash(hash)
        , fn_equal(equal)
        , start(expected_max_size * 2){};

    template <class InputIt>
    HashMap(InputIt first,
            InputIt last,
            size_type expected_max_size = 0,
            const hasher & hash = hasher(),
            const key_equal & equal = key_equal())
        : HashMap(expected_max_size, hash, equal)
    {
        for (auto it = first; it != last; ++it) {
            insert(*it);
        }
    }

    //    HashMap(const HashMap & map)
    //        : cnt_elements(map.cnt_elements)
    //        , max_cnt_elements(map.max_cnt_elements)
    //        , elements(map.elements)
    //        , exists(map.exists)
    //        , fn_hash(map.fn_hash)
    //        , fn_equal(map.fn_equal)
    //        , start(map.start){};
    HashMap(HashMap &) = default;
    HashMap(HashMap &&) = default;

    HashMap(std::initializer_list<value_type> init,
            size_type expected_max_size = 0,
            const hasher & hash = hasher(),
            const key_equal & equal = key_equal())
        : HashMap(init.begin(), init.end(), expected_max_size, hash, equal)
    {
    }

    ~HashMap() = default;

    HashMap & operator=(const HashMap & other)
    {
        *this = HashMap(other);
        return *this;
    };
    HashMap & operator=(HashMap && other) noexcept
    {
        swap(other);
        return *this;
    };

    HashMap & operator=(std::initializer_list<value_type> init)
    {
        return *this = HashMap(init, init.size());
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

    std::pair<iterator, bool> insert(const value_type & value)
    {
        return emplace(value);
    };
    std::pair<iterator, bool> insert(value_type && value)
    {
        return emplace(std::move(value));
    };
    template <class P>
    std::pair<iterator, bool> insert(P && value)
    {
        return emplace(std::forward<P>(value));
    }
    iterator insert(const_iterator /*hint*/, const value_type & value)
    {
        return insert(value).first;
    };
    iterator insert(const_iterator /*hint*/, value_type && value)
    {
        return insert(std::move(value)).first;
    };
    template <class P>
    iterator insert(const_iterator /*hint*/, P && value)
    {
        return insert(std::forward<P>(value)).first;
    }
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

    template <class M>
    std::pair<iterator, bool> insert_or_assign(const key_type & key, M && value)
    {
        auto ans = emplace(key, std::forward<M>(value));
        if (!ans.second) {
            elements[ans.first.index]->second = value;
        }
        return ans;
    }
    template <class M>
    std::pair<iterator, bool> insert_or_assign(key_type && key, M && value)
    {
        auto ans = emplace(std::move(key), std::forward<M>(value));
        if (!ans.second) {
            elements[ans.first.index]->second = std::forward<M>(value);
        }
        return ans;
    }
    template <class M>
    iterator insert_or_assign(const_iterator /*hint*/, const key_type & key, M && value)
    {
        return insert_or_assign(key, std::forward<M>(value)).first;
    }
    template <class M>
    iterator insert_or_assign(const_iterator /*hint*/, key_type && key, M && value)
    {
        return insert_or_assign(std::move(key), std::forward<M>(value)).first;
    }

    // construct element in-place, no copy or move operations are performed;
    // element's constructor is called with exact same arguments as `emplace` method
    // (using `std::forward<Args>(args)...`)
    template <class... Args>
    std::pair<iterator, bool> emplace(Args &&... args)
    {
        return emplace_impl(std::forward<Args>(args)...);
    }

    template <class... Args>
    iterator emplace_hint(const_iterator /*hint*/, Args &&... args)
    {
        return emplace(std::forward<Args>(args)...).first;
    }

    template <class... Args>
    std::pair<iterator, bool> try_emplace(const key_type & key, Args &&... args)
    {
        check_size();
        size_type insert_ind = find_index(key, true);
        bool inserted = false;
        if (!elements[insert_ind] || !exists[insert_ind]) {
            inserted = true;
            elements[insert_ind].emplace(std::piecewise_construct,
                                         std::forward_as_tuple(key),
                                         std::forward_as_tuple(std::forward<Args>(args)...));
            update_size(insert_ind);
        }

        return {iterator(&elements, &exists, insert_ind), inserted};
    }
    template <class... Args>
    std::pair<iterator, bool> try_emplace(key_type && key, Args &&... args)
    {
        check_size();
        size_type insert_ind = find_index(std::move(key), true);
        bool inserted = false;
        if (!elements[insert_ind] || !exists[insert_ind]) {
            inserted = true;
            elements[insert_ind].emplace(std::piecewise_construct,
                                         std::forward_as_tuple(std::move(key)),
                                         std::forward_as_tuple(std::forward<Args>(args)...));
            update_size(insert_ind);
        }

        return {iterator(&elements, &exists, insert_ind), inserted};
    }
    template <class... Args>
    iterator try_emplace(const_iterator /*hint*/, const key_type & key, Args &&... args)
    {
        return try_emplace(key, std::forward<Args>(args)...).first;
    }
    template <class... Args>
    iterator try_emplace(const_iterator /*hint*/, key_type && key, Args &&... args)
    {
        return try_emplace(std::move(key), std::forward<Args>(args)...).first;
    }

    iterator erase(const_iterator pos)
    {
        auto it = iterator(&elements, &exists, pos.index);
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
    void swap(HashMap & other) noexcept
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

    mapped_type & at(const key_type & key)
    {
        iterator found = find(key);
        if (found != end()) {
            return found->second;
        }
        throw std::out_of_range("HashMap::at. No element with this key");
    };

    const mapped_type & at(const key_type & key) const
    {
        const_iterator found = find(key);
        if (found != end()) {
            return found->second;
        }
        throw std::out_of_range("HashMap::at. No element with this key");
    };
    mapped_type & operator[](const key_type & key)
    {
        return try_emplace(key).first->second;
    };
    mapped_type & operator[](key_type && key)
    {
        return try_emplace(std::move(key)).first->second;
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
            new_size += (count < 2 * cnt_elements) ? 2 * cnt_elements : count;
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
                emplace(std::move(const_cast<key_type &>(new_map[i]->first)), std::move(new_map[i]->second));
            }
        }
    };

    void reserve(size_type count)
    {
        if (count > elements.size())
            rehash(count);
    };

    // compare two containers contents
    friend bool operator==(const HashMap & lhs, const HashMap & rhs)
    {
        if (lhs.size() != rhs.size())
            return false;

        for (const auto & element : lhs) {
            if (!rhs.contains(element.first)) {
                return false;
            }
        }
        return true;
    };
    friend bool operator!=(const HashMap & lhs, const HashMap & rhs)
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

    template <class K, class V>
    std::pair<iterator, bool> emplace_impl(K && key, V && val)
    {
        check_size();
        size_type insert_ind = find_index(key, true);
        bool inserted = false;
        if (!elements[insert_ind] || !exists[insert_ind]) {
            inserted = true;
            elements[insert_ind].emplace(std::forward<K>(key), std::forward<V>(val));
            update_size(insert_ind);
        }

        return {iterator(&elements, &exists, insert_ind), inserted};
    }

    template <class... KeyArgs, class... ValArgs>
    std::pair<iterator, bool> emplace_impl(std::piecewise_construct_t, std::tuple<KeyArgs...> to_key, std::tuple<ValArgs...> to_val)
    {
        check_size();
        key_type key(std::make_from_tuple<key_type>(std::forward<std::tuple<KeyArgs...>>(to_key)));
        size_type insert_ind = find_index(key, true);
        bool inserted = false;
        if (!elements[insert_ind] || !exists[insert_ind]) {
            inserted = true;
            elements[insert_ind].emplace(std::move(key),
                                         std::make_from_tuple<mapped_type>(std::forward<std::tuple<ValArgs...>>(to_val)));
            update_size(insert_ind);
        }

        return {iterator(&elements, &exists, insert_ind), inserted};
    }

    template <class P>
    std::pair<iterator, bool> emplace_impl(P && value)
    {
        check_size();
        size_type insert_ind = find_index(value.first, true);
        bool inserted = false;
        if (!elements[insert_ind] || !exists[insert_ind]) {
            inserted = true;
            elements[insert_ind].emplace(std::forward<P>(value));
            update_size(insert_ind);
        }

        return {iterator(&elements, &exists, insert_ind), inserted};
    }

    size_type find_index(const key_type & key, bool insert) const
    {
        if (elements.empty() && !insert)
            return max_cnt_elements * 2;

        size_type current = fn_hash(key) % elements.size();
        CollisionPolicy pol = CollisionPolicy(elements.size(), current);

        while (elements[current] && (exists[current] || !insert) && !(fn_equal(elements[current]->first, key) && exists[current])) {
            current = pol.next();
        }

        return (insert || (fn_equal(elements[current]->first, key) && exists[current])) ? current : max_cnt_elements * 2;
    }
};