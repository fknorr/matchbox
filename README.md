# matchbox – A `match` Statement for C++

C++'s sum type `std::variant` lacks the ergonomics of Rust's `match` statement for enum types – but it doesn't have to be this way!

```c++
#include <matchbox.hh>
using matchbox::match;

std::variant<int, const char*> var("Hello world!");

size_t magnitude = match(var,
    [](int i) { return std::abs(i); },
    [](const char *s) { return strlen(s); }
); // => 12
```

matchbox is a ~500 LOC single-header C++17 library that gives you a `match` primitive for
- `std::variant` (dispatched using `std::visit` – `constexpr` capable)
- `std::optional` (distinguish between `nullopt` and `value_type` – `constexpr` capable)
- **inheritance hierarchies** on your own types (implemented with efficient double virtual dispatch)

No configuration or building needed – just copy [matchbox.hh](include/matchbox.hh) into your source tree and be on your way!

## `match` on `std::variant`

The overload set of all match arms must span all alternatives of the variant. This also means you can use generic lambdas for a catch-all:
```c++
std::variant<std::string, std::vector<int>, int, float> var(42);

const char *type_string = match(var,
    [](const std::string &) { return "string"; },
    [](const std::vector<int> &) { return "array"; },
    [](const auto& /* default */) { return "number"; }
); // => "number"
```

matchbox will implicitly convert the return types of your lambdas to their `std::common_type`, while preserving const-qualification and references where possible.

If your lambdas return references (you have to manually specify a return type for this to happen), `match` will forward that reference:
```c++
std::variant<int, float> num(4.20f);
int left = 1, right = 2;

int &var = match(num,
    [](int) -> auto& { return left; }
    [](float) -> auto& { return right; }
); // => left
```

If the argument type is any kind of reference, it will be forwarded appropriately into the match arms:
```c++
std::variant<std::string, std::vector<int>> heavy(std::string("lorem ipsum"));

match(std::move(heavy),
    [](std::string &&str) { log_append(std::move(str)); },
    [](std::vector<int> &&vec) { write_to_db(std::move(vec)); }
);
```

## `match` on `std::optional`

For `std::optional`, matchbox allows you to choose a path for both `has_value()` and `nullopt`:
```c++
std::optional<unsigned int> opt(123u);

int next = match(num,
    [](unsigned int i) { return i + 1; }
    [](std::nullopt) { return 0; }
); // => 124
```

## `match` on inheritance hierarchies

matchbox also allows you to enter a `match` statement based on dynamic class type.
This is implemented through an ad-hoc visitor pattern, which means it's reasonably fast (implemented using double virtual dispatch, no `dynamic_casts`) and does not need RTTI support (in case you care about such a thing).

This feature requires some light boilerplate to tell matchbox which vtable entries to generate:

```c++
class first_derived;
class second_derived;

class base {
    public:
        // depending on wether any of your match statements needs const or non-const references
        // to the type, define `visitor`, `const_visitor`, or both.
        using visitor = matchbox::visitor<first_derived&, second_derived&>;
        using const_visitor = matchbox::visitor<const first_derived&, const second_derived&>;

        // only `accept` methods for the visitor types you defined above are required.
        virtual void accept(visitor &visitor) = 0;
        virtual void accept(const_visitor &visitor) const = 0;
};

class first_derived: public base {
    public:
        // the `accept` implementations on all deived types are syntactically identical
        // (but they select the appropriate overload of visitor::visit`)
        void accept(visitor &visitor) override { visitor.visit(*this); }
        void accept(const_visitor &visitor) const override { visitor.visit(*this); }
};

class first_derived: public base {
    public:
        void accept(visitor &visitor) override { visitor.visit(*this); }
        void accept(const_visitor &visitor) const override { visitor.visit(*this); }
};

// ...

first_derived instance;
const base &ref = instance;

int which = match(ref,
    [](const first_derived &) { return 1; },
    [](const first_derived &) { return 2; }
); // => 1
```

## API Reference

### `match` Family of Functions

```c++
namespace matchbox {

// Match on a variant and specify an explicit result type (when it cannot be deduced).
// Only selected if VariantCVRef is a possibly cv-qualified reference to std::variant.
template <typename Result, typename VariantCVRef, typename... Arms>
constexpr Result match(VariantCVRef &&v, Arms &&...arms);

// Match on a variant, and return the cvref-qualified std::common_type of results.
// Only selected if VariantCVRef is a possibly cv-qualified reference to std::variant.
template <typename VariantCVRef, typename... Arms>
constexpr decltype(auto) match(VariantCVRef &&v, Arms &&...arms);

// Match on an optional and specify an explicit result type (when it cannot be deduced).
// Only selected if OptionalCVRef is a possibly cv-qualified reference to std::optional.
template <typename Result, typename OptionalCVRef, typename... Arms>
constexpr Result match(OptionalCVRef &&o, Arms &&...arms);

// Match on an optional, and return the cvref-qualified std::common_type of results.
// Only selected if OptionalCVRef is a possibly cv-qualified reference to std::optional.
template <typename OptionalCVRef, typename... Arms>
constexpr decltype(auto) match(OptionalCVRef &&o, Arms &&...arms);

// Match on a reference-to-base-class and specify an explicit visitor and result type.
// Only selected if Visitor inherits from matchbox::visitor<...> and T::accept(Visitor&) is available.
template <typename Result, typename Visitor, typename T, typename... Arms>
Result match(T &target, Arms &&...arms);

// Match on a reference-to-base-class with default_visitor<T>, and specify an explicit result type.
// Only selected if T::accept(Visitor&) is available and Result does _not_ inherit from matchbox::visitor<...>.
template <typename Result, typename T, typename... Arms>
Result match(T &target, Arms &&...arms);

// Match on a reference-to-base-class, specify an explicit visitor, and return the cvref-qualified std::common_type of results.
// Only selected if Visitor inherits from matchbox::visitor<...> and T::accept(Visitor&) is available.
template <typename Visitor, typename T, typename... Arms>
decltype(auto) match(T &target, Arms &&...arms);

// Match on a reference-to-base-class with default_visitor<T> and return the cvref-qualified std::common_type of results.
// Only selected if T::accept(Visitor&) is available.
template <typename T, typename... Arms>
decltype(auto) match(T &target, Arms &&...arms);

}
```

### Visitor Pattern

You're free to implement your own visitor types on top of `matchbox::visitor` if a `match` statement does not fit your particular needs.

```c++
namespace matchbox {

template <typename... Ts>
class visitor {
  public:
    using polymorphic_visitor_tag = /* implementation defined */;
    virtual ~visitor() = default;
    (virtual void visit(Ts) = 0)...;
};

template<typename Base>
struct default_visitor {
    using type = typename Base::visitor; // if that type exists and `!std::is_const_v<Base>`
    using type = typename Base::const_visitor; // if that type exists and either `std::is_const_v<Base>` or `typename Base::visitor` does _not_ exist
};

template <typename T>
using default_visitor_t = typename default_visitor<T>::type;

}
```
