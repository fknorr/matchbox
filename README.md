# matchbox – A `match` Expression for C++

C++'s sum types `std::variant` lacks the ergonomics of Rust's `match` expression for enum types – but it doesn't have to be this way!

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
    [&](int) -> auto& { return left; },
    [&](float) -> auto& { return right; }
); // => right
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

int next = match(opt,
    [](unsigned int i) { return i + 1; },
    [](std::nullopt_t) { return 0; }
); // => 124
```

## `match` on inheritance hierarchies

matchbox also allows you to match `match` on the dynamic type of a base-class reference.
This is implemented efficiently through double dispatch using an ad-hoc visitor implementation (no `dynamic_cast` or other RTTI involved).
The feature requires some light boilerplate to tell matchbox to implement the appropriate acceptor hierarchy:

```c++
class base
    : public matchbox::acceptor<class first_derived, class second_derived> {};
class first_derived
    : public matchbox::implement_acceptor<base, first_derived> {};
class second_derived
    : public matchbox::implement_acceptor<base, second_derived> {};

first_derived instance;
const base &ref = instance;

int which = match(
    ref,
    [](const first_derived &) { return 1; },
    [](const second_derived &) { return 2; }
); // => 1
```

Note how in the example above, the derived classes can even be forward-declared within the template parameters to `acceptor` (although they can of course be declared anywhere else prior to that).

If you are in a situation with a large number of derived classes and the base class list gets unwieldy, use a typedef with `matchbox::type_list` to hoist it out of the class definition:

```c++
using derived_types = matchbox::type_list<class first_derived, class second_derived>;

class base : public derived_types::acceptor {};
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

// Match on a base-class reference, and specify an explicit result type.
// Only selected if the Acceptor type is derived from matchbox::acceptor<...>.
template <typename Result, typename AcceptorCVRef, typename... Arms>
Result match(AcceptorCVRef &&acceptor, Arms &&...arms);

// Match on a base-class reference and return the cvref-qualified std::common_type of results.
// Only selected if the Acceptor type is derived from matchbox::acceptor<...>.
template <typename AcceptorCVRef, typename... Arms>
inline decltype(auto) match(AcceptorCVRef &&acceptor, Arms &&...arms);

}
```

### Visitor Pattern

Matching on inheritance hierarchies is realized by instantiating an ad-hoc visitor type as part of `match` expression, which is accepted by user types that implement the accompanying `acceptor` interface.
The acceptor can be implemented on each derived type by inheriting through the CRTP base class `matchbox::implement_acceptor`.

There are however cases when implementing a custom visitor class can be more readable than a match statement, for example if the code for each match the number of cases to be distinguished is large.

```c++
namespace matchbox {

// You can a type_list to specify the list of visited types ahead of the acceptor class
// if that improves readability.
template <typename... Ts>
struct type_list {
    using acceptor = matchbox::acceptor<Ts...>;
    using visitor = matchbox::visitor<Ts &...>;
    using const_visitor = matchbox::visitor<const Ts &...>;
    using move_visitor = matchbox::visitor<Ts &&...>;

    template <typename T>
    static constexpr bool contains_v = (std::is_same_v<T, Ts> || ...);

    ~type_list() = delete;
};

// Visitor base class that is implemented automatically by `match`, or can be implemented
// manually to accept a list of types.
template <typename... Ts>
class visitor {
  public:
    using visited_types = type_list<Ts...>;

    virtual ~visitor() = default;

    // Exposition only
    (virtual void visit(Ts) = 0)...;
};

// Interface for types that can be visited by a `visitor`. Base types of your visitable
// hierarchies should derive from this.
template <typename... Derived>
class acceptor {
  public:
    using visitor = typename type_list<Derived...>::visitor;
    using const_visitor = typename type_list<Derived...>::const_visitor;
    using move_visitor = typename type_list<Derived...>::move_visitor;

    virtual ~acceptor() = default;

    // There are three overloads of `accept` to forward the correct reference type
    // (T&, const T&, or T&&) to the visitor.
    virtual void accept(visitor &) & = 0;
    virtual void accept(const_visitor &) const & = 0;
    virtual void accept(move_visitor &) && = 0;
};

// CRTP base class that implements `acceptor` for a derived type.
// Use as `class my_type : public implement_acceptor<my_base, my_type> {};`, where
// `my_base` itself inherits from `acceptor`
template <typename Base, typename Derived>
class implement_acceptor : public Base {
  public:
    using typename Base::const_visitor;
    using typename Base::move_visitor;
    using typename Base::visitor;

    // Use `my_type(arg): acceptor_base(arg) {}` to call the base class constructor
    using acceptor_base = implement_acceptor;
    using Base::Base;

    void accept(visitor &v) & override { v.visit((Derived&) *this); }
    void accept(const_visitor &v) const & override { v.visit((const Derived&) *this); }
    void accept(move_visitor &v) && override { v.visit((Derived &&) *this); }
};

}
```
