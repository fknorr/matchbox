#define CATCH_CONFIG_MAIN 1
#include <catch2/catch.hpp>

void throw_unless(const bool x, const char *msg) {
    if(!x) { throw std::runtime_error(msg); }
}

#define MATCHBOX_ASSERT throw_unless
#include <matchbox.hh>

using namespace matchbox;

TEST_CASE("match on std::variant") {
    struct alt_1 {};
    struct alt_2 {};
    struct alt_3 {};
    using variant3 = std::variant<alt_1, alt_2, alt_3>;

    // cvref-qualified variant alternatives are forwarded

    variant3 var_2(alt_2{});
    decltype(auto) ret_a = match(
        var_2,                     //
        [](alt_1 &) { return 1; }, //
        [](alt_2 &) { return 2; }, //
        [](alt_3 &) { return 3; });
    static_assert(std::is_same_v<decltype(ret_a), int>);
    CHECK(ret_a == 2);

    decltype(auto) ret_b = match(
        std::as_const(var_2),            //
        [](const alt_1 &) { return 1; }, //
        [](const alt_2 &) { return 2; }, //
        [](const alt_3 &) { return 3; });
    static_assert(std::is_same_v<decltype(ret_b), int>);
    CHECK(ret_b == 2);

    decltype(auto) ret_c = match(
        std::move(var_2),           //
        [](alt_1 &&) { return 1; }, //
        [](alt_2 &&) { return 2; }, //
        [](alt_3 &&) { return 3; });
    static_assert(std::is_same_v<decltype(ret_c), int>);
    CHECK(ret_c == 2);

    // cvref-qualified results are forwarded

    int int_1 = 1;
    int int_2 = 2;
    int int_3 = 3;
    decltype(auto) ret_d = match(
        var_2,                                  //
        [&](alt_1) -> auto & { return int_1; }, //
        [&](alt_2) -> auto & { return int_2; }, //
        [&](alt_3) -> auto & { return int_3; });
    static_assert(std::is_same_v<decltype(ret_d), int &>);
    CHECK(&ret_d == &int_2);

    decltype(auto) ret_e = match(
        var_2,                                        //
        [&](alt_1) -> const auto & { return int_1; }, //
        [&](alt_2) -> const auto & { return int_2; }, //
        [&](alt_3) -> const auto & { return int_3; });
    static_assert(std::is_same_v<decltype(ret_e), const int &>);
    CHECK(&ret_e == &int_2);

    decltype(auto) ret_f = match(
        var_2,                                              //
        [&](alt_1) -> auto && { return std::move(int_1); }, //
        [&](alt_2) -> auto && { return std::move(int_2); }, //
        [&](alt_3) -> auto && { return std::move(int_3); });
    static_assert(std::is_same_v<decltype(ret_f), int &&>);
    CHECK(&ret_f == &int_2);

    // common type is reference to the common base type of references

    struct value_base {
    } instance_base;
    struct value_derived_1 : value_base {
    } instance_derived_1;
    struct value_derived_2 : value_base {
    } instance_derived_2;

    decltype(auto) ret_g = match(
        var_2,                                               //
        [&](alt_1) -> auto & { return instance_base; },      //
        [&](alt_2) -> auto & { return instance_derived_1; }, //
        [&](alt_3) -> auto & { return instance_derived_2; });
    static_assert(std::is_same_v<decltype(ret_g), value_base &>);
    CHECK(&ret_g == &instance_derived_1);

    decltype(auto) ret_h = match(
        var_2,                                                     //
        [&](alt_1) -> const auto & { return instance_base; },      //
        [&](alt_2) -> const auto & { return instance_derived_1; }, //
        [&](alt_3) -> const auto & { return instance_derived_2; });
    static_assert(std::is_same_v<decltype(ret_h), const value_base &>);
    CHECK(&ret_h == &instance_derived_1);

    decltype(auto) ret_i = match(
        var_2,                                                           //
        [&](alt_1) -> auto && { return std::move(instance_base); },      //
        [&](alt_2) -> auto && { return std::move(instance_derived_1); }, //
        [&](alt_3) -> auto && { return std::move(instance_derived_2); });
    static_assert(std::is_same_v<decltype(ret_i), value_base &&>);
    CHECK(&ret_i == &instance_derived_1);

    // result is a common value type that can be constructed from all match arm results

    decltype(auto) ret_j = match(
        var_2,                                   //
        [&](alt_1) { return 1; },                //
        [&](alt_2) { return std::optional(2); }, //
        [&](alt_3) -> auto && { return std::move(std::nullopt); });
    static_assert(std::is_same_v<decltype(ret_j), std::optional<int>>);
    CHECK(ret_j.value() == 2);

    // constexpr variant match

    constexpr variant3 cx_var(alt_2{});
    constexpr decltype(auto) ret_k = match(
        cx_var,                  //
        [](alt_1) { return 1; }, //
        [](alt_2) { return 2; }, //
        [](alt_3) { return 3; });

    // generic match arms behave as expected

    decltype(auto) ret_l = match(                   //
        std::variant<double, int, const char>(1.0), //
        [](auto v) { return v; });
    static_assert(std::is_same_v<decltype(ret_l), double>);
    CHECK(ret_l == 1.0);

    // can return void

    // need to wrap this into a named function because lambdas are not allowed inside decltype()
    constexpr auto returns_void = [](variant3 &v) { return match(v, [&](const auto &) {}); };
    static_assert(std::is_void_v<decltype(returns_void(var_2))>);
}

TEST_CASE("match on std::optional") {
    // cvref-qualified variant alternatives are forwarded

    std::optional<float> opt_value(1.0f);
    std::optional<float> opt_null;

    decltype(auto) ret_a = match(
        opt_value,                        //
        [](std::nullopt_t) { return 0; }, //
        [](float &) { return 1; });
    static_assert(std::is_same_v<decltype(ret_a), int>);
    CHECK(ret_a == 1);

    decltype(auto) ret_a2 = match(
        opt_null,                         //
        [](std::nullopt_t) { return 0; }, //
        [](float &) { return 1; });
    static_assert(std::is_same_v<decltype(ret_a2), int>);
    CHECK(ret_a2 == 0);

    decltype(auto) ret_b = match(
        std::as_const(opt_value),         //
        [](std::nullopt_t) { return 0; }, //
        [](const float &) { return 1; });
    static_assert(std::is_same_v<decltype(ret_b), int>);
    CHECK(ret_b == 1);

    decltype(auto) ret_c = match(
        std::move(opt_value),             //
        [](std::nullopt_t) { return 0; }, //
        [](float &&) { return 1; });
    static_assert(std::is_same_v<decltype(ret_c), int>);
    CHECK(ret_c == 1);

    // cvref-qualified results are forwarded

    int int_0 = 0;
    int int_1 = 1;

    decltype(auto) ret_d = match(
        opt_value,                                       //
        [&](std::nullopt_t) -> auto & { return int_0; }, //
        [&](float) -> auto & { return int_1; });
    static_assert(std::is_same_v<decltype(ret_d), int &>);
    CHECK(&ret_d == &int_1);

    decltype(auto) ret_e = match(
        std::as_const(opt_value),                              //
        [&](std::nullopt_t) -> const auto & { return int_0; }, //
        [&](float) -> const auto & { return int_1; });
    static_assert(std::is_same_v<decltype(ret_e), const int &>);
    CHECK(&ret_e == &int_1);

    decltype(auto) ret_f = match(
        std::as_const(opt_value),                                    //
        [&](std::nullopt_t) -> auto && { return std::move(int_0); }, //
        [&](float) -> auto && { return std::move(int_1); });
    static_assert(std::is_same_v<decltype(ret_f), int &&>);
    CHECK(&ret_f == &int_1);

    // common type is reference to the common base type of references

    struct value_base {
    } instance_base;
    struct value_derived : value_base {
    } instance_derived;

    decltype(auto) ret_g = match(
        opt_value,                                               //
        [&](std::nullopt_t) -> auto & { return instance_base; }, //
        [&](float) -> auto & { return instance_derived; });
    static_assert(std::is_same_v<decltype(ret_g), value_base &>);
    CHECK(&ret_g == &instance_derived);

    decltype(auto) ret_h = match(
        opt_value,                                                     //
        [&](std::nullopt_t) -> const auto & { return instance_base; }, //
        [&](float) -> const auto & { return instance_derived; });
    static_assert(std::is_same_v<decltype(ret_h), const value_base &>);
    CHECK(&ret_h == &instance_derived);

    decltype(auto) ret_i = match(
        opt_value,                                                           //
        [&](std::nullopt_t) -> auto && { return std::move(instance_base); }, //
        [&](float) -> auto && { return std::move(instance_derived); });
    static_assert(std::is_same_v<decltype(ret_i), value_base &&>);
    CHECK(&ret_i == &instance_derived);

    // result is a common value type that can be constructed from all match arm results

    constexpr static const char *string = "string";
    decltype(auto) ret_j = match(
        opt_value,                              //
        [](std::nullopt_t) { return nullptr; }, //
        [](float) { return string; });
    static_assert(std::is_same_v<decltype(ret_j), const char *>);
    CHECK(ret_j == string);

    // constexpr variant match

    constexpr std::optional<float> cx_opt(42.0);
    constexpr decltype(auto) ret_k = match(
        cx_opt,                           //
        [](std::nullopt_t) { return 1; }, //
        [](float) { return 2; });         //
    CHECK(ret_k == 2);

    // generic match arms behave as expected

    decltype(auto) ret_l = match( //
        std::optional<int>(99),   //
        [](auto v) -> std::optional<int> { return v; });
    static_assert(std::is_same_v<decltype(ret_l), std::optional<int>>);
    CHECK(ret_l == std::optional<int>(99));

    // explicit return types work

    auto ret_m = match<std::optional<float>>(
        std::optional<int>(99),          //
        [](int i) { return (double)i; }, //
        [](std::nullopt_t) { return std::nullopt; });

    // can return void

    // need to wrap this into a named function because lambdas are not allowed inside decltype()
    constexpr auto returns_void = [](std::optional<float> &o) { return match(o, [&](const auto &) {}); };
    static_assert(std::is_void_v<decltype(returns_void(opt_null))>);
}

TEST_CASE("match on polymorphic acceptors") {
    struct base : public matchbox::acceptor<class derived_a, class derived_b, class derived_c> {};
    struct derived_a final : public matchbox::implement_acceptor<base, derived_a> {};
    struct derived_b final : public matchbox::implement_acceptor<base, derived_b> {};
    struct berived_c final : public matchbox::implement_acceptor<base, derived_c> {};

    derived_b instance;
    base &ref = instance;

    const auto ret = match(
        ref,                           //
        [](derived_a &) { return 0; }, //
        [](derived_b &) { return 1; }, //
        [](derived_c &) { return 2; });
    CHECK(ret == 1);

    const auto ret2 = match(
        std::as_const(ref),                  //
        [](const derived_a &) { return 0; }, //
        [](const derived_b &) { return 1; }, //
        [](const derived_c &) { return 2; });
    CHECK(ret2 == 1);

    const auto ret3 = match(
        std::move(ref),                 //
        [](derived_a &&) { return 0; }, //
        [](derived_b &&) { return 1; }, //
        [](derived_c &&) { return 2; });
    CHECK(ret3 == 1);

    // const-rvalue-references are just treated as const-lvalue-references

    static_assert(std::is_same_v<detail::select_visitor_t<const base &&>, detail::select_visitor_t<const base &>>);
    const auto ret4 = match(
        static_cast<const base &&>(ref),     //
        [](const derived_a &) { return 0; }, //
        [](const derived_b &) { return 1; }, //
        [](const derived_c &) { return 2; });
    CHECK(ret4 == 1);

    // common return types are deduced

    decltype(auto) ret5 = match(
        ref,                                          //
        [](derived_b &) { return std::optional(0); }, //
        [](auto &) { return std::nullopt; });
    static_assert(std::is_same_v<decltype(ret5), std::optional<int>>);

    decltype(auto) ret6 = match(
        std::as_const(ref),                                 //
        [](const derived_b &) { return std::optional(0); }, //
        [](const auto &) { return std::nullopt; });
    static_assert(std::is_same_v<decltype(ret6), std::optional<int>>);

    decltype(auto) ret7 = match(
        std::move(ref),                                //
        [](derived_b &&) { return std::optional(0); }, //
        [](auto &&) { return std::nullopt; });
    static_assert(std::is_same_v<decltype(ret7), std::optional<int>>);

    // can return references

    int int_1 = 1;
    int int_2 = 2;
    int int_3 = 3;

    decltype(auto) ret8 = match(
        ref,                                         //
        [&](derived_a &) -> int & { return int_1; }, //
        [&](derived_b &) -> int & { return int_2; }, //
        [&](derived_c &) -> int & { return int_3; });
    static_assert(std::is_same_v<decltype(ret8), int &>);
    CHECK(&ret8 == &int_2);

    decltype(auto) ret9 = match(
        ref,                                               //
        [&](derived_a &) -> const int & { return int_1; }, //
        [&](derived_b &) -> const int & { return int_2; }, //
        [&](derived_c &) -> const int & { return int_3; });
    static_assert(std::is_same_v<decltype(ret9), const int &>);
    CHECK(&ret9 == &int_2);

    decltype(auto) ret10 = match(
        ref,                                                     //
        [&](derived_a &) -> int && { return std::move(int_1); }, //
        [&](derived_b &) -> int && { return std::move(int_2); }, //
        [&](derived_c &) -> int && { return std::move(int_3); });
    static_assert(std::is_same_v<decltype(ret10), int &&>);
    CHECK(&ret10 == &int_2);

    // can return void

    // need to wrap this into a named function because lambdas are not allowed inside decltype()
    constexpr auto returns_void = [](base &ref) { return match(ref, [&](auto &) {}); };
    static_assert(std::is_void_v<decltype(returns_void(ref))>);
}

TEST_CASE("incorrect inheritance from implement_acceptor is detected at runtime") {
    class base : public matchbox::acceptor<class first_derived, class second_derived> {};
    class first_derived : public matchbox::implement_acceptor<base, first_derived> {};

    // this instantiates base::base(), which skips the static assertion on second_derived
    // because that type is incomplete at this point
    const first_derived instance1;

    // typo: inherits from <first_derived> instead of <second_derived>
    class second_derived : public matchbox::implement_acceptor<base, first_derived> {};
    const second_derived instance2;

    // must now cause an assertion failure in second_derived::accept()
    CHECK_THROWS_WITH(
        match(
            static_cast<const base &>(instance2), [](const first_derived &) {}, [](const second_derived &) {}),
        "Derived type does not inherit from implement_acceptor<Base, Derived>");
}

TEST_CASE("README intro") {
    std::variant<int, const char *> var("Hello world!");

    size_t magnitude = match(
        var,                               //
        [](int i) { return std::abs(i); }, //
        [](const char *s) { return strlen(s); });
    CHECK(magnitude == 12);
}

TEST_CASE("README on std::variant (1)") {
    std::variant<std::string, std::vector<int>, int, float> var(42);

    const char *type_string = match(
        var,                                              //
        [](const std::string &) { return "string"; },     //
        [](const std::vector<int> &) { return "array"; }, //
        [](const auto & /* default */) { return "number"; });
    CHECK(strcmp(type_string, "number") == 0);
}

TEST_CASE("README on std::variant (2)") {
    std::variant<int, float> num(4.20f);
    int left = 1;
    int right = 2;

    int &var = match(
        num,                                 //
        [&](int) -> auto & { return left; }, //
        [&](float) -> auto & { return right; });
    CHECK(&var == &right);
}

TEST_CASE("README on std::variant (3)") {
    std::variant<std::string, std::vector<int>> heavy(std::string("lorem ipsum"));

    match(
        std::move(heavy),                                //
        [](std::string &&str) { (void)std::move(str); }, //
        [](std::vector<int> &&vec) { (void)std::move(vec); });
}

TEST_CASE("README on std::optional") {
    std::optional<unsigned int> opt(123u);

    int next = match(
        opt,                                  //
        [](unsigned int i) { return i + 1; }, //
        [](std::nullopt_t) { return 0; });
    CHECK(next == 124);
}

TEST_CASE("README on inheritance hierarchies (1)") {
    class base : public matchbox::acceptor<class first_derived, class second_derived> {};
    class first_derived : public matchbox::implement_acceptor<base, first_derived> {};
    class second_derived : public matchbox::implement_acceptor<base, second_derived> {};

    first_derived instance;
    const base &ref = instance;

    int which = match(
        ref,                                     //
        [](const first_derived &) { return 1; }, //
        [](const second_derived &) { return 2; });
    CHECK(which == 1);
}

TEST_CASE("README on inheritance hierarchies (2)") {
    using derived_types = matchbox::type_list<class first_derived, class second_derived>;
    class base : public derived_types::acceptor {};
}
