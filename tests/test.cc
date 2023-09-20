#include <matchbox.hh>

#define CATCH_CONFIG_MAIN 1
#include <catch2/catch.hpp>

using namespace matchbox;

TEST_CASE("match on std::variant", "[utils]") {
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
}

TEST_CASE("match on polymorphic visitors", "[utils]") {
    struct derived_a;
    struct derived_b;
    struct base {
        using visitor = matchbox::visitor<derived_a &, derived_b &>;
        using const_visitor = matchbox::visitor<const derived_a &, const derived_b &>;
        virtual void accept(visitor &visitor) = 0;
        virtual void accept(const_visitor &visitor) const = 0;
    };
    struct derived_a final : base {
        void accept(visitor &visitor) override { visitor.visit(*this); }
        void accept(const_visitor &visitor) const override { visitor.visit(*this); }
    };
    struct derived_b final : base {
        void accept(visitor &visitor) override { visitor.visit(*this); }
        void accept(const_visitor &visitor) const override { visitor.visit(*this); }
    };

    derived_a instance;
    base &ref = instance;

    // base::visitor is default_visitor<base>

    decltype(auto) ret2 = match<std::optional<int>>(
        ref, [](derived_a &) { return 0; }, [](derived_b &) { return std::nullopt; });
    static_assert(std::is_same_v<decltype(ret2), std::optional<int>>);

    // base::const_visitor is default_visitor<const base>

    decltype(auto) ret3 = match<std::optional<int>>(
        std::as_const(ref), [](const derived_a &) { return 0; }, [](const derived_b &) { return std::nullopt; });
    static_assert(std::is_same_v<decltype(ret3), std::optional<int>>);
}
