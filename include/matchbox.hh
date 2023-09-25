#pragma once

#include <optional>
#include <type_traits>
#include <variant>

#ifndef MATCHBOX_ASSERT
#include <cassert>
#define MATCHBOX_ASSERT(x, msg) assert((x) && msg)
#endif

namespace matchbox::detail {

template <typename T>
class declare_visit_function {
  public:
    virtual void visit(T) = 0;
};

template <typename F, typename Param>
using invoke_result_t = decltype(std::declval<F>()(std::declval<Param>()));

template <typename Base, typename Fn, typename Ret, typename... Ts>
class implement_visit_function;

template <typename Base, typename Fn, typename Ret, typename T, typename... Ts>
class implement_visit_function<Base, Fn, Ret, T, Ts...> : public implement_visit_function<Base, Fn, Ret, Ts...> {
  private:
    using next = implement_visit_function<Base, Fn, Ret, Ts...>;

  public:
    explicit implement_visit_function(Fn &&fn) : next(std::move(fn)) {}
    void visit(T v) final { next::template invoke<T>(static_cast<T>(v)); }
    using next::visit;
};

template <typename Base, typename Fn, typename Ret>
class implement_visit_function<Base, Fn, Ret> : public Base, private Fn {
  public:
    explicit implement_visit_function(Fn &&fn) : Fn(std::move(fn)) {}
    Ret get_result() { return std::move(*m_ret); }

  protected:
    template <typename T>
    inline void invoke(T v) {
        static_assert(std::is_convertible_v<invoke_result_t<Fn, T>, Ret>);
        m_ret.emplace(static_cast<Ret>(static_cast<Fn &>(*this)(static_cast<T>(v))));
    }

  private:
    std::optional<Ret> m_ret;
};

template <typename Base, typename Fn, typename Ret>
class implement_visit_function<Base, Fn, Ret &> : public Base, private Fn {
  public:
    explicit implement_visit_function(Fn &&fn) : Fn(std::move(fn)) {}
    Ret &get_result() { return *m_ret; }

  protected:
    template <typename T>
    inline void invoke(T v) {
        static_assert(std::is_convertible_v<invoke_result_t<Fn, T>, Ret &>);
        m_ret = &static_cast<Ret &>(static_cast<Fn &>(*this)(static_cast<T>(v)));
    }

  private:
    Ret *m_ret = nullptr;
};

template <typename Base, typename Fn, typename Ret>
class implement_visit_function<Base, Fn, Ret &&> : public Base, private Fn {
  public:
    explicit implement_visit_function(Fn &&fn) : Fn(std::move(fn)) {}
    Ret &&get_result() { return std::move(*m_ret); }

  protected:
    template <typename T>
    inline void invoke(T v) {
        static_assert(std::is_convertible_v<invoke_result_t<Fn, T>, Ret &&>);
        Ret &&name = static_cast<Ret &&>(static_cast<Fn &>(*this)(static_cast<T>(v)));
        m_ret = &name;
    }

  private:
    Ret *m_ret = nullptr;
};

template <typename Base, typename Fn>
class implement_visit_function<Base, Fn, void> : public Base, private Fn {
  public:
    explicit implement_visit_function(Fn &&fn) : Fn(std::move(fn)) {}
    void get_result() {}

  protected:
    template <typename T>
    inline void invoke(T v) {
        static_assert(std::is_void_v<invoke_result_t<Fn, T>>);
        static_cast<Fn &>(*this)(static_cast<T>(v));
    }
};

template <typename...>
inline constexpr bool no = false;

template <typename T>
using remove_cvref_t = std::remove_cv_t<std::remove_reference_t<T>>;

template <typename Derived, typename IsACompleteTypeAtTimeOfAssertion = void>
struct assert_implements_correct_acceptor {};

template <typename Derived>
struct assert_implements_correct_acceptor<Derived, std::void_t<int[sizeof(Derived)]>> {
    static_assert(std::is_same_v<typename Derived::implements_acceptor_for, Derived>,
        "at least one derived acceptor type does not inherit from the correct specialization of "
        "matchbox::implement_acceptor");
};

} // namespace matchbox::detail

namespace matchbox {

template <typename... Ts>
class visitor;

template <typename... Derived>
class acceptor;

template <typename... Ts>
struct type_list {
    using acceptor = matchbox::acceptor<Ts...>;
    using visitor = matchbox::visitor<Ts &...>;
    using const_visitor = matchbox::visitor<const Ts &...>;
    using move_visitor = matchbox::visitor<Ts &&...>;

    template <typename T>
    inline static constexpr bool contains_v = (std::is_same_v<T, Ts> || ...);

    ~type_list() = delete;
};

template <typename... Ts>
class visitor : private detail::declare_visit_function<Ts>... {
  public:
    using visited_types = type_list<Ts...>;

    virtual ~visitor() = default;

    using detail::declare_visit_function<Ts>::visit...;

  protected:
    visitor() = default;
    visitor(const visitor &) = default;
    visitor(visitor &&) = default;
    visitor &operator=(const visitor &) = default;
    visitor &operator=(visitor &&) = default;
};

template <typename... Derived>
class acceptor {
  public:
    using implementors = type_list<Derived...>;
    using visitor = typename implementors::visitor;
    using const_visitor = typename implementors::const_visitor;
    using move_visitor = typename implementors::move_visitor;

    virtual ~acceptor() = default;

    virtual void accept(visitor &) & = 0;
    virtual void accept(const_visitor &) const & = 0;
    virtual void accept(move_visitor &) && = 0;

  protected:
    inline constexpr acceptor() noexcept { (detail::assert_implements_correct_acceptor<Derived>(), ...); }
    acceptor(const acceptor &) = default;
    acceptor(acceptor &&) = default;
    acceptor &operator=(const acceptor &) = default;
    acceptor &operator=(acceptor &&) = default;
};

template <typename Base, typename Derived>
class implement_acceptor : public Base {
  public:
    static_assert(Base::implementors::template contains_v<Derived>,
        "Derived type is not listed in the template arguments of the matchbox::acceptor<> base, this implementation "
        "will never receive a visitor");

    using typename Base::const_visitor;
    using typename Base::move_visitor;
    using typename Base::visitor;

    void accept(visitor &v) & override {
        assert_dynamic_type_is_derived_type();
        v.visit(static_cast<Derived &>(*this));
    }

    void accept(const_visitor &v) const & override {
        assert_dynamic_type_is_derived_type();
        v.visit(static_cast<const Derived &>(*this));
    }

    void accept(move_visitor &v) && override {
        assert_dynamic_type_is_derived_type();
        v.visit(static_cast<Derived &&>(*this));
    }

  protected:
    using acceptor_base = implement_acceptor;

    using Base::Base;

  private:
    template <typename, typename>
    friend struct detail::assert_implements_correct_acceptor;

    using implements_acceptor_for = Derived;

    inline void assert_dynamic_type_is_derived_type() const {
        // `Derived` is not a complete type at the point of the class definition, assert here
        static_assert(std::is_base_of_v<implement_acceptor, Derived>);

        // We try to catch this statically in acceptor::acceptor, but the static check only applies if the derived type
        // is defined (i.e. complete) at the point where its constructor is initiated. As a second line of defense we
        // assert that the dynamic type is correct inside accept(), and thus our static_cast<Derived> is legal.
        MATCHBOX_ASSERT(dynamic_cast<const Derived *>(this) == static_cast<const Derived *>(this),
            "Derived type does not inherit from implement_acceptor<Base, Derived>");
    }
};

} // namespace matchbox

namespace matchbox::detail {

template <typename... F>
struct overload : F... {
    explicit constexpr overload(F... f) : F(f)... {}
    using F::operator()...;
};

template <typename R, typename F>
struct convert_result : F {
    explicit constexpr convert_result(F f) : F(f) {}

    template <typename... Params>
    inline constexpr R operator()(Params &&...args) const {
        return F::operator()(std::forward<Params>(args)...);
    }

    template <typename... Params>
    inline constexpr R operator()(Params &&...args) {
        return F::operator()(std::forward<Params>(args)...);
    }
};

template <typename T>
inline constexpr bool is_variant_type_v = false;

template <typename... Ts>
inline constexpr bool is_variant_type_v<std::variant<Ts...>> = true;

template <typename CVRef>
inline constexpr bool is_variant_v = is_variant_type_v<detail::remove_cvref_t<CVRef>>;

template <typename T>
inline constexpr bool is_optional_type_v = false;

template <typename T>
inline constexpr bool is_optional_type_v<std::optional<T>> = true;

template <typename CVRef>
inline constexpr bool is_optional_v = is_optional_type_v<detail::remove_cvref_t<CVRef>>;

template <typename F, typename OptionalCVRef>
constexpr decltype(auto) visit_optional(F &&f, OptionalCVRef &&optional) {
    // std::optional::operator*() forwards the cvref-qualification of the object.
    return optional.has_value() ? std::forward<F>(f)(*std::forward<OptionalCVRef>(optional))
                                : std::forward<F>(f)(std::nullopt);
}

template <typename OptionalCVRef>
using optional_cvref_value_t = decltype(*std::declval<OptionalCVRef>());

template <typename F, typename VariantCVRef,
    typename IndexList = std::make_index_sequence<std::variant_size_v<detail::remove_cvref_t<VariantCVRef>>>>
inline constexpr bool is_overload_invocable_on_variant_v = false;

template <typename F, typename VariantCVRef, size_t... Indices>
inline constexpr bool is_overload_invocable_on_variant_v<F, VariantCVRef, std::index_sequence<Indices...>>
    = (std::is_invocable_v<F, decltype(std::get<Indices>(std::declval<VariantCVRef>()))> && ...);

template <typename F, typename OptionalCVRef>
inline constexpr bool is_overload_invocable_on_optional_v
    = std::is_invocable_v<F, optional_cvref_value_t<OptionalCVRef>> && std::is_invocable_v<F, std::nullopt_t>;

template <typename F, typename AltListCVRef, typename AltList = detail::remove_cvref_t<AltListCVRef>,
    typename Enable = void>
inline constexpr bool is_overload_invocable_v = false;

template <typename F, typename VariantCVRef, typename... Ts>
inline constexpr bool is_overload_invocable_v<F, VariantCVRef, std::variant<Ts...>,
    std::enable_if_t<is_overload_invocable_on_variant_v<F, VariantCVRef>>>
    = true;

template <typename F, typename OptionalCVRef, typename T>
inline constexpr bool is_overload_invocable_v<F, OptionalCVRef, std::optional<T>,
    std::enable_if_t<is_overload_invocable_on_optional_v<F, OptionalCVRef>>>
    = true;

template <typename F, typename TagCVRef, typename... Ts>
inline constexpr bool
    is_overload_invocable_v<F, TagCVRef, type_list<Ts...>, std::enable_if_t<(std::is_invocable_v<F, Ts> && ...)>>
    = true;

template <typename F, typename AltListCVRef>
struct assert_is_overload_invocable {
    constexpr static bool ok = is_overload_invocable_v<F, AltListCVRef>;
    static_assert(ok,
        "match arms do not cover all alternatives of the argument variant, or their parameters have incorrect cvref "
        "qualification");
    constexpr explicit operator bool() const { return ok; }
};

template <typename Enable, typename... Ts>
struct common_cvref_type_impl {};

template <typename... Ts>
struct common_cvref_type_impl<std::void_t<std::common_type_t<Ts...>>, Ts...> {
    // once we have lifted all cvref qualifiers, we apply std::common_type_t - this will std::decay some of the results
    // if they differ in qualification
    using type = std::common_type_t<Ts...>;
};

template <typename... Ts>
struct common_cvref_type_impl<void, const Ts...> {
    using type = const typename common_cvref_type_impl<void, Ts...>::type;
};

template <typename... Ts>
struct common_cvref_type_impl<void, volatile Ts...> {
    using type = volatile typename common_cvref_type_impl<void, Ts...>::type;
};
template <typename... Ts>
struct common_cvref_type_impl<void, Ts &...> {
    using type = typename common_cvref_type_impl<void, Ts...>::type &;
};

template <typename... Ts>
struct common_cvref_type_impl<void, Ts &&...> {
    using type = typename common_cvref_type_impl<void, Ts...>::type &&;
};

template <typename... Ts>
using common_cvref_type = common_cvref_type_impl<void, Ts...>;

template <typename F, typename... AltParams>
using common_cvref_invoke_result = common_cvref_type<decltype(std::declval<F>()(std::declval<AltParams>()))...>;

template <typename F, typename... AltParams>
using common_cvref_invoke_result_t = typename common_cvref_invoke_result<F, AltParams...>::type;

template <typename F, typename VariantCVRef,
    typename IndexList = std::make_index_sequence<std::variant_size_v<detail::remove_cvref_t<VariantCVRef>>>,
    typename Enable = void>
struct common_variant_invoke_result {};

template <typename F, typename VariantCVRef, size_t... Indices>
struct common_variant_invoke_result<F, VariantCVRef, std::index_sequence<Indices...>,
    std::void_t<common_cvref_invoke_result_t<F, decltype(std::get<Indices>(std::declval<VariantCVRef>()))...>>> {
    using type = common_cvref_invoke_result_t<F, decltype(std::get<Indices>(std::declval<VariantCVRef>()))...>;
};

template <typename F, typename VariantCVRef>
using common_variant_invoke_result_t = typename common_variant_invoke_result<F, VariantCVRef>::type;

template <typename F, typename OptionalCVRef>
using common_optional_invoke_result
    = common_cvref_invoke_result<F, optional_cvref_value_t<OptionalCVRef>, std::nullopt_t>;

template <typename F, typename OptionalCVRef>
using common_optional_invoke_result_t = typename common_optional_invoke_result<F, OptionalCVRef>::type;

template <typename F, typename AltListCVRef, typename AltList = detail::remove_cvref_t<AltListCVRef>,
    typename Enable = void>
struct common_invoke_result;

template <typename F, typename AltListCVRef, typename... Ts>
struct common_invoke_result<F, AltListCVRef, std::variant<Ts...>,
    std::void_t<common_variant_invoke_result_t<F, AltListCVRef>>> : common_variant_invoke_result<F, AltListCVRef> {};

template <typename F, typename OptionalCVRef, typename T>
struct common_invoke_result<F, OptionalCVRef, std::optional<T>,
    std::void_t<common_optional_invoke_result_t<F, OptionalCVRef>>> : common_optional_invoke_result<F, OptionalCVRef> {
};

template <typename F, typename TagCVRef, typename... Ts>
struct common_invoke_result<F, TagCVRef, type_list<Ts...>, std::void_t<common_cvref_invoke_result_t<F, Ts...>>>
    : common_cvref_invoke_result<F, Ts...> {};

template <typename F, typename AltListCVRef>
using common_invoke_result_t = typename common_invoke_result<F, AltListCVRef>::type;

template <typename F, typename AltListCVRef, typename Enable = void>
struct assert_has_common_invoke_result {
    static_assert(detail::no<Enable>,
        "Results of match arms cannot be implicitly converted to a common (cvref-preserving) type, use "
        "matchbox::match<Result>() to specify an explicit return type");
    constexpr explicit operator bool() const { return false; }
};

template <typename F, typename AltListCVRef>
struct assert_has_common_invoke_result<F, AltListCVRef, std::void_t<common_invoke_result_t<F, AltListCVRef>>> {
    constexpr explicit operator bool() const { return true; }
};

template <typename Result, typename Visitor, typename Fn>
class visitor_impl;

template <typename Result, typename... Ts, typename Fn>
class visitor_impl<Result, matchbox::visitor<Ts...>, Fn>
    : public implement_visit_function<matchbox::visitor<Ts...>, Fn, Result, Ts...> {
  public:
    using implement_visit_function<matchbox::visitor<Ts...>, Fn, Result, Ts...>::implement_visit_function;
};

template <typename AcceptorCVRef, typename Enable = void>
struct select_visitor {};

template <typename Acceptor>
struct select_visitor<Acceptor &, std::void_t<typename Acceptor::const_visitor, typename Acceptor::visitor>> {
    using type
        = std::conditional_t<std::is_const_v<Acceptor>, typename Acceptor::const_visitor, typename Acceptor::visitor>;
};

template <typename Acceptor>
struct select_visitor<Acceptor &&, std::void_t<typename Acceptor::const_visitor, typename Acceptor::move_visitor>> {
    using type = std::conditional_t<std::is_const_v<Acceptor>, typename Acceptor::const_visitor,
        typename Acceptor::move_visitor>;
};

template <typename AcceptorCVRef>
using select_visitor_t = typename select_visitor<AcceptorCVRef>::type;

} // namespace matchbox::detail

namespace matchbox {

template <typename Result, typename VariantCVRef, typename... Arms, //
    std::enable_if_t<detail::is_variant_v<VariantCVRef>, int> = 0>
inline constexpr Result match(VariantCVRef &&v, Arms &&...arms) {
    using overload_type = detail::overload<detail::remove_cvref_t<Arms>...>;
    // `if constexpr`: suppress follow-up compiler errors if static assertion fails
    if constexpr(detail::assert_is_overload_invocable<overload_type, VariantCVRef &&>()) {
        return std::visit(detail::convert_result<Result, overload_type>(overload_type(std::forward<Arms>(arms)...)),
            std::forward<VariantCVRef>(v));
    }
}

template <typename VariantCVRef, typename... Arms, //
    std::enable_if_t<detail::is_variant_v<VariantCVRef>, int> = 0>
inline constexpr decltype(auto) match(VariantCVRef &&v, Arms &&...arms) {
    using overload_type = detail::overload<detail::remove_cvref_t<Arms>...>;
    // `if constexpr`: suppress follow-up compiler errors if static assertion fails
    if constexpr(detail::assert_is_overload_invocable<overload_type, VariantCVRef &&>()
        && detail::assert_has_common_invoke_result<overload_type, VariantCVRef &&>()) {
        using result_type = detail::common_invoke_result_t<overload_type, VariantCVRef &&>;
        return std::visit(
            detail::convert_result<result_type, overload_type>(overload_type(std::forward<Arms>(arms)...)),
            std::forward<VariantCVRef>(v));
    }
}

template <typename Result, typename OptionalCVRef, typename... Arms, //
    std::enable_if_t<detail::is_optional_v<OptionalCVRef>, int> = 0>
inline constexpr Result match(OptionalCVRef &&o, Arms &&...arms) {
    using overload_type = detail::overload<detail::remove_cvref_t<Arms>...>;
    // `if constexpr`: suppress follow-up compiler errors if static assertion fails
    if constexpr(detail::assert_is_overload_invocable<overload_type, OptionalCVRef &&>()) {
        return detail::visit_optional(
            detail::convert_result<Result, overload_type>(overload_type(std::forward<Arms>(arms)...)),
            std::forward<OptionalCVRef>(o));
    }
}

template <typename OptionalCVRef, typename... Arms, //
    std::enable_if_t<detail::is_optional_v<OptionalCVRef>, int> = 0>
inline constexpr decltype(auto) match(OptionalCVRef &&o, Arms &&...arms) {
    using overload_type = detail::overload<detail::remove_cvref_t<Arms>...>;
    // `if constexpr`: suppress follow-up compiler errors if static assertion fails
    if constexpr(detail::assert_is_overload_invocable<overload_type, OptionalCVRef &&>()
        && detail::assert_has_common_invoke_result<overload_type, OptionalCVRef &&>()) {
        using result_type = detail::common_invoke_result_t<overload_type, OptionalCVRef &&>;
        return detail::visit_optional(
            detail::convert_result<result_type, overload_type>(overload_type(std::forward<Arms>(arms)...)),
            std::forward<OptionalCVRef>(o));
    }
}

// TODO make all of these constexpr in C++20 (when we are allowed to do virtual function calls)

template <typename Result, typename AcceptorCVRef, typename... Arms,
    typename Visitor = detail::select_visitor_t<AcceptorCVRef &&>>
inline Result match(AcceptorCVRef &&acceptor, Arms &&...arms) {
    using overload_type = detail::overload<detail::remove_cvref_t<Arms>...>;
    using visited_types = typename Visitor::visited_types;
    // `if constexpr`: suppress follow-up compiler errors if static assertion fails
    if constexpr(detail::assert_is_overload_invocable<overload_type, visited_types>()) {
        detail::visitor_impl<Result, Visitor, overload_type> vis(overload_type{std::forward<Arms>(arms)...});
        std::forward<AcceptorCVRef>(acceptor).accept(vis);
        return vis.get_result();
    }
}

template <typename AcceptorCVRef, typename... Arms, typename Visitor = detail::select_visitor_t<AcceptorCVRef &&>>
inline decltype(auto) match(AcceptorCVRef &&acceptor, Arms &&...arms) {
    using overload_type = detail::overload<detail::remove_cvref_t<Arms>...>;
    using visited_types = typename Visitor::visited_types;
    // `if constexpr`: suppress follow-up compiler errors if static assertion fails
    if constexpr(detail::assert_is_overload_invocable<overload_type, visited_types>()
        && detail::assert_has_common_invoke_result<overload_type, visited_types>()) {
        using result_type = detail::common_invoke_result_t<overload_type, visited_types>;
        detail::visitor_impl<result_type, Visitor, overload_type> vis(overload_type{std::forward<Arms>(arms)...});
        std::forward<AcceptorCVRef>(acceptor).accept(vis);
        return vis.get_result();
    }
}

} // namespace matchbox
