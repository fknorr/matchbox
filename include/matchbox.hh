#pragma once

#include <functional>
#include <optional>
#include <type_traits>
#include <variant>

#if !defined(MATCHBOX_ASSERT) && !defined(NDEBUG)
#include <cassert>
#define MATCHBOX_ASSERT(x, msg) assert(x &&msg)
#endif

namespace matchbox::detail {

template <typename T>
class declare_visit_fn {
  public:
    virtual void visit(T) = 0;
};

template <typename Base, typename Fn, typename Ret, typename... Ts>
class impl_visit_fn;

template <typename Base, typename Fn, typename Ret, typename T, typename... Ts>
class impl_visit_fn<Base, Fn, Ret, T, Ts...> : public impl_visit_fn<Base, Fn, Ret, Ts...> {
  private:
    using next = impl_visit_fn<Base, Fn, Ret, Ts...>;

  public:
    explicit impl_visit_fn(Fn &&fn) : next(std::move(fn)) {}
    void visit(T v) final { next::template invoke<T>(static_cast<T>(v)); }
    using next::visit;
};

template <typename Base, typename Fn, typename Ret>
class impl_visit_fn<Base, Fn, Ret> : public Base {
  public:
    explicit impl_visit_fn(Fn &&fn) : m_fn(std::move(fn)) {}
    Ret get_result() { return std::move(*m_ret); }

  protected:
    template <typename T>
    inline void invoke(T v) {
        static_assert(std::is_convertible_v<std::invoke_result_t<Fn, T>, Ret>);
        m_ret.emplace(static_cast<Ret>(m_fn(static_cast<T>(v))));
    }

  private:
    Fn m_fn;
    std::optional<Ret> m_ret;
};

template <typename Base, typename Fn, typename Ret>
class impl_visit_fn<Base, Fn, Ret &> : public Base {
  public:
    explicit impl_visit_fn(Fn &&fn) : m_fn(std::move(fn)) {}
    Ret &get_result() { return *m_ret; }

  protected:
    template <typename T>
    inline void invoke(T v) {
        static_assert(std::is_convertible_v<std::invoke_result_t<Fn, T>, Ret &>);
        m_ret = &static_cast<Ret &>(m_fn(static_cast<T>(v)));
    }

  private:
    Fn m_fn;
    Ret *m_ret = nullptr;
};

template <typename Base, typename Fn, typename Ret>
class impl_visit_fn<Base, Fn, Ret &&> : public Base {
  public:
    explicit impl_visit_fn(Fn &&fn) : m_fn(std::move(fn)) {}
    Ret &&get_result() { return std::move(*m_ret); }

  protected:
    template <typename T>
    inline void invoke(T v) {
        static_assert(std::is_convertible_v<std::invoke_result_t<Fn, T>, Ret &&>);
        Ret &&name = static_cast<Ret &&>(m_fn(static_cast<T>(v)));
        m_ret = &name;
    }

  private:
    Fn m_fn;
    Ret *m_ret;
};

template <typename Base, typename Fn>
class impl_visit_fn<Base, Fn, void> : public Base {
  public:
    explicit impl_visit_fn(Fn &&fn) : m_fn(std::move(fn)) {}
    void get_result() {}

  protected:
    template <typename T>
    inline void invoke(T v) {
        static_assert(std::is_void_v<std::invoke_result_t<Fn, T>>);
        m_fn(static_cast<T>(v));
    }

  private:
    Fn m_fn;
};

template <typename... Ts>
struct polymorphic_visitor_tag {};

template <typename...>
inline constexpr bool constexpr_false = false;

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
class visitor : public detail::declare_visit_fn<Ts>... {
  public:
    using polymorphic_visitor_tag = detail::polymorphic_visitor_tag<Ts...>;

    visitor() = default;
    visitor(const visitor &) = default;
    visitor(visitor &&) = default;
    visitor &operator=(const visitor &) = default;
    visitor &operator=(visitor &&) = default;

    virtual ~visitor() = default;

    using detail::declare_visit_fn<Ts>::visit...;
};

template <typename... Derived>
class acceptor {
  public:
    using visitor = matchbox::visitor<Derived &...>;
    using const_visitor = matchbox::visitor<const Derived &...>;
    using move_visitor = matchbox::visitor<Derived &&...>;

    virtual void accept(visitor &) & = 0;
    virtual void accept(const_visitor &) const & = 0;
    virtual void accept(move_visitor &) && = 0;

  protected:
    inline constexpr acceptor() noexcept { (detail::assert_implements_correct_acceptor<Derived>(), ...); }

    ~acceptor() = default;
    acceptor(const acceptor &) = default;
    acceptor(acceptor &&) = default;
    acceptor &operator=(const acceptor &) = default;
    acceptor &operator=(acceptor &&) = default;
};

template <typename Base, typename Derived>
class implement_acceptor : public Base {
  public:
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
    inline constexpr implement_acceptor() noexcept { //
        static_assert(std::is_base_of_v<implement_acceptor, Derived>);
    }

    ~implement_acceptor() = default;
    implement_acceptor(const implement_acceptor &) = default;
    implement_acceptor(implement_acceptor &&) = default;
    implement_acceptor &operator=(const implement_acceptor &) = default;
    implement_acceptor &operator=(implement_acceptor &&) = default;

  private:
    template <typename, typename>
    friend struct detail::assert_implements_correct_acceptor;

    using implements_acceptor_for = Derived;

    void assert_dynamic_type_is_derived_type() const {
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

template <typename Ret, typename F, typename OptionalCVRef>
constexpr Ret visit_optional_r(F &&f, OptionalCVRef &&optional) {
    // std::optional::operator*() forwards the cvref-qualification of the object.
    // TODO use std::invoke here when it becomes constexpr with C++20.
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
struct assert_overload_invocable {
    static_assert(constexpr_false<Enable>,
        "match arms do not cover all alternatives of the argument variant, or their parameters have incorrect cvref "
        "qualification");
};

template <typename F, typename VariantCVRef, typename... Ts>
struct assert_overload_invocable<F, VariantCVRef, std::variant<Ts...>,
    std::enable_if_t<is_overload_invocable_on_variant_v<F, VariantCVRef>>> {};

template <typename F, typename OptionalCVRef, typename T>
struct assert_overload_invocable<F, OptionalCVRef, std::optional<T>,
    std::enable_if_t<is_overload_invocable_on_optional_v<F, OptionalCVRef>>> {};

template <typename F, typename TagCVRef, typename... Ts>
struct assert_overload_invocable<F, TagCVRef, polymorphic_visitor_tag<Ts...>,
    std::enable_if_t<(std::is_invocable_v<F, Ts> && ...)>> {};

template <typename... Ts>
struct common_cvref_type {
    // once we have lifted all cvref qualifiers, we apply std::common_type_t - this will std::decay some of the results
    // if they differ in qualification
    using type = std::common_type_t<Ts...>;
};

template <typename... Ts>
struct common_cvref_type<const Ts...> {
    using type = const typename common_cvref_type<Ts...>::type;
};

template <typename... Ts>
struct common_cvref_type<volatile Ts...> {
    using type = volatile typename common_cvref_type<Ts...>::type;
};
template <typename... Ts>
struct common_cvref_type<Ts &...> {
    using type = typename common_cvref_type<Ts...>::type &;
};

template <typename... Ts>
struct common_cvref_type<Ts &&...> {
    using type = typename common_cvref_type<Ts...>::type &&;
};

template <typename... Ts>
using common_cvref_type_t = typename common_cvref_type<Ts...>::type;

template <typename F, typename VariantCVRef,
    typename IndexList = std::make_index_sequence<std::variant_size_v<detail::remove_cvref_t<VariantCVRef>>>>
struct common_variant_invoke_result;

template <typename F, typename VariantCVRef, size_t... Indices>
struct common_variant_invoke_result<F, VariantCVRef, std::index_sequence<Indices...>> {
    using type
        = common_cvref_type_t<std::invoke_result_t<F, decltype(std::get<Indices>(std::declval<VariantCVRef>()))>...>;
};

template <typename F, typename OptionalCVRef>
using common_optional_invoke_result = common_cvref_type<std::invoke_result_t<F, optional_cvref_value_t<OptionalCVRef>>,
    std::invoke_result_t<F, std::nullopt_t>>;

template <typename F, typename AltListCVRef, typename AltList = detail::remove_cvref_t<AltListCVRef>,
    typename Enable = void>
struct common_invoke_result;

template <typename F, typename AltListCVRef, typename... Ts>
struct common_invoke_result<F, AltListCVRef, std::variant<Ts...>> : common_variant_invoke_result<F, AltListCVRef> {};

template <typename F, typename OptionalCVRef, typename T>
struct common_invoke_result<F, OptionalCVRef, std::optional<T>> : common_optional_invoke_result<F, OptionalCVRef> {};

template <typename F, typename TagCVRef, typename... Ts>
struct common_invoke_result<F, TagCVRef, polymorphic_visitor_tag<Ts...>> {
    using type = common_cvref_type_t<std::invoke_result_t<F, Ts>...>;
};

template <typename F, typename AltListCVRef>
using common_invoke_result_t = typename common_invoke_result<F, AltListCVRef>::type;

template <typename F, typename AltListCVRef, typename Enable = void>
struct assert_invoke_results_compatible {
    static_assert(detail::constexpr_false<Enable>,
        "results of match arms cannot be implicitly converted to a common type, use matchbox::match<Result>() to "
        "specify an explicit return type");
};

template <typename F, typename AltListCVRef>
struct assert_invoke_results_compatible<F, AltListCVRef, std::void_t<common_invoke_result_t<F, AltListCVRef>>> {};

template <typename CVRef, typename Enable = void>
inline constexpr bool is_polymorphic_visitor_v = false;

template <typename CVRef>
inline constexpr bool
    is_polymorphic_visitor_v<CVRef, std::void_t<typename detail::remove_cvref_t<CVRef>::polymorphic_visitor_tag>>
    = true;

template <typename Result, typename Visitor, typename Fn>
class visitor_impl;

template <typename Result, typename... Ts, typename Fn>
class visitor_impl<Result, matchbox::visitor<Ts...>, Fn>
    : public impl_visit_fn<matchbox::visitor<Ts...>, Fn, Result, Ts...> {
  public:
    using impl_visit_fn<matchbox::visitor<Ts...>, Fn, Result, Ts...>::impl_visit_fn;
};

template <typename T, typename Visitor, typename Enable = void>
inline constexpr bool accepts_visitor_v = false;

template <typename T, typename Visitor>
inline constexpr bool
    accepts_visitor_v<T, Visitor, std::void_t<decltype(std::declval<T>().accept(std::declval<Visitor &>()))>>
    = true;

template <typename AcceptorCVRef, typename Enable = void>
struct select_visitor;

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
    detail::assert_overload_invocable<overload_type, VariantCVRef &&>();

    return std::visit(detail::convert_result<Result, overload_type>(overload_type(std::forward<Arms>(arms)...)),
        std::forward<VariantCVRef>(v));
}

template <typename VariantCVRef, typename... Arms, //
    std::enable_if_t<detail::is_variant_v<VariantCVRef>, int> = 0>
inline constexpr decltype(auto) match(VariantCVRef &&v, Arms &&...arms) {
    using overload_type = detail::overload<detail::remove_cvref_t<Arms>...>;
    detail::assert_overload_invocable<overload_type, VariantCVRef &&>();
    detail::assert_invoke_results_compatible<overload_type, VariantCVRef &&>();

    using result_type = detail::common_invoke_result_t<overload_type, VariantCVRef &&>;
    return std::visit(detail::convert_result<result_type, overload_type>(overload_type(std::forward<Arms>(arms)...)),
        std::forward<VariantCVRef>(v));
}

template <typename Result, typename OptionalCVRef, typename... Arms, //
    std::enable_if_t<detail::is_optional_v<OptionalCVRef>, int> = 0>
inline constexpr Result match(OptionalCVRef &&o, Arms &&...arms) {
    using overload_type = detail::overload<detail::remove_cvref_t<Arms>...>;
    detail::assert_overload_invocable<overload_type, OptionalCVRef &&>();

    return detail::visit_optional_r<Result>(
        detail::convert_result<Result, overload_type>(overload_type(std::forward<Arms>(arms)...)),
        std::forward<OptionalCVRef>(o));
}

template <typename OptionalCVRef, typename... Arms, //
    std::enable_if_t<detail::is_optional_v<OptionalCVRef>, int> = 0>
inline constexpr decltype(auto) match(OptionalCVRef &&o, Arms &&...arms) {
    using overload_type = detail::overload<detail::remove_cvref_t<Arms>...>;
    detail::assert_overload_invocable<overload_type, OptionalCVRef &&>();
    detail::assert_invoke_results_compatible<overload_type, OptionalCVRef &&>();

    using result_type = detail::common_invoke_result_t<overload_type, OptionalCVRef &&>;
    return detail::visit_optional_r<result_type>(
        detail::convert_result<result_type, overload_type>(overload_type(std::forward<Arms>(arms)...)),
        std::forward<OptionalCVRef>(o));
}

// TODO make all of these constexpr in C++20 (when we are allowed to do virtual function calls)

template <typename Result, typename Acceptor, typename... Arms,
    typename Visitor = detail::select_visitor_t<Acceptor &&>>
inline Result match(Acceptor &&acceptor, Arms &&...arms) {
    using overload_type = detail::overload<std::decay_t<Arms>...>;
    using visitor_tag = typename Visitor::polymorphic_visitor_tag;
    detail::assert_overload_invocable<overload_type, visitor_tag>();

    detail::visitor_impl<Result, Visitor, overload_type> vis(overload_type{std::forward<Arms>(arms)...});
    std::forward<Acceptor>(acceptor).accept(vis);
    return vis.get_result();
}

template <typename Acceptor, typename... Arms, typename Visitor = detail::select_visitor_t<Acceptor &&>>
inline decltype(auto) match(Acceptor &&acceptor, Arms &&...arms) {
    using overload_type = detail::overload<std::decay_t<Arms>...>;
    using visitor_tag = typename Visitor::polymorphic_visitor_tag;
    detail::assert_overload_invocable<overload_type, visitor_tag>();
    detail::assert_invoke_results_compatible<overload_type, visitor_tag>();

    using result_type = detail::common_invoke_result_t<overload_type, visitor_tag>;
    detail::visitor_impl<result_type, Visitor, overload_type> vis(overload_type{std::forward<Arms>(arms)...});
    std::forward<Acceptor>(acceptor).accept(vis);
    return vis.get_result();
}

} // namespace matchbox
