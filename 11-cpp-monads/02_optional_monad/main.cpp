#include <algorithm>
#include <iostream>
#include <optional>
#include <string>
#include <vector>

// Pandan funkcijama u Haskell-u tipa: a -> m b
std::optional<std::string> user_full_name(const std::string& login)
{
    return { "Full name for " + login };
}

// Pravicemo kompoziciju ovih funkcija, ali imamo problem kao i pre 
// jer obe vracaju optional<T> a primaju T pa je kompozicija nemoguca 
std::optional<std::string> to_html(const std::string& text)
{
    return { "<b>" + text + "</b>" };
}

// Funkcija za transformisanje sa prethodnog casa
template <typename T, typename F>
auto transform(const std::optional<T>& opt, F f)
    -> decltype(std::make_optional(f(opt.value())))
{
    if (opt) {
        return std::make_optional(f(opt.value()));
    } else {
        return {};
    }
}

// U Haskell-u smo videli da ne mozemo da koristimo funktore/aplikative kad
// radimo sa ovakvim funkcijama jer onda dobijamo "kutije u kutijama".
// Stoga smo pisali funkciju koja "spaja" kutije:
template <typename T>
std::optional<T> join(const std::optional<std::optional<T>>& opt)
{
    if (opt) {
        return opt.value();
    } else {
        return {};
    }
}

// U Haskell-u postoje monade kao koncept i prirodno se resava
// pomenuti problem uvodjenjem operatora bind (>>=).
// Operator bind u Haskell-u prima funkcije tipa: a -> m a
// Definisimo ga u C++-u:
template <typename T, typename F>
auto mbind(const std::optional<T>& opt, F f)
    -> decltype(f(opt.value()))
{
    if (opt) {
        return f(opt.value());
    } else {
        return {};
    }
}

// Pravimo infiksni operator kao precicu (do sada smo mogli
// da nas mbind koristimo samo prefiksno - videti ispod)
template <typename T, typename F>
auto operator|(const std::optional<T> &xs, F f)
{
    return mbind(xs, f);
}


// Pomocni operator za ispis optional-a
template <typename T>
std::ostream& operator<< (std::ostream& out, const std::optional<T> &value)
{
    if (value) {
        out << "value:" << *value;
    } else {
        out << "empty";
    }
    return out;
}


int main(int argc, char* argv[])
{
    // Pocinjemo od opcionog string-a
    std::optional<std::string> login;

    // Ako zelimo da koristimo transform, onda dobijamo kutije u kutijama,
    // pa ih uz pomoc join funkcije spajamo pre svakog prosledjivanja drugoj
    // funkciji
    std::cout
        << "test empty join-transform:" << std::endl
        << join(transform(
               join(transform(
                   login,
                   user_full_name)),
               to_html))
        << std::endl;

    login = "jsmith";

    std::cout
        << "test non-empty join-transform:" << std::endl
        << join(transform(
               join(transform(
                   login,
                   user_full_name)),
               to_html))
        << std::endl;

    login.reset();

    // Kompozicija `join` i `transform` je zapravo `mbind` kao sto smo
    // videli, pa mozemo skratiti:
    std::cout
        << "test empty mbind:" << std::endl
        << mbind(
               mbind(
                   login,
                   user_full_name),
               to_html)
        << std::endl;

    login = "jsmith";

    std::cout
        << "test non-empty mbind:" << std::endl
        << mbind(
               mbind(
                   login,
                   user_full_name),
               to_html)
        << std::endl;

    login.reset();

    // Ili, ukoliko koristimo nas operator| kao infiksnu precicu dobijamo
    // isti rezultat kao i u Haskell-u (vrednost sleva a funkcija zdesna):
    std::cout
        << "test empty |:" << std::endl
        << (login | user_full_name | to_html)
        << std::endl;

    login = "jsmith";

    std::cout
        << "test non-empty |:" << std::endl
        << (login | user_full_name | to_html)
        << std::endl;

    return 0;
}
