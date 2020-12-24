#include <iostream>
#include <optional>
#include <string>
#include <vector>

// Pravimo funkciju transform koja ce transformisati 
// dati optional opt tako sto ce primeniti datu funkciju
// f na vrednost unutar optional objekta ukoliko on 
// ima vrednost, inace vratiti prazan optional

// Postavlja se pitanje: koji je potpis funkcije transform?
// Mozemo pokusati sa: 
//   ??? transform(std::optional<T1>& opt, F f);
// Medjutim, ne znamo koji je povratni tip
// Tip rezultata je isti kao tip povratne vrednosti funkcije f
// Samo staviti auto kao povratni tip ne moze, stoga mozemo
// koristiti decltype() da opisemo povratni tip:
//   std::optional<decltype(f(opt.value()))>       
// Medjutim, ni ovo ne prolazi kompilaciju ako se stavi umesto 
// povratnog tipa jer tokom parsiranja povratnog tipa funkcije
// kompajler ne vidi parametre funkcije. Stoga, postoji specijalna
// sintaksa za deklaraciju ovakvih funkcija, gde se ipak koristi
// auto, a izraz za racunanje povratnog tipa se navodi nakon
// liste parametara funkcije:

template <typename T1, typename F>
auto transform(const std::optional<T1>& opt, F f)
    -> std::optional<decltype(f(opt.value()))>
{
    // Mozemo "testirati" da li optional ima vrednost
    // na trivijalan nacin
    if (opt) {
        return f(opt.value());
    } else {
        // https://en.cppreference.com/w/cpp/language/initialization
        return {};
    }
}


int main(int argc, char* argv[])
{
    // Kada pravimo optional, podrazumevano nema vrednost
    // Inace, mozemo koristiti std::make_optional()
    std::optional<int> i;

    auto res = transform(i, isalnum);

    return 0;
}
