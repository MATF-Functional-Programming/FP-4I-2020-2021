// Zadatak: Napisati sablonsku funkciju is_sorted 
// koja proverava da li je data kolekcija sortirana.

#include <iostream>
#include <string>
#include <numeric>
#include <vector>

// Mozemo iskoristiti std::inner_product funkciju
// Operacije * i + cemo zameniti sa <= i &&
// Dakle, za kolekcije xs i ys, racunamo:
// (x1 <= y1) && (x2 <= y2) && ...
// Mi zapravo poredimo kolekciju samu sa sobom
// pa ce ys biti isto xs samo bez prvog elementa
// Pocetna vrednost (akumulator) je true
template <typename T>
bool is_sorted(const T& xs)
{
    return std::inner_product(
        std::cbegin(xs), std::cend(xs) - 1,
        std::cbegin(xs) + 1,
        true,
        // Mozemo koristiti auto takodje
        [](const bool &x, const bool &y) { return x && y; },
        // Ovde ne znamo tip elemenata kolekcije, pa stoga auto
        [](const auto &x, const auto &y) { return x <= y; }
    );
}

int main(int argc, char *argv[])
{
    const std::string text = "Hooloovoo";
    std::cerr << text << ": " << is_sorted(text) << std::endl;

    const std::string word = "Almost";
    std::cerr << word << ": " << is_sorted(word) << std::endl;

    const std::vector<int> numbers{ 1, 2, 3, 3, 4, 5, 6, 6, 7 };
    std::cerr << "numbers: " << is_sorted(word) << std::endl;

    return 0;
}
