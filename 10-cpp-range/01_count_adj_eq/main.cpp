// Zadatak: Napisati sablonsku funkciju count_adj_equals 
// koja vraca broj uzastopnih jednakih elemenata kolekcije.

#include <iostream>
#include <string>
#include <numeric>
#include <vector>

// Mozemo iskoristiti std::inner_product funkciju
// Operacije * i + cemo zameniti sa == i +
// Dakle, za kolekcije xs i ys, racunamo:
// (x1 == y1) + (x2 == y2) + ...
// Mi zapravo poredimo kolekciju samu sa sobom
// pa ce ys biti isto xs samo bez prvog elementa
// Pocetna vrednost (akumulator) je 0
template <typename T>
int count_adj_equals(const T& xs)
{
    return std::inner_product(
        std::cbegin(xs), std::cend(xs) - 1,
        std::cbegin(xs) + 1,
        0,
        // Mozemo koristiti auto takodje
        [](const int& x, const int &y) { return x + y; },
        // Ovde ne znamo tip elemenata kolekcije, pa stoga auto
        [](const auto& x, const auto &y) { return x == y ? 1 : 0; }
    );
}

int main(int argc, char *argv[])
{
    const std::string text = "Hooloovoo";

    std::cerr << text << ": " << count_adj_equals(text) << std::endl;

    const std::vector<double> numbers{ -1.0, 2.36, 65.4, 65.4, 65.4, -1.0, 0.0, 5.4 };

    std::cerr << "numbers : " << count_adj_equals(numbers) << std::endl;

    return 0;
}
