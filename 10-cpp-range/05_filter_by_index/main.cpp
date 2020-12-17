#include <iostream>
#include <vector>
#include <functional>
#include <range/v3/view.hpp>

using namespace ranges::v3;

bool index_filter(size_t index) {
    return index % 3 != 0;
}

int main(int argc, char *argv[])
{
    std::vector<int> xs = { -1, -3, -5, 1, 3, 5};

    // Zipujemo xs sa listom svih prirodnih brojeva,
    // filtriramo po indeksu, posle toga zaboravljamo indeks.
    auto results = view::zip(xs, view::ints(1, unreachable))
                 | view::filter([] (auto value) { return index_filter(value.second); })
                 | view::transform([] (auto value) { return value.first; });

    for (auto value: results)
        std::cout << value << std::endl;

    return 0;
}
