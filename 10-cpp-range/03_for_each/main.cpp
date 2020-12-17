#include <iostream>
#include <vector>
#include <functional>
#include <range/v3/view.hpp>

using namespace ranges::v3;
using namespace std::placeholders;

int main(int argc, char *argv[])
{
    std::vector<int> xs = { -1, -3, -5, 1, 3, 5};

    // Pravimo pogled koji ce se sastojati od tekstualnih
    // reprezentacija apsolutnih vrednosti brojeva iz xs,
    // ali samo onih za koje vazi da je x < 6
    auto results =
        xs | view::transform(abs)
           | view::filter(std::bind(std::less<>(), _1, 6))
           | view::transform([] (auto value) { return std::to_string(value); });

    for (auto value: results) {
        std::cout << value << std::endl;
    }

    return 0;
}
