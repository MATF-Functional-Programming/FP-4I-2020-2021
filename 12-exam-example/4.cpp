// Zadatak 4
// Koristeći biblioteku range-v3, a bez upotrebe for, while, do-while petlji,
// komande goto, rekurzije, i algortima std::for_each implementirati sledeći
// zadatak:
// Napisati program koji sa standardnog ulaza čita reč po reč i
// briše uzastopna ponavljanja iste reči. Svaku reč ispisuje u novom redu.
//
// Za ulaz:
// thunder thunder
// thunder thunder
// i was caught
// in the middle of a railroad track
// thunder thunder
// thunder thunder
//
// Treba ispisati
//
// thunder
// i
// was
// caught
// in
// the
// middle
// of
// a
// railroad
// track
// thunder


#include <iostream>
#include <string>
#include <vector>
#include <functional>

#include <range/v3/view.hpp>
#include <range/v3/action.hpp>
#include <range/v3/algorithm.hpp>
#include <range/v3/to_container.hpp>

#include <cassert>

using namespace ranges::v3;
namespace views = ranges::v3::view;
namespace actions = ranges::v3::action;

// POCETAK STUDENTSKOG KODA

// KRAJ STUDENTSKOG KODA

int main(int argc, char **argv)
{
// POCETAK STUDENTSKOG KODA

// KRAJ STUDENTSKOG KODA
}

