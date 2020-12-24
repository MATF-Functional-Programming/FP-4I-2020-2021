#include <fstream>
#include <iostream>
#include <iterator>
#include <string>
#include <variant>

#include <cassert>

// Pravimo pomocni funkcionalni objekat koji ce u sebi
// sadrzati vise funkcionalnih objekata 
// "overloaded function objects"
template <typename... Fs>
struct overloaded : Fs... { using Fs::operator()...; };

template <typename... Fs> overloaded(Fs...) -> overloaded<Fs...>;


// Nasa klasa program se moze nalaziti u jednom od tri stanja:
// init_t, running_t i finished_t 
// Pritom, samo ukoliko smo u stanju running_t smemo uraditi logiku,
// u finished_t stanju smemo samo pokupiti rezultat
// Kako obezbediti ovo?
class program_t {
private:
    class init_t {
    };

    class running_t {
    public:
        running_t(const std::string& file_name)
            : m_file(file_name)
        {
        }

        void count_words()
        {
            m_count = std::distance(
                    std::istream_iterator<std::string>(m_file),
                    std::istream_iterator<std::string>());
        }

        unsigned count() const
        {
            return m_count;
        }

    private:
        unsigned m_count = 0;
        std::ifstream m_file;
    };

    class finished_t {
    public:
        finished_t(unsigned count = 0)
            : m_count(count)
        {
        }

        unsigned count() const
        {
            return m_count;
        }

    private:
        unsigned m_count;

    };

    // Tip koji u jednom trenutku moze biti samo jedno od navedenog
    // Slicno kao Either u Haskell-u s tim sto je Either ogranicen na 2
    std::variant<init_t, running_t, finished_t> m_state;

public:
    // Prilikom konstrukcije smo u inicijalnom stanju
    program_t()
        : m_state(init_t())
    {
    }

    void count_words(const std::string& file_name)
    {
        // Samo mozemo pozvati brojanje ako smo u inicijalnom stanju
        assert(m_state.index() == 0);

        // Menjamo stanje
        m_state = running_t(file_name);

        // Brojimo (sada mozemo jer je m_state tipa running_t)
        std::get<running_t>(m_state).count_words();

        // Brojanje moze trajati, radi simplifikacije nemamo 
        // paralelnih poslova - ali za njih smo u running_t stanju

        // Prelazimo u zavrsno stanje
        counting_finished();
    }

    void counting_finished()
    {
        const auto* state = std::get_if<running_t>(&m_state);

        assert(state != nullptr);

        m_state = finished_t(state->count());
    }

    unsigned count() const
    {
        // Ukoliko pozovemo count(), rezultat zavisi od toga
        // u kom smo trenutno stanju
        // Koristimo nas predefinisani funkcionalni objekat i 
        // funkciju std::visit kako bismo posetili taj objekat
        // Pozvace se odredjena funkcija u zavisnosti od toga 
        // u kom smo trenutno stanju
        return std::visit(overloaded {
                    [](init_t) {
                        return (unsigned)0;
                    },
                    [](const running_t& state) {
                        return state.count();
                    },
                    [](const finished_t& state) {
                        return state.count();
                    }
                }, m_state);
    }
};


int main(int argc, char* argv[])
{
    program_t program;
    program.count_words("main.cpp");

    std::cout << program.count() << std::endl;
}
