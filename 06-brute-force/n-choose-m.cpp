#include <iostream>
#include <vector>

void print_picked(std::vector<int>& picked)
{
    for (auto& e : picked) {
        std::cout << e;
        if (e != picked.back()) {
            std::cout << ',';
        }
    }
    std::cout << '\n';
}

void choose_aux(int n, int m, std::vector<int>& picked)
{
    if (m == 0) {
        print_picked(picked);
        return;
    }

    int largest = picked.empty() ? n : picked.back() - 1;
    for (int i = largest; i > 0; --i) {
        picked.push_back(i);
        choose_aux(n, m - 1, picked);
        picked.pop_back();
    }
}

void choose(int n, int m)
{
    std::vector<int> picked;
    choose_aux(n, m, picked);
}

int main(int argc, char *argv[])
{
    int n = 5;
    int m = 3;
    choose(n, m);
    return 0;
}
