#include <algorithm>
#include <iostream>
#include <vector>

/*
Input:
2
12 6 6 6 6 6 12 12 12 12 12 12 12 12 12 12
12 9 3 12 6 6 9 3 12 9 12 9 12 12 6 6

Output:
2
9
 */

constexpr int INF = 999, N_CLOCKS = 16, N_SWITCHES = 10;

const std::vector<int> conn[N_SWITCHES] = {
    {0, 1, 2},
    {3, 7, 9, 11},
    {4, 10, 14, 15},
    {0, 4, 5, 6, 7},
    {6, 7, 8, 10, 12},
    {0, 2, 14, 15},
    {3, 14, 15},
    {4, 5, 7, 14, 15},
    {1, 2, 3, 4, 5},
    {3, 4, 5, 9, 13}
};

void turn(int time[N_CLOCKS], int sw_num)
{
    for (int clock : conn[sw_num]) {
        ++time[clock];
        time[clock] %= 4;
    }
}

bool check_aligned(const int time[N_CLOCKS])
{
    for (int clock = 0; clock < N_CLOCKS; ++clock) {
        if (time[clock] != 0) {
            return false;
        }
    }
    return true;
}

int sync(int time[N_CLOCKS], int sw_num)
{
    if (sw_num == N_SWITCHES) {
        return check_aligned(time) ? 0 : INF;
    }

    int min = INF;
    for (int turns = 0; turns < 4; ++turns) {
        min = std::min(min, sync(time, sw_num + 1) + turns);
        turn(time, sw_num);
    }

    return min;
};

int sync(int time[N_CLOCKS])
{
    const int ret = sync(time, 0);
    return ret >= INF ? -1 : ret;
}

int main(int argc, char *argv[])
{
    int num_cases;  // <= 30
    std::cin >> num_cases;

    for (int i = 0; i < num_cases; ++i) {
        int time[N_CLOCKS];
        for (int j = 0; j < N_CLOCKS; ++j) {
            std::cin >> time[j];
            time[j] = time[j] == 12 ? 0 : time[j] / 3;
        }
        std::cout << sync(time) << '\n';
    }

    return 0;
}
