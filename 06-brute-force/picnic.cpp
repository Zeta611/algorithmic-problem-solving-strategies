#include <iostream>

/*
Input
3
2 1
0 1
4 6
0 1 1 2 2 3 3 0 0 2 1 3
6 10
0 1 0 2 1 2 1 3 1 4 2 3 2 4 3 4 3 5 4 5

Output
1
3
4
 */

int pair_count_aux(int friend_cnt, bool are_friends[10][10], bool matched[10])
{
    int first_free = -1;
    for (int i = 0; i < friend_cnt; ++i) {
        if (!matched[i]) {
            first_free = i;
            break;
        }
    }
    if (first_free == -1) { return 1; }

    int pair_cnt = 0;
    for (int to_match = first_free + 1; to_match < friend_cnt; ++to_match) {
        if (!matched[to_match] && are_friends[to_match][first_free]) {
            matched[to_match] = matched[first_free] = true;
            pair_cnt += pair_count_aux(friend_cnt, are_friends, matched);
            matched[to_match] = matched[first_free] = false;
        }
    }
    return pair_cnt;
}

int pair_count(int friend_cnt, bool are_friends[10][10])
{
    bool matched[10] = {false};
    return pair_count_aux(friend_cnt, are_friends, matched);
}

int main(int argc, char *argv[])
{
    int test_cases;
    std::cin >> test_cases;

    bool are_friends[10][10];
    for (int i = 0; i < test_cases; ++i) {
        int friend_cnt, pairs;  // 2 <= students <= 10
        std::cin >> friend_cnt >> pairs;

        // Initialize are_friends
        for (int j = 0; j < friend_cnt; ++j) {
            for (int k = 0; k < friend_cnt; ++k) {
                are_friends[j][k] = false;
            }
        }

        for (int j = 0; j < pairs; ++j) {
            int p1, p2;
            std::cin >> p1 >> p2;
            are_friends[p1][p2] = are_friends[p2][p1] = true;
        }

        std::cout << pair_count(friend_cnt, are_friends) << '\n';
    }
    return 0;
}
