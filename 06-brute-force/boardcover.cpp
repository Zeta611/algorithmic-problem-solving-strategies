#include <iostream>
#include <vector>

/*
Input
3
3 7
#.....#
#.....#
##...##
3 7
#.....#
#.....#
##..###
8 10
##########
#........#
#........#
#........#
#........#
#........#
#........#
##########

Output
0
2
1514
 */

typedef std::vector<std::vector<int>> vec2dint;
struct point { int x; int y; };

constexpr struct point cover_type[4][3] = {
    {{0, 0}, {1, 0}, {0, 1}},  // ⌜
    {{0, 0}, {1, 0}, {1, 1}},  // ⌝
    {{0, 0}, {0, 1}, {1, 1}},  // ⌞
    {{0, 0}, {0, 1}, {-1, 1}}  // ⌟
};

bool first_empty(struct point& pt0, const vec2dint& board)
{
    const int height = board.size();
    const int width = board[0].size();

    for (int y = 0; y < height; ++y) {
        for (int x = 0; x < width; ++x) {
            if (board[y][x] == 0) {
                pt0.x = x;
                pt0.y = y;
                return false;
            }
        }
    }
    return true;
}

bool cover(const struct point& pt, int type, bool uncover, vec2dint& board)
{
    const int height = board.size();
    const int width = board[0].size();
    bool overlap = false;

    for (int i = 0; i < 3; ++i) {
        const int new_x = pt.x + cover_type[type][i].x;
        const int new_y = pt.y + cover_type[type][i].y;

        if (!(new_x >= 0 && new_x < width && new_y >= 0 && new_y < height)) {
            overlap = true;
        } else if ((board[new_y][new_x] += uncover ? -1 : 1) > 1) {
            overlap = true;
        }
    }
    return overlap;
}

int count_covers(vec2dint& board)
{
    const int height = board.size();
    const int width = board[0].size();

    struct point pt0;
    if (first_empty(pt0, board)) { return 1; }

    int cover_cnt = 0;
    for (int type = 0; type < 4; ++type) {
        if (!cover(pt0, type, false, board)) {
            cover_cnt += count_covers(board);
        }
        cover(pt0, type, true, board);
    }
    return cover_cnt;
}

int main(int argc, char *argv[])
{
    int test_cases;  // test_cases <= 30
    std::cin >> test_cases;

    for (int i = 0; i < test_cases; ++i) {
        int height, width;  // 1 <= height, width <= 20
        std::cin >> height >> width;
        std::cin.ignore();
        vec2dint board(height, std::vector<int>(width));

        for (int y = 0; y < height; ++y) {
            std::string line;
            std::getline(std::cin, line);
            for (int x = 0; x < width; ++x) {
                board[y][x] = line[x] == '#';
            }
        }
        std::cout << count_covers(board) << '\n';
    }
    return 0;
}
