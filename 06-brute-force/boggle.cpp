#include <iostream>
#include <string>

constexpr char board[5][5] = {
    {'U', 'R', 'L', 'P', 'M'},
    {'X', 'P', 'R', 'E', 'T'},
    {'G', 'I', 'A', 'E', 'T'},
    {'X', 'T', 'N', 'Z', 'Y'},
    {'X', 'O', 'Q', 'R', 'S'}
};
constexpr int dx[8] = {-1,  0, +1, +1, +1,  0, -1, -1};
constexpr int dy[8] = {-1, -1, -1,  0, +1, +1, +1,  0};

bool has_word(int x, int y, const std::string& word)
{
    if (!(x >= 0 && x < 5 && y >= 0 && y < 5)) { return false; }
    if (word.length() == 0) { return true; }
    if (board[y][x] != word[0]) { return false; }

    auto substr = word.substr(1);
    for (int i = 0; i < 8; ++i) {
        int new_x = x + dx[i], new_y = y + dy[i];
        if (has_word(new_x, new_y, substr)) { return true; }
    }
    return false;
}

int main(int argc, char *argv[])
{
    std::string word = "PRETTY";
    int x = 1, y = 1;
    std::cout << (has_word(x, y, word) ? "true" : "false") << '\n';

    return 0;
}
