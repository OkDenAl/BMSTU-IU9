/*
Маленькое, но очень важное дерево
Бинарное синтаксическое дерево разбора регулярного выражения
*/
#pragma once
#include <cassert>
#include <memory>

#include "parser/tset.hpp"

using std::string;

struct s_node;
using s_node_ptr = std::unique_ptr<s_node>;

enum class node_type {
    ALTER = '|',
    CONCAT = '.',
    ITER = '*',
    SYMBOL,
};

struct s_node {
    node_type type;
    char s;
    s_node_ptr left = nullptr;
    s_node_ptr right = nullptr;

    // для grapviz-а, чтобы вершины были уникальны
    static int graph_vertex_count;
    // счетчик для линеаризации
    static int linear_count;

    // Выдает символьный узел
    static s_node_ptr symbol_node(char s_) {
        s_node_ptr res = std::make_unique<s_node>();
        res->type = node_type::SYMBOL;
        res->s = s_;
        return res;
    }

    // Выдает заказанный узел
    static s_node_ptr _node(node_type type_, s_node_ptr left_,
                            s_node_ptr right_) {
        s_node_ptr res = std::make_unique<s_node>();
        res->type = type_;
        res->s = (char)type_;
        res->left = std::move(left_);
        res->right = std::move(right_);
        return res;
    }

    // Сворачивает дерево в автомат Глушкова,
    // рекурсивно выстраивая First, Follow и Last
    StateMachine to_machine();
    TSet dfs();

    // Преобразование в dot-описание
    void to_graph(std::ostream& out);
    void to_graph_dfs(std::ostream& out, const string& parent_name);
};
