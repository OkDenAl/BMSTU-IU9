/*
Большое дерево представляет собой набор узлов. Альтернатива и конкатенация содержат
свои аргументы в массиве. REGEX - обычное регулярное выражение, содержит
указатель на своё бинарное дерево разбора, LOOKAHEAD - тоже обычное регулярное выражение,
которое было обернуто в lookahead в исходной строке.
*/
#pragma once
#include <cassert>
#include <memory>
#include <vector>

#include "StateMachine.h"
#include "parser/small_tree.hpp"

using std::vector;

struct Node;
using node_ptr = std::unique_ptr<Node>;

const int FROM_START = 0;

enum class NodeType {
    ALTER,
    CONCAT,
    ITER,
    LOOKAHEAD,
    LOOKBEHIND,
    REGEX,
};

struct Node {
    NodeType type;
    vector<node_ptr> args;
    s_node_ptr syntax_tree = nullptr;  // nullptr, если узел - не регулярка или регулярка, 
                                       // распознающая только пустое слово

    string value = "";  // внешний вид регулярки, для отладки (и красивого вывода в виде графа)

    static int graph_vertex_count; // счетчик вершин при отриовке graphviz-ом, чтобы вершины с одинаковыми 
                                   // названиями не сливались

    Node(NodeType type_) : type(type_) {}

    // Выдают, что просят
    static node_ptr empty_regex();
    static node_ptr symbol_regex(char s);
    static node_ptr _node(NodeType type_);

    // Операции над регулярками
    void iter_regex();
    void plus_regex(node_ptr arg);
    void concat_regex(node_ptr arg);
    void lookahead_regex();
    void lookbehind_regex();
    // оборачивает отладочное значение value в скобочки
    void paren_regex(); 

    // Операции над узлами-контейнерами: альтернативой и конкатенацией
    // вставляет узел как следующий аргумент операции
    void add_node(node_ptr arg);
    // "распаковывает" контейнер arg того же типа в исходный,
    // последовательно вставляя все элементы в текущий
    void merge_node(node_ptr arg);

    /* Сворачивает дерево в автомат, применяя все операции:
       - REGEX и LOOKAHEAD преобразуются в автоматы Глушкова
       - Альтернатива последовательно объединяет все автоматы из своих аргументов
       - Конкатенация последовательно конкатенирует автоматы из своих аргументов, накапливая результат, 
       пока не встретит lookahead, тогда рекурсивно получает автомат из конкатенации элементов после него
       (если после элементов нет - автомат, распознающий пустое слово) и возвращает конкатенацию 
       уже накопленного и пересечения lookahead-а c автоматом после.
    */
    StateMachine to_machine_dfs(int start = FROM_START);

    /* Преобразование в dot-описание */
    void to_graph(std::ostream& out);
    void to_graph_dfs(std::ostream& out, const string& parent_name);
};
