#include <memory>
#include <string>
#include <vector>

#include "parser/parser2.hpp"

node_ptr Parser::Parse() {
    if (next() != _START) return nullptr;
    skip();
    node_ptr R0 = ParseR0();
    if (next() != _END) return nullptr;
    skip();
    return R0;
}

node_ptr Parser::ParseR0() {
    node_ptr R = ParseR();
    if (!R) return Node::empty_regex();
    return R;
}

node_ptr Parser::ParseR() {
    node_ptr node_accum = Node::_node(NodeType::ALTER);
    node_ptr regex_accum = nullptr;

    int pos = next_index;
    node_ptr T = ParseT();
    while (T) {
        if (T->type == NodeType::REGEX) {
            if (!regex_accum) {
                regex_accum = std::move(T);
            } else {
                regex_accum->plus_regex(std::move(T));
            }
        } else if (T->type == NodeType::ALTER) {
            node_accum->merge_node(std::move(T));
        } else {
            node_accum->add_node(std::move(T));
        }
        if (next() != _PLUS) {
            if (!regex_accum) {
                if (node_accum->args.size() == 1) {
                    return std::move(node_accum->args[0]);
                }
                return node_accum;
            }
            if (node_accum->args.size() == 0) return regex_accum;
            node_accum->add_node(std::move(regex_accum));
            return node_accum;
        }
        pos = next_index;
        skip();
        T = ParseT();
    }

    next_index = pos;  // был +, а дальше пусто
    return nullptr;    // возвращаемcя к плюсу
}

node_ptr Parser::ParseT() {
    node_ptr F = ParseF();
    if (!F) return nullptr;

    node_ptr node_accum = Node::_node(NodeType::CONCAT);
    node_ptr regex_accum = nullptr;

    while (F) {
        if (F->type == NodeType::REGEX) {
            if (!regex_accum) {
                regex_accum = std::move(F);
            } else {
                regex_accum->concat_regex(std::move(F));
            }
        } else if (F->type == NodeType::CONCAT) {
            if (regex_accum) {
                node_accum->add_node(std::move(regex_accum));
            }
            node_accum->merge_node(std::move(F));
            regex_accum = nullptr;
        } else {
            if (regex_accum) {
                node_accum->add_node(std::move(regex_accum));
            }
            node_accum->add_node(std::move(F));
            regex_accum = nullptr;
        }
        F = ParseF();
    }
    if (!regex_accum) {
        if (node_accum->args.size() == 1 &&
            node_accum->args[0]->type != NodeType::LOOKAHEAD &&
            node_accum->args[0]->type != NodeType::LOOKBEHIND) {
            return std::move(node_accum->args[0]);
        }
        return node_accum;
    }
    if (node_accum->args.size() == 0) return regex_accum;
    node_accum->add_node(std::move(regex_accum));
    return node_accum;
}

node_ptr Parser::ParseF() {
    if (is_lookahead()) {
        skip(3);
        int pos = next_index;
        node_ptr L = ParseR0();
        if (L->type !=
            NodeType::REGEX) {  // не бывает lookahead внутри lookahead
            next_index = pos;
            return nullptr;
        }
        char c = next();
        if (c != _END && c != _RPAREN) {
            next_index = pos;
            return nullptr;  // незакрытый lookahead
        }
        if (c == _RPAREN) {
            L->lookahead_regex();  // добавляем точку со звездочкой
            skip();
        } else {
            skip();
            if (next() != _RPAREN) {
                next_index = pos;
                return nullptr;  // незакрытый lookahead
            }
            skip();
        }
        L->type = NodeType::LOOKAHEAD;
        return L;
    }
    if (is_lookbehind()) {
        skip(4);
        bool is_start = false;
        if (next() == _START) {
            is_start = true;
            skip();
        }
        int pos = next_index;
        node_ptr L = ParseR0();
        if (L->type !=
            NodeType::REGEX) {  // не бывает lookahead(behind) внутри lookbehind
            next_index = pos;
            return nullptr;
        }
        if (next() != _RPAREN) {
            next_index = pos;
            return nullptr;  // незакрытый lookbehind
        }
        skip();
        if (!is_start) {
            L->lookbehind_regex();  // добавляем точку со звездочкой
        }
        L->type = NodeType::LOOKBEHIND;
        return L;
    }

    node_ptr A = ParseA();
    if (!A) return nullptr;  // если не A, то вариантов больше нет
    if (next() == '*') {
        skip();
        if (A->type != NodeType::REGEX)
            return nullptr;  // нельзя итерировать что-то с lookahead
        A->iter_regex();  // навешиваем звездочку
    }
    return A;
}

node_ptr Parser::ParseA() {
    char a = next();
    if (check(a) || a == '.') {  // с точками разбирается пересекатель
        // автоматов, тут их не трогаем
        skip();
        return Node::symbol_regex(a);
    }
    if (a == '(') {
        int pos = next_index;
        skip();
        node_ptr R0 = ParseR0();  // не проверяем, так как R0 всегда парсится
        if (next() != ')') {
            next_index = pos;
            return nullptr;
        }
        skip();
        if (R0->type == NodeType::REGEX) R0->paren_regex();
        return R0;
    }
    return nullptr;
}