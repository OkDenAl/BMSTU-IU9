#include "parser/small_tree.hpp"

int s_node::graph_vertex_count = 0;
int s_node::linear_count = 0;

StateMachine s_node::to_machine() {
    s_node::linear_count = 1;
    return dfs().to_machine();
}

TSet s_node::dfs() {
    switch (type) {
        case node_type::ALTER: {
            assert(left);
            assert(right);
            TSet A = left->dfs();
            TSet B = right->dfs();
            A.plus(B);
            return A;
        }
        case node_type::CONCAT: {
            assert(left);
            assert(right);
            TSet A = left->dfs();
            TSet B = right->dfs();
            A.concat(B);
            return A;
        }
        case node_type::ITER: {
            assert(left);
            assert(!right);
            TSet A = left->dfs();
            A.iter();
            return A;
        }
        case node_type::SYMBOL: {
            if (s == 0) return TSet();
            return TSet(cchar{s, s_node::linear_count++});
        }
        default:
            return TSet();
    }
}

void s_node::to_graph(std::ostream& out) {
    s_node::graph_vertex_count = 0;
    out << "digraph { graph [ dpi = 300 ]; " << std::endl;
    to_graph_dfs(out, "\"\"");
    out << "}" << std::endl;
}

void s_node::to_graph_dfs(std::ostream& out, const string& parent_name) {
    switch (type) {
        case node_type::ALTER: {
            string own_name =
                    "\"" + std::to_string(s_node::graph_vertex_count++) + ": |\"";
            out << own_name << "[shape=square]" << std::endl;
            out << parent_name << "->" << own_name << std::endl;
            left->to_graph_dfs(out, own_name);
            right->to_graph_dfs(out, own_name);
            break;
        }
        case node_type::CONCAT: {
            string own_name =
                    "\"" + std::to_string(s_node::graph_vertex_count++) + ": .\"";
            out << own_name << "[shape=square]" << std::endl;
            out << parent_name << "->" << own_name << std::endl;
            left->to_graph_dfs(out, own_name);
            right->to_graph_dfs(out, own_name);
            break;
        }
        case node_type::ITER: {
            string own_name =
                    "\"" + std::to_string(s_node::graph_vertex_count++) + ": *\"";
            out << own_name << "[shape=square]" << std::endl;
            out << parent_name << "->" << own_name << std::endl;
            left->to_graph_dfs(out, own_name);
            break;
        }
        case node_type::SYMBOL: {
            string own_name = "\"" +
                              std::to_string(s_node::graph_vertex_count++) +
                              ": " + string(1, s) + "\"";
            out << parent_name << "->" << own_name << std::endl;
            break;
        }
    };
}
