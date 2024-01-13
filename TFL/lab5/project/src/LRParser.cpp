#include "LRParser.h"

#include <fstream>
#include <sstream>

using std::string;
using std::unordered_map;
using std::unordered_set;
using std::vector;

string join(vector<string>& src, string delim = "") {
    string res;
    for (int i = 0; i < src.size() - 1; ++i) {
        res += src[i] + delim;
    }
    res += src[src.size() - 1];
    return res;
}

void LRParser::make_screen() {
    std::string stack_name = "stack/step_" + std::to_string(step) + ".dot";
    std::string tree_name = "tree/step_" + std::to_string(step) + ".dot";
    std::ofstream stack_out(stack_name);
    std::ofstream tree_out(tree_name);
    stack_to_graph(stack_out);
    parse_tree_to_graph(tree_out);
    stack_out.close();
    tree_out.close();
    // std::string stack_command =
    //     "dot -Tpng " + stack_name + " -o " + stack_name + ".png";
    // std::string tree_command =
    //     "dot -Tpng " + tree_name + " -o " + tree_name + ".png";
    // system(stack_command.c_str());
    // system(tree_command.c_str());
}

void LRParser::next_step() {
    if (target_step == FULL_TRACE ||
        (target_step >= 0 && step == target_step)) {
        make_screen();
    }
    step++;
}

string get_vertex_name(parse_vertex_sp pv, int i) {
    std::stringstream ss;
    ss << "\"" << pv << "_" << i << "\"";
    return ss.str();
}

void LRParser::parse_tree_to_graph(std::ostream& out) {
    out << "digraph {" << std::endl;
    out << "label=\"sentence: ";
    for (auto v : terminal_vertices) {
        out << v->name << " ";
    }
    out << "\"" << std::endl;
    out << "node [shape=circle];" << std::endl;
    out << "compound=true;" << std::endl;
    out << "rank1 [style = invis];" << std::endl;
    out << "{" << std::endl;
    out << "rank = same;" << std::endl;
    out << "node [shape=box];" << std::endl;
    out << "rank1 " << std::endl;
    for (auto v : terminal_vertices) {
        out << " -> " << get_vertex_name(v, 1) << std::endl;
    }
    out << " [style = invis];" << std::endl;
    out << "}" << std::endl;
    std::unordered_set<parse_vertex_sp> visited;
    for (auto pv : stubs) {
        parse_tree_to_graph_dfs(pv, out, visited);
    }
    out << "}" << std::endl;
}

void LRParser::parse_tree_to_graph_dfs(
    parse_vertex_sp& pv, std::ostream& out,
    unordered_set<parse_vertex_sp>& visited) {
    visited.insert(pv);
    if (pv->paths.size() > 1) {
        out << "subgraph \"cluster_" << pv << "\" {" << std::endl;
        out << "style=filled;" << std::endl;
        out << "color=lightgrey;" << std::endl;
        out << "label = <" << pv->name << "<SUB>" << pv->index << "</SUB>"
            << ">;" << std::endl;
        for (int i = 0; i < pv->paths.size(); ++i) {
            auto p = pv->paths[i];
            string p_name = get_vertex_name(pv, i);
            out << p_name << " [label=" << i << "]" << std::endl;
        }
        out << "}" << std::endl;
        for (int i = 0; i < pv->paths.size(); ++i) {
            auto p = pv->paths[i];
            string p_name = get_vertex_name(pv, i);
            for (int j = p.size() - 1; j >= 0; --j) {
                auto child = p[j];
                string c_name = get_vertex_name(child, 1);
                out << p_name << " -> " << c_name;
                if (child->paths.size() > 1) {
                    out << "[lhead=\"cluster_" << child << "\"]";
                }
                out << std::endl;
                if (visited.find(child) == visited.end()) {
                    parse_tree_to_graph_dfs(child, out, visited);
                }
            }
        }
        return;
    }
    string p_name = get_vertex_name(pv, 1);
    out << p_name << "[label = <" << pv->name << "<SUB>" << pv->index
        << "</SUB>"
        << ">];" << std::endl;
    for (auto p : pv->paths) {
        for (auto child : p) {
            string c_name = get_vertex_name(child, 1);
            out << p_name << " -> " << c_name;
            if (child->paths.size() > 1) {
                out << "[lhead=\"cluster_" << child << "\"]";
            }
            out << std::endl;
            if (visited.find(child) == visited.end()) {
                parse_tree_to_graph_dfs(child, out, visited);
            }
        }
    }
}

void LRParser::stack_to_graph(std::ostream& out) {
    out << "digraph {" << std::endl;
    out << "rankdir=RL" << std::endl;
    out << "label=\"next token: " << token << "\\npos: " << pos << "\""
        << std::endl;
    out << "node [shape=box]" << std::endl;
    std::unordered_map<gss_node_sp, std::string> tops;
    std::unordered_set<gss_node_sp> visited;
    for (int i = 0; i < reduce_stack.size(); ++i) {
        auto reduce = reduce_stack[i];
        gss_node_sp node = reduce.first;
        ExtendedRule& rule = reduce.second;
        tops[node] +=
            "reduce(" + rule.LHS + " -> " + join(rule.RHS, " ") + ")" + "\\n";
    }
    for (auto shift : shift_map) {
        int shift_state = shift.first;
        auto shift_nodes = shift.second;
        for (auto node : shift_nodes) {
            tops[node] += "shift(" + std::to_string(shift_state) + ")" + "\\n";
        }
    }
    for (auto node : accepted) {
        tops[node] = "acc";
    }
    for (auto top : tops) {
        gss_node_sp node = top.first;
        std::string& xlabel = top.second;
        out << "\"" << node << "\""
            << "["
            << "xlabel=\"" << xlabel << "\", "
            << "shape=ellipse"
            << "]" << std::endl;
        stack_to_graph_dfs(node, out, visited);
    }
    out << "}" << std::endl;
}

void LRParser::stack_to_graph_dfs(gss_node_sp& t, std::ostream& out,
                                  unordered_set<gss_node_sp>& visited) {
    visited.insert(t);
    out << "\"" << t << "\" [label=<" << t->state << " ("
        << t->parse_vertex->name << "<SUB>" << t->parse_vertex->index
        << "</SUB>"
        << ")>]" << std::endl;
    for (gss_node_sp child : t->childs) {
        out << "\"" << t << "\""
            << " -> "
            << "\"" << child << "\"" << std::endl;
        if (visited.find(child) == visited.end()) {
            stack_to_graph_dfs(child, out, visited);
        }
    }
}

void LRParser::init(int _target_step) {
    target_step = _target_step;
    if (target_step == LAST_TRACE || target_step == FULL_TRACE ||
        target_step >= 0) {
        system("mkdir tree");
        system("mkdir stack");
    }
    pos = 0;
    step = 0;
    parse_vertex_count = {};
    reduce_stack = {};
    shift_map = {};
    just_created = {};
    accepted = {};
    stubs = {};
    terminal_vertices = {};
}

void LRParser::update(gss_node_sp& target, const string& token) {
    auto actions = table.GetActions(target->state, token);
    if (actions.is_acc) {
        accepted.insert(target);
    }
    for (ExtendedRule& rule : actions.reduceActions) {
        reduce_stack.emplace_back(target, rule);
    }
    for (int shift_state : actions.shiftActions) {
        shift_map[shift_state].insert(target);
    }
    int shift_number = actions.shiftActions.size();
}

LRParser::LRParser(SLRTable& _table) : table(_table) {}
bool LRParser::parse(vector<string>& in, int target_step = NO_TRACE) {
    init(target_step);
    token = in[pos];
    std::string empty_string = "";
    auto empty_vertex = parse_vertex::get_vertex(empty_string, 0);
    gss_node_sp bottom = gss_node::get_node(0, empty_vertex);
    update(bottom, in[pos]);
    next_step();

    while (true) {
        // Reduce stage
        just_created = {};
        while (reduce_stack.size() != 0) {
            auto reduce = reduce_stack.back();
            reduce_stack.pop_back();
            ExtendedRule& rule = reduce.second;
            vector<Path> paths = gss_node::look(reduce.first, rule.RHS.size());
            for (int i = 0; i < paths.size(); ++i) {
                Path& p = paths[i];
                auto p_vertex = parse_vertex::get_vertex(
                    p.vertices, rule.LHS, parse_vertex_count[rule.LHS]);
                for (parse_vertex_sp pv : p.vertices) {
                    stubs.extract(pv);
                }
                auto base_nodes = p.base_nodes;
                unordered_map<int, unordered_set<gss_node_sp>> goto_partition;
                for (gss_node_sp node : base_nodes) {
                    goto_partition[table.GoTo(node->state, rule.LHS)].insert(
                        node);
                }
                bool is_parse_vertex_created = false;
                for (auto part : goto_partition) {
                    int s = part.first;
                    unordered_set<gss_node_sp>& s_part = part.second;
                    bool amb_flag = false;
                    for (int i = 0; i < just_created.size(); ++i) {
                        auto node = just_created[i];
                        if (s == node->state && s_part == node->childs) {
                            amb_flag = true;
                            /// tree packing
                            node->parse_vertex->paths.push_back(p.vertices);
                            ///
                            break;
                        }
                    }
                    if (amb_flag) {
                        continue;
                    }
                    is_parse_vertex_created = true;
                    auto node = gss_node::get_node(s_part, s, p_vertex);
                    stubs.insert(p_vertex);
                    just_created.push_back(node);
                    update(node, in[pos]);
                }
                if (is_parse_vertex_created) {
                    parse_vertex_count[rule.LHS]++;
                }
            }
            next_step();
        }
        //
        // Shift stage
        if (shift_map.size() != 0) {
            auto parse_vertex =
                parse_vertex::get_vertex(token, parse_vertex_count[token]++);
            terminal_vertices.push_back(parse_vertex);
            stubs.insert(parse_vertex);
            std::unordered_set<gss_node_sp> next_tops;
            for (auto shift : shift_map) {
                int shift_state = shift.first;
                auto shift_nodes = shift.second;
                auto accum =
                    gss_node::get_node(shift_nodes, shift_state, parse_vertex);
                next_tops.insert(accum);
            }
            shift_map = {};
            pos++;
            token = in[pos];
            for (auto top : next_tops) {
                update(top, token);
            }
            next_step();
        }
        //
        // Check stage
        if (accepted.size() != 0) {
            if (target_step == LAST_TRACE) {
                make_screen();
            }
            return true;
        }
        if (reduce_stack.size() == 0 && shift_map.size() == 0) {
            if (target_step == LAST_TRACE) {
                make_screen();
            }
            return false;
        }
        //
    }
}
