#include <iostream>
#include <unordered_map>

#include "GSS.h"
#include "SLRTable.h"

const int LAST_TRACE = -3;
const int NO_TRACE = -3;
const int FULL_TRACE = -1;

class LRParser {
   private:
    SLRTable& table;
    int pos = 0;
    std::vector<std::pair<gss_node_sp, ExtendedRule>> reduce_stack;
    std::unordered_map<int, std::unordered_set<gss_node_sp>> shift_map;
    std::vector<gss_node_sp> just_created;
    std::unordered_set<gss_node_sp> accepted;
    std::unordered_set<parse_vertex_sp> stubs;
    int step = 0;
    int target_step = 0;
    std::string token;
    std::unordered_map<std::string, int> parse_vertex_count;
    std::vector<parse_vertex_sp> terminal_vertices;

    void next_step();
    void parse_tree_to_graph(std::ostream& out);
    void parse_tree_to_graph_dfs(parse_vertex_sp& t, std::ostream& out,
                                  unordered_set<parse_vertex_sp>& visited);

    void stack_to_graph(std::ostream& out);
    void stack_to_graph_dfs(gss_node_sp& t, std::ostream& out,
                      std::unordered_set<gss_node_sp>& visited);
    void init(int target_step);
    void update(gss_node_sp& target, const std::string& token);
    void make_screen();

   public:
    LRParser(SLRTable& _table);
    bool parse(std::vector<std::string>& in, int target_step);
};
