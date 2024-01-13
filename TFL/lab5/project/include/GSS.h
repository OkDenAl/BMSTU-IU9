#include <memory>
#include <unordered_map>
#include <unordered_set>

#include "Forest.h"

struct gss_node;

using gss_node_sp = std::shared_ptr<gss_node>;
using std::vector;
using std::unordered_set;
struct Path {
    vector<parse_vertex_sp> vertices;
    unordered_set<gss_node_sp> base_nodes;
};

struct gss_node {
    unordered_set<gss_node_sp> childs;
    int state;
    parse_vertex_sp parse_vertex;

    gss_node(unordered_set<gss_node_sp>& childs, int state,
             parse_vertex_sp pp)
        : childs(childs), state(state), parse_vertex(pp) {}
    gss_node() = default;
    static gss_node_sp get_node(unordered_set<gss_node_sp>& childs,
                                int state, parse_vertex_sp pp = nullptr);
    static gss_node_sp get_node(int state, parse_vertex_sp pp = nullptr);
    static vector<Path> look(gss_node_sp& target, int n) {
        vector<Path> result;
        look_dfs(target, n, result, {});
        return result;
    }

   private:
    static void look_dfs(const gss_node_sp& target, int n, vector<Path>& result,
                         vector<parse_vertex_sp> path) {
        path.push_back(target->parse_vertex);
        if (n == 1) {
            result.push_back(Path{path, target->childs});
            return;
        }
        for (const gss_node_sp& child : target->childs) {
            look_dfs(child, n - 1, result, path);
        }
    }
};
