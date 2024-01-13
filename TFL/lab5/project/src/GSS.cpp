#include "GSS.h"

gss_node_sp gss_node::get_node(unordered_set<gss_node_sp>& childs,
                               int state, parse_vertex_sp pp) {
    return std::make_shared<gss_node>(childs, state, pp);
}

gss_node_sp gss_node::get_node(int state, parse_vertex_sp pp) {
    auto sp = std::make_shared<gss_node>();
    sp->state = state;
    sp->parse_vertex = pp;
    return sp;
}
