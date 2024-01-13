#include <memory>
#include <unordered_set>
#include <vector>
#include <string>

struct parse_vertex;

using parse_vertex_sp = std::shared_ptr<parse_vertex>;

struct parse_vertex {
    std::vector<std::vector<parse_vertex_sp>> paths;
    std::string name;
    int index = 0;

    static parse_vertex_sp get_vertex(std::vector<parse_vertex_sp>& path,
                                      std::string& name, int index) {
        return std::make_shared<parse_vertex>(path, name, index);
    };
    static parse_vertex_sp get_vertex(std::string& name, int index) {
        auto vertex = std::make_shared<parse_vertex>();
        vertex->name = name;
        vertex->index = index;
        return vertex;
    };
    parse_vertex(std::vector<parse_vertex_sp>& path, std::string& _name, int _index)
        : name(_name), index(_index) {
        paths.push_back(path);
    }
    parse_vertex() = default;

   private:
};
