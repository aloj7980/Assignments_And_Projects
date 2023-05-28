#ifndef GRAPH_H
#define GRAPH_H
#include<vector>

struct vertex;

struct adjVertex {
    vertex *v;
};

struct vertex {
    int key;
    bool visited = false;
    std::vector<adjVertex> adj;
};

class Graph {
    public:
        void addEdge(int v1, int v2);
        void addVertex(int v);
        bool isVertexABoss(vertex *v);
        bool isGraphABoss();
        
    private:
        std::vector<vertex*> vertices;
};

#endif
