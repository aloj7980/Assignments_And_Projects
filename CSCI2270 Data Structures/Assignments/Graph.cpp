#include <iostream>
#include <vector>
using namespace std;
/*
 * adds a vertex to the graph
 */
void Graph::addVertex(int n){
    bool found = false;
    for(int i = 0; i < vertices.size(); i++){
        if(vertices[i]->key == n){
            found = true;
        }
    }
    if(found == false){
        vertex * v = new vertex;
        v->key = n;
        vertices.push_back(v);
    }
}

/*
 * adds an edge between two vertices (directed graph)
 */
void Graph::addEdge(int src, int dest){
    for(int i = 0; i < vertices.size(); i++) {
        if(vertices[i]->key == src) {
            for(int j = 0; j < vertices.size(); j++) {
                if(vertices[j]->key == dest && i != j) {
                    adjVertex av;
                    av.v = vertices[j];
                    vertices[i]->adj.push_back(av);
                }
            }
        }
    }
}

/*
 * Complete the following function which checks if vert is a boss vertex
 */

bool helper(vertex* v,int key){
    //sets visited to true for current vertex
    if(!v->visited){
        v->visited=true;
    }
    //returns true if current vertex is the one being searched for
    if(v->key==key){
        return true;
    }
    //returns true if any vertex that's reachable from the current vertex is the one being searched for
    for(int i=0;i<v->adj.size();i++){
        if(!v->adj[i].v->visited && helper(v->adj[i].v,key)){
            return true;
        }
    }
    //returns false if it was unable to reach the vertex being searched for
    return false;
}
bool Graph::isVertexABoss(vertex *vert) {
    //checks if each vertex is reachable from vert
    for(int i=0;i<vertices.size();i++){
        if(!helper(vert,vertices[i]->key)){
            //returns false if any vertex isn't reachable
            return false;
        }
        //resets visited to false for all vertices so that they are all unvisited when searching for the next vertex
        for(int j=0;j<vertices.size();j++){
            vertices[j]->visited=false;
        }
    }
    //returns true if all vertices were found
    return true;
}

/*
 * Complete the following function which checks if the graph is a Boss
 */
bool Graph::isGraphABoss() {
    //checks each vertex to see if it's a boss
    for(int i=0;i<vertices.size();i++){
        if(isVertexABoss(vertices[i])){
            //returns true if any of the vertices are bosses
            return true;
        }
    }
    //returns false if none of the vertices is a boss
    return false;
}