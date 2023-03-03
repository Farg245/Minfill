#include <stdio.h>      
#include <stdlib.h>      
#include <iostream>      
#include <math.h>      
#include <limits.h>      
#include <iostream>      
#include <list>      
using namespace std;      
#define BSIZE 1<<15      
       
char buffer[BSIZE];      
long bpos = 0L, bsize = 0L;      
long readLong()       
{      
    long d = 0L, x = 0L;      
    char c;      
       
    while (1)  {      
        if (bpos >= bsize) {      
            bpos = 0;      
            if (feof(stdin)) return x;      
            bsize = fread(buffer, 1, BSIZE, stdin);      
        }      
        c = buffer[bpos++];      
        if (c >= '0' && c <= '9') { x = x*10 + (c-'0'); d = 1; }      
        else if (d == 1) return x;      
    }      
    return -1;      
}      
    
// Kruskal algorithm's code source: https://www.geeksforgeeks.org/greedy-algorithms-set-2-kruskals-minimum-spanning-tree-mst/    
struct Edge    
{    
    long long int src, dest, weight;    
};    
     
// a structure to represent a connected, undirected    
// and weighted graph    
struct Graph    
{    
    // V-> Number of vertices, E-> Number of edges    
    long int V, E;    
     
    // graph is represented as an array of edges.     
    // Since the graph is undirected, the edge    
    // from src to dest is also edge from dest    
    // to src. Both are counted as 1 edge here.    
    struct Edge* edge;    
};    
     
// Creates a graph with V vertices and E edges    
struct Graph* createGraph(long int V, long int E)    
{    
    struct Graph* graph = new Graph;    
    graph->V = V;    
    graph->E = E;    
     
    graph->edge = new Edge[E];    
     
    return graph;    
}    
     
// A structure to represent a subset for union-find    
struct subset    
{    
    long int parent;    
    long int rank;    
};    
     
// A utility function to find set of an element i    
// (uses path compression technique)    
long int find(struct subset subsets[], long int i)    
{    
    // find root and make root as parent of i     
    // (path compression)    
    if (subsets[i].parent != i)    
        subsets[i].parent = find(subsets, subsets[i].parent);    
     
    return subsets[i].parent;    
}    
     
// A function that does union of two sets of x and y    
// (uses union by rank)    
void Union(struct subset subsets[], long int x, long int y)    
{    
    long int xroot = find(subsets, x);    
    long int yroot = find(subsets, y);    
     
    // Attach smaller rank tree under root of high     
    // rank tree (Union by Rank)    
    if (subsets[xroot].rank < subsets[yroot].rank)    
        subsets[xroot].parent = yroot;    
    else if (subsets[xroot].rank > subsets[yroot].rank)    
        subsets[yroot].parent = xroot;    
     
    // If ranks are same, then make one as root and     
    // increment its rank by one    
    else    
    {    
        subsets[yroot].parent = xroot;    
        subsets[xroot].rank++;    
    }    
}    
     
// Compare two edges according to their weights.    
// Used in qsort() for sorting an array of edges    
int myComp(const void* a, const void* b)    
{    
    struct Edge* a1 = (struct Edge*)a;    
    struct Edge* b1 = (struct Edge*)b;    
    return a1->weight > b1->weight;    
}    
     
// The main function to construct MST using Kruskal's algorithm    
void KruskalMST(struct Graph* graph,struct Edge result[])    
{    
    long int V = graph->V;    
    long int e = 0;  // An index variable, used for result[]    
    long int i = 0;  // An index variable, used for sorted edges    
     
    // Step 1:  Sort all the edges in non-decreasing     
    // order of their weight. If we are not allowed to     
    // change the given graph, we can create a copy of    
    // array of edges    
    qsort(graph->edge, graph->E, sizeof(graph->edge[0]), myComp);    
    // Allocate memory for creating V ssubsets    
    struct subset *subsets =    
        (struct subset*) malloc( V * sizeof(struct subset) );    
     
    // Create V subsets with single elements    
    for (int v = 0; v < V; ++v)    
    {    
        subsets[v].parent = v;    
        subsets[v].rank = 0;    
    }    
    // Number of edges to be taken is equal to V-1    
    while (e < V - 1)    
    {    
        // Step 2: Pick the smallest edge. And increment     
        // the index for next iteration    
        struct Edge next_edge = graph->edge[i++];    
     
        long int x = find(subsets, next_edge.src-1);    
        long int y = find(subsets, next_edge.dest-1);    
     
        // If including this edge does't cause cycle,    
        // include it in result and increment the index     
        // of result for next edge    
        if (x != y)    
        {    
            result[e++] = next_edge;    
            Union(subsets, x, y);    
        }    
        // Else discard the next_edge    
    }    
     
    return;    
}       
void gemisma(long int source,long int current,list<int> geitones[],list<long int> apostaseis[],long int max,int father,long int max_dist[])    
{    
    list<int>::iterator itr;
    list <long int>::iterator itr2;
     itr2=apostaseis[current].begin();
    for(itr=geitones[current].begin(); itr!=geitones[current].end(); ++itr)  
    {    
        if ((*itr)!=father)    
        {    
            if (max>*itr2)    
            {    
                max_dist[*itr]=max;    
                gemisma(source,*itr,geitones,apostaseis,max,current,max_dist);    
            }    
            else     
            {    
                max_dist[*itr]=*itr2;    
                gemisma(source,*itr,geitones,apostaseis,*itr2,current,max_dist);    
            }    
        } 
        itr2++;
    }    
    return;    
}    
int main(void)      
{      
    long int N,M,Q,i,j,temp;      
    N=(long)readLong();      
    M=(long)readLong();     
  long int **max_dist;   
    max_dist = (long**) malloc(N*sizeof(long*));  
for (int i = 0; i < N; i++)  
  max_dist[i] = (long*) malloc(N*sizeof(long));    struct Graph* my_graph=createGraph(N,M);    
    struct Edge mst[N];    
    for (i=0; i<M; i++)      
    {      
        temp=readLong();    
        my_graph->edge[i].src=temp;    
        temp=readLong();    
        my_graph->edge[i].dest=temp;    
        temp=readLong();    
        my_graph->edge[i].weight=temp;    
    }     
    KruskalMST(my_graph,mst);    
    list <int> neighbours[N];    
    list <long int> neig_dist[N];    
    struct Edge temp_edge;    
    for (i=0; i<N-1; i++)    
    {    
        temp_edge=mst[i];    
        neighbours[temp_edge.src-1].push_front(temp_edge.dest-1);    
        neighbours[temp_edge.dest-1].push_front(temp_edge.src-1);    
        neig_dist[temp_edge.src-1].push_front(temp_edge.weight);    
        neig_dist[temp_edge.dest-1].push_front(temp_edge.weight);    
    }    
     for (i=0; i<N; i++)    
    {    
        gemisma(i,i,neighbours,neig_dist,0L,i,max_dist[i]);    
    }    
    
    Q=(long)readLong();     
    for (i=0; i<Q; i++)      
    {      
        long int q1,q2;    
        q1=readLong();      
        q2=readLong();     
        printf("%ld\n",max_dist[q1-1][q2-1]);    
    }      
    return 0;      
}    