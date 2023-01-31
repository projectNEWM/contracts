import networkx as nx
import matplotlib.pyplot as plt

def create_graph_from_list(connections, node_colors, labels):
    G = nx.Graph()
    node_list = [node for node, _ in connections]
    G.add_nodes_from(node_list)
    for node, edges in connections:
        G.add_edges_from([(node, edge) for edge in edges])
    pos = nx.shell_layout(G)
    
    # nx.draw(G, pos, node_color=[node_colors[node_list.index(node)] for node in G.nodes], with_labels=True, labels=labels)
    # plt.savefig("graph-colored.png")
    nx.draw(G, pos, with_labels=True, labels=labels)
    plt.savefig("graph-labeled.png")
    # nx.draw(G, pos)
    # plt.savefig("graph.png")

color_dict = {
    0:'red',
    1:'blue',
    2:'yellow',
    3:'cyan',
    4:'purple',
    5:'green',
    6:'orange'
}

colors = [0,0,1,1,2,0,3,4]
node_colors = [color_dict[i] for i in colors]

node_labels = {}
for i in range(len(colors)-1,-1,-1):
    node_labels[i] = i

graph = [(7,[3,1,0]),(6,[5,4,1,0]),(5,[2,6,0]),(4,[1,3,6]),(3,[1,0,7,4]),(2,[5,0]),(1,[3,6,0,7,4]),(0,[6,7,5,3,2,1])]
create_graph_from_list(graph, node_colors, node_labels)
