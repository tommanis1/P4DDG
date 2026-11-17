ParamterizedNonTerminal_to_VertexId = 
    ()
vertices = 
    [

        Vertex "1" 1
        , Vertex
        , Final "ipv4" 5 
        , Final "tunnel"

    ]

Edges  = [
    Edge 1 2 (Terminal "hdr.ethernet")

    , Edge 2 3 (Constrained "t==IPV4")
    , Edge 3 5 (Terminal "ipv4 : ipv4_t")

    , Edge 2 4 (Constrained "t==TUNNEL")
    -- , Edge 2 6 Empty
    , Edge 4 6 (Terminal "tunnel" : "tunnel_t")
    , Edge 6 8 (Call "tunnel" ["tunnel.tunnelType"])
    , Edge 8 9 (Call "tunnel")
]