import P4DDG.AST
-- Based on the tunnel example, the second tutorial from the P4 tutorials 

{- 
P4 parser:

parser MyParser(packet_in packet,
                out headers hdr,
                inout metadata meta,
                inout standard_metadata_t standard_metadata) {

    state start {
        transition parse_ethernet;
    }

    state parse_ethernet {
        packet.extract(hdr.ethernet);
        transition select(hdr.ethernet.etherType) {
            TYPE_MYTUNNEL: parse_myTunnel;
            TYPE_IPV4: parse_ipv4;
            default: accept;
        }
    }

    state parse_myTunnel {
        packet.extract(hdr.myTunnel);
        transition select(hdr.myTunnel.proto_id) {
            TYPE_IPV4: parse_ipv4;
            default: accept;
        }
    }

    state parse_ipv4 {
        packet.extract(hdr.ipv4);
        transition accept;
    }

}

 -}

{- 
P4-DDG:

hdr = hdr.ethernet tunnel(ethernet.etherType)
tunnel(type) = 
    [type == TYPE_IPV4 ] ipv4_t:ipv4
    | [type == TYPE_MYTUNNEL] 
            myTunnel_t:myTunnel tunnel(myTunnel.proto_id)
    | // empty

 -}

parser = [ 
    NonTerminal "hdr" [
        -- Type $ "ethernet_t"
        Field "hdr.ethernet"
        , ExpressionStmt . Call $ "tunnel" ["hdr.ethernet"]
    ]
    ParamterizedNonTerminal 
    ]


