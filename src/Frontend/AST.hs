module Frontend.AST where

import qualified DDG.Grammar(Label(..), Param(..)) as G

Grammar = [AugmentedNonterminal]

data Stmt = 
    If E [Stmt] [Stmt]
    | Case E [(E, [Stmt])]
    | Stmts [Stmt]
    | KleineClosure Stmt
    | Alternation Stmt Stmt
    | Seq Stmt Stmt
    | RuleLabel G.Label
    deriving (Show, Eq)

type E = E String | Not String

data Nonterminal = Nonterminal NonTerminalId [G.Param] Stmt deriving (Show)


{- 
EthType = 
    hdr.eth_type
    \case hdr.eth_type.value of
        ETHERTYPE_MPLS -> MPLS()
        ETHERTYPE_IPV4 -> IPV4()
        _ -> ()

MPLS = 
    {fabric_metadata.mpls_label = hdr.mpls.label;
        fabric_metadata.mpls_ttl = hdr.mpls.ttl;}
    {bit<4> = packet.lookahead<bit<4>>();}
    if t == 4 then IPV4() else Ethernet()
        
Ethernet = 
    hdr.ethernet 
    {fabric_metadata.vlan_id = DEFAULT_VLAN_ID;}
    (
        {bit<16> t = packet.lookahead<bit<16>>();}
        (([t == ETHERTYPE_QINQ] | [t == ETHERTYPE_QINQ_NON_STD] | [t == ETHERTYPE_VLAN]) VlanTag())
    )



VlanTag = 
    {bit<16> possible_tag = packet.lookahead<bit<16>>();}
    ([possible_tag == ETHERTYPE_QINQ] | [possible_tag == ETHERTYPE_QINQ_NON_STD] | [possible_tag == ETHERTYPE_VLAN] )
    hdr.vlan_tag hdr.inner_vlan_tag hdr.eth_type

[hdr.eth_type.value == ETHERTYPE_MPLS] hdr.mpls {
                                                fabric_metadata.mpls_label = hdr.mpls.label;
                                                fabric_metadata.mpls_ttl = hdr.mpls.ttl;
                                                } 
                                                {bit<4> t = packet.lookahead<bit<4>>();}
                                                ([t == 4] Ipv4() | [else] Ethernet())
[hdr.eth_type.value == ETHERTYPE_IPV4] 
[else : accept]



Ipv4 (bit<1> inner)= 
    ([inner == 1] hdr.ipv4 )
    ([inner == 0] hdr.inner_ipv4 {hdr.inner_ipv4.dscp} )

    
    {
        fabric_metadata.ip_proto = hdr.ipv4.protocol;
        fabric_metadata.ip_eth_type = ETHERTYPE_IPV4;
        fabric_metadata.ipv4_src_addr = hdr.ipv4.src_addr;
        fabric_metadata.ipv4_dst_addr = hdr.ipv4.dst_addr;
        last_ipv4_dscp = hdr.ipv4.dscp;
    }
 

Int = (
        [last_ipv4_dscp == INT_DSCP &&& INT_DSCP] hdr.intl4_shim hdr.int_header
        (([hdr.intl4_shim.len_words == INT_HEADER_LEN_WORDS] hdr.intl4_tail) | )

    ) |  
// Int = hdr.int_header ([hdr.intl4_shim.len_words != INT_HEADER_LEN_WORDS] hdr.intl4_tail)

InnerIpv4 = 
    hdr.inner_ipv4 {hdr.inner_ipv4.dscp}
    (

        ([hdr.inner_ipv4.protocol == PROTO_TCP] hdr.inner_tcp 
            {fabric_metadata.inner_l4_sport = hdr.inner_tcp.sport;
            fabric_metadata.inner_l4_dport = hdr.inner_tcp.dport;})
        | ([hdr.inner_ipv4.protocol == PROTO_UDP] hdr.inner_udp 
            {fabric_metadata.inner_l4_sport = hdr.inner_udp.sport;
            fabric_metadata.inner_l4_dport = hdr.inner_udp.dport;} 
            Int()
            )
        | ([hdr.inner_ipv4.protocol == PROTO_ICMP] hdr.inner_icmp)
        | 
                                                    
    )

Tcp = 
    [hdr.ipv4.protocol == PROTO_TCP]
    hdr.tcp 
    {fabric_metadata.l4_sport = hdr.tcp.sport;
    fabric_metadata.l4_dport = hdr.tcp.dport;}
    Int()

Ipv4 = 
    hdr.ipv4
    {fabric_metadata.ip_proto = hdr.ipv4.protocol;
    fabric_metadata.ip_eth_type = ETHERTYPE_IPV4;
    fabric_metadata.ipv4_src_addr = hdr.ipv4.src_addr;
    fabric_metadata.ipv4_dst_addr = hdr.ipv4.dst_addr;
    last_ipv4_dscp = hdr.ipv4.dscp;}
    (Tcp() | Udp() | [hdr.ipv4.protocol == PROTO_ICMP] hdr.icmp | )

Udp = 
    [hdr.ipv4.protocol == PROTO_UDP]
    hdr.udp
    {    fabric_metadata.l4_sport = hdr.udp.sport;
        fabric_metadata.l4_dport = hdr.udp.dport;
        gtpu_t gtpu = packet.lookahead<gtpu_t>();}
    ([hdr.udp.dport, gtpu.version, gtpu.msgtype == (2152, 0x01, 0xff)] Gtpu() | Int())

GTPU = 
        hdr.gtpu
        if hdr.gtpu.ex_flag, hdr.gtpu.seq_flag, hdr.gtpu.npdu_flag == (0, 0, 0) then 
            InnerIpv4()
        else (
            hdr.gtpu_options
            {bit<8> gtpu_ext_len = packet.lookahead<bit<8>>();}
            if hdr.gtpu_options.next_ext, gtpu_ext_len == (0x85, 8w1) then 
                (hdr.gtpu_ext_psc
                {fabric_metadata.spgw.qfi = hdr.gtpu_ext_psc.qfi;}
                if hdr.gtpu_ext_psc.next_ext == 0x0 then InnerIpv4 () else ())
            else ()
        ) -}