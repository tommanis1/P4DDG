# from apptopo import AppTopo
from mininet.topo import Topo
import sys
# class CustomAppTopo(AppTopo):
#     def __init__(self, *args, **kwargs):
#         AppTopo.__init__(self, *args, **kwargs)

#         manifest, target = kwargs['manifest'], kwargs['target']

#         print("Using target:", manifest['targets'][target])

#         h1, h2 = self.hosts()
#         h1.setIP('10.0.1.1/24')

        # print(self.links())

        # switch = self.addSwitch('s1')
        # hosts = [self.addHost('h' + str(i)) for i in range(1, 4)]

# from mininet.topo import Topo

# class CustomAppTopo(Topo):

#     def __init__(self, manifest=None, target=None, **opts):
#         Topo.__init__(self, **opts)

#         self.manifest = manifest
#         self.target = target
#         self.conf = manifest['targets'][target]

#         nodes = sorted(list(set(sum(map(list, zip(*self.conf['links'])), []))))
#         host_names, sw_names = [], []
#         for node in nodes:
#             if node in self.conf['hosts']: host_names.append(node)
#             elif node in self.conf['switches']: sw_names.append(node)
#             elif node[0] == 'h': host_names.append(node)
#             elif node[0] == 's': sw_names.append(node)
#             else:
#                 raise Exception("Unknown node type: " + str(node))

#         self._host_links = {}
#         self._sw_links = dict([(sw, {}) for sw in sw_names])
#         self._sw_hosts = dict([(sw, {}) for sw in sw_names])
#         self._port_map = dict([(sw, {}) for sw in sw_names])

#         for sw_name in sw_names:
#             self.addSwitch(sw_name)

#         for host_name in host_names:
#             host_num = host_names.index(host_name)+1

#             self.addHost(host_name)

#             self._host_links[host_name] = {}
#             host_links = filter(lambda l: l[0]==host_name or l[1]==host_name, self.conf['links'])

#             sw_idx = 0
#             for link in host_links:
#                 sw = link[0] if link[0] != host_name else link[1]
#                 assert sw in sw_names, "Hosts should be connected to switches, not " + str(sw)
#                 sw_num = sw_names.index(sw)+1

#                 host_mac = '00:04:00:00:%02x:%02x' % (host_num, sw_idx+1)

#                 delay_key = tuple(sorted([host_name, sw]))
#                 delay = self.conf['latencies'][delay_key] if delay_key in self.conf['latencies'] else '0ms'
#                 self._port_map[sw][host_name] = len(self._port_map[sw])+1
#                 self._host_links[host_name][sw] = dict(
#                         idx=sw_idx,
#                         host_mac = host_mac,
#                         host_ip = "10.0.%d.%d" % (host_num, 100+sw_idx+1),
#                         sw = sw,
#                         sw_mac = "00:aa:00:%02x:00:%02x" % (sw_num, host_num),
#                         sw_ip = "10.0.%d.%d" % (host_num, sw_idx+1),
#                         sw_port = self._port_map[sw][host_name]
#                         )
#                 self._sw_hosts[sw][host_name] = self._host_links[host_name][sw]
#                 self.addLink(host_name, sw, delay=delay,
#                         addr1=host_mac, addr2=self._host_links[host_name][sw]['sw_mac'])
#                 sw_idx += 1

#         for link in self.conf['links']: # only check switch-switch links
#             sw1, sw2 = link
#             if sw1 not in sw_names or sw2 not in sw_names: continue

#             delay_key = tuple(sorted(link))
#             delay = self.conf['latencies'][delay_key] if delay_key in self.conf['latencies'] else '0ms'
#             self.addLink(sw1, sw2, delay=delay)
#             self._port_map[sw1][sw2] = len(self._port_map[sw1])+1
#             self._port_map[sw2][sw1] = len(self._port_map[sw2])+1

#             sw1_num, sw2_num = sw_names.index(sw1)+1, sw_names.index(sw2)+1
#             sw1_port = dict(mac="00:aa:00:%02x:%02x:00" % (sw1_num, sw2_num), port=self._port_map[sw1][sw2])
#             sw2_port = dict(mac="00:aa:00:%02x:%02x:00" % (sw2_num, sw1_num), port=self._port_map[sw2][sw1])

#             self._sw_links[sw1][sw2] = [sw1_port, sw2_port]
#             self._sw_links[sw2][sw1] = [sw2_port, sw1_port]

def check(key, config, id):
    try:
        m = config[key]

        if isinstance(m, str):
            return m
        elif isinstance(m, dict):
            try:
                py_code = eval(m["python-lambda"])
                return py_code(id)
            except: return None
        else:
            return None
    except:
        return None
    

def get_attributes(config, id):
    attrs = {
        'mac' :  check("mac", config, id)
        , 'ip' : check("ip",config , id)
        }

    attrs = {k: v for k, v in attrs.items() if v is not None}
    return attrs


class CustomAppTopo(Topo):

    def __init__(self, manifest=None, target=None, **opts):
        Topo.__init__(self, **opts)

        self.manifest = manifest
        self.target = target
        self.conf = manifest['targets'][target]

        # sys.exit(1)
        # sys.exit(manifest)

        nodes = sorted(list(set(sum(map(list, zip(*self.conf['links'])), []))))
        host_names, sw_names = [], []
        for node in nodes:
            if node in self.conf['hosts']: host_names.append(node)
            elif node in self.conf['switches']: sw_names.append(node)
            elif node[0] == 'h': host_names.append(node)
            elif node[0] == 's': sw_names.append(node)
            else:
                raise Exception("Unknown node type: " + str(node))

        self._host_links = {}
        self._sw_links = dict([(sw, {}) for sw in sw_names])
        self._sw_hosts = dict([(sw, {}) for sw in sw_names])
        self._port_map = dict([(sw, {}) for sw in sw_names])

        try:
            host_decls = self.manifest['targets'][self.target]['hosts']
        except Exception as e:
            print(f"The target {self.target} does not declare 'hosts'")
            host_decls = None

        for id, sw_name in enumerate(sw_names):

            # check if there are any attributes such as the mac or ip set
            try: 

                self.addSwitch(sw_name, get_attributes(self.conf['switches'][sw_name],id))
            except:
                # No declaration
                self.addSwitch(sw_name)
        for host_name in host_names:
            host_num = host_names.index(host_name)+1

            self._host_links[host_name] = {}
            host_links = filter(lambda l: l[0]==host_name or l[1]==host_name, self.conf['links'])

            sw_idx = 0

            # check if specific ip or mac address has been declared for 'host_name'
            try: host_decl = host_decls[host_name]
            except Exception as e : host_decl = None

            # print(self.conf)
            # print()
            # print(self.manifest)

            # print(self.target)
            print(host_decl)
            try: host_ip = host_decl['ip']
            except Exception as e : 
                sys.exit(0)
                host_ip = "10.0.%d.%d" % (host_num, 100+sw_idx+1)
            print(f"host_ip:{host_ip}")
            try:
                host_mac = host_decl['mac']
            except Exception as e:
                host_mac = '00:04:00:00:%02x:%02x' % (host_num, sw_idx+1)


            self.addHost(host_name, attr_dict={"ip":host_ip, "mac":host_mac})



            for link in host_links:
                sw = link[0] if link[0] != host_name else link[1]
                assert sw in sw_names, "Hosts should be connected to switches, not " + str(sw)
                sw_num = sw_names.index(sw)+1


                delay_key = tuple(sorted([host_name, sw]))
                delay = self.conf['latencies'][delay_key] if delay_key in self.conf['latencies'] else '0ms'
                self._port_map[sw][host_name] = len(self._port_map[sw])+1

                self._host_links[host_name][sw] = dict(
                        idx=sw_idx,
                        host_mac = host_mac,
                        host_ip = host_ip,
                        sw = sw,
                        sw_mac = "00:aa:00:%02x:00:%02x" % (sw_num, host_num),
                        sw_ip = "10.%d.0.%d" % (host_num, sw_idx+1),
                        sw_port = self._port_map[sw][host_name]
                        )
                self._sw_hosts[sw][host_name] = self._host_links[host_name][sw]
                self.addLink(host_name, sw, delay=delay,
                        addr1=host_mac, addr2=self._host_links[host_name][sw]['sw_mac'])
                sw_idx += 1
        for link in self.conf['links']: # only check switch-switch links
            sw1, sw2 = link
            if sw1 not in sw_names or sw2 not in sw_names: continue

            delay_key = tuple(sorted(link))
            delay = self.conf['latencies'][delay_key] if delay_key in self.conf['latencies'] else '0ms'
            self.addLink(sw1, sw2, delay=delay)
            self._port_map[sw1][sw2] = len(self._port_map[sw1])+1
            self._port_map[sw2][sw1] = len(self._port_map[sw2])+1

            sw1_num, sw2_num = sw_names.index(sw1)+1, sw_names.index(sw2)+1
            sw1_port = dict(mac="00:aa:00:%02x:%02x:00" % (sw1_num, sw2_num), port=self._port_map[sw1][sw2])
            sw2_port = dict(mac="00:aa:00:%02x:%02x:00" % (sw2_num, sw1_num), port=self._port_map[sw2][sw1])

            self._sw_links[sw1][sw2] = [sw1_port, sw2_port]
            self._sw_links[sw2][sw1] = [sw2_port, sw1_port]

    def show(self):
        print(f"_host_links {self._host_links}")
        print(f"_sw_hosts {self._sw_hosts}")
        print(f"_sw_links {self._sw_links}")
        # print(f"_sw_links {self.items()}")
