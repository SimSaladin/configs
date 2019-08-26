" Extending vim-polyglot's systemd

" Useful:
" man -Pcat systemd.network | gawk '/^\[(\w*)\] SECTION OPTIONS/{printf "\n\" %s {{{1\n", $1} /^\s+\w+=\s*$/{gsub(/\s+|=/,""); printf "%s\\|", $1} END {print ""}' | xclip

" systemd.link {{{1
" [Match]
syn region sdMatchBlock matchgroup=sdHeader start=/^\[Match\]/ end=/^\[/me=e-2 contains=sdMatchKey
syn match sdMatchKey contained /^\%(MACAddress\|OriginalName\|Path\|Driver\|Type\|Host\|ConditionHost\|Virtualization\|ConditionVirtualization\|KernelCommandLine\|ConditionKernelCommandLine\|KernelVersion\|ConditionKernelVersion\|Architecture\|ConditionArchitecture\)=/
" [Link]
syn region sdLinkBlock matchgroup=sdHeader start=/^\[Link\]/ end=/^\[/me=e-2 contains=sdLinkKey
syn match sdLinkKey contained /^\%(Description\|Alias\|MACAddressPolicy\|MACAddress\|MACAddressPolicy\|NamePolicy\|ifnames\|Name\|NamePolicy\|NamePolicy\|MTUBytes\|BitsPerSecond\|Duplex\|AutoNegotiation\|WakeOnLan\|Port\|Advertise\|TCPSegmentationOffload\|TCP6SegmentationOffload\|GenericSegmentationOffload\|GenericReceiveOffload\|LargeReceiveOffload\|RxChannels\|TxChannels\|OtherChannels\|CombinedChannels\)=/ 
" }}}

" systemd.network {{{
" [Network] {{{1
syn region sdNetworkBlock  matchgroup=sdHeader start=/^\[Network\]/ end=/^\[/me=e-2 contains=sdNetworkKey
syn match sdNetworkKey contained /^\%(MACAddress\|Path\|Driver\|Type\|Name\|Host\|Virtualization\|KernelCommandLine\|KernelVersion\|Architecture\)=/
" [Link] {{{1
syn region sdLinkBlock  matchgroup=sdHeader start=/^\[Link\]/ end=/^\[/me=e-2 contains=sdLinkKey
syn match sdLinkKey contained /^\%(MACAddress\|MTUBytes\|ARP\|Multicast\|AllMulticast\|Unmanaged\|RequiredForOnline\)=/
" [Network] {{{1
syn region sdNetworkBlock  matchgroup=sdHeader start=/^\[Network\]/ end=/^\[/me=e-2 contains=sdNetworkKey
syn match sdNetworkKey contained /^\%(Description\|DHCP\|DHCPServer\|LinkLocalAddressing\|IPv4LLRoute\|IPv6Token\|LLMNR\|MulticastDNS\|DNSOverTLS\|DNSSEC\|DNSSECNegativeTrustAnchors\|LLDP\|EmitLLDP\|BindCarrier\|Address\|Gateway\|DNS\|Domains\|DNSDefaultRoute\|NTP\|IPForward\|IPMasquerade\|IPv6PrivacyExtensions\|IPv6AcceptRA\|IPv6DuplicateAddressDetection\|IPv6HopLimit\|IPv4ProxyARP\|IPv6ProxyNDP\|IPv6ProxyNDPAddress\|IPv6PrefixDelegation\|IPv6MTUBytes\|Bridge\|Bond\|VRF\|VLAN\|IPVLAN\|MACVLAN\|VXLAN\|Tunnel\|ActiveSlave\|PrimarySlave\|ConfigureWithoutCarrier\)=/
" [Address] {{{1
syn region sdAddressBlock  matchgroup=sdHeader start=/^\[Address\]/ end=/^\[/me=e-2 contains=sdAddressKey
syn match sdAddressKey contained /^\%(Address\|Peer\|Broadcast\|Label\|PreferredLifetime\|Scope\|HomeAddress\|DuplicateAddressDetection\|ManageTemporaryAddress\|PrefixRoute\|AutoJoin\)=/
" [Neighbor] {{{1
syn region sdNeighborBlock  matchgroup=sdHeader start=/^\[Neighbor\]/ end=/^\[/me=e-2 contains=sdNeighborKey
syn match sdNeighborKey contained /^\%(Address\|MACAddress\)=/
" [IPv6AddressLabel] {{{1
syn region sdIPv6AddressLabelBlock  matchgroup=sdHeader start=/^\[IPv6AddressLabel\]/ end=/^\[/me=e-2 contains=sdIPv6AddressLabelKey
syn match sdIPv6AddressLabelKey contained /^\%(Label\|Prefix\)=/
" [RoutingPolicyRule] {{{1
syn region sdRoutingPolicyRuleBlock  matchgroup=sdHeader start=/^\[RoutingPolicyRule\]/ end=/^\[/me=e-2 contains=sdRoutingPolicyRuleKey
syn match sdRoutingPolicyRuleKey contained /^\%(TypeOfService\|From\|To\|FirewallMark\|Table\|Priority\|IncomingInterface\|OutgoingInterface\|SourcePort\|DestinationPort\|IPProtocol\|InvertRule\)=/
" [Route] {{{1
syn region sdRouteBlock  matchgroup=sdHeader start=/^\[Route\]/ end=/^\[/me=e-2 contains=sdRouteKey
syn match sdRouteKey contained /^\%(Gateway\|GatewayOnlink\|Destination\|Source\|Metric\|IPv6Preference\|Scope\|PreferredSource\|Protocol\|Type\|InitialCongestionWindow\|InitialAdvertisedReceiveWindow\|QuickAck\|MTUBytes\)=/
" [DHCP] {{{1
syn region sdDHCPBlock  matchgroup=sdHeader start=/^\[DHCP\]/ end=/^\[/me=e-2 contains=sdDHCPKey
syn match sdDHCPKey contained /^\%(UseDNS\|UseNTP\|UseMTU\|Anonymize\|SendHostname\|UseHostname\|Hostname\|UseDomains\|UseRoutes\|UseTimezone\|CriticalConnection\|ClientIdentifier\|VendorClassIdentifier\|UserClass\|DUIDType\|DUIDRawData\|IAID\|RequestBroadcast\|RouteMetric\|ListenPort\|RapidCommit\|ForceDHCPv6PDOtherInformation\)=/
" [IPv6AcceptRA] {{{1
syn region sdIPv6AcceptRABlock  matchgroup=sdHeader start=/^\[IPv6AcceptRA\]/ end=/^\[/me=e-2 contains=sdIPv6AcceptRAKey
syn match sdIPv6AcceptRAKey contained /^\%(UseDNS\|UseDomains\)=/
" [DHCPServer] {{{1
syn region sdDHCPServerBlock  matchgroup=sdHeader start=/^\[DHCPServer\]/ end=/^\[/me=e-2 contains=sdDHCPServerKey
syn match sdDHCPServerKey contained /^\%(EmitRouter\)=/
" [IPv6PrefixDelegation] {{{1
syn region sdIPv6PrefixDelegationBlock  matchgroup=sdHeader start=/^\[IPv6PrefixDelegation\]/ end=/^\[/me=e-2 contains=sdIPv6PrefixDelegationKey
syn match sdIPv6PrefixDelegationKey contained /^\%(RouterLifetimeSec\|RouterPreference\|DNSLifetimeSec\)=/
" [IPv6Prefix] {{{1
syn region sdIPv6PrefixBlock  matchgroup=sdHeader start=/^\[IPv6Prefix\]/ end=/^\[/me=e-2 contains=sdIPv6PrefixKey
syn match sdIPv6PrefixKey contained /^\%(Prefix\)=/
" [Bridge] {{{1
syn region sdBridgeBlock  matchgroup=sdHeader start=/^\[Bridge\]/ end=/^\[/me=e-2 contains=sdBridgeKey
syn match sdBridgeKey contained /^\%(UnicastFlood\|MulticastToUnicast\|HairPin\|UseBPDU\|FastLeave\|AllowPortToBeRoot\|Cost\|Priority\)=/
" [BridgeFDB] {{{1
syn region sdBridgeFDBBlock  matchgroup=sdHeader start=/^\[BridgeFDB\]/ end=/^\[/me=e-2 contains=sdBridgeFDBKey
syn match sdBridgeFDBKey contained /^\%(MACAddress\|VLANId\)=/
" [CAN] {{{1
syn region sdCANBlock  matchgroup=sdHeader start=/^\[CAN\]/ end=/^\[/me=e-2 contains=sdCANKey
syn match sdCANKey contained /^\%(BitRate\|SamplePoint\|RestartSec\)=/
" [BridgeVLAN] {{{1
syn region sdBridgeVLANBlock  matchgroup=sdHeader start=/^\[BridgeVLAN\]/ end=/^\[/me=e-2 contains=sdBridgeVLANKey
syn match sdBridgeVLANKey contained /^\%(VLAN\|EgressUntagged\|PVID\)=/
" }}}


" systemd.netdev {{{

" [Match] {{{1
syn region sdMatchBlock  matchgroup=sdHeader start=/^\[Match\]/ end=/^\[/me=e-2 contains=sdMatchKey
syn match sdMatchKey contained /^\%(Host\|Virtualization\|KernelCommandLine\|KernelVersion\|Architecture\)=/
" [NetDev] {{{1
syn region sdNetDevBlock  matchgroup=sdHeader start=/^\[NetDev\]/ end=/^\[/me=e-2 contains=sdNetDevKey
syn match sdNetDevKey contained /^\%(Description\|Name\|Kind\|MTUBytes\|MACAddress\)=/
" [Bridge] {{{1
syn region sdBridgeBlock  matchgroup=sdHeader start=/^\[Bridge\]/ end=/^\[/me=e-2 contains=sdBridgeKey
syn match sdBridgeKey contained /^\%(HelloTimeSec\|MaxAgeSec\|ForwardDelaySec\|AgeingTimeSec\|Priority\|GroupForwardMask\|DefaultPVID\|MulticastQuerier\|MulticastSnooping\|VLANFiltering\|STP\)=/
" [VLAN] {{{1
syn region sdVLANBlock  matchgroup=sdHeader start=/^\[VLAN\]/ end=/^\[/me=e-2 contains=sdVLANKey
syn match sdVLANKey contained /^\%(Id\|GVRP\|MVRP\|LooseBinding\|ReorderHeader\)=/
" [MACVLAN|MACVTAP] {{{1
syn region sdMACVLANBlock  matchgroup=sdHeader start=/^\[MACVLAN\]/ end=/^\[/me=e-2 contains=sdMACVLANKey
syn region sdMACVTAPBlock  matchgroup=sdHeader start=/^\[MACVTAP\]/ end=/^\[/me=e-2 contains=sdMACVLANKey
syn match sdMACVLANKey contained /^\%(Mode\)=/
" [IPVLAN] {{{1
syn region sdIPVLANBlock  matchgroup=sdHeader start=/^\[IPVLAN\]/ end=/^\[/me=e-2 contains=sdIPVLANKey
syn match sdIPVLANKey contained /^\%(Mode\|Flags\)=/
" [VXLAN] {{{1
syn region sdVXLANBlock  matchgroup=sdHeader start=/^\[VXLAN\]/ end=/^\[/me=e-2 contains=sdVXLANKey
syn match sdVXLANKey contained /^\%(Id\|Remote\|Local\|TOS\|TTL\|MacLearning\|FDBAgeingSec\|MaximumFDBEntries\|ReduceARPProxy\|L2MissNotification\|L3MissNotification\|RouteShortCircuit\|UDPChecksum\|UDP6ZeroChecksumTx\|UDP6ZeroChecksumRx\|RemoteChecksumTx\|RemoteChecksumRx\|GroupPolicyExtension\|DestinationPort\|PortRange\|FlowLabel\)=/
" [GENEVE] {{{1
syn region sdGENEVEBlock  matchgroup=sdHeader start=/^\[GENEVE\]/ end=/^\[/me=e-2 contains=sdGENEVEKey
syn match sdGENEVEKey contained /^\%(Id\|Remote\|TOS\|TTL\|UDPChecksum\|UDP6ZeroChecksumTx\|UDP6ZeroChecksumRx\|DestinationPort\|FlowLabel\)=/
" [Tunnel] {{{1
syn region sdTunnelBlock  matchgroup=sdHeader start=/^\[Tunnel\]/ end=/^\[/me=e-2 contains=sdTunnelKey
syn match sdTunnelKey contained /^\%(Local\|Remote\|TOS\|TTL\|DiscoverPathMTU\|IPv6FlowLabel\|CopyDSCP\|EncapsulationLimit\|Key\|InputKey\|OutputKey\|Mode\|Independent\|AllowLocalRemote\|FooOverUDP\|FOUDestinationPort\|FOUSourcePort\|Encapsulation\|IPv6RapidDeploymentPrefix\|ISATAP\|SerializeTunneledPackets\|ERSPANIndex\)=/
" [FooOverUDP] {{{1
syn region sdFooOverUDPBlock  matchgroup=sdHeader start=/^\[FooOverUDP\]/ end=/^\[/me=e-2 contains=sdFooOverUDPKey
syn match sdFooOverUDPKey contained /^\%(Protocol\|Encapsulation\|Port\)=/
" [Peer] {{{1
syn region sdPeerBlock  matchgroup=sdHeader start=/^\[Peer\]/ end=/^\[/me=e-2 contains=sdPeerKey
syn match sdPeerKey contained /^\%(Name\|MACAddress\)=/
" [VXCAN] {{{1
syn region sdVXCANBlock  matchgroup=sdHeader start=/^\[VXCAN\]/ end=/^\[/me=e-2 contains=sdVXCANKey
syn match sdVXCANKey contained /^\%(Peer\)=/
" [Tun|Tap] {{{1
syn region sdTunBlock  matchgroup=sdHeader start=/^\[Tun\]/ end=/^\[/me=e-2 contains=sdTunKey
syn region sdTapBlock  matchgroup=sdHeader start=/^\[Tap\]/ end=/^\[/me=e-2 contains=sdTunKey
syn match sdTunKey contained /^\%(OneQueue\|MultiQueue\|PacketInfo\|VNetHeader\|User\|Group\)=/
" [WireGuard] {{{1
syn region sdWireGuardBlock  matchgroup=sdHeader start=/^\[WireGuard\]/ end=/^\[/me=e-2 contains=sdWireGuardKey
syn match sdWireGuardKey contained /^\%(PrivateKey\|ListenPort\|FwMark\)=/
" [WireGuardPeer] {{{1
syn region sdWireGuardPeerBlock  matchgroup=sdHeader start=/^\[WireGuardPeer\]/ end=/^\[/me=e-2 contains=sdWireGuardPeerKey
syn match sdWireGuardPeerKey contained /^\%(PublicKey\|PresharedKey\|AllowedIPs\|Endpoint\|PersistentKeepalive\)=/
" [Bond] {{{1
syn region sdBondBlock  matchgroup=sdHeader start=/^\[Bond\]/ end=/^\[/me=e-2 contains=sdBondKey
syn match sdBondKey contained /^\%(Mode\|TransmitHashPolicy\|LACPTransmitRate\|MIIMonitorSec\|UpDelaySec\|DownDelaySec\|LearnPacketIntervalSec\|AdSelect\|AdActorSystemPriority\|AdUserPortKey\|AdActorSystem\|FailOverMACPolicy\|ARPValidate\|ARPIntervalSec\|ARPIPTargets\|ARPAllTargets\|PrimaryReselectPolicy\|ResendIGMP\|PacketsPerSlave\|GratuitousARP\|AllSlavesActive\|DynamicTransmitLoadBalancing\|MinLinks\)=/
" [VRF] {{{1
syn region sdVRFBlock  matchgroup=sdHeader start=/^\[VRF\]/ end=/^\[/me=e-2 contains=sdVRFKey
syn match sdVRFKey contained /^\%(Table\)=/

" Coloring links: keys {{{1
hi def link sdAddressKey    sdKey
hi def link sdBondKey    sdKey
hi def link sdBridgeFDBKey    sdKey
hi def link sdBridgeKey    sdKey
hi def link sdBridgeVLANKey    sdKey
hi def link sdCANKey    sdKey
hi def link sdDHCPKey    sdKey
hi def link sdDHCPServerKey    sdKey
hi def link sdFooOverUDPKey    sdKey
hi def link sdGENEVEKey    sdKey
hi def link sdIPv6AcceptRAKey    sdKey
hi def link sdIPv6AddressLabelKey    sdKey
hi def link sdIPv6PrefixDelegationKey    sdKey
hi def link sdIPv6PrefixKey    sdKey
hi def link sdIPVLANKey    sdKey
hi def link sdLinkKey    sdKey
hi def link sdMACVLANKey    sdKey
hi def link sdMatchKey    sdKey
hi def link sdNeighborKey    sdKey
hi def link sdNetDevKey    sdKey
hi def link sdNetworkKey    sdKey
hi def link sdPeerKey    sdKey
hi def link sdRouteKey    sdKey
hi def link sdRoutingPolicyRuleKey    sdKey
hi def link sdTunKey    sdKey
hi def link sdTunnelKey    sdKey
hi def link sdVLANKey    sdKey
hi def link sdVRFKey    sdKey
hi def link sdVXCANKey    sdKey
hi def link sdVXLANKey    sdKey
hi def link sdWireGuardKey    sdKey
hi def link sdWireGuardPeerKey    sdKey

