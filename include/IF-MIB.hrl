%%% This file was automatically generated by snmpc_mib_to_hrl version 5.2.5
%%% Date: 19-Mar-2017::21:15:42
-ifndef('IF-MIB').
-define('IF-MIB', true).

%% Notifications
-define(linkUp, [1,3,6,1,6,3,1,1,5,4]).
-define(linkDown, [1,3,6,1,6,3,1,1,5,3]).

%% Oids

-define(interfaces, [1,3,6,1,2,1,2]).
-define(ifNumber, [1,3,6,1,2,1,2,1]).
-define(ifNumber_instance, [1,3,6,1,2,1,2,1,0]).

-define(ifTable, [1,3,6,1,2,1,2,2]).

-define(ifEntry, [1,3,6,1,2,1,2,2,1]).
-define(ifIndex, 1).
-define(ifDescr, 2).
-define(ifType, 3).
-define(ifMtu, 4).
-define(ifSpeed, 5).
-define(ifPhysAddress, 6).
-define(ifAdminStatus, 7).
-define(ifOperStatus, 8).
-define(ifLastChange, 9).
-define(ifInOctets, 10).
-define(ifInUcastPkts, 11).
-define(ifInNUcastPkts, 12).
-define(ifInDiscards, 13).
-define(ifInErrors, 14).
-define(ifInUnknownProtos, 15).
-define(ifOutOctets, 16).
-define(ifOutUcastPkts, 17).
-define(ifOutNUcastPkts, 18).
-define(ifOutDiscards, 19).
-define(ifOutErrors, 20).
-define(ifOutQLen, 21).
-define(ifSpecific, 22).

-define(ifMIB, [1,3,6,1,2,1,31]).

-define(ifMIBObjects, [1,3,6,1,2,1,31,1]).

-define(ifXTable, [1,3,6,1,2,1,31,1,1]).

-define(ifXEntry, [1,3,6,1,2,1,31,1,1,1]).
-define(ifName, 1).
-define(ifInMulticastPkts, 2).
-define(ifInBroadcastPkts, 3).
-define(ifOutMulticastPkts, 4).
-define(ifOutBroadcastPkts, 5).
-define(ifHCInOctets, 6).
-define(ifHCInUcastPkts, 7).
-define(ifHCInMulticastPkts, 8).
-define(ifHCInBroadcastPkts, 9).
-define(ifHCOutOctets, 10).
-define(ifHCOutUcastPkts, 11).
-define(ifHCOutMulticastPkts, 12).
-define(ifHCOutBroadcastPkts, 13).
-define(ifLinkUpDownTrapEnable, 14).
-define(ifHighSpeed, 15).
-define(ifPromiscuousMode, 16).
-define(ifConnectorPresent, 17).
-define(ifAlias, 18).
-define(ifCounterDiscontinuityTime, 19).

-define(ifStackTable, [1,3,6,1,2,1,31,1,2]).

-define(ifStackEntry, [1,3,6,1,2,1,31,1,2,1]).
-define(ifStackHigherLayer, 1).
-define(ifStackLowerLayer, 2).
-define(ifStackStatus, 3).

-define(ifTestTable, [1,3,6,1,2,1,31,1,3]).

-define(ifTestEntry, [1,3,6,1,2,1,31,1,3,1]).
-define(ifTestId, 1).
-define(ifTestStatus, 2).
-define(ifTestType, 3).
-define(ifTestResult, 4).
-define(ifTestCode, 5).
-define(ifTestOwner, 6).

-define(ifRcvAddressTable, [1,3,6,1,2,1,31,1,4]).

-define(ifRcvAddressEntry, [1,3,6,1,2,1,31,1,4,1]).
-define(ifRcvAddressAddress, 1).
-define(ifRcvAddressStatus, 2).
-define(ifRcvAddressType, 3).
-define(ifTableLastChange, [1,3,6,1,2,1,31,1,5]).
-define(ifTableLastChange_instance, [1,3,6,1,2,1,31,1,5,0]).
-define(ifStackLastChange, [1,3,6,1,2,1,31,1,6]).
-define(ifStackLastChange_instance, [1,3,6,1,2,1,31,1,6,0]).

-define(ifConformance, [1,3,6,1,2,1,31,2]).

-define(ifGroups, [1,3,6,1,2,1,31,2,1]).

-define(ifGeneralGroup, [1,3,6,1,2,1,31,2,1,1]).

-define(ifFixedLengthGroup, [1,3,6,1,2,1,31,2,1,2]).

-define(ifHCFixedLengthGroup, [1,3,6,1,2,1,31,2,1,3]).

-define(ifPacketGroup, [1,3,6,1,2,1,31,2,1,4]).

-define(ifHCPacketGroup, [1,3,6,1,2,1,31,2,1,5]).

-define(ifVHCPacketGroup, [1,3,6,1,2,1,31,2,1,6]).

-define(ifRcvAddressGroup, [1,3,6,1,2,1,31,2,1,7]).

-define(ifTestGroup, [1,3,6,1,2,1,31,2,1,8]).

-define(ifStackGroup, [1,3,6,1,2,1,31,2,1,9]).

-define(ifGeneralInformationGroup, [1,3,6,1,2,1,31,2,1,10]).

-define(ifStackGroup2, [1,3,6,1,2,1,31,2,1,11]).

-define(ifOldObjectsGroup, [1,3,6,1,2,1,31,2,1,12]).

-define(ifCounterDiscontinuityGroup, [1,3,6,1,2,1,31,2,1,13]).

-define(linkUpDownNotificationsGroup, [1,3,6,1,2,1,31,2,1,14]).

-define(ifCompliances, [1,3,6,1,2,1,31,2,2]).


%% Range values
-define(low_ifNumber, -2147483648).
-define(high_ifNumber, 2147483647).
-define(low_ifIndex, 1).
-define(high_ifIndex, 2147483647).
-define(low_ifDescr, 0).
-define(high_ifDescr, 255).
-define(low_ifMtu, -2147483648).
-define(high_ifMtu, 2147483647).
-define(low_ifSpeed, 0).
-define(high_ifSpeed, 4294967295).
-define(low_ifInOctets, 0).
-define(high_ifInOctets, 4294967295).
-define(low_ifInUcastPkts, 0).
-define(high_ifInUcastPkts, 4294967295).
-define(low_ifInNUcastPkts, 0).
-define(high_ifInNUcastPkts, 4294967295).
-define(low_ifInDiscards, 0).
-define(high_ifInDiscards, 4294967295).
-define(low_ifInErrors, 0).
-define(high_ifInErrors, 4294967295).
-define(low_ifInUnknownProtos, 0).
-define(high_ifInUnknownProtos, 4294967295).
-define(low_ifOutOctets, 0).
-define(high_ifOutOctets, 4294967295).
-define(low_ifOutUcastPkts, 0).
-define(high_ifOutUcastPkts, 4294967295).
-define(low_ifOutNUcastPkts, 0).
-define(high_ifOutNUcastPkts, 4294967295).
-define(low_ifOutDiscards, 0).
-define(high_ifOutDiscards, 4294967295).
-define(low_ifOutErrors, 0).
-define(high_ifOutErrors, 4294967295).
-define(low_ifOutQLen, 0).
-define(high_ifOutQLen, 4294967295).
-define(low_ifName, 0).
-define(high_ifName, 255).
-define(low_ifInMulticastPkts, 0).
-define(high_ifInMulticastPkts, 4294967295).
-define(low_ifInBroadcastPkts, 0).
-define(high_ifInBroadcastPkts, 4294967295).
-define(low_ifOutMulticastPkts, 0).
-define(high_ifOutMulticastPkts, 4294967295).
-define(low_ifOutBroadcastPkts, 0).
-define(high_ifOutBroadcastPkts, 4294967295).
-define(low_ifHighSpeed, 0).
-define(high_ifHighSpeed, 4294967295).
-define(low_ifAlias, 0).
-define(high_ifAlias, 64).
-define(low_ifStackHigherLayer, 0).
-define(high_ifStackHigherLayer, 2147483647).
-define(low_ifStackLowerLayer, 0).
-define(high_ifStackLowerLayer, 2147483647).
-define(low_ifTestId, 0).
-define(high_ifTestId, 2147483647).
-define(low_ifTestOwner, 0).
-define(high_ifTestOwner, 255).


%% Enum definitions from ifType
-define(ifType_switchstack, 65534).
-define(ifType_cableDownstreamRfPort, 257).
-define(ifType_docsCableUpstreamRfPort, 256).
-define(ifType_bits, 255).
-define(ifType_capwapWtpVirtualRadio, 254).
-define(ifType_capwapDot11Bss, 253).
-define(ifType_capwapDot11Profile, 252).
-define(ifType_vdsl2, 251).
-define(ifType_gpon, 250).
-define(ifType_aluELP, 249).
-define(ifType_pip, 248).
-define(ifType_ilan, 247).
-define(ifType_ifPwType, 246).
-define(ifType_voiceEBS, 245).
-define(ifType_wwanPP2, 244).
-define(ifType_wwanPP, 243).
-define(ifType_x86Laps, 242).
-define(ifType_dvbRcsTdma, 241).
-define(ifType_dvbTdm, 240).
-define(ifType_dvbRcsMacLayer, 239).
-define(ifType_adsl2plus, 238).
-define(ifType_ieee80216WMAN, 237).
-define(ifType_mocaVersion1, 236).
-define(ifType_voiceFGDOS, 235).
-define(ifType_atmbond, 234).
-define(ifType_aviciOpticalEther, 233).
-define(ifType_macSecUncontrolledIF, 232).
-define(ifType_macSecControlledIF, 231).
-define(ifType_adsl2, 230).
-define(ifType_docsCableMCmtsDownstream, 229).
-define(ifType_cblVectaStar, 228).
-define(ifType_lmp, 227).
-define(ifType_qam, 226).
-define(ifType_rpr, 225).
-define(ifType_fcipLink, 224).
-define(ifType_actelisMetaLOOP, 223).
-define(ifType_ciscoISLvlan, 222).
-define(ifType_gfp, 221).
-define(ifType_homepna, 220).
-define(ifType_opticalChannelGroup, 219).
-define(ifType_pdnEtherLoop2, 218).
-define(ifType_pdnEtherLoop1, 217).
-define(ifType_gtp, 216).
-define(ifType_sixToFour, 215).
-define(ifType_mpegTransport, 214).
-define(ifType_voiceDID, 213).
-define(ifType_voiceFGDEANA, 212).
-define(ifType_voiceEMFGD, 211).
-define(ifType_linegroup, 210).
-define(ifType_bridge, 209).
-define(ifType_pon622, 208).
-define(ifType_pon155, 207).
-define(ifType_econet, 206).
-define(ifType_docsCableUpstreamChannel, 205).
-define(ifType_sipSig, 204).
-define(ifType_sipTg, 203).
-define(ifType_virtualTg, 202).
-define(ifType_q2931, 201).
-define(ifType_teLink, 200).
-define(ifType_infiniband, 199).
-define(ifType_voiceOverCable, 198).
-define(ifType_propAtm, 197).
-define(ifType_opticalTransport, 196).
-define(ifType_opticalChannel, 195).
-define(ifType_atmVciEndPt, 194).
-define(ifType_frDlciEndPt, 193).
-define(ifType_reachDSL, 192).
-define(ifType_mvl, 191).
-define(ifType_imt, 190).
-define(ifType_atmRadio, 189).
-define(ifType_radioMAC, 188).
-define(ifType_aal2, 187).
-define(ifType_digitalWrapperOverheadChannel, 186).
-define(ifType_sonetOverheadChannel, 185).
-define(ifType_propBWAp2Mp, 184).
-define(ifType_hiperlan2, 183).
-define(ifType_propDocsWirelessUpstream, 182).
-define(ifType_propDocsWirelessDownstream, 181).
-define(ifType_propDocsWirelessMaclayer, 180).
-define(ifType_isup, 179).
-define(ifType_gr303IDT, 178).
-define(ifType_gr303RDT, 177).
-define(ifType_tr008, 176).
-define(ifType_nfas, 175).
-define(ifType_plc, 174).
-define(ifType_dvbAsiOut, 173).
-define(ifType_dvbAsiIn, 172).
-define(ifType_pos, 171).
-define(ifType_ds1FDL, 170).
-define(ifType_shdsl, 169).
-define(ifType_hdsl2, 168).
-define(ifType_mfSigLink, 167).
-define(ifType_mpls, 166).
-define(ifType_h323Proxy, 165).
-define(ifType_h323Gatekeeper, 164).
-define(ifType_frf16MfrBundle, 163).
-define(ifType_bgppolicyaccounting, 162).
-define(ifType_ieee8023adLag, 161).
-define(ifType_usb, 160).
-define(ifType_rfc1483, 159).
-define(ifType_frForward, 158).
-define(ifType_propWirelessP2P, 157).
-define(ifType_ss7SigLink, 156).
-define(ifType_compositeLink, 155).
-define(ifType_idsl, 154).
-define(ifType_voiceOverFrameRelay, 153).
-define(ifType_voiceOverAtm, 152).
-define(ifType_srp, 151).
-define(ifType_mplsTunnel, 150).
-define(ifType_atmVirtual, 149).
-define(ifType_dvbRccUpstream, 148).
-define(ifType_dvbRccDownstream, 147).
-define(ifType_dvbRccMacLayer, 146).
-define('ifType_if-gsn', 145).
-define(ifType_ieee1394, 144).
-define(ifType_msdsl, 143).
-define(ifType_ipForward, 142).
-define(ifType_dcn, 141).
-define(ifType_dtm, 140).
-define(ifType_mediaMailOverIp, 139).
-define(ifType_digitalPowerline, 138).
-define(ifType_l3ipxvlan, 137).
-define(ifType_l3ipvlan, 136).
-define(ifType_l2vlan, 135).
-define(ifType_atmSubInterface, 134).
-define(ifType_ces, 133).
-define(ifType_coffee, 132).
-define(ifType_tunnel, 131).
-define(ifType_a12MppSwitch, 130).
-define(ifType_docsCableUpstream, 129).
-define(ifType_docsCableDownstream, 128).
-define(ifType_docsCableMaclayer, 127).
-define(ifType_ip, 126).
-define(ifType_fast, 125).
-define(ifType_interleave, 124).
-define(ifType_transpHdlc, 123).
-define(ifType_x25huntGroup, 122).
-define(ifType_x25mlp, 121).
-define(ifType_v37, 120).
-define(ifType_lapf, 119).
-define(ifType_hdlc, 118).
-define(ifType_gigabitEthernet, 117).
-define(ifType_tdlc, 116).
-define(ifType_iso88025Fiber, 115).
-define(ifType_ipOverAtm, 114).
-define(ifType_mpc, 113).
-define(ifType_virtualIpAddress, 112).
-define(ifType_stackToStack, 111).
-define(ifType_ipOverClaw, 110).
-define(ifType_ipOverCdlc, 109).
-define(ifType_pppMultilinkBundle, 108).
-define(ifType_atmIma, 107).
-define(ifType_atmFuni, 106).
-define(ifType_atmDxi, 105).
-define(ifType_voiceOverIp, 104).
-define(ifType_voiceEncap, 103).
-define(ifType_voiceFXS, 102).
-define(ifType_voiceFXO, 101).
-define(ifType_voiceEM, 100).
-define(ifType_myrinet, 99).
-define(ifType_iso88025CRFPInt, 98).
-define(ifType_vdsl, 97).
-define(ifType_sdsl, 96).
-define(ifType_radsl, 95).
-define(ifType_adsl, 94).
-define(ifType_x213, 93).
-define(ifType_frameRelayMPI, 92).
-define(ifType_termPad, 91).
-define(ifType_hostPad, 90).
-define(ifType_propCnls, 89).
-define(ifType_arap, 88).
-define(ifType_eplrs, 87).
-define(ifType_iso88025Dtr, 86).
-define(ifType_cnr, 85).
-define(ifType_async, 84).
-define(ifType_bsc, 83).
-define(ifType_ds0Bundle, 82).
-define(ifType_ds0, 81).
-define(ifType_atmLogical, 80).
-define(ifType_rsrb, 79).
-define(ifType_ipSwitch, 78).
-define(ifType_lapd, 77).
-define(ifType_isdnu, 76).
-define(ifType_isdns, 75).
-define(ifType_dlsw, 74).
-define(ifType_escon, 73).
-define(ifType_ibm370parChan, 72).
-define(ifType_ieee80211, 71).
-define(ifType_channel, 70).
-define(ifType_fastEtherFX, 69).
-define(ifType_qllc, 68).
-define(ifType_g703at2mb, 67).
-define(ifType_g703at64k, 66).
-define(ifType_v36, 65).
-define(ifType_v11, 64).
-define(ifType_isdn, 63).
-define(ifType_fastEther, 62).
-define(ifType_cctEmul, 61).
-define(ifType_aflane8025, 60).
-define(ifType_aflane8023, 59).
-define(ifType_frameRelayInterconnect, 58).
-define(ifType_hippiInterface, 57).
-define(ifType_fibreChannel, 56).
-define(ifType_ieee80212, 55).
-define(ifType_propMultiplexor, 54).
-define(ifType_propVirtual, 53).
-define(ifType_smdsIcip, 52).
-define(ifType_sonetVT, 51).
-define(ifType_sonetPath, 50).
-define(ifType_aal5, 49).
-define(ifType_modem, 48).
-define(ifType_hippi, 47).
-define(ifType_hssi, 46).
-define(ifType_v35, 45).
-define(ifType_frameRelayService, 44).
-define(ifType_smdsDxi, 43).
-define(ifType_localTalk, 42).
-define(ifType_iso88022llc, 41).
-define(ifType_x25ple, 40).
-define(ifType_sonet, 39).
-define(ifType_miox25, 38).
-define(ifType_atm, 37).
-define(ifType_arcnetPlus, 36).
-define(ifType_arcnet, 35).
-define(ifType_para, 34).
-define(ifType_rs232, 33).
-define(ifType_frameRelay, 32).
-define(ifType_sip, 31).
-define(ifType_ds3, 30).
-define(ifType_ultra, 29).
-define(ifType_slip, 28).
-define(ifType_nsip, 27).
-define(ifType_ethernet3Mbit, 26).
-define(ifType_eon, 25).
-define(ifType_softwareLoopback, 24).
-define(ifType_ppp, 23).
-define(ifType_propPointToPointSerial, 22).
-define(ifType_primaryISDN, 21).
-define(ifType_basicISDN, 20).
-define(ifType_e1, 19).
-define(ifType_ds1, 18).
-define(ifType_sdlc, 17).
-define(ifType_lapb, 16).
-define(ifType_fddi, 15).
-define(ifType_hyperchannel, 14).
-define(ifType_proteon80Mbit, 13).
-define(ifType_proteon10Mbit, 12).
-define(ifType_starLan, 11).
-define(ifType_iso88026Man, 10).
-define(ifType_iso88025TokenRing, 9).
-define(ifType_iso88024TokenBus, 8).
-define(ifType_iso88023Csmacd, 7).
-define(ifType_ethernetCsmacd, 6).
-define(ifType_rfc877x25, 5).
-define(ifType_ddnX25, 4).
-define(ifType_hdh1822, 3).
-define(ifType_regular1822, 2).
-define(ifType_other, 1).

%% Enum definitions from ifAdminStatus
-define(ifAdminStatus_testing, 3).
-define(ifAdminStatus_down, 2).
-define(ifAdminStatus_up, 1).

%% Enum definitions from ifOperStatus
-define(ifOperStatus_lowerLayerDown, 7).
-define(ifOperStatus_notPresent, 6).
-define(ifOperStatus_dormant, 5).
-define(ifOperStatus_unknown, 4).
-define(ifOperStatus_testing, 3).
-define(ifOperStatus_down, 2).
-define(ifOperStatus_up, 1).

%% Enum definitions from ifLinkUpDownTrapEnable
-define(ifLinkUpDownTrapEnable_disabled, 2).
-define(ifLinkUpDownTrapEnable_enabled, 1).

%% Enum definitions from ifPromiscuousMode
-define(ifPromiscuousMode_false, 2).
-define(ifPromiscuousMode_true, 1).

%% Enum definitions from ifConnectorPresent
-define(ifConnectorPresent_false, 2).
-define(ifConnectorPresent_true, 1).

%% Enum definitions from ifStackStatus
-define(ifStackStatus_destroy, 6).
-define(ifStackStatus_createAndWait, 5).
-define(ifStackStatus_createAndGo, 4).
-define(ifStackStatus_notReady, 3).
-define(ifStackStatus_notInService, 2).
-define(ifStackStatus_active, 1).

%% Enum definitions from ifTestStatus
-define(ifTestStatus_inUse, 2).
-define(ifTestStatus_notInUse, 1).

%% Enum definitions from ifTestResult
-define(ifTestResult_failed, 7).
-define(ifTestResult_aborted, 6).
-define(ifTestResult_unAbleToRun, 5).
-define(ifTestResult_notSupported, 4).
-define(ifTestResult_inProgress, 3).
-define(ifTestResult_success, 2).
-define(ifTestResult_none, 1).

%% Enum definitions from ifRcvAddressStatus
-define(ifRcvAddressStatus_destroy, 6).
-define(ifRcvAddressStatus_createAndWait, 5).
-define(ifRcvAddressStatus_createAndGo, 4).
-define(ifRcvAddressStatus_notReady, 3).
-define(ifRcvAddressStatus_notInService, 2).
-define(ifRcvAddressStatus_active, 1).

%% Enum definitions from ifRcvAddressType
-define(ifRcvAddressType_nonVolatile, 3).
-define(ifRcvAddressType_volatile, 2).
-define(ifRcvAddressType_other, 1).

%% Default values
-define(default_ifNumber, -2147483648).
-define(default_ifRcvAddressType, 2).
-define(default_ifTableLastChange, 0).
-define(default_ifStackLastChange, 0).

-endif.
