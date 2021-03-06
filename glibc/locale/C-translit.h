#define NTRANSLIT 590
static const uint32_t translit_from_idx[] =
{
     0,    2,    4,    6,    8,   10,   12,   14,   16,   18,   20,   22,
    24,   26,   28,   30,   32,   34,   36,   38,   40,   42,   44,   46,
    48,   50,   52,   54,   56,   58,   60,   62,   64,   66,   68,   70,
    72,   74,   76,   78,   80,   82,   84,   86,   88,   90,   92,   94,
    96,   98,  100,  102,  104,  106,  108,  110,  112,  114,  116,  118,
   120,  122,  124,  126,  128,  130,  132,  134,  136,  138,  140,  142,
   144,  146,  148,  150,  152,  154,  156,  158,  160,  162,  164,  166,
   168,  170,  172,  174,  176,  178,  180,  182,  184,  186,  188,  190,
   192,  194,  196,  198,  200,  202,  204,  206,  208,  210,  212,  214,
   216,  218,  220,  222,  224,  226,  228,  230,  232,  234,  236,  238,
   240,  242,  244,  246,  248,  250,  252,  254,  256,  258,  260,  262,
   264,  266,  268,  270,  272,  274,  276,  278,  280,  282,  284,  286,
   288,  290,  292,  294,  296,  298,  300,  302,  304,  306,  308,  310,
   312,  314,  316,  318,  320,  322,  324,  326,  328,  330,  332,  334,
   336,  338,  340,  342,  344,  346,  348,  350,  352,  354,  356,  358,
   360,  362,  364,  366,  368,  370,  372,  374,  376,  378,  380,  382,
   384,  386,  388,  390,  392,  394,  396,  398,  400,  402,  404,  406,
   408,  410,  412,  414,  416,  418,  420,  422,  424,  426,  428,  430,
   432,  434,  436,  438,  440,  442,  444,  446,  448,  450,  452,  454,
   456,  458,  460,  462,  464,  466,  468,  470,  472,  474,  476,  478,
   480,  482,  484,  486,  488,  490,  492,  494,  496,  498,  500,  502,
   504,  506,  508,  510,  512,  514,  516,  518,  520,  522,  524,  526,
   528,  530,  532,  534,  536,  538,  540,  542,  544,  546,  548,  550,
   552,  554,  556,  558,  560,  562,  564,  566,  568,  570,  572,  574,
   576,  578,  580,  582,  584,  586,  588,  590,  592,  594,  596,  598,
   600,  602,  604,  606,  608,  610,  612,  614,  616,  618,  620,  622,
   624,  626,  628,  630,  632,  634,  636,  638,  640,  642,  644,  646,
   648,  650,  652,  654,  656,  658,  660,  662,  664,  666,  668,  670,
   672,  674,  676,  678,  680,  682,  684,  686,  688,  690,  692,  694,
   696,  698,  700,  702,  704,  706,  708,  710,  712,  714,  716,  718,
   720,  722,  724,  726,  728,  730,  732,  734,  736,  738,  740,  742,
   744,  746,  748,  750,  752,  754,  756,  758,  760,  762,  764,  766,
   768,  770,  772,  774,  776,  778,  780,  782,  784,  786,  788,  790,
   792,  794,  796,  798,  800,  802,  804,  806,  808,  810,  812,  814,
   816,  818,  820,  822,  824,  826,  828,  830,  832,  834,  836,  838,
   840,  842,  844,  846,  848,  850,  852,  854,  856,  858,  860,  862,
   864,  866,  868,  870,  872,  874,  876,  878,  880,  882,  884,  886,
   888,  890,  892,  894,  896,  898,  900,  902,  904,  906,  908,  910,
   912,  914,  916,  918,  920,  922,  924,  926,  928,  930,  932,  934,
   936,  938,  940,  942,  944,  946,  948,  950,  952,  954,  956,  958,
   960,  962,  964,  966,  968,  970,  972,  974,  976,  978,  980,  982,
   984,  986,  988,  990,  992,  994,  996,  998, 1000, 1002, 1004, 1006,
  1008, 1010, 1012, 1014, 1016, 1018, 1020, 1022, 1024, 1026, 1028, 1030,
  1032, 1034, 1036, 1038, 1040, 1042, 1044, 1046, 1048, 1050, 1052, 1054,
  1056, 1058, 1060, 1062, 1064, 1066, 1068, 1070, 1072, 1074, 1076, 1078,
  1080, 1082, 1084, 1086, 1088, 1090, 1092, 1094, 1096, 1098, 1100, 1102,
  1104, 1106, 1108, 1110, 1112, 1114, 1116, 1118, 1120, 1122, 1124, 1126,
  1128, 1130, 1132, 1134, 1136, 1138, 1140, 1142, 1144, 1146, 1148, 1150,
  1152, 1154, 1156, 1158, 1160, 1162, 1164, 1166, 1168, 1170, 1172, 1174,
  1176, 1178
};
static const wchar_t translit_from_tbl[] =
  L"\x00a0" L"\0" L"\x00a9" L"\0" L"\x00ab" L"\0" L"\x00ad" L"\0" L"\x00ae"
  L"\0" L"\x00b5" L"\0" L"\x00b8" L"\0" L"\x00bb" L"\0" L"\x00bc" L"\0"
  L"\x00bd" L"\0" L"\x00be" L"\0" L"\x00c6" L"\0" L"\x00d7" L"\0" L"\x00df"
  L"\0" L"\x00e6" L"\0" L"\x0132" L"\0" L"\x0133" L"\0" L"\x0149" L"\0"
  L"\x0152" L"\0" L"\x0152" L"\0" L"\x0153" L"\0" L"\x0153" L"\0" L"\x017f"
  L"\0" L"\x01c7" L"\0" L"\x01c8" L"\0" L"\x01c9" L"\0" L"\x01ca" L"\0"
  L"\x01cb" L"\0" L"\x01cc" L"\0" L"\x01f1" L"\0" L"\x01f2" L"\0" L"\x01f3"
  L"\0" L"\x02bc" L"\0" L"\x02c6" L"\0" L"\x02c8" L"\0" L"\x02cb" L"\0"
  L"\x02cd" L"\0" L"\x02d0" L"\0" L"\x02dc" L"\0" L"\x2002" L"\0" L"\x2003"
  L"\0" L"\x2004" L"\0" L"\x2005" L"\0" L"\x2006" L"\0" L"\x2008" L"\0"
  L"\x2009" L"\0" L"\x200a" L"\0" L"\x200b" L"\0" L"\x2010" L"\0" L"\x2011"
  L"\0" L"\x2012" L"\0" L"\x2013" L"\0" L"\x2014" L"\0" L"\x2015" L"\0"
  L"\x2018" L"\0" L"\x2019" L"\0" L"\x201a" L"\0" L"\x201b" L"\0" L"\x201c"
  L"\0" L"\x201d" L"\0" L"\x201e" L"\0" L"\x201f" L"\0" L"\x2020" L"\0"
  L"\x2022" L"\0" L"\x2024" L"\0" L"\x2025" L"\0" L"\x2026" L"\0" L"\x202f"
  L"\0" L"\x2035" L"\0" L"\x2036" L"\0" L"\x2037" L"\0" L"\x2039" L"\0"
  L"\x203a" L"\0" L"\x203c" L"\0" L"\x2048" L"\0" L"\x2049" L"\0" L"\x20a8"
  L"\0" L"\x20ac" L"\0" L"\x2100" L"\0" L"\x2101" L"\0" L"\x2102" L"\0"
  L"\x2105" L"\0" L"\x2106" L"\0" L"\x210a" L"\0" L"\x210b" L"\0" L"\x210c"
  L"\0" L"\x210d" L"\0" L"\x210e" L"\0" L"\x2110" L"\0" L"\x2111" L"\0"
  L"\x2112" L"\0" L"\x2113" L"\0" L"\x2115" L"\0" L"\x2116" L"\0" L"\x2119"
  L"\0" L"\x211a" L"\0" L"\x211b" L"\0" L"\x211c" L"\0" L"\x211d" L"\0"
  L"\x2121" L"\0" L"\x2122" L"\0" L"\x2124" L"\0" L"\x2126" L"\0" L"\x2128"
  L"\0" L"\x212c" L"\0" L"\x212d" L"\0" L"\x212e" L"\0" L"\x212f" L"\0"
  L"\x2130" L"\0" L"\x2131" L"\0" L"\x2133" L"\0" L"\x2134" L"\0" L"\x2139"
  L"\0" L"\x2153" L"\0" L"\x2154" L"\0" L"\x2155" L"\0" L"\x2156" L"\0"
  L"\x2157" L"\0" L"\x2158" L"\0" L"\x2159" L"\0" L"\x215a" L"\0" L"\x215b"
  L"\0" L"\x215c" L"\0" L"\x215d" L"\0" L"\x215e" L"\0" L"\x215f" L"\0"
  L"\x2160" L"\0" L"\x2161" L"\0" L"\x2162" L"\0" L"\x2163" L"\0" L"\x2164"
  L"\0" L"\x2165" L"\0" L"\x2166" L"\0" L"\x2167" L"\0" L"\x2168" L"\0"
  L"\x2169" L"\0" L"\x216a" L"\0" L"\x216b" L"\0" L"\x216c" L"\0" L"\x216d"
  L"\0" L"\x216e" L"\0" L"\x216f" L"\0" L"\x2170" L"\0" L"\x2171" L"\0"
  L"\x2172" L"\0" L"\x2173" L"\0" L"\x2174" L"\0" L"\x2175" L"\0" L"\x2176"
  L"\0" L"\x2177" L"\0" L"\x2178" L"\0" L"\x2179" L"\0" L"\x217a" L"\0"
  L"\x217b" L"\0" L"\x217c" L"\0" L"\x217d" L"\0" L"\x217e" L"\0" L"\x217f"
  L"\0" L"\x2190" L"\0" L"\x2192" L"\0" L"\x2194" L"\0" L"\x21d0" L"\0"
  L"\x21d2" L"\0" L"\x21d4" L"\0" L"\x2212" L"\0" L"\x2215" L"\0" L"\x2216"
  L"\0" L"\x2217" L"\0" L"\x2223" L"\0" L"\x2236" L"\0" L"\x223c" L"\0"
  L"\x2264" L"\0" L"\x2265" L"\0" L"\x226a" L"\0" L"\x226b" L"\0" L"\x22d8"
  L"\0" L"\x22d9" L"\0" L"\x2400" L"\0" L"\x2401" L"\0" L"\x2402" L"\0"
  L"\x2403" L"\0" L"\x2404" L"\0" L"\x2405" L"\0" L"\x2406" L"\0" L"\x2407"
  L"\0" L"\x2408" L"\0" L"\x2409" L"\0" L"\x240a" L"\0" L"\x240b" L"\0"
  L"\x240c" L"\0" L"\x240d" L"\0" L"\x240e" L"\0" L"\x240f" L"\0" L"\x2410"
  L"\0" L"\x2411" L"\0" L"\x2412" L"\0" L"\x2413" L"\0" L"\x2414" L"\0"
  L"\x2415" L"\0" L"\x2416" L"\0" L"\x2417" L"\0" L"\x2418" L"\0" L"\x2419"
  L"\0" L"\x241a" L"\0" L"\x241b" L"\0" L"\x241c" L"\0" L"\x241d" L"\0"
  L"\x241e" L"\0" L"\x241f" L"\0" L"\x2420" L"\0" L"\x2421" L"\0" L"\x2423"
  L"\0" L"\x2424" L"\0" L"\x2460" L"\0" L"\x2461" L"\0" L"\x2462" L"\0"
  L"\x2463" L"\0" L"\x2464" L"\0" L"\x2465" L"\0" L"\x2466" L"\0" L"\x2467"
  L"\0" L"\x2468" L"\0" L"\x2469" L"\0" L"\x246a" L"\0" L"\x246b" L"\0"
  L"\x246c" L"\0" L"\x246d" L"\0" L"\x246e" L"\0" L"\x246f" L"\0" L"\x2470"
  L"\0" L"\x2471" L"\0" L"\x2472" L"\0" L"\x2473" L"\0" L"\x2474" L"\0"
  L"\x2475" L"\0" L"\x2476" L"\0" L"\x2477" L"\0" L"\x2478" L"\0" L"\x2479"
  L"\0" L"\x247a" L"\0" L"\x247b" L"\0" L"\x247c" L"\0" L"\x247d" L"\0"
  L"\x247e" L"\0" L"\x247f" L"\0" L"\x2480" L"\0" L"\x2481" L"\0" L"\x2482"
  L"\0" L"\x2483" L"\0" L"\x2484" L"\0" L"\x2485" L"\0" L"\x2486" L"\0"
  L"\x2487" L"\0" L"\x2488" L"\0" L"\x2489" L"\0" L"\x248a" L"\0" L"\x248b"
  L"\0" L"\x248c" L"\0" L"\x248d" L"\0" L"\x248e" L"\0" L"\x248f" L"\0"
  L"\x2490" L"\0" L"\x2491" L"\0" L"\x2492" L"\0" L"\x2493" L"\0" L"\x2494"
  L"\0" L"\x2495" L"\0" L"\x2496" L"\0" L"\x2497" L"\0" L"\x2498" L"\0"
  L"\x2499" L"\0" L"\x249a" L"\0" L"\x249b" L"\0" L"\x249c" L"\0" L"\x249d"
  L"\0" L"\x249e" L"\0" L"\x249f" L"\0" L"\x24a0" L"\0" L"\x24a1" L"\0"
  L"\x24a2" L"\0" L"\x24a3" L"\0" L"\x24a4" L"\0" L"\x24a5" L"\0" L"\x24a6"
  L"\0" L"\x24a7" L"\0" L"\x24a8" L"\0" L"\x24a9" L"\0" L"\x24aa" L"\0"
  L"\x24ab" L"\0" L"\x24ac" L"\0" L"\x24ad" L"\0" L"\x24ae" L"\0" L"\x24af"
  L"\0" L"\x24b0" L"\0" L"\x24b1" L"\0" L"\x24b2" L"\0" L"\x24b3" L"\0"
  L"\x24b4" L"\0" L"\x24b5" L"\0" L"\x24b6" L"\0" L"\x24b7" L"\0" L"\x24b8"
  L"\0" L"\x24b9" L"\0" L"\x24ba" L"\0" L"\x24bb" L"\0" L"\x24bc" L"\0"
  L"\x24bd" L"\0" L"\x24be" L"\0" L"\x24bf" L"\0" L"\x24c0" L"\0" L"\x24c1"
  L"\0" L"\x24c2" L"\0" L"\x24c3" L"\0" L"\x24c4" L"\0" L"\x24c5" L"\0"
  L"\x24c6" L"\0" L"\x24c7" L"\0" L"\x24c8" L"\0" L"\x24c9" L"\0" L"\x24ca"
  L"\0" L"\x24cb" L"\0" L"\x24cc" L"\0" L"\x24cd" L"\0" L"\x24ce" L"\0"
  L"\x24cf" L"\0" L"\x24d0" L"\0" L"\x24d1" L"\0" L"\x24d2" L"\0" L"\x24d3"
  L"\0" L"\x24d4" L"\0" L"\x24d5" L"\0" L"\x24d6" L"\0" L"\x24d7" L"\0"
  L"\x24d8" L"\0" L"\x24d9" L"\0" L"\x24da" L"\0" L"\x24db" L"\0" L"\x24dc"
  L"\0" L"\x24dd" L"\0" L"\x24de" L"\0" L"\x24df" L"\0" L"\x24e0" L"\0"
  L"\x24e1" L"\0" L"\x24e2" L"\0" L"\x24e3" L"\0" L"\x24e4" L"\0" L"\x24e5"
  L"\0" L"\x24e6" L"\0" L"\x24e7" L"\0" L"\x24e8" L"\0" L"\x24e9" L"\0"
  L"\x24ea" L"\0" L"\x2500" L"\0" L"\x2502" L"\0" L"\x250c" L"\0" L"\x2510"
  L"\0" L"\x2514" L"\0" L"\x2518" L"\0" L"\x251c" L"\0" L"\x2524" L"\0"
  L"\x252c" L"\0" L"\x2534" L"\0" L"\x253c" L"\0" L"\x25e6" L"\0" L"\x3000"
  L"\0" L"\x3371" L"\0" L"\x3372" L"\0" L"\x3373" L"\0" L"\x3374" L"\0"
  L"\x3375" L"\0" L"\x3376" L"\0" L"\x3380" L"\0" L"\x3381" L"\0" L"\x3382"
  L"\0" L"\x3383" L"\0" L"\x3384" L"\0" L"\x3385" L"\0" L"\x3386" L"\0"
  L"\x3387" L"\0" L"\x3388" L"\0" L"\x3389" L"\0" L"\x338a" L"\0" L"\x338b"
  L"\0" L"\x338c" L"\0" L"\x338d" L"\0" L"\x338e" L"\0" L"\x338f" L"\0"
  L"\x3390" L"\0" L"\x3391" L"\0" L"\x3392" L"\0" L"\x3393" L"\0" L"\x3394"
  L"\0" L"\x3395" L"\0" L"\x3396" L"\0" L"\x3397" L"\0" L"\x3398" L"\0"
  L"\x3399" L"\0" L"\x339a" L"\0" L"\x339b" L"\0" L"\x339c" L"\0" L"\x339d"
  L"\0" L"\x339e" L"\0" L"\x339f" L"\0" L"\x33a0" L"\0" L"\x33a1" L"\0"
  L"\x33a2" L"\0" L"\x33a3" L"\0" L"\x33a4" L"\0" L"\x33a5" L"\0" L"\x33a6"
  L"\0" L"\x33a7" L"\0" L"\x33a8" L"\0" L"\x33a9" L"\0" L"\x33aa" L"\0"
  L"\x33ab" L"\0" L"\x33ac" L"\0" L"\x33ad" L"\0" L"\x33ae" L"\0" L"\x33af"
  L"\0" L"\x33b0" L"\0" L"\x33b1" L"\0" L"\x33b2" L"\0" L"\x33b3" L"\0"
  L"\x33b4" L"\0" L"\x33b5" L"\0" L"\x33b6" L"\0" L"\x33b7" L"\0" L"\x33b8"
  L"\0" L"\x33b9" L"\0" L"\x33ba" L"\0" L"\x33bb" L"\0" L"\x33bc" L"\0"
  L"\x33bd" L"\0" L"\x33be" L"\0" L"\x33bf" L"\0" L"\x33c2" L"\0" L"\x33c3"
  L"\0" L"\x33c4" L"\0" L"\x33c5" L"\0" L"\x33c6" L"\0" L"\x33c7" L"\0"
  L"\x33c8" L"\0" L"\x33c9" L"\0" L"\x33ca" L"\0" L"\x33cb" L"\0" L"\x33cc"
  L"\0" L"\x33cd" L"\0" L"\x33ce" L"\0" L"\x33cf" L"\0" L"\x33d0" L"\0"
  L"\x33d1" L"\0" L"\x33d2" L"\0" L"\x33d3" L"\0" L"\x33d4" L"\0" L"\x33d5"
  L"\0" L"\x33d6" L"\0" L"\x33d7" L"\0" L"\x33d8" L"\0" L"\x33d9" L"\0"
  L"\x33da" L"\0" L"\x33db" L"\0" L"\x33dc" L"\0" L"\x33dd" L"\0" L"\xfb00"
  L"\0" L"\xfb01" L"\0" L"\xfb02" L"\0" L"\xfb03" L"\0" L"\xfb04" L"\0"
  L"\xfb06" L"\0" L"\xfb29" L"\0" L"\xfe4d" L"\0" L"\xfe4e" L"\0" L"\xfe4f"
  L"\0" L"\xfe50" L"\0" L"\xfe52" L"\0" L"\xfe54" L"\0" L"\xfe55" L"\0"
  L"\xfe56" L"\0" L"\xfe57" L"\0" L"\xfe59" L"\0" L"\xfe5a" L"\0" L"\xfe5b"
  L"\0" L"\xfe5c" L"\0" L"\xfe5f" L"\0" L"\xfe60" L"\0" L"\xfe61" L"\0"
  L"\xfe62" L"\0" L"\xfe63" L"\0" L"\xfe64" L"\0" L"\xfe65" L"\0" L"\xfe66"
  L"\0" L"\xfe68" L"\0" L"\xfe69" L"\0" L"\xfe6a" L"\0" L"\xfe6b" L"\0"
  L"\xfeff" L"\0" L"\xff01" L"\0" L"\xff02" L"\0" L"\xff03" L"\0" L"\xff04"
  L"\0" L"\xff05" L"\0" L"\xff06" L"\0" L"\xff07" L"\0" L"\xff08" L"\0"
  L"\xff09" L"\0" L"\xff0a" L"\0" L"\xff0b" L"\0" L"\xff0c" L"\0" L"\xff0d"
  L"\0" L"\xff0e" L"\0" L"\xff0f" L"\0" L"\xff10" L"\0" L"\xff11" L"\0"
  L"\xff12" L"\0" L"\xff13" L"\0" L"\xff14" L"\0" L"\xff15" L"\0" L"\xff16"
  L"\0" L"\xff17" L"\0" L"\xff18" L"\0" L"\xff19" L"\0" L"\xff1a" L"\0"
  L"\xff1b" L"\0" L"\xff1c" L"\0" L"\xff1d" L"\0" L"\xff1e" L"\0" L"\xff1f"
  L"\0" L"\xff20" L"\0" L"\xff21" L"\0" L"\xff22" L"\0" L"\xff23" L"\0"
  L"\xff24" L"\0" L"\xff25" L"\0" L"\xff26" L"\0" L"\xff27" L"\0" L"\xff28"
  L"\0" L"\xff29" L"\0" L"\xff2a" L"\0" L"\xff2b" L"\0" L"\xff2c" L"\0"
  L"\xff2d" L"\0" L"\xff2e" L"\0" L"\xff2f" L"\0" L"\xff30" L"\0" L"\xff31"
  L"\0" L"\xff32" L"\0" L"\xff33" L"\0" L"\xff34" L"\0" L"\xff35" L"\0"
  L"\xff36" L"\0" L"\xff37" L"\0" L"\xff38" L"\0" L"\xff39" L"\0" L"\xff3a"
  L"\0" L"\xff3b" L"\0" L"\xff3c" L"\0" L"\xff3d" L"\0" L"\xff3e" L"\0"
  L"\xff3f" L"\0" L"\xff40" L"\0" L"\xff41" L"\0" L"\xff42" L"\0" L"\xff43"
  L"\0" L"\xff44" L"\0" L"\xff45" L"\0" L"\xff46" L"\0" L"\xff47" L"\0"
  L"\xff48" L"\0" L"\xff49" L"\0" L"\xff4a" L"\0" L"\xff4b" L"\0" L"\xff4c"
  L"\0" L"\xff4d" L"\0" L"\xff4e" L"\0" L"\xff4f" L"\0" L"\xff50" L"\0"
  L"\xff51" L"\0" L"\xff52" L"\0" L"\xff53" L"\0" L"\xff54" L"\0" L"\xff55"
  L"\0" L"\xff56" L"\0" L"\xff57" L"\0" L"\xff58" L"\0" L"\xff59" L"\0"
  L"\xff5a" L"\0" L"\xff5b" L"\0" L"\xff5c" L"\0" L"\xff5d" L"\0" L"\xff5e";
static const uint32_t translit_to_idx[] =
{
     0,    3,    8,   12,   15,   20,   23,   26,   30,   37,   44,   51,
    55,   58,   62,   66,   70,   74,   78,   82,   86,   90,   94,   97,
   101,  105,  109,  113,  117,  121,  125,  129,  133,  136,  139,  142,
   145,  148,  151,  154,  157,  160,  163,  166,  169,  172,  175,  178,
   180,  183,  186,  189,  192,  196,  199,  202,  205,  208,  211,  214,
   217,  221,  224,  227,  230,  233,  237,  242,  245,  248,  252,  257,
   260,  263,  267,  271,  275,  279,  284,  289,  294,  297,  302,  307,
   310,  313,  316,  319,  322,  325,  328,  331,  334,  337,  341,  344,
   347,  350,  353,  356,  361,  367,  370,  375,  378,  381,  384,  387,
   390,  393,  396,  399,  402,  405,  412,  419,  426,  433,  440,  447,
   454,  461,  468,  475,  482,  489,  494,  497,  501,  506,  510,  513,
   517,  522,  528,  532,  535,  539,  544,  547,  550,  553,  556,  559,
   563,  568,  572,  575,  579,  584,  590,  594,  597,  601,  606,  609,
   612,  615,  618,  622,  626,  631,  635,  639,  644,  647,  650,  653,
   656,  659,  662,  665,  669,  673,  677,  681,  686,  691,  696,  701,
   706,  711,  716,  721,  726,  731,  735,  739,  743,  747,  751,  755,
   759,  763,  768,  773,  778,  783,  788,  793,  798,  803,  808,  812,
   817,  822,  826,  830,  834,  838,  842,  847,  850,  854,  859,  864,
   869,  874,  879,  884,  889,  894,  899,  905,  911,  917,  923,  929,
   935,  941,  947,  953,  959,  965,  970,  975,  980,  985,  990,  995,
  1000, 1005, 1010, 1016, 1022, 1028, 1034, 1040, 1046, 1052, 1058, 1064,
  1070, 1076, 1080, 1084, 1088, 1092, 1096, 1100, 1104, 1108, 1112, 1117,
  1122, 1127, 1132, 1137, 1142, 1147, 1152, 1157, 1162, 1167, 1172, 1177,
  1182, 1187, 1192, 1197, 1202, 1207, 1212, 1217, 1222, 1227, 1232, 1237,
  1242, 1247, 1252, 1257, 1262, 1267, 1272, 1277, 1282, 1287, 1292, 1297,
  1302, 1307, 1312, 1317, 1322, 1327, 1332, 1337, 1342, 1347, 1352, 1357,
  1362, 1367, 1372, 1377, 1382, 1387, 1392, 1397, 1402, 1407, 1412, 1417,
  1422, 1427, 1432, 1437, 1442, 1447, 1452, 1457, 1462, 1467, 1472, 1477,
  1482, 1487, 1492, 1497, 1502, 1507, 1512, 1517, 1522, 1527, 1532, 1537,
  1542, 1547, 1552, 1557, 1562, 1565, 1568, 1571, 1574, 1577, 1580, 1583,
  1586, 1589, 1592, 1595, 1598, 1601, 1606, 1610, 1614, 1619, 1623, 1627,
  1631, 1635, 1639, 1643, 1647, 1651, 1655, 1659, 1664, 1670, 1674, 1678,
  1682, 1686, 1690, 1694, 1698, 1703, 1708, 1713, 1718, 1722, 1726, 1730,
  1734, 1738, 1742, 1746, 1750, 1754, 1758, 1764, 1770, 1775, 1781, 1787,
  1793, 1798, 1804, 1809, 1816, 1820, 1825, 1830, 1835, 1840, 1847, 1856,
  1860, 1864, 1868, 1872, 1876, 1880, 1884, 1888, 1892, 1896, 1900, 1904,
  1908, 1912, 1916, 1920, 1926, 1930, 1934, 1938, 1944, 1949, 1953, 1957,
  1961, 1965, 1969, 1973, 1977, 1981, 1985, 1989, 1994, 1998, 2002, 2007,
  2012, 2016, 2022, 2027, 2031, 2035, 2039, 2043, 2047, 2051, 2055, 2060,
  2065, 2069, 2072, 2075, 2078, 2081, 2084, 2087, 2090, 2093, 2096, 2099,
  2102, 2105, 2108, 2111, 2114, 2117, 2120, 2123, 2126, 2129, 2132, 2135,
  2138, 2141, 2144, 2147, 2149, 2152, 2155, 2158, 2161, 2164, 2167, 2170,
  2173, 2176, 2179, 2182, 2185, 2188, 2191, 2194, 2197, 2200, 2203, 2206,
  2209, 2212, 2215, 2218, 2221, 2224, 2227, 2230, 2233, 2236, 2239, 2242,
  2245, 2248, 2251, 2254, 2257, 2260, 2263, 2266, 2269, 2272, 2275, 2278,
  2281, 2284, 2287, 2290, 2293, 2296, 2299, 2302, 2305, 2308, 2311, 2314,
  2317, 2320, 2323, 2326, 2329, 2332, 2335, 2338, 2341, 2344, 2347, 2350,
  2353, 2356, 2359, 2362, 2365, 2368, 2371, 2374, 2377, 2380, 2383, 2386,
  2389, 2392, 2395, 2398, 2401, 2404, 2407, 2410, 2413, 2416, 2419, 2422,
  2425, 2428
};
static const wchar_t translit_to_tbl[] =
  L" \0" L"\0" L"(C)\0" L"\0" L"<<\0" L"\0" L"-\0" L"\0" L"(R)\0" L"\0" L"u\0"
  L"\0" L",\0" L"\0" L">>\0" L"\0" L" 1/4 \0" L"\0" L" 1/2 \0" L"\0"
  L" 3/4 \0" L"\0" L"AE\0" L"\0" L"x\0" L"\0" L"ss\0" L"\0" L"ae\0" L"\0"
  L"IJ\0" L"\0" L"ij\0" L"\0" L"'n\0" L"\0" L"OE\0" L"\0" L"OE\0" L"\0"
  L"oe\0" L"\0" L"oe\0" L"\0" L"s\0" L"\0" L"LJ\0" L"\0" L"Lj\0" L"\0" L"lj\0"
  L"\0" L"NJ\0" L"\0" L"Nj\0" L"\0" L"nj\0" L"\0" L"DZ\0" L"\0" L"Dz\0" L"\0"
  L"dz\0" L"\0" L"'\0" L"\0" L"^\0" L"\0" L"'\0" L"\0" L"`\0" L"\0" L"_\0"
  L"\0" L":\0" L"\0" L"~\0" L"\0" L" \0" L"\0" L" \0" L"\0" L" \0" L"\0"
  L" \0" L"\0" L" \0" L"\0" L" \0" L"\0" L" \0" L"\0" L" \0" L"\0" L"\0" L"\0"
  L"-\0" L"\0" L"-\0" L"\0" L"-\0" L"\0" L"-\0" L"\0" L"--\0" L"\0" L"-\0"
  L"\0" L"'\0" L"\0" L"'\0" L"\0" L",\0" L"\0" L"'\0" L"\0" L"\"\0" L"\0"
  L"\"\0" L"\0" L",,\0" L"\0" L"\"\0" L"\0" L"+\0" L"\0" L"o\0" L"\0" L".\0"
  L"\0" L"..\0" L"\0" L"...\0" L"\0" L" \0" L"\0" L"`\0" L"\0" L"``\0" L"\0"
  L"```\0" L"\0" L"<\0" L"\0" L">\0" L"\0" L"!!\0" L"\0" L"?!\0" L"\0" L"!?\0"
  L"\0" L"Rs\0" L"\0" L"EUR\0" L"\0" L"a/c\0" L"\0" L"a/s\0" L"\0" L"C\0"
  L"\0" L"c/o\0" L"\0" L"c/u\0" L"\0" L"g\0" L"\0" L"H\0" L"\0" L"H\0" L"\0"
  L"H\0" L"\0" L"h\0" L"\0" L"I\0" L"\0" L"I\0" L"\0" L"L\0" L"\0" L"l\0"
  L"\0" L"N\0" L"\0" L"No\0" L"\0" L"P\0" L"\0" L"Q\0" L"\0" L"R\0" L"\0"
  L"R\0" L"\0" L"R\0" L"\0" L"TEL\0" L"\0" L"(TM)\0" L"\0" L"Z\0" L"\0"
  L"Ohm\0" L"\0" L"Z\0" L"\0" L"B\0" L"\0" L"C\0" L"\0" L"e\0" L"\0" L"e\0"
  L"\0" L"E\0" L"\0" L"F\0" L"\0" L"M\0" L"\0" L"o\0" L"\0" L"i\0" L"\0"
  L" 1/3 \0" L"\0" L" 2/3 \0" L"\0" L" 1/5 \0" L"\0" L" 2/5 \0" L"\0"
  L" 3/5 \0" L"\0" L" 4/5 \0" L"\0" L" 1/6 \0" L"\0" L" 5/6 \0" L"\0"
  L" 1/8 \0" L"\0" L" 3/8 \0" L"\0" L" 5/8 \0" L"\0" L" 7/8 \0" L"\0" L" 1/\0"
  L"\0" L"I\0" L"\0" L"II\0" L"\0" L"III\0" L"\0" L"IV\0" L"\0" L"V\0" L"\0"
  L"VI\0" L"\0" L"VII\0" L"\0" L"VIII\0" L"\0" L"IX\0" L"\0" L"X\0" L"\0"
  L"XI\0" L"\0" L"XII\0" L"\0" L"L\0" L"\0" L"C\0" L"\0" L"D\0" L"\0" L"M\0"
  L"\0" L"i\0" L"\0" L"ii\0" L"\0" L"iii\0" L"\0" L"iv\0" L"\0" L"v\0" L"\0"
  L"vi\0" L"\0" L"vii\0" L"\0" L"viii\0" L"\0" L"ix\0" L"\0" L"x\0" L"\0"
  L"xi\0" L"\0" L"xii\0" L"\0" L"l\0" L"\0" L"c\0" L"\0" L"d\0" L"\0" L"m\0"
  L"\0" L"<-\0" L"\0" L"->\0" L"\0" L"<->\0" L"\0" L"<=\0" L"\0" L"=>\0" L"\0"
  L"<=>\0" L"\0" L"-\0" L"\0" L"/\0" L"\0" L"\\\0" L"\0" L"*\0" L"\0" L"|\0"
  L"\0" L":\0" L"\0" L"~\0" L"\0" L"<=\0" L"\0" L">=\0" L"\0" L"<<\0" L"\0"
  L">>\0" L"\0" L"<<<\0" L"\0" L">>>\0" L"\0" L"NUL\0" L"\0" L"SOH\0" L"\0"
  L"STX\0" L"\0" L"ETX\0" L"\0" L"EOT\0" L"\0" L"ENQ\0" L"\0" L"ACK\0" L"\0"
  L"BEL\0" L"\0" L"BS\0" L"\0" L"HT\0" L"\0" L"LF\0" L"\0" L"VT\0" L"\0"
  L"FF\0" L"\0" L"CR\0" L"\0" L"SO\0" L"\0" L"SI\0" L"\0" L"DLE\0" L"\0"
  L"DC1\0" L"\0" L"DC2\0" L"\0" L"DC3\0" L"\0" L"DC4\0" L"\0" L"NAK\0" L"\0"
  L"SYN\0" L"\0" L"ETB\0" L"\0" L"CAN\0" L"\0" L"EM\0" L"\0" L"SUB\0" L"\0"
  L"ESC\0" L"\0" L"FS\0" L"\0" L"GS\0" L"\0" L"RS\0" L"\0" L"US\0" L"\0"
  L"SP\0" L"\0" L"DEL\0" L"\0" L"_\0" L"\0" L"NL\0" L"\0" L"(1)\0" L"\0"
  L"(2)\0" L"\0" L"(3)\0" L"\0" L"(4)\0" L"\0" L"(5)\0" L"\0" L"(6)\0" L"\0"
  L"(7)\0" L"\0" L"(8)\0" L"\0" L"(9)\0" L"\0" L"(10)\0" L"\0" L"(11)\0" L"\0"
  L"(12)\0" L"\0" L"(13)\0" L"\0" L"(14)\0" L"\0" L"(15)\0" L"\0" L"(16)\0"
  L"\0" L"(17)\0" L"\0" L"(18)\0" L"\0" L"(19)\0" L"\0" L"(20)\0" L"\0"
  L"(1)\0" L"\0" L"(2)\0" L"\0" L"(3)\0" L"\0" L"(4)\0" L"\0" L"(5)\0" L"\0"
  L"(6)\0" L"\0" L"(7)\0" L"\0" L"(8)\0" L"\0" L"(9)\0" L"\0" L"(10)\0" L"\0"
  L"(11)\0" L"\0" L"(12)\0" L"\0" L"(13)\0" L"\0" L"(14)\0" L"\0" L"(15)\0"
  L"\0" L"(16)\0" L"\0" L"(17)\0" L"\0" L"(18)\0" L"\0" L"(19)\0" L"\0"
  L"(20)\0" L"\0" L"1.\0" L"\0" L"2.\0" L"\0" L"3.\0" L"\0" L"4.\0" L"\0"
  L"5.\0" L"\0" L"6.\0" L"\0" L"7.\0" L"\0" L"8.\0" L"\0" L"9.\0" L"\0"
  L"10.\0" L"\0" L"11.\0" L"\0" L"12.\0" L"\0" L"13.\0" L"\0" L"14.\0" L"\0"
  L"15.\0" L"\0" L"16.\0" L"\0" L"17.\0" L"\0" L"18.\0" L"\0" L"19.\0" L"\0"
  L"20.\0" L"\0" L"(a)\0" L"\0" L"(b)\0" L"\0" L"(c)\0" L"\0" L"(d)\0" L"\0"
  L"(e)\0" L"\0" L"(f)\0" L"\0" L"(g)\0" L"\0" L"(h)\0" L"\0" L"(i)\0" L"\0"
  L"(j)\0" L"\0" L"(k)\0" L"\0" L"(l)\0" L"\0" L"(m)\0" L"\0" L"(n)\0" L"\0"
  L"(o)\0" L"\0" L"(p)\0" L"\0" L"(q)\0" L"\0" L"(r)\0" L"\0" L"(s)\0" L"\0"
  L"(t)\0" L"\0" L"(u)\0" L"\0" L"(v)\0" L"\0" L"(w)\0" L"\0" L"(x)\0" L"\0"
  L"(y)\0" L"\0" L"(z)\0" L"\0" L"(A)\0" L"\0" L"(B)\0" L"\0" L"(C)\0" L"\0"
  L"(D)\0" L"\0" L"(E)\0" L"\0" L"(F)\0" L"\0" L"(G)\0" L"\0" L"(H)\0" L"\0"
  L"(I)\0" L"\0" L"(J)\0" L"\0" L"(K)\0" L"\0" L"(L)\0" L"\0" L"(M)\0" L"\0"
  L"(N)\0" L"\0" L"(O)\0" L"\0" L"(P)\0" L"\0" L"(Q)\0" L"\0" L"(R)\0" L"\0"
  L"(S)\0" L"\0" L"(T)\0" L"\0" L"(U)\0" L"\0" L"(V)\0" L"\0" L"(W)\0" L"\0"
  L"(X)\0" L"\0" L"(Y)\0" L"\0" L"(Z)\0" L"\0" L"(a)\0" L"\0" L"(b)\0" L"\0"
  L"(c)\0" L"\0" L"(d)\0" L"\0" L"(e)\0" L"\0" L"(f)\0" L"\0" L"(g)\0" L"\0"
  L"(h)\0" L"\0" L"(i)\0" L"\0" L"(j)\0" L"\0" L"(k)\0" L"\0" L"(l)\0" L"\0"
  L"(m)\0" L"\0" L"(n)\0" L"\0" L"(o)\0" L"\0" L"(p)\0" L"\0" L"(q)\0" L"\0"
  L"(r)\0" L"\0" L"(s)\0" L"\0" L"(t)\0" L"\0" L"(u)\0" L"\0" L"(v)\0" L"\0"
  L"(w)\0" L"\0" L"(x)\0" L"\0" L"(y)\0" L"\0" L"(z)\0" L"\0" L"(0)\0" L"\0"
  L"-\0" L"\0" L"|\0" L"\0" L"+\0" L"\0" L"+\0" L"\0" L"+\0" L"\0" L"+\0"
  L"\0" L"+\0" L"\0" L"+\0" L"\0" L"+\0" L"\0" L"+\0" L"\0" L"+\0" L"\0"
  L"o\0" L"\0" L" \0" L"\0" L"hPa\0" L"\0" L"da\0" L"\0" L"AU\0" L"\0"
  L"bar\0" L"\0" L"oV\0" L"\0" L"pc\0" L"\0" L"pA\0" L"\0" L"nA\0" L"\0"
  L"uA\0" L"\0" L"mA\0" L"\0" L"kA\0" L"\0" L"KB\0" L"\0" L"MB\0" L"\0"
  L"GB\0" L"\0" L"cal\0" L"\0" L"kcal\0" L"\0" L"pF\0" L"\0" L"nF\0" L"\0"
  L"uF\0" L"\0" L"ug\0" L"\0" L"mg\0" L"\0" L"kg\0" L"\0" L"Hz\0" L"\0"
  L"kHz\0" L"\0" L"MHz\0" L"\0" L"GHz\0" L"\0" L"THz\0" L"\0" L"ul\0" L"\0"
  L"ml\0" L"\0" L"dl\0" L"\0" L"kl\0" L"\0" L"fm\0" L"\0" L"nm\0" L"\0"
  L"um\0" L"\0" L"mm\0" L"\0" L"cm\0" L"\0" L"km\0" L"\0" L"mm^2\0" L"\0"
  L"cm^2\0" L"\0" L"m^2\0" L"\0" L"km^2\0" L"\0" L"mm^3\0" L"\0" L"cm^3\0"
  L"\0" L"m^3\0" L"\0" L"km^3\0" L"\0" L"m/s\0" L"\0" L"m/s^2\0" L"\0" L"Pa\0"
  L"\0" L"kPa\0" L"\0" L"MPa\0" L"\0" L"GPa\0" L"\0" L"rad\0" L"\0" L"rad/s\0"
  L"\0" L"rad/s^2\0" L"\0" L"ps\0" L"\0" L"ns\0" L"\0" L"us\0" L"\0" L"ms\0"
  L"\0" L"pV\0" L"\0" L"nV\0" L"\0" L"uV\0" L"\0" L"mV\0" L"\0" L"kV\0" L"\0"
  L"MV\0" L"\0" L"pW\0" L"\0" L"nW\0" L"\0" L"uW\0" L"\0" L"mW\0" L"\0"
  L"kW\0" L"\0" L"MW\0" L"\0" L"a.m.\0" L"\0" L"Bq\0" L"\0" L"cc\0" L"\0"
  L"cd\0" L"\0" L"C/kg\0" L"\0" L"Co.\0" L"\0" L"dB\0" L"\0" L"Gy\0" L"\0"
  L"ha\0" L"\0" L"HP\0" L"\0" L"in\0" L"\0" L"KK\0" L"\0" L"KM\0" L"\0"
  L"kt\0" L"\0" L"lm\0" L"\0" L"ln\0" L"\0" L"log\0" L"\0" L"lx\0" L"\0"
  L"mb\0" L"\0" L"mil\0" L"\0" L"mol\0" L"\0" L"PH\0" L"\0" L"p.m.\0" L"\0"
  L"PPM\0" L"\0" L"PR\0" L"\0" L"sr\0" L"\0" L"Sv\0" L"\0" L"Wb\0" L"\0"
  L"ff\0" L"\0" L"fi\0" L"\0" L"fl\0" L"\0" L"ffi\0" L"\0" L"ffl\0" L"\0"
  L"st\0" L"\0" L"+\0" L"\0" L"_\0" L"\0" L"_\0" L"\0" L"_\0" L"\0" L",\0"
  L"\0" L".\0" L"\0" L";\0" L"\0" L":\0" L"\0" L"?\0" L"\0" L"!\0" L"\0"
  L"(\0" L"\0" L")\0" L"\0" L"{\0" L"\0" L"}\0" L"\0" L"#\0" L"\0" L"&\0"
  L"\0" L"*\0" L"\0" L"+\0" L"\0" L"-\0" L"\0" L"<\0" L"\0" L">\0" L"\0"
  L"=\0" L"\0" L"\\\0" L"\0" L"$\0" L"\0" L"%\0" L"\0" L"@\0" L"\0" L"\0"
  L"\0" L"!\0" L"\0" L"\"\0" L"\0" L"#\0" L"\0" L"$\0" L"\0" L"%\0" L"\0"
  L"&\0" L"\0" L"'\0" L"\0" L"(\0" L"\0" L")\0" L"\0" L"*\0" L"\0" L"+\0"
  L"\0" L",\0" L"\0" L"-\0" L"\0" L".\0" L"\0" L"/\0" L"\0" L"0\0" L"\0"
  L"1\0" L"\0" L"2\0" L"\0" L"3\0" L"\0" L"4\0" L"\0" L"5\0" L"\0" L"6\0"
  L"\0" L"7\0" L"\0" L"8\0" L"\0" L"9\0" L"\0" L":\0" L"\0" L";\0" L"\0"
  L"<\0" L"\0" L"=\0" L"\0" L">\0" L"\0" L"?\0" L"\0" L"@\0" L"\0" L"A\0"
  L"\0" L"B\0" L"\0" L"C\0" L"\0" L"D\0" L"\0" L"E\0" L"\0" L"F\0" L"\0"
  L"G\0" L"\0" L"H\0" L"\0" L"I\0" L"\0" L"J\0" L"\0" L"K\0" L"\0" L"L\0"
  L"\0" L"M\0" L"\0" L"N\0" L"\0" L"O\0" L"\0" L"P\0" L"\0" L"Q\0" L"\0"
  L"R\0" L"\0" L"S\0" L"\0" L"T\0" L"\0" L"U\0" L"\0" L"V\0" L"\0" L"W\0"
  L"\0" L"X\0" L"\0" L"Y\0" L"\0" L"Z\0" L"\0" L"[\0" L"\0" L"\\\0" L"\0"
  L"]\0" L"\0" L"^\0" L"\0" L"_\0" L"\0" L"`\0" L"\0" L"a\0" L"\0" L"b\0"
  L"\0" L"c\0" L"\0" L"d\0" L"\0" L"e\0" L"\0" L"f\0" L"\0" L"g\0" L"\0"
  L"h\0" L"\0" L"i\0" L"\0" L"j\0" L"\0" L"k\0" L"\0" L"l\0" L"\0" L"m\0"
  L"\0" L"n\0" L"\0" L"o\0" L"\0" L"p\0" L"\0" L"q\0" L"\0" L"r\0" L"\0"
  L"s\0" L"\0" L"t\0" L"\0" L"u\0" L"\0" L"v\0" L"\0" L"w\0" L"\0" L"x\0"
  L"\0" L"y\0" L"\0" L"z\0" L"\0" L"{\0" L"\0" L"|\0" L"\0" L"}\0" L"\0"
  L"~\0";
