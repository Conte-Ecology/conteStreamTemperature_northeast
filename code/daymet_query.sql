
COPY(SELECT featureid, date_part('year', date) as year, date, tmax, tmin, prcp, dayl, srad, swe FROM daymet WHERE featureid IN (730259, 730482, 730531, 730650, 730675, 730733, 730764, 730849, 730912, 730916, 730933, 731120, 731587, 731705, 731788, 731959, 731967, 732022, 732106, 732107, 732173, 732185, 732203, 732205, 732216, 732310, 732350, 732379, 732418, 732487, 732589, 732645, 732666, 732932, 732934, 732951, 732963, 733000, 733114, 733132, 733144, 733173, 733187, 733234, 733251, 733379, 733394, 733406, 733408, 733433, 733500, 733557, 733604, 733619, 733621, 733622, 733648, 733699, 733761, 733802, 733852, 733930, 733970, 734147, 734220, 734280, 734287, 734638, 735115, 735232, 735742, 735938, 735939, 736064, 736073, 736168, 736187, 736350, 736389, 736542, 736543, 736578, 736605, 736638, 736689, 736705, 736717, 736720, 736738, 736762, 736777, 736778, 736779, 736799, 736822, 736830, 736831, 736965, 736968, 736972, 736973, 737035, 737152, 737381, 737473, 739187, 739675, 739706, 740020, 740152, 740393, 740783, 741447, 741575, 741656, 741665, 741673, 741720, 741866, 741927, 743348, 743602, 743806, 743807, 743914, 743989, 744041, 744276, 744350, 744434, 744485, 744625, 744679, 744840, 744846, 745000, 745066, 745172, 745247, 745249, 745702, 745714, 745800, 745808, 745811, 745847, 745871, 746011, 746098, 746116, 746203, 746229, 746289, 746333, 746430, 746447, 746502, 746516, 746533, 746598, 746630, 746652, 746669, 746724, 746755, 746855, 746982, 747070, 747074, 747100, 747144, 747252, 747355, 747375, 747433, 747477, 747525, 747527, 747579, 747594, 747621, 747635, 747638, 747683, 747747, 747751, 747927, 747933, 747964, 748018, 748045, 748072, 748236, 748279, 748336, 748349, 748443, 748521, 748568, 748691, 748692, 748738, 748864, 749015, 749073, 749089, 749143, 749255, 749292, 749313, 749389, 749491, 749570, 749688, 749707, 749865, 749887, 749980, 750068, 750106, 750190, 750240, 750302, 750335, 750345, 750357, 750428, 750687, 750688, 750708, 750755, 750767, 750893, 750978, 750991, 751053, 751071, 751072, 751085, 751146, 751150, 751182, 751183, 751209, 751229, 751326, 751329, 751362, 751397, 751448, 751507, 751525, 751639, 751681, 751741, 751771, 751802, 751879, 751889, 751903, 751928, 751988, 752130, 752230, 752235, 752354, 752650, 752674, 752707, 752823, 752852, 752925, 753110, 753177, 753240, 753243, 753272, 753315, 753345, 753350, 753616, 753655, 753793, 753812, 753835, 754048, 754090, 754099, 754150, 754180, 754243, 754366, 754405, 754439, 754453, 754485, 754765, 754934, 754986, 754997, 755411, 755414, 755429, 755471, 755525, 755534, 755614, 755688, 755741, 755914, 756377, 756449, 756671, 756672, 756994, 757022, 757079, 757217, 757438, 757632, 757711, 757865, 757867, 757905, 758028, 758078, 758177, 758237, 758243, 758307, 758336, 758366, 758438, 758643, 758751, 758789, 758995, 759074, 759108, 759119, 759168, 759261, 759469, 759607, 759658, 759691, 759901, 759986, 760081, 760266, 760270, 760327, 760347, 760389, 760410, 760412, 760481, 760501, 760505, 760523, 760688, 760711, 760724, 760881, 761016, 761056, 761057, 761076, 761091, 761102, 761339, 761369, 761370, 761438, 761485, 761622, 761705, 761751, 761789, 761964, 762097, 762114, 762115, 762159, 762166, 762191, 762282, 762311, 762359, 762462, 762486, 762557, 762599, 762718, 762745, 762746, 762768, 762827, 762835, 762926, 762962, 763072, 763130, 763170, 763228, 763236, 763396, 763419, 763446, 763479, 763532, 763602, 763762, 763765, 763805, 763843, 763885, 763969, 764115, 764163, 764294, 764438, 764573, 764601, 764725, 764886, 765091, 765103, 765114, 765115, 765146, 765229, 765258, 765315, 765416, 765452, 765570, 765593, 765648, 765650, 765766, 765964, 766018, 766049, 766061, 766439, 766444, 766459, 766460, 766495, 766651, 766800, 766926, 766933, 766963, 766965, 766966, 767038, 767147, 767265, 767352, 767353, 767468, 767580, 767583, 767597, 767598, 767609, 767948, 768105, 768150, 768363, 769083, 769532, 769614, 769685, 769881, 770003, 770177, 770204, 770279, 770304, 770354, 770367, 770773, 770781, 770823, 770973, 770987, 771797, 771844, 771872, 771986, 771987, 772315, 772371, 772495, 772522, 772634, 772643, 772752, 772792, 772873, 772898, 772937, 772959, 773096, 773097, 773144, 773317, 773377, 773534, 773538, 773683, 773776, 773960, 773974, 774023, 774066, 774148, 774183, 774212, 774231, 774232, 774318, 774357, 774459, 774511, 774539, 774549, 774629, 774716, 774728, 774789, 775019, 775064, 775769, 775854, 776480, 776793, 777209, 777246, 777365, 777372, 777502, 777536, 777739, 777914, 777969, 778176, 778252, 778674, 778852, 779254, 779539, 779540, 779612, 779678, 779821, 780238, 780800, 780951, 780981, 781053, 781490, 782086, 782370, 782612, 783567, 783592, 784009, 784087, 784216, 784456, 784642, 784721, 784769, 784781, 784822, 784827, 785114, 785144, 785252, 785337, 785415, 785571, 785628, 785797, 785839, 785869, 785890, 785891, 785953, 785986, 786037, 786203, 786309, 786455, 786548, 786588, 786707, 786764, 786821, 786846, 786978, 787015, 787066, 787194, 787246, 787378, 787379, 787614, 787861, 788038, 788691, 788778, 789052, 789706, 790333, 790485, 790518, 791110, 791121, 791649, 791689, 791909, 792006, 792010, 792011, 792982, 793090, 793304, 793924, 794697, 794908, 795007, 795183, 795211, 795502, 795690, 795741, 795928, 795941, 796072, 796687, 797130, 797174, 797645, 797741, 797880, 797929, 798689, 798885, 799898, 800049, 800110, 800331, 800340, 801132, 801377, 801602, 802053, 802530, 802738, 803624, 803658, 804171, 804568, 805455, 806220, 807082, 807125, 807659, 807792, 807825, 808322, 808540, 809179, 809408, 809517, 809788, 809933, 810037, 810363, 811181, 811392, 811418, 811446, 811715, 811884, 812039, 812372, 812403, 812717, 813035, 813109, 813632, 813765, 814009, 814460, 814571, 814801, 815619, 815653, 816132, 816457, 816590, 816747, 817115, 817228, 817375, 817498, 817714, 817886, 818138, 818552, 818739, 818847, 818865, 818995, 819404, 819420, 819838, 819869, 819969, 820386, 820519, 820570, 820786, 820907, 821005, 821232, 821590, 821646, 821687, 822156, 822347, 822376, 822378, 822962, 822994, 823054, 823105, 823248, 823281, 823447, 823803, 823950, 823965, 824195, 824368, 824708, 824935, 824972, 825012, 825153, 825310, 825347, 825361, 825741, 826053, 826325, 826326, 826371, 826628, 826800, 826941, 826973, 827041, 827053, 827155, 827206, 827384, 827456, 827625, 827760, 827984, 828004, 828105, 828220, 828249, 828435, 828444, 828543, 828619, 828634, 828738, 828835, 828972, 829095, 829145, 829273, 829293, 829435, 829471, 829474, 829586, 829634, 829941, 829948, 830038, 830189, 830232, 830344, 830482, 830598, 830633, 830736, 830769, 830778, 830802, 830830, 831204, 831211, 831270, 831293, 831339, 831406, 831456, 831556, 831586, 831625, 831699, 831777, 831927, 831928, 831967, 832450, 832460, 832532, 832740, 832872, 833001, 833118, 833141, 833142, 833158, 833192, 833244, 833329, 833354, 833368, 833458, 833493, 833674, 833879, 833880, 833910, 833922, 833923, 834069, 834122, 834183, 834215, 834362, 834385, 834423, 834432, 834443, 834454, 834456, 834472, 834533, 834585, 834768, 834816, 834836, 834868, 834912, 834943, 835015, 835639, 835730, 836195, 836217, 836306, 836315, 836522, 836579, 836683, 836698, 836717, 836778, 836877, 837021, 837476, 837596, 837610, 837692, 837836, 838152, 838187, 838377, 838430, 838431, 838486, 838495, 838630, 838757, 838932, 839306, 839353, 839453, 839631, 840136, 840199, 841368, 841445, 841984, 842017, 842037, 842041, 842042, 842197, 842322, 842405, 842601, 842681, 842700, 842739, 842827, 842926, 842950, 843440, 843484, 844034, 844127, 844333, 844474, 844517, 844720, 844862, 844993, 845232, 845245, 845329, 845441, 845691, 845700, 845784, 845935, 846145, 846189, 846395, 846460, 846512, 846578, 847418, 847590, 847662, 847717, 847872, 847984, 848103, 848112, 848226, 848385, 848387, 848456, 848561, 848605, 848611, 848612, 848671, 848898, 849006, 849372, 849419, 849486, 849561, 849671, 849672, 849673, 849674, 849816, 849854, 849889, 850133, 850221, 850258, 850291, 850348, 850363, 850394, 850514, 850547, 850548, 850557, 850788, 850789, 850953, 850960, 851027, 851178, 851243, 851270, 851379, 851380, 851433, 851490, 851580, 851620, 851629, 851647, 851773, 851830, 851839, 851886, 851902, 851943, 852052, 852112, 852121, 852225, 852265, 852333, 852334, 852542, 852547, 852565, 852620, 852680, 852742, 852744, 852766, 852788, 852789, 852790, 852885, 852898, 852900, 852901, 852906, 852948, 853015, 853058, 853196, 853197, 853228, 853431, 853439, 853489, 853566, 853596, 853597, 853633, 853634, 853662, 853673, 853688, 853701, 853714, 853716, 853782, 853850, 853887, 854107, 854116, 854117, 854142, 854242, 854277, 854290, 854358, 854375, 854457, 854563, 854660, 854712, 854746, 854775, 854872, 854934, 854962, 854963, 854978, 855059, 855360, 855397, 855416, 855774, 855803, 855921, 855950, 856152, 856153, 856327, 856330, 856331, 856393, 856459, 856531, 856556, 856671, 856683, 856745, 856760, 856769, 856830, 856843, 856845, 856854, 856902, 856922, 856959, 856970, 857038, 857041, 857050, 857070, 857101, 857102, 857143, 857144, 857157, 857213, 857521, 857607, 857643, 857685, 857715, 857737, 857800, 857847, 857911, 857970, 857994, 858022, 858245, 858260, 858271, 858282, 858293, 858398, 858427, 858437, 858468, 858534, 858535, 858652, 858738, 858829, 858838, 858963, 858991, 859045, 859046, 859072, 859180, 859194, 859267, 859298, 859392, 859406, 859420, 859432, 859479, 859493, 859502, 859522, 859527, 859701, 859720, 859975, 860037, 860090, 860132, 860219, 860236, 860250, 860311, 860349, 860389, 860442, 860493, 860511, 860581, 860727, 860758, 860838, 860961, 860984, 861265, 861266, 861334, 861335, 861360, 861466, 861485, 861527, 861533, 861535, 861617, 861692, 861716, 861766, 861796, 861867, 861941, 861959, 861991, 862094, 862107, 862182, 862632, 862647, 862651, 862669, 862758, 862781, 862795, 862942, 863179, 863197, 863457, 863472, 863686, 863688, 863827, 863842, 863945, 864075, 864115, 864116, 864218, 864287, 864304, 864434, 864562, 864614, 864657, 864745, 864748, 864857, 864888, 864897, 864918, 865008, 865138, 865140, 865168, 865287, 865376, 865429, 865537, 865574, 865597, 865606, 865946, 865947, 866017, 866032, 866074, 866171, 866249, 866266, 866313, 866385, 866496, 866590, 866620, 866653, 866952, 866966, 867081, 867095, 867110, 867133, 867202, 867316, 867317, 867344, 867367, 867369, 867375, 867402, 867429, 867462, 867504, 867506, 867568, 867694, 867850, 867986, 867996, 868009, 868132, 868133, 868190, 868245, 868311, 868356, 868358, 868391, 868404, 868430, 868481, 868484, 868577, 868578, 868624, 868632, 868633, 868688, 868721, 868750, 868762, 868812, 868813, 868829, 868842, 868864, 868877, 868878, 868999, 869004, 869070, 869113, 869149, 869283, 869305, 869321, 869358, 869367, 869492, 869552, 869626, 869642, 869677, 869680, 869752, 869759, 869858, 869882, 869893, 869960, 869969, 869970, 869975, 869987, 870054, 870377, 870414, 870459, 870521, 870529, 870605, 870667, 870681, 870702, 870733, 870805, 870936, 870984, 871038, 871091, 871113, 871202, 871215, 871301, 871337, 871348, 871368, 871380, 871444, 871470, 871487, 871499, 871558, 871632, 871815, 871816, 871934, 871935, 871966, 872010, 872146, 872184, 872243, 872287, 872408, 872431, 872541, 872630, 872658, 872708, 872709, 872715, 872755, 872811, 872819, 872820, 873024, 873028, 873104, 873174, 873275, 873321, 873331, 873362, 873384, 873403, 873502, 873527, 873556, 873557, 873573, 873639, 873679, 873749, 873781, 873819, 873841, 873843, 873944, 874029, 874051, 874088, 874108, 874137, 874154, 874212, 874271, 874272, 874301, 874681, 874693, 874728, 874762, 874774, 874879, 874892, 874901, 874946, 874992, 875028, 875057, 875124, 875141, 875161, 875231, 875288, 875401, 875402, 875403, 875459, 875566, 875730, 875739, 875784, 875837, 875906, 875948, 875981, 875995, 876019, 876344, 876384, 876386, 876517, 876518, 876543, 876547, 876548, 876580, 876611, 876699, 876772, 876820, 876874, 877054, 877068, 877163, 877173, 877191, 877288, 877297, 877490, 877511, 877656, 877664, 877747, 877810, 877852, 877854, 878145, 878204, 878228, 878330, 878370, 878470, 878535, 878540, 878656, 878730, 878731, 878905, 878991, 879168, 879373, 879374, 879456, 879490, 879493, 879633, 879820, 879834, 879950, 879957, 879968, 880162, 880202, 880237, 880289, 880318, 880319, 880364, 880404, 880434, 880470, 880471, 880550, 880595, 880613, 880652, 880653, 880695, 880719, 880754, 880780, 880891, 880970, 881067, 881111, 881113, 881114, 881188, 881233, 881274, 881512, 881597, 881705, 881777, 881839, 881858, 882024, 882041, 882062, 882074, 882143, 882856, 882931, 883040, 883456, 883569, 884790, 885108, 885165, 885166, 885167, 885312, 885615, 885701, 885887, 886376, 886396, 886517, 886653, 886759, 886906, 886940, 887102, 887104, 887127, 887344, 887601, 887669, 887983, 888204, 888333, 888387, 888741, 888917, 888941, 889298, 889899, 890011, 890068, 890289, 890300, 890420, 890432, 890458, 890867, 891224, 891437, 891608, 892358, 892941, 894592, 895539, NA) AND year IN (2004, 2009, 2014, 2012, 2010, 2013, 2007, 2011, 2006, 2008, 2002, 1998, 2001, 1995, 2003, 1999, 2000, 2005, 1991, 1992, 1994, 1996, 1997, 1993, 2015) GROUP BY featureid) TO STDOUT CSV HEADER;
