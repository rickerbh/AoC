(ns aoc2017.core
  (:require [aoc2017.day-1 :as day-1]
            [aoc2017.day-2 :as day-2]))

(def day1
  (let [input 818275977931166178424892653779931342156567268946849597948944469863818248114327524824136924486891794739281668741616818614613222585132742386168687517939432911753846817997473555693821316918473474459788714917665794336753628836231159578734813485687247273288926216976992516314415836985611354682821892793983922755395577592859959966574329787693934242233159947846757279523939217844194346599494858459582798326799512571365294673978955928416955127211624234143497546729348687844317864243859238665326784414349618985832259224761857371389133635711819476969854584123589566163491796442167815899539788237118339218699137497532932492226948892362554937381497389469981346971998271644362944839883953967698665427314592438958181697639594631142991156327257413186621923369632466918836951277519421695264986942261781256412377711245825379412978876134267384793694756732246799739464721215446477972737883445615664755923441441781128933369585655925615257548499628878242122434979197969569971961379367756499884537433839217835728263798431874654317137955175565253555735968376115749641527957935691487965161211853476747758982854811367422656321836839326818976668191525884763294465366151349347633968321457954152621175837754723675485348339261288195865348545793575843874731785852718281311481217515834822185477982342271937155479432673815629144664144538221768992733498856934255518875381672342521819499939835919827166318715849161715775427981485233467222586764392783699273452228728667175488552924399518855743923659815483988899924199449721321589476864161778841352853573584489497263216627369841455165476954483715112127465311353411346132671561568444626828453687183385215975319858714144975174516356117245993696521941589168394574287785233685284294357548156487538175462176268162852746996633977948755296869616778577327951858348313582783675149343562362974553976147259225311183729415381527435926224781181987111454447371894645359797229493458443522549386769845742557644349554641538488252581267341635761715674381775778868374988451463624332123361576518411234438681171864923916896987836734129295354684962897616358722633724198278552339794629939574841672355699222747886785616814449297817352118452284785694551841431869545321438468118
        part-1-solution (day-1/solve input)
        part-2-solution (day-1/solve-part-2 input)]
    (println "Day 1, part 1 solution: " part-1-solution)
    (println "Day 1, part 2 solution: " part-2-solution)))

(def day2
  (let [input "1919	2959	82	507	3219	239	3494	1440	3107	259	3544	683	207	562	276	2963
587	878	229	2465	2575	1367	2017	154	152	157	2420	2480	138	2512	2605	876
744	6916	1853	1044	2831	4797	213	4874	187	6051	6086	7768	5571	6203	247	285
1210	1207	1130	116	1141	563	1056	155	227	1085	697	735	192	1236	1065	156
682	883	187	307	269	673	290	693	199	132	505	206	231	200	760	612
1520	95	1664	1256	685	1446	253	88	92	313	754	1402	734	716	342	107
146	1169	159	3045	163	3192	1543	312	161	3504	3346	3231	771	3430	3355	3537
177	2129	3507	3635	2588	3735	3130	980	324	266	1130	3753	175	229	517	3893
4532	164	191	5169	4960	3349	3784	3130	5348	5036	2110	151	5356	193	1380	3580
2544	3199	3284	3009	3400	953	3344	3513	102	1532	161	143	2172	2845	136	2092
194	5189	3610	4019	210	256	5178	4485	5815	5329	5457	248	5204	4863	5880	3754
3140	4431	4534	4782	3043	209	216	5209	174	161	3313	5046	1160	160	4036	111
2533	140	4383	1581	139	141	2151	2104	2753	4524	4712	866	3338	2189	116	4677
1240	45	254	1008	1186	306	633	1232	1457	808	248	1166	775	1418	1175	287
851	132	939	1563	539	1351	1147	117	1484	100	123	490	152	798	1476	543
1158	2832	697	113	121	397	1508	118	2181	2122	809	2917	134	2824	3154	2791"
        part-1-solution (day-2/solve-part-1 input)
        part-2-solution (day-2/solve-part-2 input)]
    (println "Day 2, part 1 solution: " part-1-solution)
    (println "Day 2, part 2 solution: " part-2-solution)))
