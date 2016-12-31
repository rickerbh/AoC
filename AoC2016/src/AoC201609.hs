{-# LANGUAGE QuasiQuotes #-}

module AoC201609
  ( runDay,
  ) where

import Str
import Text.Parsec
import Text.Parsec.String

runDay :: IO ()
runDay = do
  let part1Result = execute fullInput
  putStrLn $ "9) Decompressed length is " ++ part1Result ++ "."

data Marker = Marker { countola :: Int, repetitions :: Int } deriving Show

-- Part 1

execute xs =
  case parse decompressingParser "Part 1" xs of
  Prelude.Left msg -> show msg
  Prelude.Right output -> show $ length output

decompressingParser :: Parser String
decompressingParser = do
--  Bit of a problem. I can't figure out a parser that will support starting with either a marker or a string :-(
--  s <- many1 letter
--  marker <- option (Marker 0 0) markerParser
  -- Hacked solution below for the first line - the real data doesn't start with a non-marker
  marker <- markerParser
  r <-  repeatingParser (repetitions marker) (countola marker)
  rest <- option [] decompressingParser
  return $ r ++ rest

markerParser :: Parser Marker
markerParser = do
  char '('
  c <- many1 digit
  char 'x'
  r <- many1 digit
  char ')'
  return $ Marker (read c :: Int) (read r :: Int)

repeatingParser :: Int -> Int -> Parser String
repeatingParser r c = do
  s <- count c anyToken
  return $ concat $ replicate r s

-- Input data

smallInput :: String
smallInput = [str|ADVENT
A(1x5)BC
(3x3)XYZ
A(2x2)BCD(2x2)EFG
(6x1)(1x3)A
X(8x2)(3x3)ABCY
|]

fullInput :: String
fullInput = [str|(6x9)JUORKH(10x13)LNWIKDMACM(126x14)(21x8)QLKUJNVVZIQGGFCJZMPHK(2x1)ZH(59x3)(38x14)KELEPIDYLCGJUBCXACRSOCEZYXLOFJSADZAYXN(8x11)HORSWAQU(21x2)YEZNNYDLDSTGWMQFSMTEZ(111x14)(14x14)IMLQOUGGZSUMFQ(73x10)(5x8)KWHRS(6x1)OQMANE(6x4)FALFZJ(3x3)OAQ(26x15)LALOJKTKJWTMRDDQQVCSDHJYGF(4x12)OXMQ(63x6)(6x10)OSJMJX(9x13)(4x2)FAUQ(29x13)UDCLMPVICKKUFMPQUENGCRIUGHOKM(7x7)JNKRSAT(79x7)(11x1)(5x13)RUQIL(47x12)HDIEMWYDIMTVXSIRZSRMIMZVMCEIUOMJDGHQXPWDFLOYIWL(3x5)IHX(1x6)M(33x3)ORLXEMAAICHAIJUIPKKWPWZMVXJSCZZYG(9x11)TIMKMEBMC(9930x13)(1513x15)(779x2)(333x4)(197x8)(27x4)IVLXTLXGDDQPWQOAXDEISEOTSAW(68x4)(4x13)OOZQ(3x5)NIN(10x12)UDGXNSJXLP(17x6)FZETGJDTLNUQRLHWU(4x13)AIZY(9x10)HESLLDOPY(69x1)(5x13)LULVA(10x1)CBTQWNWMRT(3x11)UYT(4x13)APGU(17x2)ADKQUAPCVNCXGBUGI(8x7)EGCQRTUU(108x11)(30x14)(2x13)LW(16x7)OTSCBRYUGBHSLRHU(2x1)KT(58x4)(3x12)SBW(7x15)IJTOSYM(30x8)MGFBFVKJDCFQHTLVSLZSSRVALHIYUN(25x4)(19x6)(5x2)ULAFN(4x1)LZBJ(60x10)(33x1)(26x14)FXVHHVMIQZBNSJRJWVQQBTLCJT(14x15)MCNCQPHNNPDVLT(326x1)(90x6)(8x9)(2x10)QQ(7x9)(2x9)QA(15x5)(3x12)YYF(1x9)E(2x5)PN(30x10)(16x1)DNOMTDTPGQECFDZR(2x11)PO(83x9)(18x6)(6x6)WCIAVF(2x4)PG(4x10)LMEG(2x9)TM(7x8)CYTGIPV(23x13)(1x9)V(1x15)E(4x14)EFSQ(102x15)(19x4)(6x5)GOXZGT(3x2)VQM(5x12)ZPRXJ(17x1)(10x15)RJNMNYZGHQ(37x2)(11x8)BZHBHSYQFZR(7x3)ISJOGDR(3x8)HEC(25x2)(19x6)(7x7)WKDHBGA(2x1)HH(2x11)TS(297x11)(289x14)(6x13)QERECU(4x3)IKNT(13x7)(7x11)(1x13)C(242x1)(28x4)(22x3)PHZYSAKYKBEGOCKKMRKOOO(12x1)RJDMLFYCFSIP(90x1)(17x2)YMAFXOHNVIPPURYIK(7x15)UYDUWQX(33x9)AIRWLTEJXUBYKOIBBDABIUKOYZXOPEGIW(9x11)RMBVBJFRV(88x9)(8x10)UQXEIUOO(24x2)OJEVOVTOFTKJSBPOOUPDZHGM(5x14)OWMBL(8x13)DKXEFKWI(12x12)WDCXEDVGOCPY(299x2)(239x3)(172x5)(67x7)(5x8)SYGVZ(5x8)DREXA(6x1)FNDUMM(30x2)PMYQAWIEQSOAWKGTANFXJVIPIXAVXK(5x5)ANQRI(83x3)(15x12)TGXTICPXLKWBCIP(13x5)DVMOKEUOBYENI(10x14)MIBPUUQJWN(12x3)GRHQZIDWWGLK(2x7)WR(11x7)(5x15)EIKHL(36x12)(21x4)AJDVITQLAQQOJJOFFKNLS(3x11)EZR(5x2)YHNYY(6x7)YCSGKC(18x13)ZEIUSEMQHQWMZVPLNZ(2x9)NN(79x7)(73x7)(67x3)(60x10)(9x5)TGBRMOCPG(32x2)FDWNZGDQFMFCQUHYSAOQAOTEIUBCDNIW(2x12)JM(24x15)IRWDGUFXLEKCWDNZRVBNRYSS(1280x14)(193x2)(12x4)(6x10)ZHTRGA(155x7)(34x8)(27x12)(5x10)VXVVJ(4x5)PLYR(2x4)YO(10x2)XXATONMJPD(7x5)(1x15)L(72x8)(21x7)(4x13)QGHA(5x15)BYFQB(18x5)(12x9)GHLYVOFFOHII(15x4)XQRPRDSSBORQHKA(3x11)LVM(8x8)IYHEOOSI(575x5)(289x5)(15x3)WUJJUHESVBKMKWI(144x1)(27x8)(21x2)ZWKIBQKYDICTDWZJSMFMB(85x4)(2x11)YJ(9x9)NEIHAQFQS(3x13)FMR(18x7)HTTQTCRJQAOGPSGCEP(24x3)FGXUVDQTTFSBUWGYWIWIVLNH(13x12)(1x3)A(1x13)H(14x9)(9x5)(4x3)SJAA(74x13)(18x6)FKPLHFUKSNVZXJTTYB(21x3)(15x8)LDLRHRRFVCEAEZW(17x8)BUQDCTSPWOMNYXSXQ(10x5)GYOKDVURIQ(10x1)ZSXQJVASBA(244x5)(237x3)(56x1)(2x3)IH(8x5)JPNMZEJU(15x10)SOIGFVVDBCHWLUX(8x11)IMNGAHMV(36x11)(6x6)AVOERJ(1x15)I(11x15)TGRCPFFXZWN(80x2)(2x11)UD(3x4)AJO(19x12)FQOHLQVIFPAEBOCLBOD(7x13)ZLCBKLN(18x12)BOEPSWVDLNKVUFDHKV(39x15)(10x4)AYYJAXAJXB(10x10)EWKJCLSLBE(1x6)C(6x12)EMSRFU(2x8)PX(483x14)(186x8)(178x12)(6x9)VJVAVY(65x1)(1x5)F(2x6)GE(4x13)BUJP(9x14)IMEJXLWER(20x12)HUUEGUTSDWPZHFZKXVFV(59x8)(5x13)VYEFV(25x6)DWBRYFPBHUNYZJUMZXCKFIHIE(1x11)F(5x8)DLUUQ(1x2)K(19x6)PPXCSNESTBPIWHDFLGG(4x10)NHOY(273x3)(18x9)SEWPNVRXHLBCKGESND(31x1)(1x6)J(19x5)SIQBBERDYWEKHMAJEWB(58x1)(43x13)(15x13)BKGAXDZLFQHWDIS(7x7)JWIGQYE(4x9)DHHW(3x6)OHR(140x13)(28x15)(10x12)JOMQKBRMDS(5x13)PUVLJ(44x7)(4x13)DKZR(16x15)NQDHKHTUOJOXBWWY(5x13)BAOCL(34x10)(28x1)JCJWLCJRLTIFXVJQBKHWOTLJFIVI(9x4)YFQNAGIPW(2478x7)(1083x5)(417x15)(117x10)(92x11)(31x1)XHEFNUFJDMXCTUQFXSZMYHFEHXUVGYQ(7x2)ZITTPBY(9x14)SSCNHQYKS(22x5)UVJATRGWLHAIVMQAGJMHEK(4x4)IUEZ(4x5)YKGS(96x14)(12x1)ZELBJAPFUHLZ(72x4)(2x14)NP(6x8)SMHEEP(14x11)HKYNMOLKTUVIXZ(15x1)NSAGJXXSAYRTIUK(5x15)NPRBV(182x9)(76x12)(4x9)LLZF(35x11)DLKHAYYAZXRSZKTEHJNIZQERZANTBHDVFQM(13x6)XPSSVVUTYEUKX(1x2)X(33x14)(2x11)IG(11x8)FPEMVSXFOHQ(3x6)BRW(46x11)(1x3)Q(3x10)ZHT(3x7)VNL(1x7)R(10x12)GZWTUNGVDQ(1x1)Y(120x10)(14x9)PBYCGWOFRQUZWJ(83x6)(17x12)(3x4)EAM(4x6)CJOU(53x6)(13x5)INHCJBTGHXSDE(19x8)CWOKMSSFKYMDMSLRSPA(4x6)XDUP(6x4)LYNJVO(2x7)AD(509x1)(188x4)(74x10)(41x9)FUNFGSCSMVNLSJIJIBJZMBXKRACPLVGYEVJVRXTLF(1x7)O(15x5)ZIFCLBIMIFPOFVA(8x5)(2x10)SA(3x15)SYB(29x12)(2x4)LR(15x13)SUPSUGVKOEIHPAZ(43x9)(18x6)IONNEOWXXVFDIQOMQM(1x14)N(7x7)VICZLXN(253x1)(26x11)(2x15)OF(4x4)EINM(4x7)VPFW(90x2)(2x12)OS(18x15)IHDJBISYOELRBFBMRT(16x5)ZWDVZWPFHTFZRHGM(6x15)RMASVP(17x8)HHEQZPDKXKRYHABWF(64x9)(24x4)XXMFXKEZKTMCOQDOYEXVQWGY(12x14)IXJHAYHVQVBG(9x11)OEOVLDHWO(47x10)(14x6)OQYNLIFVBHCLDV(21x7)GNVWPMWHRPNVRYWBOKXXH(2x1)GE(25x9)(3x3)YHT(10x10)WGUTPDXLCO(10x4)PTGWGFREXV(2x8)CP(569x3)(119x13)(56x1)(50x5)(1x5)T(25x8)QVDBRPILAVKKPNKCWACDVQKAM(8x1)UDZHAVIS(44x10)(32x5)(8x9)WVPZVSEF(12x10)EBBNJTJBZPSH(1x5)I(1x2)M(1x10)K(27x1)(2x4)IV(14x1)(9x8)(4x4)JVUB(112x15)(99x2)(49x3)(1x14)C(1x5)D(2x14)WK(5x15)XXYUI(11x9)KLPOYCKHWFV(14x6)GVHBCOJYAOCJAH(10x3)BATKUDEPLN(3x2)SPJ(2x1)TA(275x3)(41x4)(18x5)(2x12)PT(4x15)MAUM(11x6)(5x15)RFIRF(220x15)(29x11)XWFPDWBZOCIASNAPTYSYUUVQCLCYG(5x7)LJJUN(34x4)(1x1)I(4x3)DPAN(12x12)LFHZRFTTUZLE(46x13)(6x5)FMHJMK(2x7)PZ(9x13)SATIDLMYD(7x10)RXCEUNC(75x9)(5x15)WCKRG(15x3)MYYAJYGJIBOWXOX(29x1)AABPHEDEUZZCJICBHSJZFVGLJCQMX(3x1)UNW(803x15)(441x13)(65x6)(59x2)(8x4)VCYBDQQS(15x9)MHGZPTDZETHSKLL(7x2)NMOFOIS(7x14)KUAWWUP(15x8)GNNUKIVQVUVZULA(161x1)(87x6)(4x4)BHUM(1x14)W(40x2)HZRMQLYLMDMWKBSXEAOHCJVANHFGTEAPXAALYUNE(18x14)MIGSNNUDEKHXIVPAOP(62x6)(1x13)B(12x8)SJKLEEFTVPLM(4x3)WONT(5x9)TAESF(12x5)AQTLVIYDFKZV(53x9)SWGZYFRQBSYVZRKBUETKECZABCQOBKWTLUXIIRCYXIYYCIZPWXYLJ(114x13)(65x12)(33x12)FYYFQQIQMKTPWLRATCIXJWQWZHDZEMDSX(19x9)RDXSFWGCVHIAEFRATQB(8x4)SMHSAZUP(22x14)(1x4)O(2x9)TL(3x10)JWD(325x11)(107x6)(80x10)(1x9)I(7x14)VRZNXOX(17x5)SXTYFBITZSWXLUUFJ(11x3)LOCMMDXNMXN(15x6)ESLRVLYBOQKPEZA(2x3)ES(7x11)EDZPYNR(69x15)(8x15)HATTKOTD(1x15)C(1x14)O(35x1)(2x1)CW(21x13)MQITCFLIILUSRLVQGSKXW(121x6)(12x15)(6x13)HZQFNF(9x9)UIDOFTGVI(57x8)(15x14)DJHSOBMGQLFKFTQ(11x6)QWKTRHFEAGK(2x4)IH(6x5)ZHDHJX(10x15)ACPQGUGWIL(3x2)QMK(2x3)OT(15x6)NZGYKDPCNTWTZCH(3552x13)(852x7)(284x12)(277x1)(8x11)TGYIXQNL(40x13)(17x9)LWDCCCCWVOGYOWYZP(3x8)ZVN(3x10)SDQ(3x3)DWU(123x2)(10x14)NSZEKFDZRL(26x2)ENELBBZQLDXFNGFSVGYFTSJPBP(19x5)STCNNNFTTGNFSHLQIMV(7x14)BMGHAMA(30x9)LWDBIXXPHDSHEHLGUWINWVVCSQLNYK(72x1)(5x3)SMBBE(1x15)U(4x3)XLJH(12x6)LYEVOLDUQOII(22x3)UYBOYDZYRBWIHVBZJZJIBD(10x15)ORGFBMUTUK(535x12)(100x8)(6x15)PJFDCS(29x7)(4x2)RERM(4x1)BFHS(6x7)GNJACX(13x6)THUMFYQLTLTCJ(28x8)RTDQHJQHMBOOPZUEWWKMSANXGGQL(56x7)(50x4)(4x2)EUXN(3x12)LWU(2x13)YV(12x8)PXZISAYRVHIE(1x4)L(77x13)(70x12)(24x14)XLKAJZMEOBGJQOZFGDAARDDF(4x13)HMEZ(22x13)TRAZZTZVHZFJQBCVPUERWP(116x7)(8x13)FQQDJSSI(21x4)(3x3)GUX(1x12)J(1x7)F(26x14)(12x6)JVDQPKZGKVQJ(2x13)DV(36x7)(10x4)HVKCDIWLKC(8x5)LGTFQODZ(1x12)L(152x5)(4x6)CURR(80x1)(17x15)GUVFWMLWUJEGNLRVL(6x7)NJXRSS(7x14)AKRRJPI(8x5)DQVRMWFL(12x15)UMLQPEZXZYIZ(10x10)FTKQEWMORC(25x8)(4x1)NPOF(2x4)ZB(4x5)MNGN(4x1)SVFL(819x3)(422x1)(161x5)(63x11)(10x9)LMWKXPVXUD(12x11)NVMYGSXJQBVT(2x12)XS(14x1)LCJFACSGQHOPUG(44x6)(31x9)IYNFDKSDSHMXTGMRIOPAQJRHQBFEKDH(2x4)SX(14x3)HIZENUSZORNUXE(7x2)(1x12)C(3x10)LQD(246x14)(79x13)(32x14)FBUFQJMOZAUHZPGKGNZOTHLBIYETINMF(13x1)GKCLXCMWWPDZV(15x7)MNXJKTCLCEEKJCJ(39x4)(14x14)QYBWZMQBFIWBYQ(12x6)VHQHHLDLMOBZ(93x2)(40x1)TYXOJHUCXHXFBLOHLFUUDAHEWHQJFIUZMAGFRGWP(2x7)MS(9x13)VVJYBKHTL(19x4)IEBQYXDIHQGRMTODBIF(5x6)GWEDR(1x3)M(345x13)(185x7)(8x14)ZXFOFWFV(9x8)EZMSOIYCO(61x13)(33x11)OFPSLJWRGPSLTALOWBSVBFAAYMKCPMYTY(9x6)IUDMWEHGP(1x10)H(6x9)QPVXCO(71x11)(1x11)Y(5x14)PSMCR(7x2)ZAZQZSB(24x14)DCIFQYMEDXSNYMDBSIPOWQUL(5x5)OWYAX(4x5)ISZB(7x3)HWLAEOE(125x9)(45x13)(21x11)YRXZKZQVBPNYBKZIFSYMH(2x13)EL(3x14)HXM(6x15)YQWWDS(5x10)VWCRE(44x7)(8x6)ONMUMKWM(24x15)OBREAALATPCTXPSADMXUXKDK(31x8)(25x8)(18x15)(1x6)K(6x15)WFSHOE(1064x8)(180x12)(91x7)(2x9)UQ(77x10)(14x9)KQHDSHPDIDEWFX(7x13)DBZDGLP(17x1)PEQPQFLLLGAZMPJLK(5x8)DTSJE(5x10)JGHKO(16x10)(2x7)LP(3x11)PDI(40x10)(1x7)Y(27x13)IXDUDNKRJKLBBKIVORLOGMGJWCQ(8x6)(3x2)RTB(323x5)(79x9)(44x13)(19x1)VGWKUHADQZUENXNLNGP(12x10)HCFBPZQRWQEQ(3x10)PLL(12x13)(7x2)GVCTUOT(118x1)(57x15)(13x12)XQESMYXNDFYYW(1x8)G(3x6)XFU(17x8)BHBGTSSJBGGFRPPRT(48x7)(10x1)BLQHPNBCVO(8x13)QAYCKEPP(11x14)FBTYJKKUQCA(4x4)JNWW(97x11)(33x3)WYMAFQHRKESJTBRMNMOARAUYBABCIIZHH(11x8)DTIJCCBXCHK(4x2)ZLMO(25x11)(1x3)L(2x6)EN(7x1)FBJYMYH(11x4)GRWQQFIQDAL(89x9)(83x7)(23x14)QSLZVMSWOGEMUVQUMMBBXZS(33x5)(7x11)ZCEHLWD(14x2)BSRRFIINKZJUTM(9x3)XBPUGZIBI(426x11)(144x11)(10x2)ZVQMPUEXDS(7x14)(2x3)NP(8x8)(2x10)WY(31x4)(8x14)MQXSEJDJ(2x7)FW(4x13)XEQJ(59x3)(17x11)MTJKGUKCQPWDUSFMV(3x6)VDL(8x6)OHZZPPZE(8x12)JMJZCIBE(267x9)(6x7)QQULTD(109x3)(32x9)KHIFGECKKBYSJQUPZYCSWRGOISZBQZMP(5x11)WWVRO(53x13)RNKMDBXJEJEYGREGYLVFXJAGXKPLZGSBEGCLRHUBWMMIMMSEJLKVS(20x10)(1x12)Q(1x4)J(1x10)T(44x7)(3x1)HQS(2x14)OY(4x3)NYOT(3x11)HRB(5x3)AWNPF(57x4)(51x4)NRDWDRLNIEMNTPTKPXWYVRZVJXCEPIWDOZHHSPWTINYYPNKRIJM(788x7)(373x2)(13x13)OMNINNXSMZGVU(110x10)(9x4)MUCJLTXXY(23x8)(16x10)QPEZTMHGTAZMDZFT(4x12)QARM(50x15)(3x11)XOH(16x6)YBLZLQWJABJGQWOU(4x1)PMXY(4x15)OHDQ(118x4)(5x11)EPKSP(24x4)AWIBAENYEFRILVCXFWNDTDKD(1x4)B(65x2)(12x2)YLEUJZAKLKME(24x13)TTXHIHEKQRZMJJYYKWQLCBJP(10x2)FCVCPOTJEU(102x12)(76x14)(7x12)UZQCZAB(3x1)XMW(9x1)UJSGSEURM(11x13)BOKCOFUHRNM(17x1)AEMFGQNASPGQHPXUB(1x13)P(6x11)CVFTDM(166x1)(74x3)(3x12)ONA(36x9)(29x12)AGQWRIAFZDTLKFAWUBZVULOQIYWPP(17x9)AKFFQIWNYGWXFBZZN(71x7)(31x2)XOIACTRUJMFXAVRZUMXKBXZFREZNVJJ(27x15)(20x12)EWQHGQNTXRUIOVHORPOV(3x12)TVU(117x1)(109x14)(10x12)BEIZCDNSAS(86x7)(27x9)FURZRGYPYLGCCCEKYZJPGYYDBQQ(5x2)SBYXR(25x4)ZXXGJHSMUOPRERLXZXQJLECBO(6x15)JUOYIW(91x3)(23x14)MGGWDYYRPCJTIYELCDIZVHS(2x12)KD(6x3)BCDBJV(19x3)(13x4)RXIRDQSOWIPGW(11x1)QGAYMDNYTSL(8x11)FJQAMNNG(1063x13)(250x5)(60x10)(24x11)(8x12)BJYJZMJY(5x8)BJXMU(7x14)NZTAXLS(4x8)VBAT(2x3)HO(176x5)(169x4)(13x15)TWSLJCGYWHLHA(73x6)(36x12)AOUGLFXLSSEWZXFMZUNGDGZVCYYJZUPNKRIH(3x3)UFP(3x1)QIQ(8x15)NPNWNAWS(50x15)(3x6)YIW(1x10)D(10x14)FCLZEIFJWG(1x12)N(6x8)AZDFPX(7x13)DICRGNA(799x2)(533x11)(276x6)(100x9)(17x5)LPMZDSXVBZCMEDFQB(24x8)TCDTFHHXXVEQIVVZCESDANQS(10x15)EPDXGVSSBQ(7x13)WQSTXRF(10x11)RIBWTNVIOX(3x15)VSF(61x6)(8x15)TWCPLZZZ(8x9)AVRPJLHR(21x9)QDODHNZHZERQGXTVKLHYW(1x13)U(53x11)(2x15)RC(32x12)CWBFTTDPKPGPZVZFIJHVLZRCOPJRNFOJ(1x1)I(26x11)CDUFFOGUBYLOBYJNLCWGPDRSMS(201x12)(8x5)(3x1)YIC(8x10)BDXPEMTD(64x4)(10x1)RBHVYIUNFH(5x14)KYIHZ(14x2)TGSOMUZWRGPUNE(11x3)QJHVUCUELOW(8x14)VKMCUJKE(83x11)(1x11)L(5x12)WLWRL(4x12)NQVE(36x3)FJDUVTWHNITJXXVVUMFUDALUXKSIPQFJOEDT(8x1)GDWDODFR(35x2)(22x13)(4x14)OSLD(7x4)VWJVMIF(1x6)H(10x6)VDQYBSDXHG(149x10)(16x14)DROZRQNSVIJWCMPZ(67x6)(54x6)NDDMSLQZBUOSOSAFMDFQGMQBMSGREAKGVTMWIRQPTESMEACCZIOGQH(1x15)J(8x10)CFHPKNID(24x1)(18x2)EMDBIEKPGRSNYXDATP(4x4)HSHR(79x9)(51x1)(2x12)BB(6x15)LNUMRD(25x8)(10x6)FBWLKYGCNJ(3x13)XRN(16x7)(3x9)ONH(2x10)HS(47x1)(2x2)MS(34x9)VLUZLSWIOQZSOBREZDZXEQYKYWTNHHWRNZ(9x6)(4x4)WMMX(78x9)(17x7)(1x10)J(4x14)WZCZ(49x9)(3x5)IPZ(35x7)ROJEZWTCDQCBOWTBEEJCVWBPNMBNXJDJZMG(183x15)(3x6)KRR(54x13)(3x11)JXV(4x7)WRTT(1x6)Q(16x15)LLQNPHNXKWRMAGKX(2x1)ID(21x15)UGXWFMKPKPUMLJCQDBWGS(4x1)NZTP(71x2)(21x7)PKYTHOAENWACXNCPHTTTU(14x12)ZFQGENBKFBMONH(1x7)G(10x10)VSHYAMJWEV(33x7)(4x6)LAZL(12x9)KIDULBTPZHYR(1x4)U(143x8)(29x3)(22x13)LQNQIWKSHNSMJZKWJQWDQA(79x10)(1x8)V(35x12)BTLWYXERIMYWZOFPFAVAAZOFQAHKQXAPOWO(24x13)RLAORQSPJCCOLZLXDAAOXYHY(16x1)MZLJXILHCLGHOAPW(12x3)(7x1)ELWCOIZ|]
