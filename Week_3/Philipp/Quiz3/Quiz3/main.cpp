#include <iostream>
#include <vector>
#include <algorithm>

const char *s6 = "ibfdgaeadiaefgbhbdghhhbgdfgeiccbiehhfcggchgghadhdhagfbahhddgghbdehidbibaeaagaeeigffcebfbaieggabcfbiiedcabfihchdfabifahcbhagccbdfifhghcadfiadeeaheeddddiecaicbgigccageicehfdhdgafaddhffadigfhhcaedcedecafeacbdacgfgfeeibgaiffdehigebhhehiaahfidibccdcdagifgaihacihadecgifihbebffebdfbchbgigeccahgihbcbcaggebaaafgfedbfgagfediddghdgbgehhhifhgcedechahidcbchebheihaadbbbiaiccededchdagfhccfdefigfibifabeiaccghcegfbcghaefifbachebaacbhbfgfddeceababbacgffbagidebeadfihaefefegbghgddbbgddeehgfbhafbccidebgehifafgbghafacgfdccgifdcbbbidfifhdaibgigebigaedeaaiadegfefbhacgddhchgcbgcaeaieiegiffchbgbebgbehbbfcebciiagacaiechdigbgbghefcahgbhfibhedaeeiffebdiabcifgccdefabccdghehfibfiifdaicfedagahhdcbhbicdgibgcedieihcichadgchgbdcdagaihebbabhibcihicadgadfcihdheefbhffiageddhgahaidfdhhdbgciiaciegchiiebfbcbhaeagccfhbfhaddagnfieihghfbaggiffbbfbecgaiiidccdceadbbdfgigibgcgchafccdchgifdeieicbaididhfcfdedbhaadedfageigfdehgcdaecaebebebfcieaecfagfdieaefdiedbcadchabhebgehiidfcgahcdhcdhgchhiiheffiifeegcfdgbdeffhgeghdfhbfbifgidcafbfcd";
const char *s7 = "aabbccddeefghi";
const char *s5 = "glwdyhrgcynwkwwxwnfxysjckddahgyvbydalgnaqrgrfflcdfrboeuepyensmihftjtmrdlqxckcpqkoprvngfsujbdlebpfxpqihwpisgsbyhhmalghnumpetgpgyduilvpvooeitefpdfjxmuropkmjjohvhtbyckqlcpicyutuybjcseingxiyfmalkqaoajsxvskfklghxufjnodqvekqlagihjgtlbowxmukwtespwemdgkiaxtvaimyrdlionrnallvjehlbgexnjvascogiwdqexkqthhaogbrjwcoxthmngebbkonufvesivabyuoymhubstifnfqovluvmlsixdcomarbimjbmtmltmcbusmddouqkaukhlkktdkpttnwlppcjcrwdbwgcxkjytaifywslenyhrcrwvueceqjakotjydmofmwiudvbjavclveqosixrmbnplkispknrifubflrjyamdhfjuedthjeeouyubfuvgvapjqljxxqrqhgcuacvxboncehvlryblcxjfdvsfmwnewampsyeiwlopshadvonskixwjglaafflircnaasrgpxiqsyyinhcgwibrnrehoduycvrtuchmfrhfxyonnojjyxvdscwnjgqcefiyarrogrtefcsdtbuacobptpxdwuqhpkxqsoyiheevibtdhmmsjjjnnvcdkwrwchpqtxnydyautygnvbwtuyadymugwrcdpubxswvyxkulgjvauyxtfgxewvnwsnjlbvxqyfijebrvdsqfpqpxdoeqfmupiwqwetqtqjqtqidldxjxuojjxprflxmapbmstpghejblpnvtauwlygdiomdnnqbsvptclcrsexnaekiqanobhoknftitkkxhrhrutfqgwmirousqnvkguqqamxgkobqglqkdcmvrbhwkrcafslmhogsepwrppmmrsvssqgkgcfbivhehfamwkwwcpkpstitbykkofh";
const char *s4 = "tfgyrknpgngtqgjccbyctwdcymwrcjtpoaflkeoxfxijxkngpjoofggsozftkpgxakptmzjxugavazwllxdtrjrrbjmrqwfxaby";
const char *s3 = "hfchdkkbfifgbgebfaahijchgeeeiagkadjfcbekbdaifchkjfejckbiiihegacfbchdihkgbkbddgaefhkdgccjejjaajgijdkd";
const char *s2 = "jtqgugmcsxvdwidtcyqpogkdifapuloqykjfxruvfrshcehekoiwbpbrprahwvhliglyxynjotbaswnnnmxbkmcftvsdqajemeul";
const char *s1 = "aabbcd";

bool check(const char *s) {
    int c[256] = {0};
    while(*s) c[*s++]++;
    int countCompare1 = 0, countCompare2 = 0;
    for(int i = 0, a = 0, compare1 = 0, compare2 = 0; i < 256; i++) {
        if(countCompare1 > 1 && countCompare2 > 1)
            return false;
        if(c[i] == 0)
            continue;
        if(a == 0) {
            compare1 = c[i];
            countCompare1++;
            a = 1;
        } else if(a == 1) {
            if(c[i] == compare1) {
                countCompare1++;
                continue;
            } else {
                if(c[i] != 1 && std::abs(compare1 - c[i]) > 1)
                    return false;
                compare2 = c[i];
                countCompare2++;
                a = 2;
            }
        } else if(a == 2) {
            if(c[i] == compare1) {
                countCompare1++;
            } else if(c[i] == compare2) {
                countCompare2++;
            } else {
                return false;
            }
        }
    }
    return true;
}

int main(int argc, char **argv) 
{
    char buffer[100000];

    std::cin >> buffer;
    std::cout << (check(buffer) ? "JA" : "NEIN");

    return 0;
}