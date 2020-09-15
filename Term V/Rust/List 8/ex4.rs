fn main() {
    println!("Hello, world!");
}

use std::collections::HashMap;

// Preloaded:
//
// struct MorseDecoder {
//     morse_code: HashMap<String, String>,
// }
//
// MorseDecoder::new() populates the morse_code map, e.g. ".-" -> "A".

struct MorseDecoder {
    morse_code: HashMap<String, String>
}

impl MorseDecoder {
    fn new() -> MorseDecoder {
        let mut morse_code: HashMap<String, String> = HashMap::new();
        morse_code.insert("--.-".to_string(), "Q".to_string());
morse_code.insert("....-".to_string(), "4".to_string());
morse_code.insert("-..".to_string(), "D".to_string());
morse_code.insert(".--.-.".to_string(), "@".to_string());
morse_code.insert(".--.".to_string(), "P".to_string());
morse_code.insert("-.--".to_string(), "Y".to_string());
morse_code.insert(".-".to_string(), "A".to_string());
morse_code.insert(".--".to_string(), "W".to_string());
morse_code.insert(".-.".to_string(), "R".to_string());
morse_code.insert("..-.".to_string(), "F".to_string());
morse_code.insert("---...".to_string(), ",".to_string());
morse_code.insert("-.-".to_string(), "K".to_string());
morse_code.insert("-....-".to_string(), "-".to_string());
morse_code.insert(".-.-.".to_string(), "+".to_string());
morse_code.insert("----.".to_string(), "9".to_string());
morse_code.insert("...-..-".to_string(), "$".to_string());
morse_code.insert("...--".to_string(), "3".to_string());
morse_code.insert("..--.-".to_string(), "_".to_string());
morse_code.insert("-..-".to_string(), "X".to_string());
morse_code.insert("-..-.".to_string(), "/".to_string());
morse_code.insert(".---".to_string(), "J".to_string());
morse_code.insert("..---".to_string(), "2".to_string());
morse_code.insert("-".to_string(), "T".to_string());
morse_code.insert("-....".to_string(), "6".to_string());
morse_code.insert("--..--".to_string(), ",".to_string());
morse_code.insert(".".to_string(), "E".to_string());
morse_code.insert("--.".to_string(), "G".to_string());
morse_code.insert("....".to_string(), "H".to_string());
morse_code.insert("-.-.".to_string(), "C".to_string());
morse_code.insert("---".to_string(), "O".to_string());
morse_code.insert("...".to_string(), "S".to_string());
morse_code.insert(".----".to_string(), "1".to_string());
morse_code.insert("-.--.-".to_string(), ")".to_string());
morse_code.insert("...-".to_string(), "V".to_string());
morse_code.insert(".-.-.-".to_string(), ".".to_string());
morse_code.insert(".-...".to_string(), "&".to_string());
morse_code.insert("-.-.-.".to_string(), ";".to_string());
morse_code.insert("-.".to_string(), "N".to_string());
morse_code.insert(".....".to_string(), "5".to_string());
morse_code.insert("-...-".to_string(), "=".to_string());
morse_code.insert("..".to_string(), "I".to_string());
morse_code.insert("..--..".to_string(), "?".to_string());
morse_code.insert(".-..-.".to_string(), "\"".to_string());
morse_code.insert("-...".to_string(), "B".to_string());
morse_code.insert("--..".to_string(), "Z".to_string());
morse_code.insert(".-..".to_string(), "L".to_string());
morse_code.insert("--...".to_string(), "7".to_string());
morse_code.insert("...---...".to_string(), "SOS".to_string());
morse_code.insert("-.-.--".to_string(), "!".to_string());
morse_code.insert("--".to_string(), "M".to_string());
morse_code.insert("-----".to_string(), "0".to_string());
morse_code.insert("..-".to_string(), "U".to_string());
morse_code.insert("-.--.".to_string(), "(".to_string());
morse_code.insert(".----.".to_string(), "'".to_string());
morse_code.insert("---..".to_string(), "8".to_string());
        MorseDecoder {morse_code: morse_code}
    }

    fn decode_morse(&self, encoded: &str) -> String {
        encoded
        .trim()
        .split("   ")
        .map(|word| word.split_whitespace()
                        .map(|s| self.morse_code.get(s).unwrap())
                        .cloned()
                        .collect())
        .collect::<Vec<String>>().join(" ")
    }
    
}

#[test]
fn test1() {
    let decoder = MorseDecoder::new();
    assert_eq!(decoder.decode_morse(".... . -.--   .--- ..- -.. ."), "HEY JUDE");
}

#[test]
fn test2() {
    let decoder = MorseDecoder::new();
    assert_eq!(decoder.decode_morse(".   ."), "E E");
}

#[test]
fn test3() {
    let decoder = MorseDecoder::new();
    assert_eq!(decoder.decode_morse("   .   . "), "E E");
}

#[test]
fn test4() {
    let decoder = MorseDecoder::new();
    assert_eq!(decoder.decode_morse("      ...---... -.-.--   - .... .   --.- ..- .. -.-. -.-   -... .-. --- .-- -.   ..-. --- -..-   .--- ..- -- .--. ...   --- ...- . .-.   - .... .   .-.. .- --.. -.--   -.. --- --. .-.-.-     "), "SOS! THE QUICK BROWN FOX JUMPS OVER THE LAZY DOG.");
}

#[test]
fn test5() {
    let decoder = MorseDecoder::new();
    assert_eq!(decoder.decode_morse("         ..- -.-. -. ..--.. ...-- .-..-. ..- ....-   --... ----. .-..   ----- ...- -.- ..-. -.-.-- .- -...- -.--.-   ----- .----.   .....   --...   --. .-.. - -.. .-.-.   --.-   --... -.. -----   -.--.- ....- -....-   ----- ----. .----. -....- .-..-. --... ---... .-.. ..- .----.   ..... .-.-.- --..--   ...---... -.   .----. ....   ..--.. .----. --- ... ..-.   -....   .- . .- ----. .-..-. ----- -..- -..-. .. .----.   --   ---... ..... ----- .--.-. -- ...---...   ---...   -.. .-...   -...-   -..-. .. -.-. ..-   ----- .   ....- .-... -.-.-- ... ....- --... ---.. .- ----. ..... -.-.-- .-.-. .----. -.--   -...-   ...-..- ..   .-.-. -.--. -...- -.-.-. --.. -.. -.-.       "), "UCN?3\"U4 79L 0VKF!A=) 0' 5 7 GLTD+ Q 7D0 )4- 09'-\"7,LU' 5., SOSN 'H ?'OSF 6 AEA9\"0X/I' M ,50@MSOS , D& = /ICU 0E 4&!S478A95!+'Y = $I +(=;ZDC");
}

#[test]
fn test6() {
    let decoder = MorseDecoder::new();
    assert_eq!(decoder.decode_morse(""), "");
}

#[test]
fn test7() {
    let decoder = MorseDecoder::new();
    assert_eq!(decoder.decode_morse(".... . -.-- "), "HEY");
}

#[test]
fn test8() {
    let decoder = MorseDecoder::new();
    assert_eq!(decoder.decode_morse("       ..   -...-   ..   -..-   -..- -.- -- .. .-..   -.- .... ----. -.-.--   .---- -.--. -.--.-   ..-. -- .--.-. .-..-. -.-- .----.   -.--. -..-. -.--.- ---..   .-- .- . .----.   ....-   -- -.-- .-.-.- -....- . -.-- -.- .-.-. ...-..- ...---... -.-.-- -.--.- -.-   .-.. ---.. .-- -.- .-. --..-- ..-. .-... ..-. -....- .-- -....   -. .-... ---... --- --... .----. ...---... .---- -... ... .-. .----   -.   ---.. -.... .. -..- --. .-.   .--.-. .-- .---- .--.-. -.. -..-. .--. .--. --..   -   -   -....- --... ..-. -- -.--.-   -   -..-   ....-   --. -.- ---... -..-. --- .-.. -..-. ..--- -.-.-- ....- .-..-. -.. --   ..-.   ....- ..   --... ----. ...-   ....   ....-   ...-   ..-. ----- .-.-. ... ..- .-..   .. .....   .--.       "),
    "I = I X XKMIL KH9! 1() FM@\"Y' (/)8 WAE' 4 MY.-EYK+$SOS!)K L8WKR,F&F-W6 N&,O7'SOS1BSR1 N 86IXGR @W1@D/PPZ T T -7FM) T X 4 GK,/OL/2!4\"DM F 4I 79V H 4 V F0+SUL I5 P");
}

#[test]
fn test9() {
    let decoder = MorseDecoder::new();
    assert_eq!(decoder.decode_morse("..--.- --... --.- ----. -- --..-- .. -.--.   .-. .-.-.- ----- --   .... -.--.- -.--.- .-..-. --- ... ...- .-- ..--.. -.... --.- ...---... -.-   ...-   --..--   .-- -.--. -..-. ...---... .. .-.. ...- .-... -..   --. ---..   -.. -- .----. .-... .--- -..-   --.- ..-. ..-. -....   -..- ....- .... -.-. -.-.   .- -..- .... ...---... ----. -..-.   ----. .--. --.. -.-.-. -..-. --.   -- ...-..-   -..-. ...- --..--   .....   -.-.-. -.-. -.... .-.-.-   .-.. .----. ... . --.- -- -....   ..   ...-..- --. -.--. ..--..   .-..   ---.. -.-.-. --...   .----. -.-.-- .--.-. -.. --   ... ----.   ..--.-   .-..-. --.-   ---... - ....- .--. .-- - ---... .. -.-.-- .-.-. -.-   .   - . --... .. -..-.   .-... -.--. .--.-.   -.-.-- ....          "),
    "_7Q9M,I( R.0M H))\"OSVW?6QSOSK V , W(/SOSILV&D G8 DM'&JX QFF6 X4HCC AXHSOS9/ 9PZ;/G M$ /V, 5 ;C6. L'SEQM6 I $G(? L 8;7 '!@DM S9 _ \"Q ,T4PWT,I!+K E TE7I/ &(@ !H");
}

#[test]
fn test10() {
    let decoder = MorseDecoder::new();
    assert_eq!(decoder.decode_morse("  .---- ----- ...-- ----. -.- ...- -....   .... -. -.--.-   ..--- ..-. ----. ---.. -.-- -.--. ..... .-.   --- .--.-.   .--. ---...   -..   ... -.-.-- -.... --- ...-   .--   ..- -.--.   ...---...   -... .- ...-- .-- .-.-.- .--- ..... .----. -...- ... .----   --- ..-. ..-   -.- ...- .- ..-. -.-. --- ..--- ---.. .---- -.   -...-   -- ...- -.--. ---..   .----. --...   ...- --... -..- --..-- -.- -.. ... ..-. -.--.   -....- -- ...- --- ...---...   ----- ..--.. --..-- --.- ---... -.- -. .....   -.-.-- --..-- .. .--- -..-. ---..   .--- .... --..--   ----- ...-..-   .-..-. .-- .-.-.-   ..--- -.-.-- .-. -- ...--   .. --.. ....- -.. .-.. -..-.   -..-. -....   ..--.. -..   .. .. -..   .--.-.   -....   -.-- ...- -.-.-- -.--.- ---...   --..--   ....   .-... .--- .... -.-. -.... -.- -.--. ..--.- -.-.--   -....- ..--..   --.. ----.   --... .-.. -..- ....- -....-   --... -... -.- ... -- ..-. -.-.-- .-..   -.--.- -- .-..   .-. ----- -.- ..--.. .--.-. ...-- --.   -...- ...-- ... -..-.   .- ..--..   .-.-.- -.-.-. ...-- -.... ..---   .... --. - .--.-. .- .-         "), 
    "1039KV6 HN) 2F98Y(5R O@ P, D S!6OV W U( SOS BA3W.J5'=S1 OFU KVAFCO281N = MV(8 '7 V7X,KDSF( -MVOSOS 0?,Q,KN5 !,IJ/8 JH, 0$ \"W. 2!RM3 IZ4DL/ /6 ?D IID @ 6 YV!), , H &JHC6K(_! -? Z9 7LX4- 7BKSMF!L )ML R0K?@3G =3S/ A? .;362 HGT@AA");
}