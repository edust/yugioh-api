import com.atilika.kuromoji.ipadic.Token
import com.atilika.kuromoji.ipadic.Tokenizer
import org.junit.Test

class NormalKanaTest {

    @Test
    fun test() {
        // val str = "高い攻撃力を誇る伝説のドラゴン。どんな相手でも粉砕する、その破壊力は計り知れない。"
        // val str = "王室をガードする衛兵ロボ。当たるまで追い続けるミサイルを撃つ。"
        // val str = "スロットに揃う数で能力を変化させる事ができるという機械。"
        // val str = "誰もいないはずの13番目の墓から突然現れたゾンビ。"
        val str = "重機関銃装備の装甲車。どんな荒れ地も平気で走る事ができる。"
        val tokenizer = Tokenizer()
        val tokens: List<Token> = tokenizer.tokenize(str)
        var retStr = ""
        for (token in tokens) {
            retStr += token.kana()
        }
        println(retStr)
    }

    fun Token.kana(): String {
        if (surface.toKataKana() == reading.toKataKana() || reading == "*") return surface
        var head = ""
        var tail = ""
        var s1Ori = surface
        var s1 = surface.toHiragana()
        var s2 = reading.toHiragana()
        // 判断是否头部相同
        while (s1[0] == s2[0]) {
            head += s1[0]
            s1Ori = s1Ori.drop(1)
            s1 = s1.drop(1)
            s2 = s2.drop(1)
        }
        // 判断是否尾部相同
        while (s1[s1.length - 1] == s2[s2.length - 1]) {
            tail = s1[s1.length - 1] + tail
            s1Ori = s1Ori.dropLast(1)
            s1 = s1.dropLast(1)
            s2 = s2.dropLast(1)
        }
        return "$head[$s1Ori($s2)]$tail"
    }

    val hiragana = "ぁあぃいぅうぇえぉおかがきぎくぐけげこごさざしじすずせぜそぞただちぢっつづてでとどなにぬねのはばぱひびぴふぶぷへべぺほぼぽまみむめもゃやゅゆょよらりるれろゎわゐゑをん"
    val katakana = "ァアィイゥウェエォオカガキギクグケゲコゴサザシジスズセゼソゾタダチヂッツヅテデトドナニヌネノハバパヒビピフブプヘベペホボポマミムメモャヤュユョヨラリルレロヮワヰヱヲンヴヵヶ"

    fun String.toKataKana(): String {
        var ret = ""
        forEach { c ->
            val idx = hiragana.indexOf(c)
            ret += if (idx != -1) katakana[idx] else c
        }
        return ret
    }

    fun String.toHiragana(): String {
        var ret = ""
        forEach { c ->
            val idx = katakana.indexOf(c)
            ret += if (idx != -1) hiragana[idx] else c
        }
        return ret
    }
}