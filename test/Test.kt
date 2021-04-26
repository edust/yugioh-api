import com.atilika.kuromoji.ipadic.Token
import com.atilika.kuromoji.ipadic.Tokenizer
import com.isyscore.kotlin.common.HttpMethod
import com.isyscore.kotlin.common.http
import com.isyscore.kotlin.common.json.JSONObject
import com.isyscore.kotlin.common.toJson
import com.rarnu.yugioh.ReqCardName
import com.rarnu.yugioh.common.effectCardNames
import com.rarnu.yugioh.common.kana
import com.rarnu.yugioh.common.loadKanjiKana
import org.junit.Test

class Test {
    @Test
    fun test() {

        val tokenizer = Tokenizer()
        val tokens: List<Token> = tokenizer.tokenize("お寿司が食べたい。")
        for (token in tokens) {
            println("surface = ${token.surface}")
            println("baseForm = ${token.baseForm}")
            println("conjugationForm = ${token.conjugationForm}")
            println("conjugationType = ${token.conjugationType}")
            println("lv1 = ${token.partOfSpeechLevel1}")
            println("lv2 = ${token.partOfSpeechLevel2}")
            println("lv3 = ${token.partOfSpeechLevel3}")
            println("lv4 = ${token.partOfSpeechLevel4}")
            println("reading = ${token.reading}")
            println("pronunciation = ${token.pronunciation}")
        }
    }

    @Test
    fun kana() {
        // val effect = "このカードは通常召喚できない。「バスター・モード」の効果及びこのカードの効果でのみ特殊召喚する事ができる。魔法・罠・効果モンスターの効果が発動した時、このカードをリリースする事でその発動を無効にし破壊する。この効果を適用したターンのエンドフェイズ時、この効果を発動するためにリリースされ墓地に存在するこのカードを、自分フィールド上に特殊召喚する事ができる。また、フィールド上に存在するこのカードが破壊された時、自分の墓地に存在する「スターダスト・ドラゴン」１体を特殊召喚する事ができる。"
        // val effect = "このカードが「剣闘獣」と名のついたモンスターの効果によって特殊召喚に成功した場合、このカードが戦闘によって相手モンスターを破壊し墓地へ送った時、自分のデッキから「剣闘獣」と名のついたカード１枚を手札に加える事ができる。このカードが戦闘を行ったバトルフェイズ終了時にこのカードをデッキに戻す事で、デッキから「剣闘獣サムニテ」以外の「剣闘獣」と名のついたモンスター１体を自分フィールド上に特殊召喚する。"
        val effect = "「剣闘獣ウェスパシアス」＋「剣闘獣」モンスター×２\n自分フィールドの上記カードをデッキに戻した場合のみ、ＥＸデッキから特殊召喚できる（「融合」は必要としない）。①：１ターンに１度、相手がモンスターの効果を発動した時に発動できる。その発動を無効にし破壊する。②：このカードがモンスターゾーンに存在する限り、相手モンスターの攻撃対象は自分が選択する。③：このカードが戦闘を行ったバトルフェイズ終了時にこのカードを持ち主のＥＸデッキに戻して発動できる。デッキから「剣闘獣」モンスター１体を特殊召喚する。"
        val jsonstr = http {
            url = "http://0.0.0.0:9987/kk/effect"
            method = HttpMethod.POST
            mimeType = "application/json"
            data = ReqCardName(effect).toJson()
        }

        println(jsonstr)
    }

    @Test
    fun kanaWithToken() {
        val effect = "①：自分のメインモンスターゾーンにモンスターが存在しない場合に発動できる。自分フィールドに「閃刀姫トークン」（戦士族・闇・星１・攻／守０）１体を守備表示で特殊召喚する。このトークンはリリースできない。自分の墓地に魔法カードが３枚以上存在する場合、そのトークンの攻撃力・守備力は１５００になる。"
        val jsonstr = http {
            url = "http://0.0.0.0:9987/kk/effect"
            method = HttpMethod.POST
            mimeType = "application/json"
            data = ReqCardName(effect).toJson()
        }

        println(jsonstr)
    }

    @Test
    fun cardNamesInEffect() {
        val str = "種族と属性が異なるモンスター２体\n①：このカードのカード名は、フィールド・墓地に存在する限り「召喚師アレイスター」として扱う。②：このカードが既にモンスターゾーンに存在する状態で、融合モンスターが融合召喚された場合に発動できる。手札を１枚選んで捨て、デッキから「召喚魔術」または「法の聖典」１枚を手札に加える。③：表側表示のこのカードが相手の効果でフィールドから離れた場合に発動できる。デッキから「魔法名－「大いなる獣」」１枚を手札に加える。"
        val list = str.effectCardNames()
        println(list)
    }

}