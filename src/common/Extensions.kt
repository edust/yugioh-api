package com.rarnu.yugioh.common

import com.atilika.kuromoji.ipadic.Token
import com.atilika.kuromoji.ipadic.Tokenizer
import com.rarnu.yugioh.database.cardName
import io.ktor.application.*
import io.ktor.util.*
import kotlin.text.toCharArray

@ExperimentalStdlibApi
fun String.toDBC(): String {
    var n = characterToHalf()
    n = n.numberToHalf()
    n = n.replace("Ɐ", "∀").replace("´", "’")
    return n
}

fun String.toCardName(): String {
    return replace("∀", "Ɐ").replace("’", "´")
}

fun String.effectCardNames(): Set<String> {
    val result = mutableListOf<String>()
    var tmp = this
    while (tmp.contains("「")) {
        val start = tmp.indexOf("「")
        var leftCount = 1
        var rightCount = 0
        for (i in start + 1 until tmp.length) {
            if (tmp[i] == '「') {
                leftCount++
                continue
            }
            if (tmp[i] == '」') {
                rightCount++
                if (rightCount == leftCount) {
                    val tStr = tmp.substring(start + 1, i)
                    result.add(tStr)
                    tmp = tmp.substring(i + 1)
                    break
                }
            }
        }
    }
    return result.toSet()
    // return split("「").drop(1).map { it.substring(0, it.indexOf("」")) }.toSet()
}

fun String.kana(): String =
    replace("\\[.*?\\(.*?\\)]".toRegex()) { s -> "|${s.value}|" }.split("|").filter { it.isNotEmpty() }.joinToString("") { value ->
        if (!"\\[.*?\\(.*?\\)]".toRegex().matches(value)) {
            value.replace(kanjiKanaReg) { s -> kanjiKanaMap[s.value] ?: "" }
        } else value
    }

/* 对效果文字进行完整的注音 */
@KtorExperimentalAPI
@ExperimentalStdlibApi
fun String.kanaEffect(app: Application): String {
    val cn = effectCardNames()
    var e2 = this
    cn.forEachIndexed { index, s -> e2 = e2.replace(s, "{{$index}}") }
    e2 = e2.kana()
    cn.forEachIndexed { index, s ->
        var isToken = false
        val tmp = if (s.endsWith("トークン")) {
            isToken = true
            s.replace("トークン", "").trim()
        } else s
        val (found, kk) = app.cardName.findKanjiKana(tmp)
        var kkstr = if (!found) {
            //卡名没找到，找字段
            val (found1, kk1) = app.cardName.findSetKanjiKana(tmp)
            if (found1) kk1 else tmp.kana()
        } else kk
        if (isToken) kkstr += "トークン"
        e2 = e2.replace("{{$index}}", kkstr)
    }
    return e2
}

fun String.kanaNormal(): String {
    val tokenizer = Tokenizer()
    val tokens: List<Token> = tokenizer.tokenize(this)
    var retStr = ""
    for (token in tokens) {
        retStr += token.kana()
    }
    return retStr
}

fun String.removeKana(): String =
    replace("\\[.*?\\(.*?\\)]".toRegex()) { s ->
        s.value.replace("[", "").run { substring(0, indexOf("(")) }
    }


@ExperimentalStdlibApi
private fun String.characterToHalf(): String =
    toCharArray().map { c ->
        val code = c.code
        if (c == '　') {
            ' '
        } else if (c == '﹒') {
            '·'
        } else if ((c in arrayOf('＠', '．', '＆', '？', '！')) || (code in 65313..65338) || (code in 65345..65370)) {
            (code - 65248).toChar()
        } else {
            c
        }
    }.joinToString("").replace("「.*?」".toRegex()) { s -> s.value.numberToHalf() }

@ExperimentalStdlibApi
private fun String.numberToHalf(): String =
    toCharArray().map { c ->
        val code = c.code
        if (code in 65296..65305) {
            (code - 65248).toChar()
        } else {
            c
        }
    }.joinToString("")


private fun Token.kana(): String {
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

private const val hiragana = "ぁあぃいぅうぇえぉおかがきぎくぐけげこごさざしじすずせぜそぞただちぢっつづてでとどなにぬねのはばぱひびぴふぶぷへべぺほぼぽまみむめもゃやゅゆょよらりるれろゎわゐゑをん"
private const val katakana = "ァアィイゥウェエォオカガキギクグケゲコゴサザシジスズセゼソゾタダチヂッツヅテデトドナニヌネノハバパヒビピフブプヘベペホボポマミムメモャヤュユョヨラリルレロヮワヰヱヲンヴヵヶ"

private fun String.toKataKana(): String {
    var ret = ""
    forEach { c ->
        val idx = hiragana.indexOf(c)
        ret += if (idx != -1) katakana[idx] else c
    }
    return ret
}

private fun String.toHiragana(): String {
    var ret = ""
    forEach { c ->
        val idx = katakana.indexOf(c)
        ret += if (idx != -1) hiragana[idx] else c
    }
    return ret
}