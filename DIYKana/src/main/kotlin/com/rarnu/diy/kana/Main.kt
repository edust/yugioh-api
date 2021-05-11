package com.rarnu.diy.kana

import com.atilika.kuromoji.ipadic.Token
import com.atilika.kuromoji.ipadic.Tokenizer
import com.isyscore.kotlin.common.toJson
import java.util.*

data class RespData(val kana: String = "")

fun main(args: Array<String>) {
    if (args.isEmpty()) {
        println(RespData().toJson())
        return
    }
    val b64 = args[0].trim()
    if (b64 == "") {
        println(RespData().toJson())
        return
    }
    val aname = String(Base64.getDecoder().decode(b64))
    val tokenizer = Tokenizer()
    val tokens: List<Token> = tokenizer.tokenize(aname)
    val ret = tokens.map { it.kana() }.reduce { acc, s -> "$acc$s" }
    val retB64 = Base64.getEncoder().encodeToString(ret.toByteArray())
    println(RespData(retB64).toJson())
}

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