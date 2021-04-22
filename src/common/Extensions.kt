package com.rarnu.yugioh.common

import com.isyscore.kotlin.common.HttpMethod
import com.isyscore.kotlin.common.http
import com.isyscore.kotlin.common.json.JSONObject
import com.rarnu.yugioh.database.cardName
import com.sun.javafx.text.PrismTextLayout
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

fun String.effectCardNames(): Set<String> = split("「").drop(1).map { it.substring(0, it.indexOf("」")) }.toSet()

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
