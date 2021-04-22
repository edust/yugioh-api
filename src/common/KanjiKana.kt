package com.rarnu.yugioh.common

import com.isyscore.kotlin.common.Resource
import com.isyscore.kotlin.common.json.JSONObject

val kanjiKanaMap = mutableMapOf<String, String>()
lateinit var kanjiKanaReg: Regex

fun loadKanjiKana() {
    val jsonstr = Resource.read("kanji-kana.json")
    val json = JSONObject(jsonstr)
    json.keySet().forEach { k ->
        kanjiKanaMap[k] = json.getString(k)
    }
    kanjiKanaReg = kanjiKanaMap.keys.sortedWith { a, b -> b.length - a.length }.joinToString("|").toRegex()
}