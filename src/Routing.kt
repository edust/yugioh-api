package com.rarnu.yugioh

import com.isyscore.kotlin.common.decodeURLPart
import com.isyscore.kotlin.ktor.requestParameters
import com.rarnu.yugioh.database.kanjiKana
import com.rarnu.yugioh.database.kanjiKanaMap
import io.ktor.application.*
import io.ktor.response.respond
import io.ktor.routing.*
import io.ktor.util.*

data class RespCommon(val message: String) {
    companion object {
        fun success(): RespCommon = RespCommon("OK")
        fun error(): RespCommon = RespCommon("ERROR")
        fun isSucc(b: Boolean): RespCommon = if (b) success() else error()
    }
}
data class RespKana(val kana: String)
data class ReqAddKana(val name: String, val kana: String)
data class ReqDeleteKana(val id: Int)
data class DataKana(val id: Int, val name: String, val kana: String)
data class RespListKana(val list: List<DataKana>)

@KtorExperimentalAPI
fun Routing.yugiohAPIRouting() {

    /* 路由：请求重新加载所有的注音 */
    get("/api/yugioh/kana/reload") {
        application.kanjiKana.load()
        call.respond(RespCommon.success())
    }

    post("/api/yugioh/kana/reset") {
        application.kanjiKana.resetData()
        call.respond(RespCommon.success())
    }

    /* 路由：请求对字符串进行注音 */
    get("/api/yugioh/kana") {
        val name = (call.requestParameters()["name"] ?: "").decodeURLPart()
        if (name.trim() == "") {
            call.respond(RespKana(""))
            return@get
        }
        if (kanjiKanaMap.isEmpty()) application.kanjiKana.load()
        val kanjiKanaReg = kanjiKanaMap.keys.sortedWith { a, b -> b.length - a.length }.joinToString("|").toRegex()
        val retStr = name.replace("\\[.*?\\(.*?\\)]".toRegex()) { s -> "|${s.value}|" }.split("|").filter { it.isNotEmpty() }.joinToString("") { value ->
            if (!"\\[.*?\\(.*?\\)]".toRegex().matches(value)) {
                value.replace(kanjiKanaReg) { s -> kanjiKanaMap[s.value] ?: "" }
            } else value
        }
        call.respond(RespKana(retStr))
    }

    /* 路由：请求增加一个注音 */
    post<ReqAddKana>("/api/yugioh/kana/add") { p ->
        val name = p.name.decodeURLPart()
        val kana = p.kana.decodeURLPart()
        val ret = application.kanjiKana.add(name, kana)
        call.respond(RespCommon.isSucc(ret))
    }

    /* 路由；请求删除一个注音 */
    post<ReqDeleteKana>("/api/yugioh/kana/delete") { p ->
        val ret = application.kanjiKana.delete(p.id)
        call.respond(RespCommon.isSucc(ret))
    }

    /* 路由：请求列出所有的注音 */
    get("/api/yugioh/kana/list") {
        val list = application.kanjiKana.list()
        call.respond(RespListKana(list))
    }

    /* 路由；请求查找注音（模糊查询） */
    get("/api/yugioh/kana/search") {
        val key = (call.requestParameters()["key"] ?: "").decodeURLPart()
        if (key.trim() == "") {
            call.respond(RespListKana(listOf()))
            return@get
        }
        val list = application.kanjiKana.search(key)
        call.respond(RespListKana(list))
    }

}