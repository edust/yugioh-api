package com.rarnu.yugioh

import com.rarnu.yugioh.database.SQLiteDatabase
import com.rarnu.yugioh.database.sqlite
import io.ktor.application.*
import io.ktor.http.*
import io.ktor.locations.*
import io.ktor.response.*
import io.ktor.routing.*
import io.ktor.util.*

data class ReqFindCard(val byEffect: Boolean, val key: String, val language: String)
data class ReqCardNames(val ids: List<String>, val language: String)

data class Resp<T>(val status: Int = 200, val message: String = "", val data: T? = null)


/* YDK 工具用，按卡名关键字或效果关键字查询完整的卡名和卡片ID */
@KtorExperimentalAPI
fun Routing.sqliteRouting() {

    /* 查找卡片 */
    post<ReqFindCard>("/card/find") { p ->
        // 查询要带上语言
        val ret = application.sqlite.find(p.key, p.byEffect, p.language)
        call.respond(ret)
    }

    /* 根据卡片ID列表查询卡片名称列表 */
    post<ReqCardNames>("/card/names") { p ->
        val ret = application.sqlite.getList(p.ids, p.language)
        call.respond(ret)
    }

    get("api/yugioh/card/{id}") {
        val id = try { (call.parameters["id"] ?: "0").toInt() } catch (th: Throwable) { null }
        if (id == null) {
            call.respond(HttpStatusCode.InternalServerError, "ID 错误")
            return@get
        }
        val lang = call.request.queryParameters["lang"] ?: "jp"
        val info = application.sqlite.getCardInfo(id, lang)
        if (info == null) {
            call.respond(HttpStatusCode.InternalServerError, "数据不存在")
        } else {
            call.respond(Resp(data = info))
        }
    }

    get("/api/yugioh/card") {
        val name = call.request.queryParameters["name"] ?: ""
        val lang = call.request.queryParameters["lang"] ?: "jp"
        val list = application.sqlite.getCardList(name, lang)
        call.respond(Resp(data = list))
    }

    get("/api/yugioh/random-card") {
        val lang = call.request.queryParameters["lang"] ?: "jp"
        val info = application.sqlite.getRandomCard(lang)
        if (info == null) {
            call.respond(HttpStatusCode.InternalServerError, "数据不存在")
        } else {
            call.respond(Resp(data = info))
        }
    }
}