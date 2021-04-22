package com.rarnu.yugioh

import com.rarnu.yugioh.database.SQLiteDatabase
import com.rarnu.yugioh.database.sqlite
import io.ktor.application.*
import io.ktor.response.*
import io.ktor.routing.*
import io.ktor.util.*

data class ReqFindCard(val byEffect: Boolean, val key: String, val language: String)
data class ReqCardNames(val ids: List<String>, val language: String)


/* YDK 工具用，按卡名关键字或效果关键字查询完整的卡名和卡片ID */
@KtorExperimentalAPI
fun Routing.sqliteRouting() {

    /* 查找卡片 */
    post<ReqFindCard>("/card/find") { p ->
        // 查询要带上语言
        val ret = application.sqlite.find(p.key, p.byEffect, SQLiteDatabase.Language.valueOf(p.language))
        call.respond(ret)
    }

    /* 根据卡片ID列表查询卡片名称列表 */
    post<ReqCardNames>("/card/names") { p ->
        val ret = application.sqlite.getList(p.ids, SQLiteDatabase.Language.valueOf(p.language))
        call.respond(ret)
    }

}