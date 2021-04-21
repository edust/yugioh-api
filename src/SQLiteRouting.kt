package com.rarnu.yugioh

import com.rarnu.yugioh.database.SQLiteDatabase
import com.rarnu.yugioh.database.sqlite
import io.ktor.application.*
import io.ktor.response.*
import io.ktor.routing.*
import io.ktor.util.*

data class ReqFindCard(val byEffect: Boolean, val key: String, val language: String)
data class ReqCardNames(val ids: List<String>, val language: String)

@KtorExperimentalAPI
fun Routing.sqliteRouting() {

    post<ReqFindCard>("/card/find") { p ->
        // 查询要带上语言
        val ret = application.sqlite.find(p.key, p.byEffect, SQLiteDatabase.Language.valueOf(p.language))
        call.respond(ret)
    }

    post<ReqCardNames>("/card/names") { p ->
        val ret = application.sqlite.getList(p.ids, SQLiteDatabase.Language.valueOf(p.language))
        call.respond(ret)
    }

}