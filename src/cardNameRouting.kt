package com.rarnu.yugioh

import com.rarnu.yugioh.database.cardName
import io.ktor.application.*
import io.ktor.response.*
import io.ktor.routing.*
import io.ktor.util.*

data class ReqCardName(val name: String)
data class RespCardName(val found: Boolean, val kk: String)

@KtorExperimentalAPI
fun Routing.cardNameRouting() {

    /* 根据日文卡名查询卡片的注音 */
    post<ReqCardName>("/kk/search") { p ->
        val (found, kk) = application.cardName.findKanjiKana(p.name)
        call.respond(RespCardName(found, kk))
    }

}