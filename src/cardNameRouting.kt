package com.rarnu.yugioh

import com.rarnu.yugioh.common.kanaEffect
import com.rarnu.yugioh.common.kanaNormal
import com.rarnu.yugioh.common.removeKana
import com.rarnu.yugioh.database.cardName
import io.ktor.application.*
import io.ktor.response.*
import io.ktor.routing.*
import io.ktor.util.*

data class ReqCardName(val name: String)

data class RespCardName(val found: Boolean, val kk: String)

@ExperimentalStdlibApi
@KtorExperimentalAPI
fun Routing.cardNameRouting() {

    /* 根据日文卡名查询卡片的注音 */
    /* 找到原始卡名的情况，found为true,否则为 false */
    post<ReqCardName>("/kk/search") { p ->
        val name = p.name.removeKana()
        val (found, kk) = application.cardName.findKanjiKana(name)
        call.respond(RespCardName(found, kk))
    }

    /* 根据日文效果查询注音，返回注音后的文本 */
    post<ReqCardName>("/kk/effect") { p ->
        val name = p.name.removeKana()
        val ret = name.kanaEffect(application)
        call.respond(RespCardName(true, ret))
    }

    post<ReqCardName>("/kk/normal") { p ->
        val name = p.name.removeKana()
        val ret = name.kanaNormal()
        call.respond(RespCardName(true, ret))
    }
}