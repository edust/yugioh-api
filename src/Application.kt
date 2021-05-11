package com.rarnu.yugioh

import com.isyscore.kotlin.ktor.installPlugin
import com.rarnu.yugioh.common.loadKanjiKana
import io.ktor.application.*
import io.ktor.http.content.*
import io.ktor.routing.*
import io.ktor.util.*

fun main(args: Array<String>): Unit = io.ktor.server.tomcat.EngineMain.main(args)

@ExperimentalStdlibApi
@KtorExperimentalAPI
@Suppress("unused")
fun Application.module() {
    installPlugin<YugiohAPISession>(
        useCompress = true,
        sessionIdentifier = "YugiohAPISession",
        allowCors = true,
        headers = mapOf("X-Engine" to "Ktor")) {
    }
    loadKanjiKana()
    routing {
        resources("web")
        resources("static")
        static("/static") { resources("static") }
        static { defaultResource("index.html", "web") }
        sqliteRouting()
        cardNameRouting()
        commonRouting()
    }
}