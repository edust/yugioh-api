package com.rarnu.yugioh

import com.isyscore.kotlin.ktor.installPlugin
import com.rarnu.yugioh.common.loadKanjiKana
import com.rarnu.yugioh.database.cardName
import io.ktor.application.*
import io.ktor.features.*
import io.ktor.http.*
import io.ktor.http.content.defaultResource
import io.ktor.http.content.resources
import io.ktor.http.content.static
import io.ktor.routing.routing
import io.ktor.util.*
import kotlin.time.Duration
import kotlin.time.ExperimentalTime

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
    }
}