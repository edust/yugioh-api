package com.rarnu.yugioh

import com.rarnu.yugioh.database.common
import io.ktor.application.*
import io.ktor.response.*
import io.ktor.routing.*
import io.ktor.util.*

@KtorExperimentalAPI
@ExperimentalStdlibApi
fun Routing.commonRouting() {

    get("/api/common/count") {

        val ret = application.common.getCommonData()
        call.respond(ret)
    }
}