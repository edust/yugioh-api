package com.rarnu.yugioh

import com.isyscore.kotlin.ktor.session
import io.ktor.application.ApplicationCall
import io.ktor.util.pipeline.PipelineContext
import java.util.*

data class YugiohAPISession(val uuid: String)

inline val PipelineContext<*, ApplicationCall>.localSession: YugiohAPISession
    get() = session {
        YugiohAPISession(UUID.randomUUID().toString())
    }
