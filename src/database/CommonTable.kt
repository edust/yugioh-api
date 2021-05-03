package com.rarnu.yugioh.database

import io.ktor.application.*
import io.ktor.util.*

@ExperimentalStdlibApi
@KtorExperimentalAPI
class CommonTable(val app: Application) {

    data class CommonData(val cardCount: Int, val kanaCount: Int, val setCount: Int)

    fun getCommonData(): CommonData {
        val c1 = app.sqlite.getCardCount()
        val c2 = app.cardName.getKanaCount()
        val c3 = app.cardName.getSetCount()
        return CommonData(c1, c2, c3)
    }
}

@ExperimentalStdlibApi
@KtorExperimentalAPI
val Application.common: CommonTable get() = CommonTable(this)