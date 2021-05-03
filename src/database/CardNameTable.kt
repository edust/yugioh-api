package com.rarnu.yugioh.database

import com.isyscore.kotlin.ktor.conn
import com.rarnu.yugioh.common.toCardName
import com.rarnu.yugioh.common.toDBC
import io.ktor.application.*
import io.ktor.util.*

@ExperimentalStdlibApi
@KtorExperimentalAPI
class CardNameTable(private val app: Application) {

    /* 根据 kanji 查唯一对应的 kanji-kana */
    /* return: found, kanjikana */
    fun findKanjiKana(name: String): Pair<Boolean, String> =
        app.conn.prepareStatement("select kk from YGOCardName where kanji = ? or kanji = ?").use { stmt ->
            stmt.setString(1, name)
            stmt.setString(2, name.toDBC())
            stmt.executeQuery().use { resultSet ->
                if (resultSet.next()) {
                    true to resultSet.getString(1).toCardName()
                } else false to ""
            }
        }

    fun findSetKanjiKana(name: String): Pair<Boolean, String> =
        app.conn.prepareStatement("select kk from YGOSetName where kanji = ? or kanji = ?").use { stmt ->
            stmt.setString(1, name)
            stmt.setString(2, name.toDBC())
            stmt.executeQuery().use { resultSet ->
                if (resultSet.next()) {
                    true to resultSet.getString(1).toCardName()
                } else false to ""
            }
        }

    fun getKanaCount(): Int =
        app.conn.createStatement().use { stmt ->
            stmt.executeQuery("select count(1) from YGOCardName").use { resultSet ->
                if (resultSet.next()) resultSet.getInt(1) else 0
            }
        }

    fun getSetCount(): Int =
        app.conn.createStatement().use { stmt ->
            stmt.executeQuery("select count(1) from YGOSetName").use { resultSet ->
                if (resultSet.next()) resultSet.getInt(1) else 0
            }
        }
}

@ExperimentalStdlibApi
@KtorExperimentalAPI
val Application.cardName: CardNameTable get() = CardNameTable(this)