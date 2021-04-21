package com.rarnu.yugioh.database

import com.isyscore.kotlin.ktor.conn
import io.ktor.application.*
import io.ktor.util.*

data class CardNameData(val id: Int, val kanji: String, val kana: String, val kk: String, val done: Int)

@KtorExperimentalAPI
class CardNameTable(private val app: Application) {

    /* 根据 kanji 查唯一对应的 kanji-kana */
    /* return: found, kanjikana */
    fun findKanjiKana(name: String): Pair<Boolean, String> =
        app.conn.prepareStatement("select kk from YGOCardName where done = 1 and kanji = ?").use { stmt ->
            stmt.setString(1, name)
            stmt.executeQuery().use { resultSet ->
                if (resultSet.next()) {
                    true to resultSet.getString(1)
                } else false to ""
            }
        }

    fun batchExecuteSQL(sqlList: List<String>): Int =
        sqlList.count { sql ->
            app.conn.createStatement().use { stmt ->
                stmt.executeUpdate(sql) != 0
            }
        }

    fun deleteById(id: Int): Boolean =
        app.conn.prepareStatement("delete from YGOCardName where id = ?").use { stmt ->
            stmt.setInt(1, id)
            stmt.executeUpdate() != 0
        }

    fun getListByKey(key: String, done: Int): List<CardNameData> =
        app.conn.createStatement().use { stmt ->
            stmt.executeQuery("select * from YGOCardName where kanji like '%$key%' and done = $done").use { resultSet ->
                val list = mutableListOf<CardNameData>()
                while (resultSet.next()) {
                    list.add(
                        CardNameData(
                            resultSet.getInt("id"),
                            resultSet.getString("kanji"),
                            resultSet.getString("kana"),
                            resultSet.getString("kk"),
                            resultSet.getInt("done")
                        )
                    )
                }
                list
            }
        }

    fun updateById(id: Int, kk: String, done: Int): Boolean =
        app.conn.createStatement().use { stmt ->
            stmt.executeUpdate("update YGOCardName set kk = '$kk', done = $done where id = $id") != 0
        }
}

@KtorExperimentalAPI
val Application.cardName: CardNameTable
    get() = CardNameTable(this)