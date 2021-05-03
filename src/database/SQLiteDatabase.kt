package com.rarnu.yugioh.database

import com.isyscore.kotlin.ktor.config
import com.rarnu.yugioh.common.CardData
import com.rarnu.yugioh.common.CardNameData
import io.ktor.application.*
import io.ktor.util.*
import java.sql.Connection
import java.sql.DriverManager

// https://chocolatecannoli.com/database/OmegaDB.cdb

@KtorExperimentalAPI
class SQLiteDatabase(private val app: Application) {

    enum class Language { sc, tc, jp, en }
    data class CardInfo(val id: String, val name: String)

    private lateinit var conn: Connection

    init {
        // org.sqlite.JDBC
        Class.forName("org.sqlite.JDBC")
        val path = app.config("ktor.sqlite.filePath")
        val jdbcUrl = "jdbc:sqlite://$path"
        try {
            conn = DriverManager.getConnection(jdbcUrl)
        } catch (th: Throwable) {
            println("Database Error: $th")
        }
    }

    fun getList(ids: List<String>, language: Language): List<CardInfo> {
        val table = when (language) {
            Language.jp -> "ja_texts"
            Language.en -> "texts"
            Language.sc -> "zhcn_texts"
            Language.tc -> "zhtw_texts"
        }
        val list = mutableListOf<CardInfo>()
        conn.prepareStatement("select id, name from $table where id in (${ids.joinToString(",")}) limit 100").use { stmt ->
            stmt.executeQuery().use { resultSet ->
                while (resultSet.next()) {
                    list.add(CardInfo(resultSet.getString("id"), resultSet.getString("name")))
                }
            }
        }
        return list
    }

    fun find(key: String, isEffect: Boolean, lang: Language): List<CardInfo> = find(key, lang, if (isEffect) "desc" else "name")

    private fun find(key: String, language: Language, field: String): List<CardInfo> {
        val table = when (language) {
            Language.jp -> "ja_texts"
            Language.en -> "texts"
            Language.sc -> "zhcn_texts"
            Language.tc -> "zhtw_texts"
        }
        val list = mutableListOf<CardInfo>()
        conn.prepareStatement("select id, name from $table where $field like '%$key%' limit 100").use { stmt ->
            stmt.executeQuery().use { resultSet ->
                while (resultSet.next()) {
                    list.add(CardInfo(resultSet.getString("id"), resultSet.getString("name")))
                }
            }
        }
        return list
    }

    fun getCardInfo(id: Int, lang: String): CardData? {
        val t = getTextTable(lang)
        return conn.createStatement().use { stmt ->
            stmt.executeQuery("select ${t}.id, ${t}.name, ${t}.desc, datas.type, datas.atk, datas.def, datas.level, datas.race, datas.attribute, datas.setid from $t, datas where ${t}.id = $id and datas.id = $id").use { resultSet ->
                if (resultSet.next()) {
                    CardData(
                        resultSet.getInt("id"),
                        resultSet.getString("name").replace("&#64025;", "神"),
                        resultSet.getString("desc").replace("&#64025;", "神"),
                        resultSet.getInt("type"),
                        resultSet.getInt("atk"),
                        resultSet.getInt("def"),
                        resultSet.getInt("level"),
                        resultSet.getInt("race"),
                        resultSet.getInt("attribute"),
                        resultSet.getString("setid")
                    )
                } else null
            }
        }
    }

    fun getCardList(name: String, lang: String): List<CardNameData> {
        val t = getTextTable(lang)
        return conn.createStatement().use { stmt ->
            stmt.executeQuery("select id, name from $t where name like '%${name}%' limit 10").use { resultSet ->
                val list = mutableListOf<CardNameData>()
                while (resultSet.next()) {
                    list.add(CardNameData(resultSet.getInt("id"), resultSet.getString("name")))
                }
                list
            }
        }
    }

    fun getRandomCard(lang: String): CardData? {
        val t = getTextTable(lang)
        val id = conn.createStatement().use { stmt ->
            stmt.executeQuery("select id from $t where id >= 10000 and id <= 99999999 order by RANDOM() limit 1").use { resultSet ->
                if (resultSet.next()) resultSet.getInt("id") else -1
            }
        }
        return if (id != -1) getCardInfo(id, lang) else null
    }

    fun getCardCount(): Int =
        conn.createStatement().use { stmt ->
            stmt.executeQuery("select count(1) from datas").use { resultSet ->
                if (resultSet.next()) resultSet.getInt(1) else 0
            }
        }

    private fun getTextTable(lang: String): String = when (lang) {
        "sc" -> "zhcn_texts"
        "tc" -> "zhtw_texts"
        "jp" -> "ja_texts"
        "en" -> "texts"
        else -> "ja_texts"
    }
}

@KtorExperimentalAPI
val Application.sqlite: SQLiteDatabase
    get() = SQLiteDatabase(this)