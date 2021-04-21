package com.rarnu.yugioh.database

import com.isyscore.kotlin.ktor.config
import io.ktor.application.*
import io.ktor.util.*
import java.sql.Connection
import java.sql.DriverManager

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

}

@KtorExperimentalAPI
val Application.sqlite: SQLiteDatabase get() = SQLiteDatabase(this)