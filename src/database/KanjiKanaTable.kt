package com.rarnu.yugioh.database

import com.isyscore.kotlin.common.json.JSONObject
import com.isyscore.kotlin.ktor.conn
import com.rarnu.yugioh.DataKana
import io.ktor.application.*
import io.ktor.util.*

val kanjiKanaMap = mutableMapOf<String, String>()

@KtorExperimentalAPI
class KanjiKanaTable(val app: Application) {

    /* 增加一个注音 */
    fun add(name: String, kana: String): Boolean =
        app.conn.createStatement().use { stmt ->
            stmt.executeUpdate("insert into KanjiKana(name, kana) values ('$name', '$kana')") > 0
        }

    /* 删除一个注音 */
    fun delete(id: Int): Boolean =
        app.conn.createStatement().use { stmt ->
            stmt.executeUpdate("delete from KanjiKana where id = $id") > 0
        }

    /* 列出所有的注音 */
    fun list(): List<DataKana> =
        app.conn.createStatement().use { stmt ->
            stmt.executeQuery("select * from KanjiKana").use { resultSet ->
                val list = mutableListOf<DataKana>()
                while (resultSet.next()) {
                    list.add(DataKana(resultSet.getInt("id"), resultSet.getString("name"), resultSet.getString("kana")))
                }
                list
            }
        }

    /* 模糊查询注音 */
    fun search(key: String): List<DataKana> =
        app.conn.createStatement().use { stmt ->
            stmt.executeQuery("select * from KanjiKana where name like '%$key%'").use { resultSet ->
                val list = mutableListOf<DataKana>()
                while (resultSet.next()) {
                    list.add(DataKana(resultSet.getInt("id"), resultSet.getString("name"), resultSet.getString("kana")))
                }
                list
            }
        }

    /* 加载全部的注音 */
    fun load() {
        kanjiKanaMap.clear()
        app.conn.createStatement().use { stmt ->
            stmt.executeQuery("select * from KanjiKana").use { resultSet ->
                while (resultSet.next()) {
                    kanjiKanaMap[resultSet.getString("name")] = resultSet.getString("kana")
                }
            }
        }
    }

    /* 重置所有数据 */
    fun resetData() {
        app.conn.createStatement().use { stmt ->
            stmt.executeUpdate("delete from KanjiKana")
        }
        val json = JSONObject(ORIGIN_DATA)
        val map = json.keySet().map { k -> k to json.getString(k) }.toMap()
        val count = app.conn.createStatement().use { stmt ->
            map.forEach { (k, v) ->
                stmt.addBatch("insert into KanjiKana(name, kana) values ('$k', '$v')")
            }
            stmt.executeLargeBatch()
        }.size
        println("reset data => $count")
    }
}


@KtorExperimentalAPI
val Application.kanjiKana: KanjiKanaTable get() = KanjiKanaTable(this)