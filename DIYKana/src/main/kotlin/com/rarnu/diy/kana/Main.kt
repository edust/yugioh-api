package com.rarnu.diy.kana

import com.isyscore.kotlin.common.toJson
import java.util.*

data class RespData(val kana: String = "")

fun main(args: Array<String>) {
    if (args.isEmpty()) {
        println(RespData().toJson())
        return
    }
    val b64 = args[0].trim()
    if (b64 == "") {
        println(RespData().toJson())
        return
    }
    val aname = String(Base64.getDecoder().decode(b64))
    val ret = KanaUtil.kana(aname)
    val retB64 = Base64.getEncoder().encodeToString(ret.toByteArray())
    println(RespData(retB64).toJson())
}

