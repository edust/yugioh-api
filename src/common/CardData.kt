package com.rarnu.yugioh.common

data class CardData(
    val id: Int,
    val name: String,
    val desc: String,
    val type: Int,
    val atk: Int,
    val def: Int,
    val level: Int,
    val race: Int,
    val attribute: Int,
    val setid: String
)

data class CardNameData(
    val id: Int,
    val name: String
)