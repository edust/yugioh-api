create database YugiohAPI character set utf8mb4;

/* 卡名表 */
CREATE TABLE `YGOCardName`
(
    `id`    bigint       NOT NULL AUTO_INCREMENT,
    `pack`  varchar(512) NOT NULL,
    `kanji` varchar(512) NOT NULL,
    `kana`  varchar(512) NOT NULL,
    `kk`    varchar(1024) DEFAULT '',
    `done`  int           DEFAULT '0',
    `donetime` bigint default 0,
    PRIMARY KEY (`id`),
    UNIQUE KEY `kanji` (`kanji`)
) character set utf8mb4;

alter table YGOCardName modify kanji varchar(512) collate utf8mb4_unicode_ci;

/* 字段表 */
create table `YGOSetName` (
    `id`    bigint       NOT NULL AUTO_INCREMENT,
    `kanji` varchar(512) NOT NULL,
    `kana`  varchar(512) NOT NULL,
    `kk`    varchar(1024) DEFAULT '',
    `done`  int           DEFAULT '0',
    `donetime` bigint default 0,
    PRIMARY KEY (`id`),
    UNIQUE KEY `kanji` (`kanji`)
) character set utf8mb4;

alter table YGOSetName modify kanji varchar(512) collate utf8mb4_bin;

