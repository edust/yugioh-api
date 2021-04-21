create
database YugiohAPI character set utf8mb4;

create table KanjiKana
(
    id   bigint primary key auto_increment,
    name varchar(256)  not null,
    kana varchar(1024) not null
) character set utf8mb4;

CREATE TABLE `YGOCardName`
(
    `id`    bigint       NOT NULL AUTO_INCREMENT,
    `pack`  varchar(512) NOT NULL,
    `kanji` varchar(512) NOT NULL,
    `kana`  varchar(512) NOT NULL,
    `kk`    varchar(1024) DEFAULT '',
    `done`  int           DEFAULT '0',
    PRIMARY KEY (`id`),
    UNIQUE KEY `kanji` (`kanji`)
) ENGINE=InnoDB AUTO_INCREMENT=20948 DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci;

CREATE TABLE `YGOPack`
(
    `id`   bigint       NOT NULL AUTO_INCREMENT,
    `pack` varchar(512) NOT NULL,
    PRIMARY KEY (`id`),
    UNIQUE KEY `pack` (`pack`)
) ENGINE=InnoDB AUTO_INCREMENT=1040 DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci;

alter table YGOCardName add column donetime bigint default 0;