## 游戏王卡片服务

### 卡片信息接口

#### 1. 根据卡片 ID 获取卡片数据

请求路由: ```/api/yugioh/card/<id>``` [GET]

请求参数:

| 参数 | 类型 | 含义 | 备注 |
|:-- |:--| :-- |:-- |
| id | int | 卡片的 ID(密码) | 不可为空 |

CURL 示例:

```
$ curl 'http://yugioh.vip:9800/api/yugioh/card/89631139'
```

#### 2. 根据卡片名称关键字获取卡片名称联想

请求路由: ```/api/yugioh/list?name=<name>&lang=<lang>``` [GET]

请求参数:

| 参数 | 类型 | 含义 | 备注 |
|:-- |:--| :-- | :-- |
| name | string | 卡片名称关键字 | 不可为空 |
| lang | string | 语言 | 默认为 jp |

**返回数据时，最多返回 10 条信息**

CURL 示例:

```
$ curl 'http://yugioh.vip:9800/api/yugioh/list?name=青眼&lang=jp'
```

#### 3. 根据条件搜索卡片

请求路由: ```/api/yugioh/search``` [POST]

请求参数以 JSON 形式，在 POST BODY 内送入:

| 参数 | 类型 | 含义 | 备注 |
|:-- |:--| :-- | :-- |
| key | string | 卡片名称或效果关键字 | 可以为空串 |
| cardtype | string | 卡片类型 | 可以为空串，可选 monster/spell/trap/pendulum |
| icon | string | 魔法陷阱的图标 | 可以为空串，可选 counter/field/equip/continuous/quick-play/ritual |
| monstertype | string | 怪兽卡的主类型 | 可以为空串，可选 normal/effect/fusion/ritual/synchro/xyz/link/token |
| subtype | string | 怪兽卡的子类型 | 可以为空串，可选值为对应类型的英文小写名称 |
| attribute | string | 怪兽的属性 | 可以为空串，可选 light/dark/earth/wind/fire/water/divine |
| race | string | 怪兽的种族 | 可以为空串，可选值为对应种族的英文小写名称 |
| lang | string | 语言 | 默认为 jp |

CURL 示例:

```
$ curl -X POST -d '{"key":"青眼","cardtype":"","icon":"","attribute":"dark","subtype":"","race":"dragon","monstertype":"","lang":"jp"}' http://yugioh.vip:9800/api/yugioh/search
```

#### 4. 随机获得一张卡片的数据

请求路由: ```/api/yugioh/random?lang=<lang>``` [GET]

请求参数:

| 参数 | 类型 | 含义 | 备注 |
|:-- |:--| :-- | :-- |
| lang | string | 语言 | 默认为 jp |

CURL 示例:

```
$ curl 'http://yugioh.vip:9800/api/yugioh/random?lang=jp'
```

### YDK 接口

#### 1. 根据卡片 ID 列表获取名称列表

请求路由: ```/api/ydk/names``` [POST]

请求参数以 JSON 形式，在 POST BODY 内送入:

| 参数 | 类型 | 含义 | 备注 |
|:-- |:--| :-- | :-- |
| ids | list | 卡片 ID 的列表 | 不可以为空 |
| lang | string | 语言 | 默认为 jp |

**返回数据时，仅返回卡片的 ID 和名称**

CURL 示例:

```
$ curl -X POST -d '{"ids":[9433350,24221739], "lang":"jp"}' 'http://yugioh.vip:9800/api/ydk/names'
```

#### 2. 搜索卡片名称或效果对应的卡片

请求路由: ```/api/ydk/find``` [POST]

请求参数以 JSON 形式，在 POST BODY 内送入:

| 参数 | 类型 | 含义 | 备注 |
|:-- |:--| :-- | :-- |
| key | string | 卡片名称或效果关键字 | 不可以为空 |
| byEffect | boolean | 是否查询效果 | 默认为false，只查卡名 |
| lang | string | 语言 | 默认为 jp |

**返回数据时，仅返回卡片的 ID 和名称，最多返回 100 条数据**

CURL 示例:

```
$ curl -X POST -d '{"key":"青眼", "byEffect": false, "lang":"jp"}' 'http://yugioh.vip:9800/api/ydk/find'
```

### 注音接口

#### 1. 查询卡名的注音

请求路由: ```/api/kanjikana/name``` [POST]

请求参数以 JSON 形式，在 POST BODY 内送入:

| 参数 | 类型 | 含义 | 备注 |
|:-- |:--| :-- | :-- |
| name | string | 要注音的卡片名 | 不可以为空 |

CURL 示例:

```
$ curl -X POST -d '{"name":"青眼の白龍"}' 'http://yugioh.vip:9800/api/kanjikana/name'
```

#### 2. 查询卡片效果的注音

请求路由: ```/api/kanjikana/effect``` [POST]

请求参数以 JSON 形式，在 POST BODY 内送入:

| 参数 | 类型 | 含义 | 备注 |
|:-- |:--| :-- | :-- |
| name | string | 要注音的卡片效果文 | 不可以为空 |

CURL 示例:

```
$ curl -X POST -d '{"name":"「青眼の白龍」＋「青眼の白龍」＋「青眼の白龍」"}' 'http://yugioh.vip:9800/api/kanjikana/effect'
```

#### 3. 查询常规日文语句的注音

请求路由: ```/api/kanjikana/text``` [POST]

请求参数以 JSON 形式，在 POST BODY 内送入:

| 参数 | 类型 | 含义 | 备注 |
|:-- |:--| :-- | :-- |
| name | string | 要注音的日语文本 | 不可以为空 |

CURL 示例:

```
$ curl -X POST -d '{"name":"高い攻撃力を誇る伝説のドラゴン。どんな相手でも粉砕する、その破壊力は計り知れない。"}' 'http://yugioh.vip:9800/api/kanjikana/text'
```

